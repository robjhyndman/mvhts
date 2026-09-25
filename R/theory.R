# ====================================================================
# Theory: when does joint (multivariate) MinT reconciliation differ
# from reconciling each variable separately?
#
# Notation. m variables share a hierarchy with summing matrix S
# (n x n_b) and constraint matrix C (n_a x n, C S = 0). Stacking the
# variables (variable-major, matching vec(Y_t)) gives S* = I_m (x) S and
# C* = I_m (x) C. W is the (mn x mn) base forecast error covariance, and
# W_jk its (n x n) block for variables j and k.
#
# Joint MinT map:    M = I - G C*,  G = W C*' (C* W C*')^{-1}.
# Separate MinT map: blockdiag(M_j), M_j = I - W_jj C'(C W_jj C')^{-1} C.
#
# Proposition. The following are equivalent:
#   (a) M equals the separate map;
#   (b) M is block diagonal by variable;
#   (c) G is block diagonal by variable;
#   (d) M_j W_jk C' = 0 for all j != k, i.e. Cov(M_j e_j, C e_k) = 0;
#   (e) col(W_jk C') is contained in col(W_jj C') for all j != k.
# Separable W = V (x) Sigma_0 satisfies (e), and so does any W + S* K S*'
# (the map ignores S* K S*' because C* S* = 0). Statements and proofs are
# in sections/theory.tex and sections/appendix-proofs.tex.
# ====================================================================

# --------------------------------------------------------------------
# Hierarchy matrices
# --------------------------------------------------------------------

# Constraint matrix C = [I, -A] for S = [A; I] (aggregate rows first)
make_C <- function(S) {
  n_b <- NCOL(S)
  n_a <- NROW(S) - n_b
  if (!isTRUE(all.equal(unname(S[-seq_len(n_a), , drop = FALSE]), diag(n_b)))) {
    stop("S must have its identity block in the last n_b rows.")
  }
  cbind(diag(n_a), -S[seq_len(n_a), , drop = FALSE])
}

stack_matrix <- function(X, m) kronecker(diag(m), X)

# Rows/columns of variable j in the stacked system
var_index <- function(j, n) (j - 1) * n + seq_len(n)

block <- function(X, j, k, n_row, n_col = n_row) {
  X[var_index(j, n_row), var_index(k, n_col), drop = FALSE]
}

# --------------------------------------------------------------------
# Reconciliation maps
# --------------------------------------------------------------------

# G = W C'(C W C')^{-1}; works for C or C*
G_matrix <- function(W, C) {
  WCt <- W %*% t(C)
  t(solve(C %*% WCt, t(WCt)))
}

# MinT projection M = I - G C (zero-constrained form)
mint_map <- function(W, C) {
  diag(NROW(W)) - G_matrix(W, C) %*% C
}

# Separate reconciliation: univariate MinT for each variable, using the
# diagonal blocks of W
separate_map <- function(W, C, m) {
  n <- NCOL(C)
  M <- matrix(0, m * n, m * n)
  for (j in seq_len(m)) {
    idx <- var_index(j, n)
    M[idx, idx] <- mint_map(W[idx, idx], C)
  }
  M
}

# Condition (d): largest |M_j W_jk C'| over j != k
cross_condition <- function(W, C, m) {
  n <- NCOL(C)
  worst <- 0
  for (j in seq_len(m)) {
    Mj <- mint_map(block(W, j, j, n), C)
    for (k in setdiff(seq_len(m), j)) {
      worst <- max(worst, abs(Mj %*% block(W, j, k, n) %*% t(C)))
    }
  }
  worst
}

# Population MSE (trace of reconciled error covariance) of map M
pop_mse <- function(M, W) sum(diag(M %*% W %*% t(M)))

# --------------------------------------------------------------------
# The kappa diagnostic: off-diagonal block mass of G.
# With standardise = TRUE each variable is first scaled by the square
# root of the mean diagonal of C W_jj C' (its incoherence variances), so
# kappa does not depend on the units of the variables. (Rescaling
# variable j by c_j multiplies block G_jk by c_j / c_k.) Because C S = 0,
# adding S* K S*' to W changes neither G nor the scaling, so the value of
# kappa is invariant to such components, not just whether it is zero.
# --------------------------------------------------------------------
kappa_mv <- function(W, C, m, standardise = TRUE) {
  n <- NCOL(C)
  if (standardise) {
    s <- vapply(
      seq_len(m),
      \(j) sqrt(mean(diag(C %*% block(W, j, j, n) %*% t(C)))),
      numeric(1)
    )
    d <- rep(1 / s, each = n)
    W <- W * outer(d, d)
  }
  G <- G_matrix(W, stack_matrix(C, m))
  n_a <- NROW(C)
  on_diag <- 0
  for (j in seq_len(m)) {
    on_diag <- on_diag + sum(block(G, j, j, n, n_a)^2)
  }
  total <- sum(G^2)
  sqrt(max(0, total - on_diag) / total)
}

# --------------------------------------------------------------------
# Plug-in gain: the proportional MSE reduction that joint reconciliation
# would achieve over separate reconciliation if W were the true error
# covariance. Zero exactly when kappa is zero, and bounded by the size of
# the incoherent component, which kappa does not measure.
# --------------------------------------------------------------------
plugin_gain <- function(W, C, m) {
  C_star <- stack_matrix(C, m)
  1 - pop_mse(mint_map(W, C_star), W) / pop_mse(separate_map(W, C, m), W)
}

# Plug-in gain for a linear combination a'y of the stacked series (for
# example admissions minus dismissals at one node). With the true W,
# joint MinT minimises the reconciled error covariance in the positive
# semidefinite order, so this is non-negative for every a; the gain can be
# much larger for some combinations than the trace-based plugin_gain().
plugin_gain_combination <- function(W, C, m, a) {
  M <- mint_map(W, stack_matrix(C, m))
  M_sep <- separate_map(W, C, m)
  1 -
    drop(t(a) %*% M %*% W %*% t(M) %*% a) /
      drop(t(a) %*% M_sep %*% W %*% t(M_sep) %*% a)
}

# --------------------------------------------------------------------
# Nearest Kronecker product V (x) Sigma_0 to W in Frobenius norm
# (Van Loan & Pitsianis, 1993). Row j + (k - 1) m of the rearranged
# matrix is vec(W_jk), so W = A (x) B gives vec(A) vec(B)'.
# rel_error = sqrt(1 - sigma_1^2 / sum sigma_i^2).
# --------------------------------------------------------------------
nearest_kronecker <- function(W, m) {
  n <- NROW(W) / m
  R <- matrix(0, m * m, n * n)
  for (k in seq_len(m)) {
    for (j in seq_len(m)) {
      R[j + (k - 1) * m, ] <- c(block(W, j, k, n))
    }
  }
  sv <- svd(R, nu = 1, nv = 1)
  V <- matrix(sv$u, m, m)
  Sigma0 <- matrix(sv$v, n, n)
  if (sum(diag(V)) < 0) {
    V <- -V
    Sigma0 <- -Sigma0
  }
  V <- sv$d[1] * V
  list(
    V = V,
    Sigma0 = Sigma0,
    W = kronecker(V, Sigma0),
    rel_error = sqrt(max(0, 1 - sv$d[1]^2 / sum(sv$d^2)))
  )
}

# --------------------------------------------------------------------
# Covariance families for the controlled error experiments.
# All return an (mn x mn) matrix in variable-major order.
# --------------------------------------------------------------------

# F0: separable
w_separable <- function(V, Sigma0) kronecker(V, Sigma0)

# F1: add a component that reconciliation cannot see
w_invisible <- function(W, S, m, K) {
  W + stack_matrix(S, m) %*% K %*% t(stack_matrix(S, m))
}

# F2: variable-specific structure across the hierarchy. Blocks are
# W_jk = R_jk L_j L_k', where Sigma_j = L_j L_j' and R is an m x m
# correlation matrix. Separable when all Sigma_j are equal.
w_variable_specific <- function(Sigma_list, R) {
  L <- lapply(Sigma_list, \(s) t(chol(s)))
  m <- length(L)
  n <- NROW(L[[1]])
  W <- matrix(0, m * n, m * n)
  for (j in seq_len(m)) {
    for (k in seq_len(m)) {
      W[var_index(j, n), var_index(k, n)] <- R[j, k] * L[[j]] %*% t(L[[k]])
    }
  }
  W
}

# F3: cross-variable correlation that varies by series (two variables).
# W_11 = W_22 = Sigma_0 and W_12 = L_0 diag(rho) L_0', where
# Sigma_0 = L_0 L_0'. Separable when rho is constant.
w_node_varying <- function(Sigma0, rho) {
  L0 <- t(chol(Sigma0))
  cross <- L0 %*% diag(rho, length(rho)) %*% t(L0)
  rbind(cbind(Sigma0, cross), cbind(t(cross), Sigma0))
}

# F4: MinT collapses to OLS reconciliation
w_zyskind <- function(S, m, K, tau) {
  S_star <- stack_matrix(S, m)
  S_star %*% K %*% t(S_star) + tau * diag(NROW(S_star))
}

# Random positive definite matrix, for tests and examples
random_pd <- function(n, df = n + 2) {
  X <- matrix(stats::rnorm(n * df), df, n)
  crossprod(X) / df
}

# --------------------------------------------------------------------
# Proposition 3: under node-invariant dynamics, population-optimal
# univariate forecasts are coherent.
#
# Each node i follows the same VAR(1), eta_t = Phi eta_{t-1} + eps_t,
# with Cov(vec(E_t)) = V (x) Sigma. Series (j, a) is s_a' eta_j, which
# has autocovariances (s_a' Sigma s_a) Gamma_h[j, j], where Gamma_h is the
# autocovariance of a VAR(1) with innovation covariance V. The
# autocorrelations, and hence the optimal linear predictor, are the same
# at every node, and applying the same linear filter to every series
# gives coherent forecasts.
# --------------------------------------------------------------------

# Autocovariances Gamma_0, ..., Gamma_lag of a VAR(1)
var1_acov <- function(Phi, V, lag) {
  m <- NROW(Phi)
  G0 <- matrix(solve(diag(m^2) - kronecker(Phi, Phi), c(V)), m, m)
  out <- vector("list", lag + 1)
  out[[1]] <- G0
  for (h in seq_len(lag)) {
    out[[h + 1]] <- Phi %*% out[[h]]
  }
  out
}

# Coefficients of the optimal AR(p) predictor for variable j of a series
# whose autocovariances are scale * Gamma_h[j, j] (Yule-Walker)
population_ar <- function(Phi, V, j, p, scale = 1) {
  g <- scale * vapply(var1_acov(Phi, V, p), \(G) G[j, j], numeric(1))
  solve(stats::toeplitz(g[seq_len(p)]), g[-1])
}

# One-step forecast from AR coefficients a (a[1] multiplies y_{T})
ar_forecast <- function(y, a) sum(a * rev(utils::tail(y, length(a))))
