# ====================================================================
# Simulation of hierarchical multivariate time series (Experiment 2)
# ====================================================================

# ====================================================================
# VAR(1) with zero starting value: eta_t = Phi eta_{t-1} + innov_t.
# innov is an n x m matrix; returns an n x m matrix. Identical to
# tsDyn::VAR.sim(Phi, n, include = "none", innov = innov), which was
# used previously (tsDyn was archived from CRAN in August 2026).
# ====================================================================

sim_var1 <- function(Phi, innov) {
  eta <- matrix(0, nrow(innov), ncol(innov))
  prev <- numeric(ncol(innov))
  for (t in seq_len(nrow(innov))) {
    prev <- drop(Phi %*% prev) + innov[t, ]
    eta[t, ] <- prev
  }
  eta
}

# --------------------------------------------------------------------
# Simulate every series of hierarchy S for m variables (named A, B, ...),
# quarterly from 2000 Q1. Bottom-level node i follows a VAR(1) with
# coefficient Phi_list[[i]], plus a quarterly sine wave with a random
# amplitude in [0, 4]. Omega is the (m n_b x m n_b) innovation
# covariance in node-major order (variables vary fastest within each
# node), so Omega = Sigma (x) V gives innovation covariance V between
# the variables and Sigma between the nodes. The aggregates are sums of
# the bottom-level series.
# --------------------------------------------------------------------
sim_hierarchy <- function(len_T, S, Phi_list, Omega) {
  n_b <- NCOL(S)
  m <- NROW(Phi_list[[1]])
  if (length(Phi_list) != n_b || NROW(Omega) != m * n_b) {
    stop("Need one Phi per bottom-level node and Omega of size m n_b.")
  }

  # Innovations with N(0, Omega) distribution, as an m x n_b x T array
  noise <- mvtnorm::rmvnorm(len_T, rep(0, n_b * m), Omega)
  E <- array(t(noise), dim = c(m, n_b, len_T))

  # Bottom-level series
  B <- array(dim = c(m, n_b, len_T))
  for (i in seq(n_b)) {
    B[, i, ] <- t(
      sim_var1(Phi_list[[i]], innov = t(E[, i, ])) +
        runif(1, 0, 4) * sin(2 * pi * seq(len_T) / 4)
    )
  }

  tibble::tibble(
    time = rep(
      tsibble::yearquarter("2000 Q1") + seq_len(len_T) - 1,
      each = m * n_b
    ),
    node = rep(rep(colnames(S), each = m), len_T),
    series = rep(LETTERS[seq_len(m)], n_b * len_T),
    value = as.vector(B)
  ) |>
    aggregate_hierarchy(S) |>
    tsibble::as_tsibble(index = time, key = c(node, series))
}
