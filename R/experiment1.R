# ====================================================================
# Experiment 1: controlled error design
#
# Base forecast errors are simulated directly. For variable j,
#   e_j = S u_j + v_j,
# where S u_j is a coherent component (invisible to reconciliation) and
# v_j is incoherent noise with variances lambda_ij at each series i and
# cross-variable correlation rho_i. With two variables and diagonal
# incoherent noise, joint and separate reconciliation coincide exactly
# when d_i = rho_i sqrt(lambda_2i / lambda_1i) is the same at every
# series (condition (e) of the proposition in sections/theory.tex).
#
# Part (a) computes population quantities exactly. Part (b) estimates
# W from T simulated errors and evaluates the expected loss of the
# estimated maps, tr(M_hat W M_hat'), which needs no test errors.
# ====================================================================

# Hierarchy used in Experiment 2 and the original simulations
small_hierarchy <- function() {
  S <- rbind(
    rep(1, 5),
    c(1, 1, 0, 0, 0),
    c(0, 0, 1, 1, 1),
    diag(5)
  )
  colnames(S) <- seq(5)
  rownames(S) <- c("Total", "agg_1", "agg_2", colnames(S))
  S
}

# --------------------------------------------------------------------
# Covariance from components (two variables).
# lambda: n x 2 matrix of incoherent variances; rho: length-n vector of
# incoherent cross-variable correlations; U: optional (2 n_b x 2 n_b)
# covariance of the coherent component (u_1', u_2')'.
# --------------------------------------------------------------------
w_components <- function(S, lambda, rho, U = NULL) {
  n <- NROW(S)
  cross <- diag(rho * sqrt(lambda[, 1] * lambda[, 2]), n)
  W <- rbind(
    cbind(diag(lambda[, 1], n), cross),
    cbind(cross, diag(lambda[, 2], n))
  )
  if (!is.null(U)) {
    S_star <- stack_matrix(S, 2)
    W <- W + S_star %*% U %*% t(S_star)
  }
  W
}

# Fixed node pattern in [-1, 1] for node-varying correlations: alternates
# in sign across the series of each level so it is not a level effect
node_pattern <- function(S) {
  z <- seq(-1, 1, length.out = NROW(S))
  z[order(sin(seq_len(NROW(S)) * 2.4))]
}

# --------------------------------------------------------------------
# Scenario grid. Each row is a covariance family and dial value.
#   F0 separable:          equal profiles, constant rho (dial = rho)
#   F1 invisible:          F0 (rho = 0.7) plus a non-separable coherent
#                          component scaled by the dial
#   F2 variable-specific:  lambda_2 = a^(1 + dial), lambda_1 = a, rho = 0.7
#   F3 node-varying:       equal profiles, rho_i = 0.3 + dial * z_i
#   F4 OLS:                lambda = 1 everywhere, rho = 0.7, plus a
#                          coherent component: MinT reduces to OLS
# where a_i is the number of bottom-level series summed by series i.
# --------------------------------------------------------------------
exp1_grid <- function() {
  dplyr::bind_rows(
    tibble::tibble(family = "F0", dial = seq(-0.9, 0.9, by = 0.3)),
    tibble::tibble(family = "F1", dial = seq(0, 4, by = 0.5)),
    tibble::tibble(family = "F2", dial = seq(0, 1.5, by = 0.1)),
    tibble::tibble(family = "F3", dial = seq(0, 0.6, by = 0.05)),
    tibble::tibble(family = "F4", dial = c(0, 1, 2))
  )
}

exp1_covariance <- function(S, family, dial, seed = 1) {
  n <- NROW(S)
  a <- rowSums(S)
  ones <- rep(1, n)
  random_coherent <- function(scale) {
    # Non-separable coherent covariance with a fixed seed, so every dial
    # value uses the same direction
    withr::with_seed(seed, scale * random_pd(2 * NCOL(S)))
  }
  switch(
    family,
    F0 = w_components(S, cbind(a, a), dial * ones),
    F1 = w_components(S, cbind(a, a), 0.7 * ones, U = random_coherent(dial)),
    F2 = w_components(S, cbind(a, a^(1 + dial)), 0.7 * ones),
    F3 = w_components(S, cbind(a, a), 0.3 + dial * node_pattern(S)),
    F4 = w_components(
      S,
      cbind(ones, ones),
      0.7 * ones,
      U = random_coherent(dial)
    ),
    stop("Unknown family ", family)
  )
}

# --------------------------------------------------------------------
# Part (a): population quantities for one covariance
# --------------------------------------------------------------------
exp1_population_one <- function(W, S) {
  C <- make_C(S)
  C_star <- stack_matrix(C, 2)
  M_joint <- mint_map(W, C_star)
  M_sep <- separate_map(W, C, 2)
  S_star <- stack_matrix(S, 2)
  M_ols <- S_star %*% solve(crossprod(S_star), t(S_star))
  mse_joint <- pop_mse(M_joint, W)
  mse_sep <- pop_mse(M_sep, W)
  tibble::tibble(
    kappa = kappa_mv(W, C, 2),
    kronecker_error = nearest_kronecker(W, 2)$rel_error,
    mse_base = sum(diag(W)),
    mse_joint = mse_joint,
    mse_sep = mse_sep,
    mse_ols = pop_mse(M_ols, W),
    gain = 1 - mse_joint / mse_sep,
    joint_vs_ols = max(abs(M_joint - M_ols))
  )
}

exp1_population <- function(hierarchies, grid = exp1_grid()) {
  purrr::imap(hierarchies, \(S, h) {
    grid |>
      dplyr::mutate(
        hierarchy = h,
        res = purrr::map2(family, dial, \(f, d) {
          exp1_population_one(exp1_covariance(S, f, d), S)
        })
      ) |>
      tidyr::unnest(res)
  }) |>
    dplyr::bind_rows()
}

# --------------------------------------------------------------------
# Part (b): finite-sample performance of estimated maps.
# For each replication, draw T errors from N(0, W), estimate W with the
# shrinkage estimator, and compute the expected loss tr(M W M') of:
#   joint:        joint MinT with the shrinkage estimate of W
#   separate:     univariate MinT per variable, each with its own
#                 shrinkage estimate (the usual practice)
#   sep_blocks:   univariate MinT using the diagonal blocks of the joint
#                 shrinkage estimate
#   oracle_joint: joint MinT with the true W (the population benchmark)
# --------------------------------------------------------------------
exp1_sample_one <- function(W, S, T) {
  C <- make_C(S)
  C_star <- stack_matrix(C, 2)
  n <- NROW(S)
  e <- mvtnorm::rmvnorm(T, sigma = W)
  W_joint <- shrinkage_cov(e)
  W_sep <- matrix(0, 2 * n, 2 * n)
  for (j in 1:2) {
    idx <- var_index(j, n)
    W_sep[idx, idx] <- shrinkage_cov(e[, idx])
  }
  tibble::tibble(
    kappa_hat = kappa_mv(W_joint, C, 2),
    gain_hat = plugin_gain(W_joint, C, 2),
    kronecker_error_hat = nearest_kronecker(W_joint, 2)$rel_error,
    mse_joint = pop_mse(mint_map(W_joint, C_star), W),
    mse_separate = pop_mse(separate_map(W_sep, C, 2), W),
    mse_sep_blocks = pop_mse(separate_map(W_joint, C, 2), W),
    mse_oracle_joint = pop_mse(mint_map(W, C_star), W),
    mse_oracle_sep = pop_mse(separate_map(W, C, 2), W)
  )
}

exp1_sample <- function(S, hierarchy, family, dial, T, reps) {
  W <- exp1_covariance(S, family, dial)
  purrr::map(seq_len(reps), \(r) exp1_sample_one(W, S, T)) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      hierarchy = hierarchy,
      family = family,
      dial = dial,
      T = T,
      kappa = kappa_mv(W, make_C(S), 2),
      rep = dplyr::row_number(),
      .before = 1
    )
}

# Scenarios for part (b): a separable control, an invisible departure,
# and three strengths of each visible departure
exp1_sample_scenarios <- function() {
  tidyr::expand_grid(
    dplyr::bind_rows(
      tibble::tibble(family = "F0", dial = 0.7),
      tibble::tibble(family = "F1", dial = 2),
      tibble::tibble(family = "F2", dial = c(0.25, 0.5, 1)),
      tibble::tibble(family = "F3", dial = c(0.2, 0.4, 0.6))
    ),
    hierarchy = c("small", "brazil"),
    T = c(50, 100, 200, 500, 1000, 2000)
  )
}
