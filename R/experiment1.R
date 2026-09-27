# ====================================================================
# Experiment 1: controlled error design
#
# Base forecast errors are simulated directly. For variable j,
#   e_j = S u_j + v_j,
# where S u_j is a coherent component (invisible to reconciliation) and
# v_j is incoherent noise with variances lambda_ij at each series i and
# cross-variable correlation rho_i. With two variables, diagonal
# incoherent noise and a hierarchy with a single total, joint and
# separate reconciliation coincide exactly when rho_i and
# lambda_2i / lambda_1i are both the same at every series, or rho_i = 0
# (condition (e) of the proposition in sections/theory.qmd, which must
# hold for both ordered pairs of variables).
#
# Part (a) computes population quantities exactly. Part (b) estimates
# W from T simulated errors and evaluates the expected loss of the
# estimated maps, tr(M_hat W M_hat'), which needs no test errors.
# ====================================================================

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

# Fixed node pattern in [-1, 1] for node-varying correlations: evenly
# spaced values in a scrambled order, so the pattern is not a level effect
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
  M_joint <- mint_map(W, stack_matrix(C, 2))
  M_sep <- separate_map(W, C, 2)
  M_ols <- ols_map(S, 2)
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
# For each replication, draw T errors from N(0, W), estimate the maps as
# in reconciliation_maps() (R/reconciliation.R), and compute their
# expected loss tr(M W M'). The oracle maps use the true W and are the
# population benchmark.
# --------------------------------------------------------------------
exp1_sample_one <- function(W, S, T) {
  C <- make_C(S)
  e <- mvtnorm::rmvnorm(T, sigma = W)
  W_hat <- shrinkage_cov(e)
  maps <- reconciliation_maps(e, S)
  tibble::tibble(
    kappa_hat = kappa_mv(W_hat, C, 2),
    gain_hat = plugin_gain(W_hat, C, 2),
    kronecker_error_hat = nearest_kronecker(W_hat, 2)$rel_error,
    mse_joint = pop_mse(maps$joint, W),
    mse_separate = pop_mse(maps$separate, W),
    mse_sep_blocks = pop_mse(maps$sep_blocks, W)
  )
}

exp1_sample <- function(S, hierarchy, family, dial, T, reps) {
  W <- exp1_covariance(S, family, dial)
  C <- make_C(S)
  purrr::map(seq_len(reps), \(r) exp1_sample_one(W, S, T)) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      hierarchy = hierarchy,
      family = family,
      dial = dial,
      T = T,
      kappa = kappa_mv(W, C, 2),
      rep = dplyr::row_number(),
      .before = 1
    ) |>
    dplyr::mutate(
      mse_oracle_joint = pop_mse(mint_map(W, stack_matrix(C, 2)), W),
      mse_oracle_sep = pop_mse(separate_map(W, C, 2), W)
    )
}

# Mean expected loss of estimated joint relative to estimated separate
# reconciliation (below 1 favours joint), and the spread of kappa_hat
exp1_sample_summary <- function(exp1_samp) {
  exp1_samp |>
    dplyr::group_by(hierarchy, family, dial, kappa, T) |>
    dplyr::summarise(
      ratio = mean(mse_joint) / mean(mse_separate),
      oracle_ratio = mean(mse_oracle_joint) / mean(mse_oracle_sep),
      se = stats::sd(mse_joint / mse_separate) / sqrt(dplyr::n()),
      kappa_hat = mean(kappa_hat),
      kappa_hat_q10 = stats::quantile(kappa_hat, 0.1),
      kappa_hat_q90 = stats::quantile(kappa_hat, 0.9),
      .groups = "drop"
    )
}

# Covariances for the finite-sample studies (part (b) here, and the
# power study in R/diagnostic.R): a separable control, an invisible
# departure, and three strengths of each visible departure
sample_families <- function() {
  dplyr::bind_rows(
    tibble::tibble(family = "F0", dial = 0.7),
    tibble::tibble(family = "F1", dial = 2),
    tibble::tibble(family = "F2", dial = c(0.25, 0.5, 1)),
    tibble::tibble(family = "F3", dial = c(0.2, 0.4, 0.6))
  )
}

exp1_sample_scenarios <- function() {
  tidyr::expand_grid(
    sample_families(),
    hierarchy = c("small", "brazil"),
    T = c(50, 100, 200, 500, 1000, 2000)
  )
}
