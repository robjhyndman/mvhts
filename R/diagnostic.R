# ====================================================================
# The kappa diagnostic in finite samples: bootstrap calibration and power
#
# kappa_hat is biased upwards: under an exactly separable truth it is
# far from zero at realistic sample sizes. So the observed value is
# compared with its distribution under a separable null fitted to the
# data, simulated at the same sample size and re-estimated with the same
# covariance estimator.
# ====================================================================

# Symmetrise and clip eigenvalues so a matrix is positive definite
make_pd <- function(X, floor = 1e-8) {
  X <- (X + t(X)) / 2
  e <- eigen(X, symmetric = TRUE)
  vals <- pmax(e$values, floor * max(e$values))
  e$vectors %*% diag(vals) %*% t(e$vectors)
}

# Parametric bootstrap test that joint and separate reconciliation
# coincide. Two statistics are calibrated from the same null draws:
# kappa (how much the joint map leans on other variables' incoherences)
# and the plug-in gain (how much MSE that is expected to save).
# e: T x (m n) matrix of base forecast errors (variable-major columns).
kappa_test <- function(e, S, m = 2, B = 999, estimator = shrinkage_cov) {
  C <- make_C(S)
  T <- NROW(e)
  W_hat <- estimator(e)
  obs <- c(kappa = kappa_mv(W_hat, C, m), gain = plugin_gain(W_hat, C, m))
  W0 <- make_pd(nearest_kronecker(W_hat, m)$W)
  null <- t(vapply(seq_len(B), \(b) {
    W_b <- estimator(mvtnorm::rmvnorm(T, sigma = W0))
    c(kappa = kappa_mv(W_b, C, m), gain = plugin_gain(W_b, C, m))
  }, numeric(2)))
  list(
    kappa = obs[["kappa"]],
    gain = obs[["gain"]],
    null = null,
    p_kappa = (1 + sum(null[, "kappa"] >= obs[["kappa"]])) / (B + 1),
    p_gain = (1 + sum(null[, "gain"] >= obs[["gain"]])) / (B + 1)
  )
}

# Rejection rates of both tests at level alpha for errors drawn from W
kappa_power <- function(S, hierarchy, family, dial, T, reps = 200, B = 199, alpha = 0.05) {
  W <- exp1_covariance(S, family, dial)
  p <- t(vapply(seq_len(reps), \(r) {
    res <- kappa_test(mvtnorm::rmvnorm(T, sigma = W), S, B = B)
    c(res$p_kappa, res$p_gain)
  }, numeric(2)))
  C <- make_C(S)
  tibble::tibble(
    hierarchy = hierarchy,
    family = family,
    dial = dial,
    T = T,
    kappa = kappa_mv(W, C, 2),
    gain = plugin_gain(W, C, 2),
    reps = reps,
    statistic = c("kappa", "gain"),
    rejection = colMeans(p <= alpha)
  ) |>
    dplyr::mutate(rejection_se = sqrt(rejection * (1 - rejection) / reps))
}

# Scenarios for the size and power study: the separable null (size),
# the invisible departure (also kappa = 0), and visible departures
kappa_power_scenarios <- function() {
  tidyr::expand_grid(
    dplyr::bind_rows(
      tibble::tibble(family = "F0", dial = 0.7),
      tibble::tibble(family = "F1", dial = 2),
      tibble::tibble(family = "F2", dial = c(0.25, 0.5, 1)),
      tibble::tibble(family = "F3", dial = c(0.2, 0.4, 0.6))
    ),
    hierarchy = c("small", "brazil"),
    T = c(100, 200, 500, 1000)
  )
}
