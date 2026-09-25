# ====================================================================
# The diagnostic in finite samples
#
# kappa_hat and the plug-in gain are biased in finite samples (upwards by
# sampling noise, downwards by shrinkage), so they are reported as effect
# sizes. Whether joint reconciliation can help at all is tested with a
# likelihood ratio test of condition (d) of the proposition. (Bootstrap
# calibration of kappa_hat was unreliable: any null generated from an
# estimate of W inherits its estimation noise, which dominates when the
# number of series is large relative to T.)
# ====================================================================

# Rejection rate of the likelihood ratio test (cross_test) at level alpha
# for errors drawn from W, with the mean estimated kappa and plug-in gain
kappa_power <- function(
  S,
  hierarchy,
  family,
  dial,
  T,
  reps = 1000,
  alpha = 0.05
) {
  W <- exp1_covariance(S, family, dial)
  C <- make_C(S)
  res <- t(vapply(
    seq_len(reps),
    \(r) {
      e <- mvtnorm::rmvnorm(T, sigma = W)
      W_hat <- shrinkage_cov(e)
      c(
        cross_test(e, S)$p_value,
        kappa_mv(W_hat, C, 2),
        plugin_gain(W_hat, C, 2)
      )
    },
    numeric(3)
  ))
  tibble::tibble(
    hierarchy = hierarchy,
    family = family,
    dial = dial,
    T = T,
    kappa = kappa_mv(W, C, 2),
    gain = plugin_gain(W, C, 2),
    reps = reps,
    rejection = mean(res[, 1] <= alpha),
    rejection_se = sqrt(rejection * (1 - rejection) / reps),
    kappa_hat = mean(res[, 2]),
    gain_hat = mean(res[, 3])
  )
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

# --------------------------------------------------------------------
# Likelihood ratio test of condition (d) of the proposition.
#
# MinT regresses each variable's errors on the incoherences. Given its
# own incoherences z_j = C e_j, variable j's errors are determined by
# their bottom-level part b_j, so condition (d) says that z_k (k != j)
# adds nothing to the regression of b_j on z_j. That is a standard
# multivariate linear hypothesis: in b_j = a + z_j B_j + z_k B_k + u,
# test B_k = 0 with Wilks' lambda and Rao's F approximation. For more than two variables, all other variables'
# incoherences are tested together. The m tests (one per variable) are
# combined with a Bonferroni correction.
# e: T x (m n) matrix of base forecast errors (variable-major columns).
# --------------------------------------------------------------------
cross_test <- function(e, S, m = 2) {
  C <- make_C(S)
  n <- NROW(S)
  n_b <- NCOL(S)
  n_a <- NROW(C)
  T <- NROW(e)
  z <- lapply(seq_len(m), \(j) e[, var_index(j, n), drop = FALSE] %*% t(C))
  tests <- vapply(
    seq_len(m),
    \(j) {
      Y <- e[, var_index(j, n)[n_a + seq_len(n_b)], drop = FALSE]
      X_own <- cbind(1, z[[j]])
      X_full <- cbind(X_own, do.call(cbind, z[-j]))
      E <- crossprod(stats::lm.fit(X_full, Y)$residuals)
      E0 <- crossprod(stats::lm.fit(X_own, Y)$residuals)
      q <- NCOL(X_full) - NCOL(X_own)
      nu <- T - NCOL(X_full)
      lambda <- exp(as.numeric(
        determinant(E)$modulus - determinant(E0)$modulus
      ))
      # Rao's F approximation to Wilks' lambda (p = n_b responses, q
      # hypothesis degrees of freedom, nu error degrees of freedom)
      p <- n_b
      a <- nu - (p - q + 1) / 2
      b <- if (p^2 + q^2 - 5 > 0) sqrt((p^2 * q^2 - 4) / (p^2 + q^2 - 5)) else 1
      df1 <- p * q
      df2 <- a * b - (p * q - 2) / 2
      lb <- lambda^(1 / b)
      stat <- (1 - lb) / lb * df2 / df1
      c(
        stat = stat,
        df1 = df1,
        df2 = df2,
        p = stats::pf(stat, df1, df2, lower.tail = FALSE)
      )
    },
    numeric(4)
  )
  list(
    stat = tests["stat", ],
    df1 = tests["df1", ],
    df2 = tests["df2", ],
    p_each = tests["p", ],
    p_value = min(1, m * min(tests["p", ]))
  )
}
