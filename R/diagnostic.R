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
      ct <- cross_test(e, S)
      c(
        ct$p_value_rao,
        ct$p_value_pillai,
        ct$p_value_LH,
        ct$p_value_roy,
        kappa_mv(W_hat, C, 2),
        plugin_gain(W_hat, C, 2)
      )
    },
    numeric(6)
  ))
  tibble::tibble(
    hierarchy = hierarchy,
    family = family,
    dial = dial,
    T = T,
    kappa = kappa_mv(W, C, 2),
    gain = plugin_gain(W, C, 2),
    reps = reps,
    rejection_rao = mean(res[, 1] <= alpha),
    rejection_rao_se = sqrt(rejection_rao * (1 - rejection_rao) / reps),
    rejection_pillai = mean(res[, 2] <= alpha),
    rejection_pillai_se = sqrt(rejection_pillai * (1 - rejection_pillai) / reps),
    rejection_LH = mean(res[, 3] <= alpha),
    rejection_LH_se = sqrt(rejection_LH * (1 - rejection_LH) / reps),
    rejection_roy = mean(res[, 4] <= alpha),
    rejection_roy_se = sqrt(rejection_roy * (1 - rejection_roy) / reps),
    kappa_hat = mean(res[, 5]),
    gain_hat = mean(res[, 6])
  )
}

# Scenarios for the size and power study: the separable null (size),
# the invisible departure (also kappa = 0), and visible departures
kappa_power_scenarios <- function() {
  tidyr::expand_grid(
    sample_families(),
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
      H <- E0 - E
      q <- NCOL(X_full) - NCOL(X_own)
      nu <- T - NCOL(X_full)
      lambda <- exp(as.numeric(
        determinant(E)$modulus - determinant(E0)$modulus
        # exp() transforms the test statistic, but do we need to transform
        # the f-distribution as well?
      ))
      # Rao's F approximation to Wilks' lambda (p = n_b responses, q
      # hypothesis degrees of freedom, nu error degrees of freedom)
      p <- n_b
      a <- nu - (p - q + 1) / 2
      b <- if (p^2 + q^2 - 5 > 0) sqrt((p^2 * q^2 - 4) / (p^2 + q^2 - 5)) else 1
      df_rao1 <- p * q
      df_rao2 <- a * b - (p * q - 2) / 2
      lb <- lambda^(1 / b)
      stat_rao <- (1 - lb) / lb * df_rao2 / df_rao1
      
      # Pillai's trace:
      V_P <- tr(solve(E0, H))
      h <- qr(H)$rank # should be the same as q
      s <- min(p, h)
      mm <- abs(p-h) / 2 - 0.5
      
      df_pillai1 <- s*(2 + s + mm)
      df_pillai2 <- (nu + mm + 0.5) * (2 + s + mm) - s * (2 + s + mm)
      F_pillai <- V_P/(1-V_P) * df_pillai2 / df_pillai1
      
      # Lawley-Hotelling's trace:
      # same degrees of freedom as Pillai
      V_LH <- tr(solve(E, H))
      F_LH <- V_LH / s * df_pillai2 / df_pillai1
      
      # Roy's largest root:
      theta <- max(Re(eigen(solve(E, H))$values))
      # i got some complex eigenvalues and just took the real part
      # didn't think about why this happens yet
      df_roy1 <- 2 * s
      df_roy2 <- 2 * max(p, h) - s - 0.5
      F_roy <- theta / (1-theta) * df_roy2 / df_roy1
      
      c(
        stat_rao = stat_rao,
        F_pillai = F_pillai,
        F_LH = F_LH,
        F_roy = F_roy,
        df_rao1 = df_rao1,
        df_rao2 = df_rao2,
        df_pillai1 = df_pillai1,
        df_pillai2 = df_pillai2,
        df_roy1 = df_roy1,
        df_roy2 = df_roy2, 
        p_rao = stats::pf(stat_rao, df_rao1, df_rao2, lower.tail = FALSE),
        p_pillai = stats::pf(F_pillai, df_pillai1, df_pillai2, lower.tail = FALSE),
        p_LH = stats::pf(F_LH, df_pillai1, df_pillai2, lower.tail = FALSE),
        p_roy = stats::pf(F_roy, df_roy1, df_roy2, lower.tail = FALSE)
      )
    },
    numeric(14)
  )
  list(
    stat_rao = tests["stat_rao", ],
    df_rao1 = tests["df_rao1", ],
    df_rao2 = tests["df_rao2", ],
    p_each_rao = tests["p_rao", ],
    p_value_rao = min(1, m * min(tests["p_rao", ])),
    F_pillai = tests["F_pillai", ],
    df_pillai1 = tests["df_pillai1", ],
    df_pillai2 = tests["df_pillai2", ],
    p_each_pillai = tests["p_pillai", ],
    p_value_pillai = min(1, m * min(tests["p_pillai", ])),
    F_LH = tests["F_LH", ],
    p_each_LH = tests["p_LH", ],
    p_value_LH = min(1, m * min(tests["p_LH", ])),
    F_roy = tests["F_roy", ],
    df_roy1 = tests["df_roy1", ],
    df_roy2 = tests["df_roy2", ],
    p_each_roy = tests["p_roy", ],
    p_value_roy = min(1, m * min(tests["p_roy", ]))
  )
}
