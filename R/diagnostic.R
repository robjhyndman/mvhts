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
  error = "normal",
  param = NA,
  reps = 1000,
  alpha = 0.05
) {
  W <- exp1_covariance(S, family, dial)
  C <- make_C(S)
  res <- t(vapply(
    seq_len(reps),
    \(r) {
      e <- sim_errors(T, W, error, param)
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
    error = error,
    param = param,
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

# --------------------------------------------------------------------
# T x NCOL(W) errors with unconditional covariance W (up to scale for
# "t"), for checking the size of cross_test() away from iid normality.
# "normal": iid N(0, W).
# "t": multivariate t with param degrees of freedom (covariance
#   W df / (df - 2); the test is scale-invariant).
# "GARCH": W^(1/2) u_t, with independent GARCH(1,1) u_it of unit
#   unconditional variance, alpha = param and persistence
#   alpha + beta = 0.95.
# "AR": W^(1/2) u_t, with independent AR(1) u_it of coefficient param
#   and unit variance.
# --------------------------------------------------------------------
sim_errors <- function(T, W, error = "normal", param = NA) {
  N <- NCOL(W)
  switch(
    error,
    normal = mvtnorm::rmvnorm(T, sigma = W),
    t = mvtnorm::rmvt(T, sigma = W, df = param),
    GARCH = garch_errors(T, N, alpha = param, beta = 0.95 - param) %*% sqrt_sym(W),
    AR = vapply(
      seq_len(N),
      \(i) sqrt(1 - param^2) * stats::arima.sim(list(ar = param), n = T),
      numeric(T)
    ) %*% sqrt_sym(W),
    stop("Unknown error ", error)
  )
}

# T x N matrix of independent GARCH(1,1) series with normal innovations,
# scaled to unit unconditional variance (omega = 1 - alpha - beta)
garch_errors <- function(T, N, alpha, beta, burn = 200) {
  spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    distribution.model = "norm",
    fixed.pars = list(omega = 1 - alpha - beta, alpha1 = alpha, beta1 = beta)
  )
  path <- rugarch::ugarchpath(spec, n.sim = T, m.sim = N, n.start = burn)
  matrix(rugarch::fitted(path), T, N)
}

# Symmetric square root of a positive definite matrix
sqrt_sym <- function(W) {
  ev <- eigen(W, symmetric = TRUE)
  ev$vectors %*% (sqrt(pmax(ev$values, 0)) * t(ev$vectors))
}

# Scenarios for the size of the test under serially correlated errors:
# the two null designs of Experiment 1 with AR(1) errors (sim_errors())
ar_size_scenarios <- function(phi = c(0.1, 0.2, 0.3)) {
  tidyr::expand_grid(
    dplyr::filter(sample_families(), family %in% c("F0", "F1")),
    hierarchy = c("small", "brazil"),
    T = c(100, 200, 500, 1000),
    phi = phi
  )
}

# --------------------------------------------------------------------
# How much serial correlation in the errors e would distort cross_test().
# The test is built from the sample covariances between u, the residuals
# from regressing variable j's bottom-level errors on its own
# incoherences, and the other variables' incoherences z after the same
# regression. Serial correlation inflates the variance of each such
# covariance by about f = 1 + 2 sum_h rho_u(h) rho_z(h) (Bartlett); f = 1
# when either series is serially uncorrelated. Returns f averaged over
# all (u, z) pairs and all m variables.
# --------------------------------------------------------------------
serial_inflation <- function(e, S, m = 2, lag = 24) {
  C <- make_C(S)
  n <- NROW(S)
  n_a <- NROW(C)
  n_b <- NCOL(S)
  acfs <- \(x) apply(x, 2, \(v) stats::acf(v, lag.max = lag, plot = FALSE)$acf[-1])
  z <- lapply(seq_len(m), \(j) e[, var_index(j, n), drop = FALSE] %*% t(C))
  f <- lapply(seq_len(m), \(j) {
    X_own <- cbind(1, z[[j]])
    Y <- e[, var_index(j, n)[n_a + seq_len(n_b)], drop = FALSE]
    u <- stats::lm.fit(X_own, Y)$residuals
    z_other <- stats::lm.fit(X_own, do.call(cbind, z[-j]))$residuals
    1 + 2 * crossprod(acfs(u), acfs(z_other))
  })
  mean(unlist(f))
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
        # No: this is log|E| - log|E0|, so exp() just gives Wilks' lambda
        # = |E| / |E0| itself, which is what Rao's approximation uses.
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
      V_P <- sum(diag(solve(E0, H)))
      h <- q # rank(H) = min(p, q), so use q itself
      s <- min(p, h)
      mm <- abs(p-h) / 2 - 0.5
      nn <- (nu - p - 1) / 2
      
      df_pillai1 <- s*(2*mm + s + 1)
      df_pillai2 <- s*(2*nn + s + 1)
      F_pillai <- V_P/(s-V_P) * df_pillai2 / df_pillai1
      
      # Lawley-Hotelling's trace:
      # same df1 as Pillai, but a different df2
      df_LH2 <- 2 * (s*nn + 1)
      V_LH <- sum(diag(solve(E, H)))
      F_LH <- V_LH / s * df_LH2 / df_pillai1
      
      # Roy's largest root:
      theta <- max(Re(eigen(solve(E, H))$values))
      # i got some complex eigenvalues and just took the real part
      # didn't think about why this happens yet
      # (solve(E, H) is not symmetric, so eigen() can return complex values
      # from rounding error; the true eigenvalues are real, so Re() is fine.
      # theta is an eigenvalue of E^-1 H, not of E0^-1 H, so it can exceed 1.
      # This F is only an upper bound, so the test is oversized.)
      df_roy1 <- max(p, h)
      df_roy2 <- nu - max(p, h) + h
      F_roy <- theta * df_roy2 / df_roy1
      
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
        p_LH = stats::pf(F_LH, df_pillai1, df_LH2, lower.tail = FALSE),
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
