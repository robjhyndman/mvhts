# ====================================================================
# Experiment 2: time-series data generating processes with fitted base
# models
#
# Bottom-level series follow VAR(1) processes as in the original
# simulation design (R/simulation_functions.R). Controls have the same
# dynamics at every node, so by Proposition 2 their population base
# forecasts are coherent. The other scenarios break node invariance in a
# way that differs between the variables.
#
# For each scenario we compute exactly the one-step error covariance of
# the population-optimal univariate AR(p) forecasts, and then run a
# finite-sample experiment with ARIMA and VAR base models fitted by
# fable.
# ====================================================================

# Permutation from variable-major to node-major order
node_major <- function(m, n_b) {
  as.vector(t(matrix(seq_len(m * n_b), n_b, m)))
}

# --------------------------------------------------------------------
# Scenarios. Each is a list with Phi_list (one m x m matrix per bottom
# node) and Omega (node-major innovation covariance).
# --------------------------------------------------------------------
exp2_scenarios <- function() {
  n_b <- 5
  Phi0 <- matrix(c(0.7, 0.2, 0.2, 0.7), 2)
  corr <- function(r) matrix(c(1, r, r, 1), 2)
  # Node covariances from the original design
  Sigma_pos <- rbind(
    c(1, 0.7, 0, 0, 0),
    c(0.7, 1, 0, 0, 0),
    c(0, 0, 1, 0.7, 0.7),
    c(0, 0, 0.7, 1, 0.7),
    c(0, 0, 0.7, 0.7, 1)
  )
  Sigma_neg <- rbind(
    c(1, -0.4, 0, 0, 0),
    c(-0.4, 1, 0, 0, 0),
    c(0, 0, 1, -0.4, -0.4),
    c(0, 0, -0.4, 1, -0.4),
    c(0, 0, -0.4, -0.4, 1)
  )
  same_phi <- function(Phi) rep(list(Phi), n_b)

  # N1: innovation correlation between the variables varies by node
  rho <- seq(-0.6, 0.9, length.out = n_b)
  L <- as.matrix(Matrix::bdiag(lapply(rho, \(r) t(chol(corr(r))))))
  Omega_node_corr <- L %*% kronecker(Sigma_pos, diag(2)) %*% t(L)

  # N2: each variable has its own node covariance
  perm <- node_major(2, n_b)
  Omega_var_sigma <- w_variable_specific(list(Sigma_pos, Sigma_neg), corr(0.7))[
    perm,
    perm
  ]

  # N3: dynamics vary by node, in opposite directions for the two variables
  a <- seq(0.2, 0.8, length.out = n_b)
  Phi_node <- lapply(seq_len(n_b), \(i) matrix(c(a[i], 0.2, 0.2, rev(a)[i]), 2))

  list(
    C1_sep_pos = list(
      Phi_list = same_phi(Phi0),
      Omega = kronecker(Sigma_pos, corr(0.7))
    ),
    C2_sep_neg = list(
      Phi_list = same_phi(Phi0),
      Omega = kronecker(Sigma_pos, corr(-0.7))
    ),
    C3_var_dynamics = list(
      Phi_list = same_phi(diag(c(0.9, 0.2))),
      Omega = kronecker(Sigma_pos, corr(0.7))
    ),
    N1_node_corr = list(Phi_list = same_phi(Phi0), Omega = Omega_node_corr),
    N2_var_sigma = list(Phi_list = same_phi(Phi0), Omega = Omega_var_sigma),
    N3_node_dynamics = list(
      Phi_list = Phi_node,
      Omega = kronecker(Sigma_pos, corr(0.7))
    )
  )
}

# --------------------------------------------------------------------
# Exact one-step error covariance of the population-optimal AR(p)
# forecast of every series in the hierarchy.
#
# The bottom-level vector eta_t (node-major) is a VAR(1) with block
# diagonal coefficient Phi and innovation covariance Omega, so
# Gamma(d) = Cov(eta_{t+d}, eta_t) = Phi^d Gamma(0). Series x is
# w_x' eta_t, and its AR(p) forecast error is sum_k c_x[k] x_{t+1-k}
# with c_x = (1, -a_x). Hence
#   Cov(e_x, e_y) = sum_{k,l} c_x[k] c_y[l] w_x' Gamma(l - k) w_y.
# Deterministic components are assumed known and do not enter.
# --------------------------------------------------------------------
exp2_population <- function(scenario, S, p = 20) {
  Phi_list <- scenario$Phi_list
  Omega <- scenario$Omega
  m <- NROW(Phi_list[[1]])
  n_b <- length(Phi_list)
  n <- NROW(S)
  Phi <- as.matrix(Matrix::bdiag(Phi_list))

  # Gamma(0) = sum_k Phi^k Omega Phi'^k
  G0 <- Omega
  term <- Omega
  repeat {
    term <- Phi %*% term %*% t(Phi)
    G0 <- G0 + term
    if (max(abs(term)) < 1e-14 * max(abs(G0))) break
  }
  Gamma <- vector("list", p + 1)
  Gamma[[1]] <- G0
  for (d in seq_len(p)) {
    Gamma[[d + 1]] <- Phi %*% Gamma[[d]]
  }

  # Selection vectors, series in variable-major order (variable j, node a)
  Wsel <- matrix(0, m * n_b, m * n)
  for (j in seq_len(m)) {
    for (a in seq_len(n)) {
      Wsel[(seq_len(n_b) - 1) * m + j, (j - 1) * n + a] <- S[a, ]
    }
  }
  # Projected autocovariances P[[d + 1]] = Wsel' Gamma(d) Wsel
  P <- lapply(Gamma, \(G) t(Wsel) %*% G %*% Wsel)

  # AR(p) coefficients for each series
  coefs <- vapply(
    seq_len(m * n),
    \(x) {
      g <- vapply(P, \(Pd) Pd[x, x], numeric(1))
      c(1, -solve(stats::toeplitz(g[seq_len(p)]), g[-1]))
    },
    numeric(p + 1)
  )

  W <- matrix(0, m * n, m * n)
  for (k in 0:p) {
    for (l in 0:p) {
      d <- l - k
      Pd <- if (d >= 0) P[[d + 1]] else t(P[[1 - d]])
      W <- W + outer(coefs[k + 1, ], coefs[l + 1, ]) * Pd
    }
  }
  W <- (W + t(W)) / 2

  C <- make_C(S)
  C_star <- stack_matrix(C, m)
  incoherence <- sum(diag(C_star %*% W %*% t(C_star))) / sum(diag(W))
  visible <- incoherence > 1e-10
  tibble::tibble(
    incoherence = incoherence,
    kappa = if (visible) kappa_mv(W, C, m) else 0,
    gain = if (visible) {
      1 - pop_mse(mint_map(W, C_star), W) / pop_mse(separate_map(W, C, m), W)
    } else {
      0
    },
    W = list(W)
  )
}

# --------------------------------------------------------------------
# One replication: simulate, fit ARIMA (per series) and VAR (per node),
# forecast h steps, and reconcile jointly and separately with shrinkage
# estimates of W from the one-step residuals. Returns the mean squared
# error over all series by method and horizon, and kappa_hat.
# --------------------------------------------------------------------
exp2_rep <- function(scenario, S, T_train, h = 12) {
  Y <- sim_mvhts_general(T_train + h, scenario$Phi_list, scenario$Omega) |>
    sim_aggregate()
  times <- sort(unique(Y$time))
  train <- Y |> dplyr::filter(time <= times[T_train])
  test <- Y |> dplyr::filter(time > times[T_train])
  nodes <- order_nodes(unique(Y$node))
  series <- sort(unique(Y$series))
  cols <- paste0(
    rep(series, each = length(nodes)),
    ".",
    rep(nodes, times = length(series))
  )

  fits <- list(
    arima = fabletools::model(train, arima = fable::ARIMA(value)),
    var = train |>
      tidyr::pivot_wider(names_from = series, values_from = value) |>
      fabletools::model(var = fable::VAR(vars(A, B)))
  )
  fcs <- list(
    arima = fabletools::forecast(fits$arima, h = h),
    var = fabletools::forecast(fits$var, h = h) |>
      tidy_var_forecast(series_names = c("A", "B"))
  )
  actual <- make_matrix(test, "value")[, cols]
  C <- make_C(S)
  C_star <- stack_matrix(C, 2)
  n <- NROW(S)

  purrr::imap(fits, \(fit, name) {
    res <- reorder_cols(get_residuals(fit, simulated = TRUE), nodes, series)
    Yhat <- make_matrix(fcs[[name]], ".mean")[, cols]
    W_joint <- shrinkage_cov(res)
    W_sep <- matrix(0, 2 * n, 2 * n)
    for (j in 1:2) {
      idx <- var_index(j, n)
      W_sep[idx, idx] <- shrinkage_cov(res[, idx])
    }
    maps <- list(
      base = diag(2 * n),
      joint = mint_map(W_joint, C_star),
      separate = separate_map(W_sep, C, 2),
      sep_blocks = separate_map(W_joint, C, 2)
    )
    purrr::imap(maps, \(M, method) {
      err <- Yhat %*% t(M) - actual
      tibble::tibble(
        model = name,
        method = method,
        h = seq_len(h),
        mse = rowMeans(err^2)
      )
    }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        kappa_hat = kappa_mv(W_joint, C, 2),
        gain_hat = plugin_gain(W_joint, C, 2)
      )
  }) |>
    dplyr::bind_rows()
}

# A batch of replications for one scenario and sample size. Failed
# replications (for example, a model that cannot be fitted) are dropped
# and counted.
exp2_batch <- function(scenario_name, scenarios, S, T_train, reps, batch) {
  out <- purrr::map(seq_len(reps), \(r) {
    tryCatch(
      exp2_rep(scenarios[[scenario_name]], S, T_train) |>
        dplyr::mutate(rep = r),
      error = function(e) NULL
    )
  })
  dplyr::bind_rows(out) |>
    dplyr::mutate(
      scenario = scenario_name,
      T = T_train,
      batch = batch,
      failed = sum(vapply(out, is.null, logical(1))),
      .before = 1
    )
}
