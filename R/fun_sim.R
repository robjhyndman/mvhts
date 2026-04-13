# ====================================================================
# Functions need for simulations
# ====================================================================

simulacao <- function(Phi, V, Sigma, S, n_sim) {
  # Data frame that will store all reconciled forecasts
  fc_list <- list()
  # List to store all simulated series for each repetition
  Y_sim <- list()
  # Counter of valid simulations (only counts when no error occurs)
  i <- 0

  # Loop that runs until n_sim valid simulations are obtained
  while (i < n_sim) {
    # ============================================================
    # 1) Generation of the simulated series only at the bottom level
    # ============================================================
    Y <- sim_mvhts(120, Phi, V, Sigma)

    # ============================================================
    # 2) Aggregate the bottom-level series
    # ============================================================
    Y2 <- sim_aggregate(Y)

    # ============================================================
    # 3) Fit models up to 2026 Q4 (training data)
    # ============================================================
    fit_arima <- Y2 |> filter_index(~"2026 Q4") |> model(arima = ARIMA(value))
    fit_ets <- Y2 |> filter_index(~"2026 Q4") |> model(ets = ETS(value))
    fit_var <- Y2 |>
      pivot_wider(names_from = series, values_from = value) |>
      filter_index(~"2026 Q4") |>
      model(var = VAR(vars(A, B)))

    # ============================================================
    # 4) 12-step-ahead forecasts
    # ============================================================
    fc_arima <- forecast(fit_arima, h = 12)
    fc_ets <- forecast(fit_ets, h = 12)
    fc_var <- forecast(fit_var, h = 12)

    # ============================================================
    # 5) Process VAR output to return to long tsibble format
    # ============================================================
    fc_var <- fc_var |>
      mutate(.mean = as.data.frame(.mean)) |>
      unnest_wider(.mean, names_sep = "_") |>
      rename(A = .mean_V1, B = .mean_V2, value = .distribution) |>
      pivot_longer(
        -c(node, .model, time, value),
        names_to = "series",
        values_to = ".mean",
        cols_vary = "slowest"
      ) |>
      arrange(node) |>
      as_tsibble(index = time, key = c(node, series))

    # ============================================================
    # 6) Multivariate reconciliations (covariance and shrinkage)
    # ============================================================
    # Using covariance estimator for W
    fc2_arima <- try(
      mv_reconcile(fit_arima, fc_arima, S, time_fn = tsibble::yearquarter),
      silent = TRUE
    )
    fc2_ets <- try(
      mv_reconcile(fit_ets, fc_ets, S, time_fn = tsibble::yearquarter),
      silent = TRUE
    )
    fc2_var <- try(
      mv_reconcile(fit_var, fc_var, S, time_fn = tsibble::yearquarter),
      silent = TRUE
    )
    # Using shrinkage estimator for W
    fc3_arima <- try(
      mv_reconcile(
        fit_arima,
        fc_arima,
        S,
        cov_fn = shrinkage_cov,
        time_fn = tsibble::yearquarter
      ),
      silent = TRUE
    )
    fc3_ets <- try(
      mv_reconcile(
        fit_ets,
        fc_ets,
        S,
        cov_fn = shrinkage_cov,
        time_fn = tsibble::yearquarter
      ),
      silent = TRUE
    )
    fc3_var <- try(
      mv_reconcile(
        fit_var,
        fc_var,
        S,
        cov_fn = shrinkage_cov,
        time_fn = tsibble::yearquarter
      ),
      silent = TRUE
    )

    # ============================================================
    # 7) Univariate shrinkage reconciliation for each series and model
    # ============================================================
    fc_uni <- bind_rows(
      # ARIMA model
      data.frame(uv_reconcile(
        fit_arima,
        fc_arima,
        S,
        "A",
        cov_fn = shrinkage_cov,
        time_fn = tsibble::yearquarter
      )) |>
        select(-value),
      data.frame(uv_reconcile(
        fit_arima,
        fc_arima,
        S,
        "B",
        cov_fn = shrinkage_cov,
        time_fn = tsibble::yearquarter
      )) |>
        select(-value),

      # ETS model
      data.frame(uv_reconcile(
        fit_ets,
        fc_ets,
        S,
        "A",
        cov_fn = shrinkage_cov,
        time_fn = tsibble::yearquarter
      )) |>
        select(-value),
      data.frame(uv_reconcile(
        fit_ets,
        fc_ets,
        S,
        "B",
        cov_fn = shrinkage_cov,
        time_fn = tsibble::yearquarter
      )) |>
        select(-value),

      # VAR model
      data.frame(uv_reconcile(
        fit_var,
        fc_var,
        S,
        "A",
        cov_fn = shrinkage_cov,
        time_fn = tsibble::yearquarter
      )) |>
        select(-value),
      data.frame(uv_reconcile(
        fit_var,
        fc_var,
        S,
        "B",
        cov_fn = shrinkage_cov,
        time_fn = tsibble::yearquarter
      )) |>
        select(-value)
    )

    # ============================================================
    # 8) If any reconciliation fails, discard the simulation
    # ============================================================
    if (
      inherits(fc2_arima, "try-error") ||
        inherits(fc2_ets, "try-error") ||
        inherits(fc2_var, "try-error")
    ) {
      print(i)
    } else {
      # Count as a valid simulation
      i <- i + 1

      # ============================================================
      # 9) Combine multivariate reconciliations (shrinkage and cov) in fc2
      # ============================================================
      fc2_arima$.reconciled_mean_shrink <- fc3_arima$.reconciled_mean_cov
      fc2_ets$.reconciled_mean_shrink <- fc3_ets$.reconciled_mean_cov
      fc2_var$.reconciled_mean_shrink <- fc3_var$.reconciled_mean_cov

      # ============================================================
      # 10) Add the true future values to fc2
      # ============================================================
      fc2_arima <- fc2_arima |>
        left_join(Y2 |> rename(Y = value), by = c("time", "node", "series"))
      fc2_ets <- fc2_ets |>
        left_join(Y2 |> rename(Y = value), by = c("time", "node", "series"))
      fc2_var <- fc2_var |>
        left_join(Y2 |> rename(Y = value), by = c("time", "node", "series"))

      # Identify which simulation generated the results
      fc2_arima$simulacao <- i
      fc2_ets$simulacao <- i
      fc2_var$simulacao <- i

      # ============================================================
      # 11) Combine ARIMA, ETS and VAR results
      # ============================================================
      fc <- bind_rows(
        data.frame(fc2_arima) |> select(-value),
        data.frame(fc2_ets) |> select(-value),
        data.frame(fc2_var) |> select(-value)
      )

      # Join with univariate reconciliation
      fc <- left_join(
        fc,
        fc_uni,
        by = c("time", "node", "series", ".model", ".mean")
      )

      # Accumulate all forecasts
      fc_list[[i]] <- fc

      # Store the complete simulated series
      Y2$simulacao <- i
      Y_sim[[i]] <- data.frame(Y2)
      print(i)
      print("#####################################")
    }
  }

  # ============================================================
  # 12) Final return
  # ============================================================
  return(list(
    fc_sim = bind_rows(fc_list),
    Y_sim = bind_rows(Y_sim)
  ))
}

# ============================================================
# Aggregate the bottom-level series to create the hierarchy structure
# ============================================================

sim_aggregate <- function(Y) {
  bind_rows(
    # Original bottom series
    Y,

    # First aggregated level: sum of nodes 1 and 2
    Y |>
      filter(node %in% c(1, 2)) |>
      group_by(series) |>
      summarise(value = sum(value)) |>
      mutate(node = "agg_1"),

    # Second aggregated level: sum of nodes 3, 4 and 5
    Y |>
      filter(node %in% c(3, 4, 5)) |>
      group_by(series) |>
      summarise(value = sum(value)) |>
      mutate(node = "agg_2")
  )
}

# ============================================================
# Simulation of multivariate series at the bottom level
# ============================================================

sim_mvhts <- function(T, Phi, V, Sigma) {
  # Get dimensions
  m <- NROW(V)
  n_b <- NROW(Sigma)
  if (NROW(Phi) != m) {
    stop("Phi must have the same number of rows as V")
  }

  # Covariance matrix for the whole system
  W <- kronecker(Sigma, V)

  # Set up space for storing the simulation
  B <- array(dim = c(m, n_b, T))

  # Generate noise with N(0,W) distribution
  # E[t,,] contains E_t
  noise <- mvtnorm::rmvnorm(T, rep(0, n_b * m), W)
  E <- array(noise, dim = c(m, n_b, T))

  # Generate bottom level series
  for (i in seq(n_b)) {
    B[, i, ] <- t(
      tsDyn::VAR.sim(B = Phi, n = T, include = "none", innov = t(E[, i, ])) +
        runif(1, 0, 4) * sin(2 * pi * seq(T) / 4)
    )
  }

  A <- apply(B, c(1, 3), sum)
  Y <- array(dim = c(m, n_b + 1, T))
  Y[, 1, ] <- A
  Y[, -1, ] <- B

  # Return as a tsibble object
  tibble::tibble(
    time = make_yearquarter(
      year = rep(2000:(2000 + T / 4 - 1), each = 4 * (m * (n_b + 1))),
      quarter = rep(rep(1:4, each = m * (n_b + 1)), T / 4)
    ),
    node = rep(rep(c("Total", seq(n_b)), each = m), T),
    series = rep(LETTERS[seq(m)], T * (n_b + 1)),
    value = as.vector(Y)
  ) |>
    tsibble::as_tsibble(index = time, key = c(node, series))
}
