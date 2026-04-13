simulacao <- function(Phi, V, Sigma, S, n_sim) {
  # Data frame that will store all reconciled forecasts
  fc_sim <- data.frame()

  # List to store all simulated series for each repetition
  Y_sim <- list()

  # Counter of valid simulations (only counts when no error occurs)
  i <- 0

  # Loop that runs until n_sim valid simulations are obtained
  while (i != n_sim) {
    # ============================================================
    # 1) Generation of the simulated series only at the bottom level
    # ============================================================
    Y <- sim_mvhts(120, Phi, V, Sigma)

    # ============================================================
    # 2) Aggregate the bottom-level series
    # ============================================================
    Y2 <- bind_rows(
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

    # ============================================================
    # 3) Fit models up to 2026 Q4 (training data)
    # ============================================================

    # ARIMA model
    fit_arima <- Y2 |>
      filter_index(~"2026 Q4") |>
      model(arima = ARIMA(value))

    # ETS model
    fit_ets <- Y2 |>
      filter_index(~"2026 Q4") |>
      model(ets = ETS(value))

    # VAR must be in wide format (one column per series)
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
    fc2_arima <- try(mv_reconcile_cov(fit_arima, fc_arima, S), silent = TRUE)
    fc2_ets <- try(mv_reconcile_cov(fit_ets, fc_ets, S), silent = TRUE)
    fc2_var <- try(mv_reconcile_cov(fit_var, fc_var, S), silent = TRUE)

    # Using shrinkage estimator for W
    fc3_arima <- try(mv_reconcile_shrink(fit_arima, fc_arima, S), silent = TRUE)
    fc3_ets <- try(mv_reconcile_shrink(fit_ets, fc_ets, S), silent = TRUE)
    fc3_var <- try(mv_reconcile_shrink(fit_var, fc_var, S), silent = TRUE)

    # ============================================================
    # 7) Univariate shrinkage reconciliation for each series and model
    # ============================================================
    fc_uni <- bind_rows(
      # ARIMA model
      data.frame(reconcile_shrink(fit_arima, fc_arima, S, "A")) |>
        select(-value),
      data.frame(reconcile_shrink(fit_arima, fc_arima, S, "B")) |>
        select(-value),

      # ETS model
      data.frame(reconcile_shrink(fit_ets, fc_ets, S, "A")) |> select(-value),
      data.frame(reconcile_shrink(fit_ets, fc_ets, S, "B")) |> select(-value),

      # VAR model
      data.frame(reconcile_shrink(fit_var, fc_var, S, "A")) |> select(-value),
      data.frame(reconcile_shrink(fit_var, fc_var, S, "B")) |> select(-value)
    )

    # ============================================================
    # 8) If any reconciliation fails, discard the simulation
    # ============================================================
    if (
      inherits(fc2_arima, "try-error") ||
        inherits(fc2_ets, "try-error") ||
        inherits(fc2_var, "try-error")
    ) {
      # Do not increment the counter
      i <- i
      print(i)
    } else {
      # Count as a valid simulation
      i <- i + 1

      # ============================================================
      # 9) Combine multivariate reconciliations (shrinkage and cov) in fc2
      # ============================================================
      fc2_arima$.reconciled_mean_shrink <- fc3_arima$.reconciled_mean_shrink
      fc2_ets$.reconciled_mean_shrink <- fc3_ets$.reconciled_mean_shrink
      fc2_var$.reconciled_mean_shrink <- fc3_var$.reconciled_mean_shrink

      # ============================================================
      # 10) Add the true future values to fc2
      # ============================================================
      fc2_arima <- bind_cols(
        fc2_arima,
        (Y2 |> filter_index("2027 Q1" ~ "2029 Q4") |> mutate(Y = value))[, 5]
      )

      fc2_ets <- bind_cols(
        fc2_ets,
        (Y2 |> filter_index("2027 Q1" ~ "2029 Q4") |> mutate(Y = value))[, 5]
      )

      fc2_var <- bind_cols(
        fc2_var,
        (Y2 |> filter_index("2027 Q1" ~ "2029 Q4") |> mutate(Y = value))[, 5]
      )

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
      fc_sim <- bind_rows(fc_sim, fc)

      # Store the complete simulated series
      Y2$simulacao <- i
      print(i)
      print("#####################################")
    }

    # Store the simulated series in the list
    Y_sim[[i]] <- data.frame(Y2)
  }

  # ============================================================
  # 12) Final return
  # ============================================================
  return(list(
    fc_sim = fc_sim,
    Y_sim = bind_rows(Y_sim)
  ))
}
