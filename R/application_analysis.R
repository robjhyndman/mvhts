# ====================================================================
# Application: Brazilian admissions and dismissals by state
#
# Rolling-origin evaluation of base forecasts (R/base_models.R) and the
# reconciliation methods of R/reconciliation.R, and the kappa / plug-in
# gain diagnostic on the base forecast errors. Columns of every matrix
# are in the stacked order of R/hierarchy.R: all series of Admissões in
# the row order of S, then all series of Demissões.
# ====================================================================

# --------------------------------------------------------------------
# One forecast origin: fit on data up to `origin`, forecast up to h
# steps (no further than `last`), and reconcile. Returns a list with
#   point: errors by model, method, horizon, series and node;
#   prob:  CRPS of net change for the probabilistic comparison (ARIMA).
# --------------------------------------------------------------------
app_origin <- function(
  emprego,
  S,
  origin,
  h = 12,
  K = 1000,
  last = tsibble::yearmonth("2019 Dec")
) {
  origin <- tsibble::yearmonth(origin)
  train <- emprego |> dplyr::filter(time <= origin)
  test <- emprego |> dplyr::filter(time > origin, time <= last)
  h <- min(h, length(unique(test$time)))
  test <- test |> dplyr::filter(time <= origin + h)
  actual <- wide_matrix(test, "value", stacked_names(S, app_series))
  # Labels for the elements of an h x (2n) error matrix, column by column
  n <- NROW(S)
  labels <- tibble::tibble(
    horizon = rep(seq_len(h), times = 2 * n),
    series = rep(app_series, each = h * n),
    node = rep(rep(rownames(S), times = 2), each = h)
  ) |>
    dplyr::rename(h = horizon)
  fits <- fit_base_models(train, app_series)
  out <- purrr::imap(fits, \(fit, name) {
    res <- base_residuals(fit, name, S, app_series)
    Yhat <- base_forecasts(fit, name, h, S, app_series)
    maps <- reconciliation_maps(res, S)
    point <- purrr::imap(maps, \(M, method) {
      err <- Yhat %*% t(M) - actual
      tibble::tibble(model = name, method = method, labels, error = c(err))
    }) |>
      dplyr::bind_rows()
    prob <- if (name == "arima") {
      app_prob(fit, res, maps, actual, S, origin, K)
    } else {
      NULL
    }
    list(point = point, prob = prob)
  })
  list(
    point = dplyr::bind_rows(purrr::map(out, "point")) |>
      dplyr::mutate(origin = as.character(origin), .before = 1),
    prob = out$arima$prob
  )
}

# Forecast origins: expanding windows from `min_train` months to the
# month before `last`
app_origins <- function(
  emprego,
  min_train = 108,
  last = tsibble::yearmonth("2019 Dec")
) {
  times <- sort(unique(emprego$time[emprego$time <= last]))
  as.character(times[min_train:(length(times) - 1)])
}

# --------------------------------------------------------------------
# Diagnostic on the full-sample residuals of each base model
# --------------------------------------------------------------------
app_diagnostic <- function(emprego, S, last = tsibble::yearmonth("2019 Dec")) {
  train <- emprego |> dplyr::filter(time <= last)
  fits <- fit_base_models(train, app_series, c("arima", "var"))
  C <- make_C(S)
  n <- NROW(S)
  purrr::imap(fits, \(fit, name) {
    res <- base_residuals(fit, name, S, app_series)
    test <- cross_test(res, S)
    W_hat <- shrinkage_cov(res)
    # Plug-in gain for net change (admissions - dismissals) at each node
    net_gain <- vapply(
      seq_len(n),
      \(i) {
        a <- numeric(2 * n)
        a[c(i, n + i)] <- c(1, -1)
        plugin_gain_combination(W_hat, C, 2, a)
      },
      numeric(1)
    )
    level <- node_level(rownames(S))
    tibble::tibble(
      model = name,
      T = NROW(res),
      kappa = kappa_mv(W_hat, C, 2),
      gain = plugin_gain(W_hat, C, 2),
      stat_adm = test$stat[1],
      stat_dis = test$stat[2],
      df1 = test$df1[1],
      df2 = test$df2[1],
      p_adm = test$p_each[1],
      p_dis = test$p_each[2],
      p_value = test$p_value,
      kronecker_error = nearest_kronecker(W_hat, 2)$rel_error,
      incoherence = incoherence_ratio(W_hat, C, 2),
      net_gain_total = mean(net_gain[level == "Total"]),
      net_gain_regions = mean(net_gain[level == "Regions"]),
      net_gain_states = mean(net_gain[level == "States"])
    )
  }) |>
    dplyr::bind_rows()
}
