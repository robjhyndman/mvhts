# ====================================================================
# Application: Brazilian admissions and dismissals by state
#
# Rolling-origin evaluation of base forecasts and several reconciliation
# methods, and the kappa / plug-in gain diagnostic on the base forecast
# errors. Columns of every matrix are in the variable-major order of the
# stacked hierarchy: all series of variable 1 (Admissões) in the row
# order of S, then all series of variable 2 (Demissões).
# ====================================================================

app_series <- c("Admissões", "Demissões")

# Column names "series.node" in stacked order
app_cols <- function(S) {
  paste0(rep(app_series, each = NROW(S)), ".", rep(rownames(S), times = 2))
}

# Long data (time, node, series, <variable>) to a time x series matrix
wide_matrix <- function(x, variable, cols) {
  x |>
    tibble::as_tibble() |>
    dplyr::transmute(time, col = paste0(series, ".", node), v = .data[[variable]]) |>
    tidyr::pivot_wider(names_from = col, values_from = v) |>
    dplyr::arrange(time) |>
    dplyr::select(dplyr::all_of(cols)) |>
    as.matrix()
}

# Fit ARIMA to every series and a bivariate VAR to every node
app_fit <- function(train) {
  list(
    arima = fabletools::model(train, arima = fable::ARIMA(value)),
    var = train |>
      tidyr::pivot_wider(names_from = series, values_from = value) |>
      fabletools::model(var = fable::VAR(vars(Admissões, Demissões)))
  )
}

# One-step in-sample residuals, T x 2n, rows with any missing value dropped
app_residuals <- function(fit, name, cols) {
  res <- stats::residuals(fit)
  if (name == "var") {
    res <- res |>
      tidyr::pivot_longer(dplyr::all_of(app_series), names_to = "series", values_to = ".resid")
  }
  stats::na.omit(wide_matrix(res, ".resid", cols))
}

# Point forecasts, h x 2n
app_forecasts <- function(fit, name, h, cols) {
  fc <- fabletools::forecast(fit, h = h)
  if (name == "var") {
    fc <- fc |> tidy_var_forecast(series_names = app_series, extra_keys = "Região")
  }
  wide_matrix(fc, ".mean", cols)
}

# Reconciliation maps from a residual matrix
app_maps <- function(res, S) {
  C <- make_C(S)
  C_star <- stack_matrix(C, 2)
  S_star <- stack_matrix(S, 2)
  n <- NROW(S)
  W_joint <- shrinkage_cov(res)
  W_sep <- matrix(0, 2 * n, 2 * n)
  for (j in 1:2) {
    idx <- var_index(j, n)
    W_sep[idx, idx] <- shrinkage_cov(res[, idx])
  }
  # WLS with structural scaling: variances proportional to the number of
  # bottom-level series in each aggregate
  W_struct <- diag(rep(rowSums(S), 2))
  list(
    base = diag(2 * n),
    ols = S_star %*% solve(crossprod(S_star), t(S_star)),
    wls_struct = mint_map(W_struct, C_star),
    separate = separate_map(W_sep, C, 2),
    sep_blocks = separate_map(W_joint, C, 2),
    joint = mint_map(W_joint, C_star)
  )
}

# --------------------------------------------------------------------
# One forecast origin: fit on data up to `origin`, forecast up to h
# steps (no further than `last`), and reconcile. Returns a list with
#   point: errors by model, method, horizon, series and node;
#   prob:  CRPS of net change for the probabilistic comparison (ARIMA).
# --------------------------------------------------------------------
app_origin <- function(emprego, S, origin, h = 12, K = 1000,
                       last = tsibble::yearmonth("2019 Dec")) {
  cols <- app_cols(S)
  origin <- tsibble::yearmonth(origin)
  train <- emprego |> dplyr::filter(time <= origin)
  test <- emprego |> dplyr::filter(time > origin, time <= last)
  h <- min(h, length(unique(test$time)))
  test <- test |> dplyr::filter(time <= origin + h)
  actual <- wide_matrix(test, "value", cols)
  fits <- app_fit(train)
  horizon <- rep(seq_len(h), times = length(cols))
  series <- rep(sub("\\..*$", "", cols), each = h)
  node <- rep(sub("^[^.]*\\.", "", cols), each = h)
  out <- purrr::imap(fits, \(fit, name) {
    res <- app_residuals(fit, name, cols)
    Yhat <- app_forecasts(fit, name, h, cols)
    maps <- app_maps(res, S)
    point <- purrr::imap(maps, \(M, method) {
      err <- Yhat %*% t(M) - actual
      tibble::tibble(model = name, method = method, h = horizon, series = series, node = node, error = c(err))
    }) |>
      dplyr::bind_rows()
    prob <- if (name == "arima") app_prob(fit, res, maps, actual, S, origin, K) else NULL
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
app_origins <- function(emprego, min_train = 108, last = tsibble::yearmonth("2019 Dec")) {
  times <- sort(unique(emprego$time[emprego$time <= last]))
  as.character(times[min_train:(length(times) - 1)])
}

# --------------------------------------------------------------------
# Diagnostic on the full-sample residuals of each base model
# --------------------------------------------------------------------
app_diagnostic <- function(emprego, S, last = tsibble::yearmonth("2019 Dec"), B = 999) {
  cols <- app_cols(S)
  train <- emprego |> dplyr::filter(time <= last)
  fits <- app_fit(train)
  C <- make_C(S)
  purrr::imap(fits, \(fit, name) {
    res <- app_residuals(fit, name, cols)
    test <- kappa_test(res, S, B = B)
    W_hat <- shrinkage_cov(res)
    tibble::tibble(
      model = name,
      T = NROW(res),
      kappa = test$kappa,
      gain = test$gain,
      p_kappa = test$p_kappa,
      p_gain = test$p_gain,
      kappa_null_median = stats::median(test$null[, "kappa"]),
      kappa_null_q95 = stats::quantile(test$null[, "kappa"], 0.95),
      gain_null_median = stats::median(test$null[, "gain"]),
      gain_null_q95 = stats::quantile(test$null[, "gain"], 0.95),
      kronecker_error = nearest_kronecker(W_hat, 2)$rel_error,
      incoherence = sum(diag(stack_matrix(C, 2) %*% W_hat %*% t(stack_matrix(C, 2)))) / sum(diag(W_hat)),
      null = list(test$null)
    )
  }) |>
    dplyr::bind_rows()
}
