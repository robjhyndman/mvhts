# ====================================================================
# Base forecasting models, shared by Experiment 2 and the application
#
# ARIMA and ETS models are fitted to every series separately; a VAR is
# fitted to the variables jointly at each node. Residuals and point
# forecasts are returned as matrices with one column per series, in the
# stacked order of R/hierarchy.R (stacked_names(S, variables)).
# ====================================================================

# Long data (time, node, series, <variable>) to a time x series matrix
wide_matrix <- function(x, variable, cols) {
  x |>
    tibble::as_tibble() |>
    dplyr::transmute(
      time,
      col = paste0(series, ".", node),
      v = .data[[variable]]
    ) |>
    tidyr::pivot_wider(names_from = col, values_from = v) |>
    dplyr::arrange(time) |>
    dplyr::select(dplyr::all_of(cols)) |>
    as.matrix()
}

# Fit the requested base models to long training data. Returns a named
# list of mables, in the order arima, ets, var.
fit_base_models <- function(
  train,
  variables,
  models = c("arima", "ets", "var")
) {
  fits <- list()
  if ("arima" %in% models) {
    fits$arima <- fabletools::model(train, arima = fable::ARIMA(value))
  }
  if ("ets" %in% models) {
    fits$ets <- fabletools::model(train, ets = fable::ETS(value))
  }
  if ("var" %in% models) {
    # One column per variable, and a VAR(vars(<variables>)) at each node
    fits$var <- train |>
      tidyr::pivot_wider(names_from = series, values_from = value) |>
      fabletools::model(
        var = rlang::inject(fable::VAR(vars(!!!rlang::syms(variables))))
      )
  }
  fits
}

# One-step in-sample residuals, T x (m n), rows with any missing value
# dropped. Response residuals are used for ETS, whose innovation
# residuals are relative errors when the error is multiplicative.
base_residuals <- function(fit, model, S, variables) {
  res <- if (model == "ets") {
    stats::residuals(fit, type = "response")
  } else {
    stats::residuals(fit)
  }
  if (model == "var") {
    # VAR residuals have one column per variable
    res <- res |>
      tidyr::pivot_longer(
        dplyr::all_of(variables),
        names_to = "series",
        values_to = ".resid"
      )
  }
  stats::na.omit(wide_matrix(res, ".resid", stacked_names(S, variables)))
}

# Point forecasts, h x (m n)
base_forecasts <- function(fit, model, h, S, variables) {
  fc <- fabletools::forecast(fit, h = h)
  if (model == "var") {
    # A VAR forecast has one row per node and time, with .mean a matrix
    # whose columns are the variables in the order of the VAR formula
    fc <- tibble::tibble(
      time = rep(fc$time, times = length(variables)),
      node = rep(fc$node, times = length(variables)),
      series = rep(variables, each = NROW(fc)),
      .mean = c(fc$.mean)
    )
  }
  wide_matrix(fc, ".mean", stacked_names(S, variables))
}
