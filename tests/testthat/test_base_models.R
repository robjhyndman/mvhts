# Checks of R/base_models.R: every matrix has its columns in stacked
# order, whatever the model

library(testthat)

test_that("wide_matrix orders columns as requested, whatever the row order", {
  x <- tidyr::expand_grid(
    time = 1:3,
    node = c("Total", "a", "b"),
    series = c("A", "D")
  ) |>
    mutate(value = seq_len(n())) |>
    arrange(desc(node), desc(series), desc(time))
  cols <- c("A.Total", "A.a", "A.b", "D.Total", "D.a", "D.b")
  M <- wide_matrix(x, "value", cols)
  expect_equal(colnames(M), cols)
  expect_equal(
    unname(M[2, "D.a"]),
    x$value[x$time == 2 & x$node == "a" & x$series == "D"]
  )
})

test_that("residuals and forecasts line up with stacked_names for every model", {
  S <- small_hierarchy()
  variables <- c("A", "B")
  cols <- stacked_names(S, variables)
  set.seed(3)
  train <- sim_hierarchy(40, S, rep(list(diag(0.5, 2)), 5), diag(10))
  fits <- fit_base_models(train, variables)
  expect_equal(names(fits), c("arima", "ets", "var"))
  for (name in names(fits)) {
    res <- base_residuals(fits[[name]], name, S, variables)
    fc <- base_forecasts(fits[[name]], name, 3, S, variables)
    expect_equal(colnames(res), cols)
    expect_equal(colnames(fc), cols)
    expect_equal(dim(fc), c(3, length(cols)))
  }
  # VAR: each column holds the forecast of that variable at that node
  fc_var <- fabletools::forecast(fits$var, h = 3)
  x <- fc_var[fc_var$node == "agg_1", ]
  fc <- base_forecasts(fits$var, "var", 3, S, variables)
  expect_equal(unname(fc[, "A.agg_1"]), x$.mean[, 1])
  expect_equal(unname(fc[, "B.agg_1"]), x$.mean[, 2])
  r <- stats::residuals(fits$var)
  r <- r[r$node == "3", ]
  res <- base_residuals(fits$var, "var", S, variables)
  expect_equal(unname(res[, "B.3"]), tail(r$B, NROW(res)))
})
