# Checks of the application helpers in R/application_analysis.R and
# R/application_prob.R

library(testthat)
suppressPackageStartupMessages({
  library(dplyr)
  library(tsibble)
  library(fable)
})
source(here::here("R/application_analysis.R"))
source(here::here("R/application_prob.R"))

test_that("crps_sample matches the direct definition", {
  set.seed(1)
  x <- rnorm(300)
  for (y in c(-2, 0.3, 5)) {
    direct <- mean(abs(x - y)) - 0.5 * mean(abs(outer(x, x, "-")))
    expect_equal(crps_sample(y, x), direct, tolerance = 1e-10)
  }
})

test_that("psi weights reproduce fable's response to future innovations", {
  set.seed(2)
  x <- tsibble(
    time = yearmonth("2000 Jan") + 0:143,
    value = 100 + cumsum(rnorm(144)) + 5 * sin(2 * pi * (1:144) / 12),
    index = time
  )
  h <- 12
  specs <- list(
    ARIMA(value ~ 0 + pdq(1, 1, 1) + PDQ(0, 1, 1)),
    ARIMA(value ~ 0 + pdq(2, 1, 0) + PDQ(1, 0, 0)),
    ARIMA(value ~ 0 + pdq(0, 1, 2) + PDQ(1, 1, 1))
  )
  for (spec in specs) {
    fit <- model(x, m = spec)
    e <- rnorm(h)
    gen <- function(innov) generate(fit, new_data = new_data(x, h) |> mutate(.innov = innov))$.sim
    Psi <- psi_matrix(arima_psi(fit$m[[1]]$fit$model, h))
    expect_lt(max(abs(gen(e) - gen(rep(0, h)) - Psi %*% e)), 1e-8)
  }
  # A single step has psi_0 = 1
  expect_equal(arima_psi(fit$m[[1]]$fit$model, 1), 1)
})

test_that("wide_matrix orders columns as requested", {
  x <- tidyr::expand_grid(time = 1:3, node = c("Total", "a", "b"), series = c("A", "D")) |>
    mutate(value = seq_len(n()))
  cols <- c("A.Total", "A.a", "A.b", "D.Total", "D.a", "D.b")
  M <- wide_matrix(x, "value", cols)
  expect_equal(colnames(M), cols)
  expect_equal(unname(M[2, "D.a"]), x$value[x$time == 2 & x$node == "a" & x$series == "D"])
})
