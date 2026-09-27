# Checks of the application helpers in R/application_data.R and
# R/application_prob.R

library(testthat)

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

test_that("the employment data are coherent and match the state file", {
  S <- brazil_hierarchy(state_meta, region_meta)
  emprego <- read_data(here::here("Dados/emprego_uf.csv"), S)
  Y <- wide_matrix(emprego, "value", stacked_names(S, app_series))
  C_star <- stack_matrix(make_C(S), 2)
  expect_equal(max(abs(Y %*% t(C_star))), 0)
  raw <- read.csv(here::here("Dados/emprego_uf.csv"))
  raw <- raw[raw$UF == "SP" & raw$month == "2010-03", ]
  sp <- emprego |> filter(node == "SP", time == yearmonth("2010 Mar"))
  expect_equal(sp$value[sp$series == "Admissões"], raw$admissions)
  expect_equal(sp$value[sp$series == "Demissões"], raw$dismissals)
})
