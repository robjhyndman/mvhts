library(testthat)
library(dplyr)
library(tsibble)
library(fable)
library(tibble)
library(tidyr)

source(here::here("R/helpers.R"))
source(here::here("R/reconcile.R"))
source(here::here("R/simulation_functions.R"))
source(here::here("R/simulation_setup.R"))

# ------------------------------------------------------------
# Helper: small toy dataset (2 series, 3 bottom nodes)
# ------------------------------------------------------------
make_toy_tsibble <- function() {
  times <- yearquarter("2000 Q1") + 0:3
  nodes <- c("Total", "agg_1", "agg_2", "1", "2", "3")
  series <- c("A", "B")

  expand.grid(time = times, node = nodes, series = series) |>
    as_tibble() |>
    arrange(series, node, time) |>
    mutate(value = row_number()) |>
    as_tsibble(index = time, key = c(node, series))
}

# ------------------------------------------------------------
# 1. make_matrix(): column ordering
# ------------------------------------------------------------
test_that("make_matrix column ordering matches expected pattern", {
  x <- make_toy_tsibble()
  M <- make_matrix(x, "value")

  nodes <- order_nodes(unique(x$node))
  series <- sort(unique(x$series))

  expected_names <- paste0(
    rep(series, each = length(nodes)),
    ".",
    rep(nodes, times = length(series))
  )

  expect_equal(colnames(M), expected_names)
})

# ------------------------------------------------------------
# 2. make_array(): dimensions
# ------------------------------------------------------------
test_that("make_array dimensions are correct", {
  x <- make_toy_tsibble()
  A <- make_array(x, "value")

  expect_equal(dim(A)[1], length(unique(x$time)))
  expect_equal(dim(A)[2], length(order_nodes(unique(x$node))))
  expect_equal(dim(A)[3], length(unique(x$series)))
})

# ------------------------------------------------------------
# 3. make_matrix2(): column naming
# ------------------------------------------------------------
test_that("make_matrix2 column names follow series.node format", {
  x <- make_toy_tsibble() |>
    mutate(.id = 1, Região = "R")

  M <- make_matrix2(x, "value")

  expect_true(all(grepl("^[A-Z]\\..+", colnames(M))))
})

# ------------------------------------------------------------
# 4. sim_aggregate(): structure check
# ------------------------------------------------------------
test_that("sim_aggregate preserves one row per (time,node,series)", {
  Y <- sim_mvhts(8, Phi, V_list[[1]], Sigma_list[[1]])
  Y2 <- sim_aggregate(Y)

  df <- as_tibble(Y2)

  expect_false(any(is.na(df$time)))

  expect_equal(
    nrow(df),
    nrow(distinct(df, time, node, series))
  )
})

# ------------------------------------------------------------
# 5. S matrix row alignment sanity
# ------------------------------------------------------------
test_that("S rows match node ordering", {
  x <- make_toy_tsibble()
  nodes <- order_nodes(unique(x$node))

  S_test <- rbind(
    Total = c(1, 1, 1),
    agg_1 = c(1, 1, 0),
    agg_2 = c(0, 0, 1),
    `1` = c(1, 0, 0),
    `2` = c(0, 1, 0),
    `3` = c(0, 0, 1)
  )

  expect_true(setequal(rownames(S_test), nodes))
})

# ------------------------------------------------------------
# 6. Reconciliation coherence
# ------------------------------------------------------------
test_that("mv_reconcile produces coherent forecasts", {
  x <- make_toy_tsibble()

  fit <- x |>
    model(arima = ARIMA(value))

  fc <- forecast(fit, h = 2)

  S_test <- rbind(
    Total = c(1, 1, 1),
    agg_1 = c(1, 1, 0),
    agg_2 = c(0, 0, 1),
    `1` = c(1, 0, 0),
    `2` = c(0, 1, 0),
    `3` = c(0, 0, 1)
  )
  colnames(S_test) <- c("1", "2", "3")

  rec <- mv_reconcile(fit, fc, S_test, time_fn = yearquarter)
  df <- as_tibble(rec)

  check <- df |>
    group_by(time, series) |>
    summarise(
      total = first(.reconciled_mean_cov[node == "Total"]),
      sum_bottom = sum(.reconciled_mean_cov[node %in% c("1", "2", "3")]),
      .groups = "drop"
    )

  expect_true(all(abs(check$total - check$sum_bottom) < 1e-6))
})

# ------------------------------------------------------------
# 7. ytilde_to_tsibble(): structure
# ------------------------------------------------------------
test_that("ytilde_to_tsibble returns valid tsibble", {
  x <- make_toy_tsibble()
  M <- make_matrix(x, "value")

  Ytilde <- t(M)

  out <- ytilde_to_tsibble(Ytilde, x, "recon", time_fn = yearquarter)

  expect_true(is_tsibble(out))
  expect_true(all(c("node", "series", "time") %in% colnames(out)))
})
