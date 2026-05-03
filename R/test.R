# Unit tests for ordering consistency in multivariate reconciliation
#
# Save this file as: tests/testthat/test-ordering.R
# Run with: testthat::test_file("tests/testthat/test-ordering.R")
# or from the project root: testthat::test_dir("tests/testthat")

library(testthat)
library(dplyr)
library(tidyr)
library(tsibble)

# These paths assume the test is run from the project root.
# If your project uses an R package structure, you can replace these with devtools::load_all().
source(here::here("R/helpers.R"))
source(here::here("R/reconcile.R"))

test_that("order_nodes returns Total, aggregate nodes, then bottom nodes", {
  nodes <- c("2", "agg_2", "Total", "1", "agg_1", "5", "3", "4")

  expect_equal(
    order_nodes(nodes),
    c("Total", "agg_1", "agg_2", "1", "2", "3", "4", "5")
  )
})

test_that("make_matrix uses series-major vec(Y) ordering", {
  times <- 1:2
  nodes <- c("Total", "agg_1", "agg_2", "1", "2", "3", "4", "5")
  series <- c("A", "B")

  dat <- tidyr::expand_grid(
    time = times,
    node = nodes,
    series = series
  ) |>
    mutate(
      # Unique value encoding: 1000 * series_id + 100 * node_id + time.
      # This makes it easy to detect wrong ordering.
      series_id = match(series, series),
      node_id = match(node, nodes),
      value = 1000 * series_id + 100 * node_id + time
    ) |>
    select(time, node, series, value) |>
    # Scramble the input rows. make_matrix() should still return the canonical order.
    arrange(desc(node), desc(series), desc(time)) |>
    as_tsibble(index = time, key = c(node, series))

  mat <- make_matrix(dat, "value")

  expected_cols <- paste0(
    rep(series, each = length(nodes)),
    ".",
    rep(nodes, times = length(series))
  )

  expect_equal(colnames(mat), expected_cols)
  expect_equal(nrow(mat), length(times))
  expect_equal(ncol(mat), length(nodes) * length(series))

  expected <- matrix(
    NA_real_,
    nrow = length(times),
    ncol = length(expected_cols)
  )
  colnames(expected) <- expected_cols
  rownames(expected) <- as.character(times)

  for (s in series) {
    for (nd in nodes) {
      col <- paste0(s, ".", nd)
      s_id <- match(s, series)
      n_id <- match(nd, nodes)
      expected[, col] <- 1000 * s_id + 100 * n_id + times
    }
  }

  expect_equal(mat, expected, ignore_attr = FALSE)
})

test_that("reorder_cols restores canonical series-major ordering", {
  nodes <- c("Total", "agg_1", "agg_2", "1", "2", "3", "4", "5")
  series <- c("A", "B")

  canonical_cols <- paste0(
    rep(series, each = length(nodes)),
    ".",
    rep(nodes, times = length(series))
  )

  # Deliberately interleave by node, which is a common wrong order:
  # A.Total, B.Total, A.agg_1, B.agg_1, ...
  wrong_cols <- paste0(
    rep(series, times = length(nodes)),
    ".",
    rep(nodes, each = length(series))
  )

  mat <- matrix(seq_along(wrong_cols), nrow = 1)
  colnames(mat) <- wrong_cols

  out <- reorder_cols(mat, nodes, series)

  expect_equal(colnames(out), canonical_cols)

  # Check that values moved with their column names.
  expected_values <- match(canonical_cols, wrong_cols)
  expect_equal(as.numeric(out[1, ]), expected_values)
})

test_that("S_* column and row ordering matches make_matrix ordering", {
  S <- rbind(
    Total = rep(1, 5),
    agg_1 = c(1, 1, 0, 0, 0),
    agg_2 = c(0, 0, 1, 1, 1),
    `1` = c(1, 0, 0, 0, 0),
    `2` = c(0, 1, 0, 0, 0),
    `3` = c(0, 0, 1, 0, 0),
    `4` = c(0, 0, 0, 1, 0),
    `5` = c(0, 0, 0, 0, 1)
  )

  nodes <- rownames(S)
  bottom_nodes <- colnames(S)
  if (is.null(bottom_nodes)) {
    bottom_nodes <- c("1", "2", "3", "4", "5")
    colnames(S) <- bottom_nodes
  }
  series <- c("A", "B")

  S_star <- kronecker(diag(length(series)), S)

  row_labels <- paste0(
    rep(series, each = length(nodes)),
    ".",
    rep(nodes, times = length(series))
  )
  col_labels <- paste0(
    rep(series, each = length(bottom_nodes)),
    ".",
    rep(bottom_nodes, times = length(series))
  )

  rownames(S_star) <- row_labels
  colnames(S_star) <- col_labels

  # A bottom vector in series-major order.
  b <- c(
    A.1 = 11,
    A.2 = 12,
    A.3 = 13,
    A.4 = 14,
    A.5 = 15,
    B.1 = 21,
    B.2 = 22,
    B.3 = 23,
    B.4 = 24,
    B.5 = 25
  )

  y <- as.vector(S_star %*% b)
  names(y) <- row_labels

  expect_equal(unname(y["A.Total"]), sum(b[paste0("A.", 1:5)]))
  expect_equal(unname(y["A.agg_1"]), unname(b["A.1"] + b["A.2"]))
  expect_equal(unname(y["A.agg_2"]), unname(b["A.3"] + b["A.4"] + b["A.5"]))
  expect_equal(unname(y["B.Total"]), sum(b[paste0("B.", 1:5)]))
  expect_equal(unname(y["B.agg_1"]), unname(b["B.1"] + b["B.2"]))
  expect_equal(unname(y["B.agg_2"]), unname(b["B.3"] + b["B.4"] + b["B.5"]))
})

test_that("canonical residual covariance is aligned with forecast matrix columns", {
  times <- 1:4
  nodes <- c("Total", "agg_1", "agg_2", "1", "2", "3", "4", "5")
  series <- c("A", "B")

  canonical_cols <- paste0(
    rep(series, each = length(nodes)),
    ".",
    rep(nodes, times = length(series))
  )

  # Forecast-like object for make_matrix().
  fc <- tidyr::expand_grid(
    time = times,
    node = nodes,
    series = series
  ) |>
    mutate(.mean = row_number()) |>
    as_tsibble(index = time, key = c(node, series))

  Yhat_mat <- make_matrix(fc, ".mean")

  # Residual matrix starts in a deliberately wrong order.
  wrong_cols <- paste0(
    rep(series, times = length(nodes)),
    ".",
    rep(nodes, each = length(series))
  )
  res_wrong <- matrix(
    seq_len(length(times) * length(wrong_cols)),
    nrow = length(times)
  )
  colnames(res_wrong) <- wrong_cols

  res_fixed <- reorder_cols(res_wrong, nodes, series)

  expect_equal(colnames(res_fixed), colnames(Yhat_mat))
  expect_equal(colnames(res_fixed), canonical_cols)
})
