# Checks of the hierarchies and the stacked layout in R/hierarchy.R

library(testthat)

test_that("the Brazilian hierarchy has Total, five regions and 27 states", {
  S <- brazil_hierarchy(state_meta, region_meta)
  expect_equal(dim(S), c(33, 27))
  expect_equal(
    rownames(S)[1:6],
    c(
      "Total",
      paste0("agg_", c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul"))
    )
  )
  expect_equal(colnames(S), state_meta$UF)
  expect_equal(unname(colSums(S[2:6, ])), rep(1, 27))
  expect_equal(unname(S["agg_Sul", c("PR", "RS", "SC")]), c(1, 1, 1))
  expect_equal(max(abs(make_C(S) %*% S)), 0)
})

test_that("stacked_names follows the rows of S* = I_m (x) S", {
  S <- small_hierarchy()
  variables <- c("A", "B")
  # Bottom-level values named "variable.node", in stacked order
  b <- stats::setNames(
    c(11:15, 21:25),
    paste0(rep(variables, each = 5), ".", 1:5)
  )
  y <- stats::setNames(
    drop(stack_matrix(S, 2) %*% b),
    stacked_names(S, variables)
  )
  expect_equal(unname(y["A.Total"]), sum(11:15))
  expect_equal(unname(y["A.agg_1"]), 11 + 12)
  expect_equal(unname(y["B.agg_2"]), 23 + 24 + 25)
  expect_equal(unname(y["B.4"]), 24)
})

test_that("aggregate_hierarchy sums the bottom-level series as S does", {
  S <- small_hierarchy()
  bottom <- tidyr::expand_grid(
    time = 1:3,
    node = colnames(S),
    series = c("A", "B")
  ) |>
    mutate(value = rnorm(n()))
  all <- aggregate_hierarchy(bottom, S)
  expect_equal(nrow(all), 3 * NROW(S) * 2)
  Y <- wide_matrix(all, "value", stacked_names(S, c("A", "B")))
  B <- wide_matrix(
    bottom,
    "value",
    paste0(rep(c("A", "B"), each = 5), ".", 1:5)
  )
  expect_equal(unname(Y), unname(B %*% t(stack_matrix(S, 2))))
  expect_error(
    aggregate_hierarchy(filter(bottom, node != "3"), S),
    "Missing: 3"
  )
  expect_error(
    aggregate_hierarchy(mutate(bottom, node = sub("5", "6", node)), S),
    "unknown: 6"
  )
})

test_that("sim_hierarchy gives coherent series with one row per time, node and variable", {
  S <- small_hierarchy()
  set.seed(1)
  Y <- sim_hierarchy(8, S, rep(list(diag(0.5, 2)), 5), diag(10))
  expect_equal(nrow(Y), 8 * NROW(S) * 2)
  expect_equal(nrow(distinct(as_tibble(Y), time, node, series)), nrow(Y))
  M <- wide_matrix(Y, "value", stacked_names(S, c("A", "B")))
  expect_lt(max(abs(M %*% t(stack_matrix(make_C(S), 2)))), 1e-12)
})
