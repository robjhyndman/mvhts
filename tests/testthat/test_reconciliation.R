# Checks of the estimated reconciliation maps in R/reconciliation.R

library(testthat)

S <- small_hierarchy()
C_star <- stack_matrix(make_C(S), 2)
S_star <- stack_matrix(S, 2)
n <- NROW(S)
set.seed(4)
res <- mvtnorm::rmvnorm(60, sigma = random_pd(2 * n))

test_that("separate_cov is block diagonal with each variable's own estimate", {
  W <- separate_cov(res, 2)
  expect_equal(block(W, 1, 1, n), shrinkage_cov(res[, 1:n]))
  expect_equal(block(W, 2, 2, n), shrinkage_cov(res[, n + 1:n]))
  expect_equal(max(abs(block(W, 1, 2, n))), 0)
})

test_that("every map except base gives coherent forecasts and keeps coherent ones", {
  maps <- reconciliation_maps(res, S)
  expect_equal(
    names(maps),
    c("base", "ols", "wls_struct", "separate", "sep_blocks", "joint")
  )
  for (method in setdiff(names(maps), "base")) {
    expect_lt(max(abs(C_star %*% maps[[method]])), 1e-10)
    expect_lt(max(abs(maps[[method]] %*% S_star - S_star)), 1e-10)
  }
  expect_equal(maps$base, diag(2 * n))
})

test_that("separate maps act on each variable alone; the joint map need not", {
  maps <- reconciliation_maps(res, S)
  for (method in c("separate", "sep_blocks")) {
    expect_equal(max(abs(block(maps[[method]], 1, 2, n))), 0)
  }
  expect_gt(max(abs(block(maps$joint, 1, 2, n))), 1e-6)
})
