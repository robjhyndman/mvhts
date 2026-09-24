# Numerical checks of the results in R/theory.R

library(testthat)
source(here::here("R/theory.R"))
source(here::here("R/simulation_functions.R"))

# Hierarchy used in the simulations: Total, two aggregates, five bottom
S <- rbind(
  rep(1, 5),
  c(1, 1, 0, 0, 0),
  c(0, 0, 1, 1, 1),
  diag(5)
)
C <- make_C(S)
n <- NROW(S)
m <- 2
S_star <- stack_matrix(S, m)
C_star <- stack_matrix(C, m)
tol <- 1e-10

# A base error covariance for one variable: coherent part plus noise
sigma_one <- function() S %*% random_pd(5) %*% t(S) + diag(runif(n, 0.5, 2))

set.seed(2026)

test_that("C annihilates S", {
  expect_lt(max(abs(C %*% S)), tol)
})

test_that("zero-constrained and GLS forms of MinT agree", {
  W <- random_pd(m * n)
  Winv <- solve(W)
  gls <- S_star %*% solve(t(S_star) %*% Winv %*% S_star, t(S_star) %*% Winv)
  expect_lt(max(abs(mint_map(W, C_star) - gls)), tol)
})

test_that("MinT ignores components of the form S* K S*'", {
  W <- random_pd(m * n)
  W2 <- w_invisible(W, S, m, random_pd(m * 5))
  expect_lt(max(abs(mint_map(W, C_star) - mint_map(W2, C_star))), tol)
})

test_that("separable W: joint equals separate for any V", {
  Sigma0 <- sigma_one()
  for (rho in c(-0.95, -0.5, 0, 0.5, 0.95)) {
    W <- w_separable(matrix(c(1, rho, rho, 2), 2), Sigma0)
    expect_lt(max(abs(mint_map(W, C_star) - separate_map(W, C, m))), tol)
    expect_lt(cross_condition(W, C, m), tol)
    expect_lt(kappa_mv(W, C, m), 1e-6)
  }
})

test_that("non-separable but invisible departures: joint equals separate", {
  W <- w_invisible(w_separable(matrix(c(1, 0.7, 0.7, 1), 2), sigma_one()), S, m, random_pd(m * 5))
  expect_gt(nearest_kronecker(W, m)$rel_error, 0.01)
  expect_lt(max(abs(mint_map(W, C_star) - separate_map(W, C, m))), tol)
  expect_lt(kappa_mv(W, C, m), 1e-6)
})

test_that("general W: joint differs from separate, and conditions (a)-(d) agree", {
  W <- random_pd(m * n)
  M <- mint_map(W, C_star)
  G <- G_matrix(W, C_star)
  off_M <- max(abs(block(M, 1, 2, n)), abs(block(M, 2, 1, n)))
  off_G <- max(abs(block(G, 1, 2, n, NROW(C))), abs(block(G, 2, 1, n, NROW(C))))
  expect_gt(max(abs(M - separate_map(W, C, m))), 1e-4)
  expect_gt(off_M, 1e-4)
  expect_gt(off_G, 1e-4)
  expect_gt(cross_condition(W, C, m), 1e-4)
  expect_gt(kappa_mv(W, C, m), 1e-4)
})

test_that("condition (d) is sufficient without separability", {
  # Cross block W_12 = W_11 C' B C W_22 satisfies (d), because
  # M_1 W_11 C' = 0 (and similarly for M_2 W_21 C'), yet W is not separable.
  W11 <- sigma_one()
  W22 <- sigma_one()
  n_a <- NROW(C)
  B <- matrix(rnorm(n_a * n_a, sd = 0.05), n_a)
  W12 <- W11 %*% t(C) %*% B %*% C %*% W22
  W <- rbind(cbind(W11, W12), cbind(t(W12), W22))
  skip_if(min(eigen(W, symmetric = TRUE, only.values = TRUE)$values) <= 0)
  # M_1 W_12 C' = M_1 W_11 C' (...) = 0 since M_1 W_11 C' = 0
  expect_lt(cross_condition(W, C, m), 1e-8)
  expect_lt(max(abs(mint_map(W, C_star) - separate_map(W, C, m))), 1e-8)
  expect_gt(nearest_kronecker(W, m)$rel_error, 0.01)
})

test_that("variable-specific and node-varying families depart from separate", {
  Sig <- sigma_one()
  W2 <- w_variable_specific(list(Sig, sigma_one()), matrix(c(1, 0.8, 0.8, 1), 2))
  expect_gt(kappa_mv(W2, C, m), 1e-3)
  W3 <- w_node_varying(Sig, seq(-0.6, 0.9, length.out = n))
  expect_gt(kappa_mv(W3, C, m), 1e-3)
  # Constant rho, or equal Sigma_j, gives separable W
  expect_lt(kappa_mv(w_node_varying(Sig, rep(0.6, n)), C, m), 1e-6)
  expect_lt(kappa_mv(w_variable_specific(list(Sig, Sig), matrix(c(1, 0.8, 0.8, 1), 2)), C, m), 1e-6)
})

test_that("S* K S*' + tau I gives OLS reconciliation", {
  W <- w_zyskind(S, m, random_pd(m * 5), tau = 0.7)
  ols <- S_star %*% solve(crossprod(S_star), t(S_star))
  expect_lt(max(abs(mint_map(W, C_star) - ols)), tol)
})

test_that("joint MinT has no larger MSE than separate reconciliation", {
  for (i in 1:20) {
    W <- random_pd(m * n)
    expect_lte(
      pop_mse(mint_map(W, C_star), W),
      pop_mse(separate_map(W, C, m), W) + tol
    )
  }
})

test_that("kappa is invariant to the units of each variable when standardised", {
  W <- random_pd(m * n)
  d <- rep(c(1, 1000), each = n)
  W_scaled <- W * outer(d, d)
  expect_equal(kappa_mv(W, C, m), kappa_mv(W_scaled, C, m), tolerance = 1e-8)
  expect_gt(
    abs(kappa_mv(W, C, m, standardise = FALSE) - kappa_mv(W_scaled, C, m, standardise = FALSE)),
    1e-3
  )
})

test_that("nearest_kronecker recovers a separable matrix", {
  V <- matrix(c(2, -0.5, -0.5, 1), 2)
  Sigma0 <- sigma_one()
  nk <- nearest_kronecker(w_separable(V, Sigma0), m)
  expect_lt(nk$rel_error, 1e-6)
  expect_lt(max(abs(nk$W - w_separable(V, Sigma0))), 1e-8)
})

test_that("Proposition 3: node-invariant dynamics give coherent population forecasts", {
  Phi <- matrix(c(0.7, 0.2, 0.2, 0.7), 2)
  V <- matrix(c(1, 0.7, 0.7, 1), 2)
  Sigma <- rbind(
    c(1, 0.7, 0, 0, 0), c(0.7, 1, 0, 0, 0),
    c(0, 0, 1, 0.7, 0.7), c(0, 0, 0.7, 1, 0.7), c(0, 0, 0.7, 0.7, 1)
  )
  p <- 20
  for (j in 1:2) {
    # Optimal AR(p) coefficients are the same at every node
    coefs <- sapply(seq_len(n), \(a) {
      population_ar(Phi, V, j, p, scale = drop(S[a, ] %*% Sigma %*% S[a, ]))
    })
    expect_lt(max(abs(coefs - coefs[, 1])), 1e-12)
  }
  # Apply those filters to a simulated path: the forecasts are coherent
  len <- 500
  E <- mvtnorm::rmvnorm(len, sigma = kronecker(Sigma, V))
  E <- array(t(E), dim = c(m, 5, len))
  B <- sapply(1:5, \(i) sim_var1(Phi, t(E[, i, ])), simplify = "array") # len x m x 5
  fc <- sapply(1:2, \(j) {
    Y <- S %*% t(B[, j, ]) # n x len
    a <- population_ar(Phi, V, j, p)
    apply(Y, 1, ar_forecast, a = a)
  })
  expect_lt(max(abs(C %*% fc)), 1e-10)
})

test_that("plug-in gain is zero exactly when kappa is zero, and positive otherwise", {
  W0 <- w_invisible(w_separable(matrix(c(1, 0.7, 0.7, 1), 2), sigma_one()), S, m, random_pd(m * 5))
  expect_lt(abs(plugin_gain(W0, C, m)), 1e-10)
  W1 <- w_node_varying(sigma_one(), seq(-0.6, 0.9, length.out = n))
  expect_gt(plugin_gain(W1, C, m), 1e-4)
})

test_that("cross_test holds its size under the null and detects strong departures", {
  source(here::here("R/helpers.R"))
  source(here::here("R/experiment1.R"))
  source(here::here("R/diagnostic.R"))
  set.seed(10)
  H <- small_hierarchy()
  p_null <- replicate(300, cross_test(mvtnorm::rmvnorm(200, sigma = exp1_covariance(H, "F1", 2)), H)$p_value)
  expect_lt(abs(mean(p_null <= 0.05) - 0.05), 0.04)
  p_alt <- cross_test(mvtnorm::rmvnorm(500, sigma = exp1_covariance(H, "F3", 0.6)), H)$p_value
  expect_lt(p_alt, 1e-4)
})
