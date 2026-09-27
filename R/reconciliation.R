# ====================================================================
# Reconciliation with an estimated error covariance
#
# W is estimated from a T x (m n) matrix of base forecast errors or
# one-step residuals (columns in the stacked order of R/hierarchy.R) and
# plugged into the maps of R/theory.R. A map M turns stacked base
# forecasts yhat into coherent forecasts M yhat.
# ====================================================================

# Sample estimator for covariance matrix
# Computes uncentred second moments as errors should have zero mean
sample_cov <- function(res) {
  e <- stats::na.omit(res)
  crossprod(e) / nrow(e)
}

# Shrinkage estimator for covariance matrix
shrinkage_cov <- function(res) {
  res <- stats::na.omit(res)
  t <- nrow(res)
  # Sample covariance matrix
  covm <- sample_cov(res)
  tar <- diag(diag(covm))
  corm <- cov2cor(covm)
  xs <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
  v <- (1 / (t * (t - 1))) * (crossprod(xs^2) - 1 / t * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- cov2cor(tar)
  d <- (corm - corapn)^2
  denom <- sum(d)
  lambda <- if (denom <= .Machine$double.eps) {
    1
  } else {
    sum(v) / denom
  }
  lambda <- max(min(lambda, 1), 0)
  # Shrinkage estimator
  lambda * tar + (1 - lambda) * covm
}

# Block diagonal estimate: each variable's block is the shrinkage
# estimate from that variable's errors alone, as when the variables are
# reconciled one at a time
separate_cov <- function(res, m) {
  n <- NCOL(res) / m
  W <- matrix(0, m * n, m * n)
  for (j in seq_len(m)) {
    idx <- var_index(j, n)
    W[idx, idx] <- shrinkage_cov(res[, idx])
  }
  W
}

# --------------------------------------------------------------------
# The reconciliation methods compared in the paper:
#   base        no reconciliation
#   ols         OLS
#   wls_struct  WLS with variances proportional to the number of
#               bottom-level series in each aggregate
#   separate    MinT for each variable, each with its own shrinkage
#               estimate (the usual practice)
#   sep_blocks  MinT for each variable, using the diagonal blocks of the
#               joint shrinkage estimate
#   joint       MinT for all variables jointly, with the joint shrinkage
#               estimate
# --------------------------------------------------------------------
reconciliation_maps <- function(res, S, m = 2) {
  C <- make_C(S)
  C_star <- stack_matrix(C, m)
  W_joint <- shrinkage_cov(res)
  W_struct <- diag(rep(rowSums(S), m))
  list(
    base = diag(m * NROW(S)),
    ols = ols_map(S, m),
    wls_struct = mint_map(W_struct, C_star),
    separate = separate_map(separate_cov(res, m), C, m),
    sep_blocks = separate_map(W_joint, C, m),
    joint = mint_map(W_joint, C_star)
  )
}
