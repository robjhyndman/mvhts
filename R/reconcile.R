# ============================
# Multivariate reconciliation
# ============================

mv_reconcile <- function(
  fit,
  fc,
  S,
  cov_fn = sample_cov,
  time_fn = tsibble::yearmonth
) {
  # Turn forecasts into matrix
  Yhat <- t(make_matrix(fc, ".mean"))
  m <- length(unique(fc$series))
  # Summing matrix
  SI <- kronecker(S, diag(m))
  # Residual matrix
  res <- get_residuals(fit)
  # Covariance matrix
  W <- cov_fn(res)
  Winv <- solve(W)
  Ytilde <- SI %*% solve(t(SI) %*% Winv %*% SI) %*% t(SI) %*% Winv %*% Yhat
  dimnames(Ytilde) <- dimnames(Yhat)
  ytilde_to_tsibble(Ytilde, fc, ".reconciled_mean_cov", time_fn)
}

# ==========================
# Univariate reconciliation
# ==========================

uv_reconcile <- function(
  fit,
  fc,
  S,
  serie,
  cov_fn = sample_cov,
  time_fn = tsibble::yearmonth
) {
  ## Filter series (A or B)
  fc <- fc |> filter(series == serie)
  # Turn forecasts into matrix
  Yhat <- t(make_matrix(fc, ".mean"))
  # Residual matrix
  res <- get_residuals(fit)
  # Select columns corresponding to serie A or B
  res <- res[, grep(paste0(serie, "$"), colnames(res)), drop = FALSE]
  W <- cov_fn(res)
  Winv <- solve(W)
  # Reconciliation
  Ytilde <- S %*% solve(t(S) %*% Winv %*% S) %*% t(S) %*% Winv %*% Yhat
  dimnames(Ytilde) <- dimnames(Yhat)
  ytilde_to_tsibble(Ytilde, fc, ".reconciled_uni_mean_shrink", time_fn)
}

# ============================================
# Reconcile forecasts for each .id separately
# ============================================

reconcile_over_id <- function(
  mod_tbl,
  fc_tbl,
  S,
  rec_fun,
  cov_fn = sample_cov,
  time_fn = tsibble::yearmonth
) {
  mod_n <- split(mod_tbl, mod_tbl$.id)
  fc_n <- split(fc_tbl, fc_tbl$.id)
  purrr::map2(
    mod_n,
    fc_n,
    ~ rec_fun(.x, .y, S, cov_fn = cov_fn, time_fn = time_fn)
  ) |>
    bind_rows()
}
