# ============================
# Multivariate reconciliation
# ============================

mv_reconcile <- function(
  fit,
  fc,
  S,
  cov_fn = sample_cov,
  time_fn = tsibble::yearmonth,
  simulated = TRUE
) {
  # Turn forecasts into matrix
  Yhat <- t(make_matrix(fc, ".mean"))
  m <- length(unique(fc$series))
  # Ensure S is in correct order
  nodes <- order_nodes(unique(fc$node))
  if (is.null(rownames(S)) || is.null(colnames(S))) {
    stop("S must have row names and column names matching forecast nodes.")
  }
  if (!setequal(rownames(S), nodes)) {
    stop("Rows of S do not match forecast nodes.")
  }
  if (!setequal(colnames(S), tail(nodes, NCOL(S)))) {
    stop("Columns of S do not match forecast nodes.")
  }
  # Summing matrix, matching vec(Y_t) = (I_m \otimes S) vec(B_t)
  SI <- kronecker(diag(m), S)
  # Residual matrix
  res <- get_residuals(fit, simulated = simulated)
  series <- sort(unique(fc$series))
  res <- reorder_cols(res, nodes, series)
  # Covariance matrix
  W <- cov_fn(res)
  Winv <- solve(W + diag(1e-8, nrow(W))) # Add small ridge for numerical stability
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
  time_fn = tsibble::yearmonth,
  simulated = TRUE
) {
  ## Filter series (A or B)
  fc <- fc |> filter(series == serie)
  # Turn forecasts into matrix
  Yhat <- t(make_matrix(fc, ".mean"))
  # Residual matrix
  res <- get_residuals(fit, simulated = simulated)
  # Select columns corresponding to serie A or B
  res <- res[, grep(paste0("^", serie, "\\."), colnames(res)), drop = FALSE]
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
  time_fn = tsibble::yearmonth,
  simulated = TRUE
) {
  if (!identical(names(mod_n), names(fc_n))) {
    stop("Model and forecast .id sets do not match.")
  }
  mod_n <- split(mod_tbl, mod_tbl$.id)
  fc_n <- split(fc_tbl, fc_tbl$.id)
  output <- purrr::map2(
    mod_n,
    fc_n,
    ~ rec_fun(
      .x,
      .y,
      S,
      cov_fn = cov_fn,
      time_fn = time_fn,
      simulated = simulated
    )
  ) |>
    bind_rows()
  shrinkage <- if (identical(cov_fn, shrinkage_cov)) "shrinkage" else "sample"
  output |>
    as_tibble() |>
    select(-value) |>
    rename(base = .mean, reconciled = .reconciled_mean_cov) |>
    tidyr::pivot_longer(
      cols = c(base, reconciled),
      names_to = "type",
      values_to = "forecast"
    ) |>
    mutate(
      type = if_else(
        type == "reconciled",
        paste("reconciled", shrinkage, sep = "_"),
        type
      )
    )
}
