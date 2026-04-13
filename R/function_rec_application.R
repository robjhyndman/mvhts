
# ============================================================
# Multivariate reconciliation using the cov estimator for W
# ============================================================

mv_reconcile <- function(fit, fc, S) {
  # Turn forecasts into matrix
  Yhat <- t(make_matrix(fc, ".mean"))
  n <- length(unique(fc$node))
  m <- length(unique(fc$series))

  # Summing matrix
  SI <- kronecker(S, diag(m))
  # Covariance matrix
  W <- est_cov2(fit)
  Winv <- solve(W)
  Ytilde <- SI %*% solve(t(SI) %*% Winv %*% SI) %*% t(SI) %*% Winv %*% Yhat
  dimnames(Ytilde) <- dimnames(Yhat)

  # Now we need to turn Ytilde back into a tsibble object
  out <- t(Ytilde) |>
    as.data.frame() |>
    rownames_to_column("time") |>
    pivot_longer(
      cols = -time,
      names_to = c("node", "series"),
      names_pattern = "(.*)\\.(.*)",
      values_to = ".reconciled_mean_cov"
    ) |>
    mutate(time = as.numeric(time)) |>
    tsibble::as_tsibble(index = time, key = c(node, series))

  out$time <- fc$time

  # Add in anything else from the original fc object
  left_join(fc, out, by = c("time", "node", "series"))
}

# ================================================================
# Multivariate reconciliation using the shrinkage estimator for W
# ================================================================

mv_reconcile_s <- function(fit, fc, S) {
  # Turn forecasts into matrix
  Yhat <- t(make_matrix(fc, ".mean"))
  m <- length(unique(fc$series))

  # Summing matrix
  SI <- kronecker(S, diag(m))

  # Residual matrix
  res <- fit |> residuals()

  if (unique(res$.model) == "var") {
    res <- res |>
      pivot_longer(
        -c(node, .model, time),
        names_to = "series",
        values_to = ".resid",
        cols_vary = "slowest"
      ) |>
      arrange(node) |>
      make_matrix2(".resid")
  } else {
    res <- res |> make_matrix2(".resid")
  }

  t <- nrow(res)

  # Sample covariance matrix
  covm <- crossprod(stats::na.omit(res)) / t

  tar <- diag(apply(res, 2, purrr::compose(crossprod, stats::na.omit)) / t)
  corm <- cov2cor(covm)
  xs <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
  xs <- xs[stats::complete.cases(xs), ]
  v <- (1 / (t * (t - 1))) * (crossprod(xs^2) - 1 / t * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v) / sum(d)
  lambda <- max(min(lambda, 1), 0)

  # Shrinkage estimator
  W <- lambda * tar + (1 - lambda) * covm
  Winv <- solve(W)

  # Reconciliation
  Ytilde <- SI %*% solve(t(SI) %*% Winv %*% SI) %*% t(SI) %*% Winv %*% Yhat
  dimnames(Ytilde) <- dimnames(Yhat)
  # Now we need to turn Ytilde back into a tsibble object
  out <- t(Ytilde) |>
    as.data.frame() |>
    rownames_to_column("time") |>
    pivot_longer(
      cols = -time,
      names_to = c("node", "series"),
      names_pattern = "(.*)\\.(.*)",
      values_to = ".reconciled_mean_shrink"
    ) |>
    mutate(time = as.numeric(time)) |>
    tsibble::as_tsibble(index = time, key = c(node, series))

  out$time <- fc$time

  # Add in anything else from the original fc object
  left_join(fc, out, by = c("time", "node", "series"))
}

reconcile_over_id <- function(mod_tbl, fc_tbl, S, rec_fun) {
  mod_n <- split(mod_tbl, mod_tbl$.id)
  fc_n <- split(fc_tbl, fc_tbl$.id)
  purrr::map2(mod_n, fc_n, ~ rec_fun(.x, .y, S)) |>
    bind_rows()
}
