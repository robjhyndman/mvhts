# ====================================================================
# Application: probabilistic forecasts of net employment change
#
# Net change (admissions - dismissals) is a function of both variables,
# so its forecast distribution depends on how they are modelled jointly.
# We compare three ways of producing coherent sample paths:
#   independent + separate: base innovations for the two variables are
#       resampled independently; each variable reconciled separately
#   joint + separate:      base innovations resampled jointly (whole
#       residual vectors); each variable reconciled separately
#   joint + joint:         joint innovations; joint reconciliation
# Reconciliation is linear, so reconciling sample paths gives the
# reconciled distribution. If the joint and separate maps coincide, the
# last two are identical: the cross-variable information enters through
# the base distribution, not the reconciliation step.
# ====================================================================

# Empirical CRPS of sample x for observation y
crps_sample <- function(y, x) {
  x <- sort(x)
  m <- length(x)
  2 / m^2 * sum((x - y) * (m * (y < x) - seq_len(m) + 0.5))
}

# Sample paths of all series from fitted ARIMA models, driven by
# innovations taken from rows of the residual matrix `res`:
#   path = point forecast + Psi e,
# where Psi holds the psi weights of each model (arima_psi below). This
# is the exact conditional distribution for a linear ARIMA model with
# these innovations. (fabletools::generate() with supplied innovations
# has the same response to innovations but starts from a slightly
# different mean, so it is not used.)
# idx_list gives, for each variable, an (h x K) matrix of residual row
# indices; sharing the matrix keeps the variables' innovations jointly
# distributed. Returns an array h x (2n) x K in stacked column order.
app_paths <- function(fit, res, idx_list, h, K, cols) {
  fc <- app_forecasts(fit, "arima", h, cols)
  keys <- tibble::as_tibble(fit)
  out <- array(NA_real_, dim = c(h, length(cols), K))
  for (k in seq_len(NROW(keys))) {
    c_k <- match(paste0(keys$series[k], ".", keys$node[k]), cols)
    Psi <- psi_matrix(arima_psi(keys$arima[[k]]$fit$model, h))
    E <- matrix(res[c(idx_list[[keys$series[k]]]), c_k], h, K)
    out[, c_k, ] <- fc[, c_k] + Psi %*% E
  }
  out
}

# CRPS of net change at every node and horizon for the three ways of
# producing coherent sample paths, from an ARIMA fit at one origin
app_prob <- function(fit, res, maps, actual, S, origin, K = 1000) {
  cols <- app_cols(S)
  n <- NROW(S)
  h <- NROW(actual)
  # Residual rows for each step and path: shared (joint) or drawn
  # separately for each variable (independent)
  draw <- function() matrix(sample.int(NROW(res), h * K, replace = TRUE), h, K)
  shared <- draw()
  paths <- list(
    joint = app_paths(fit, res, list(Admissões = shared, Demissões = shared), h, K, cols),
    independent = app_paths(fit, res, list(Admissões = shared, Demissões = draw()), h, K, cols)
  )
  combos <- list(
    `independent + separate` = list(paths = "independent", map = "separate"),
    `joint + separate` = list(paths = "joint", map = "separate"),
    `joint + joint` = list(paths = "joint", map = "joint")
  )
  idx_a <- var_index(1, n)
  idx_d <- var_index(2, n)
  net_actual <- actual[, idx_a, drop = FALSE] - actual[, idx_d, drop = FALSE]
  grid <- expand.grid(h = seq_len(h), node = seq_len(n))
  purrr::imap(combos, \(cmb, label) {
    P <- paths[[cmb$paths]]
    M <- maps[[cmb$map]]
    # Reconcile every path: for each step t, (2n x K) -> (2n x K)
    rec <- array(NA_real_, dim(P))
    for (t in seq_len(h)) rec[t, , ] <- M %*% P[t, , ]
    net <- rec[, idx_a, , drop = FALSE] - rec[, idx_d, , drop = FALSE]
    tibble::tibble(
      origin = as.character(origin),
      method = label,
      h = grid$h,
      node = rownames(S)[grid$node],
      crps = mapply(\(t, a) crps_sample(net_actual[t, a], net[t, a, ]), grid$h, grid$node)
    )
  }) |>
    dplyr::bind_rows()
}

# --------------------------------------------------------------------
# Psi weights of a fitted ARIMA model, including differencing:
# y_{T+t} = yhat_{T+t|T} + sum_{j=0}^{t-1} psi_j e_{T+t-j}.
# `model` is the stats::Arima object stored by fable::ARIMA.
# --------------------------------------------------------------------
poly_mult <- function(a, b) {
  out <- numeric(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    idx <- i:(i + length(b) - 1)
    out[idx] <- out[idx] + a[i] * b
  }
  out
}

seasonal_poly <- function(coefs, s) {
  out <- 1
  if (length(coefs)) {
    out <- numeric(s * length(coefs) + 1)
    out[1] <- 1
    out[s * seq_along(coefs) + 1] <- coefs
  }
  out
}

arima_psi <- function(model, h) {
  if (h == 1) {
    return(1)
  }
  arma <- model$arma # p, q, P, Q, s, d, D
  p <- arma[1]; q <- arma[2]; P <- arma[3]; Q <- arma[4]; s <- arma[5]; d <- arma[6]; D <- arma[7]
  cf <- model$coef
  ar <- cf[seq_len(p)]
  ma <- cf[p + seq_len(q)]
  sar <- cf[p + q + seq_len(P)]
  sma <- cf[p + q + P + seq_len(Q)]
  ar_poly <- poly_mult(c(1, -ar), seasonal_poly(-sar, s))
  for (i in seq_len(d)) ar_poly <- poly_mult(ar_poly, c(1, -1))
  for (i in seq_len(D)) ar_poly <- poly_mult(ar_poly, seasonal_poly(-1, s))
  ma_poly <- poly_mult(c(1, ma), seasonal_poly(sma, s))
  c(1, stats::ARMAtoMA(ar = -ar_poly[-1], ma = ma_poly[-1], lag.max = h - 1))[seq_len(h)]
}

# Lower-triangular h x h matrix mapping future innovations to paths
psi_matrix <- function(psi) {
  h <- length(psi)
  Psi <- matrix(0, h, h)
  for (t in seq_len(h)) Psi[t, seq_len(t)] <- rev(psi[seq_len(t)])
  Psi
}
