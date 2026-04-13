# ============================================================
# Simulation of multivariate series at the bottom level
# ============================================================

sim_mvhts <- function(T, Phi, V, Sigma) {
  # Get dimensions
  m <- NROW(V)
  n_b <- NROW(Sigma)
  if (NROW(Phi) != m) {
    stop("Phi must have the same number of rows as V")
  }

  # Covariance matrix for the whole system
  W <- kronecker(Sigma, V)

  # Set up space for storing the simulation
  B <- array(dim = c(m, n_b, T))

  # Generate noise with N(0,W) distribution
  # E[t,,] contains E_t
  noise <- mvtnorm::rmvnorm(T, rep(0, n_b * m), W)
  E <- array(noise, dim = c(m, n_b, T))

  # Generate bottom level series
  for (i in seq(n_b)) {
    B[, i, ] <- t(
      tsDyn::VAR.sim(B = Phi, n = T, include = "none", innov = t(E[, i, ])) +
        runif(1, 0, 4) * sin(2 * pi * seq(T) / 4)
    )
  }

  A <- apply(B, c(1, 3), sum)
  Y <- array(dim = c(m, n_b + 1, T))
  Y[, 1, ] <- A
  Y[, -1, ] <- B

  # Return as a tsibble object
  tibble::tibble(
    time = make_yearquarter(
      year = rep(2000:(2000 + T / 4 - 1), each = 4 * (m * (n_b + 1))),
      quarter = rep(rep(1:4, each = m * (n_b + 1)), T / 4)
    ),
    node = rep(rep(c("Total", seq(n_b)), each = m), T),
    series = rep(LETTERS[seq(m)], T * (n_b + 1)),
    value = as.vector(Y)
  ) |>
    mutate(time = time) |>
    tsibble::as_tsibble(index = time, key = c(node, series))
}

make_array <- function(object, variable = "value") {
  if (!is_tsibble(object)) {
    stop("Object must be a tsibble or fable object")
  }
  if (!(variable %in% colnames(object))) {
    stop("Variable not found in object")
  }
  # Time dimension
  times <- unique(object$time)
  T <- length(times)
  # Series
  series <- unique(object$series)
  m <- length(series)
  # Nodes
  nodes <- unique(object$node)
  n <- length(nodes)
  if (!("Total" %in% nodes)) {
    stop("nodes must contain a 'Total' series")
  } else {
    nodes <- c(
      "Total",
      grep("^agg", nodes, value = TRUE),
      nodes[!grepl("^agg", nodes) & nodes != "Total"]
    )
  }
  # Make sure result is ordered correctly
  object <- arrange(object, node, series, time)
  # Create array output
  Y <- array(dim = c(T, m, n))
  dimnames(Y) <- list(time = times, series = series, node = nodes)

  # Fill array.
  # This is slower than necessary, but it is easy to get things in the wrong place
  # and a triple loop ensures we don't make mistakes
  for (i in seq(T)) {
    for (j in seq(m)) {
      for (k in seq(n)) {
        Y[i, j, k] <- object[[variable]][
          object$time == times[i] &
            object$series == series[j] &
            object$node == nodes[k]
        ]
      }
    }
  }
  return(Y)
}

make_matrix <- function(object, variable = "value") {
  Y <- make_array(object, variable)
  Ymat <- matrix(Y, nrow = dim(Y)[1], ncol = dim(Y)[2] * dim(Y)[3])
  rownames(Ymat) <- dimnames(Y)[[1]]
  colnames(Ymat) <- paste0(
    rep(dimnames(Y)[[3]], each = dim(Y)[2]),
    ".",
    rep(dimnames(Y)[[2]], times = dim(Y)[3])
  )
  return(Ymat)
}

# ============================================================
# Extract the residual matrix in the required format
# ============================================================

residuals_matrix <- function(fit) {
  # Get residuals from the fitted model
  res <- fit |> residuals()

  # Fix node order to match the hierarchy structure
  res$node <- factor(
    res$node,
    levels = c("Total", "agg_1", "agg_2", "1", "2", "3", "4", "5")
  )
  res <- res |> arrange(node)

  ##### Adjust the residual matrix

  ## VAR model

  if (unique(res$.model) == "var") {
    res <- res |>
      select(-.model) |>
      # Transform from long to wide format:
      # Each column becomes: node.variable (e.g., Total.A, Total.B, agg_1.A, agg_1.B ...)
      pivot_wider(
        names_from = node,
        values_from = c(A, B),
        names_glue = "{node}.{.value}"
      ) |>
      drop_na() |>
      select(-time) |>
      as.matrix()
  } else {
    ## ARIMA and ETS
    res <- res |>
      select(-.model) |>
      # Transform from long to wide format:
      # Each column becomes: node.variable (e.g., Total.A, Total.B, agg_1.A, agg_1.B ...)
      pivot_wider(
        names_from = c(node, series),
        names_glue = "{node}.{series}",
        values_from = .resid
      ) |>
      drop_na() |>
      select(-time) |>
      as.matrix()
  }
}

# ============================================================
# Multivariate reconciliation using the cov estimator for W
# ============================================================

mv_reconcile_cov <- function(fit, fc, S) {
  # Turn forecasts into matrix
  Yhat <- t(make_matrix(fc, ".mean"))
  n <- length(unique(fc$node))
  m <- length(unique(fc$series))

  # Summing matrix
  SI <- kronecker(S, diag(m))
  # Covariance matrix
  W <- residuals_matrix(fit) |> cov()
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

mv_reconcile_shrink <- function(fit, fc, S) {
  # Turn forecasts into matrix
  Yhat <- t(make_matrix(fc, ".mean"))
  m <- length(unique(fc$series))

  # Summing matrix
  SI <- kronecker(S, diag(m))

  # Residual matrix
  res <- residuals_matrix(fit)

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

# ================================================================
# univariate reconciliation using the shrinkage estimator for W
# ================================================================

reconcile_shrink <- function(fit, fc, S, serie) {
  ## Filter series (A or B)
  fc <- fc |> filter(series == serie)

  # Turn forecasts into matrix
  Yhat <- t(make_matrix(fc, ".mean"))

  # Residual matrix
  res <- residuals_matrix(fit)

  # Select columns corresponding to serie A or B
  res <- res[, grep(paste0(serie, "$"), colnames(res)), drop = FALSE]

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
  Ytilde <- S %*% solve(t(S) %*% Winv %*% S) %*% t(S) %*% Winv %*% Yhat
  dimnames(Ytilde) <- dimnames(Yhat)

  # Now we need to turn Ytilde back into a tsibble object
  out <- t(Ytilde) |>
    as.data.frame() |>
    rownames_to_column("time") |>
    pivot_longer(
      cols = -time,
      names_to = c("node", "series"),
      names_pattern = "(.*)\\.(.*)",
      values_to = ".reconciled_uni_mean_shrink"
    ) |>
    mutate(time = as.numeric(time)) |>
    tsibble::as_tsibble(index = time, key = c(node, series))

  # Add in anything else from the original fc object
  out$time <- fc$time

  left_join(fc, out, by = c("time", "node", "series"))
}
