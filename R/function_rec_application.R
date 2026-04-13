
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
    
    nodes <- c("Total", 
               grep("^agg", nodes, value = TRUE), 
               nodes[!grepl("^agg", nodes) & nodes != "Total"])
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
        Y[i, j, k] <- object[[variable]][object$time == times[i] & 
                                           object$series == series[j] &
                                           object$node == nodes[k]]
        
      }
    }
  }
  return(Y)
}

make_matrix <- function(object, variable = "value") {
  Y <- make_array(object, variable)
  Ymat <- matrix(Y, nrow = dim(Y)[1], ncol = dim(Y)[2] * dim(Y)[3])
  rownames(Ymat) <- dimnames(Y)[[1]]
  colnames(Ymat) <- paste0(rep(dimnames(Y)[[3]], each = dim(Y)[2]), ".", rep(dimnames(Y)[[2]], times = dim(Y)[3]))
  return(Ymat)
}

make_matrix2 <- function(object, variable = "value") {
  if (!is_tsibble(object)) {
    stop("Object must be a tsibble or fable object")
  }
  if (!(variable %in% colnames(object))) {
    stop("Variable not found in object")
  }
  
  nodes <- unique(object$node)
  if (!("Total" %in% nodes)) {
    stop("nodes must contain a 'Total' series")
  }
  
  nodes <- c("Total", 
             grep("^agg", nodes, value = TRUE), 
             nodes[!grepl("^agg", nodes) & nodes != "Total"])
  
  
  Y <- object %>% as_tibble() |> select(-.model)  |> 
    filter(node %in% nodes) %>%
    pivot_wider(names_from = node, values_from = all_of(variable)) %>%
    filter(if_all(everything(), ~ !is.na(.))) |> 
    pivot_wider(names_from = series, values_from = all_of(nodes), names_sep = ".") |>
    select(-time) |> 
    as.matrix()
  
  return(Y)
}


est_cov2 <- function(fit) {
  if (!is_mable(fit)) {
    stop("fit must be a mable object")
  }
  
  res <- fit |> residuals()
  
  if(unique(res$.model) == "var"){
    res |> 
      pivot_longer(-c(node, .model, time), names_to = "series", values_to = ".resid", cols_vary =  "slowest")|>
      arrange(node) |> make_matrix2(".resid") |> cov()
  } else {res |> make_matrix2(".resid") |> cov()}
}

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
    pivot_longer(cols = -time, names_to = c("node", "series"), names_pattern = "(.*)\\.(.*)",
                 values_to = ".reconciled_mean_cov") |>
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
  
  if(unique(res$.model) == "var"){
    res <- res |> 
      pivot_longer(-c(node, .model, time), names_to = "series", values_to = ".resid", cols_vary =  "slowest")|>
      arrange(node) |> make_matrix2(".resid")
  } else {res <- res |> make_matrix2(".resid")}
  
  t <- nrow(res)
  
  # Sample covariance matrix
  covm <- crossprod(stats::na.omit(res)) / t
  
  tar <- diag(apply(res, 2, purrr::compose(crossprod, stats::na.omit))/t)
  corm <- cov2cor(covm)
  xs <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
  xs <- xs[stats::complete.cases(xs),]
  v <- (1/(t * (t - 1))) * (crossprod(xs^2) - 1/t * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v)/sum(d)
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
    pivot_longer(cols = -time, names_to = c("node", "series"), names_pattern = "(.*)\\.(.*)",
                 values_to = ".reconciled_mean_shrink") |>
    mutate(time = as.numeric(time)) |> 
    tsibble::as_tsibble(index = time, key = c(node, series))
  
  out$time <- fc$time
  
  # Add in anything else from the original fc object
  left_join(fc, out, by = c("time", "node", "series"))
}
