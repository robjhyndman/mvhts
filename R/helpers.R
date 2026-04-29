order_nodes <- function(nodes) {
  nodes <- unique(nodes)
  agg <- sort(grep("^agg", nodes, value = TRUE))
  bottom <- sort(nodes[!grepl("^agg", nodes) & nodes != "Total"])
  c("Total", agg, bottom)
}

# --------------------------------------------------------------------
# Reshape a VAR forecast from wide mable format to long tsibble format.
# series_names: character vector of the two series names (in V1, V2 order).
# extra_keys: additional tsibble key columns beyond node and series.
# --------------------------------------------------------------------
tidy_var_forecast <- function(
  fc_var,
  series_names = c("A", "B"),
  extra_keys = character()
) {
  fc_var |>
    mutate(.mean = as.data.frame(.mean)) |>
    unnest_wider(.mean, names_sep = "_") |>
    rename(
      !!series_names[1] := .mean_V1,
      !!series_names[2] := .mean_V2,
      value = .distribution
    ) |>
    pivot_longer(
      -c(all_of(extra_keys), node, .model, time, value),
      names_to = "series",
      values_to = ".mean",
      cols_vary = "slowest"
    ) |>
    arrange(node) |>
    as_tsibble(index = time, key = c(all_of(extra_keys), node, series))
}

make_array <- function(object, variable = "value") {
  if (!is_tsibble(object)) {
    stop("Object must be a tsibble or fable object")
  }
  if (!(variable %in% colnames(object))) {
    stop("Variable not found in object")
  }
  # Time dimension
  times <- sort(unique(object$time))
  len_T <- length(times)
  # Series
  series <- sort(unique(object$series))
  m <- length(series)
  # Nodes
  raw_nodes <- unique(object$node)
  if (!("Total" %in% raw_nodes)) {
    stop("nodes must contain a 'Total' series")
  }
  nodes <- order_nodes(raw_nodes)
  n <- length(nodes)
  # Make sure result is ordered correctly
  object <- object |> arrange(factor(node, levels = nodes), series, time)
  # Create array output
  Y <- array(object[[variable]], dim = c(len_T, m, n))
  dimnames(Y) <- list(time = as.character(times), series = series, node = nodes)
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

make_matrix2 <- function(object, variable = "value") {
  if (!is_tsibble(object)) {
    stop("Object must be a tsibble or fable object")
  }
  if (!(variable %in% colnames(object))) {
    stop("Variable not found in object")
  }

  raw_nodes <- unique(object$node)
  if (!("Total" %in% raw_nodes)) {
    stop("nodes must contain a 'Total' series")
  }
  nodes <- order_nodes(raw_nodes)

  if (length(unique(object$.id)) > 1) {
    stop("Object must contain only one .id")
  }
  Y <- object |>
    as_tibble() |>
    select(-any_of(c(".model", "Região", ".id"))) |>
    filter(node %in% nodes) |>
    pivot_wider(names_from = node, values_from = all_of(variable)) |>
    # filter(if_all(everything(), ~ !is.na(.))) |>
    pivot_wider(
      names_from = series,
      values_from = all_of(nodes),
      names_sep = "."
    ) |>
    select(-time) |>
    as.matrix()

  return(Y)
}

# Sample estimator for covariance matrix
sample_cov <- function(res) {
  cov(res, use = "complete.obs")
}

# Shrinkage estimator for covariance matrix
shrinkage_cov <- function(res) {
  res <- stats::na.omit(res)
  t <- nrow(res)
  # Sample covariance matrix
  covm <- crossprod(res) / t
  tar <- diag(apply(res, 2, crossprod) / t)
  corm <- cov2cor(covm)
  xs <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
  v <- (1 / (t * (t - 1))) * (crossprod(xs^2) - 1 / t * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v) / sum(d)
  lambda <- max(min(lambda, 1), 0)
  # Shrinkage estimator
  lambda * tar + (1 - lambda) * covm
}

# Turn reconciled forecasts back into a tsibble

ytilde_to_tsibble <- function(
  Ytilde,
  fc,
  col_name,
  time_fn = tsibble::yearmonth
) {
  # Now we need to turn Ytilde back into a tsibble object
  out <- t(Ytilde) |>
    as.data.frame() |>
    rownames_to_column("time") |>
    pivot_longer(
      cols = -time,
      names_to = c("node", "series"),
      names_pattern = "(.*)\\.(.*)",
      values_to = col_name
    ) |>
    mutate(time = time_fn(time)) |>
    tsibble::as_tsibble(index = time, key = c(node, series))
  # Add in anything else from the original fc object
  left_join(fc, out, by = c("time", "node", "series"))
}

# ============================================================
# Extract the residual matrix in the required format
# ============================================================

residuals_matrix <- function(res) {
  res$node <- factor(res$node, levels = order_nodes(unique(res$node)))
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
      )
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
      )
  }
  res |>
    drop_na() |>
    select(-time) |>
    as.matrix()
}

get_residuals <- function(fit) {
  if (!is_mable(fit)) {
    stop("fit must be a mable object")
  }
  res <- fit |> residuals()

  if ("agg_1" %in% res$node) {
    # fit from simulation
    return(residuals_matrix(res))
  } else if (unique(res$.model) == "var") {
    res |>
      pivot_longer(
        -c(node, .model, time, .id, Região),
        names_to = "series",
        values_to = ".resid",
        cols_vary = "slowest"
      ) |>
      arrange(node) |>
      make_matrix2(".resid")
  } else {
    res |>
      make_matrix2(".resid")
  }
}
