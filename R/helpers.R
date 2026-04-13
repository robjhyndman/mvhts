order_nodes <- function(nodes) {
  c(
    "Total",
    grep("^agg", nodes, value = TRUE),
    nodes[!grepl("^agg", nodes) & nodes != "Total"]
  )
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
  T <- length(times)
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
  Y <- array(object[[variable]], dim = c(T, m, n))
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
    select(-.model, -Região, -.id) |>
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

est_cov2 <- function(fit) {
  if (!is_mable(fit)) {
    stop("fit must be a mable object")
  }

  res <- fit |> residuals()

  if (unique(res$.model) == "var") {
    res |>
      pivot_longer(
        -c(node, .model, time, .id, Região),
        names_to = "series",
        values_to = ".resid",
        cols_vary = "slowest"
      ) |>
      arrange(node) |>
      make_matrix2(".resid") |>
      cov(use = "complete.obs")
  } else {
    res |>
      make_matrix2(".resid") |>
      cov(use = "complete.obs")
  }
}

# Shrinkage estimator for covariance matrix

shrinkage_cov <- function(res) {
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
