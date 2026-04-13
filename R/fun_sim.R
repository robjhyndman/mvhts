# ====================================================================
# Functions needed for simulations
# ====================================================================

# --------------------------------------------------------------------
# Main simulation loop: runs until n_sim valid simulations are obtained.
#
# Uses furrr::future_map() to run attempts in parallel. Because some
# attempts may fail, we run in batches of `batch_size`, keep successes,
# and repeat until n_sim are collected. batch_size controls how many
# attempts are launched per round.
# --------------------------------------------------------------------
simulacao <- function(
  Phi,
  V,
  Sigma,
  S,
  n_sim = 1000,
  batch_size = 20
) {
  successes <- list()
  attempt <- 1
  while (length(successes) < n_sim) {
    needed <- n_sim - length(successes)
    results <- furrr::future_map(
      seq_len(batch_size),
      \(j) run_one_simulation(Phi, V, Sigma, S, sim_id = attempt + j - 1),
      .options = furrr::furrr_options(seed = TRUE)
    )
    attempt <- attempt + batch_size
    new_successes <- Filter(Negate(is.null), results)
    n_new <- min(length(new_successes), needed)
    successes <- c(successes, new_successes[seq_len(n_new)])
    message(length(successes), " / ", n_sim, " simulations complete.")
  }
  list(
    fc_sim = bind_rows(lapply(successes, `[[`, "fc")),
    Y_sim = bind_rows(lapply(successes, `[[`, "Y2"))
  )
}

# --------------------------------------------------------------------
# Run a single simulation attempt.
# Returns a list(fc, Y2) on success, or NULL if reconciliation fails.
# --------------------------------------------------------------------
run_one_simulation <- function(Phi, V, Sigma, S, sim_id) {
  Y <- sim_mvhts(120, Phi, V, Sigma)
  Y2 <- sim_aggregate(Y)
  fits <- fit_models(Y2)
  fcs <- make_forecasts(fits)
  mv <- reconcile_mv(fits, fcs, S)
  if (is.null(mv)) {
    return(NULL)
  }
  uv <- reconcile_uv(fits, fcs, S)
  Y_true <- Y2 |> rename(Y = value)
  fc <- purrr::imap(mv, \(fc_model, name) {
    fc_model |>
      left_join(Y_true, by = c("time", "node", "series")) |>
      mutate(simulacao = sim_id) |>
      data.frame() |>
      select(-value)
  }) |>
    bind_rows() |>
    left_join(uv, by = c("time", "node", "series", ".model", ".mean"))
  Y2$simulacao <- sim_id
  list(fc = fc, Y2 = data.frame(Y2))
}

# ====================================================================
# Simulation of multivariate series at the bottom level
# ====================================================================

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
  Y <- array(dim = c(m, n_b + 1, T))
  Y[, 1, ] <- apply(B, c(1, 3), sum)
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
    tsibble::as_tsibble(index = time, key = c(node, series))
}

# ====================================================================
# Aggregate the bottom-level series to create the hierarchy structure
# ====================================================================

sim_aggregate <- function(Y) {
  bind_rows(
    # Original bottom series
    Y,

    # First aggregated level: sum of nodes 1 and 2
    Y |>
      filter(node %in% c(1, 2)) |>
      group_by(series) |>
      summarise(value = sum(value)) |>
      mutate(node = "agg_1"),

    # Second aggregated level: sum of nodes 3, 4 and 5
    Y |>
      filter(node %in% c(3, 4, 5)) |>
      group_by(series) |>
      summarise(value = sum(value)) |>
      mutate(node = "agg_2")
  )
}

# --------------------------------------------------------------------
# Fit ARIMA, ETS, and VAR models on training data (up to 2026 Q4)
# --------------------------------------------------------------------
fit_models <- function(Y2) {
  train <- Y2 |> filter_index(~"2026 Q4")
  train_wide <- train |> pivot_wider(names_from = series, values_from = value)
  list(
    arima = train |> model(arima = ARIMA(value)),
    ets = train |> model(ets = ETS(value)),
    var = train_wide |> model(var = VAR(vars(A, B)))
  )
}

# --------------------------------------------------------------------
# Produce 12-step-ahead forecasts from each fitted model
# --------------------------------------------------------------------
make_forecasts <- function(fits) {
  list(
    arima = forecast(fits$arima, h = 12),
    ets = forecast(fits$ets, h = 12),
    var = forecast(fits$var, h = 12) |> tidy_var_forecast()
  )
}

# --------------------------------------------------------------------
# Multivariate reconciliation (covariance + shrinkage) for one model.
# Returns a data frame with both reconciled columns, or NULL on error.
# --------------------------------------------------------------------
reconcile_mv_one <- function(fit, fc, S) {
  fc_cov <- try(
    mv_reconcile(fit, fc, S, time_fn = tsibble::yearquarter),
    silent = TRUE
  )
  if (inherits(fc_cov, "try-error")) {
    return(NULL)
  }
  fc_shrink <- try(
    mv_reconcile(
      fit,
      fc,
      S,
      cov_fn = shrinkage_cov,
      time_fn = tsibble::yearquarter
    ),
    silent = TRUE
  )
  if (inherits(fc_shrink, "try-error")) {
    return(NULL)
  }
  fc_cov$.reconciled_mean_shrink <- fc_shrink$.reconciled_mean_cov
  fc_cov
}

# --------------------------------------------------------------------
# Run multivariate reconciliation for all three models.
# Returns a named list (arima, ets, var), or NULL if any model fails.
# --------------------------------------------------------------------
reconcile_mv <- function(fits, fcs, S) {
  results <- list(
    arima = reconcile_mv_one(fits$arima, fcs$arima, S),
    ets = reconcile_mv_one(fits$ets, fcs$ets, S),
    var = reconcile_mv_one(fits$var, fcs$var, S)
  )
  if (any(vapply(results, is.null, logical(1)))) {
    return(NULL)
  }
  results
}

# --------------------------------------------------------------------
# Univariate shrinkage reconciliation across all models and series.
# Returns a single data frame.
# --------------------------------------------------------------------
reconcile_uv <- function(fits, fcs, S) {
  models <- c("arima", "ets", "var")
  series <- c("A", "B")
  combos <- expand.grid(
    model = models,
    series = series,
    stringsAsFactors = FALSE
  )

  purrr::pmap(combos, \(model, series) {
    data.frame(uv_reconcile(
      fits[[model]],
      fcs[[model]],
      S,
      series,
      cov_fn = shrinkage_cov,
      time_fn = tsibble::yearquarter
    )) |>
      select(-value)
  }) |>
    bind_rows()
}
