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
  n_sim = 1000
) {
  results <- furrr::future_map(
    seq_len(n_sim),
    \(sim_id) run_one_simulation(Phi, V, Sigma, S, sim_id = sim_id),
    .options = furrr::furrr_options(seed = TRUE)
  )
  list(
    fc_sim = bind_rows(lapply(results, `[[`, "fc")),
    Y_sim = bind_rows(lapply(results, `[[`, "Y2"))
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

  fc <- purrr::map(mv, \(fc_model) {
    fc_model |>
      left_join(Y_true, by = c("time", "node", "series")) |>
      mutate(simulacao = sim_id) |>
      data.frame() |>
      select(-value)
  }) |>
    purrr::list_rbind() |>
    left_join(uv, by = c("time", "node", "series", ".model", ".mean"))

  Y2$simulacao <- sim_id
  list(fc = fc, Y2 = data.frame(Y2))
}

# ====================================================================
# VAR(1) with zero starting value: eta_t = Phi eta_{t-1} + innov_t.
# innov is an n x m matrix; returns an n x m matrix. Identical to
# tsDyn::VAR.sim(Phi, n, include = "none", innov = innov), which was
# used previously (tsDyn was archived from CRAN in August 2026).
# ====================================================================

sim_var1 <- function(Phi, innov) {
  eta <- matrix(0, nrow(innov), ncol(innov))
  prev <- numeric(ncol(innov))
  for (t in seq_len(nrow(innov))) {
    prev <- drop(Phi %*% prev) + innov[t, ]
    eta[t, ] <- prev
  }
  eta
}

# ====================================================================
# Simulation of multivariate series at the bottom level
#
# NOTE: Series names "A" and "B" and start at year 2000
# so that a length-120 quarterly series spans 2000 Q1 to 2029 Q4.
# ====================================================================

sim_mvhts <- function(len_T, Phi, V, Sigma, start_year = 2000) {
  if (len_T %% 4 != 0) {
    stop("len_T must be a multiple of 4 for quarterly indexing.")
  }

  # Get dimensions
  m <- NROW(V)
  n_b <- NROW(Sigma)
  if (NROW(Phi) != m) {
    stop("Phi must have the same number of rows as V")
  }

  # Covariance matrix for the whole system
  W <- kronecker(Sigma, V)

  # Set up space for storing the simulation
  B <- array(dim = c(m, n_b, len_T))

  # Generate noise with N(0,W) distribution
  noise <- mvtnorm::rmvnorm(len_T, rep(0, n_b * m), W)
  E <- array(t(noise), dim = c(m, n_b, len_T))

  # Generate bottom level series
  for (i in seq(n_b)) {
    B[, i, ] <- t(
      sim_var1(Phi, innov = t(E[, i, ])) +
        runif(1, 0, 4) * sin(2 * pi * seq(len_T) / 4)
    )
  }

  Y <- array(dim = c(m, n_b + 1, len_T))
  Y[, 1, ] <- apply(B, c(1, 3), sum)
  Y[, -1, ] <- B

  series_names <- LETTERS[seq_len(m)]

  tibble::tibble(
    time = make_yearquarter(
      year = rep(
        start_year:(start_year + len_T / 4 - 1),
        each = 4 * (m * (n_b + 1))
      ),
      quarter = rep(rep(1:4, each = m * (n_b + 1)), len_T / 4)
    ),
    node = rep(rep(c("Total", seq(n_b)), each = m), len_T),
    series = rep(series_names, len_T * (n_b + 1)),
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
      summarise(value = sum(value), .groups = "drop") |>
      mutate(node = "agg_1"),

    # Second aggregated level: sum of nodes 3, 4 and 5
    Y |>
      filter(node %in% c(3, 4, 5)) |>
      group_by(series) |>
      summarise(value = sum(value), .groups = "drop") |>
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
    var = forecast(fits$var, h = 12) |>
      tidy_var_forecast(series_names = c("A", "B"))
  )
}

# --------------------------------------------------------------------
# Multivariate reconciliation (covariance + shrinkage) for one model.
# Returns a data frame with both reconciled columns, or NULL on error.
# --------------------------------------------------------------------
reconcile_mv_one <- function(fit, fc, S) {
  fc_cov <- #try(
    mv_reconcile(
      fit,
      fc,
      S,
      time_fn = tsibble::yearquarter,
      simulated = TRUE
    ) #,
  #silent = TRUE
  #)
  #if (inherits(fc_cov, "try-error")) {
  #  return(NULL)
  #}
  fc_shrink <- #try(
    mv_reconcile(
      fit,
      fc,
      S,
      cov_fn = shrinkage_cov,
      time_fn = tsibble::yearquarter,
      simulated = TRUE
    ) #,
  #silent = TRUE
  #)
  #if (inherits(fc_shrink, "try-error")) {
  #  return(NULL)
  #}
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
    data.frame(
      uv_reconcile(
        fits[[model]],
        fcs[[model]],
        S,
        series,
        cov_fn = shrinkage_cov,
        time_fn = tsibble::yearquarter,
        simulated = TRUE
      )
    ) |>
      select(-value)
  }) |>
    purrr::list_rbind()
}
