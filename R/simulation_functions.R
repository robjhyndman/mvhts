# ====================================================================
# Functions needed for simulations
# ====================================================================

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
  if (NROW(Phi) != NROW(V)) {
    stop("Phi must have the same number of rows as V")
  }
  n_b <- NROW(Sigma)
  sim_mvhts_general(
    len_T,
    Phi_list = rep(list(Phi), n_b),
    Omega = kronecker(Sigma, V),
    start_year = start_year
  )
}

# --------------------------------------------------------------------
# General version: node i follows a VAR(1) with coefficient Phi_list[[i]],
# and Omega is the (m n_b x m n_b) innovation covariance in node-major
# order (variables vary fastest within each node). With a common Phi and
# Omega = Sigma (x) V this is identical to sim_mvhts(), including the
# random number stream.
# --------------------------------------------------------------------
sim_mvhts_general <- function(len_T, Phi_list, Omega, start_year = 2000) {
  if (len_T %% 4 != 0) {
    stop("len_T must be a multiple of 4 for quarterly indexing.")
  }
  n_b <- length(Phi_list)
  m <- NROW(Phi_list[[1]])
  if (NROW(Omega) != m * n_b) {
    stop("Omega must be (m n_b x m n_b)")
  }

  # Set up space for storing the simulation
  B <- array(dim = c(m, n_b, len_T))

  # Generate noise with N(0, Omega) distribution
  noise <- mvtnorm::rmvnorm(len_T, rep(0, n_b * m), Omega)
  E <- array(t(noise), dim = c(m, n_b, len_T))

  # Generate bottom level series
  for (i in seq(n_b)) {
    B[, i, ] <- t(
      sim_var1(Phi_list[[i]], innov = t(E[, i, ])) +
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
