library(fpp3)
library(fable.prophet)

source(here::here("R/fun_sim.R"))
source(here::here("R/helpers.R"))
source(here::here("R/reconcile.R"))

set.seed(30)
nsim <- 1000

# ============================================================
# Definition of the aggregation structure matrix (S matrix)
# ============================================================
S <- rbind(
  rep(1, 5),
  c(1, 1, 0, 0, 0),
  c(0, 0, 1, 1, 1),
  diag(5)
)

# ============================================================
# Autoregressive parameter
# ============================================================
Phi <- matrix(c(0.7, 0.2, 0.2, 0.7), 2)

# ================================================================
# Different correlation structures for V (between series A and B)
# ================================================================
V_list <- list(
  V1 = diag(2),
  V2 = matrix(c(1, 0.7, 0.7, 1), 2),
  V3 = matrix(c(1, -0.7, -0.7, 1), 2)
)

# ==================================================================
# Different correlation structures for Sigma (between bottom nodes)
# ==================================================================
Sigma_list <- list(
  Sigma1 = diag(5),
  Sigma2 = rbind(
    c(1, 0.7, 0, 0, 0),
    c(0.7, 1, 0, 0, 0),
    c(0, 0, 1, 0.7, 0.7),
    c(0, 0, 0.7, 1, 0.7),
    c(0, 0, 0.7, 0.7, 1)
  ),
  Sigma3 = rbind(
    c(1, -0.4, 0, 0, 0),
    c(-0.4, 1, 0, 0, 0),
    c(0, 0, 1, -0.4, -0.4),
    c(0, 0, -0.4, 1, -0.4),
    c(0, 0, -0.4, -0.4, 1)
  )
)

# ============================================================
# Simulation design — run over all (V, Sigma) combinations
# ============================================================
params <- expand.grid(
  v_name = names(V_list),
  sigma_name = names(Sigma_list),
  stringsAsFactors = FALSE
) |>
  mutate(i = row_number())

fc_list <- vector("list", nrow(params))

pwalk(params, function(v_name, sigma_name, i) {
  path <- here::here(sprintf("Simulations/sim_rec%d.rds", i))
  if (fs::file_exists(path)) {
    fc_list[[i]] <<- readRDS(path)
  } else {
    fc_list[[i]] <<- simulacao(
      Phi,
      V_list[[v_name]],
      Sigma_list[[sigma_name]],
      S,
      nsim
    )
    saveRDS(fc_list[[i]], file = path)
  }
})
