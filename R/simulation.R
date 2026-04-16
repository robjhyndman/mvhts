library(fpp3)
library(fable.prophet)
library(furrr)

source(here::here("R/helpers.R"))
source(here::here("R/reconcile.R"))
source(here::here("R/simulation_functions.R"))
source(here::here("R/simulation_setup.R"))

# Use all available cores; adjust workers as needed
future::plan(future::multisession, workers = parallelly::availableCores() - 2)

set.seed(30, kind = "L'Ecuyer-CMRG")
nsim <- 1000

# ============================================================
# Simulation design — run over all (V, Sigma) combinations
# ============================================================
params <- expand.grid(
  v_name = names(V_list),
  sigma_name = names(Sigma_list),
  stringsAsFactors = FALSE
) |>
  mutate(i = row_number())

fc_list <- purrr::pmap(params, function(v_name, sigma_name, i) {
  path <- here::here(sprintf("Saida/sim_rec%d.rds", i))
  if (fs::file_exists(path)) {
    message(paste("Loading simulation for", v_name, "and", sigma_name))
    result <- readRDS(path)
  } else {
    message(paste("Running simulation for", v_name, "and", sigma_name))
    result <- simulacao(
      Phi,
      V_list[[v_name]],
      Sigma_list[[sigma_name]],
      S,
      nsim
    )
    saveRDS(result, file = path, compress = "xz")
  }
  result
})
