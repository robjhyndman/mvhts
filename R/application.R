library(fpp3)
library(tsibble)
source(here::here("R/function_rec_application.R"))
source(here::here("R/read_data.R"))
source(here::here("R/compute_S.R"))

# ==============================================================
# Read employment data (Admissions and Dismissals)
# ==============================================================
Dados <- read_data()

# ==============================================================
# Hierarchy information
# ==============================================================
S <- compute_S(Dados)

# ==============================================================
# Multivariate reconciliation using covariance and shrinkage
# ==============================================================

# Fit ARIMA with rolling origin cross-validation ---------------
if (fs::file_exists(here::here("Saida/mod_arima_regiao.rds"))) {
  mod <- readRDS(here::here("Saida/mod_arima_regiao.rds"))
} else {
  mod <- Dados |>
    filter_index(~"2022 Q12") |>
    stretch_tsibble(.init = 217, .step = 1) |>
    model(arima = ARIMA(value))
  saveRDS(mod, file = here::here("Saida/mod_arima_regiao.rds"))
}
# Forecast 12 steps ahead
fc <- mod |>
  forecast(h = 12) |>
  # Create forecast horizon index
  group_by(.id, node, series) |>
  mutate(h = row_number()) |>
  ungroup()
# Reconciliation using covariance estimator
fc_rec_arima_cov <- reconcile_over_id(
  mod_tbl = mod,
  fc_tbl = fc,
  S = S,
  rec_fun = mv_reconcile
)
# Reconciliation using shrinkage estimator
fc_rec_arima_sh <- reconcile_over_id(
  mod_tbl = mod,
  fc_tbl = fc,
  S = S,
  rec_fun = mv_reconcile_s
)

# Fit VAR model (bivariate: Admissions and Dismissals) ---------
if (fs::file_exists(here::here("Saida/mod_var_regiao.rds"))) {
  mod_var <- readRDS(here::here("Saida/mod_var_regiao.rds"))
} else {
  mod_var <- Dados |>
    pivot_wider(names_from = series, values_from = value) |>
    filter_index(~"2022 Q12") |>
    stretch_tsibble(.init = 217, .step = 1) |>
    model(var = VAR(vars(Admissões, Demissões)))
  saveRDS(mod_var, file = here::here("Saida/mod_var_regiao.rds"))
}
# Forecast 12 steps ahead
fc_var <- mod_var |>
  forecast(h = 12) |>
  # Reshape VAR output back to long format
  mutate(.mean = as.data.frame(.mean)) |>
  unnest_wider(.mean, names_sep = "_") |>
  rename(Admissões = .mean_V1, Demissões = .mean_V2, value = .distribution) |>
  pivot_longer(
    -c(.id, Região, node, .model, time, value),
    names_to = "series",
    values_to = ".mean",
    cols_vary = "slowest"
  ) |>
  arrange(Região) |>
  as_tsibble(index = time, key = c(.id, Região, node, .model, series)) |>
  # Create forecast horizon index
  group_by(.id, node, series) |>
  mutate(h = row_number()) |>
  ungroup()

# Reconciliation using covariance estimator
fc_rec_var_cov <- reconcile_over_id(
  mod_tbl = mod_var,
  fc_tbl = fc_var,
  S = S,
  rec_fun = mv_reconcile
)
# Reconciliation using shrinkage estimator
fc_rec_var_s <- reconcile_over_id(
  mod_tbl = mod_var,
  fc_tbl = fc_var,
  S = S,
  rec_fun = mv_reconcile_s
)
