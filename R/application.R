library(fpp3)
library(tsibble)
source(here::here("R/application_data.R"))
source(here::here("R/application_S.R"))
source(here::here("R/helpers.R"))
source(here::here("R/reconcile.R"))
source(here::here("R/tables.R"))

# ==============================================================
# Read employment data (Admissions and Dismissals)
# ==============================================================
Dados <- read_data(state_meta, region_meta)

# ==============================================================
# Hierarchy information
# ==============================================================
S <- compute_S(Dados, state_meta, region_meta)

# ==============================================================
# Multivariate reconciliation using covariance and shrinkage
# ==============================================================

# Fit ARIMA with rolling origin cross-validation ---------------
if (fs::file_exists(here::here("Saida/mod_arima_regiao.rds"))) {
  mod <- readRDS(here::here("Saida/mod_arima_regiao.rds"))
} else {
  mod <- Dados |>
    filter_index(~"2022 Dec") |>
    stretch_tsibble(.init = 217, .step = 1) |>
    model(arima = ARIMA(value))
  saveRDS(mod, file = here::here("Saida/mod_arima_regiao.rds"), compress = "xz")
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
  rec_fun = mv_reconcile,
  cov_fn = shrinkage_cov
)

# Fit VAR model (bivariate: Admissions and Dismissals) ---------
if (fs::file_exists(here::here("Saida/mod_var_regiao.rds"))) {
  mod_var <- readRDS(here::here("Saida/mod_var_regiao.rds"))
} else {
  mod_var <- Dados |>
    pivot_wider(names_from = series, values_from = value) |>
    filter_index(~"2022 Dec") |>
    stretch_tsibble(.init = 217, .step = 1) |>
    model(var = VAR(vars(Admissões, Demissões)))
  saveRDS(
    mod_var,
    file = here::here("Saida/mod_var_regiao.rds"),
    compress = "xz"
  )
}
# Forecast 12 steps ahead
fc_var <- mod_var |>
  forecast(h = 12) |>
  tidy_var_forecast(
    series_names = c("Admissões", "Demissões"),
    extra_keys = c(".id", "Região")
  ) |>
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
  rec_fun = mv_reconcile,
  cov_fn = shrinkage_cov
)

# =========================================
# Compute accuracy measures and save
# =========================================

fc_combined <- bind_rows(
  fc_rec_arima_cov,
  fc_rec_arima_sh,
  fc_rec_var_cov,
  fc_rec_var_s
) |>
  left_join(
    Dados |> select(time, node, series, value),
    by = c("time", "node", "series")
  )

app_rmse <- fc_combined |>
  group_by(node, series, h, .model, type) |>
  summarise(
    RMSE = sqrt(mean((forecast - value)^2, na.rm = TRUE)),
    .groups = "drop"
  )
app_relrmse <- app_rmse |>
  pivot_wider(names_from = type, values_from = RMSE) |>
  pivot_longer(
    starts_with("reconciled"),
    names_to = "type",
    values_to = "reconciled"
  ) |>
  mutate(type = stringr::str_remove(type, "reconciled_")) |>
  mutate(RelRMSE = 1 - reconciled / base) |>
  filter(type == "shrinkage")

fs::dir_create(here::here("Saida"))
saveRDS(app_relrmse, file = here::here("Saida/app_relrmse.rds"))
