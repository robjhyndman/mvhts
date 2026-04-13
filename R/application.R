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


# ============================================================
# Multivariate reconciliation using covariance and shrinkage
# ============================================================

########################### ARIMA MODEL ###################################

# Fit ARIMA with rolling origin cross-validation
mod <- Dados |>
  filter_index(~"2022 Q12") |>
  stretch_tsibble(.init = 217, .step = 1) |>
  model(arima = ARIMA(value))

# Forecast 12 steps ahead
fc <- mod |> forecast(h = 12)

# Create forecast horizon index
fc <- fc |>
  group_by(.id, node, series) |>
  mutate(h = row_number()) |>
  ungroup()

save(mod, fc, file = "mod_fc_arima_regiao.RData")

# ---------------- Reconciliation using covariance estimator ----------------

fc_rec_arima_cov <- reconcile_over_id(
  mod_tbl = mod,
  fc_tbl = fc,
  S = S,
  rec_fun = mv_reconcile
)

save(fc_rec_arima_cov, file = "fc_rec_arima_regiao.RData")

# ---------------- Reconciliation using shrinkage estimator ----------------

fc_rec_arima_sh <- reconcile_over_id(
  mod_tbl = mod,
  fc_tbl = fc,
  S = S,
  rec_fun = mv_reconcile_s
)

save(fc_rec_arima_cov, fc_rec_arima_sh, file = "fc_rec_arima_regiao2.RData")

########################### VAR MODEL ########################################

# Fit VAR model (bivariate: Admissions and Dismissals)
mod_var <- Dados |>
  pivot_wider(names_from = series, values_from = value) |>
  filter_index(~"2022 Q12") |>
  stretch_tsibble(.init = 217, .step = 1) |>
  model(var = VAR(vars(Admissões, Demissões)))

# Forecast 12 steps ahead
fc_var <- mod_var |> forecast(h = 12)

# Reshape VAR output back to long format
fc_var <- fc_var |>
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
  as_tsibble(index = time, key = c(.id, Região, node, .model, series))

# Create forecast horizon index
fc_var <- fc_var |>
  group_by(.id, node, series) |>
  mutate(h = row_number()) |>
  ungroup()

save(mod_var, fc_var, file = "mod_fc_var_regiao.RData")

# ---------------- Reconciliation using covariance estimator ----------------

fc_rec_var_cov <- reconcile_over_id(
  mod_tbl = mod_var,
  fc_tbl = fc_var,
  S = S,
  rec_fun = mv_reconcile
)

save(fc_rec_var_cov, file = "fc_rec_var_regiao.RData")

# ---------------- Reconciliation using shrinkage estimator ----------------

fc_rec_var_s <- reconcile_over_id(
  mod_tbl = mod_var,
  fc_tbl = fc_var,
  S = S,
  rec_fun = mv_reconcile_s
)

save(fc_rec_var_cov, fc_rec_var_s, file = "fc_rec_var_regiao.RData")
