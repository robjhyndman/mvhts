library(fpp3)
library(tsibble)
source(here::here("R/read_data.R"))
source(here::here("R/compute_S.R"))
source(here::here("R/helpers.R"))
source(here::here("R/reconcile.R"))
source(here::here("R/tables.R"))

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
    filter_index(~"2022 Q12") |>
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

# ==============================================================
# Generate application RelRMSE_Base tables (tab:adm_sh, tab:dem_sh)
# ==============================================================

# Node display ordering and labels (Total → regions alphabetically → states)
all_nodes <- unique(as_tibble(Dados)$node)
app_agg_nodes <- sort(all_nodes[startsWith(all_nodes, "agg_")])
app_states <- sort(all_nodes[!all_nodes %in% c("Total", app_agg_nodes)])
app_node_order <- c("Total", app_agg_nodes, app_states)
region_english <- c(
  "agg_Centro-Oeste" = "Midwest",
  "agg_Nordeste" = "Northeast",
  "agg_Norte" = "North",
  "agg_Sudeste" = "Southeast",
  "agg_Sul" = "South"
)
app_node_labels <- c(
  Total = "Total",
  region_english[app_agg_nodes],
  setNames(app_states, app_states)
)

# Actual values for joining (drop Região to keep join keys simple)
actuals <- as_tibble(Dados) |> select(time, node, series, value)

# Base forecasts: drop the distribution column, keep .mean and h
fc_base <- as_tibble(fc) |>
  select(.id, node, series, time, h, .mean)

# Reconciled (ARIMA + shrinkage): keep .reconciled_mean_cov and h
fc_rec <- as_tibble(fc_rec_arima_sh) |>
  select(.id, node, series, time, h, .reconciled_mean_cov)

# Compute RelRMSE_Base per (node, series, h) across all cross-validation origins
app_relrmse <- fc_rec |>
  left_join(actuals, by = c("time", "node", "series")) |>
  group_by(node, series, h) |>
  summarise(
    RMSE_rec = sqrt(mean((.reconciled_mean_cov - value)^2, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  left_join(
    fc_base |>
      left_join(actuals, by = c("time", "node", "series")) |>
      group_by(node, series, h) |>
      summarise(
        RMSE_base = sqrt(mean((.mean - value)^2, na.rm = TRUE)),
        .groups = "drop"
      ),
    by = c("node", "series", "h")
  ) |>
  mutate(RelRMSE_Base = 1 - RMSE_rec / RMSE_base)

app_caption <- function(series_name) {
  paste0(
    "$\\RelRMSE^{\\Base}$ of ",
    series_name,
    " series.",
    " ARIMA model for base forecasts and the shrinkage approach to",
    " estimate $\\bm{W}$.",
    " Values in red indicate a $\\RelRMSE^{\\Base}$ less than 0."
  )
}

write_app_relrmse_table(
  app_relrmse |> filter(series == "Admissões"),
  node_order = app_node_order,
  node_labels_map = app_node_labels,
  caption = app_caption("admission"),
  label = "tab:adm_sh",
  file = here::here("Tabelas/Tabs_adm_sh.tex")
)
write_app_relrmse_table(
  app_relrmse |> filter(series == "Demissões"),
  node_order = app_node_order,
  node_labels_map = app_node_labels,
  caption = app_caption("dismissal"),
  label = "tab:dem_sh",
  file = here::here("Tabelas/Tabs_dem_sh.tex")
)
