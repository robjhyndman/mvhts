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

# =========================================
# Generate application RelRMSE_Base tables
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

# Node display ordering and labels (Total → regions alphabetically → states)
all_nodes <- unique(Dados$node)
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
app_caption <- function(model, series_name) {
  paste0(
    "$\\RelRMSE^{\\Base}$ of ",
    series_name,
    " series.",
    " ",
    model,
    " model for base forecasts and the shrinkage approach to",
    " estimate $\\bm{W}$.",
    " Values in red indicate a $\\RelRMSE^{\\Base}$ less than 0."
  )
}

write_app_relrmse_table(
  app_relrmse |> filter(series == "Admissões", .model == "arima"),
  node_order = app_node_order,
  node_labels_map = app_node_labels,
  caption = app_caption("ARIMA", "admission"),
  label = "tab:adm_sh",
  file = here::here("Tabelas/Tabs_adm_sh.tex")
)
write_app_relrmse_table(
  app_relrmse |> filter(series == "Demissões", .model == "arima"),
  node_order = app_node_order,
  node_labels_map = app_node_labels,
  caption = app_caption("ARIMA", "dismissal"),
  label = "tab:dem_sh",
  file = here::here("Tabelas/Tabs_dem_sh.tex")
)
write_app_relrmse_table(
  app_relrmse |> filter(series == "Admissões", .model == "var"),
  node_order = app_node_order,
  node_labels_map = app_node_labels,
  caption = paste("Table S19:", app_caption("VAR", "admission")),
  label = "tab:adm_var_sh",
  file = here::here("Tabelas/Tabs_var_adm_sh.tex")
)
write_app_relrmse_table(
  app_relrmse |> filter(series == "Demissões", .model == "var"),
  node_order = app_node_order,
  node_labels_map = app_node_labels,
  caption = paste("Table S19:", app_caption("VAR", "dismissal")),
  label = "tab:dem_var_sh",
  file = here::here("Tabelas/Tabs_var_dem_sh.tex")
)
