library(fpp3)
source(here::here("R/application_data.R"))
source(here::here("R/tables.R"))

if (!fs::file_exists(here::here("Saida/app_relrmse.rds"))) {
  source(here::here("R/application.R"))
}
app_relrmse <- readRDS(here::here("Saida/app_relrmse.rds"))

# Node display ordering and labels (Total → regions alphabetically → states)
all_nodes <- unique(app_relrmse$node)
app_agg_nodes <- sort(all_nodes[startsWith(all_nodes, "agg_")])
app_states <- sort(all_nodes[!all_nodes %in% c("Total", app_agg_nodes)])
app_node_order <- c("Total", app_agg_nodes, app_states)
region_english <- setNames(
  region_meta$region_label[-1],
  paste0("agg_", region_meta$Região[-1])
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

fs::dir_create(here::here("Tabelas"))

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
  caption = app_caption("VAR", "admission"),
  label = "tab:adm_var_sh",
  file = here::here("Tabelas/Tabs_var_adm_sh.tex")
)
write_app_relrmse_table(
  app_relrmse |> filter(series == "Demissões", .model == "var"),
  node_order = app_node_order,
  node_labels_map = app_node_labels,
  caption = app_caption("VAR", "dismissal"),
  label = "tab:dem_var_sh",
  file = here::here("Tabelas/Tabs_var_dem_sh.tex")
)
