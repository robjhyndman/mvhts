source(here::here("R/simulation.R"))
source(here::here("R/tables.R"))

# Ensure output directory exists
fs::dir_create(here::here("Tabelas"))

# ============================================================
# Generate LaTeX tables from simulation results
# ============================================================

# Combine forecast and actual data across all 9 scenarios
all_fc <- purrr::imap_dfr(fc_list, \(x, i) {
  mutate(as_tibble(x$fc_sim), scenario = i)
}) |>
  group_by(scenario, simulacao, .model, node, series) |>
  arrange(time, .by_group = TRUE) |>
  mutate(h = row_number()) |>
  ungroup()

all_Y <- purrr::imap_dfr(fc_list, \(x, i) {
  mutate(as_tibble(x$Y_sim), scenario = i)
})

# Scaling factors for RMSSE: seasonal naive (p = 4) on training data
scale_factors <- all_Y |>
  filter(time <= yearquarter("2026 Q4")) |>
  group_by(scenario, simulacao, node, series) |>
  arrange(time, .by_group = TRUE) |>
  mutate(lag4 = lag(value, 4)) |>
  summarise(scale = mean((value - lag4)^2, na.rm = TRUE), .groups = "drop") |>
  group_by(scenario, node, series) |>
  summarise(mean_scale = mean(scale), .groups = "drop")

# RMSSE per (scenario, model, horizon) — multivariate shrinkage forecasts
rmsse_df <- all_fc |>
  group_by(scenario, .model, node, series, h) |>
  summarise(
    mse = mean((.reconciled_mean_shrink - Y)^2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(scale_factors, by = c("scenario", "node", "series")) |>
  mutate(sq_scaled = mse / mean_scale) |>
  group_by(scenario, .model, h) |>
  summarise(RMSSE = sqrt(mean(sq_scaled, na.rm = TRUE)), .groups = "drop")

# RelRMSE metrics per (scenario, model, node, series, horizon)
relrmse_df <- all_fc |>
  group_by(scenario, .model, node, series, h) |>
  summarise(
    RMSE_base = sqrt(mean((.mean - Y)^2, na.rm = TRUE)),
    RMSE_sh = sqrt(mean((.reconciled_mean_shrink - Y)^2, na.rm = TRUE)),
    RMSE_cov = sqrt(mean((.reconciled_mean_cov - Y)^2, na.rm = TRUE)),
    RMSE_uni = sqrt(mean((.reconciled_uni_mean_shrink - Y)^2, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    RelRMSE_sh = 1 - RMSE_sh / RMSE_base,
    RelRMSE_cov = 1 - RMSE_cov / RMSE_base,
    RelRMSE_uni = 1 - RMSE_sh / RMSE_uni
  ) |>
  mutate(
    node_label = node_labels[node],
    series_label = series_labels[series],
    model_label = toupper(.model)
  )

# Write tables
write_rmsse_table(
  rmsse_df,
  here::here("Tabelas/Tabs_RMSSE.tex")
)

write_relrmse_tables(
  relrmse_df,
  metric_col = "RelRMSE_sh",
  metric_tex = "\\RelRMSE_{\\Base}",
  cov_phrase = "the shrinkage approach to estimate $\\boldsymbol{W}$",
  tab_start = 1,
  file = here::here("Tabelas/Tabs_sh.tex")
)

write_relrmse_tables(
  relrmse_df,
  metric_col = "RelRMSE_uni",
  metric_tex = "\\RelRMSE_{\\Uni}",
  cov_phrase = "the shrinkage approach to estimate $\\boldsymbol{W}$",
  tab_start = 10,
  file = here::here("Tabelas/Tabs_uni.tex")
)

write_relrmse_tables(
  relrmse_df,
  metric_col = "RelRMSE_cov",
  metric_tex = "\\RelRMSE_{\\Base}",
  cov_phrase = "the sample covariance to estimate $\\boldsymbol{W}$",
  tab_start = 10,
  file = here::here("Tabelas/Tabs_cov.tex")
)

# Mean RelRMSE_Base (shrinkage) per (scenario, model, horizon) — tab:sh_mean
sh_mean_df <- relrmse_df |>
  group_by(scenario, .model, h) |>
  summarise(mean_val = mean(RelRMSE_sh), .groups = "drop")

write_mean_relrmse_table(
  sh_mean_df,
  caption = paste0(
    "Average of $\\RelRMSE^{\\Base}$ of the 16 series for different forecast",
    " horizons and for each scenario. ARIMA, ETS, and VAR models for base",
    " forecasts. Using the shrinkage approach to estimate $\\bm{W}$.",
    " All values are positive, indicating reconciliation always improves",
    " upon base forecasts, on average."
  ),
  label = "tab:sh_mean",
  has_red = FALSE,
  file = here::here("Tabelas/Tabs_sh_mean.tex")
)

# % RelRMSE_Base >= 0 per (scenario, model) — tab:porc_sh
porc_sh_df <- relrmse_df |>
  group_by(scenario, .model) |>
  summarise(pct = 100 * mean(RelRMSE_sh >= 0), .groups = "drop")

write_perc_table(
  porc_sh_df,
  caption = paste0(
    "Percentage of $\\RelRMSE^{\\Base}$ greater than or equal to 0",
    " considering the 16 series in the hierarchy and the 12 forecast horizons.",
    " Using the shrinkage approach to estimate $\\bm{W}$ for the 9 scenarios."
  ),
  label = "tab:porc_sh",
  file = here::here("Tabelas/Tabs_porc_sh.tex")
)

# Mean RelRMSE_Uni (shrinkage) per (scenario, model, horizon) — tab:sh_uni_mean
uni_mean_df <- relrmse_df |>
  group_by(scenario, .model, h) |>
  summarise(mean_val = mean(RelRMSE_uni), .groups = "drop")

write_mean_relrmse_table(
  uni_mean_df,
  caption = paste0(
    "Average of $\\RelRMSE^{\\Uni}$ of the 16 series for different forecast",
    " horizons and for each scenario. ARIMA, ETS, and VAR models for base",
    " forecasts. Using the shrinkage approach to estimate $\\bm{W}$.",
    " Values in red indicate a $\\RelRMSE^{\\Uni}$ less than 0."
  ),
  label = "tab:sh_uni_mean",
  has_red = TRUE,
  file = here::here("Tabelas/Tabs_sh_uni_mean.tex")
)

# % RelRMSE_Uni >= 0 per (scenario, model) — tab:porc_sh_uni
porc_uni_df <- relrmse_df |>
  group_by(scenario, .model) |>
  summarise(pct = 100 * mean(RelRMSE_uni >= 0), .groups = "drop")

write_perc_table(
  porc_uni_df,
  caption = paste0(
    "Percentage of $\\RelRMSE^{\\Uni}$ greater than or equal to 0",
    " considering the 16 series in the hierarchy and the 12 forecast horizons.",
    " Using the shrinkage approach to estimate $\\bm{W}$ for the 9 scenarios."
  ),
  label = "tab:porc_sh_uni",
  file = here::here("Tabelas/Tabs_porc_sh_uni.tex")
)
