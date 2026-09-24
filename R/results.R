# ====================================================================
# Summaries, LaTeX tables and in-text numbers from the pipeline results.
# Tables are written to Tabelas/ and returned as paths (format = "file").
# ====================================================================

node_level <- function(node) {
  dplyr::case_when(
    node == "Total" ~ "Total",
    grepl("^agg_", node) ~ "Regions",
    TRUE ~ "States"
  )
}

method_labels <- c(
  base = "Base",
  ols = "OLS",
  wls_struct = "WLS (structural)",
  separate = "MinT, separate",
  sep_blocks = "MinT, separate (joint estimate)",
  joint = "MinT, joint"
)

app_series_labels <- c("Admissões" = "Admissions", "Demissões" = "Dismissals")

fmt <- function(x, digits = 3) formatC(x, format = "f", digits = digits)

fmt_p <- function(p) ifelse(p < 0.001, "$<$0.001", fmt(p, 3))

write_table <- function(lines, file) {
  fs::dir_create(dirname(file))
  writeLines(lines, file)
  file
}

# --------------------------------------------------------------------
# Experiment 2 summary: mean MSE over horizons and replications,
# relative to separate reconciliation, with population quantities
# --------------------------------------------------------------------
exp2_summary <- function(exp2_sims, exp2_pop) {
  sims <- exp2_sims |>
    dplyr::group_by(scenario, T, model, method) |>
    dplyr::summarise(
      mse = mean(mse),
      kappa_hat = mean(kappa_hat),
      gain_hat = mean(gain_hat),
      reps = dplyr::n_distinct(batch, rep),
      .groups = "drop"
    )
  failed <- exp2_sims |>
    dplyr::distinct(scenario, T, batch, failed) |>
    dplyr::group_by(scenario, T) |>
    dplyr::summarise(failed = sum(failed), .groups = "drop")
  ratios <- sims |>
    dplyr::select(scenario, T, model, method, mse) |>
    tidyr::pivot_wider(names_from = method, values_from = mse) |>
    dplyr::mutate(
      joint_vs_sep = joint / separate,
      blocks_vs_sep = sep_blocks / separate,
      sep_vs_base = separate / base
    )
  diag <- sims |>
    dplyr::filter(method == "joint") |>
    dplyr::select(scenario, T, model, kappa_hat, gain_hat, reps)
  # Paired standard error of the log ratio joint / separate, from the
  # per-replication mean MSE over horizons
  paired <- exp2_sims |>
    dplyr::filter(method %in% c("joint", "separate")) |>
    dplyr::group_by(scenario, T, model, batch, rep, method) |>
    dplyr::summarise(mse = mean(mse), .groups = "drop") |>
    tidyr::pivot_wider(names_from = method, values_from = mse) |>
    dplyr::group_by(scenario, T, model) |>
    dplyr::summarise(
      log_ratio_se = stats::sd(log(joint / separate)) / sqrt(dplyr::n()),
      .groups = "drop"
    )
  ratios |>
    dplyr::left_join(paired, by = c("scenario", "T", "model")) |>
    dplyr::left_join(diag, by = c("scenario", "T", "model")) |>
    dplyr::left_join(failed, by = c("scenario", "T")) |>
    dplyr::left_join(
      exp2_pop |> dplyr::select(scenario, pop_kappa = kappa, pop_gain = gain, incoherence),
      by = "scenario"
    )
}

tab_exp2 <- function(summary, file) {
  lab <- c(
    C1_sep_pos = "Separable, $\\rho = 0.7$",
    C2_sep_neg = "Separable, $\\rho = -0.7$",
    C3_var_dynamics = "Variable-specific dynamics",
    N1_node_corr = "Node-varying correlation",
    N2_var_sigma = "Variable-specific node covariance",
    N3_node_dynamics = "Node-varying dynamics"
  )
  wide <- summary |>
    dplyr::select(scenario, T, model, joint_vs_sep, log_ratio_se, kappa_hat, gain_hat, pop_kappa, pop_gain, incoherence) |>
    tidyr::pivot_wider(names_from = model, values_from = c(joint_vs_sep, log_ratio_se, kappa_hat, gain_hat)) |>
    dplyr::arrange(factor(scenario, levels = names(lab)), T)
  rows <- sprintf(
    "%s & %d & %s & %s & %s & %s (%s) & %s (%s) & %s \\\\",
    lab[wide$scenario], wide$T,
    fmt(100 * pmax(wide$incoherence, 0), 2), fmt(wide$pop_kappa, 2), fmt(100 * wide$pop_gain, 2),
    fmt(wide$joint_vs_sep_arima), fmt(wide$log_ratio_se_arima),
    fmt(wide$joint_vs_sep_var), fmt(wide$log_ratio_se_var),
    fmt(100 * wide$gain_hat_arima, 1)
  )
  write_table(c(
    "\\begin{table}[!htb]",
    "\\centering\\small",
    "\\caption{Experiment 2. Population quantities for the optimal AR forecasts: incoherent share of error variance (\\%), $\\kappa$, and MSE reduction from joint reconciliation (\\%). Fitted base models: MSE of joint relative to separate reconciliation, averaged over 500 replications, 16 series and horizons 1--12, with the standard error of the log ratio in parentheses; mean plug-in gain $\\widehat\\gamma$ (\\%, ARIMA).}",
    "\\label{tab:exp2}",
    "\\begin{tabular}{lrrrrrrr}",
    "\\hline",
    " & & \\multicolumn{3}{c}{Population} & \\multicolumn{2}{c}{Joint / separate} & \\\\",
    "Scenario & $T$ & Incoh. & $\\kappa$ & Gain & ARIMA & VAR & $\\widehat\\gamma$ \\\\",
    "\\hline",
    rows,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  ), file)
}

# --------------------------------------------------------------------
# Application: point forecast accuracy.
# For each series, MSE over origins and horizons relative to base; then
# geometric mean over the series in each level.
# --------------------------------------------------------------------
app_accuracy <- function(app_point) {
  app_point |>
    dplyr::group_by(model, method, series, node) |>
    dplyr::summarise(mse = mean(error^2), .groups = "drop") |>
    dplyr::group_by(model, series, node) |>
    dplyr::mutate(rel = mse / mse[method == "base"]) |>
    dplyr::ungroup() |>
    dplyr::mutate(level = node_level(node))
}

app_accuracy_table <- function(accuracy) {
  accuracy |>
    dplyr::group_by(model, method, series, level) |>
    dplyr::summarise(rel = exp(mean(log(rel))), .groups = "drop")
}

tab_app_accuracy <- function(accuracy, file, base_model = "arima") {
  tab <- app_accuracy_table(accuracy) |>
    dplyr::filter(model == base_model) |>
    dplyr::mutate(col = paste(series, level)) |>
    dplyr::select(method, col, rel) |>
    tidyr::pivot_wider(names_from = col, values_from = rel)
  order_cols <- as.vector(outer(names(app_series_labels), c("Total", "Regions", "States"), paste))
  order_cols <- order_cols[c(1, 3, 5, 2, 4, 6)]
  tab <- tab[match(names(method_labels), tab$method), c("method", order_cols)]
  rows <- apply(tab, 1, \(r) {
    paste0(method_labels[r[["method"]]], " & ", paste(fmt(as.numeric(r[-1])), collapse = " & "), " \\\\")
  })
  write_table(c(
    "\\begin{table}[!htb]",
    "\\centering\\small",
    sprintf("\\caption{Application: MSE relative to base forecasts (%s), geometric mean over the series at each level, averaged over %s forecast origins and horizons 1--12.}", toupper(base_model), "48"),
    sprintf("\\label{tab:app-accuracy-%s}", base_model),
    "\\begin{tabular}{lrrrrrr}",
    "\\hline",
    " & \\multicolumn{3}{c}{Admissions} & \\multicolumn{3}{c}{Dismissals} \\\\",
    "Method & Total & Regions & States & Total & Regions & States \\\\",
    "\\hline",
    rows,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  ), file)
}

# --------------------------------------------------------------------
# Application: diagnostic table
# --------------------------------------------------------------------
tab_app_diag <- function(app_diag, file) {
  rows <- sprintf(
    "%s & %d & %s & %s & %s & %s & %s & %s & %s \\\\",
    toupper(app_diag$model), app_diag$T,
    fmt(app_diag$kappa, 3), fmt(100 * app_diag$gain, 2),
    fmt(app_diag$stat_adm, 2), fmt(app_diag$stat_dis, 2), fmt_p(app_diag$p_value),
    fmt(100 * app_diag$incoherence, 1), fmt(app_diag$kronecker_error, 3)
  )
  write_table(c(
    "\\begin{table}[!htb]",
    "\\centering\\small",
    sprintf("\\caption{Application: diagnostics from the one-step residuals over 2007--2019. Estimated $\\widehat\\kappa$ and plug-in gain $\\widehat\\gamma$ (\\%%); $F$ statistics ($%d$ and $%.0f$ degrees of freedom) for the other variable's incoherences in the regression of each variable's errors, and the Bonferroni $p$-value; incoherent share of error variance (\\%%); relative error of the nearest Kronecker product.}", app_diag$df1[1], app_diag$df2[1]),
    "\\label{tab:app-diag}",
    "\\begin{tabular}{lrrrrrrrr}",
    "\\hline",
    " & & & & \\multicolumn{2}{c}{$F$} & & & \\\\",
    "Base model & $T$ & $\\widehat\\kappa$ & $\\widehat\\gamma$ & Adm. & Dis. & $p$ & Incoh. & Kron. \\\\",
    "\\hline",
    rows,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  ), file)
}

# --------------------------------------------------------------------
# Application: CRPS of net change, skill relative to independent draws
# with separate reconciliation
# --------------------------------------------------------------------
app_prob_summary <- function(app_prob_scores) {
  app_prob_scores |>
    dplyr::mutate(level = node_level(node)) |>
    dplyr::group_by(method, level, node) |>
    dplyr::summarise(crps = mean(crps), .groups = "drop") |>
    dplyr::group_by(level, node) |>
    dplyr::mutate(rel = crps / crps[method == "independent + separate"]) |>
    dplyr::group_by(method, level) |>
    dplyr::summarise(rel = exp(mean(log(rel))), .groups = "drop")
}

# Point forecasts of net change: MSE relative to separate MinT,
# geometric mean over the series at each level
app_net_point <- function(app_point) {
  app_point |>
    tidyr::pivot_wider(names_from = series, values_from = error) |>
    dplyr::mutate(net = .data[["Admissões"]] - .data[["Demissões"]], level = node_level(node)) |>
    dplyr::group_by(model, method, level, node) |>
    dplyr::summarise(mse = mean(net^2), .groups = "drop") |>
    dplyr::group_by(model, level, node) |>
    dplyr::mutate(rel = mse / mse[method == "separate"]) |>
    dplyr::group_by(model, method, level) |>
    dplyr::summarise(rel = exp(mean(log(rel))), .groups = "drop")
}

tab_app_prob <- function(prob_summary, net_point, file) {
  lv <- c("Total", "Regions", "States")
  row <- function(label, df, m) {
    v <- df$rel[match(paste(m, lv), paste(df$method, df$level))]
    paste0(label, " & ", paste(fmt(v), collapse = " & "), " \\\\")
  }
  np <- net_point |> dplyr::filter(model == "arima")
  write_table(c(
    "\\begin{table}[!htb]",
    "\\centering\\small",
    "\\caption{Application: forecasts of net employment change (admissions minus dismissals), ARIMA base models, geometric mean over the series at each level, 48 origins, horizons 1--12. Top: MSE of point forecasts relative to separate MinT. Bottom: CRPS from 1000 sample paths relative to independent innovations with separate reconciliation.}",
    "\\label{tab:app-prob}",
    "\\begin{tabular}{lrrr}",
    "\\hline",
    " & Total & Regions & States \\\\",
    "\\hline",
    "\\multicolumn{4}{l}{\\emph{Point forecasts (MSE)}} \\\\",
    row("\\quad MinT, separate", np, "separate"),
    row("\\quad MinT, separate (joint estimate)", np, "sep_blocks"),
    row("\\quad MinT, joint", np, "joint"),
    "\\multicolumn{4}{l}{\\emph{Sample paths (CRPS)}} \\\\",
    row("\\quad Independent innovations, separate reconciliation", prob_summary, "independent + separate"),
    row("\\quad Joint innovations, separate reconciliation", prob_summary, "joint + separate"),
    row("\\quad Joint innovations, joint reconciliation", prob_summary, "joint + joint"),
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  ), file)
}

# --------------------------------------------------------------------
# In-text numbers as LaTeX macros, so no number is typed by hand.
# `values` is a named list; names become macro names (letters only).
# --------------------------------------------------------------------
write_numbers <- function(values, file) {
  stopifnot(all(grepl("^[A-Za-z]+$", names(values))))
  lines <- sprintf("\\newcommand{\\%s}{%s}", names(values), unlist(values))
  write_table(c("% Generated by the targets pipeline; do not edit.", lines), file)
}

# --------------------------------------------------------------------
# In-text numbers from Experiment 1
# --------------------------------------------------------------------
numbers_exp1 <- function(exp1_pop, exp1_summary, exp1_samp) {
  f2b <- exp1_pop |> dplyr::filter(family == "F2", hierarchy == "brazil") |> dplyr::slice_max(gain, n = 1)
  f3 <- exp1_pop |> dplyr::filter(family == "F3")
  sep <- exp1_summary |> dplyr::filter(family == "F0", T == min(T))
  weak <- exp1_summary |>
    dplyr::filter(family == "F3", hierarchy == "brazil") |>
    dplyr::filter(dial == min(dial), T == max(T))
  list(
    expOneReps = max(exp1_samp$rep),
    gainFtwoBrazilMax = fmt(100 * f2b$gain, 1),
    kappaFtwoBrazilMax = fmt(f2b$kappa, 2),
    gainFthreeSmallMax = fmt(100 * max(f3$gain), 1),
    sepPenaltySmall = fmt(100 * (sep$ratio[sep$hierarchy == "small"] - 1), 1),
    sepPenaltyBrazil = fmt(100 * (sep$ratio[sep$hierarchy == "brazil"] - 1), 1),
    gainFthreeWeakBrazil = fmt(100 * (1 - weak$oracle_ratio), 1),
    gainFthreeWeakBrazilEst = fmt(100 * (1 - weak$ratio), 1),
    kappaNoiseSep = fmt(stats::median(exp1_samp$kappa_hat[exp1_samp$family == "F0" & exp1_samp$T %in% c(100, 200)]), 2)
  )
}

# --------------------------------------------------------------------
# In-text numbers from Experiment 2
# --------------------------------------------------------------------
numbers_exp2 <- function(exp2_sum) {
  pop <- dplyr::distinct(exp2_sum, scenario, pop_kappa, pop_gain, incoherence)
  ns <- pop |> dplyr::filter(grepl("^N", scenario))
  ar <- exp2_sum |> dplyr::filter(model == "arima")
  ctrl400 <- ar |> dplyr::filter(grepl("^C", scenario), T == 400)
  va <- exp2_sum |> dplyr::filter(model == "var")
  rng <- function(x, d, scale = 1) paste0(fmt(scale * min(x), d), "--", fmt(scale * max(x), d))
  list(
    expTwoIncohRange = rng(ns$incoherence, 1, 100),
    expTwoKappaRange = rng(ns$pop_kappa, 2),
    expTwoGainRange = rng(ns$pop_gain, 1, 100),
    expTwoSepVsBaseRange = rng(1 - ar$sep_vs_base, 0, 100),
    expTwoCtrlGainMax = fmt(100 * (1 - min(ctrl400$joint_vs_sep)), 1),
    expTwoArimaSmallRange = rng(ar$joint_vs_sep[ar$T == 108], 3),
    expTwoArimaLargeRange = rng(ar$joint_vs_sep[ar$T == 400], 3),
    expTwoVarMaxDiff = fmt(100 * max(abs(1 - va$joint_vs_sep)), 1),
    expTwoBlocksMaxDiff = fmt(100 * max(abs(1 - exp2_sum$blocks_vs_sep)), 1)
  )
}

# --------------------------------------------------------------------
# In-text numbers from the diagnostic study and the application
# --------------------------------------------------------------------
numbers_power <- function(power) {
  null <- power |> dplyr::filter(family %in% c("F0", "F1"))
  list(
    testSizeRange = paste0(fmt(100 * min(null$rejection), 1), "--", fmt(100 * max(null$rejection), 1))
  )
}

numbers_app <- function(app_diag, accuracy, prob_summary, net_point) {
  d <- split(app_diag, app_diag$model)
  acc <- app_accuracy_table(accuracy)
  js <- acc |>
    dplyr::filter(method %in% c("joint", "separate")) |>
    tidyr::pivot_wider(names_from = method, values_from = rel) |>
    dplyr::mutate(ratio = joint / separate)
  pr <- prob_summary
  skill <- function(m, l) fmt(100 * (1 - pr$rel[pr$method == m & pr$level == l]), 0)
  list(
    appTArima = d$arima$T,
    appKappaArima = fmt(d$arima$kappa, 2),
    appGainArima = fmt(100 * d$arima$gain, 1),
    appPArima = fmt(d$arima$p_value, 3),
    appIncohArima = fmt(100 * d$arima$incoherence, 0),
    appKappaVar = fmt(d$var$kappa, 2),
    appGainVar = fmt(100 * d$var$gain, 1),
    appIncohVar = fmt(100 * d$var$incoherence, 0),
    appKronArima = fmt(d$arima$kronecker_error, 2),
    appJointSepArimaRange = paste0(
      fmt(min(js$ratio[js$model == "arima"]), 3), "--", fmt(max(js$ratio[js$model == "arima"]), 3)
    ),
    appJointSepVarRange = paste0(
      fmt(min(js$ratio[js$model == "var"]), 3), "--", fmt(max(js$ratio[js$model == "var"]), 3)
    ),
    appJointSepEtsRange = paste0(
      fmt(min(js$ratio[js$model == "ets"]), 3), "--", fmt(max(js$ratio[js$model == "ets"]), 3)
    ),
    appNetJointSepStates = skill("joint + separate", "States"),
    appNetJointSepTotal = skill("joint + separate", "Total"),
    appNetJointJointStates = skill("joint + joint", "States"),
    appNetJointJointTotal = skill("joint + joint", "Total"),
    appNetJointJointRegions = skill("joint + joint", "Regions"),
    appNetJointSepRegions = skill("joint + separate", "Regions"),
    appNetPointTotal = fmt(100 * (1 - net_point$rel[net_point$model == "arima" & net_point$method == "joint" & net_point$level == "Total"]), 0),
    appNetPointRegions = fmt(100 * (1 - net_point$rel[net_point$model == "arima" & net_point$method == "joint" & net_point$level == "Regions"]), 0),
    appNetPointStates = fmt(100 * (1 - net_point$rel[net_point$model == "arima" & net_point$method == "joint" & net_point$level == "States"]), 0),
    appNetPlugTotal = fmt(100 * d$arima$net_gain_total, 1),
    appNetPlugRegions = fmt(100 * d$arima$net_gain_regions, 1),
    appNetPlugStates = fmt(100 * d$arima$net_gain_states, 1),
    appSepVsBaseStatesAdm = fmt(100 * (1 - acc$rel[acc$model == "arima" & acc$method == "separate" & acc$series == "Admissões" & acc$level == "States"]), 1)
  )
}

# ====================================================================
# Supplementary tables
# ====================================================================

# Experiment 2: MSE of separate and joint reconciliation relative to base
tab_supp_exp2 <- function(exp2_sum, file) {
  lab <- c(
    C1_sep_pos = "Separable, $\\rho = 0.7$",
    C2_sep_neg = "Separable, $\\rho = -0.7$",
    C3_var_dynamics = "Variable-specific dynamics",
    N1_node_corr = "Node-varying correlation",
    N2_var_sigma = "Variable-specific node covariance",
    N3_node_dynamics = "Node-varying dynamics"
  )
  x <- exp2_sum |>
    dplyr::mutate(joint_vs_base = joint / base) |>
    dplyr::select(scenario, T, model, sep_vs_base, joint_vs_base) |>
    tidyr::pivot_wider(names_from = model, values_from = c(sep_vs_base, joint_vs_base)) |>
    dplyr::arrange(factor(scenario, levels = names(lab)), T)
  rows <- sprintf(
    "%s & %d & %s & %s & %s & %s \\\\",
    lab[x$scenario], x$T,
    fmt(x$sep_vs_base_arima), fmt(x$joint_vs_base_arima),
    fmt(x$sep_vs_base_var), fmt(x$joint_vs_base_var)
  )
  write_table(c(
    "\\begin{table}[!htb]",
    "\\centering\\small",
    "\\caption{Experiment 2: MSE of separate and joint MinT reconciliation relative to the base forecasts, averaged over 500 replications, 16 series and horizons 1--12.}",
    "\\label{tab:supp-exp2}",
    "\\begin{tabular}{lrrrrr}",
    "\\hline",
    " & & \\multicolumn{2}{c}{ARIMA} & \\multicolumn{2}{c}{VAR} \\\\",
    "Scenario & $T$ & Separate & Joint & Separate & Joint \\\\",
    "\\hline",
    rows,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  ), file)
}

# Application: joint / separate MSE ratio by horizon, level and variable
tab_supp_horizon <- function(app_point, file, base_model = "arima") {
  x <- app_point |>
    dplyr::filter(model == base_model, method %in% c("joint", "separate")) |>
    dplyr::mutate(
      level = node_level(node),
      hgroup = cut(h, c(0, 3, 6, 9, 12), labels = c("1--3", "4--6", "7--9", "10--12"))
    ) |>
    dplyr::group_by(method, series, level, node, hgroup) |>
    dplyr::summarise(mse = mean(error^2), .groups = "drop") |>
    tidyr::pivot_wider(names_from = method, values_from = mse) |>
    dplyr::group_by(series, level, hgroup) |>
    dplyr::summarise(ratio = exp(mean(log(joint / separate))), .groups = "drop") |>
    dplyr::mutate(col = paste(series, level)) |>
    dplyr::select(hgroup, col, ratio) |>
    tidyr::pivot_wider(names_from = col, values_from = ratio)
  order_cols <- as.vector(outer(names(app_series_labels), c("Total", "Regions", "States"), paste))[c(1, 3, 5, 2, 4, 6)]
  rows <- apply(x[, c("hgroup", order_cols)], 1, \(r) {
    paste0(r[[1]], " & ", paste(fmt(as.numeric(r[-1])), collapse = " & "), " \\\\")
  })
  write_table(c(
    "\\begin{table}[!htb]",
    "\\centering\\small",
    sprintf("\\caption{Application (%s base forecasts): MSE of joint relative to separate MinT reconciliation by forecast horizon, geometric mean over the series at each level.}", toupper(base_model)),
    sprintf("\\label{tab:supp-horizon-%s}", base_model),
    "\\begin{tabular}{lrrrrrr}",
    "\\hline",
    " & \\multicolumn{3}{c}{Admissions} & \\multicolumn{3}{c}{Dismissals} \\\\",
    "Horizon & Total & Regions & States & Total & Regions & States \\\\",
    "\\hline",
    rows,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  ), file)
}

# Application: every node, MSE relative to base (ARIMA)
tab_supp_nodes <- function(accuracy, file, base_model = "arima") {
  x <- accuracy |>
    dplyr::filter(model == base_model, method %in% c("separate", "joint")) |>
    dplyr::select(series, node, level, method, rel) |>
    tidyr::pivot_wider(names_from = c(series, method), values_from = rel)
  lvl <- factor(x$level, levels = c("Total", "Regions", "States"))
  x <- x[order(lvl, x$node), ]
  regions_en <- c(
    "Centro-Oeste" = "Midwest", "Nordeste" = "Northeast", "Norte" = "North",
    "Sudeste" = "Southeast", "Sul" = "South"
  )
  node_lab <- sub("^agg_", "", x$node)
  node_lab <- dplyr::coalesce(unname(regions_en[node_lab]), node_lab)
  rows <- sprintf(
    "%s & %s & %s & %s & %s \\\\",
    node_lab,
    fmt(x[["Admissões_separate"]]), fmt(x[["Admissões_joint"]]),
    fmt(x[["Demissões_separate"]]), fmt(x[["Demissões_joint"]])
  )
  write_table(c(
    "\\begin{longtable}{lrrrr}",
    sprintf("\\caption{Application (%s base forecasts): MSE of separate and joint MinT reconciliation relative to the base forecasts for every series, averaged over 48 origins and horizons 1--12.}\\label{tab:supp-nodes} \\\\", toupper(base_model)),
    "\\hline",
    " & \\multicolumn{2}{c}{Admissions} & \\multicolumn{2}{c}{Dismissals} \\\\",
    "Series & Separate & Joint & Separate & Joint \\\\",
    "\\hline",
    "\\endhead",
    rows,
    "\\hline",
    "\\end{longtable}"
  ), file)
}
