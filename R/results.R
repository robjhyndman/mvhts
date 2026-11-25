# ====================================================================
# Summaries, LaTeX tables and in-text numbers from the pipeline results.
# Tables are written to Tabelas/ and returned as paths (format = "file").
# ====================================================================

method_labels <- c(
  base = "Base",
  ols = "OLS",
  wls_struct = "WLS (structural)",
  separate = "MinT, separate",
  sep_blocks = "MinT, separate (joint estimate)",
  joint = "MinT, joint"
)

exp2_scenario_labels <- c(
  C1_sep_pos = "Separable, $\\rho = 0.7$",
  C2_sep_neg = "Separable, $\\rho = -0.7$",
  C3_var_dynamics = "Variable-specific dynamics",
  N1_node_corr = "Node-varying correlation",
  N2_var_sigma = "Variable-specific node covariance",
  N3_node_dynamics = "Node-varying dynamics"
)

fmt <- function(x, digits = 3) formatC(x, format = "f", digits = digits)

fmt_p <- function(p) ifelse(p < 0.001, "$<$0.001", fmt(p, 3))

# "min--max" of scale * x
fmt_range <- function(x, digits, scale = 1) {
  paste0(fmt(scale * min(x), digits), "--", fmt(scale * max(x), digits))
}

write_table <- function(lines, file) {
  fs::dir_create(dirname(file))
  writeLines(lines, file)
  file
}

# A centred LaTeX tabular with column alignment `align`, header lines and
# body rows
write_tabular <- function(file, align, header, rows) {
  write_table(
    c(
      "\\centering\\small",
      paste0("\\begin{tabular}{", align, "}"),
      "\\hline",
      header,
      "\\hline",
      rows,
      "\\hline",
      "\\end{tabular}"
    ),
    file
  )
}

# One LaTeX table row: "label & cell & cell ... \\"
latex_row <- function(label, cells) {
  paste0(label, " & ", paste(cells, collapse = " & "), " \\\\")
}

# Application tables with one column per variable and level. Columns of
# the summary are named "<variable> <level>"; these are in table order.
var_level_cols <- function() {
  as.vector(outer(
    c("Total", "Regions", "States"),
    app_series,
    \(level, variable) paste(variable, level)
  ))
}

var_level_header <- function(first) {
  c(
    latex_row("", sprintf("\\multicolumn{3}{c}{%s}", app_series_labels)),
    paste(first, "& Total & Regions & States & Total & Regions & States \\\\")
  )
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
      exp2_pop |>
        dplyr::select(
          scenario,
          pop_kappa = kappa,
          pop_gain = gain,
          incoherence
        ),
      by = "scenario"
    )
}

tab_exp2 <- function(summary, file) {
  lab <- exp2_scenario_labels
  wide <- summary |>
    dplyr::select(
      scenario,
      T,
      model,
      joint_vs_sep,
      log_ratio_se,
      kappa_hat,
      gain_hat,
      pop_kappa,
      pop_gain,
      incoherence
    ) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = c(joint_vs_sep, log_ratio_se, kappa_hat, gain_hat)
    ) |>
    dplyr::arrange(factor(scenario, levels = names(lab)), T)
  rows <- sprintf(
    "%s & %d & %s & %s & %s & %s (%s) & %s (%s) & %s \\\\",
    lab[wide$scenario],
    wide$T,
    fmt(100 * pmax(wide$incoherence, 0), 2),
    fmt(wide$pop_kappa, 2),
    fmt(100 * wide$pop_gain, 2),
    fmt(wide$joint_vs_sep_arima),
    fmt(wide$log_ratio_se_arima),
    fmt(wide$joint_vs_sep_var),
    fmt(wide$log_ratio_se_var),
    fmt(100 * wide$gain_hat_arima, 1)
  )
  write_tabular(
    file,
    "lrrrrrrr",
    c(
      " & & \\multicolumn{3}{c}{Population} & \\multicolumn{2}{c}{Joint / separate} & \\\\",
      "Scenario & $T$ & Incoh. & $\\kappa$ & Gain & ARIMA & VAR & $\\widehat\\gamma$ \\\\"
    ),
    rows
  )
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
  tab <- tab[match(names(method_labels), tab$method), ]
  rows <- vapply(
    seq_len(NROW(tab)),
    \(i) {
      latex_row(
        method_labels[[tab$method[i]]],
        fmt(unlist(tab[i, var_level_cols()]))
      )
    },
    character(1)
  )
  write_tabular(file, "lrrrrrr", var_level_header("Method"), rows)
}

# --------------------------------------------------------------------
# Application: diagnostic table
# --------------------------------------------------------------------
tab_app_diag <- function(app_diag, file) {
  rows <- sprintf(
    "%s & %d & %s & %s & %s & %s & %s & %s & %s \\\\",
    toupper(app_diag$model),
    app_diag$T,
    fmt(app_diag$kappa, 3),
    fmt(100 * app_diag$gain, 2),
    fmt(app_diag$stat_adm, 2),
    fmt(app_diag$stat_dis, 2),
    fmt_p(app_diag$p_value),
    fmt(100 * app_diag$incoherence, 1),
    fmt(app_diag$kronecker_error, 3)
  )
  write_tabular(
    file,
    "lrrrrrrrr",
    c(
      " & & & & \\multicolumn{2}{c}{$F$} & & & \\\\",
      "Base model & $T$ & $\\widehat\\kappa$ & $\\widehat\\gamma$ & Adm. & Dis. & $p$ & Incoh. & Kron. \\\\"
    ),
    rows
  )
}

# --------------------------------------------------------------------
# Application: CRPS of net change, skill relative to independent draws
# with separate reconciliation
# --------------------------------------------------------------------
app_prob_summary <- function(app_prob_scores) {
  app_prob_scores |>
    dplyr::mutate(level = node_level(node)) |>
    dplyr::group_by(model, method, level, node) |>
    dplyr::summarise(crps = mean(crps), .groups = "drop") |>
    dplyr::group_by(level, node) |>
    dplyr::mutate(
      rel = crps / crps[model == "arima" & method == "independent + separate"]
    ) |>
    dplyr::group_by(model, method, level) |>
    dplyr::summarise(rel = exp(mean(log(rel))), .groups = "drop")
}

# Point forecasts of net change: MSE relative to separate MinT of the
# ARIMA base forecasts, geometric mean over the series at each level
app_net_point <- function(app_point) {
  app_point |>
    tidyr::pivot_wider(names_from = series, values_from = error) |>
    dplyr::mutate(
      net = .data[["Admissões"]] - .data[["Demissões"]],
      level = node_level(node)
    ) |>
    dplyr::group_by(model, method, level, node) |>
    dplyr::summarise(mse = mean(net^2), .groups = "drop") |>
    dplyr::group_by(level, node) |>
    dplyr::mutate(rel = mse / mse[model == "arima" & method == "separate"]) |>
    dplyr::group_by(model, method, level) |>
    dplyr::summarise(rel = exp(mean(log(rel))), .groups = "drop")
}

tab_app_prob <- function(prob_summary, net_point, file) {
  lv <- c("Total", "Regions", "States")
  models <- c("arima", "var")
  row <- function(label, df, m) {
    cells <- unlist(lapply(models, \(mod) {
      d <- df[df$model == mod, ]
      fmt(d$rel[match(paste(m, lv), paste(d$method, d$level))])
    }))
    latex_row(label, cells)
  }
  np <- net_point
  pr <- prob_summary
  write_tabular(
    file,
    "lrrrrrr",
    c(
      " & \\multicolumn{3}{c}{ARIMA} & \\multicolumn{3}{c}{VAR} \\\\",
      paste0(" & ", paste(rep(lv, 2), collapse = " & "), " \\\\")
    ),
    c(
      "\\multicolumn{7}{l}{\\emph{Point forecasts (MSE)}} \\\\",
      row("\\quad Base", np, "base"),
      row("\\quad MinT, separate", np, "separate"),
      row("\\quad MinT, separate (joint estimate)", np, "sep_blocks"),
      row("\\quad MinT, joint", np, "joint"),
      "\\multicolumn{7}{l}{\\emph{Sample paths (CRPS)}} \\\\",
      row(
        "\\quad Independent innovations, separate reconciliation",
        pr,
        "independent + separate"
      ),
      row(
        "\\quad Joint innovations, separate reconciliation",
        pr,
        "joint + separate"
      ),
      row("\\quad Joint innovations, joint reconciliation", pr, "joint + joint")
    )
  )
}

# --------------------------------------------------------------------
# In-text numbers from Experiment 1
# --------------------------------------------------------------------
numbers_exp1 <- function(exp1_pop, exp1_summary, exp1_samp) {
  f2b <- exp1_pop |>
    dplyr::filter(family == "F2", hierarchy == "brazil") |>
    dplyr::slice_max(gain, n = 1)
  f3 <- exp1_pop |> dplyr::filter(family == "F3", hierarchy == "small")
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
    kappaNoiseSep = fmt(
      stats::median(exp1_samp$kappa_hat[
        exp1_samp$family == "F0" & exp1_samp$T %in% c(100, 200)
      ]),
      2
    )
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
  list(
    expTwoIncohRange = fmt_range(ns$incoherence, 1, 100),
    expTwoKappaRange = fmt_range(ns$pop_kappa, 2),
    expTwoGainRange = fmt_range(ns$pop_gain, 1, 100),
    expTwoSepVsBaseRange = fmt_range(1 - ar$sep_vs_base, 0, 100),
    expTwoCtrlGainMax = fmt(100 * (1 - min(ctrl400$joint_vs_sep)), 1),
    expTwoNonsepGainMax = fmt(
      100 * (1 - min(ar$joint_vs_sep[grepl("^N", ar$scenario) & ar$T == 400])),
      1
    ),
    expTwoArimaSmallRange = fmt_range(ar$joint_vs_sep[ar$T == 108], 3),
    expTwoArimaLargeRange = fmt_range(ar$joint_vs_sep[ar$T == 400], 3),
    expTwoVarMaxDiff = fmt(100 * max(abs(1 - va$joint_vs_sep)), 1),
    expTwoBlocksMaxDiff = fmt(100 * max(abs(1 - exp2_sum$blocks_vs_sep)), 1),
    # Supplementary material: reconciliation relative to the base forecasts
    suppArimaVsBaseRange = fmt_range(1 - c(ar$sep_vs_base, ar$joint / ar$base), 0, 100),
    suppVarVsBaseRange = fmt_range(1 - c(va$sep_vs_base, va$joint / va$base), 0, 100),
    suppMaxSepJointDiff = fmt(max(abs(exp2_sum$sep_vs_base - exp2_sum$joint / exp2_sum$base)), 3)
  )
}

# --------------------------------------------------------------------
# In-text numbers from the diagnostic study and the application
# --------------------------------------------------------------------
numbers_power <- function(power) {
  null <- power |> dplyr::filter(family %in% c("F0", "F1"))
  alt <- power |> dplyr::filter(!family %in% c("F0", "F1"))
  # The departure with the smallest population gain (one row per sample size)
  weak <- alt |> dplyr::filter(gain == min(gain))
  stopifnot(dplyr::n_distinct(weak$hierarchy, weak$family, weak$dial) == 1)
  # Departures with population gains below 1%
  small <- alt |> dplyr::filter(gain < 0.01)
  list(
    testSizeMin = fmt(100 * min(null$rejection_rao), 1),
    testSizeMax = fmt(100 * max(null$rejection_rao), 1),
    powerSmallMonthlyMax = fmt(100 * max(small$rejection_rao[small$T <= 200]), 0),
    # Strict upper bound on their kappa, at the next multiple of 0.1
    powerSmallKappaBound = fmt(floor(10 * max(small$kappa)) / 10 + 0.1, 1),
    powerWeakKappa = fmt(weak$kappa[1], 2),
    powerWeakGain = fmt(100 * weak$gain[1], 1),
    powerWeakRejection = fmt(100 * weak$rejection_rao[weak$T == max(weak$T)], 0)
  )
}

# Supplement: Wilks, Pillai and Hotelling-Lawley tests in Experiment 1
# and the application (Roy's largest root is computed but not reported)
numbers_supp_tests <- function(power, app_diag) {
  null <- power |> dplyr::filter(family %in% c("F0", "F1"))
  pp <- function(x) fmt(100 * x, 1)
  # Largest difference from Wilks, in percentage points
  max_diff <- function(d) {
    100 * max(abs(c(d$rejection_pillai, d$rejection_LH) - d$rejection_rao))
  }
  list(
    suppTestPillaiSize = paste0(pp(min(null$rejection_pillai)), "--", pp(max(null$rejection_pillai))),
    suppTestLHSize = paste0(pp(min(null$rejection_LH)), "--", pp(max(null$rejection_LH))),
    suppTestMaxDiff = fmt(max_diff(power), 1),
    suppTestMaxDiffLargeT = fmt(max_diff(power |> dplyr::filter(T >= 500)), 1),
    # Bound on the application p-values, for use inside $...$
    suppTestAppPBound = {
      p_max <- max(unlist(app_diag[c("p_value", "p_value_pillai", "p_value_LH")]))
      if (p_max < 0.001) "< 0.001" else paste("\\le", fmt(p_max, 3))
    }
  )
}

# Supplement: size of the test under AR(1) errors, and the serial
# correlation inflation factor of the application residuals
numbers_supp_ar <- function(ar_size, app_diag) {
  ar_size <- dplyr::mutate(ar_size, phi = param)
  pp <- function(x) fmt(100 * x, 1)
  rej <- function(h, p) ar_size$rejection_rao[ar_size$hierarchy == h & ar_size$phi == p]
  d <- split(app_diag, app_diag$model)
  phi_min <- min(ar_size$phi)
  list(
    suppArPhiMin = format(phi_min),
    suppArInflPhiMin = fmt(ar_inflation(phi_min), 3),
    suppArPhiMinMax = pp(max(ar_size$rejection_rao[ar_size$phi == phi_min])),
    suppArSmallPhiMax = fmt_range(rej("small", max(ar_size$phi)), 1, 100),
    suppArBrazilPhiMax = fmt_range(rej("brazil", max(ar_size$phi)), 1, 100),
    suppArInflArima = fmt(d$arima$serial_inflation, 3),
    suppArInflVar = fmt(d$var$serial_inflation, 3)
  )
}

# Serial-correlation inflation factor (serial_inflation()) when the
# residuals and incoherences are all AR(1) with coefficient phi
ar_inflation <- function(phi) (1 + phi^2) / (1 - phi^2)

# Supplement: size of the Wilks test with Rao's F approximation and with
# the wild bootstrap under heavy-tailed and heteroskedastic errors, its
# power under normal errors, and the application bootstrap p-values
numbers_supp_boot <- function(boot_sims, app_diag) {
  pct <- function(x) fmt_range(x, 1, 100)
  null <- boot_sims |> dplyr::filter(family %in% c("F0", "F1"))
  sel <- function(err, par = NA, h = c("small", "brazil"), Ts = c(100, 200, 500, 1000)) {
    keep <- null$error == err & null$hierarchy %in% h & null$T %in% Ts
    keep <- keep & (if (is.na(par)) is.na(null$param) else null$param %in% par)
    null[keep, ]
  }
  alt <- boot_sims |> dplyr::filter(!family %in% c("F0", "F1"))
  d <- split(app_diag, app_diag$model)
  p_text <- function(p) if (p < 0.001) "< 0.001" else paste("=", fmt(p, 3))
  garch_max <- max(null$param[null$error == "GARCH"])
  list(
    suppBootNormalRao = pct(sel("normal")$rejection_rao),
    suppBootNormal = pct(sel("normal")$rejection_boot),
    suppBootTTenRao = pct(sel("t", 10)$rejection_rao),
    suppBootTTen = pct(sel("t", 10)$rejection_boot),
    suppBootTFiveSmall = pct(sel("t", 5, "small")$rejection_boot),
    suppBootTFiveBrazilShort = pct(sel("t", 5, "brazil", c(100, 200))$rejection_boot),
    suppBootTFiveBrazilLong = pct(sel("t", 5, "brazil", 1000)$rejection_boot),
    suppBootGarchAlpha = format(garch_max),
    suppBootGarchRao = pct(sel("GARCH", garch_max)$rejection_rao),
    suppBootGarch = pct(sel("GARCH", garch_max)$rejection_boot),
    suppBootPowerMaxLoss = fmt(100 * max(alt$rejection_rao - alt$rejection_boot), 1),
    appPBootTextArima = p_text(d$arima$p_value_boot),
    appPBootTextVar = p_text(d$var$p_value_boot),
    appPBlockTwelveArima = fmt(d$arima$p_value_boot12, 3),
    appPBlockTwelveVar = fmt(d$var$p_value_boot12, 3),
    appPBlockTwentyFourArima = fmt(d$arima$p_value_boot24, 3),
    appPBlockTwentyFourVar = fmt(d$var$p_value_boot24, 3)
  )
}

numbers_app <- function(
  app_diag,
  accuracy,
  prob_summary,
  net_point,
  app_point
) {
  # Correlation between admissions and dismissals errors (ARIMA), pooled
  w <- app_point |>
    dplyr::filter(model == "arima", method %in% c("separate", "joint")) |>
    tidyr::pivot_wider(names_from = series, values_from = error)
  err_cor <- vapply(
    c("separate", "joint"),
    \(m) {
      x <- w[w$method == m, ]
      stats::cor(x[["Admissões"]], x[["Demissões"]])
    },
    numeric(1)
  )
  # Base forecast MSE of VAR relative to ARIMA, geometric mean over series
  base_mse <- accuracy |>
    dplyr::filter(method == "base") |>
    dplyr::select(model, series, node, mse) |>
    tidyr::pivot_wider(names_from = model, values_from = mse)
  var_vs_arima <- exp(mean(log(base_mse$var / base_mse$arima)))
  d <- split(app_diag, app_diag$model)
  acc <- app_accuracy_table(accuracy)
  js <- acc |>
    dplyr::filter(method %in% c("joint", "separate")) |>
    tidyr::pivot_wider(names_from = method, values_from = rel) |>
    dplyr::mutate(ratio = joint / separate)
  pr <- prob_summary
  # Percentage reductions in CRPS (sample paths) and MSE (point forecasts)
  # of net change, at each level
  skill <- function(m, l) {
    fmt(100 * (1 - pr$rel[pr$model == "arima" & pr$method == m & pr$level == l]), 0)
  }
  # Net change with VAR base forecasts: range over the base forecasts and
  # the reconciliation methods (MSE) or the three kinds of sample paths
  # (CRPS), relative to the ARIMA references, at level l
  var_net_range <- function(df, l) {
    fmt_range(df$rel[df$model == "var" & df$level == l], 2)
  }
  var_point <- net_point |>
    dplyr::filter(method %in% c("base", "separate", "sep_blocks", "joint"))
  arima_joint <- function(df, m, l) {
    fmt(df$rel[df$model == "arima" & df$method == m & df$level == l], 2)
  }
  point_skill <- function(l) {
    np <- net_point |>
      dplyr::filter(model == "arima", method == "joint", level == l)
    fmt(100 * (1 - np$rel), 0)
  }
  joint_vs_sep <- function(m, s = NULL) {
    keep <- js$model == m
    if (!is.null(s)) keep <- keep & js$series == s
    fmt_range(js$ratio[keep], 3)
  }
  p_text <- function(p) if (p < 0.001) "< 0.001" else paste("=", fmt(p, 3))
  shrink <- net_point |>
    dplyr::filter(model == "arima", method == "sep_blocks")
  # ARIMA, national dismissals: share of origins at which separate MinT
  # is worse than the base forecasts
  by_origin <- app_point |>
    dplyr::filter(
      model == "arima",
      series == "Demissões",
      node == "Total",
      method %in% c("base", "separate")
    ) |>
    dplyr::group_by(origin, method) |>
    dplyr::summarise(mse = mean(error^2), .groups = "drop") |>
    tidyr::pivot_wider(names_from = method, values_from = mse)
  sep_dis_total <- acc$rel[
    acc$model == "arima" &
      acc$method == "separate" &
      acc$series == "Demissões" &
      acc$level == "Total"
  ]
  # ARIMA dismissals: MSE of the sum of the state base forecasts relative
  # to that of the national base forecast
  base_dis <- app_point |>
    dplyr::filter(model == "arima", method == "base", series == "Demissões")
  bottom_up <- base_dis |>
    dplyr::filter(node_level(node) == "States") |>
    dplyr::group_by(origin, h) |>
    dplyr::summarise(bu = sum(error), .groups = "drop") |>
    dplyr::inner_join(
      dplyr::filter(base_dis, node == "Total"),
      by = c("origin", "h")
    )
  # ARIMA: number of series at which joint beats separate MinT
  nodes <- accuracy |>
    dplyr::filter(model == "arima", method %in% c("joint", "separate")) |>
    dplyr::select(series, node, method, rel) |>
    tidyr::pivot_wider(names_from = method, values_from = rel)
  n_joint_better <- function(s) {
    x <- nodes[nodes$series == s, ]
    sum(x$joint < x$separate)
  }
  n_series <- NROW(nodes) / 2
  # "every series", or "k of the n series"
  of_series <- function(k) {
    if (k == n_series) "every series" else paste(k, "of the", n_series, "series")
  }
  # Forecast origins, and those with all twelve horizons in the sample
  max_h <- tapply(app_point$h, app_point$origin, max)
  origins <- sort(names(max_h))
  list(
    appOrigins = length(max_h),
    appOriginsFull = sum(max_h == 12),
    appFirstOrigin = format(tsibble::yearmonth(origins[1]), "%B %Y"),
    appTArima = d$arima$T,
    appKappaArima = fmt(d$arima$kappa, 2),
    appGainArima = fmt(100 * d$arima$gain, 1),
    appPArima = fmt(d$arima$p_value, 3),
    appPTextArima = p_text(d$arima$p_value),
    appPTextVar = p_text(d$var$p_value),
    appLbArima = fmt(100 * d$arima$lb_share, 0),
    appLbVar = fmt(100 * d$var$lb_share, 0),
    appIncohArima = fmt(100 * d$arima$incoherence, 0),
    appDfOneArima = d$arima$df1,
    appDfTwoArima = fmt(d$arima$df2, 0),
    appDfOneVar = d$var$df1,
    appDfTwoVar = fmt(d$var$df2, 0),
    appKappaVar = fmt(d$var$kappa, 2),
    appGainVar = fmt(100 * d$var$gain, 1),
    appIncohVar = fmt(100 * d$var$incoherence, 0),
    appKronArima = fmt(d$arima$kronecker_error, 2),
    appJointSepArimaRange = joint_vs_sep("arima"),
    appJointSepVarRange = joint_vs_sep("var"),
    appJointSepEtsRange = joint_vs_sep("ets"),
    appJointSepArimaAdm = joint_vs_sep("arima", "Admissões"),
    appJointSepArimaDis = joint_vs_sep("arima", "Demissões"),
    appJointSepEtsAdm = joint_vs_sep("ets", "Admissões"),
    appJointSepEtsDis = joint_vs_sep("ets", "Demissões"),
    appNetShrinkMax = fmt(100 * (1 - min(shrink$rel)), 1),
    appSepDisTotalLoss = fmt(100 * (sep_dis_total - 1), 0),
    appSepDisWorseShare = fmt(100 * mean(by_origin$separate > by_origin$base), 0),
    appBottomUpDisLoss = fmt(100 * (mean(bottom_up$bu^2) / mean(bottom_up$error^2) - 1), 0),
    appNodesJointBetterAdm = of_series(n_joint_better("Admissões")),
    appNodesJointBetterDis = of_series(n_joint_better("Demissões")),
    appNetJointSepStates = skill("joint + separate", "States"),
    appNetJointSepTotal = skill("joint + separate", "Total"),
    appNetJointJointStates = skill("joint + joint", "States"),
    appNetJointJointTotal = skill("joint + joint", "Total"),
    appNetJointJointRegions = skill("joint + joint", "Regions"),
    appNetJointSepRegions = skill("joint + separate", "Regions"),
    appNetVarPointTotal = var_net_range(var_point, "Total"),
    appNetVarPointRegions = var_net_range(var_point, "Regions"),
    appNetVarCrpsTotal = var_net_range(pr, "Total"),
    appNetVarCrpsRegions = var_net_range(pr, "Regions"),
    appNetArimaJointPointTotal = arima_joint(net_point, "joint", "Total"),
    appNetArimaJointPointRegions = arima_joint(net_point, "joint", "Regions"),
    appNetArimaJointCrpsTotal = arima_joint(pr, "joint + joint", "Total"),
    appNetArimaJointCrpsRegions = arima_joint(pr, "joint + joint", "Regions"),
    appNetPointTotal = point_skill("Total"),
    appNetPointRegions = point_skill("Regions"),
    appNetPointStates = point_skill("States"),
    appNetPlugTotal = fmt(100 * d$arima$net_gain_total, 1),
    appNetPlugRegions = fmt(100 * d$arima$net_gain_regions, 1),
    appNetPlugStates = fmt(100 * d$arima$net_gain_states, 1),
    appErrCorSep = fmt(err_cor[["separate"]], 2),
    appErrCorJoint = fmt(err_cor[["joint"]], 2),
    # Percentage by which the VAR base forecasts' MSE is lower than ARIMA's
    appVarBaseGain = fmt(100 * (1 - var_vs_arima), 0)
  )
}

# ====================================================================
# Supplementary tables
# ====================================================================

# Experiment 2: MSE of separate and joint reconciliation relative to base
tab_supp_exp2 <- function(exp2_sum, file) {
  lab <- exp2_scenario_labels
  x <- exp2_sum |>
    dplyr::mutate(joint_vs_base = joint / base) |>
    dplyr::select(scenario, T, model, sep_vs_base, joint_vs_base) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = c(sep_vs_base, joint_vs_base)
    ) |>
    dplyr::arrange(factor(scenario, levels = names(lab)), T)
  rows <- sprintf(
    "%s & %d & %s & %s & %s & %s \\\\",
    lab[x$scenario],
    x$T,
    fmt(x$sep_vs_base_arima),
    fmt(x$joint_vs_base_arima),
    fmt(x$sep_vs_base_var),
    fmt(x$joint_vs_base_var)
  )
  write_tabular(
    file,
    "lrrrrr",
    c(
      " & & \\multicolumn{2}{c}{ARIMA} & \\multicolumn{2}{c}{VAR} \\\\",
      "Scenario & $T$ & Separate & Joint & Separate & Joint \\\\"
    ),
    rows
  )
}

# Experiment 1: rejection rates of the Wilks, Pillai and Hotelling-Lawley
# tests at the two shortest sample sizes, by hierarchy and covariance design
tab_supp_tests <- function(power, file, T_show = c(100, 200)) {
  stats <- c("rao", "pillai", "LH")
  stat_head <- "Wilks & Pillai & H--L"
  n_col <- 2 + length(stats) * length(T_show)
  dial_symbol <- c(F2 = "\\delta", F3 = "\\delta")
  x <- power |>
    dplyr::filter(T %in% T_show) |>
    dplyr::arrange(hierarchy != "small", family, dial, T)
  rows <- unlist(lapply(c("small", "brazil"), \(h) {
    xh <- x |> dplyr::filter(hierarchy == h)
    designs <- xh |> dplyr::distinct(family, dial, kappa)
    body <- vapply(
      seq_len(NROW(designs)),
      \(i) {
        d <- xh |>
          dplyr::filter(family == designs$family[i], dial == designs$dial[i])
        cells <- unlist(lapply(T_show, \(t) {
          fmt(unlist(d[d$T == t, paste0("rejection_", stats)]), 3)
        }))
        label <- if (designs$family[i] %in% c("F0", "F1")) {
          designs$family[i]
        } else {
          sprintf(
            "%s, $%s = %s$",
            designs$family[i],
            dial_symbol[designs$family[i]],
            format(designs$dial[i])
          )
        }
        latex_row(label, c(fmt(designs$kappa[i], 3), cells))
      },
      character(1)
    )
    c(sprintf("\\multicolumn{%d}{l}{\\emph{%s}} \\\\", n_col, hierarchy_labels[h]), body)
  }))
  write_tabular(
    file,
    paste0("l", strrep("r", n_col - 1)),
    c(
      paste0(
        " & & ",
        paste(
          sprintf("\\multicolumn{%d}{c}{$T = %d$}", length(stats), T_show),
          collapse = " & "
        ),
        " \\\\"
      ),
      paste0(
        "Design & $\\kappa$ & ",
        paste(rep(stat_head, length(T_show)), collapse = " & "),
        " \\\\"
      )
    ),
    rows
  )
}

# Supplement: rejection rates at the 5% level of the Wilks test with
# Rao's F approximation and with the wild bootstrap, and of the CR2/HTZ
# robust Wald test, under the null designs F0 and F1 with normal,
# multivariate t, GARCH and lognormal errors. CR2/HTZ is computed only for
# the small hierarchy at T <= 200 (see boot_size()), so it has columns
# only for those T and "--" for the Brazilian hierarchy.
tab_supp_boot <- function(boot_sims, file) {
  x <- boot_sims |> dplyr::filter(family %in% c("F0", "F1"))
  Ts <- sort(unique(x$T))
  Ts_cr2 <- sort(unique(x$T[!is.na(x$rejection_cr2)]))
  errs <- x |>
    dplyr::distinct(error, param) |>
    dplyr::mutate(
      ord = match(error, c("normal", "t", "GARCH", "lognormal")),
      ord2 = ifelse(error == "t", -param, param)
    ) |>
    dplyr::arrange(ord, ord2)
  err_label <- function(err, par) {
    switch(
      err,
      normal = "Normal",
      t = sprintf("$t$, %g df", par),
      GARCH = sprintf("GARCH, $\\alpha = %g$", par),
      lognormal = sprintf("Lognormal, $\\sigma = %g$", par)
    )
  }
  fmt_na <- function(x) ifelse(is.na(x), "--", fmt(x, 3))
  n_col <- 2 + 2 * length(Ts) + length(Ts_cr2)
  rows <- unlist(lapply(c("small", "brazil"), \(h) {
    body <- unlist(lapply(c("F0", "F1"), \(fam) {
      vapply(
        seq_len(NROW(errs)),
        \(i) {
          d <- x |>
            dplyr::filter(
              hierarchy == h,
              family == fam,
              error == errs$error[i],
              (is.na(param) & is.na(errs$param[i])) | param %in% errs$param[i]
            ) |>
            dplyr::arrange(T)
          latex_row(
            if (i == 1) fam else "",
            c(
              err_label(errs$error[i], errs$param[i]),
              fmt(d$rejection_rao, 3),
              fmt(d$rejection_boot, 3),
              fmt_na(d$rejection_cr2[match(Ts_cr2, d$T)])
            )
          )
        },
        character(1)
      )
    }))
    c(sprintf("\\multicolumn{%d}{l}{\\emph{%s}} \\\\", n_col, hierarchy_labels[h]), body)
  }))
  write_tabular(
    file,
    paste0("ll", strrep("r", 2 * length(Ts) + length(Ts_cr2))),
    c(
      sprintf(
        " & & \\multicolumn{%d}{c}{Rao's $F$} & \\multicolumn{%d}{c}{Wild bootstrap} & \\multicolumn{%d}{c}{CR2/HTZ} \\\\",
        length(Ts),
        length(Ts),
        length(Ts_cr2)
      ),
      paste0(
        "Design & Errors & ",
        paste(sprintf("$T = %d$", c(Ts, Ts, Ts_cr2)), collapse = " & "),
        " \\\\"
      )
    ),
    rows
  )
}

# Supplement: rejection rate of the Wilks test at the 5% level under the
# null designs F0 and F1, with serially uncorrelated (phi = 0, from the
# main size and power study) and AR(1) errors
tab_supp_ar <- function(power, ar_size, file) {
  ar_size <- dplyr::mutate(ar_size, phi = param)
  x <- dplyr::bind_rows(
    power |> dplyr::filter(family %in% c("F0", "F1")) |> dplyr::mutate(phi = 0),
    ar_size
  )
  Ts <- sort(unique(x$T))
  rows <- unlist(lapply(c("small", "brazil"), \(h) {
    xh <- x |> dplyr::filter(hierarchy == h)
    designs <- xh |> dplyr::distinct(family, phi) |> dplyr::arrange(family, phi)
    body <- vapply(
      seq_len(NROW(designs)),
      \(i) {
        d <- xh |>
          dplyr::filter(family == designs$family[i], phi == designs$phi[i]) |>
          dplyr::arrange(T)
        label <- if (designs$phi[i] == 0) designs$family[i] else ""
        latex_row(
          label,
          c(format(designs$phi[i]), fmt(ar_inflation(designs$phi[i]), 3), fmt(d$rejection_rao, 3))
        )
      },
      character(1)
    )
    c(sprintf("\\multicolumn{%d}{l}{\\emph{%s}} \\\\", 3 + length(Ts), hierarchy_labels[h]), body)
  }))
  write_tabular(
    file,
    paste0("l", strrep("r", 2 + length(Ts))),
    paste0("Design & $\\phi$ & $f$ & ", paste(sprintf("$T = %d$", Ts), collapse = " & "), " \\\\"),
    rows
  )
}

# Application: joint / separate MSE ratio by horizon, level and variable
tab_supp_horizon <- function(app_point, file, base_model = "arima") {
  x <- app_point |>
    dplyr::filter(model == base_model, method %in% c("joint", "separate")) |>
    dplyr::mutate(
      level = node_level(node),
      hgroup = cut(
        h,
        c(0, 3, 6, 9, 12),
        labels = c("1--3", "4--6", "7--9", "10--12")
      )
    ) |>
    dplyr::group_by(method, series, level, node, hgroup) |>
    dplyr::summarise(mse = mean(error^2), .groups = "drop") |>
    tidyr::pivot_wider(names_from = method, values_from = mse) |>
    dplyr::group_by(series, level, hgroup) |>
    dplyr::summarise(
      ratio = exp(mean(log(joint / separate))),
      .groups = "drop"
    ) |>
    dplyr::mutate(col = paste(series, level)) |>
    dplyr::select(hgroup, col, ratio) |>
    tidyr::pivot_wider(names_from = col, values_from = ratio)
  rows <- vapply(
    seq_len(NROW(x)),
    \(i) latex_row(x$hgroup[i], fmt(unlist(x[i, var_level_cols()]))),
    character(1)
  )
  write_tabular(file, "lrrrrrr", var_level_header("Horizon"), rows)
}

# Application: every node, MSE relative to base (ARIMA)
tab_supp_nodes <- function(accuracy, file, base_model = "arima") {
  x <- accuracy |>
    dplyr::filter(model == base_model, method %in% c("separate", "joint")) |>
    dplyr::select(series, node, level, method, rel) |>
    tidyr::pivot_wider(names_from = c(series, method), values_from = rel)
  lvl <- factor(x$level, levels = c("Total", "Regions", "States"))
  x <- x[order(lvl, x$node), ]
  # English names for the regions; states keep their abbreviations
  regions_en <- stats::setNames(
    region_meta$region_label,
    paste0("agg_", region_meta$Região)
  )
  node_lab <- dplyr::coalesce(unname(regions_en[x$node]), x$node)
  rows <- sprintf(
    "%s & %s & %s & %s & %s \\\\",
    node_lab,
    fmt(x[["Admissões_separate"]]),
    fmt(x[["Admissões_joint"]]),
    fmt(x[["Demissões_separate"]]),
    fmt(x[["Demissões_joint"]])
  )
  header <- c(
    "\\hline",
    " & \\multicolumn{2}{c}{Admissions} & \\multicolumn{2}{c}{Dismissals} \\\\",
    "Series & Separate & Joint & Separate & Joint \\\\",
    "\\hline"
  )
  write_table(
    c(
      "\\begin{longtable}{lrrrr}",
      header,
      "\\endfirsthead",
      "\\caption[]{(continued)} \\\\",
      header,
      "\\endhead",
      rows,
      "\\hline",
      "\\end{longtable}"
    ),
    file
  )
}
