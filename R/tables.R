# R/tables.R
# Helper functions for generating LaTeX table files in Tabelas/.
# Sourced by simulation.R and application.R

# ============================================================
# Node / series display labels (simulation hierarchy)
# ============================================================

# Canonical row order for supplementary tables
node_order_lab <- c("Total", "A", "B", "AA", "AB", "BA", "BB", "BC")

node_labels <- c(
  Total = "Total",
  agg_1 = "A",
  agg_2 = "B",
  `1` = "AA",
  `2` = "AB",
  `3` = "BA",
  `4` = "BB",
  `5` = "BC"
)

series_labels <- c(A = "1", B = "2")

# ============================================================
# LaTeX formatting helpers
# ============================================================

fmt3 <- function(x) formatC(x, digits = 3, format = "f")

# Colour negative values red, leave others plain
fmt_red <- function(x) {
  s <- fmt3(x)
  if (!is.na(x) && x < 0) paste0("\\textcolor{red}{", s, "}") else s
}

fmt_bold <- function(x) paste0("\\textbf{", fmt3(x), "}")

# ============================================================
# write_rmsse_table()
#   rmsse_df  — data frame with columns: scenario, .model, h, RMSSE
#   file      — output path
# ============================================================

write_rmsse_table <- function(rmsse_df, file) {
  model_order <- c("arima", "ets", "var")
  model_labels <- c(arima = "ARIMA", ets = "ETS", var = "VAR")
  lines <- c(
    "",
    "\\begin{table}",
    paste(
      "\\caption{Mean RMSSE of the reconciled multivariate forecasts by forecast",
      "horizon and base model for the nine analyzed scenarios, using the shrinkage",
      "approach to estimate $\\boldsymbol{W}$. Bold values represent the lowest",
      "values per horizon and model in each scenario.}"
    ),
    "\\centering\\resizebox{1\\textwidth}{!}{%",
    "\\begin{tabular}{lcccccccccccc}\\hline",
    "\\multirow{2}{*}{Model} & \\multicolumn{12}{c}{Forecast horizons ($h$)}",
    "\\\\ \\cline{2-13}",
    " & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & 11 & 12 \\\\"
  )
  for (sc in seq_len(9)) {
    lines <- c(
      lines,
      "\\hline",
      paste0("Scenario ", sc, "\\\\"),
      "\\hline"
    )
    sc_data <- rmsse_df |> dplyr::filter(scenario == sc)
    min_by_h <- sc_data |>
      dplyr::group_by(h) |>
      dplyr::summarise(min_val = min(RMSSE), .groups = "drop")
    for (mod in model_order) {
      mod_data <- sc_data |>
        dplyr::filter(.model == mod) |>
        dplyr::arrange(h) |>
        dplyr::left_join(min_by_h, by = "h")
      vals <- mapply(
        function(v, mv) if (abs(v - mv) < 1e-9) fmt_bold(v) else fmt3(v),
        mod_data$RMSSE,
        mod_data$min_val
      )
      lines <- c(
        lines,
        paste(c(model_labels[[mod]], vals), collapse = " & ") |> paste0("\\\\")
      )
    }
  }
  lines <- c(
    lines,
    "\\hline\\end{tabular}}\\label{tab:RMSSE}\\end{table}"
  )
  writeLines(lines, file)
}

# ============================================================
# write_relrmse_tables()
#   relrmse_df  — data frame with node_label, series_label, model_label,
#                 scenario, .model, h, and the metric columns
#   metric_col  — column to use: "RelRMSE_sh", "RelRMSE_cov", "RelRMSE_uni"
#   metric_tex  — LaTeX string for the metric name in captions
#   cov_phrase  — description of the covariance method for the caption
#   tab_start   — number of first supplementary table (e.g. 1 or 10)
#   file        — output path
# ============================================================

write_relrmse_tables <- function(
  relrmse_df,
  metric_col,
  metric_tex,
  cov_phrase,
  tab_start,
  file
) {
  model_order <- c("arima", "ets", "var")
  model_labels <- c(arima = "ARIMA", ets = "ETS", var = "VAR")

  lines <- character(0)

  for (sc in 1:9) {
    tab_num <- tab_start + sc - 1

    caption <- paste0(
      "\\caption*{Table S",
      tab_num,
      ": $",
      metric_tex,
      "$ for all series in the hierarchy across different forecast horizons.",
      " ARIMA, ETS, and VAR models for base forecasts.",
      " Using ",
      cov_phrase,
      " under scenario ",
      sc,
      ". Values in red indicate a $",
      metric_tex,
      "$ less than 0.}"
    )

    lines <- c(
      lines,
      if (sc > 1) "" else character(0),
      "\\begin{table}",
      caption,
      "\\centering\\resizebox{1\\textwidth}{!}{%",
      "\\begin{tabular}{llcccccccccccc}\\hline",
      "\\multirow{2}{*}{Series} & \\multirow{2}{*}{Variable}",
      "& \\multicolumn{12}{c}{Forecast horizons ($h$)} \\\\ \\cline{3-14}",
      "& & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & 11 & 12 \\\\ \\hline"
    )

    sc_data <- relrmse_df |>
      dplyr::filter(scenario == sc) |>
      dplyr::mutate(
        node_label = factor(node_labels[node], levels = node_order_lab)
      )

    for (mod in model_order) {
      lines <- c(
        lines,
        paste0(
          model_labels[[mod]],
          " &  &  &  &  &  &  &  &  &  &  &  &  & \\\\"
        ),
        "\\hline"
      )

      mod_data <- sc_data |> dplyr::filter(.model == mod)

      for (nd_lab in node_order_lab) {
        for (ser_lab in c("1", "2")) {
          row_data <- mod_data |>
            dplyr::filter(
              as.character(node_label) == nd_lab,
              series_label == ser_lab
            ) |>
            dplyr::arrange(h)
          if (nrow(row_data) == 0) {
            next
          }
          vals <- vapply(row_data[[metric_col]], fmt_red, character(1))
          lines <- c(
            lines,
            paste(c(nd_lab, ser_lab, vals), collapse = " & ") |> paste0("\\\\")
          )
        }
      }
      lines <- c(lines, "\\hline")
    }

    lines <- c(
      lines,
      "    \\end{tabular}%",
      "    }",
      "\\end{table}"
    )
  }

  writeLines(lines, file)
}

# ============================================================
# write_mean_relrmse_table()
#   df         — data frame with columns: scenario, .model, h, mean_val
#   caption    — full caption text (without \caption{})
#   label      — LaTeX label string (without \label{})
#   has_red    — colour negative values red?
#   file       — output path
# ============================================================

write_mean_relrmse_table <- function(df, caption, label, has_red, file) {
  model_order <- c("arima", "ets", "var")
  model_labels <- c(arima = "ARIMA", ets = "ETS", var = "VAR")
  fmt_cell <- if (has_red) fmt_red else fmt3

  lines <- c(
    "\\begin{table}",
    paste0("  \\caption{", caption, "}"),
    "  \\centering",
    "  \\resizebox{1\\textwidth}{!}{%",
    "    \\begin{tabular}{lrrrrrrrrrrrr}",
    "      \\hline",
    paste0(
      "      \\multirow{2}{*}{Model} &",
      " \\multicolumn{12}{c}{Forecast horizons ($h$)} \\\\ \\cline{2-13}"
    ),
    "      & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & 11 & 12 \\\\"
  )

  for (sc in 1:9) {
    lines <- c(
      lines,
      "      \\hline",
      paste0("      Scenario ", sc, " & & & & & & & & & & & & \\\\"),
      "      \\hline"
    )
    sc_data <- df |> dplyr::filter(scenario == sc)
    for (mod in model_order) {
      mod_data <- sc_data |>
        dplyr::filter(.model == mod) |>
        dplyr::arrange(h)
      vals <- vapply(mod_data$mean_val, fmt_cell, character(1))
      lines <- c(
        lines,
        paste(
          c(paste0("      ", model_labels[[mod]]), vals),
          collapse = " & "
        ) |>
          paste0(" \\\\")
      )
    }
  }

  lines <- c(
    lines,
    "      \\hline",
    "    \\end{tabular}%",
    "  }",
    paste0("  \\label{", label, "}"),
    "\\end{table}"
  )
  writeLines(lines, file)
}

# ============================================================
# write_perc_table()
#   df      — data frame with columns: scenario, .model, pct
#   caption — full caption text
#   label   — LaTeX label string
#   file    — output path
# ============================================================

write_perc_table <- function(df, caption, label, file) {
  model_order <- c("arima", "ets", "var")
  model_labels <- c(arima = "ARIMA", ets = "ETS", var = "VAR")
  fmt_pct <- function(x) formatC(x, digits = 1, format = "f")

  lines <- c(
    "\\begin{table}",
    paste0("  \\caption{", caption, "}"),
    "  \\centering",
    "  \\begin{tabular}{lrrrrrrrrr}",
    "    \\hline",
    paste0(
      "    \\multirow{2}{*}{Model} & \\multicolumn{9}{c}{Scenario}",
      " \\\\ \\cline{2-10}"
    ),
    paste0(
      "    & \\multicolumn{1}{c}{1} & \\multicolumn{1}{c}{2}",
      " & \\multicolumn{1}{c}{3} & \\multicolumn{1}{c}{4}",
      " & \\multicolumn{1}{c}{5} & 6 & 7 & 8 & 9 \\\\ \\hline"
    )
  )

  for (mod in model_order) {
    mod_data <- df |>
      dplyr::filter(.model == mod) |>
      dplyr::arrange(scenario)
    vals <- vapply(mod_data$pct, fmt_pct, character(1))
    lines <- c(
      lines,
      paste(c(paste0("    ", model_labels[[mod]]), vals), collapse = " & ") |>
        paste0(" \\\\")
    )
  }

  lines <- c(
    lines,
    "    \\hline",
    "  \\end{tabular}%",
    paste0("  \\label{", label, "}"),
    "\\end{table}"
  )
  writeLines(lines, file)
}

# ============================================================
# write_app_relrmse_table()
#   df              — data frame with columns: node, h, RelRMSE_Base
#   node_order      — character vector of node names in display order
#   node_labels_map — named character vector: node name -> display label
#   caption         — full caption text
#   label           — LaTeX label string
#   file            — output path
# ============================================================

write_app_relrmse_table <- function(
  df,
  node_order,
  node_labels_map,
  caption,
  label,
  file
) {
  lines <- c(
    "\\begin{table}",
    paste0("  \\caption{", caption, "}"),
    "  \\centering",
    "  \\resizebox{1\\textwidth}{!}{%",
    "    \\begin{tabular}{lrrrrrrrrrrrr}",
    "      \\hline",
    paste0(
      "      \\multirow{2}{*}{Series} &",
      " \\multicolumn{12}{c}{Forecast horizons ($h$)} \\\\ \\cline{2-13}"
    ),
    "      & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & 11 & 12 \\\\ \\hline"
  )

  for (nd in node_order) {
    row_data <- df |>
      dplyr::filter(node == nd) |>
      dplyr::arrange(h)
    if (nrow(row_data) == 0) {
      next
    }
    nd_lab <- node_labels_map[[nd]]
    vals <- vapply(row_data$RelRMSE_Base, fmt_red, character(1))
    lines <- c(
      lines,
      paste(c(paste0("      ", nd_lab), vals), collapse = " & ") |>
        paste0(" \\\\")
    )
  }

  lines <- c(
    lines,
    "      \\hline",
    "    \\end{tabular}%",
    "  }",
    paste0("  \\label{", label, "}"),
    "\\end{table}"
  )
  writeLines(lines, file)
}
