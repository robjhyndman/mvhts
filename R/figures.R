# ====================================================================
# Shared figure style and Experiment 1 figures.
# Figures are written as PDF files and returned as paths, so they can be
# format = "file" targets.
# ====================================================================

# Categorical colours in fixed order (validated reference palette)
fig_colours <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")

# Sequential ramp (one hue, light to dark) for ordered dial strengths
fig_ramp <- c("#86b6ef", "#3987e5", "#1c5cab", "#0d366b")

hierarchy_labels <- c(
  small = "Small hierarchy (8 series)",
  brazil = "Brazil hierarchy (33 series)"
)

family_labels <- c(
  F0 = "Separable",
  F1 = "Separable + invisible",
  F2 = "Variable-specific profiles",
  F3 = "Node-varying correlation",
  F4 = "OLS case"
)

theme_paper <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = "grey90",
        linewidth = 0.3
      ),
      axis.line = ggplot2::element_line(colour = "grey40", linewidth = 0.3),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(4, 8, 4, 4)
    )
}

save_figure <- function(plot, file, width = 6.5, height = 3.2) {
  fs::dir_create(dirname(file))
  ggplot2::ggsave(
    file,
    plot,
    width = width,
    height = height,
    device = grDevices::cairo_pdf
  )
  file
}

# --------------------------------------------------------------------
# Figure: population gain from joint reconciliation against kappa.
# F0, F1 and F4 all have kappa = 0 and zero gain.
# --------------------------------------------------------------------
fig_exp1_gain <- function(exp1_pop, file) {
  df <- exp1_pop |>
    dplyr::filter(family %in% c("F2", "F3")) |>
    dplyr::mutate(
      family = factor(
        family_labels[family],
        levels = family_labels[c("F2", "F3")]
      ),
      hierarchy = factor(hierarchy_labels[hierarchy], levels = hierarchy_labels)
    )
  # kappa = 0 families (separable, invisible, OLS) all sit at the origin;
  # the caption says so
  origin <- df |>
    dplyr::distinct(hierarchy) |>
    dplyr::mutate(x = 0, y = 0)
  p <- ggplot2::ggplot(df, ggplot2::aes(kappa, 100 * gain, colour = family)) +
    ggplot2::geom_line(ggplot2::aes(linetype = family), linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(shape = family), size = 1.6) +
    ggplot2::geom_point(
      data = origin,
      ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.4,
      fill = "white",
      colour = "grey20"
    ) +
    ggplot2::facet_wrap(~hierarchy, scales = "free_x") +
    ggplot2::scale_colour_manual(values = fig_colours[3:4]) +
    ggplot2::scale_shape_manual(values = c(16, 17)) +
    ggplot2::scale_linetype_manual(values = c("solid", "22")) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.04, 0.1))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.02, 0.12))
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(
      x = expression("Population " * kappa),
      y = "MSE reduction from joint\nreconciliation (%)",
      colour = NULL,
      shape = NULL,
      linetype = NULL
    ) +
    theme_paper()
  save_figure(p, file)
}

# --------------------------------------------------------------------
# Figure: expected loss of estimated joint reconciliation relative to
# estimated separate reconciliation, by training sample size.
# Values below 1 favour joint reconciliation.
# --------------------------------------------------------------------
exp1_sample_summary <- function(exp1_samp) {
  exp1_samp |>
    dplyr::group_by(hierarchy, family, dial, kappa, T) |>
    dplyr::summarise(
      ratio = mean(mse_joint) / mean(mse_separate),
      oracle_ratio = mean(mse_oracle_joint) / mean(mse_oracle_sep),
      se = stats::sd(mse_joint / mse_separate) / sqrt(dplyr::n()),
      kappa_hat = mean(kappa_hat),
      kappa_hat_q10 = stats::quantile(kappa_hat, 0.1),
      kappa_hat_q90 = stats::quantile(kappa_hat, 0.9),
      .groups = "drop"
    )
}

fig_exp1_samplesize <- function(exp1_summary, file) {
  df <- exp1_summary |>
    dplyr::group_by(hierarchy, family) |>
    dplyr::mutate(strength = paste0("s", dplyr::dense_rank(dial))) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      separable = family %in% c("F0", "F1"),
      panel = dplyr::if_else(
        separable,
        "Separable (\u03ba = 0)",
        family_labels[family]
      ),
      panel = factor(
        panel,
        levels = c("Separable (\u03ba = 0)", family_labels[c("F2", "F3")])
      ),
      hierarchy = factor(
        hierarchy_labels[hierarchy],
        levels = hierarchy_labels
      ),
      key = dplyr::if_else(separable, family, strength),
      label = dplyr::if_else(
        separable,
        dplyr::if_else(family == "F0", "Separable", "+ invisible"),
        sprintf("\u03ba = %.2f", kappa)
      ),
      vjust = dplyr::case_when(
        family == "F0" ~ -0.6,
        family == "F1" ~ 1.6,
        TRUE ~ 0.5
      )
    )
  ends <- df |>
    dplyr::filter(T == max(T))
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(T, ratio, group = interaction(family, dial))
  ) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey40", linewidth = 0.4) +
    ggplot2::geom_line(
      ggplot2::aes(colour = key, linetype = separable),
      linewidth = 0.7
    ) +
    ggplot2::geom_point(ggplot2::aes(colour = key), size = 1.4) +
    ggplot2::geom_text(
      data = ends,
      ggplot2::aes(label = label, vjust = vjust),
      hjust = -0.12,
      size = 2.4,
      colour = "grey20"
    ) +
    ggplot2::facet_grid(hierarchy ~ panel, scales = "free_y") +
    ggplot2::scale_x_log10(
      breaks = c(50, 200, 1000),
      expand = ggplot2::expansion(mult = c(0.03, 0.5))
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        F0 = fig_colours[1],
        F1 = fig_colours[2],
        s1 = fig_ramp[2],
        s2 = fig_ramp[3],
        s3 = fig_ramp[4]
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = c(`FALSE` = "solid", `TRUE` = "22")
    ) +
    ggplot2::labs(
      x = "Training sample size T (log scale)",
      y = "Expected MSE, joint / separate"
    ) +
    theme_paper() +
    ggplot2::theme(legend.position = "none")
  save_figure(p, file, height = 4.6)
}

# --------------------------------------------------------------------
# Figure: the generic separability measure (nearest-Kronecker error)
# does not track what matters for reconciliation.
# --------------------------------------------------------------------
fig_kronecker_vs_gain <- function(exp1_pop, file) {
  df <- exp1_pop |>
    dplyr::filter(family %in% c("F1", "F2", "F3")) |>
    dplyr::mutate(
      family = factor(
        family_labels[family],
        levels = family_labels[c("F1", "F2", "F3")]
      ),
      hierarchy = factor(hierarchy_labels[hierarchy], levels = hierarchy_labels)
    )
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(kronecker_error, 100 * gain, colour = family)
  ) +
    ggplot2::geom_path(ggplot2::aes(linetype = family), linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(shape = family), size = 1.6) +
    ggplot2::facet_wrap(~hierarchy) +
    ggplot2::scale_colour_manual(values = fig_colours[2:4]) +
    ggplot2::scale_shape_manual(values = c(15, 16, 17)) +
    ggplot2::scale_linetype_manual(values = c("22", "solid", "42")) +
    ggplot2::labs(
      x = "Relative error of nearest Kronecker product",
      y = "MSE reduction from joint\nreconciliation (%)",
      colour = NULL,
      shape = NULL,
      linetype = NULL
    ) +
    theme_paper()
  save_figure(p, file)
}

# --------------------------------------------------------------------
# Figure: sampling distribution of kappa_hat under a separable truth
# (the noise floor), with a strongly non-separable case for contrast.
# --------------------------------------------------------------------
fig_kappa_noise <- function(exp1_samp, file) {
  df <- exp1_samp |>
    dplyr::filter(family %in% c("F0", "F3")) |>
    dplyr::group_by(hierarchy, family) |>
    dplyr::filter(dial == max(dial)) |>
    dplyr::group_by(hierarchy, family, dial, kappa, T) |>
    dplyr::summarise(
      med = stats::median(kappa_hat),
      lo = stats::quantile(kappa_hat, 0.05),
      hi = stats::quantile(kappa_hat, 0.95),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      case = dplyr::if_else(
        family == "F0",
        "Separable (κ = 0)",
        "Node-varying correlation (strongest)"
      ),
      case = factor(
        case,
        levels = c(
          "Separable (\u03ba = 0)",
          "Node-varying correlation (strongest)"
        )
      ),
      hierarchy = factor(hierarchy_labels[hierarchy], levels = hierarchy_labels)
    )
  truth <- df |> dplyr::distinct(hierarchy, case, kappa)
  noise_colours <- c(fig_colours[1], fig_colours[4])
  names(noise_colours) <- c(
    "Separable (\u03ba = 0)",
    "Node-varying correlation (strongest)"
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(T, med, colour = case, fill = case)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lo, ymax = hi),
      alpha = 0.15,
      colour = NA
    ) +
    ggplot2::geom_hline(
      data = truth,
      ggplot2::aes(yintercept = kappa, colour = case),
      linetype = "22",
      linewidth = 0.4
    ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(shape = case), size = 1.6) +
    ggplot2::facet_wrap(~hierarchy) +
    ggplot2::scale_x_log10(breaks = c(50, 100, 200, 500, 1000, 2000)) +
    ggplot2::scale_colour_manual(values = noise_colours) +
    ggplot2::scale_fill_manual(values = noise_colours) +
    ggplot2::labs(
      x = "Training sample size T (log scale)",
      y = expression("Estimated " * kappa * " (median, 5-95%)"),
      colour = NULL,
      fill = NULL,
      shape = NULL
    ) +
    theme_paper()
  save_figure(p, file)
}

# --------------------------------------------------------------------
# Figure: rejection rate of the likelihood ratio test of condition (d)
# --------------------------------------------------------------------
fig_kappa_power <- function(power, file) {
  df <- power |>
    dplyr::group_by(hierarchy, family) |>
    dplyr::mutate(strength = paste0("s", dplyr::dense_rank(dial))) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      separable = family %in% c("F0", "F1"),
      panel = dplyr::if_else(
        separable,
        "Separable (\u03ba = 0): size",
        family_labels[family]
      ),
      panel = factor(
        panel,
        levels = c("Separable (\u03ba = 0): size", family_labels[c("F2", "F3")])
      ),
      hierarchy = factor(
        hierarchy_labels[hierarchy],
        levels = hierarchy_labels
      ),
      key = dplyr::if_else(separable, family, strength),
      label = sprintf("\u03ba = %.2f", kappa)
    )
  # Label the non-separable lines at the smallest T, where they are distinct
  starts <- df |> dplyr::filter(!separable, T == min(T))
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(T, rejection, group = interaction(family, dial))
  ) +
    ggplot2::geom_hline(yintercept = 0.05, colour = "grey40", linewidth = 0.4) +
    ggplot2::geom_line(
      ggplot2::aes(colour = key, linetype = separable),
      linewidth = 0.7
    ) +
    ggplot2::geom_point(ggplot2::aes(colour = key), size = 1.4) +
    ggplot2::geom_text(
      data = starts,
      ggplot2::aes(label = label),
      hjust = 1.15,
      size = 2.4,
      colour = "grey20"
    ) +
    ggplot2::facet_grid(hierarchy ~ panel) +
    ggplot2::scale_x_log10(
      breaks = c(100, 200, 500, 1000),
      expand = ggplot2::expansion(mult = c(0.45, 0.05))
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_colour_manual(
      values = c(
        F0 = fig_colours[1],
        F1 = fig_colours[2],
        s1 = fig_ramp[2],
        s2 = fig_ramp[3],
        s3 = fig_ramp[4]
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = c(`FALSE` = "solid", `TRUE` = "22")
    ) +
    ggplot2::labs(
      x = "Training sample size T (log scale)",
      y = "Rejection rate at 5% level"
    ) +
    theme_paper() +
    ggplot2::theme(legend.position = "none")
  save_figure(p, file, height = 4.6)
}

# --------------------------------------------------------------------
# Figure: national admissions, dismissals and net change. The period
# after 2019 (Novo CAGED, COVID-19) is shaded; it is not used in the
# main analysis.
# --------------------------------------------------------------------
fig_app_data <- function(emprego, file) {
  nat <- emprego |>
    tibble::as_tibble() |>
    dplyr::filter(node == "Total") |>
    dplyr::mutate(date = as.Date(time)) |>
    dplyr::select(date, series, value) |>
    tidyr::pivot_wider(names_from = series, values_from = value) |>
    dplyr::mutate(Net = .data[["Admissões"]] - .data[["Demissões"]])
  flows <- nat |>
    tidyr::pivot_longer(
      c("Admissões", "Demissões"),
      names_to = "series",
      values_to = "value"
    ) |>
    dplyr::mutate(
      series = dplyr::recode(
        series,
        "Admissões" = "Admissions",
        "Demissões" = "Dismissals"
      ),
      panel = "Admissions and dismissals (millions)",
      value = value / 1e6
    )
  net <- nat |>
    dplyr::transmute(
      date,
      series = "Net change",
      value = Net / 1e6,
      panel = "Net change (millions)"
    )
  df <- dplyr::bind_rows(flows, net) |>
    dplyr::mutate(
      panel = factor(
        panel,
        levels = c(
          "Admissions and dismissals (millions)",
          "Net change (millions)"
        )
      )
    )
  shade <- tibble::tibble(
    xmin = as.Date("2020-01-01"),
    xmax = max(df$date) + 31
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(date, value, colour = series)) +
    ggplot2::geom_rect(
      data = shade,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill = "grey92"
    ) +
    ggplot2::geom_hline(
      data = tibble::tibble(
        panel = factor("Net change (millions)", levels = levels(df$panel)),
        y = 0
      ),
      ggplot2::aes(yintercept = y),
      colour = "grey50",
      linewidth = 0.3
    ) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y") +
    ggplot2::scale_colour_manual(
      values = c(
        Admissions = fig_colours[1],
        Dismissals = fig_colours[2],
        `Net change` = "grey25"
      )
    ) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
    theme_paper()
  save_figure(p, file, height = 4.2)
}

# --------------------------------------------------------------------
# Figure: map of Brazilian regions and federative units. Downloads state
# boundaries with geobr, so the target that calls this is built once and
# not rerun (see _targets.R).
# --------------------------------------------------------------------
fig_map <- function(state_meta, region_meta, file) {
  region_colors <- region_meta |>
    dplyr::filter(!is.na(map_color)) |>
    dplyr::arrange(order) |>
    dplyr::select(region_label, map_color) |>
    tibble::deframe()
  uf <- geobr::read_state(year = 2020, cache = FALSE, showProgress = FALSE) |>
    dplyr::left_join(
      state_meta |>
        dplyr::select(abbrev_state = UF, Região) |>
        dplyr::left_join(region_meta, by = "Região"),
      by = "abbrev_state"
    )
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  world_points <- cbind(
    world,
    sf::st_coordinates(sf::st_centroid(world$geometry))
  )
  world_points$name[world_points$name == "Brazil"] <- NA
  ocean <- function(x, y, label) {
    ggplot2::annotate(
      "text",
      x = x,
      y = y,
      label = label,
      fontface = "italic",
      colour = "#3a739c",
      size = 5.5
    )
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world, colour = "#9f9f9f", fill = "#e6e7e8") +
    ggplot2::geom_sf(
      data = uf,
      ggplot2::aes(fill = region_label),
      colour = "#e6e7e8"
    ) +
    ggplot2::scale_fill_manual(
      values = region_colors,
      name = "Region",
      breaks = names(region_colors)
    ) +
    ggplot2::geom_sf_text(
      data = uf,
      ggplot2::aes(label = abbrev_state),
      size = 3.3,
      colour = "#e6e7e8",
      fontface = "bold"
    ) +
    ggplot2::geom_text(
      data = world_points,
      ggplot2::aes(x = X, y = Y, label = name),
      colour = "#9f9f9f",
      fontface = "bold"
    ) +
    ocean(-33, -17.5, "Atlantic") +
    ocean(-33, -19, "Ocean") +
    ocean(-74, -21.5, "Pacific") +
    ocean(-74, -23, "Ocean") +
    ggplot2::labs(x = "", y = "") +
    ggplot2::coord_sf(xlim = c(-75, -30), ylim = c(-35, 5)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#d7f9f8"),
      legend.position = "inside",
      legend.position.inside = c(0.9, 0.13),
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.key.size = ggplot2::unit(0.6, "cm"),
      panel.grid.major = ggplot2::element_line(colour = "#dadad9")
    )
  grDevices::pdf(file, width = 9, height = 9)
  print(p)
  crop::dev.off.crop(file)
  file
}
