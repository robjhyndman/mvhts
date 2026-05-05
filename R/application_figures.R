library(fpp3)
library(stringr)
library(scales)
library(sf)
library(geobr)
library(rnaturalearth)

source(here::here("R/application_data.R"))
fs::dir_create(here::here("Imagens"))

# ============================================================
# Shared settings and helper functions
# ============================================================

save_cropped_pdf <- function(plot, filename, width, height) {
  pdf(here::here("Imagens", filename), width = width, height = height)
  print(plot)
  crop::dev.off.crop(here::here("Imagens", filename))
}

employment_plot <- function(
  data,
  facet_var,
  facet_labels,
  ncol = 1,
  linewidth = 0.6,
  series_colors = c("Admissions" = "#064179", "Dismissals" = "#c3170b"),
  plot_start = as.Date("2004-01-01"),
  plot_end = as.Date("2023-12-31")
) {
  data |>
    mutate(
      series = recode(
        series,
        "Admissões" = "Admissions",
        "Demissões" = "Dismissals"
      )
    ) |>
    ggplot(aes(x = as.Date(time), y = value, color = series)) +
    geom_line(linewidth = linewidth) +
    labs(y = "Totals", x = "Month", color = "") +
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%b %Y",
      limits = c(plot_start, plot_end)
    ) +
    scale_y_continuous(labels = label_number()) +
    facet_wrap(
      vars({{ facet_var }}),
      ncol = ncol,
      scales = "free_y",
      labeller = as_labeller(facet_labels)
    ) +
    scale_color_manual(values = series_colors) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "bottom",
      strip.background = element_rect(
        fill = "white",
        linetype = "solid",
        color = "black"
      )
    )
}

# ============================================================
# Read employment data (Admissions and Dismissals)
# ============================================================

Dados <- read_data(state_meta, region_meta)

# ============================================================
# Labels, ordering, and palettes from metadata
# ============================================================

state_labels <- state_meta |>
  left_join(region_meta, by = "Região") |>
  transmute(UF, label = paste0(State, " - ", UF, " (", region_label, ")")) |>
  deframe()

state_order <- state_meta |>
  left_join(region_meta, by = "Região") |>
  arrange(order, UF) |>
  pull(UF)

reg_labels <- region_meta |>
  select(Região, region_label) |>
  deframe()

reg_order <- region_meta |>
  arrange(order) |>
  pull(Região)

region_colors <- region_meta |>
  filter(!is.na(map_color)) |>
  arrange(order) |>
  select(region_label, map_color) |>
  deframe()

# ============================================================
# Plot 1 — Top hierarchical level (Brazil Total)
# ============================================================

p <- Dados |>
  filter(node == "Total") |>
  employment_plot(Região, reg_labels, ncol = 1)

save_cropped_pdf(p, "fig_emprego_br.pdf", width = 9, height = 4.5)

# ============================================================
# Plot 2 — Intermediate hierarchical level (Regions)
# ============================================================

p <- Dados |>
  filter(str_starts(node, "agg_")) |>
  mutate(Região = factor(Região, levels = reg_order)) |>
  employment_plot(Região, reg_labels, ncol = 2)

save_cropped_pdf(p, "fig_emprego_reg.pdf", width = 9, height = 6)

# ============================================================
# Plot 3 — Bottom hierarchical level (States)
# ============================================================

p <- Dados |>
  filter(!str_starts(node, "agg_"), node != "Total") |>
  mutate(node = factor(node, levels = state_order)) |>
  employment_plot(node, state_labels, ncol = 3)

save_cropped_pdf(p, "fig_emprego_uf.pdf", width = 9, height = 12)

# ============================================================
# Plot 4 — Map: location of Brazilian regions and states
# ============================================================

# geobr code is fragile and a little slow, so only run this if the file doesn't already exist. If you need to re-run it, just delete the mapa_reg.pdf file and run this section again.
if (!fs::file_exists(here::here("Imagens", "mapa_reg.pdf"))) {
  brazilian_states <- geobr::read_state(year = 2020, cache = FALSE)
  uf <- brazilian_states |>
    left_join(
      state_meta |>
        select(abbrev_state = UF, Região) |>
        left_join(region_meta, by = "Região"),
      by = "abbrev_state"
    )

  world <- ne_countries(scale = "medium", returnclass = "sf")
  world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
  world_points$name[world_points$name == "Brazil"] <- NA

  p <- ggplot() +
    geom_sf(data = world, colour = "#9f9f9f", fill = "#e6e7e8") +
    geom_sf(data = uf, aes(fill = region_label), color = "#e6e7e8") +
    scale_fill_manual(
      values = region_colors,
      name = "Region",
      breaks = names(region_colors)
    ) +
    geom_sf_text(
      data = uf,
      aes(label = abbrev_state),
      size = 3.3,
      color = "#e6e7e8",
      fontface = "bold"
    ) +
    geom_text(
      data = world_points,
      aes(x = X, y = Y, label = name),
      color = "#9f9f9f",
      fontface = "bold",
      check_overlap = FALSE
    ) +
    annotate(
      geom = "text",
      x = -33,
      y = -17.5,
      label = "Atlantic",
      fontface = "italic",
      color = "#3a739c",
      size = 5.5
    ) +
    annotate(
      geom = "text",
      x = -33,
      y = -19,
      label = "Ocean",
      fontface = "italic",
      color = "#3a739c",
      size = 5.5
    ) +
    annotate(
      geom = "text",
      x = -74,
      y = -21.5,
      label = "Pacific",
      fontface = "italic",
      color = "#3a739c",
      size = 5.5
    ) +
    annotate(
      geom = "text",
      x = -74,
      y = -23,
      label = "Ocean",
      fontface = "italic",
      color = "#3a739c",
      size = 5.5
    ) +
    labs(x = "", y = "") +
    coord_sf(xlim = c(-75, -30), ylim = c(-35, 5)) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "#d7f9f8"),
      legend.position = c(.9, .13),
      legend.background = element_rect(fill = "transparent"),
      legend.key.size = unit(0.6, "cm"),
      panel.grid.major = element_line(color = "#dadad9")
    )

  save_cropped_pdf(p, "mapa_reg.pdf", width = 9, height = 9)
}
