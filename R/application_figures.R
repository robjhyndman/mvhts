library(fpp3)
library(stringr)
library(scales)
library(sf)
library(geobr)
library(rnaturalearth)

source(here::here("R/read_data.R"))


# ============================================================
# Create descriptive labels for each Brazilian state (used in plots)
# ============================================================

state_labels <- c(
  "AC" = "Acre - AC (North)",
  "AL" = "Alagoas - AL (Northeast)",
  "AM" = "Amazonas - AM (North)",
  "AP" = "Amapá - AP (North)",
  "BA" = "Bahia - BA (Northeast)",
  "CE" = "Ceará - CE (Northeast)",
  "DF" = "Distrito Federal - DF (Midwest)",
  "ES" = "Espírito Santo - ES (Southeast)",
  "GO" = "Goiás - GO (Midwest)",
  "MA" = "Maranhão - MA (Northeast)",
  "MG" = "Minas Gerais - MG (Southeast)",
  "MS" = "Mato Grosso do Sul - MS (Midwest)",
  "MT" = "Mato Grosso - MT (Midwest)",
  "PA" = "Pará - PA (North)",
  "PB" = "Paraíba - PB (Northeast)",
  "PE" = "Pernambuco - PE (Northeast)",
  "PI" = "Piauí - PI (Northeast)",
  "PR" = "Paraná - PR (South)",
  "RJ" = "Rio de Janeiro - RJ (Southeast)",
  "RN" = "Rio Grande do Norte - RN (Northeast)",
  "RO" = "Rondônia - RO (North)",
  "RR" = "Roraima - RR (Norte)",
  "RS" = "Rio Grande do Sul - RS (South)",
  "SC" = "Santa Catarina - SC (South)",
  "SE" = "Sergipe - SE (Northeast)",
  "SP" = "São Paulo - SP (Southeast)",
  "TO" = "Tocantins - TO (North)"
)

# ===============================================================================
# Define the order in which states will appear in the facets (grouped by region)
# ===============================================================================

state_order <- c(
  # Centro-Oeste
  "DF",
  "GO",
  "MS",
  "MT",
  # Nordeste
  "AL",
  "BA",
  "CE",
  "MA",
  "PB",
  "PE",
  "PI",
  "RN",
  "SE",
  # Norte
  "AC",
  "AM",
  "AP",
  "PA",
  "RO",
  "RR",
  "TO",
  # Sudeste
  "ES",
  "MG",
  "RJ",
  "SP",
  # Sul
  "PR",
  "RS",
  "SC"
)

# ============================================================
# Define region order and labels for plots
# ============================================================

reg_order <- c("Total", "Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")

reg_labels <- c(
  "Total" = "Brazil",
  "Centro-Oeste" = "Midwest",
  "Nordeste" = "Northeast",
  "Norte" = "North",
  "Sudeste" = "Southeast",
  "Sul" = "South"
)

# ============================================================
# Plot 1 — Top hierarchical level (Brazil Total)
# ============================================================

Dados |>
  filter(str_starts(node, "Total")) |>
  pivot_wider(names_from = series, values_from = value) |>
  ggplot() +
  geom_line(
    aes(y = Admissões, x = as.Date(time), color = "Admissions"),
    linewidth = 0.6
  ) +
  geom_line(
    aes(y = Demissões, x = as.Date(time), color = "Dismissals"),
    linewidth = 0.6
  ) +
  labs(y = "Totals", x = "Month", color = "") +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%b %Y",
    limits = c(as.Date("2004-01-01"), as.Date("2023-12-31"))
  ) +
  scale_y_continuous(labels = label_number()) +
  facet_wrap(
    vars(Região),
    ncol = 1,
    scales = "free_y",
    labeller = labeller(Região = reg_labels)
  ) +
  scale_color_manual(
    values = c("Admissions" = "#064179", "Dismissals" = "#c3170b")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "bottom"
  ) +
  theme(
    strip.background = element_rect(
      fill = "white",
      linetype = "solid",
      color = "black"
    )
  )


# ============================================================
# Plot 2 — Intermediate hierarchical level (Regions)
# ============================================================

Dados_reg <- Dados |>
  mutate(Região = factor(Região, levels = reg_order))

Dados_reg |>
  filter(str_starts(node, "agg")) |>
  pivot_wider(names_from = series, values_from = value) |>
  ggplot() +
  geom_line(
    aes(y = Admissões, x = as.Date(time), color = "Admissions"),
    linewidth = 0.6
  ) +
  geom_line(
    aes(y = Demissões, x = as.Date(time), color = "Dismissals"),
    linewidth = 0.6
  ) +
  labs(y = "Totals", x = "Month", color = "") +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%b %Y",
    limits = c(as.Date("2004-01-01"), as.Date("2023-12-31"))
  ) +
  scale_y_continuous(labels = label_number()) +
  facet_wrap(
    vars(Região),
    ncol = 2,
    scales = "free_y",
    labeller = labeller(Região = reg_labels)
  ) +
  scale_color_manual(
    values = c("Admissions" = "#064179", "Dismissals" = "#c3170b")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "bottom"
  ) +
  theme(
    strip.background = element_rect(
      fill = "white",
      linetype = "solid",
      color = "black"
    )
  )


# ============================================================
# Plot 3 — Bottom hierarchical level (States)
# ============================================================

Dados_state <- Dados |>
  mutate(node = factor(node, levels = state_order))

Dados_state |>
  filter(!str_starts(node, "agg")) |>
  filter(!str_starts(node, "Total")) |>
  pivot_wider(names_from = series, values_from = value) |>
  ggplot() +
  geom_line(aes(y = Admissões, x = as.Date(time), color = "Admissions")) +
  geom_line(aes(y = Demissões, x = as.Date(time), color = "Dismissals")) +
  labs(y = "Totals", x = "Month", color = "") +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%b %Y",
    limits = c(as.Date("2004-01-01"), as.Date("2023-12-31"))
  ) +
  scale_y_continuous(labels = label_number()) +
  facet_wrap(
    vars(node),
    ncol = 3,
    scales = "free_y",
    labeller = labeller(node = state_labels)
  ) +
  scale_color_manual(
    values = c("Admissions" = "#064179", "Dismissals" = "#c3170b")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "bottom"
  ) +
  theme(
    strip.background = element_rect(
      fill = "white",
      linetype = "solid",
      color = "black"
    )
  )


# ============================================================
# Plot 4 — Map: location of brazilian regions and states
# ============================================================

# ============================================================
# Load Brazilian states spatial data (sf object)
# ============================================================

uf = read_state(year = 2020)

# ============================================================
# Load world map
# ============================================================

world <- ne_countries(scale = "medium", returnclass = "sf")

# Extract centroid coordinates of each country (for labeling)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

# Remove label for Brazil to avoid overlapping with the main map
world_points$name[world_points$name == "Brazil"] = NA


# ============================================================
# Map: Brazil by regions
# ============================================================

ggplot() +

  # Background world map
  geom_sf(data = world, colour = '#9f9f9f', fill = "#e6e7e8") +

  # Brazilian states filled by region
  geom_sf(data = uf, aes(fill = as.character(code_region)), color = "#e6e7e8") +

  # Manual color palette for the five Brazilian regions
  scale_fill_manual(
    values = c("#98a54b", "#f13f31", "#0b7374", "#54bebe", "#f2972c"),
    name = "Region",
    labels = c("North", "Northeast", "Southeast", "South", "Midwest")
  ) +

  # Add state abbreviations on the map
  geom_sf_text(
    data = uf,
    aes(label = abbrev_state),
    size = 3.3,
    color = "#e6e7e8",
    fontface = "bold"
  ) +

  # Add country names around Brazil
  geom_text(
    data = world_points,
    aes(x = X, y = Y, label = name),
    color = '#9f9f9f',
    fontface = "bold",
    check_overlap = FALSE
  ) +

  # Annotate oceans
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

  # Remove axis labels
  labs(x = "", y = "") +

  # Adjust map window to focus on South America and Brazil
  coord_sf(xlim = c(-30, -75), ylim = c(-35, 5)) +

  # Map styling
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#d7f9f8"), # light ocean color
    legend.position = c(.9, .13), # manual legend position
    legend.background = element_rect(fill = 'transparent'),
    legend.key.size = unit(0.6, 'cm'),
    panel.grid.major = element_line(color = "#dadad9")
  )
