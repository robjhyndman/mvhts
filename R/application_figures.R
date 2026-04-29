library(fpp3)
library(stringr)
library(scales)
library(sf)
library(geobr)
library(rnaturalearth)

source(here::here("R/application_data.R"))

# ============================================================
# Read employment data (Admissions and Dismissals)
# ============================================================
Dados <- read_data()

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

pdf(here::here("Imagens/fig_emprego_br.pdf"), width = 9, height = 4.5)
p <- Dados |>
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
print(p)
crop::dev.off.crop(here::here("Imagens/fig_emprego_br.pdf"))

# ============================================================
# Plot 2 — Intermediate hierarchical level (Regions)
# ============================================================

Dados_reg <- Dados |>
  mutate(Região = factor(Região, levels = reg_order))

pdf(here::here("Imagens/fig_emprego_reg.pdf"), width = 9, height = 6)
p <- Dados_reg |>
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
print(p)
crop::dev.off.crop(here::here("Imagens/fig_emprego_reg.pdf"))

# ============================================================
# Plot 3 — Bottom hierarchical level (States)
# ============================================================

Dados_state <- Dados |>
  mutate(node = factor(node, levels = state_order))

pdf(here::here("Imagens/fig_emprego_uf.pdf"), width = 9, height = 12)
p <- Dados_state |>
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
print(p)
crop::dev.off.crop(here::here("Imagens/fig_emprego_uf.pdf"))

# ============================================================
# Plot 4 — Map: location of brazilian regions and states
# ============================================================

uf <- read_state(year = 2020)
world <- ne_countries(scale = "medium", returnclass = "sf")
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
world_points$name[world_points$name == "Brazil"] <- NA

pdf(here::here("Imagens/mapa_reg.pdf"), width = 9, height = 9)
ggplot() +
  geom_sf(data = world, colour = "#9f9f9f", fill = "#e6e7e8") +
  geom_sf(data = uf, aes(fill = as.character(code_region)), color = "#e6e7e8") +
  scale_fill_manual(
    values = c("#98a54b", "#f13f31", "#0b7374", "#54bebe", "#f2972c"),
    name = "Region",
    labels = c("North", "Northeast", "Southeast", "South", "Midwest")
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
  coord_sf(xlim = c(-30, -75), ylim = c(-35, 5)) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#d7f9f8"),
    legend.position = c(.9, .13),
    legend.background = element_rect(fill = "transparent"),
    legend.key.size = unit(0.6, "cm"),
    panel.grid.major = element_line(color = "#dadad9")
  )
print(p)
crop::dev.off.crop(here::here("Imagens/mapa_reg.pdf"))
