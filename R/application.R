library(fpp3)
library(tsibble)
source(here::here("R/function_rec_application.R"))

# ============================================================
# Read employment data (Admissions and Dismissals)
# ============================================================

Dados <- read.csv2(
  here::here("Dados/Dados_emprego_rgi.csv"),
  fileEncoding = "latin1"
)

# Convert date column to year-month format
Dados$Data <- yearmonth(Dados$Data)

Dados$cod_rgi <- as.character(Dados$cod_rgi)

# ============================================================
# Create table mapping states (UF) to regions
# ============================================================

regioes <- data.frame(
  UF = c(
    "AC",
    "AL",
    "AM",
    "AP",
    "BA",
    "CE",
    "DF",
    "ES",
    "GO",
    "MA",
    "MG",
    "MS",
    "MT",
    "PA",
    "PB",
    "PE",
    "PI",
    "PR",
    "RJ",
    "RN",
    "RO",
    "RR",
    "RS",
    "SC",
    "SE",
    "SP",
    "TO"
  ),
  Região = c(
    "Norte",
    "Nordeste",
    "Norte",
    "Norte",
    "Nordeste",
    "Nordeste",
    "Centro-Oeste",
    "Sudeste",
    "Centro-Oeste",
    "Nordeste",
    "Sudeste",
    "Centro-Oeste",
    "Centro-Oeste",
    "Norte",
    "Nordeste",
    "Nordeste",
    "Nordeste",
    "Sul",
    "Sudeste",
    "Nordeste",
    "Norte",
    "Norte",
    "Sul",
    "Sul",
    "Nordeste",
    "Sudeste",
    "Norte"
  )
)

# Merge region information into the dataset
Dados <- merge(Dados, regioes, by = "UF")

# ============================================================
# Aggregate data by state within each region and month
# ============================================================

Dados <- Dados |>
  group_by(Data, Região, UF) |>
  summarise(Admissões = sum(Admissões), Demissões = sum(Demissões))

# ============================================================
# Build hierarchical structure: States → Regions → Total
# ============================================================

Dados <- bind_rows(
  # Bottom level: states
  Dados |> mutate(node = UF),

  # Intermediate level: regions (sum of states)
  Dados |>
    group_by(Data, Região) |>
    summarise(Admissões = sum(Admissões), Demissões = sum(Demissões)) |>
    mutate(node = paste0("agg_", Região)),

  # Top level: Brazil (sum of all regions)
  Dados |>
    group_by(Data) |>
    summarise(Admissões = sum(Admissões), Demissões = sum(Demissões)) |>
    mutate(node = "Total", Região = "Total")
)

# ============================================================
# Convert to long format (series = Admissions / Dismissals)
# ============================================================

Dados <- Dados |>
  select(-c(UF)) |>
  pivot_longer(
    -c(Data, Região, node),
    names_to = "series",
    values_to = "Valor"
  ) |>
  ungroup()

# ============================================================
# Convert to tsibble format
# ============================================================

Dados <- Dados |>
  as_tsibble(key = c(Região, node, series), index = Data)
colnames(Dados) <- c("time", "Região", "node", "series", "value")
Dados <- Dados |>
  arrange(Região) |>
  ungroup()

# save(Dados, file = "Dados_regiao.RData")

# ============================================================
# Hierarchy information
# ============================================================

# Number of nodes: 27 states + 5 regions + Brazil
n <- length(unique(Dados$node))

# Number of series: Admissions and Dismissals
m <- length(unique(Dados$series))

# Number of states within each region
count_agg <- c(4, 9, 7, 4, 3)

# Number of intermediate nodes (regions)
n_agg <- 5

# ============================================================
# Build summing matrix S (hierarchical structure)
# ============================================================

# Aggregation matrix for regions → states
matrix_agg <- matrix(0, n_agg, n - n_agg - 1)
start = 1
for (i in 1:n_agg) {
  matrix_agg[i, start:sum(count_agg[1:i])] <- 1
  start <- start + count_agg[i]
}

# Full summing matrix
S <- rbind(
  rep(1, n - n_agg - 1), # Total (Brazil)
  matrix_agg, # Regions
  diag(1, n - n_agg - 1) # States (bottom level)
)

# ============================================================
# Multivariate reconciliation using covariance and shrinkage
# ============================================================

########################### ARIMA MODEL ###################################

# Fit ARIMA with rolling origin cross-validation
mod <- Dados |>
  filter_index(~"2022 Q12") |>
  stretch_tsibble(.init = 217, .step = 1) |>
  model(arima = ARIMA(value))

# Forecast 12 steps ahead
fc <- mod |> forecast(h = 12)

# Create forecast horizon index
fc <- fc |>
  group_by(.id, node, series) |>
  mutate(h = row_number()) |>
  ungroup()

save(mod, fc, file = "mod_fc_arima_regiao.RData")

# ---------------- Reconciliation using covariance estimator ----------------

fc_rec_arima_cov = mv_reconcile(
  mod |> filter(.id == 1),
  fc |> filter(.id == 1),
  S
)

for (i in 2:12) {
  a = mv_reconcile(mod |> filter(.id == i), fc |> filter(.id == i), S)
  fc_rec_arima_cov <- add_row(fc_rec_arima_cov, a)
}

save(fc_rec_arima_cov, file = "fc_rec_arima_regiao.RData")

# ---------------- Reconciliation using shrinkage estimator ----------------

fc_rec_arima_sh = mv_reconcile_s(
  mod |> filter(.id == 1),
  fc |> filter(.id == 1),
  S
)

for (i in 2:12) {
  a = mv_reconcile_s(mod |> filter(.id == i), fc |> filter(.id == i), S)
  fc_rec_arima_sh <- add_row(fc_rec_arima_sh, a)
}

save(fc_rec_arima_cov, fc_rec_arima_sh, file = "fc_rec_arima_regiao2.RData")

########################### VAR MODEL ########################################

# Fit VAR model (bivariate: Admissions and Dismissals)
mod_var <- Dados |>
  pivot_wider(names_from = series, values_from = value) |>
  filter_index(~"2022 Q12") |>
  stretch_tsibble(.init = 217, .step = 1) |>
  model(var = VAR(vars(Admissões, Demissões)))

# Forecast 12 steps ahead
fc_var <- mod_var |> forecast(h = 12)

# Reshape VAR output back to long format
fc_var <- fc_var |>
  mutate(.mean = as.data.frame(.mean)) |>
  unnest_wider(.mean, names_sep = "_") |>
  rename(Admissões = .mean_V1, Demissões = .mean_V2, value = .distribution) |>
  pivot_longer(
    -c(.id, Região, node, .model, time, value),
    names_to = "series",
    values_to = ".mean",
    cols_vary = "slowest"
  ) |>
  arrange(Região) |>
  as_tsibble(index = time, key = c(.id, Região, node, .model, series))

# Create forecast horizon index
fc_var <- fc_var |>
  group_by(.id, node, series) |>
  mutate(h = row_number()) |>
  ungroup()

save(mod_var, fc_var, file = "mod_fc_var_regiao.RData")

# ---------------- Reconciliation using covariance estimator ----------------

fc_rec_var_cov = mv_reconcile(
  mod_var |> filter(.id == 1),
  fc_var |> filter(.id == 1),
  S
)

for (i in 2:12) {
  a = mv_reconcile(mod_var |> filter(.id == i), fc_var |> filter(.id == i), S)
  fc_rec_var_cov <- add_row(fc_rec_var_cov, a)
}

save(fc_rec_var_cov, file = "fc_rec_var_regiao.RData")

# ---------------- Reconciliation using shrinkage estimator ----------------

fc_rec_var_s = mv_reconcile_s(
  mod_var |> filter(.id == 1),
  fc_var |> filter(.id == 1),
  S
)

for (i in 2:12) {
  a = mv_reconcile_s(mod_var |> filter(.id == i), fc_var |> filter(.id == i), S)
  fc_rec_var_s <- add_row(fc_rec_var_s, a)
}

save(fc_rec_var_cov, fc_rec_var_s, file = "fc_rec_var_regiao.RData")
