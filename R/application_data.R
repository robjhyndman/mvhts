read_data <- function() {
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
    summarise(
      Admissões = sum(Admissões),
      Demissões = sum(Demissões),
      .groups = "drop"
    )

  # ============================================================
  # Build hierarchical structure: States → Regions → Total
  # ============================================================

  Dados <- bind_rows(
    # Bottom level: states
    Dados |> mutate(node = UF),

    # Intermediate level: regions (sum of states)
    Dados |>
      group_by(Data, Região) |>
      summarise(
        Admissões = sum(Admissões),
        Demissões = sum(Demissões),
        .groups = "drop"
      ) |>
      mutate(node = paste0("agg_", Região)),

    # Top level: Brazil (sum of all regions)
    Dados |>
      group_by(Data) |>
      summarise(
        Admissões = sum(Admissões),
        Demissões = sum(Demissões),
        .groups = "drop"
      ) |>
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

  Dados |>
    arrange(Região) |>
    ungroup()
}
