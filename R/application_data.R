# Brazilian geographical meta data

state_meta <- tibble::tribble(
  ~UF  , ~State                , ~Região        ,
  "AC" , "Acre"                , "Norte"        ,
  "AL" , "Alagoas"             , "Nordeste"     ,
  "AM" , "Amazonas"            , "Norte"        ,
  "AP" , "Amapá"               , "Norte"        ,
  "BA" , "Bahia"               , "Nordeste"     ,
  "CE" , "Ceará"               , "Nordeste"     ,
  "DF" , "Distrito Federal"    , "Centro-Oeste" ,
  "ES" , "Espírito Santo"      , "Sudeste"      ,
  "GO" , "Goiás"               , "Centro-Oeste" ,
  "MA" , "Maranhão"            , "Nordeste"     ,
  "MG" , "Minas Gerais"        , "Sudeste"      ,
  "MS" , "Mato Grosso do Sul"  , "Centro-Oeste" ,
  "MT" , "Mato Grosso"         , "Centro-Oeste" ,
  "PA" , "Pará"                , "Norte"        ,
  "PB" , "Paraíba"             , "Nordeste"     ,
  "PE" , "Pernambuco"          , "Nordeste"     ,
  "PI" , "Piauí"               , "Nordeste"     ,
  "PR" , "Paraná"              , "Sul"          ,
  "RJ" , "Rio de Janeiro"      , "Sudeste"      ,
  "RN" , "Rio Grande do Norte" , "Nordeste"     ,
  "RO" , "Rondônia"            , "Norte"        ,
  "RR" , "Roraima"             , "Norte"        ,
  "RS" , "Rio Grande do Sul"   , "Sul"          ,
  "SC" , "Santa Catarina"      , "Sul"          ,
  "SE" , "Sergipe"             , "Nordeste"     ,
  "SP" , "São Paulo"           , "Sudeste"      ,
  "TO" , "Tocantins"           , "Norte"
)

region_meta <- tibble::tribble(
  ~Região        , ~region_label , ~order , ~map_color ,
  "Total"        , "Brazil"      ,      1 , NA         ,
  "Centro-Oeste" , "Midwest"     ,      2 , "#f2972c"  ,
  "Nordeste"     , "Northeast"   ,      3 , "#f13f31"  ,
  "Norte"        , "North"       ,      4 , "#98a54b"  ,
  "Sudeste"      , "Southeast"   ,      5 , "#0b7374"  ,
  "Sul"          , "South"       ,      6 , "#54bebe"
)

read_data <- function(state_meta, region_meta) {
  Dados <- read.csv2(
    here::here("Dados/Dados_emprego_rgi.csv"),
    fileEncoding = "latin1"
  )

  # Convert date column to year-month format
  Dados$Data <- yearmonth(Dados$Data)

  Dados$cod_rgi <- as.character(Dados$cod_rgi)

  # ============================================================
  # Add state and region information by joining with metadata tables
  # ============================================================
  Dados <- Dados |>
    left_join(state_meta, by = "UF") |>
    left_join(region_meta, by = "Região")

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
