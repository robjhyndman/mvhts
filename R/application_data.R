# Brazilian geographical meta data

state_meta <- tibble::tribble(
  ~UF  , ~ibge , ~State                , ~Região        ,
  "AC" ,    12 , "Acre"                , "Norte"        ,
  "AL" ,    27 , "Alagoas"             , "Nordeste"     ,
  "AM" ,    13 , "Amazonas"            , "Norte"        ,
  "AP" ,    16 , "Amapá"               , "Norte"        ,
  "BA" ,    29 , "Bahia"               , "Nordeste"     ,
  "CE" ,    23 , "Ceará"               , "Nordeste"     ,
  "DF" ,    53 , "Distrito Federal"    , "Centro-Oeste" ,
  "ES" ,    32 , "Espírito Santo"      , "Sudeste"      ,
  "GO" ,    52 , "Goiás"               , "Centro-Oeste" ,
  "MA" ,    21 , "Maranhão"            , "Nordeste"     ,
  "MG" ,    31 , "Minas Gerais"        , "Sudeste"      ,
  "MS" ,    50 , "Mato Grosso do Sul"  , "Centro-Oeste" ,
  "MT" ,    51 , "Mato Grosso"         , "Centro-Oeste" ,
  "PA" ,    15 , "Pará"                , "Norte"        ,
  "PB" ,    25 , "Paraíba"             , "Nordeste"     ,
  "PE" ,    26 , "Pernambuco"          , "Nordeste"     ,
  "PI" ,    22 , "Piauí"               , "Nordeste"     ,
  "PR" ,    41 , "Paraná"              , "Sul"          ,
  "RJ" ,    33 , "Rio de Janeiro"      , "Sudeste"      ,
  "RN" ,    24 , "Rio Grande do Norte" , "Nordeste"     ,
  "RO" ,    11 , "Rondônia"            , "Norte"        ,
  "RR" ,    14 , "Roraima"             , "Norte"        ,
  "RS" ,    43 , "Rio Grande do Sul"   , "Sul"          ,
  "SC" ,    42 , "Santa Catarina"      , "Sul"          ,
  "SE" ,    28 , "Sergipe"             , "Nordeste"     ,
  "SP" ,    35 , "São Paulo"           , "Sudeste"      ,
  "TO" ,    17 , "Tocantins"           , "Norte"
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

read_data <- function(
  state_meta,
  region_meta,
  path = here::here("Dados/emprego_uf.csv")
) {
  # Built from raw PDET microdata by R/pdet_extract.R
  Dados <- read.csv(path) |>
    transmute(
      Data = yearmonth(month),
      UF,
      Admissões = admissions,
      Demissões = dismissals
    )

  # ============================================================
  # Add state and region information by joining with metadata tables
  # ============================================================
  Dados <- Dados |>
    left_join(state_meta, by = "UF") |>
    left_join(region_meta, by = "Região")
  if (any(is.na(Dados$Região))) {
    stop("Some UF values were not matched to state_meta.")
  }
  if (any(is.na(Dados$region_label))) {
    stop("Some Região values were not matched to region_meta.")
  }
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
    select(-UF) |>
    pivot_longer(
      -c(Data, Região, node),
      names_to = "series",
      values_to = "value"
    ) |>
    rename(time = Data)

  # ============================================================
  # Convert to tsibble format
  # ============================================================

  Dados |>
    as_tsibble(key = c(Região, node, series), index = time) |>
    arrange(Região)
}
