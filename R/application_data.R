# ====================================================================
# Application data: Brazilian admissions and dismissals by state
# ====================================================================

# Federative units (the bottom level) and their regions

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

# Regions, with English labels, table order and map colours
region_meta <- tibble::tribble(
  ~Região        , ~region_label , ~order , ~map_color ,
  "Total"        , "Brazil"      ,      1 , NA         ,
  "Centro-Oeste" , "Midwest"     ,      2 , "#f2972c"  ,
  "Nordeste"     , "Northeast"   ,      3 , "#f13f31"  ,
  "Norte"        , "North"       ,      4 , "#98a54b"  ,
  "Sudeste"      , "Southeast"   ,      5 , "#0b7374"  ,
  "Sul"          , "South"       ,      6 , "#54bebe"
)

# The two variables: names in the data, and English labels for output
app_series_labels <- c("Admissões" = "Admissions", "Demissões" = "Dismissals")
app_series <- names(app_series_labels)

# --------------------------------------------------------------------
# Monthly admissions and dismissals for every series of hierarchy S, as
# a tsibble with columns time, node, series and value. The state counts
# in `path` are built from raw PDET microdata by R/pdet_extract.R.
# --------------------------------------------------------------------
read_data <- function(path, S) {
  utils::read.csv(path) |>
    dplyr::transmute(
      time = tsibble::yearmonth(month),
      node = UF,
      Admissões = admissions,
      Demissões = dismissals
    ) |>
    tidyr::pivot_longer(
      dplyr::all_of(app_series),
      names_to = "series",
      values_to = "value"
    ) |>
    aggregate_hierarchy(S) |>
    tsibble::as_tsibble(index = time, key = c(node, series))
}
