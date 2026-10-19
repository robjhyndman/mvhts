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
# Old CAGED state counts, 2004-2019, as exported from PDET: one row per
# state and movement type ("Admitidos" or "Desligados"; the state is
# given only on the first of its two rows), one column per month from
# "dez/19" back to "jan/04". Returns columns month ("YYYY-MM"), UF,
# admissions and dismissals.
# --------------------------------------------------------------------
read_caged_uf <- function(path) {
  lines <- readLines(path, encoding = "latin1")
  header <- strsplit(lines[2], ";")[[1]]
  labels <- header[-(1:2)]
  pt_months <- c("jan", "fev", "mar", "abr", "mai", "jun",
                 "jul", "ago", "set", "out", "nov", "dez")
  months <- sprintf(
    "%d-%02d",
    2000L + as.integer(sub(".*/", "", labels)),
    match(sub("/.*", "", labels), pt_months)
  )
  stopifnot(!anyNA(months))
  x <- utils::read.csv2(
    text = lines[-(1:2)],
    header = FALSE,
    colClasses = "character"
  )[, seq_along(header)]
  names(x) <- c("uf", "type", months)
  x <- x[x$type %in% c("Admitidos", "Desligados"), ]
  # Fill the state down to its "Desligados" row; "11 - Rondônia" -> 11
  x$ibge <- as.integer(substr(x$uf, 1, 2))
  x |>
    tidyr::fill(ibge) |>
    dplyr::select(-uf) |>
    tidyr::pivot_longer(dplyr::all_of(months), names_to = "month", values_to = "n") |>
    dplyr::mutate(
      n = as.numeric(n),
      type = ifelse(type == "Admitidos", "admissions", "dismissals")
    ) |>
    tidyr::pivot_wider(names_from = type, values_from = n) |>
    dplyr::inner_join(state_meta[c("UF", "ibge")], by = "ibge") |>
    dplyr::select(month, UF, admissions, dismissals)
}

# --------------------------------------------------------------------
# Novo CAGED counts by municipality from IpeaData ("sem ajuste"), one
# file per movement type, with one column per month ("2020.01").
# Summed to states; same columns as read_caged_uf(). The 33 rows with
# no values are historical municipalities that no longer exist.
# --------------------------------------------------------------------
read_novo_caged <- function(admissions_path, dismissals_path) {
  read_one <- function(path, type) {
    utils::read.csv2(path, skip = 1, check.names = FALSE, fileEncoding = "UTF-8-BOM") |>
      dplyr::select(UF = Sigla, dplyr::matches("^\\d{4}\\.\\d{2}$")) |>
      tidyr::pivot_longer(-UF, names_to = "month", values_to = "n") |>
      dplyr::mutate(month = sub(".", "-", month, fixed = TRUE)) |>
      dplyr::summarise(n = sum(n, na.rm = TRUE), .by = c(month, UF)) |>
      dplyr::rename("{type}" := n)
  }
  dplyr::inner_join(
    read_one(admissions_path, "admissions"),
    read_one(dismissals_path, "dismissals"),
    by = c("month", "UF")
  )
}

# State counts for every month, old CAGED then Novo CAGED
read_states <- function(caged_path, novo_paths) {
  dplyr::bind_rows(
    read_caged_uf(caged_path),
    read_novo_caged(novo_paths[1], novo_paths[2])
  ) |>
    dplyr::arrange(month, UF)
}

# --------------------------------------------------------------------
# Monthly admissions and dismissals for every series of hierarchy S
# from `first` on, as a tsibble with columns time, node, series and
# value. `paths` are the old CAGED file and the two Novo CAGED files.
# --------------------------------------------------------------------
read_data <- function(paths, S, first = "2005 Jan") {
  read_states(paths[1], paths[2:3]) |>
    dplyr::filter(tsibble::yearmonth(month) >= tsibble::yearmonth(first)) |>
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
