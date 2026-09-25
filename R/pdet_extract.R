# ====================================================================
# Build monthly admissions and dismissals by state from raw PDET files
#
# Input: the 7z archives downloaded by R/pdet_download.R (Dados/pdet/).
# Output: Dados/emprego_uf.csv, with one row per month and state.
#
# Each archive is streamed through 7z and awk, counting movements by
# month, state and movement type, so the 400MB text files are never
# written to disk. Nothing else is done to the data.
#
# Definitions (both match the "sem ajuste" official series):
#   Old CAGED (2007-2019): all records in CAGEDEST_MMYYYY, by
#     "Competência Declarada"; "Admitidos/Desligados" is 1 for an
#     admission and 2 for a dismissal.
#   Novo CAGED (2020 onwards): all records in CAGEDMOVYYYYMM (on-time
#     declarations), by "competênciamov"; "saldomovimentação" is 1 for
#     an admission and -1 for a dismissal. Late declarations (CAGEDFOR)
#     and exclusions (CAGEDEXC) are not included.
#
# 22 old-CAGED archives are damaged on the server (pdet_damaged in
# R/pdet_download.R). Those months come from the legacy file
# Dados/legacy/Dados_emprego_rgi.csv, which holds the same PDET counts
# but with 2004-2019 in reversed time order in two blocks (2004-2010 and
# 2011-2019). read_legacy() undoes the reversal. R/test_data.R checks
# that the remapped legacy file agrees with the raw extraction for every
# intact month. The source column records where each month came from.
# ====================================================================

source(here::here("R/application_data.R"))
source(here::here("R/pdet_download.R"))

# --------------------------------------------------------------------
# Count records in one archive by month, state and movement type.
# Columns are located by matching (lower case) header names, because
# column positions differ between years.
# --------------------------------------------------------------------
count_movements <- function(path, type_col, month_col, uf_col) {
  awk_prog <- paste(
    "NR == 1 {",
    "  sub(/\\r$/, \"\")",
    "  for (i = 1; i <= NF; i++) {",
    "    h = tolower($i)",
    "    if (h ~ tcol) t = i",
    "    if (h ~ mcol) m = i",
    "    if (h ~ ucol) u = i",
    "  }",
    "  if (!t || !m || !u) { print \"columns not found\" > \"/dev/stderr\"; exit 2 }",
    "  next",
    "}",
    "{ sub(/\\r$/, \"\"); n[$m \";\" $u \";\" $t]++ }",
    "END { for (k in n) print k \";\" n[k] }",
    sep = "\n"
  )
  cmd <- paste(
    "set -o pipefail;",
    "7z e -so",
    shQuote(path),
    "2>/dev/null |",
    "LC_ALL=C awk -F';'",
    "-v",
    shQuote(paste0("tcol=", type_col)),
    "-v",
    shQuote(paste0("mcol=", month_col)),
    "-v",
    shQuote(paste0("ucol=", uf_col)),
    shQuote(awk_prog)
  )
  out <- suppressWarnings(system2("bash", c("-c", shQuote(cmd)), stdout = TRUE))
  if (!is.null(attr(out, "status"))) {
    stop("Failed to read ", path)
  }
  counts <- utils::read.table(
    text = out,
    sep = ";",
    col.names = c("month", "uf", "type", "n"),
    colClasses = c("character", "character", "integer", "numeric")
  )
  # Unclassified states are coded "{ñ class}" in some old CAGED files
  counts$uf <- suppressWarnings(as.integer(counts$uf))
  counts$file <- basename(path)
  counts
}

count_caged <- function(path) {
  counts <- count_movements(path, "^admitidos", "^compet", "^uf$")
  # Old CAGED files each hold a single declared month
  expected <- sub("^CAGEDEST_(\\d{2})(\\d{4})\\.7z$", "\\2\\1", basename(path))
  if (!all(counts$month == expected)) {
    stop("Unexpected months in ", path)
  }
  counts$type <- c("admissions", "dismissals")[counts$type]
  counts$source <- "CAGED"
  counts
}

count_novo <- function(path) {
  counts <- count_movements(path, "^saldomovimenta", "^compet.*mov$", "^uf$")
  counts$type <- ifelse(counts$type == 1, "admissions", "dismissals")
  counts$source <- "Novo CAGED"
  counts
}

# --------------------------------------------------------------------
# Combine all files into one table of monthly counts by state
# --------------------------------------------------------------------
build_emprego_uf <- function(files) {
  paths <- file.path(pdet_dir, files)
  missing <- !file.exists(paths)
  if (any(missing)) {
    stop(
      sum(missing),
      " raw files missing; run R/pdet_download.R first.\n",
      paste(head(files[missing]), collapse = "\n")
    )
  }
  counts <- parallel::mclapply(
    paths,
    \(p) if (grepl("CAGEDEST", p)) count_caged(p) else count_novo(p),
    mc.cores = max(1, parallel::detectCores() - 2)
  ) |>
    dplyr::bind_rows()

  if (any(is.na(counts$type))) {
    stop("Unknown movement type codes found.")
  }
  unmatched <- counts |> dplyr::filter(!uf %in% state_meta$ibge)
  if (nrow(unmatched) > 0) {
    message(
      "Dropping ",
      sum(unmatched$n),
      " records with unknown state codes: ",
      paste(unique(unmatched$uf), collapse = ", ")
    )
  }

  counts |>
    dplyr::inner_join(
      state_meta |> dplyr::select(ibge, UF),
      by = c(uf = "ibge")
    ) |>
    dplyr::mutate(
      month = paste0(substr(month, 1, 4), "-", substr(month, 5, 6))
    ) |>
    dplyr::group_by(month, UF, source, type) |>
    dplyr::summarise(n = sum(n), .groups = "drop") |>
    tidyr::pivot_wider(names_from = type, values_from = n, values_fill = 0) |>
    dplyr::select(month, UF, admissions, dismissals, source) |>
    dplyr::arrange(month, UF)
}

# --------------------------------------------------------------------
# Legacy file with the time reversal undone, aggregated to states.
# File month (y, m) is true month (4014 - y, 13 - m) for 2004-2010 and
# (4030 - y, 13 - m) for 2011-2019. Later months are correctly ordered.
# Missing counts in the legacy file are regions with no movements.
# --------------------------------------------------------------------
read_legacy <- function() {
  legacy <- utils::read.csv2(
    here::here("Dados/legacy/Dados_emprego_rgi.csv"),
    fileEncoding = "latin1",
    header = FALSE,
    skip = 1,
    col.names = c("date", "rgi", "UF", "admissions", "dismissals")
  )
  y <- as.integer(substr(legacy$date, 1, 4))
  m <- as.integer(substr(legacy$date, 6, 7))
  reversed <- y <= 2019
  legacy$month <- sprintf(
    "%d-%02d",
    ifelse(y <= 2010, 4014L - y, ifelse(reversed, 4030L - y, y)),
    ifelse(reversed, 13L - m, m)
  )
  legacy |>
    dplyr::group_by(month, UF) |>
    dplyr::summarise(
      admissions = sum(admissions, na.rm = TRUE),
      dismissals = sum(dismissals, na.rm = TRUE),
      .groups = "drop"
    )
}

damaged_months <- function() {
  sub("^.*CAGEDEST_(\\d{2})(\\d{4})\\.7z$", "\\2-\\1", pdet_damaged)
}

# ====================================================================
# Run
# ====================================================================
if (sys.nframe() == 0L) {
  emprego <- dplyr::bind_rows(
    build_emprego_uf(pdet_files(caged_years, novo_years)),
    read_legacy() |>
      dplyr::filter(month %in% damaged_months()) |>
      dplyr::mutate(source = "CAGED (legacy file)")
  ) |>
    dplyr::arrange(month, UF)
  utils::write.csv(
    emprego,
    here::here("Dados/emprego_uf.csv"),
    row.names = FALSE
  )
}
