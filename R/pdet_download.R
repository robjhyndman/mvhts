# ====================================================================
# Download raw CAGED microdata from the PDET FTP server
#
# Old CAGED (CAGEDEST, one file per month): 2007-2019. PDET does not
# publish microdata for earlier years.
# Novo CAGED (CAGEDMOV, on-time declarations): 2020 onwards.
#
# Files are stored under Dados/pdet/ mirroring the FTP layout (not
# tracked by git; about 5GB). Downloads resume if interrupted, and each
# archive is integrity-tested with 7z before being accepted. A manifest
# of file sizes and MD5 checksums is written to Dados/pdet_manifest.csv
# so the data vintage is recorded in the repository.
#
# The server is slow (roughly 100-200 KB/s), so a full download takes
# several hours. Re-running skips files that are already complete.
# ====================================================================

pdet_url <- "ftp://ftp.mtps.gov.br/pdet/microdados/"
pdet_dir <- here::here("Dados/pdet")

caged_years <- 2007:2019
novo_years <- 2020:2023

# --------------------------------------------------------------------
# Archives that are damaged on the PDET server (checked September 2026):
# repeated downloads have identical MD5 checksums and match the size
# listed on the server, but both 7z and unar fail partway through
# decompression. These months are taken from the legacy file instead;
# see R/pdet_extract.R.
# --------------------------------------------------------------------
pdet_damaged <- c(
  "CAGED/2008/CAGEDEST_052008.7z",
  "CAGED/2008/CAGEDEST_082008.7z",
  "CAGED/2009/CAGEDEST_062009.7z",
  "CAGED/2009/CAGEDEST_082009.7z",
  "CAGED/2009/CAGEDEST_102009.7z",
  "CAGED/2009/CAGEDEST_112009.7z",
  "CAGED/2010/CAGEDEST_052010.7z",
  "CAGED/2010/CAGEDEST_062010.7z",
  "CAGED/2010/CAGEDEST_072010.7z",
  "CAGED/2010/CAGEDEST_102010.7z",
  "CAGED/2010/CAGEDEST_122010.7z",
  "CAGED/2011/CAGEDEST_032011.7z",
  "CAGED/2012/CAGEDEST_052012.7z",
  "CAGED/2012/CAGEDEST_062012.7z",
  "CAGED/2012/CAGEDEST_082012.7z",
  "CAGED/2012/CAGEDEST_102012.7z",
  "CAGED/2013/CAGEDEST_012013.7z",
  "CAGED/2013/CAGEDEST_102013.7z",
  "CAGED/2014/CAGEDEST_032014.7z",
  "CAGED/2014/CAGEDEST_052014.7z",
  "CAGED/2014/CAGEDEST_092014.7z",
  "CAGED/2014/CAGEDEST_122014.7z"
)

# --------------------------------------------------------------------
# List of usable files, as paths relative to the microdata root
# --------------------------------------------------------------------
pdet_files <- function(caged_years, novo_years) {
  caged <- expand.grid(month = 1:12, year = caged_years)
  novo <- expand.grid(month = 1:12, year = novo_years)
  files <- c(
    sprintf("CAGED/%d/CAGEDEST_%02d%d.7z", caged$year, caged$month, caged$year),
    sprintf(
      "NOVO CAGED/%d/%d%02d/CAGEDMOV%d%02d.7z",
      novo$year,
      novo$year,
      novo$month,
      novo$year,
      novo$month
    )
  )
  setdiff(files, pdet_damaged)
}

# --------------------------------------------------------------------
# Download one file (resumable), test it, then move it into place.
# Returns TRUE on success.
# --------------------------------------------------------------------
download_pdet_file <- function(file) {
  dest <- file.path(pdet_dir, file)
  if (file.exists(dest)) {
    return(TRUE)
  }
  fs::dir_create(dirname(dest))
  part <- paste0(dest, ".part")
  url <- paste0(pdet_url, utils::URLencode(file))
  status <- system2(
    "curl",
    c(
      "--silent",
      "--show-error",
      "--fail",
      "--retry",
      "5",
      "--retry-delay",
      "30",
      "--continue-at",
      "-",
      "--output",
      shQuote(part),
      shQuote(url)
    )
  )
  if (status != 0) {
    message("Download failed: ", file)
    return(FALSE)
  }
  ok <- system2("7z", c("t", shQuote(part)), stdout = FALSE, stderr = FALSE)
  if (ok != 0) {
    message("Corrupt archive, deleted: ", file)
    file.remove(part)
    return(FALSE)
  }
  file.rename(part, dest)
}

# --------------------------------------------------------------------
# Record sizes and checksums of all downloaded files
# --------------------------------------------------------------------
write_manifest <- function(files) {
  paths <- file.path(pdet_dir, files)
  have <- file.exists(paths)
  manifest <- data.frame(
    file = files[have],
    bytes = file.size(paths[have]),
    md5 = unname(tools::md5sum(paths[have]))
  )
  utils::write.csv(
    manifest,
    here::here("Dados/pdet_manifest.csv"),
    row.names = FALSE
  )
  invisible(manifest)
}

# ====================================================================
# Run
# ====================================================================
if (sys.nframe() == 0L) {
  files <- pdet_files(caged_years, novo_years)
  # A few parallel connections; the server throttles each one
  ok <- parallel::mclapply(files, download_pdet_file, mc.cores = 4)
  ok <- unlist(ok)
  write_manifest(files)
  if (!all(ok)) {
    stop(
      sum(!ok),
      " files not downloaded; re-run to resume:\n",
      paste(files[!ok], collapse = "\n")
    )
  }
  message("All ", length(files), " files downloaded.")
}
