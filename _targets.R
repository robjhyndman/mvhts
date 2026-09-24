# Pipeline for the paper. Run with: uvr run run.R
# Functions are defined in R/; this file only declares targets.

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("dplyr", "tidyr", "tsibble", "fable", "feasts"),
  seed = 30,
  controller = crew::crew_controller_local(
    workers = max(1, parallel::detectCores() - 2)
  )
)

tar_source()

list(
  # Employment data, built from raw PDET microdata by R/pdet_extract.R
  tar_target(emprego_file, "Dados/emprego_uf.csv", format = "file"),
  tar_target(emprego, read_data(state_meta, region_meta, path = emprego_file)),
  tar_target(S_brazil, compute_S(emprego, state_meta, region_meta))
)
