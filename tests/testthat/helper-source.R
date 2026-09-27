# Load every function in R/, as tar_source() does for the pipeline
suppressPackageStartupMessages({
  library(dplyr)
  library(tsibble)
  library(fable)
})
for (f in list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}
