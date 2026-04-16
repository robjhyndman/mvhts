library(ggplot2)
library(dplyr)
library(tsibble)

source(here::here("R/simulation_setup.R"))
source(here::here("R/simulation_functions.R"))

set.seed(11)

# ================================================================
# Figures: Simulation of a hierarchical multivariate time series
# ================================================================

params <- expand.grid(
  v_name = names(V_list),
  sigma_name = names(Sigma_list),
  stringsAsFactors = FALSE
) |>
  mutate(i = row_number())

purrr::pmap(params, function(v_name, sigma_name, i) {
  filename <- here::here(sprintf("Imagens/%d.pdf", i))
  Y <- sim_mvhts(120, Phi, V_list[[v_name]], Sigma_list[[sigma_name]]) |>
    sim_aggregate() |>
    as_tibble() |>
    mutate(
      node = factor(
        node,
        levels = c("Total", "agg_1", "agg_2", 1:5),
        labels = c("Total", "A", "B", "AA", "AB", "BA", "BB", "BC")
      )
    )
  pdf(filename, width = 9, height = 5)
  p <- Y |>
    ggplot(aes(x = time, y = value, color = series)) +
    facet_wrap(vars(node), ncol = 2) +
    geom_line() +
    labs(x = "Time", color = "Variable") +
    theme_bw()
  print(p)
  crop::dev.off.crop(filename)
  invisible()
})
