# Pipeline for the paper. Run with: uvr run run.R
# Functions are defined in R/; this file only declares targets.

library(targets)
library(tarchetypes)

# One BLAS thread per process: the crew workers already use every core,
# and multithreaded BLAS in each worker oversubscribes the CPU.
Sys.setenv(OPENBLAS_NUM_THREADS = "1", OMP_NUM_THREADS = "1")

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
  tar_target(S_brazil, compute_S(emprego, state_meta, region_meta)),
  tar_target(hierarchies, list(small = small_hierarchy(), brazil = S_brazil)),

  # Experiment 1: controlled error design (R/experiment1.R)
  tar_target(exp1_pop, exp1_population(hierarchies)),
  tar_target(exp1_scenarios, exp1_sample_scenarios()),
  tar_target(
    exp1_samp,
    exp1_sample(
      hierarchies[[exp1_scenarios$hierarchy]],
      exp1_scenarios$hierarchy,
      exp1_scenarios$family,
      exp1_scenarios$dial,
      exp1_scenarios$T,
      reps = 1000
    ),
    pattern = map(exp1_scenarios)
  ),
  tar_target(exp1_summary, exp1_sample_summary(exp1_samp)),
  tar_target(num_exp1, numbers_exp1(exp1_pop, exp1_summary, exp1_samp)),
  tar_target(fig_gain, fig_exp1_gain(exp1_pop, "Imagens/exp1_gain.pdf"), format = "file"),
  tar_target(
    fig_samplesize,
    fig_exp1_samplesize(exp1_summary, "Imagens/exp1_samplesize.pdf"),
    format = "file"
  ),

  # The kappa diagnostic (R/diagnostic.R)
  tar_target(
    fig_kronecker,
    fig_kronecker_vs_gain(exp1_pop, "Imagens/kronecker_vs_gain.pdf"),
    format = "file"
  ),
  tar_target(fig_noise, fig_kappa_noise(exp1_samp, "Imagens/kappa_noise.pdf"), format = "file"),
  tar_target(power_scenarios, kappa_power_scenarios()),
  tar_target(
    power,
    kappa_power(
      hierarchies[[power_scenarios$hierarchy]],
      power_scenarios$hierarchy,
      power_scenarios$family,
      power_scenarios$dial,
      power_scenarios$T,
      reps = 1000
    ),
    pattern = map(power_scenarios)
  ),
  tar_target(fig_power, fig_kappa_power(power, "Imagens/test_power.pdf"), format = "file"),

  # Experiment 2: time-series DGPs with fitted base models (R/experiment2.R)
  tar_target(exp2_scenario_list, exp2_scenarios()),
  tar_target(
    exp2_pop,
    purrr::imap(exp2_scenario_list, \(s, nm) {
      exp2_population(s, hierarchies$small) |> dplyr::mutate(scenario = nm, .before = 1)
    }) |>
      dplyr::bind_rows()
  ),
  tar_target(
    exp2_grid,
    tidyr::expand_grid(scenario = names(exp2_scenario_list), T_train = c(108, 400), batch = 1:20)
  ),
  tar_target(
    exp2_sims,
    exp2_batch(
      exp2_grid$scenario,
      exp2_scenario_list,
      hierarchies$small,
      exp2_grid$T_train,
      reps = 25,
      batch = exp2_grid$batch
    ),
    pattern = map(exp2_grid)
  ),
  tar_target(exp2_sum, exp2_summary(exp2_sims, exp2_pop)),
  tar_target(table_exp2, tab_exp2(exp2_sum, "Tabelas/exp2.tex"), format = "file"),
  tar_target(num_exp2, numbers_exp2(exp2_sum)),

  # Application: Brazilian admissions and dismissals, 2007-2019
  # (R/application_analysis.R, R/application_prob.R)
  tar_target(app_origin_list, app_origins(emprego)),
  tar_target(
    app_results,
    app_origin(emprego, S_brazil, app_origin_list, K = 1000),
    pattern = map(app_origin_list),
    iteration = "list"
  ),
  tar_target(app_point, dplyr::bind_rows(purrr::map(app_results, "point"))),
  tar_target(app_prob_scores, dplyr::bind_rows(purrr::map(app_results, "prob"))),
  tar_target(app_diag, app_diagnostic(emprego, S_brazil)),
  tar_target(app_acc, app_accuracy(app_point)),
  tar_target(app_prob_sum, app_prob_summary(app_prob_scores)),
  tar_target(table_app_arima, tab_app_accuracy(app_acc, "Tabelas/app_accuracy_arima.tex", "arima"), format = "file"),
  tar_target(table_app_var, tab_app_accuracy(app_acc, "Tabelas/app_accuracy_var.tex", "var"), format = "file"),
  tar_target(table_app_diag, tab_app_diag(app_diag, "Tabelas/app_diag.tex"), format = "file"),
  tar_target(app_net, app_net_point(app_point)),
  tar_target(table_app_prob, tab_app_prob(app_prob_sum, app_net, "Tabelas/app_prob.tex"), format = "file"),
  tar_target(fig_data, fig_app_data(emprego, "Imagens/app_data.pdf"), format = "file"),
  tar_target(num_power, numbers_power(power)),
  tar_target(num_app, numbers_app(app_diag, app_acc, app_prob_sum, app_net)),

  # In-text numbers for the paper (R/results.R)
  tar_target(numbers, write_numbers(c(num_exp1, num_exp2, num_power, num_app), "Tabelas/numbers.tex"), format = "file")
)
