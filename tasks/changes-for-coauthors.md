# Major changes (for coauthors)

- **Employment data rebuilt from raw PDET microdata.** The previous file
  (`Dados/Dados_emprego_rgi.csv`) had 2004–2019 in reversed time order, in two
  blocks (2004–2010 and 2011–2019). This was checked against the official
  IpeaData CAGED series. All earlier application results used the reversed data
  and are invalid. 22 months (2008–2014) whose PDET archives are damaged come
  from the old file with its dates corrected in code. It matches the raw data
  exactly in all other months.
- **Application sample now 2007–2019** (previously 2004–2023). PDET publishes
  no microdata before 2007, and ending in 2019 avoids the January 2020 switch
  to Novo CAGED and the COVID shock. 2020–2023 becomes a robustness check.
- **Reproducible tooling.** R and package versions are now locked with uvr
  (`uvr.toml`, `uvr.lock`), and the workflow is moving from the Makefile to
  targets (`_targets.R`). Driver scripts moved to `scripts/` and tests to
  `tests/testthat/`. `tsDyn` (archived from CRAN) is replaced by an equivalent
  VAR(1) simulator.
- **The theory is largely known.** A literature check found the invariance
  result in Wickramasuriya (2021) and the separability result as a special case
  of Girolimetto & Di Fonzo (2025). The paper will credit these, and its main
  contribution becomes the diagnostic and the empirical findings. The current
  arXiv version (2605.17920) needs replacing.
- **New theory section drafted** (`sections/theory.tex`, proofs in
  `sections/appendix-proofs.tex`). One proposition characterises exactly when
  joint and separate reconciliation coincide. The known results
  (invariance, separability, OLS) appear as credited corollaries. A second
  proposition shows why the old simulation design could not show a difference.
- **New simulations.** Experiment 1 simulates forecast errors directly, so the
  gain from joint reconciliation can be computed exactly; Experiment 2 replaces
  the old nine-scenario design with separable controls and three
  non-separable scenarios, using fitted ARIMA and VAR models. The old design
  could not show any difference (Proposition 2).
- **A diagnostic for real data.** Two effect sizes: kappa (how much the joint
  map uses the other variable) and the plug-in gain (how much it would save).
  Plus a likelihood ratio test of whether joint reconciliation can help at all.
  Bootstrap calibration was tried and dropped because it was mis-sized.
- **Application redesigned.** 48 rolling origins over 2007–2019, univariate
  and joint MinT compared with base, OLS and WLS, the diagnostic on the real
  residuals, and a probabilistic comparison for net employment change.
- **Main empirical message.** Joint reconciliation does not improve forecasts
  of admissions or dismissals individually, but it does improve forecasts of
  net employment change (national MSE down 18%), because MinT is optimal for
  every linear combination, not just on average. For probabilistic forecasts
  of net change, keeping the dependence in the base forecasts matters most.
- **Old analysis removed.** `scripts/`, `Saida/` and the old tables and
  figures are gone (all were based on the time-reversed data). The supplement
  is rewritten.
