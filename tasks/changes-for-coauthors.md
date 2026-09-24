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
