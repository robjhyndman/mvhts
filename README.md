# When does multivariate forecast reconciliation help?

Ana Caroline Pinheiro, Rodrigo de Souza Bulhões, Rob J Hyndman, Paulo Canas Rodrigues, Felix Fesca.

## Setup

R and package versions are managed by [uvr](https://github.com/nbafrank/uvr); `uvr sync` installs them into `.uvr/library/`.
Rendering the PDFs also needs [Quarto](https://quarto.org) 1.4 or later and a LaTeX distribution such as TeX Live.

## Building the paper

```bash
make            # run the pipeline, which renders both PDFs
make help       # all targets
```

The analysis is a [targets](https://docs.ropensci.org/targets/) pipeline: functions in `R/`, pipeline in `_targets.R`, tests in `tests/testthat/`.
The paper (`multivariate-reconciliation.qmd`) and supplement (`supplementary_material.qmd`) are themselves targets, and read their figures, tables and in-text numbers with `tar_read()`, so each is re-rendered whenever anything it uses changes.
The paper text is split into `sections/*.qmd`; shared LaTeX settings are in `preamble.tex` and `_quarto.yml`, and the paper uses the [quarto-journals/elsevier](https://github.com/quarto-journals/elsevier) extension bundled in `_extensions/`.
Figures go to `Imagens/`, tables to `Tabelas/`, and in-text numbers come from the `numbers` target (used as `` `r num$name` ``).

### Code in `R/`

  | File                                                                 | Contents                                                                                                             |
  | -------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- |
  | `hierarchy.R`                                                        | The two hierarchies (`S`), the stacked variable-major layout used everywhere, and aggregation of bottom-level series |
  | `theory.R`                                                           | Population quantities: MinT and separate maps, kappa, the plug-in gain, the nearest Kronecker product                |
  | `reconciliation.R`                                                   | Covariance estimators and the reconciliation methods compared in the paper (`reconciliation_maps()`)                 |
  | `base_models.R`                                                      | ARIMA, ETS and VAR base models: fitting, residuals and forecasts as matrices in stacked order                        |
  | `experiment1.R`, `diagnostic.R`                                      | Experiment 1 (controlled error covariances) and the size and power of the test                                       |
  | `simulation_functions.R`, `experiment2.R`                            | Experiment 2 (simulated time series with fitted base models)                                                         |
  | `application_data.R`, `application_analysis.R`, `application_prob.R` | The Brazilian employment application                                                                                 |
  | `results.R`, `figures.R`                                             | Tables, in-text numbers and figures                                                                                  |

A full run takes several hours on 8 cores, mostly the ARIMA simulation and the rolling-origin application.
Results are cached in `_targets/`, so later runs recompute only what has changed; `targets::tar_visnetwork()` shows the dependency graph.

The map of Brazilian regions (`Imagens/mapa_reg.pdf`) downloads state boundaries with geobr, so it is built once and not rerun; to redraw it, delete the file and call `targets::tar_invalidate(fig_region_map)`.
`Imagens/diagrama_mult.pdf` is a static diagram.

## Employment data

The pipeline reads three files of monthly admissions and dismissals, without seasonal adjustment (`read_states()` in `R/application_data.R`):

- `Dados/caged_uf_2004_2019.csv`: old CAGED counts for the 27 federative units, January 2004 to December 2019, as exported from PDET (Ministry of Labour and Employment), with one column per month running from December 2019 back to January 2004.
- `Dados/novo_caged_admissoes_2020_2023.csv` and `Dados/novo_caged_desligamentos_2020_2023.csv`: Novo CAGED admissions and dismissals by municipality for 2020–2023, downloaded from IpeaData on 14 October 2026 and summed to states.

The analysis uses January 2005 to December 2019; 2020–2023 appear only in the data figure.
