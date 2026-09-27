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

| File | Contents |
| --- | --- |
| `hierarchy.R` | The two hierarchies (`S`), the stacked variable-major layout used everywhere, and aggregation of bottom-level series |
| `theory.R` | Population quantities: MinT and separate maps, kappa, the plug-in gain, the nearest Kronecker product |
| `reconciliation.R` | Covariance estimators and the reconciliation methods compared in the paper (`reconciliation_maps()`) |
| `base_models.R` | ARIMA, ETS and VAR base models: fitting, residuals and forecasts as matrices in stacked order |
| `experiment1.R`, `diagnostic.R` | Experiment 1 (controlled error covariances) and the size and power of the test |
| `simulation_functions.R`, `experiment2.R` | Experiment 2 (simulated time series with fitted base models) |
| `application_data.R`, `application_analysis.R`, `application_prob.R` | The Brazilian employment application |
| `results.R`, `figures.R` | Tables, in-text numbers and figures |
| `pdet_download.R`, `pdet_extract.R` | Scripts that rebuild the employment data (not part of the pipeline; see below) |

A full run takes several hours on 8 cores, mostly the ARIMA simulation and the rolling-origin application.
Results are cached in `_targets/`, so later runs recompute only what has changed; `targets::tar_visnetwork()` shows the dependency graph.

The map of Brazilian regions (`Imagens/mapa_reg.pdf`) downloads state boundaries with geobr, so it is built once and not rerun; to redraw it, delete the file and call `targets::tar_invalidate(fig_region_map)`.
`Imagens/diagrama_mult.pdf` is a static diagram.

## Employment data

`Dados/emprego_uf.csv` holds monthly admissions and dismissals for the 27 Brazilian federative units, built entirely by script from the raw CAGED microdata published by PDET (Ministry of Labour and Employment): old CAGED for 2007–2019 (PDET publishes no earlier microdata) and Novo CAGED on-time declarations for 2020–2023.
Records with no identified state (about 1% in Novo CAGED) are dropped.

22 old-CAGED archives between 2008 and 2014 are damaged on the PDET server (listed in `R/pdet_download.R`).
Those months come instead from `Dados/legacy/Dados_emprego_rgi.csv`, an earlier extract of the same data with its time order reversed within 2004–2010 and 2011–2019; `read_legacy()` in `R/pdet_extract.R` undoes the reversal.
For every intact month the remapped legacy file agrees with the raw microdata to within one record per state, which `tests/testthat/test_data.R` checks.
The `source` column of `Dados/emprego_uf.csv` records where each month came from, and `Dados/pdet_manifest.csv` the size and MD5 checksum of every raw file used, since PDET occasionally re-issues files.

Rebuilding it needs `curl` and `7z` (p7zip):

```bash
make raw-data   # download ~5GB into Dados/pdet/ (not tracked; several hours, resumable)
make emprego    # count admissions and dismissals by month and state
make test       # includes checking national totals against IpeaData
```
