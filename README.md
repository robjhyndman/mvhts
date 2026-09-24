# Multivariate reconciliation for hierarchical time series

Ana Caroline Pinheiro, Rodrigo de Souza Bulhões, Rob J. Hyndman, Paulo Canas Rodrigues.

## Requirements

### R and R packages

R and package versions are managed by [uvr](https://github.com/nbafrank/uvr). `uvr.toml` lists the packages, `uvr.lock` pins their exact versions, and `.r-version` pins R. To install everything into a project library (`.uvr/library/`):

```bash
uvr sync
```

Run scripts with `uvr run <script.R>`, which uses only the project library.

### Workflow

The analysis is being migrated to [targets](https://docs.ropensci.org/targets/). Functions live in `R/`, the pipeline is defined in `_targets.R`, and it is run with:

```bash
uvr run run.R
```

Until the migration is complete, the older scripts in `scripts/` are run by the Makefile described below. Tests are in `tests/testthat/` and run with `uvr run tests/testthat.R`.

### LaTeX

A standard LaTeX distribution (e.g. TeX Live or MiKTeX) with `latexmk` is required to compile the paper. The paper uses the Elsevier `elsarticle` class, which is bundled in this repository.

### Other tools

`curl` and `7z` (p7zip) are needed only to rebuild the employment data from the raw microdata.

## Employment data

`Dados/emprego_uf.csv` holds monthly admissions and dismissals for the 27 Brazilian federative units. It is built entirely by script from the raw CAGED microdata published by PDET (Ministry of Labour and Employment):

- 2007–2019: old CAGED, files `CAGEDEST_MMYYYY.7z`. PDET does not publish microdata for earlier years.
- 2020–2023: Novo CAGED on-time declarations, files `CAGEDMOVYYYYMM.7z`.

Records with no identified state (about 1% in Novo CAGED) are dropped.

22 old-CAGED archives between 2008 and 2014 are damaged on the PDET server (listed in `R/pdet_download.R`). Those months are taken from `Dados/legacy/Dados_emprego_rgi.csv`, an earlier extract of the same data with its time order reversed within 2004–2010 and 2011–2019. The reversal is undone in code (`read_legacy()` in `R/pdet_extract.R`). For every intact month the remapped legacy file agrees with the raw microdata to within one record per state, and `tests/testthat/test_data.R` checks this. The `source` column of `Dados/emprego_uf.csv` records where each month came from.

To rebuild it:

```bash
make raw-data   # download ~5GB into Dados/pdet/ (not tracked; several hours, resumable)
make emprego    # count admissions and dismissals by month and state
uvr run tests/testthat.R   # includes checking national totals against IpeaData
```

`Dados/pdet_manifest.csv` records the size and MD5 checksum of every raw file used, since PDET occasionally re-issues files.

## Using the Makefile

The simplest approach is to use `make` as the main interface for reproducibility. It handles dependencies and only rebuilds stale outputs.

```bash
make          # same as: make all
```

Individual targets can also be specified:

| Target                 | Action                                                      |
| ---------------------- | ----------------------------------------------------------- |
| `make paper`           | Build `multivariate-reconciliation.pdf`                     |
| `make supplement`      | Build `supplementary_material.pdf`                          |
| `make tables`          | Regenerate all LaTeX tables in `Tabelas/`                  |
| `make figures`         | Regenerate all figures in `Imagens/`                       |
| `make data`            | Regenerate cached R outputs in `Saida/`                    |
| `make simulation`      | Run simulation scripts only                                 |
| `make application`     | Run application scripts only                                |
| `make pdf-only`        | Compile PDFs without regenerating R outputs                 |
| `make clean`           | Remove LaTeX auxiliary files                                |
| `make clean-generated` | Remove generated caches, tables, and figures               |


## Not using the Makefile

Run the following R scripts. Each script caches its output in `Saida/` so it can be resumed if interrupted.

### 1. Simulation study

```r
source("scripts/simulation.R")
```

Runs 9 Monte Carlo simulation scenarios (3 cross-series correlation structures × 3 cross-node correlation structures), each with 1000 replications using parallel processing. Results are saved to `Saida/sim_rec1.rds` through `Saida/sim_rec9.rds`. Completed scenarios are skipped on re-runs.

```r
source("scripts/simulation_tables.R")
```

LaTeX tables are written to `Tabelas/`.

```r
source("scripts/simulation_figures.R")
```

Generates figures in `Imagens`

### 2. Real-data application

```r
source("scripts/application.R")
```

Applies the methodology to the Brazilian employment data in `Dados/emprego_uf.csv`. Fits ARIMA and VAR models with rolling-origin cross-validation and performs multivariate and univariate reconciliation. Fitted models are cached in `Saida/mod_arima_regiao.rds` and `Saida/mod_var_regiao.rds`. Accuracy measures are saved to `Saida/app_relrmse.rds`.

```r
source("scripts/application_tables.R")
```

Writes LaTeX tables to `Tabelas/`.

```r
source("scripts/application_figures.R")
```

Generates figures in `Imagens`

### 3. Compiling the paper and supplementary material

After running the R scripts, compile the paper and supplementary material:

```bash
latexmk -pdf multivariate-reconciliation.tex
latexmk -pdf supplementary_material.tex
```

The compiled PDFs are `multivariate-reconciliation.pdf` and `supplementary_material.pdf`.
