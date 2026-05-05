# Multivariate reconciliation for hierarchical time series

Ana Caroline Pinheiro, Rodrigo de Souza Bulhões, Rob J. Hyndman, Paulo Canas Rodrigues.

## Requirements

### R packages

Install all required packages from CRAN:

```r
pak::pak(c(
  "dplyr", "fable.prophet", "fpp3", "fs", "furrr", "geobr",
  "ggplot2", "here", "mvtnorm", "parallelly", "rnaturalearth",
  "scales", "sf", "stringr", "tsDyn", "tsibble"
))
```

### LaTeX

A standard LaTeX distribution (e.g. TeX Live or MiKTeX) with `latexmk` is required to compile the paper. The paper uses the Elsevier `elsarticle` class, which is bundled in this repository.

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
source("R/simulation.R")
```

Runs 9 Monte Carlo simulation scenarios (3 cross-series correlation structures × 3 cross-node correlation structures), each with 1000 replications using parallel processing. Results are saved to `Saida/sim_rec1.rds` through `Saida/sim_rec9.rds`. Completed scenarios are skipped on re-runs.

```r
source("R/simulation_tables.R")
```

LaTeX tables are written to `Tabelas/`.

```r
source("R/simulation_figures.R")
```

Generates figures in `Imagens`

### 2. Real-data application

```r
source("R/application.R")
```

Applies the methodology to the Brazilian employment data in `Dados/Dados_emprego_rgi.csv`. Fits ARIMA and VAR models with rolling-origin cross-validation and performs multivariate and univariate reconciliation. Fitted models are cached in `Saida/mod_arima_regiao.rds` and `Saida/mod_var_regiao.rds`. Accuracy measures are saved to `Saida/app_relrmse.rds`.

```r
source("R/application_tables.R")
```

Writes LaTeX tables to `Tabelas/`.

```r
source("R/application_figures.R")
```

Generates figures in `Imagens`

### Compiling the paper

After running the R scripts, compile the paper and supplementary material:

```bash
latexmk -pdf multivariate-reconciliation.tex
latexmk -pdf supplementary_material.tex
```

The compiled PDFs are `multivariate-reconciliation.pdf` and `supplementary_material.pdf`.
