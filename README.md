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

## Reproducing the results

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

Applies the methodology to the Brazilian employment data in `Dados/Dados_emprego_rgi.csv`. Fits ARIMA and VAR models with rolling-origin cross-validation and performs multivariate and univariate reconciliation. Fitted models are cached in `Saida/mod_arima_regiao.rds` and `Saida/mod_var_regiao.rds`.

```r
source("R/application_tables.R")
```

LaTeX tables are written to `Tabelas/`.

```r
source("R/application_figures.R")
```

Generates figures in `Imagens`

## Compiling the paper

After running the R scripts, compile the paper and supplementary material:

```bash
latexmk -pdf multivariate-reconciliation.tex
latexmk -pdf supplementary_material.tex
```

The compiled PDFs are `multivariate-reconciliation.pdf` and `supplementary_material.pdf`.
