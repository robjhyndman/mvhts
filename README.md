# When does multivariate forecast reconciliation help?

Ana Caroline Pinheiro, Rodrigo de Souza Bulhões, Rob J. Hyndman, Paulo Canas Rodrigues, Felix Fesca.

## Requirements

### R and R packages

R and package versions are managed by [uvr](https://github.com/nbafrank/uvr). `uvr.toml` lists the packages, `uvr.lock` pins their exact versions, and `.r-version` pins R. To install everything into a project library (`.uvr/library/`):

```bash
uvr sync
```

Run scripts with `uvr run <script.R>`, which uses only the project library.

### Workflow

The analysis is a [targets](https://docs.ropensci.org/targets/) pipeline. Functions live in `R/`, the pipeline is defined in `_targets.R`, and it is run with:

```bash
uvr run run.R
```

Tests are in `tests/testthat/` and run with `uvr run tests/testthat.R` (or `make test`).

### Quarto and LaTeX

The paper and supplement are [Quarto](https://quarto.org) documents rendered to PDF with pdflatex, so Quarto (1.4 or later) and a LaTeX distribution (e.g. TeX Live) are required. The paper uses the [quarto-journals/elsevier](https://github.com/quarto-journals/elsevier) extension, which is bundled in `_extensions/`.

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
make test       # includes checking national totals against IpeaData
```

`Dados/pdet_manifest.csv` records the size and MD5 checksum of every raw file used, since PDET occasionally re-issues files.

## Building the paper

```bash
make            # run the targets pipeline, which also renders both PDFs
```

The paper (`multivariate-reconciliation.qmd`) and supplement (`supplementary_material.qmd`) are targets of the pipeline (`tarchetypes::tar_quarto()`). They read their figures, tables and in-text numbers with `tar_read()`, so a document is re-rendered whenever anything it uses changes.

| Target            | Action                                                                  |
| ----------------- | ----------------------------------------------------------------------- |
| `make pipeline`   | Same as `make` (`uvr run run.R`); only stale steps rerun                |
| `make paper`      | Build the paper PDF and any stale targets it depends on                 |
| `make pdf-only`   | Render the paper PDF from the stored pipeline results                   |
| `make status`     | List pipeline targets that are out of date                              |
| `make test`       | Run the test suite                                                      |
| `make sync`       | Install the locked R packages (`uvr sync`)                              |
| `make supplement` | Build `supplementary_material.pdf` and any stale targets it needs       |
| `make raw-data`   | Download the raw PDET microdata (several hours)                         |
| `make emprego`    | Rebuild `Dados/emprego_uf.csv` from the raw microdata                   |
| `make clean`      | Remove Quarto and LaTeX intermediate files                              |

The full pipeline takes several hours on 8 cores, mostly in the simulation with fitted ARIMA models and in the rolling-origin application. Results are cached in `_targets/`, so later runs recompute only what has changed. `targets::tar_visnetwork()` shows the dependency graph.

The paper is split into `sections/*.qmd`, included by `multivariate-reconciliation.qmd`. Shared LaTeX settings are in `preamble.tex` and `_quarto.yml`. Figures are written to `Imagens/` and tables to `Tabelas/` by the pipeline; in-text numbers come from the `numbers` target (a named list, used as `` `r num$name` ``).

The map of Brazilian regions (`Imagens/mapa_reg.pdf`) downloads state boundaries with geobr, so its target is built once and not rerun; delete the file and invalidate the target (`targets::tar_invalidate(fig_region_map)`) to redraw it. `Imagens/diagrama_mult.pdf` is a static diagram.
