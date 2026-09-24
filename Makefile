# Makefile for the multivariate reconciliation paper
#
# The analysis is a targets pipeline (_targets.R) run with the project's
# locked R library (uvr). targets decides what is out of date, so the
# pipeline target always runs and only stale steps are recomputed.
#
# Common targets:
#   make              Run the pipeline, then build the paper and supplement PDFs.
#   make pipeline     Run the targets pipeline only (simulations, application,
#                     figures, tables and in-text numbers).
#   make paper        Run the pipeline, then build the paper PDF only.
#   make pdf-only     Build the paper PDF from existing outputs.
#   make supplement   Build the supplement PDF.
#   make status       Show which pipeline targets are out of date.
#   make test         Run the test suite.
#   make sync         Install the locked R packages (uvr sync).
#   make raw-data     Download raw CAGED microdata from PDET (several hours).
#   make emprego      Rebuild Dados/emprego_uf.csv from the raw microdata.
#   make clean        Remove LaTeX auxiliary files.

SHELL := /bin/bash

RUN := uvr run
LATEXMK := latexmk
LATEXMKFLAGS := -pdf -interaction=nonstopmode -halt-on-error

.PHONY: all paper pipeline pdf-only supplement status test sync raw-data emprego clean help

all: paper supplement

paper: pipeline
	$(LATEXMK) $(LATEXMKFLAGS) multivariate-reconciliation.tex

pipeline:
	$(RUN) run.R

pdf-only:
	$(LATEXMK) $(LATEXMKFLAGS) multivariate-reconciliation.tex

supplement: pipeline
	$(LATEXMK) $(LATEXMKFLAGS) supplementary_material.tex

status:
	@echo 'print(targets::tar_outdated())' > .tar_status.R; $(RUN) .tar_status.R; rm -f .tar_status.R

test:
	$(RUN) tests/testthat.R

sync:
	uvr sync

# -----------------------------------------------------------------------------
# Employment data from raw PDET microdata. Dados/emprego_uf.csv is tracked by
# git, so these are only needed to rebuild it. They are not part of the
# pipeline because the download takes hours and depends on an external server.
# -----------------------------------------------------------------------------

raw-data:
	$(RUN) R/pdet_download.R

emprego:
	$(RUN) R/pdet_extract.R

clean:
	$(LATEXMK) -c multivariate-reconciliation.tex
	$(LATEXMK) -c supplementary_material.tex

help:
	@sed -n '1,20p' Makefile
