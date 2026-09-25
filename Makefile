# Makefile for the multivariate reconciliation paper
#
# The analysis is a targets pipeline (_targets.R) run with the project's
# locked R library (uvr). targets decides what is out of date, so the
# pipeline target always runs and only stale steps are recomputed. The paper
# and supplement are Quarto documents rendered as targets of the pipeline.
#
# Common targets:
#   make              Run the pipeline, including rendering the paper and
#                     supplement PDFs.
#   make pipeline     Same as make.
#   make paper        Build the paper PDF and anything it depends on.
#   make supplement   Build the supplement PDF and anything it depends on.
#   make pdf-only     Render the paper PDF from the stored pipeline results.
#   make status       Show which pipeline targets are out of date.
#   make test         Run the test suite.
#   make sync         Install the locked R packages (uvr sync).
#   make raw-data     Download raw CAGED microdata from PDET (several hours).
#   make emprego      Rebuild Dados/emprego_uf.csv from the raw microdata.
#   make clean        Remove Quarto and LaTeX intermediate files.

SHELL := /bin/bash

RUN := uvr run

.PHONY: all paper pipeline pdf-only supplement status test sync raw-data emprego clean help

all: pipeline

pipeline:
	$(RUN) run.R

paper supplement:
	@echo 'targets::tar_make($@)' > .tar_make.R; $(RUN) .tar_make.R; rm -f .tar_make.R

pdf-only:
	quarto render multivariate-reconciliation.qmd

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
	rm -rf .quarto *_files *.knit.md *.log *.aux *.out

help:
	@sed -n '1,20p' Makefile
