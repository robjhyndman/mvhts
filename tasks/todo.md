# Rewrite plan: Journal of Forecasting submission

Working thesis: **joint (multivariate) MinT reconciliation differs from
variable-by-variable reconciliation only when the reconciliation weights are not
block diagonal by variable. Separable error covariance is the main case where
they are block diagonal. For real data we measure how far the estimated
covariance is from that case, and whether the departure can be estimated well
enough to exploit.**

This plan builds on `tasks/revision-abstract-intro.md` (the theory, draft
abstract and intro, and the kappa diagnostic). It does not repeat that material;
it says what to build, in what order, and how to check each step.

---

## 0. Two findings from planning that change the design

These came out of reading the code while writing this plan. Both need to be
reflected in `revision-abstract-intro.md` before drafting text from it.

### 0.1 In the current simulation DGP, the population base forecasts are coherent

The current DGP gives every node the same bivariate VAR(1), rescaled. Each
aggregate is a linear combination of bottom series, so each aggregate follows
the same process up to scale. A population-optimal univariate forecaster is then
the *same linear filter* at every node, and linear filters commute with
aggregation. So the population base forecasts are already coherent, the
population base-error covariance has the form `S* K S*'`, and by the invariance
lemma that component is invisible to reconciliation.

Consequences:

- The earlier argument ("population `W_h` is `K (x) S Sigma S'`, hence separable")
  is correct but misses the point. That covariance is entirely in the invisible
  component. Everything reconciliation does in these simulations comes from the
  *incoherent* part of the base errors: estimation error, and different
  ARIMA/ETS orders chosen at different nodes.
- So the paper should say that what matters is the structure of the
  **incoherent component** of the base errors, not of `W_h` as a whole. This
  follows from the lemma, and it sharpens the story.
- Generating non-separability through a time-series DGP is harder than it
  looks. You need an incoherent component that differs across variables, which
  means the dynamics must differ by node, and differ in a different way for
  each variable. That motivates the two-experiment design in Section 3 below.
- Verify numerically before relying on it (task P1.4).

### 0.2 The application never compares against univariate reconciliation

`R/application.R` computes only base forecasts and *multivariate* reconciliation
(`mv_reconcile`). The univariate comparison, which is the whole point of the new
paper, does not exist for the real data. Also:

- **Test window:** only 12 forecast origins (2022). That is too few to separate
  two methods whose forecasts differ by little.
- **Data break:** the series cross the CAGED to Novo CAGED break (January 2020)
  and the COVID shock. Both distort the residual covariance, and the break may
  affect admissions and dismissals differently. That would itself be a source of
  non-separability, and a spurious one.
- **Ridge inconsistency:** `mv_reconcile` adds a `1e-8` ridge and `uv_reconcile`
  does not (`R/reconcile.R`). Harmless, but the two should be consistent.

### 0.3 Literature gate (P0.3): the theory is mostly known

Checked 2026-09-24. Items marked "verified" were read in the full text.

| Claim | Status | Source |
|---|---|---|
| Invariance of MinT to `S K S'` | **Known** (verified) | Wickramasuriya (2021, arXiv:2103.11129), Prop. 1 proof: `W_h = S Omega S' + Sigma_h`, and MinT depends only on `Sigma_h`. Classical in linear models: Rao (1967), Zyskind (1967). |
| Separable `W` gives joint = separate | **Known as a special case** (verified) | Girolimetto & Di Fonzo (arXiv:2410.19407; SMA 2025), Thm 1(3): with covariance `W (x) Omega`, optimal combination equals sequential reconciliation. Our case has no constraints in the second dimension. |
| `W = S K S' + delta I` gives OLS | **Known** | Hyndman et al. (2011); Wickramasuriya (2021). The full Rao/Zyskind condition also allows a `C' Gamma C` term. |
| iff: joint = separate exactly when `G` is block diagonal | Not found as stated | New, but mathematically light (`C*` is block diagonal). |
| kappa diagnostic with bootstrap calibration | **Not found** | Nearest relatives are covariance separability tests. |
| Joint and separate reconciliation perform almost identically | Not found | No published empirical report. |
| Zellner/SUR analogy in reconciliation | Not found | Framing only. |

Also found: **arXiv:2605.17920** (18 May 2026) is the current paper, with the
old framing and results based on the reversed data. The rewrite must replace it
(P7.6).

**Consequence: re-weight the contribution.** Section 3 of the paper presents
the theory as a short unifying proposition that credits Wickramasuriya (2021),
Hyndman et al. (2011), Girolimetto & Di Fonzo (2025) and Rao/Zyskind. The iff
condition is the organising statement, the known results are its corollaries,
and the Zellner analogy supplies the intuition. **The headline becomes
practical:** when is joint reconciliation of several variables worth doing?
Its parts are:
- the kappa diagnostic with bootstrap calibration and the noise floor
  (Section 4 of the paper);
- Proposition 3 (the incoherent-component point in §0.1);
- the simulation evidence;
- the application's near-null result;
- the probabilistic net-change result.

Citations still to check before use: Rao (1967), Zyskind (1967), Kruskal
(1968), and the SMA volume and pages for Girolimetto & Di Fonzo (2025).

---

## 1. Decisions (settled 2026-09-24 unless marked open)

- [x] **D1. Sample period:** main analysis **2007–2019** (156 months, a single
      CAGED definition). PDET publishes no microdata before 2007. 2007–2023
      with 2020 intervention dummies goes in the supplement as a robustness
      check. Novo CAGED uses on-time declarations only (CAGEDMOV), matching
      the old-CAGED "sem ajuste" definition.
- [x] **D2. Net change:** include the *probabilistic* net-change analysis
      (Section 5, item 4). The net-change *constraint* is mentioned in the
      discussion only.
- [x] **D3. Structured estimator:** out. One paragraph of future work.
- [x] **D4. Base models:** ARIMA and a per-node VAR in the main text; ETS in the
      supplement.
- [x] **D5. Title:** "Separability and the limits of multivariate forecast
      reconciliation".
- [ ] **D6. Authorship (open):** to be settled with Rodrigo and Paulo, and Ana
      informed, before submission. Ana is no longer working on the paper.
      Drafting proceeds meanwhile.

---

## 2. Target paper structure

Target length is about 25 manuscript pages plus a supplement. Check the Journal
of Forecasting limits (task P7.1).

| # | Section | Content | Main outputs |
|---|---|---|---|
| 1 | Introduction | Paragraphs 1–10 of the revision notes, updated for §0.1 | none |
| 2 | Reconciling several variables | Notation (compress current Sec 2.1), `S* = I_m (x) S`, `C* = I_m (x) C`, MinT on the stacked system stated as a definition, not a contribution | Fig 1 (hierarchy diagram, keep) |
| 3 | When does joint reconciliation matter? | Short and credit-giving (§0.3). Proposition (iff block-diagonal `G`), with known results as corollaries: invariance (Wickramasuriya 2021; Rao/Zyskind), separability (Girolimetto & Di Fonzo 2025), OLS. Then Prop 3 (coherent population forecasts under node-invariant dynamics, §0.1), and remarks on shrinkage, horizon and `m > 2`. Proofs in the Appendix | none |
| 4 | Measuring departure from block diagonality | kappa, scale convention, why the nearest-Kronecker error fails, bootstrap calibration, power | Fig 2 (kappa vs Kronecker error, the "wrong direction" example), Fig 3 (null distribution of kappa vs T) |
| 5 | Simulations | Exp 1: controlled error design. Exp 2: time-series DGP | Fig 4 (gain vs population kappa), Fig 5 (gain vs T, the noise floor), Table 1 (Exp 2 summary) |
| 6 | Application: Brazilian admissions and dismissals | Data, base forecasts, accuracy (base / OLS / WLS / uv-MinT / mv-MinT), kappa diagnostic, net-change distribution | Fig 6 (map, keep), Fig 7 (data, one combined figure), Table 2 (accuracy), Table 3 (diagnostic), Fig 8 (bootstrap null and power), Table 4 (net-change scores) |
| 7 | Discussion | Practical recommendation, constraints linking variables, estimation of cross-variable blocks, cross-temporal comparison, limitations | none |
| A | Appendix | Proofs; population kappa computation | none |
| S | Supplement | Per-node, per-horizon tables; ETS results; 2020–2023 robustness; full scenario details | Tables S* |

Material to cut from the current paper:

- The per-node, per-horizon RelRMSE tables in the main text. Move them to the
  supplement or drop them.
- The "percentage non-negative" tables. They are uninformative once the question
  is mv vs uv.
- The cross-validation diagram (`Imagens/validacao.pdf`), the long exploratory
  prose, and the state-by-state commentary.
- The Sec 3.1 step-by-step recipe, which becomes a short DGP description.

---

## 3. Simulation design

### Experiment 1: controlled error design (the main evidence)

Simulate base forecast errors directly. This is the standard device in the
reconciliation literature. Because MinT is linear and unbiased, the truth
`y = S* b` drops out: the reconciled error is `M e`, and the population MSE is
`tr(M W M')`, which can be computed exactly.

**Hierarchies:**
- H-small: the current 8-node hierarchy (`n_b = 5`), `m = 2`, so `nm = 16`.
- H-Brazil: the actual 33-node Brazil `S` (`n_b = 27`), `m = 2`, so `nm = 66`.
  Using the real hierarchy links Experiment 1 directly to the application.

**Covariance families,** each indexed by a dial parameter:

| Family | Construction | Population kappa | Role |
|---|---|---|---|
| F0 separable | `V (x) Sigma_0`, rho in {0, ±0.5, ±0.9} | 0 | negative control |
| F1 invisible | `V (x) Sigma_0 + S* K S*'`, random non-separable `K` | 0 (Kronecker error > 0) | shows the generic measure is wrong |
| F2 variable-specific hierarchy structure | `Sigma_A != Sigma_B`, cross block `rho Sigma_A^{1/2} Sigma_B^{1/2}'`; dial = distance between `Sigma_A` and `Sigma_B` (e.g. top-heavy vs bottom-heavy error variance) | > 0 | positive case |
| F3 node-varying correlation | `Sigma_0 = diag`, node-specific `rho_i = rho_bar + delta d_i`; dial = delta | > 0 | positive case |
| F4 Zyskind | `S* K S*' + tau I` | 0, and MinT = OLS | second degeneracy |

Check positive definiteness for every construction (for F3, `|rho_i| < 1`).

**Part (a), population (no sampling):** compute the uv/mv MSE ratio and kappa
exactly over a fine grid of each dial. The expected result is that the gain is
exactly 0 on F0, F1 and F4 and rises with kappa on F2 and F3. This is **Fig 4**.

**Part (b), finite sample:** draw `T` training errors from `N(0, W)`, with
`T` in {50, 100, 200, 500, 2000}. Estimate `W` with shrinkage, reconcile
independent test errors, and compare mv, uv (each variable with its own
shrinkage `lambda`) and uv-joint-blocks (the diagonal blocks of the joint
estimate). Use 1000 replications. The expected result is that for small `T`,
mv loses to uv even when population kappa > 0, and the crossover `T*` depends
on kappa and `nm`. This is **Fig 5**, and it is the quantified answer to
Referee 1.

Part (b) is cheap because nothing is fitted, so it can use many replications.

### Experiment 2: time-series DGP (realism check)

This keeps the current VAR machinery so the paper still includes fitted models.

**Scenarios:**
- **Negative controls:** three of the current nine (V1Σ2, V2Σ2, V3Σ2), plus
  `Phi = diag(0.9, 0.2)` (variable-specific but node-invariant dynamics).
- **Non-separable 1:** node-varying cross-variable innovation correlation, with
  `V_i` depending on `i`.
- **Non-separable 2:** variable-specific node covariance, `Sigma_A != Sigma_B`.
- **Non-separable 3:** dynamics that vary across nodes, in a different pattern
  for each variable (`Phi_i` differs by node, and the variable A and B diagonal
  entries follow different node patterns). By §0.1, this is the scenario most
  likely to create an incoherent component that differs between variables.

**Population kappa:** simulate one very long path (for example `T = 50,000`)
per scenario, fit the base models once, and compute kappa from the residual
covariance. Report this next to the per-replication estimated kappa.

**Sample sizes:** `T = 108` (current) and `T = 400`.

**Report:** the MSE ratio mv/uv averaged over series and horizons, with Monte
Carlo standard errors; mean estimated kappa; population kappa; and the
nearest-Kronecker error. This is **Table 1**. Move per-horizon profiles to the
supplement.

**Contingency:** if Non-separable 1–3 produce only small population kappa,
report that honestly. It is consistent with §0.1: realistic DGPs rarely create
exploitable non-separability. Experiment 1 carries the main message either way.

---

## 4. The kappa diagnostic

- `kappa(W) = ||offdiag blocks of G||_F / ||G||_F` with `G = W C*'(C* W C*')^-1`.
- **Scale convention (decide in P3.1).** Recommended: standardise each variable
  by the square root of the mean diagonal of its block of `W` before computing
  `G`. That makes kappa unit-free and leaves the reconciled forecasts unchanged
  up to the same rescaling. Verify the second claim numerically.
- **Horizon:** under `W_h = k_h W_1`, kappa does not depend on `h`. State this
  as a remark.
- **More than two variables:** off-diagonal mass over all `m(m-1)` blocks, and
  optionally a pairwise kappa matrix showing which variable pairs matter.
- **Bootstrap null:** take the nearest separable `W` of the shrinkage estimate
  (Van Loan–Pitsianis, then symmetrised and projected to positive definite),
  simulate `T` errors, re-estimate with the *same* shrinkage estimator, and
  recompute kappa. Use B = 999. As a robustness check, repeat with a residual
  bootstrap (resample rows of whitened residuals) and a block bootstrap.
- **Power:** for alternatives from F2 and F3 at the application's `nm` and `T`,
  compute the rejection rate over a grid of population kappa. This is **Fig 8**.
- **Localisation (descriptive only):** show which blocks of `G` carry the
  off-diagonal mass (by aggregation constraint and by state). Present this only
  if the power analysis says it can be estimated; otherwise say that it cannot.

---

## 5. Application redesign

1. **Data (D1):** 2007–2019 for the main analysis, from raw PDET microdata
   (`Dados/emprego_uf.csv`). Justify ending in 2019 by the definitional change
   and COVID, not by convenience.
2. **Base forecasts:** ARIMA per series (and VAR per node for comparison).
   Consider log or Box-Cox transforms: the state series differ by orders of
   magnitude and have heteroskedastic errors. If transformed, reconcile on the
   original scale, because coherence is linear in levels. Document the choice.
3. **Evaluation:**
   - Rolling origin with a 108-month minimum training window: 37 origins for
     12-step forecasts (48 for one-step), against 12 in the current paper.
   - Methods: base, OLS, WLS-structural, uv-MinT-shrink, mv-MinT-shrink.
   - Metrics: MSE ratio mv/uv (headline) and RMSSE by level, plus
     Diebold–Mariano or a Model Confidence Set for mv vs uv at the aggregate
     level.
   - This is **Table 2**.
4. **Net employment change (probabilistic):** build joint base samples
   (Gaussian with `W_hat_h`, or a block bootstrap of residual vectors). Compare:
   - (i) uv reconciliation of *independent* per-variable samples;
   - (ii) uv reconciliation of *joint* samples;
   - (iii) mv reconciliation of joint samples.

   Score admissions minus dismissals at every node with CRPS, and the pair with
   the energy score. Expected: (ii) ≈ (iii), both clearly better than (i). This
   is **Table 4**, and it is the empirical version of intro Paragraph 8.
5. **Diagnostic:** kappa, the bootstrap p-value, the power curve, and the
   nearest-Kronecker error (**Table 3** and **Fig 8**). Use the actual ARIMA
   residuals. This replaces the provisional benchmark-residual numbers in the
   revision notes.
6. **Robustness (supplement):** 2007–2023 with 2020 dummies; ETS base models.

---

## 6. Code changes

Do everything on a branch; don't edit the current files on `main`.

### 6.1 Tooling: uvr for packages, targets for the workflow

**uvr** (installed: 0.4.6) replaces the README's `pak::pak()` list with a
locked, per-project library.

- `uvr init` creates `uvr.toml` (the manifest); `uvr add <pkgs>` resolves and
  writes `uvr.lock` (exact versions and checksums); `uvr sync` installs exactly
  the lockfile into `.uvr/library/`; `uvr run <script.R>` runs R with that
  library.
- Pin R 4.6.1 in `.r-version`.
- Commit `uvr.toml`, `uvr.lock` and `.r-version`; git-ignore `.uvr/`.
- Packages: fable, feasts, tsibble, dplyr, tidyr, purrr, tibble, stringr,
  ggplot2, scales, here, fs, mvtnorm, tsDyn, sf, geobr, rnaturalearth,
  jsonlite, testthat, targets, tarchetypes, crew. Use explicit packages rather
  than the `fpp3` metapackage. Drop `fable.prophet` (loaded in
  `R/simulation.R` but never used), and drop furrr, future and parallelly once
  crew handles parallelism.
- Check that `uvr run` works from the IDE and from `Rscript`, and how to pass
  an expression (for example `targets::tar_make()`), before relying on it. If
  it can't take an expression, add a one-line `run.R`.

**targets** replaces the Makefile and the manual `.rds` caching in `Saida/`.

- `_targets.R` sets options and calls `tar_source()` on `R/`. **`R/` must then
  contain only function definitions:** scripts with top-level side effects
  (`simulation.R`, `application.R`, the `*_tables.R` and `*_figures.R`
  scripts) are replaced by targets, not ported line by line, since most are
  rewritten in Phases 2–5 anyway.
- **Tests move to `tests/testthat/`** (`test_data.R`, the new
  `test_theory.R`, and whatever survives of `test.R` and `test2.R`).
  Otherwise `tar_source()` would run them.
- **Data:** `Dados/emprego_uf.csv` enters the pipeline as a
  `format = "file"` target. The PDET download and extraction stay outside the
  pipeline, because they take hours and depend on an external server. They
  remain standalone scripts, run with `uvr run R/pdet_download.R` and
  `uvr run R/pdet_extract.R`. Their `sys.nframe()` guards stop `tar_source()`
  from running them.
- **Simulations:** `tarchetypes::tar_map()` over scenarios and
  `tar_rep()` batches over replications, executed in parallel by a
  `crew_controller_local()`. targets gives each target its own seed
  (`tar_option_set(seed = ...)`), so results do not depend on the number of
  workers. This replaces `furrr`, `future` and `mclapply` in the simulation
  code.
- **Outputs:** tables (`Tabelas/*.tex`) and figures (`Imagens/*.pdf`) are
  `format = "file"` targets. The paper and supplement PDFs are final targets
  that run `latexmk` and depend on those files.
- **Makefile:** removed once targets builds everything, so there is one
  orchestrator. The `raw-data` and `emprego` targets become the two `uvr run`
  commands above. Git-ignore `_targets/`, and delete `Saida/` once nothing
  reads it.
- **Workflow:** `uvr sync`, then `uvr run -e 'targets::tar_make()'` (or
  `run.R`). `tar_visnetwork()` gives a dependency graph worth including in the
  README.

### 6.2 File by file

- [x] Create branch `jf-rewrite` from `main` (the rewrite happens there; `main`
      keeps the pre-rewrite version).
- [x] **New file `R/theory.R`:** `make_C(S)`, `mint_map(W, S_star)`,
      `G_matrix(W, C_star)`, `kappa(W, C_star, m, scale = TRUE)`,
      `nearest_kronecker(W, m, n)`, `pop_mse(M, W)`, and the family
      constructors F0–F4.
- [x] **New file `tests/testthat/test_theory.R`:**
  - The Theorem: mv = uv exactly when `G` is block diagonal (random `W`).
  - Corollary 1 for random `V` and `Sigma_0`.
  - Invariance to `S* K S*'`.
  - Corollary 2.
  - Kappa scale invariance under the chosen convention.
  - Proposition 3 (coherent population forecasts under node-invariant filters).
- [ ] **New file `R/experiment1.R`:** functions for parts (a) and (b), with
      targets in `_targets.R`.
- [ ] **New file `R/experiment1_figures.R`:** functions for Figures 4 and 5.
- [ ] **`R/simulation_setup.R`:** add the Experiment 2 scenarios (node-varying
      `V_i`, `Sigma_A` / `Sigma_B`, `Phi_i`) and the `diag` Φ control.
- [ ] **`R/simulation_functions.R`:**
  - Generalise `sim_mvhts()` to accept node-specific `V_i` and `Phi_i` and a
    variable-specific `Sigma`.
  - In `run_one_simulation()`, also store estimated kappa, the nearest-Kronecker
    error, and uv-joint-blocks forecasts.
  - Add `population_kappa()`.
- [ ] **`R/reconcile.R`:**
  - Make the ridge consistent between `mv_reconcile` and `uv_reconcile`.
  - Add OLS and WLS-structural reconcilers.
  - Add an option for uv reconciliation to use the diagonal blocks of the joint
    `W_hat`.
- [ ] **`R/application.R`:**
  - Apply the sample period from D1.
  - Add uv, OLS and WLS reconciliation.
  - Add more forecast origins.
  - Save the residual matrices per origin, for the diagnostic.
- [ ] **New file `R/application_kappa.R`:** bootstrap null, power, localisation.
- [ ] **New file `R/application_netchange.R`:** probabilistic comparison
      (i)–(iii).
- [ ] **Tables and figures scripts:** rewrite `R/simulation_tables.R`,
      `R/application_tables.R` and `R/tables.R` for the new tables. Delete the
      generators for tables that are no longer used.
- [ ] **`_targets.R`:** targets for Experiments 1 and 2, the kappa diagnostic,
      the application, net change, tables, figures and PDFs (see 6.1).
- [ ] **`README.md`:** replace the Makefile and `pak` instructions with
      `uvr sync` and `tar_make()`.
- [ ] Move `R/test.R` and `R/test2.R` to `tests/testthat/`, or delete them if
      superseded.

---

## 7. Work order, with checkpoints

### Phase 0: groundwork
- [x] P0.1 Make decisions D1–D5 (D6 open; see Section 1).
- [x] P0.2 Create the `jf-rewrite` branch.
- [x] P0.3 **Literature gate.** Done: the theory is mostly known, so the
      contribution is re-weighted (see §0.3).
- [x] P0.4 Update `revision-abstract-intro.md` for §0.1–§0.3 (a status note at
      the top lists what is superseded).
- [x] P0.5 **uvr:** `uvr init`, pin R, `uvr add` the package list (6.1), and
      commit `uvr.toml`, `uvr.lock` and `.r-version`. **Checkpoint:** a fresh
      `uvr sync` followed by `uvr run R/pdet_extract.R` reproduces
      `Dados/emprego_uf.csv` byte for byte.
- [x] P0.6 **targets skeleton:** create `_targets.R` with `tar_source()`, crew
      and seed options, and the data file target. Move the tests to
      `tests/testthat/`. **Checkpoint:** `tar_make()` runs and
      `tar_visnetwork()` shows the data target. The remaining targets are added
      phase by phase, as each piece is written.
- [~] P0.7 Retire the old pipeline. **Makefile done (2026-09-24):** it now runs
      the targets pipeline and latexmk, plus status, test, sync and data
      targets. Still to do, at the end of Phase 5: delete `scripts/` and
      `Saida/`, and remove the old table and figure files.

Phase 0 notes (2026-09-24):
- **tsDyn replaced.** It was archived from CRAN on 2026-08-21. It was used only
  for `VAR.sim()`, which is replaced by `sim_var1()` in
  `R/simulation_functions.R`. That gives identical output to tsDyn's own code
  on 200 test paths.
- **urca must be declared.** fable's `ARIMA()` needs it, but it is only a
  suggested dependency. `uvr run` isolation exposed this.
- **Isolation confirmed.** `uvr run` and the crew workers see only
  `.uvr/library` and base R.
- **File layout.** Driver scripts moved to `scripts/` and tests to
  `tests/testthat/`, so `R/` holds only functions. Run the tests with
  `uvr run tests/testthat.R` and the pipeline with `uvr run run.R`.

### Phase 1: theory and tests
- [x] P1.1 Write `R/theory.R`: maps, `kappa_mv()` (renamed so it does not
      mask `base::kappa`), `nearest_kronecker()`, the covariance families
      F0–F4, and the Proposition 3 helpers.
- [x] P1.2 Write `tests/testthat/test_theory.R` (61 checks). **Checkpoint
      passed:** all identities hold to 1e-10. The exception is kappa = 0, which
      holds to about 1.5e-8 (sqrt of machine epsilon), so it is tested at 1e-6.
- [x] P1.3 Draft `sections/theory.tex` (Section 3) and
      `sections/appendix-proofs.tex`. They compile in a test harness, and all
      citations resolve. For the paper they need `amsthm`, plus `proposition`
      and `corollary` environments in the preamble.
- [x] P1.4 Proposition 3 verified numerically. Population AR(20) coefficients
      are identical across all 8 nodes to 1e-12, and applying them to a
      simulated path gives forecasts coherent to 1e-10.

Phase 1 findings:
- **Sharper proposition.** Joint = separate ⟺ M block diagonal ⟺ G block
  diagonal ⟺ `M_j W_jk C' = 0` ⟺ `col(W_jk C')` ⊆ `col(W_jj C')`.
  **Interpretation:** MinT regresses the errors on the observed incoherences.
  Joint reconciliation helps only if, once each variable has been reconciled
  on its own, its remaining errors are correlated with other variables'
  incoherences. This framing organises Section 3.
- **The kappa = 0 set is strictly larger** than separable plus invisible. A
  test builds a `W` with neither structure that still gives joint = separate.
- **Proposition numbering in the draft:** Prop 1 is the characterisation and
  Prop 2 is the node-invariant result, called "Prop 3" elsewhere in this plan.
- **Citations to check:** Rao (1967) and Zyskind (1967) are from memory, and
  the published SMA details for Girolimetto & Di Fonzo are missing. All three
  are marked TODO in `cas-refs.bib`.

### Phase 2: Experiment 1 (done)
- [x] P2.1 Population grid, and Fig `Imagens/exp1_gain.pdf`. Checkpoint passed:
      gain is exactly 0 for F0, F1 and F4, and rises with kappa for F2 and F3,
      to at most about 16%.
- [x] P2.2 Finite-sample study (1000 reps; T from 50 to 2000), and Fig
      `Imagens/exp1_samplesize.pdf`. Under separability, joint is 1.7–2.9% worse
      at T = 50. Weak departures are not recovered even at T = 2000.
- Design change: covariances are built from a coherent part plus an
  incoherent part. With diagonal incoherent noise, joint = separate exactly
  when `rho_i sqrt(lambda_2i/lambda_1i)` is constant across series.

### Phase 3: diagnostic (done, redesigned)
- [x] Scale convention: standardise each variable by the square root of the
      mean diagonal of its block (tested).
- [x] **Plug-in gain gamma added.** Kappa alone is not enough: in Experiment 2,
      kappa is 0.65–0.97 but the gain is under 2.5%, because only the small
      incoherent component can be improved.
- [x] **Bootstrap dropped.** The Kronecker-null and decoupling bootstraps were
      mis-sized in the 33-series hierarchy, because any null built from W-hat
      inherits its noise. Replaced by a likelihood ratio test of condition (d):
      regress each variable's bottom-level errors on its own and the other
      variable's incoherences, using Wilks' lambda with Rao's F and a
      Bonferroni combination. Size is 4–6% at T = 100 and 200.
- [ ] Figs `kronecker_vs_gain.pdf`, `kappa_noise.pdf` and `test_power.pdf`:
      built by targets. The power target reruns with the LR test.

### Phase 4: Experiment 2 (running)
- [x] P4.1–P4.2 Scenarios and exact population W (population-optimal AR(20)
      forecasts). Controls are exactly coherent (incoherence 1e-16). The
      non-separable scenarios have an incoherent share of 0.4–1.5%, kappa of
      0.65–0.97, and a gain of 0.5–2.4%.
- [ ] P4.3 Fitted ARIMA/VAR runs (6 scenarios × T in {108, 400} × 500 reps),
      then Table `tab:exp2`.

### Phase 5: application (code done; runs after Experiment 2)
- [x] P5.1–P5.3 code: 48 origins, methods base / OLS / WLS / separate /
      sep_blocks / joint, the LR diagnostic, and net-change CRPS from
      psi-weight sample paths. Point forecast + Psi e is exact.
      `fabletools::generate()` starts from a slightly wrong mean, so it is not
      used.
- [ ] Run, then write Tables 2–4 and Section 6.
- [ ] P5.4 Robustness (2007–2023, ETS) for the supplement.

### Phase 6: writing
Order: Sec 3, then Sec 4, then Sec 5, then Sec 6, then Sec 2, then Sec 7, then
the Introduction, then the Abstract.

- [ ] P6.1 Section 3 (Theorem-first framing; Proposition 3 from §0.1).
- [ ] P6.2 Section 4.
- [ ] P6.3 Section 5.
- [ ] P6.4 Section 6. Present the near-null accuracy result as a confirmed
      prediction.
- [ ] P6.5 Section 2: compressed, with no novelty claim.
- [ ] P6.6 Section 7. Practical recommendation: model the base forecasts
      jointly, reconcile variable by variable unless kappa says otherwise, and
      reconcile jointly when constraints link the variables.
- [ ] P6.7 Introduction and abstract, from the revision notes, updated with the
      real numbers.
- [ ] P6.8 Check every numerical claim in the text against the generated
      tables, with no hand-typed numbers. Generate inline numbers from R where
      practical.
- [ ] P6.9 Deslop pass; consistent notation (`W_h`, `S*`, `C*`, `G`, kappa).

### Phase 7: submission
- [ ] P7.1 Check the Journal of Forecasting (Wiley) author guidelines: length,
      abstract limit, reference style, required statements (data availability,
      conflicts, funding), whether a template is required at first submission.
      Convert from `elsarticle` accordingly and update the PDF targets.
- [ ] P7.2 Supplement: rebuild `supplementary_material.tex` around the new
      tables.
- [ ] P7.3 Reproducibility: from a fresh clone, run `uvr sync`, then
      `tar_destroy()` and `tar_make()`, and confirm the PDFs rebuild with
      identical numbers.
- [ ] P7.4 Coauthor review round.
- [ ] P7.5 Cover letter: the contribution in three sentences.
- [ ] P7.6 Replace arXiv:2605.17920 (the current version) with the rewrite, as
      a new version of the same arXiv entry, and say in the comments field that
      the data and conclusions changed.

---

## 8. Risks

| Risk | Mitigation |
|---|---|
| The theory is largely known (confirmed, §0.3) | Credit the known results explicitly. The headline is the diagnostic, the noise floor, Proposition 3 and the empirical findings. |
| Referees call the iff proposition trivial | Present it as organising known results, not as the contribution. |
| Referees find arXiv:2605.17920 with contradictory results | Replace it with the rewrite (P7.6), and explain the data error in the cover letter. |
| Experiment 2 cannot generate meaningful non-separability | Expected given §0.1. Report it; Experiment 1 carries the argument. |
| The kappa test has almost no power at the application's size | That is the finding (the noise floor). Show it with the power curve rather than hide it. |
| 2007–2019 (156 months) gives few origins | A 108-month minimum window gives 37 origins for 12-step forecasts (a 120-month window gives 25). If that is too few, add 2020–2023 with dummies. |
| The paper grows too long | Main text: one table per experiment. Everything per-node goes to the supplement. |

---

## Review

(To be filled in as phases complete.)
