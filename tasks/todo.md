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

---

## 1. Decisions needed from Rob before starting

- [ ] **D1. Sample period for the application.** Recommended: main analysis
      2004–2019, which has a consistent CAGED definition (192 months), with
      2004–2023 as a robustness check using intervention dummies for 2020.
      The alternative is 2004–2023 throughout, with dummies.
- [ ] **D2. Net-change constraint in or out.** Recommended: *out* of the main
      paper and mentioned in the discussion as the case where joint
      reconciliation is genuinely needed. It fits the general `C y = 0`
      framework and would widen the paper's scope. Keep the *probabilistic*
      net-change analysis (Section 5.4 below) *in*, because it shows where
      cross-variable information actually matters.
- [ ] **D3. Structured estimator in or out.** Optional extension: shrink `W_hat`
      towards its nearest separable approximation, with a data-driven
      intensity. That interpolates between univariate and multivariate
      reconciliation. Recommended: *out* for this paper; one paragraph of
      future work.
- [ ] **D4. Base models.** Recommended: keep ARIMA and VAR, and move ETS to the
      supplement. ETS added little in the current results and it lengthens
      every table.
- [ ] **D5. Title.** "Separability and the limits of multivariate forecast
      reconciliation" (preferred in the revision notes).
- [ ] **D6. Coauthor roles.** This is close to a new paper. Agree with Ana,
      Rodrigo and Paulo who drafts which sections, and whether Ana stays first
      author.

---

## 2. Target paper structure

Target length is about 25 manuscript pages plus a supplement. Check the Journal
of Forecasting limits (task P7.1).

| # | Section | Content | Main outputs |
|---|---|---|---|
| 1 | Introduction | Paragraphs 1–10 of the revision notes, updated for §0.1 | none |
| 2 | Reconciling several variables | Notation (compress current Sec 2.1), `S* = I_m (x) S`, `C* = I_m (x) C`, MinT on the stacked system stated as a definition, not a contribution | Fig 1 (hierarchy diagram, keep) |
| 3 | When does joint reconciliation matter? | Lemma (invariance), Theorem (iff block-diagonal `G`), Cor 1 (separability), Cor 2 (Zyskind/OLS), Prop 3 (coherent population forecasts under node-invariant dynamics, §0.1), remarks on shrinkage, horizon, and `m > 2`. Proofs in the Appendix | none |
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

1. **Data (D1):** 2004–2019 for the main analysis. Justify by the definitional
   change and COVID, not by convenience.
2. **Base forecasts:** ARIMA per series (and VAR per node for comparison).
   Consider log or Box-Cox transforms: the state series differ by orders of
   magnitude and have heteroskedastic errors. If transformed, reconcile on the
   original scale, because coherence is linear in levels. Document the choice.
3. **Evaluation:**
   - Rolling origin with many more origins, for example 48 or more, with each
     training window at least 120 months.
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
6. **Robustness (supplement):** 2004–2023 with 2020 dummies; VAR base model; ETS.

---

## 6. Code changes (file by file)

Do everything on a branch; don't edit the current files on `main`.

- [x] Create branch `jf-rewrite` from `main` (the rewrite happens there; `main`
      keeps the pre-rewrite version).
- [ ] **New file `R/theory.R`:** `make_C(S)`, `mint_map(W, S_star)`,
      `G_matrix(W, C_star)`, `kappa(W, C_star, m, scale = TRUE)`,
      `nearest_kronecker(W, m, n)`, `pop_mse(M, W)`, and the family
      constructors F0–F4.
- [ ] **New file `R/test_theory.R` (testthat):**
  - The Theorem: mv = uv exactly when `G` is block diagonal (random `W`).
  - Corollary 1 for random `V` and `Sigma_0`.
  - Invariance to `S* K S*'`.
  - Corollary 2.
  - Kappa scale invariance under the chosen convention.
  - Proposition 3 (coherent population forecasts under node-invariant filters).
- [ ] **New file `R/experiment1.R`:** parts (a) and (b), cached to
      `Saida/exp1_*.rds`.
- [ ] **New file `R/experiment1_figures.R`:** Figures 4 and 5.
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
- [ ] **`Makefile`:** add targets for exp1, kappa and netchange, and update the
      table and figure lists.
- [ ] **`README.md`:** update to match.
- [ ] Remove or fold in `R/test.R` and `R/test2.R` if they are superseded.

---

## 7. Work order, with checkpoints

### Phase 0: groundwork
- [ ] P0.1 Make decisions D1–D6.
- [x] P0.2 Create the `jf-rewrite` branch.
- [ ] P0.3 **Literature gate.** Search for prior statements of the Theorem or
      Corollary 1:
  - multi-variable reconciliation;
  - the SUR analogue in reconciliation;
  - Kronecker covariance in cross-temporal work (Di Fonzo & Girolimetto;
    Girolimetto et al. on cross-temporal probabilistic reconciliation);
  - general linearly constrained reconciliation;
  - the geometric view (Panagiotelis et al. 2021).

  If the result is already stated somewhere, re-weight the contribution towards
  kappa, the noise floor and the application *before* writing anything.
- [ ] P0.4 Update `revision-abstract-intro.md` for §0.1 and §0.2, or write the
      update into the new Section 3 draft.

### Phase 1: theory and tests
- [ ] P1.1 Write `R/theory.R`.
- [ ] P1.2 Write `R/test_theory.R`. **Checkpoint:** all identities hold to
      about 1e-10.
- [ ] P1.3 Write the proofs (Appendix) and the Section 3 statements in LaTeX.
- [ ] P1.4 Verify Proposition 3 numerically: a long simulated path from the
      current DGP, with the base forecasts using the true marginal filters, is
      coherent.

### Phase 2: Experiment 1
- [ ] P2.1 Part (a) population grid, then Fig 4. **Checkpoint:** zero gain on
      F0, F1 and F4 to machine precision, and gain increasing with kappa on F2
      and F3.
- [ ] P2.2 Part (b) finite sample, then Fig 5. **Checkpoint:** a clear
      crossover `T*` that grows as kappa falls.

### Phase 3: diagnostic tooling
- [ ] P3.1 Settle the scale convention and verify it.
- [ ] P3.2 Bootstrap null and power on the Experiment 1 families. Check that
      the size is correct under F0 (rejection rate about 5%).
- [ ] P3.3 Figs 2 and 3.

### Phase 4: Experiment 2 (expensive; run in the background)
- [ ] P4.1 Implement the new DGP scenarios, and run a smoke test with
      `nsim = 20`.
- [ ] P4.2 Compute population kappa per scenario. **Checkpoint:** kappa is
      about 0 for the negative controls. If the non-separable scenarios give
      tiny kappa, trigger the contingency in Section 3.
- [ ] P4.3 Full runs, then Table 1.

### Phase 5: application
- [ ] P5.1 Update the data window and base forecasts, and add the uv, OLS and
      WLS baselines. Produce Table 2.
- [ ] P5.2 Diagnostic on the real ARIMA residuals: Table 3 and Fig 8.
- [ ] P5.3 Net-change probabilistic comparison: Table 4.
- [ ] P5.4 Robustness runs for the supplement.

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
      Convert from `elsarticle` accordingly and update the Makefile.
- [ ] P7.2 Supplement: rebuild `supplementary_material.tex` around the new
      tables.
- [ ] P7.3 Reproducibility: run `make clean-generated && make` from a clean
      checkout and confirm the PDFs rebuild.
- [ ] P7.4 Coauthor review round.
- [ ] P7.5 Cover letter: the contribution in three sentences.
- [ ] P7.6 Decide whether to post an arXiv preprint.

---

## 8. Risks

| Risk | Mitigation |
|---|---|
| The Theorem is already known | P0.3 literature gate. The contribution then rests on kappa, the invariance, the noise floor and the net-change result. |
| Referees call the Theorem trivial | Present the package: the iff characterisation, the invariance explaining which departures matter, Proposition 3, and quantified finite-sample limits. |
| Experiment 2 cannot generate meaningful non-separability | Expected given §0.1. Report it; Experiment 1 carries the argument. |
| The kappa test has almost no power at the application's size | That is the finding (the noise floor). Show it with the power curve rather than hide it. |
| 2004–2019 is too short once many origins are used | Use a minimum 120-month window, which still gives 72 origins. If needed, fall back to 2004–2023 with dummies. |
| The paper grows too long | Main text: one table per experiment. Everything per-node goes to the supplement. |

---

## Review

(To be filled in as phases complete.)
