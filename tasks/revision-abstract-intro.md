# Reframing the paper: draft abstract and introduction

> **Status (2026-09-24): partly superseded.** Read with `tasks/todo.md` §0–§1.
>
> - **The theory is mostly known** (todo §0.3). The invariance lemma is in
>   Wickramasuriya (2021), the separability corollary is a special case of
>   Girolimetto & Di Fonzo (2025, Thm 1(3)), and the OLS corollary is Hyndman
>   et al. (2011) plus Rao/Zyskind. Paragraph 5 and the abstract must credit
>   these and stop claiming the result as new. The headline moves to kappa, the
>   noise floor, Proposition 3 and the empirical findings.
> - **"Why the current simulations are separable" is incomplete** (todo §0.1).
>   Under node-invariant dynamics the population base forecasts are *coherent*,
>   so their error covariance lies entirely in the invisible `S* K S*'`
>   component. What matters is the incoherent component.
> - **The Brazilian numbers at the end are void.** They came from the
>   time-reversed legacy data. The data are now rebuilt from raw PDET microdata
>   for 2007–2023, and the main sample is 2007–2019 (todo D1).
> - **The application never compared against univariate reconciliation**
>   (todo §0.2). Paragraph 6's "nine-scenario design" point is about the
>   simulations only.
> - **Title decided:** "Separability and the limits of multivariate forecast
>   reconciliation" (todo D5).

## The core problem with the current framing

The paper claims novelty in the *estimator* ("we extend MinT to several variables").
Both referees correctly observed that the estimator is MinT applied to a stacked
system, and the editor correctly observed that it buys no accuracy. Both criticisms
land because the claim is located in the wrong place.

There is a real result sitting in the paper that nobody has stated.

## The result

Write the multivariate summing matrix as `S* = I_m (x) S`, the constraint matrix
as `C* = I_m (x) C`, and let `W_h` be the (positive definite) covariance of the
stacked base forecast errors. In zero-constrained form the reconciliation map is
`M = I - G C*` with

    G = W C*' (C* W C*')^-1     (size nm x n_a m).

**Lemma (invariance).** Since `C* S* = 0`, both `W C*'` and `C* W C*'` are
unchanged when `W` is replaced by `W + S* K S*'` for any `K`. So the
reconciliation map is invariant to adding `S* K S*'` to the covariance.

**Theorem (characterisation).** Multivariate MinT returns exactly the
variable-by-variable univariate MinT forecasts, for every base forecast, if and
only if `G` is block diagonal by variable. (`C*` is block diagonal with full row
rank, so `M` is block diagonal iff `G` is.)

**Corollary 1 (separability).** If `W_h = V (x) Sigma_0` for some positive definite
`m x m` matrix `V` and `n x n` matrix `Sigma_0` common to all variables, then

    S*(S*' W^-1 S*)^-1 S*' W^-1  =  I_m (x) S(S' Sigma_0^-1 S)^-1 S' Sigma_0^-1

for any `V`. Two lines of Kronecker algebra: `S*' W^-1 S* = V^-1 (x) S'Sigma_0^-1 S`,
and the `V` factors cancel. More generally, the collapse holds for
`W = V (x) Sigma_0 + S* K S*'` for any `K`, by the lemma. Separability is sufficient,
not necessary.

**Corollary 2 (Zyskind).** If `W = S* K S*' + delta I`, MinT collapses to OLS
reconciliation. This is the same invariance again, and is a second way a
simulation design can degenerate.

This is the reconciliation analogue of Zellner's result that seemingly unrelated
regressions with identical regressors collapse to equation-by-equation least
squares. On its own, Corollary 1 is short enough that a referee may call it
trivial. The contribution is the package: the iff characterisation, the invariance
that explains which departures from separability matter, and the finite-sample
result (below) on whether those departures can be detected and exploited.

### Verified numerically
- Separable `W`, cross-variable correlation 0, +-0.7, +-0.9, 0.999:
  max difference between multivariate and univariate reconciled forecasts is
  order 1e-15, while MinT itself differs from OLS reconciliation by ~56%.
  So the collapse is not a degenerate case where everything becomes OLS.
- Break separability (different error-variance profiles per variable, or
  cross-variable correlation varying by node) and the two methods diverge
  substantially, by 7% to 140% depending on the strength of cross-variable
  correlation.

### Why the current simulations are separable

The plan originally argued from the innovations alone (`cov(vec(E_t)) = V (x) Sigma`
propagated through `I_m (x) S`). That is not enough: `W_h` is the covariance of
the *base forecast errors*, and the DGP has VAR(1) dynamics with `Phi` coupling
the variables. The claim still holds, for a stronger reason:

- Every series (variable j, node a) is `s_a' eta_j` plus a deterministic seasonal
  term, where `s_a` is row a of `S`.
- The bivariate process `(s_a' eta_1, s_a' eta_2)` is the *same* VAR(1) with
  innovation covariance `(s_a' Sigma s_a) V` at every node, differing only in scale.
  Cross-node covariances likewise factor as `(s_a' Sigma s_b)` times a fixed
  `m x m` matrix.
- So any base forecaster that is linear, scale-equivariant and identical across
  nodes produces errors with covariance `K_h (x) S Sigma S'`. Separable.

The code confirms the design: `kronecker(Sigma, V)` in
`R/simulation_functions.R:82` with the variable index running fastest, which is
`V (x) Sigma` in the paper's variable-major ordering.

**Caveat: separability holds for the population, not for the estimate.**
Auto-selected ARIMA/ETS orders differ across series, and the shrinkage target
`diag(W_hat)` is not a Kronecker product, so the estimated `W_h` is only
approximately separable. The Table 5 values of +-0.003 are a close confirmation
of the theorem, not an exact one. The paper should say "agree to within
estimation noise", not "exactly".

A related trap for the rebuilt simulation: variable-specific dynamics do **not**
break separability on their own. `Phi = diag(0.9, 0.2)` gives very different
processes for the two variables, but each is the same across nodes, so `W_h`
is still separable and that scenario is another negative control.

### Consequences for this paper
1. **All nine simulation scenarios are separable in the population**, for the
   reason above. The design cannot show a difference beyond estimation noise.
2. **The editor's complaint becomes the paper's main finding.** "Most relative
   scores close to zero, which implies identical performance" is what the
   theorem predicts.
3. **Reviewer 1's grouped-hierarchy objection has a sharp answer.** In a grouped
   hierarchy the variables are linked by *constraints*: the total over travel
   purposes is observed, so `S` is not block diagonal. Here the variables are
   linked only by *correlation*: `S* = I_m (x) S` is block diagonal and nothing
   sums admissions to dismissals. The theorem shows this distinction has teeth.
   When constraints do not link the variables, correlation changes the
   reconciled forecasts only through the part of `W` that makes `G`
   non-block-diagonal.
4. **The case for the paper's method narrows, and becomes clearer.** Joint
   reconciliation is needed when a constraint links the variables (see item 5
   under "What else has to change"). Cross-variable dependence in the base
   forecasts matters for joint probabilistic forecasts, but it can be carried
   through separate reconciliation maps (see Paragraph 8).

---

## Draft abstract

Forecast reconciliation is normally applied to one variable observed over a
hierarchy of aggregation levels. When several variables share the same hierarchy,
such as worker admissions and dismissals recorded by region, it seems natural to
reconcile them jointly so that cross-variable dependence can inform the adjustment.
We show that this intuition is usually wrong. Joint minimum trace reconciliation
returns exactly the forecasts obtained by reconciling each variable separately if
and only if its reconciliation weights are block diagonal by variable. This holds
in particular whenever the covariance matrix of the base forecast errors is
separable, factorising as a Kronecker product of a cross-variable matrix and a
cross-series matrix common to all variables, whatever the sign or strength of the
cross-variable correlation. Because the reconciliation map ignores any component
of the covariance lying in the column space of the summing matrix, many departures
from separability also leave the forecasts unchanged. The result explains why
joint and separate reconciliation perform almost identically in our simulations,
whose design is separable by construction. We identify the structures that do
change the reconciled forecasts, namely hierarchy-level error structures that
differ between variables and cross-variable correlations that vary by node, and
we propose a diagnostic that measures directly how far the reconciliation weights
depart from block diagonality, calibrated against a bootstrap null. On twenty
years of monthly Brazilian employment data we cannot reject that the weights are
block diagonal, and at this sample size the diagnostic's noise floor is so high
that any departure could not be estimated well enough to exploit. Joint
predictive distributions for functions of several variables, such as net
employment change, need a joint model for the base forecasts, but under these
conditions the reconciliation step itself can be done variable by variable. Joint
reconciliation is needed when constraints link the variables, as when net
employment change is itself forecast.

[ROB: the application sentence depends on recomputing kappa with the paper's
ARIMA residuals. See the note at the end.]

**Keywords:** hierarchical time series, forecast reconciliation, minimum trace,
separable covariance, Kronecker product, employment forecasting

### Suggested title
"Multivariate reconciliation for hierarchical time series" now understates the
paper. Preferred:

- *Separability and the limits of multivariate forecast reconciliation*

Alternative, suitable only if the iff theorem is the headline:

- *When does cross-variable dependence change a reconciled forecast?*

---

## Draft introduction

**Paragraph 1 (keep, compressed).** Collections of time series can often be
aggregated at multiple levels by geography, product classification, or other
attributes. Forecasts are usually required for every series at every level, and
they should be coherent, so that each aggregate forecast equals the sum of the
disaggregate forecasts below it. Applications span tourism, macroeconomics,
energy, retail, and healthcare (Athanasopoulos et al., 2023).

**Paragraph 2 (keep, compressed).** Hyndman et al. (2011) showed that coherent
forecasts can be obtained by forecasting every series independently and then
reconciling. Wickramasuriya et al. (2019) showed that the reconciliation weights
minimising the trace of the coherent forecast error covariance are a generalised
least squares solution using `W_h`, the covariance of the base forecast errors.
Their MinT method is now the standard approach.

**Paragraph 3 (reframed as a question, not a gap).**
Reconciliation is almost always applied to a single variable. Yet many hierarchies
carry several variables measured over the same structure. Brazil records monthly
worker admissions and dismissals for each of 27 federative units, which aggregate
to five regions and to the country. The two variables are driven by common labour
market conditions and their forecast errors are correlated. Since MinT works by
exploiting the covariance of base forecast errors, one would expect that
reconciling both variables jointly, with a `W_h` spanning them, should beat
reconciling each separately. This paper asks when that expectation is correct.
The answer is: less often than one would think, and for reasons that
are not visible from the estimator itself.

**Paragraph 4 (pre-empt the grouped-hierarchy objection).**
Joint reconciliation of several variables is not the same problem as a grouped
hierarchy, although the two are easily confused. In a grouped hierarchy, such as
the Australian tourism data of Wickramasuriya et al. (2019) disaggregated by both
region and travel purpose, the categories of each grouping variable are linked by
*aggregation constraints*: total tourism is the sum over purposes, and that total
is observed and forecast. The summing matrix is not block diagonal. In the setting
we consider, nothing sums admissions to dismissals. The summing matrix is exactly
`S* = I_m (x) S`, block diagonal by variable, and the only channel connecting the
variables is the error covariance `W_h`. This structural difference is what makes
the problem worth separate treatment, and, as we show, it has a consequence that
does not arise for grouped hierarchies.

**Paragraph 5 (the main result).**
We give an exact characterisation. Joint MinT reconciliation returns the same
point forecasts as reconciling each variable on its own if and only if the
reconciliation weights are block diagonal by variable. The most important
sufficient condition is separability: `W_h = V (x) Sigma_0`, where `V` is the
`m x m` cross-variable covariance and `Sigma_0` is an `n x n` covariance across
the hierarchy that is the same for every variable. Under separability the
cross-variable covariance cancels from the reconciliation weights, so strong
positive correlation, strong negative correlation and independence all give
identical answers. The mechanism is the same one behind Zellner's (1962)
observation that seemingly unrelated regressions with identical regressors
collapse to equation-by-equation least squares, and it applies here because
every variable shares the hierarchy `S`. Separability is not necessary: the
reconciliation map ignores any component of `W_h` of the form `S* K S*'`, so
non-separability in that component is invisible. The same invariance explains
the known collapse of MinT to OLS reconciliation.

**Paragraph 6 (why this matters, and the diagnosis).**
The result explains a pattern that is easy to misread as a failed experiment.
Simulation designs for multivariate reconciliation typically generate
bottom-level errors with a Kronecker covariance `V (x) Sigma`, pass them through
dynamics that are the same at every node, and aggregate up the hierarchy. The
resulting base forecast error covariance is separable in the population, so the
design can show no benefit from joint reconciliation beyond estimation noise,
however the cross-variable correlation is varied. We verify this on a
nine-scenario design of that form, where joint and separate reconciliation agree
to three decimal places in every scenario. When the population `W_h` is separable,
any accuracy difference that appears in practice comes from estimating `W_h`, not
from the cross-variable dependence the method was meant to exploit.

**Paragraph 7 (constructive half).**
Joint reconciliation changes the point forecasts only when the reconciliation
weights are not block diagonal, which requires a departure from separability
outside the invisible `S* K S*'` component. Such a departure requires the
structure across the hierarchy to differ between variables. Variable-specific
dynamics are not enough: if each variable follows its own process, but the same
process at every node, the error covariance remains separable. We identify two
departures that are empirically plausible. The first is variables whose
forecast errors are distributed differently across the hierarchy, as when one
variable is much harder to forecast at the bottom level relative to the top than
the other, or when aggregation changes the dynamics of one variable more than
the other. The second is a cross-variable error correlation that varies across
nodes, so that admissions and dismissals move together in some states and not in
others. The standard measure of distance from separability, the relative error
of the nearest Kronecker product approximation (Van Loan and Pitsianis, 1993),
is the wrong diagnostic here because it responds to components that
reconciliation ignores, and can move in the opposite direction to the quantity
of interest. We instead measure the off-diagonal block mass of the
reconciliation weights directly, calibrate it against a parametric bootstrap
null, and report it for the Brazilian data.

**Paragraph 8 (what the cross-variable information is for).**
Point forecast accuracy is not where cross-variable dependence earns its keep.
Users of these forecasts care about net employment change, the difference between
admissions and dismissals, and about churn rates, which are ratios. Forecasting
any function of several variables requires their joint predictive distribution,
and requires that distribution to respect the hierarchy for every variable at
once. The joint information enters through the base forecast distribution, not
through the reconciliation map. Reconciliation pushes the joint base distribution
through a linear map, so when that map is block diagonal, applying separate
reconciliation to joint base samples gives exactly the same coherent joint
distribution as joint reconciliation (in the Gaussian case both give the same
`M W M'`). The practical recommendation is therefore to model the variables
jointly at the base forecast stage, then reconcile variable by variable unless
the diagnostic indicates otherwise. Joint reconciliation becomes essential when
constraints link the variables. Net employment change is a case in point: if it
is forecast as a series in its own right, it imposes a signed cross-variable
constraint that no variable-by-variable reconciliation can respect.

**Paragraph 9 (honest positioning against prior work).**
A related separability result is known in cross-temporal reconciliation, where
Di Fonzo and Girolimetto (2023, 2025) show that iterative reconciliation across
the cross-sectional and temporal dimensions coincides with the optimal combination
solution when the error covariance has a Kronecker structure. Our setting differs
in that the second dimension carries no aggregation constraint at all, which
sharpens the conclusion from "the order of reconciliation does not matter" to
"the second dimension does not matter". The net-change constraint is an instance
of reconciliation under general linear constraints (Girolimetto and Di Fonzo,
general linearly constrained reconciliation), and we position it as an
application of that framework rather than as a new method.
[ROB: check the exact conditions of the cross-temporal result and the reference
for the general-linear-constraints paper before submission.]

**Paragraph 10 (roadmap, short).**
Section 2 sets out notation and the reconciliation problem for several variables.
Section 3 proves the characterisation, its corollaries, and the invariance, and
characterises the departures that matter. Section 4 reports simulations,
contrasting separable designs with non-separable ones. Section 5 applies the
diagnostic to the Brazilian employment data.

---

## What else has to change

The reframing is not honest unless the rest of the paper follows.

1. **Section 2.2 keeps the derivation but loses the novelty claim.** Present
   `S* = I_m (x) S` as a definition, not a contribution. State plainly that the
   estimator is MinT on the stacked system. That concession costs nothing once
   the theorem is the contribution, and it disarms both referees.

2. **New Section 3 with the theorem and its proof.** Structure: invariance lemma,
   then the iff characterisation (block-diagonal `G`), then Corollary 1
   (separability, including `V (x) Sigma_0 + S* K S*'`) and Corollary 2 (Zyskind:
   `W = S* W_b S*' + delta I` collapses MinT to OLS). Then the population
   separability argument for Kronecker-innovation VAR designs with node-invariant
   dynamics, including the point that variable-specific dynamics alone do not
   break it.

3. **The simulation must be rebuilt.** The current nine scenarios are all separable
   in the population. Keep three of them as the *negative control*, demonstrating
   the theorem, and add `Phi = diag(0.9, 0.2)` as a fourth control to show that
   variable-specific dynamics alone change nothing. Add scenarios that break
   separability through a node-by-variable interaction: a different `Sigma` per
   variable, a cross-variable correlation (`V` or `Phi`) that varies by node, and
   node-heterogeneous dynamics that differ between variables. For each scenario
   report the population kappa (computed from the true `W_h`) alongside the
   estimated kappa and the accuracy, so the reader sees accuracy gain tracking
   kappa. Report the nearest-Kronecker error too, as a contrast showing why it is
   the wrong measure.

4. **The application needs the diagnostic, not just the accuracy table.**
   Report kappa, its bootstrap null and p-value, and a power curve under a few
   non-separable alternatives at the application's sample size. Present the
   near-null accuracy result as a confirmed prediction rather than a
   disappointment, and the noise floor as the answer to "when is this worth
   doing".

5. **Add the net-change constraint.** Admissions minus dismissals is net
   employment change, and if it is forecast too, it introduces a genuine
   cross-variable constraint with coefficients +1 and -1. A signed contrast of
   this kind cannot be represented by a 0/1 grouped-hierarchy summing matrix,
   but it fits the general `C y = 0` framework of Girolimetto and Di Fonzo. This
   is now the case where joint reconciliation is genuinely needed, so it anchors
   the paper's positive recommendation. Present it as an application of that
   framework, not a new method.

6. **Rewrite the probabilistic argument.** Wherever the paper argues for joint
   reconciliation via joint distributions, restate it as an argument for joint
   *base* modelling. If probabilistic results are reported, show separate
   reconciliation of joint base samples alongside joint reconciliation, and
   show that they coincide when kappa is 0.

---

# Measuring departure from separability

## What exists in the literature

Separability of a covariance matrix is a well-studied question, though the work
sits in spatio-temporal and matrix-variate statistics rather than forecasting.

- **Van Loan & Pitsianis (1993)**, *Approximation with Kronecker products*.
  Rearrange `W` so that each `n x n` block becomes a row, then take the SVD.
  `W` is exactly separable iff the rearranged matrix has rank one, so
  `sqrt(1 - sigma_1^2 / sum sigma_i^2)` is the natural relative error of the
  nearest Kronecker product. This is the standard descriptive measure.
- **Lu & Zimmerman (2005)** give the likelihood ratio test for Kronecker-product
  covariance under normality.
- **Mitchell, Genton & Gumpertz (2005, 2006)** develop LRTs for separability of
  spatio-temporal covariances; **Fuentes (2006)** gives a spectral test and
  **Li, Genton & Sherman (2007)** a nonparametric one.
- **Aston, Pigoli & Tavakoli (2017)** test separability of covariance operators
  for functional data.

So the generic question has accepted answers. None of them is the right tool here.

## Why the generic measure is the wrong diagnostic

Reconciliation does not see all of `W`: by the invariance lemma, adding
`S* K S*'` to `W` leaves the reconciliation map unchanged. Any non-separability
living in that component is invisible. Verified: adding `S* K S*'` with a random
non-separable `K` moved the nearest-Kronecker error from 0.085 to 0.246 while the
difference between multivariate and univariate reconciliation stayed fixed at
0.4707 to four decimals.

Worse, in the family where cross-variable correlation is dialled from 0 to 0.9,
the nearest-Kronecker error **falls** from 0.098 to 0.073 while the reconciliation
difference **rises** from 0 to 1.6. Correlation with the quantity of interest is
-0.85. The generic measure points the wrong way.

## The measure to use instead

With `G = W C*' (C* W C*')^-1`, multivariate MinT equals variable-by-variable
univariate MinT **exactly when `G` is block diagonal by variable** (the theorem).
This condition is weaker than separability: separable `W` implies block-diagonal
`G`, but not conversely.

Take as the measure the off-diagonal block mass

    kappa(W) = ||offdiag blocks of G||_F / ||G||_F

- Exactly 0 iff joint reconciliation changes nothing.
- Automatically inherits the `S* K S*'` invariance.
- Correlates 0.91 with the realised `||P_mv - P_uv||_F / ||P_uv||_F`.

**Scale dependence.** kappa is not invariant to the units of the variables.
Rescaling variable 2 by `c` multiplies the off-diagonal blocks of `G` by `c` and
`1/c`, which changes kappa. Admissions and dismissals are on similar scales, so
this matters little for the application, but the paper must fix a convention.
Options: standardise each variable (e.g. by the trace of its diagonal block of
`W`) before computing kappa, or report the change in reconciled forecasts in
standardised units. Decide which and state it.

## kappa must be calibrated, not read on an absolute scale

kappa is consistent but badly inflated in finite samples. Under an exactly
separable truth with `nm = 16`:

| T | median kappa |
|---|---|
| 100 | 0.44 |
| 500 | 0.22 |
| 2 000 | 0.12 |
| 10 000 | 0.05 |
| 50 000 | 0.02 |

It decays at about `T^-1/2`. So always compare the observed kappa to a parametric
bootstrap null: fit the nearest separable `W`, simulate at the same sample size,
re-estimate `W` with the same (shrinkage) estimator, recompute kappa, and read
off a p-value.

Choices in the bootstrap that the paper must state:

- **Which null.** kappa is 0 on the larger set of `W` with block-diagonal `G`, not
  just separable `W`. Using the nearest separable `W` as the null is defensible
  (it is the natural fitted null and is easy to simulate from), but it is one
  member of the null set and the paper should say so.
- **Resampling scheme.** iid Gaussian draws ignore heavy tails and any residual
  autocorrelation. At minimum, check the p-value under a residual bootstrap
  (resampling rows of the rotated residuals) or a block bootstrap.
- **Power.** A test whose null median is 0.65 has little power. Compute a power
  curve under a few non-separable alternatives (node-varying cross-variable
  correlation, different `Sigma` per variable) at the application's `T`.
  "Cannot be rejected" means little without it.

## Result for the Brazilian data (provisional)

With 227 monthly residuals on 66 series (`n = 33`, `m = 2`), shrinkage estimate
of `W`:

| Quantity | Value |
|---|---|
| nearest-Kronecker relative error | 0.037 |
| observed kappa | 0.719 |
| median kappa under exactly separable null, same T | 0.647 |
| 95% quantile of null | 0.867 |
| p-value | 0.33 |

Block diagonality of `G` (and a fortiori separability) cannot be rejected. The
near-zero accuracy differences in the application are what the theory predicts,
and the raw kappa of 0.72 is mostly estimation noise. Do not claim to have
located where any non-separable part lives: the data cannot resolve it.

The noise floor is the more useful framing, and it answers Referee 1's request
for attention to high dimensionality and the estimation of `W`. At 66 series and
227 months the noise floor on kappa is around 0.65, so even if a non-separable
component existed it could not be estimated well enough to exploit. Joint
reconciliation needs either many more observations or a structured estimator for
the cross-variable blocks. That is a concrete, quantified answer to "when is this
worth doing", which is what the referee said the paper was missing. The power
curve makes this precise.

[ROB: residuals here come from a lag-1/12/13 plus monthly dummy benchmark, not
the paper's ARIMA fits. Recompute with the real residuals, the chosen kappa
scaling convention, and the power curve before quoting any of these numbers.]
