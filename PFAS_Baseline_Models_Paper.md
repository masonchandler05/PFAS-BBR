# Industrial, Airport, and Military Proximity and PFAS in U.S. Drinking Water
## Baseline IDW–Tobit Models, Their Offshoots, and What Survives Validity Testing

*Working paper — Big Blue Research (BBR) Scholars Program. July 2026.*
*All results reproducible from the scripts named in each section; all model
output CSVs live in `model_outputs_military/` and `model_outputs_censored/`.*

---

## 0. One-page summary

**Question.** Does proximity to PFAS-relevant sites — industrial facilities,
civilian airports, and military installations — causally raise PFAS levels in
public drinking water?

**Design.** For every PFAS-tested water-system location in the complete UCMR5
dataset (26,260 locations, 10,151 systems), we build exponential
inverse-distance-weighted (IDW) exposure indices to six source sets at three
decay ranges (5, 10, 25 km), and estimate left-censored (Tobit) regressions of
log concentration on exposure, with treatment covariates and standard errors
clustered by water system. Models are run on two samples: **bucket A** (only
locations that detected at least one PFAS compound — the design the project
used historically) and **bucket B** (all tested locations — the preferred,
selection-free sample). No fixed effects are used anywhere (§2.4).

**Baseline result.** In bucket B, essentially every source–compound pair is
positive and significant: PFOS and PFHxS respond most strongly to military
AFFF-related sites, PFOA to industrial density, PFBA/PFPeA to airports. A
standard deviation of exposure moves latent concentrations by roughly 5–20%.

**Validity testing overturns much of that.** A matched **facility placebo**
(same construction from non-PFAS industries) kills the milbase/milfire
gradients and industrial PFOS but spares suspected-AFFF sites (milsusp),
airport→PFHxS, and industrial→PFOA. A **county population-density control**
is more brutal: it absorbs everything except **airport→PFHxS**
(β = 0.12–0.13, p ≤ 0.009 with placebo, industry, and density all in the
model). Density itself predicts PFAS at p ≈ 1e-13…1e-25 in every model.

**Bottom line.** (1) PFAS occurrence in U.S. drinking water is, first and
foremost, an urbanization phenomenon. (2) The one source-specific causal
signal that survives every test is the AFFF fingerprint compound PFHxS
tracking civilian airports. (3) The suspected-AFFF military result and the
industrial PFOA result are real associations that survive a facility placebo
but cannot currently be separated from county-level urbanization — they are
"between the estimate and zero," not proven and not refuted. Next steps
(§7) are designed to break exactly that tie.

---

## 1. The baseline model

Every result in this paper is a variant of one specification:

$$\log(C_{ij}^*) = \beta \cdot \text{IDW}_{ij}(\lambda) + \gamma' X_j + \varepsilon_{ij}$$

- $C_{ij}^*$: latent (true) concentration of compound $c$ at sampling
  location $i$ of water system $j$. Observed only when above the UCMR5
  reporting limit (MRL); otherwise we know only $C \le \text{MRL}$.
  Estimated by **Tobit** (left-censored Gaussian survival regression,
  `survreg`), censoring at each compound's official MRL taken from the UCMR5
  data itself (e.g., PFOS/PFOA 4 ng/L, PFHxS/PFBS 3 ng/L).
- $\text{IDW}_{ij}(\lambda) = \sum_{k \in 10\text{NN}} w_k e^{-d_{ik}/\lambda}$:
  exponential inverse-distance exposure summed over the 10 nearest source
  points, with decay $\lambda \in \{5, 10, 25\}$ km
  (`build_weighted_idw()` in `naics_tiers.R` — identical machinery for
  every source set).
- $X_j$: UCMR5 system covariates — disinfectant type, treatment technology,
  PFAS-specific treatment, self-reported PFAS occurrence/source indicators
  (~40 dummies; constant/aliased columns dropped).
- **Clustering:** all standard errors clustered by PWSID
  (`+ cluster(PWSID)`, robust). Locations within a system share one
  coordinate and one exposure value, so unclustered SEs are mechanically
  overconfident (the Moulton problem).

### 1.1 The six source sets

| Set | Points | What it is |
|---|---|---|
| `all_equal` | 21,479 | FRS facilities in the 39 presumptive-PFAS NAICS codes, equal weight |
| `tierweighted` | 21,479 | Same facilities, weighted 1.0/0.5/0.15 by PFAS-likelihood tier ("Spec D") |
| `airport` | 774 | FRS NAICS 488119 (airport operations — AFFF use) |
| `milfire` | 305 | DoD fire/crash training installations (2014 KBCRS inventory), geocoded via EWG's map layer + MIRTA |
| `milsusp` | 91 | EWG "suspected AFFF discharge" military sites (2025 layer) |
| `milbase` | 737 | All MIRTA military installations |

Combined military sets (with 2.5 km cross-source dedup) exist in the results
files but the paper focuses on the six singles — combinations add little
beyond their strongest member.

### 1.2 The two sampling buckets

- **Bucket A ("any-detect", 6,314 locations / 3,500 systems).** Only
  locations that detected ≥1 PFAS compound. This replicates the project's
  original construction, which silently dropped all-non-detect locations.
  It conditions the sample on an outcome and is kept for comparison.
- **Bucket B ("all tested", 26,260 locations / 10,151 systems).** Every
  PFAS-tested location; non-detects enter the Tobit as censored. This is
  the selection-free sample and the paper's default.

Detection rates differ enormously by bucket (PFOS: 36% in A, 8.7% in B),
which is why bucket choice changes results: bucket B contains the full
detection margin, including thousands of small, rural, all-ND systems.

### 1.3 Why Tobit (and not OLS on detects)

UCMR5 non-detects are not missing — they are "below X ng/L." OLS on detected
values only (the project's original spec) conditions on the outcome and is
badly biased when exposure operates on the detection margin: it produced a
false null for PFHxS and a spurious *negative* for PFBS, both reversed by the
Tobit (diagnosis in `diagnose_pfhxs.R`; the PFBS sign flip is
`results_military_full_clustered.csv`, bucket A vs B). The Tobit uses every
location and every piece of information. Its Gaussian assumption is checked
by an assumption-light detection-margin LPM, which agrees with the Tobit
throughout (both margins are in every results CSV).

### 1.4 Why no fixed effects

ZCTA fixed effects absorb ~100% of the exposure variation (exposure is one
number per system; systems rarely share ZCTAs with meaningfully different
exposure), so FE "significance" was driven by thin, unrepresentative slices
of data. This was diagnosed early (`diagnose_fe_variation.R`,
`SIMPLE_MODELS_EXPLAINED.md`) and all FE specifications were abandoned.
The identification burden that FE would have carried is instead addressed
head-on by the placebo and density tests of §5.

---

## 2. Baseline results

All tables: Tobit, covariate spec, clustered SEs. β = per-unit coefficient
(≈ proportional effect on latent concentration); β/SD = effect of a
one-standard-deviation exposure increase (comparable across sources);
`***` p<.01, `**` p<.05, `*` p<.10. Bucket A: n = 6,314 (3,500 clusters);
bucket B: n = 26,223 (10,143 clusters).
Full grids incl. detection-margin models:
`model_outputs_military/results_military_full_clustered.csv`,
`model_outputs_censored/results_industrial_full_clustered.csv`.

### 2.1 PFOS (legacy AFFF sulfonate — the headline compound)

| Source | λ km | A: β (SE) | A: p | B: β (SE) | B: p | B: β/SD |
|---|---|---|---|---|---|---|
| airport | 5 | 0.073 (0.067) | .28 | 0.236 (0.079) | .003*** | 0.058 |
| airport | 10 | 0.030 (0.033) | .36 | 0.185 (0.037) | 6e-7*** | 0.105 |
| airport | 25 | 0.016 (0.016) | .31 | 0.124 (0.018) | 5e-12*** | 0.169 |
| milfire | 5 | 0.381 (0.145) | .009*** | 0.199 (0.146) | .17 | 0.032 |
| milfire | 10 | 0.420 (0.081) | 2e-7*** | 0.257 (0.092) | .005*** | 0.077 |
| milfire | 25 | 0.214 (0.035) | 1e-9*** | 0.212 (0.046) | 4e-6*** | 0.139 |
| milsusp | 5 | 1.160 (0.279) | 3e-5*** | 0.827 (0.346) | .017** | 0.064 |
| milsusp | 10 | 0.825 (0.162) | 4e-7*** | 0.539 (0.183) | .003*** | 0.081 |
| milsusp | 25 | 0.499 (0.082) | 1e-9*** | 0.317 (0.087) | 3e-4*** | 0.100 |
| milbase | 5 | 0.207 (0.095) | .029** | 0.158 (0.093) | .087* | 0.043 |
| milbase | 10 | 0.208 (0.056) | 2e-4*** | 0.180 (0.055) | .001*** | 0.089 |
| milbase | 25 | 0.134 (0.027) | 4e-7*** | 0.159 (0.028) | 2e-8*** | 0.156 |
| industry equal | 5 | 0.020 (0.012) | .10 | 0.065 (0.013) | 1e-6*** | 0.131 |
| industry equal | 10 | 0.018 (0.010) | .072* | 0.070 (0.011) | 3e-10*** | 0.178 |
| industry equal | 25 | 0.019 (0.010) | .062* | 0.083 (0.013) | 7e-11*** | 0.214 |
| industry tier | 5 | 0.027 (0.018) | .14 | 0.088 (0.020) | 7e-6*** | 0.118 |
| industry tier | 10 | 0.022 (0.014) | .12 | 0.092 (0.016) | 4e-9*** | 0.156 |
| industry tier | 25 | 0.020 (0.014) | .14 | 0.094 (0.015) | 9e-10*** | 0.171 |

Reading: in bucket B everything is significant; the *military point sources*
(milsusp, milfire) have the largest per-unit coefficients, while diffuse
sources gain per-SD strength with radius. Bucket A mutes the diffuse sources
entirely (airport, industry n.s.) but keeps the military ones — the
military effects never depended on the detection margin, the diffuse ones do.

### 2.2 PFHxS (the ECF-AFFF fingerprint compound)

| Source | λ km | A: β (SE) | A: p | B: β (SE) | B: p | B: β/SD |
|---|---|---|---|---|---|---|
| airport | 5 | 0.185 (0.065) | .004*** | 0.369 (0.069) | 8e-8*** | 0.091 |
| airport | 10 | 0.052 (0.031) | .098* | 0.214 (0.035) | 1e-9*** | 0.121 |
| airport | 25 | −0.014 (0.016) | .39 | 0.094 (0.018) | 2e-7*** | 0.129 |
| milfire | 5 | 0.519 (0.138) | 2e-4*** | 0.329 (0.130) | .012** | 0.052 |
| milfire | 10 | 0.336 (0.078) | 2e-5*** | 0.199 (0.078) | .011** | 0.059 |
| milfire | 25 | 0.084 (0.035) | .015** | 0.097 (0.039) | .014** | 0.063 |
| milsusp | 5 | 1.543 (0.267) | 7e-9*** | 1.138 (0.395) | .004*** | 0.087 |
| milsusp | 10 | 0.889 (0.150) | 3e-9*** | 0.620 (0.212) | .003*** | 0.093 |
| milsusp | 25 | 0.376 (0.086) | 1e-5*** | 0.267 (0.096) | .006*** | 0.084 |
| milbase | 5 | 0.315 (0.097) | .001*** | 0.236 (0.097) | .015** | 0.064 |
| milbase | 10 | 0.192 (0.052) | 3e-4*** | 0.161 (0.056) | .004*** | 0.080 |
| milbase | 25 | 0.049 (0.025) | .056* | 0.088 (0.027) | 9e-4*** | 0.086 |
| industry equal | 10 | 0.002 (0.010) | .83 | 0.053 (0.012) | 2e-5*** | 0.134 |
| industry tier | 10 | 0.010 (0.015) | .51 | 0.074 (0.017) | 2e-5*** | 0.125 |

Reading: in bucket A — where the detection-margin/density channel is muted —
PFHxS loads **only on AFFF-related sources** (milsusp ≫ milfire, milbase,
airport-5km) and is flat zero on industry. That clean fingerprint is a key
piece of evidence that the AFFF mechanism is real. In bucket B industry
"lights up" too — a warning sign correctly flagged by the density test (§5.5).

### 2.3 PFOA (the manufacturing carboxylate)

| Source | λ km | A: β (SE) | A: p | B: β (SE) | B: p | B: β/SD |
|---|---|---|---|---|---|---|
| industry equal | 5 | 0.024 (0.009) | .006*** | 0.057 (0.010) | 4e-9*** | 0.115 |
| industry equal | 10 | 0.030 (0.008) | 7e-5*** | 0.067 (0.008) | 1e-16*** | 0.172 |
| industry equal | 25 | 0.040 (0.008) | 6e-7*** | 0.086 (0.009) | 2e-21*** | 0.223 |
| industry tier | 10 | 0.032 (0.011) | .002*** | 0.083 (0.011) | 5e-14*** | 0.142 |
| industry tier | 25 | 0.033 (0.010) | .002*** | 0.088 (0.011) | 2e-16*** | 0.160 |
| airport | 25 | 0.042 (0.013) | .001*** | 0.120 (0.014) | 7e-17*** | 0.164 |
| milbase | 25 | 0.134 (0.022) | 1e-9*** | 0.155 (0.021) | 7e-14*** | 0.152 |
| milfire | 25 | 0.190 (0.035) | 4e-8*** | 0.201 (0.039) | 3e-7*** | 0.132 |
| milsusp | 10 | 0.011 (0.161) | .94 | 0.032 (0.115) | .78 | 0.005 |

Reading: PFOA is the mirror image of PFHxS — **industry-led, strengthening
with radius** (its per-SD effect nearly doubles from 5 to 25 km, consistent
with PFOA's high mobility), significant even in bucket A, and *not*
responsive to the suspected-AFFF sites. Compound-source differentiation of
this kind is not something a generic artifact produces — but §5 shows part of
it is still density.

### 2.4 All eight compounds at a glance

Count of positive-significant (p<.05) Tobit models out of 18 (6 sources × 3
radii), plus wrong-signed counts:

| Compound | A: +sig | B: +sig | A: −sig | B: −sig |
|---|---|---|---|---|
| PFOS | 9 | 16 | 0 | 0 |
| PFHxS | 9 | 18 | 0 | 0 |
| PFOA | 11 | 12 | 0 | 0 |
| PFHxA | 14 | 12 | 0 | 0 |
| PFPeA | 12 | 12 | 0 | 0 |
| PFHpA | 5 | 11 | 0 | 0 |
| PFBA | 3 | 9 | 8 | 4 |
| PFBS | 0 | 7 | 6 | 0 |

Two structural facts: (i) bucket B strengthens nearly everything —
consistent with more power *and* with a density channel operating on the
detection margin; (ii) **PFBS flips sign across buckets** (6 significant
negatives in A, 7 significant positives in B). The infamous "PFBS paradox"
of earlier analyses was a collider artifact of the any-detect sample:
conditioning on detecting *something* manufactured a negative correlation
that does not exist in the full population.

---

## 3. Offshoot analyses on the baseline (what we learned along the way)

Each of these was run on the same Tobit-clustered machinery.

**3.1 Dose–response (PFOA × tier-weighted, bucket B;
`results_pfoa_deepdive_dose.csv`).** Latent PFOA rises monotonically across
exposure quintiles: Q2 +0.24 (p=2e-4), Q3 +0.35 (2e-7), Q4 +0.59 (5e-17),
Q5 +0.54 vs Q1; detection rates rise 2.9% → 14.3% (Q1→Q4). A gradient, not a
top-tail artifact. (The mild Q4→Q5 plateau is consistent with treatment
investment at the most exposed systems.)

**3.2 Within-set weighting horse race (`results_pfoa_deepdive_horserace.csv`).**
Tier-weighted and equal-weight industrial IDW are interchangeable: jointly,
equal-weight wins (tier −0.04 n.s.; equal +0.09, p=2e-5). The fine
PFAS-likelihood gradations *within* the 39-code set carry no extra signal —
but the *boundary* of the set does (§5.4).

**3.3 Sector decomposition (`results_pfoa_deepdive_sectors.csv`).** Every
industrial sector "predicts" PFOA and PFOS with near-identical coefficients —
including chemical *wholesalers* (distributors, β=0.17, p=1e-16) and
printing. Ten sectors moving two chemically distinct compounds in lockstep is
the signature of a common factor (density), and was the finding that
triggered the placebo program.

**3.4 System-size robustness (`results_pfoa_deepdive_size.csv`).** The
industrial PFOA effect is unchanged by the UCMR5 L/S size class and holds
within large (0.053, p=2e-4) and small (0.151, p=3e-20) systems separately —
stronger in small systems, consistent with less treatment between source and
tap.

---

## 4. Historical dead ends (why the modeling looks the way it does)

For the record, three model families were tried and rejected before the
baseline stabilized: **ZCTA fixed effects** (absorb all exposure variation;
significance was a thin-slice artifact), **detects-only OLS** (selection on
the outcome; produced the false PFHxS null and fake PFBS negative), and a
**UCMR3→UCMR5 difference-in-differences** (null; crippled by 10–40×
reporting-limit changes across rounds). Their outputs remain in
`model_outputs/` but carry no evidentiary weight.

---

## 5. The validity gauntlet

The baseline's threat is confounding: PFAS sources sit where people sit.
Three escalating tests. All at 10 km, bucket B, Tobit + covariates,
clustered.

**5.1 Facility placebo — industry
(`results_industrial_placebo.csv`, `placebo_industrial.R`).**
An IDW from 11,867 FRS facilities in non-PFAS sectors (food, beverage, wood,
furniture; identical filters and construction; corr with the real index 0.79).

| PFOA | real index | placebo |
|---|---|---|
| each alone | 0.067 (p=1e-16) | 0.061 (p=5e-11) |
| **jointly** | **0.066 (p=3e-8)** | **0.002 (p=.89)** |

| PFOS | real index | placebo |
|---|---|---|
| **jointly** | 0.044 (p=.003) | 0.040 (p=.014) |

PFOA passes decisively — proximity to PFAS-relevant industry specifically,
not factories in general. PFOS splits 50/50 — its industrial gradient is
substantially generic.

**5.2 Facility placebo — military/airport
(`results_military_placebo*.csv`, `placebo_military.R`).**
Same placebo raced against each military source (and again with the real
industrial index added):

| Source, compound | alone | + placebo | + placebo + industry |
|---|---|---|---|
| milsusp, PFOS | 0.54 (.003) | 0.42 (.031) | 0.41 (.043) |
| milsusp, PFHxS | 0.62 (.003) | 0.55 (.014) | 0.54 (.019) |
| airport, PFHxS | 0.21 (1e-9) | 0.17 (9e-6) | 0.15 (4e-4) |
| airport, PFOS | 0.19 (6e-7) | 0.086 (.034) | 0.055 (.21) |
| milbase, PFOS | 0.18 (.001) | 0.12 (.032) | 0.11 (.059) |
| milfire, PFOS | 0.26 (.005) | 0.13 (.18) | 0.11 (.28) |

The AFFF-specific lists survive (milsusp, airport-PFHxS); the generic
installation lists (milbase, milfire-2014) do not. The falsification
gradient tracks AFFF-specificity of the source list — itself evidence.

**5.3 County population density
(`results_popdensity_tests.csv`, `popdensity_tests.R`).**
Log county population density (Census POPESTIMATE2023 / county land area,
spatially joined; 25,903 of 26,260 locations matched) added to the
covariates. Density enters at β≈0.17 with p between 1e-13 and 1e-25 in
*every* model, and:

| Result | before density | with density |
|---|---|---|
| industry → PFOA (equal or tier) | 0.067 (1e-16) | 0.001 (.90) — dies |
| milsusp → PFOS | 0.42 (.03) | 0.10 (.55) — dies |
| milsusp → PFHxS | 0.55 (.014) | 0.12 (.45) — dies |
| milbase / milfire (all) | mixed | p=.5–.8 — die |
| airport → PFOS | 0.086 (.03) | −0.005 (.91) — dies |
| **airport → PFHxS** | 0.17 (9e-6) | **0.13 (.002); 0.12 (.009) with placebo+industry too** |

Interpretation requires care in both directions. County density is coarse
and is itself a *real* aggregate PFAS source (wastewater, landfills,
consumer products), so it plausibly over-controls — especially for milsusp,
whose 91 sites sit mostly in populous counties, leaving little identifying
variation after a county-level control. But the conservative reading is
unavoidable: **only airport→PFHxS survives**, and it survives everything
simultaneously.

**5.4 What the gauntlet leaves standing**

| Claim | placebo | + industry | + density | verdict |
|---|---|---|---|---|
| airport → PFHxS | pass | pass | **pass** | **causal-grade** |
| industry → PFOA | **pass (clean)** | — | fail | real association; ≤ estimate, > 0 unproven |
| milsusp → PFOS/PFHxS | pass | pass | fail | same status; design can't separate from county urbanization |
| milbase, milfire → anything | fail | fail | fail | descriptive only |
| industry/airport → PFOS | split/fail | fail | fail | descriptive only |
| PFBS negatives (old) | — | — | — | exposed as selection artifact; PFBS has no source signal |

Supporting coherence evidence that a pure density story does not explain:
the compound fingerprint (PFHxS↔AFFF sources only in bucket A; PFOA↔industry
with radius-increasing reach; milsusp ≫ other military sets; PFBS correctly
null), the monotone dose–response, and the PFOA placebo's clean kill.

---

## 6. How big are the surviving effects?

**Airport → PFHxS (density-robust):** β = 0.12–0.21 per unit of 10 km
airport IDW ≈ **9–12% higher latent PFHxS per SD of airport exposure**, and
roughly a doubling of detection odds from the bottom to the top of the
exposure distribution. For a compound with a 3 ng/L reporting limit sitting
just under the (10 ppt-equivalent) hazard-index compounds, this is
policy-relevant magnitude.

**If the milsusp and industrial-PFOA associations are causal** (unproven),
they would be of similar per-SD size (8–17%). These are the numbers that a
damage/cost calculation would eventually price — but they should not be
priced until §7's designs resolve the density tie.

---

## 7. Next steps (in order of value)

1. **Part 139 vs general-aviation placebo for the airport→PFHxS result.**
   Certificated (Part 139) airports were federally required to maintain
   AFFF; GA airports were not; both span the density distribution. PFHxS
   loading on certificated but not GA airports would close the causal case.
   (FAA airport file is public; NPIAS/5010 data has the certification flag.)
2. **Tract-level density + within-county contrasts** for milsusp and
   industrial-PFOA. County-level density is too blunt an instrument to
   adjudicate them; tract density removes the "coarse over-control"
   objection in both directions. A within-county specification retains
   ample exposure variation (unlike the degenerate ZCTA FE).
3. **Re-run the gauntlet in bucket A** (density's channel is the detection
   margin, which bucket A lacks) as a triangulation, acknowledging its
   selection bias runs the other way.
4. **EWG methodology confirmation** for the milsusp layer (is "suspected"
   purely activity-based?) plus their materials-request for publication.
5. Then, and only then: population-served-weighted damage translation.

---

## 8. Data appendix: sources, construction, and concerns

### 8.1 Outcomes — UCMR5 (complete release)

- **Source:** EPA UCMR5 occurrence data, January 2026 release
  (`ucmr5-occurrence-data.zip` from epa.gov/dwucmr), 1,928,116 analytical
  results — ~95% of the final expected total; EPA continues minor updates
  through 2026. Files: `PFAS_Project_Data/ucmr5_full/`.
- **Prior defect fixed:** the previously used `UCMR5_All.csv` had been
  opened in Excel and silently truncated at exactly 1,048,575 rows (the
  Excel limit) — non-randomly, since the file is sorted. All current results
  use the complete tab-delimited file read directly in R.
- **Unit of analysis:** sampling location = PWSID × FacilityID (26,475
  tested for PFAS; 10,297 systems). Per location × compound: `det` = any
  sample detection; `conc` = mean of detected sample values; censoring point
  = official per-compound MRL from the file's MRL column.
- **Covariates:** UCMR5 Additional Data Elements (disinfectant type,
  treatment, PFAS treatment, self-reported occurrence/sources), dcast to
  ~40 system-level dummies.
- **Buckets:** A = locations with ≥1 PFAS detection (6,366; 6,314 after
  geocoding); B = all tested locations (26,260 after geocoding). Bucket A
  exists only to expose/inspect the historical selection.

### 8.2 Water-system coordinates

- **Source:** EPA ECHO `ECHO_EXPORTER.csv` (FAC_LAT/FAC_LONG via SDWA_IDS),
  one coordinate per PWSID; 99.2% of locations matched.
- **Concern:** every sampling location inherits its system's single
  coordinate — within-system geography is invisible (motivating the PWSID
  clustering), and the coordinate is an administrative/facility point, not
  necessarily the raw-water intake. Attenuates all distance-based effects;
  worst for surface-water systems. Upgrade path: EPA Community Water System
  Service Area Boundaries.

### 8.3 Exposure sources

- **Industrial:** EPA FRS national files (facility, NAICS, environmental
  interest), 39 presumptive-PFAS NAICS codes (Salvatore et al. 2022 +
  EPA effluent-guidelines programs), filtered to geocoded facilities with
  ACCURACY_VALUE ≤ 1000 m and a valid START_DATE → 21,479 facilities.
  *Concerns:* NAICS ≠ actual PFAS use (misclassification attenuates);
  START_DATE requirement drops facilities of unknown vintage; no emissions
  quantities (TRI PFAS reporting is post-2020 and sparse).
- **Airports:** the FRS NAICS 488119 subset (774 facilities). *Concern:*
  this is "airport operations" businesses, an imperfect census of airports;
  the Part 139 upgrade (§7.1) replaces it with an authoritative list.
- **Military fire/crash training sites (305):** DoD's FY2014 KBCRS
  inventory (FOIA-released PDF), parsed to installations
  (`geocode_fire_training_sites.py`), geocoded 98% — primarily via EWG's
  2019 map GeoJSON of the same list, MIRTA fallback, alias table for joint
  bases. *Concerns:* installation centroids, not burn-pit coordinates (on
  large bases the error is km-scale — likely a factor in milfire's weak
  falsification performance); EWG coordinates are themselves approximate;
  7 sites ungeocoded (6 non-CONUS + Gentile AFS).
- **Suspected-AFFF sites (91):** EWG 2020/2025 map layer
  (`suspected_sites_2025AUG01.geojson`). *Concerns:* n=91; EWG's
  "suspected" criterion needs confirmation as activity-based (if it is
  partly informed by nearby detections the exposure is endogenous);
  EWG materials-request required before publication use.
- **Military installations (737):** DoD MIRTA point file. *Concern:*
  centroids; includes closed sites (desirable for legacy exposure).
- **Population density:** Census county population estimates
  (co-est2024-alldata.csv, POPESTIMATE2023) over county land area (2023 cb
  county boundaries), point-in-polygon joined; 25,903/26,260 matched.
  *Concern:* county scale is coarse — the central caveat of §5.3.

### 8.4 Known analytical limitations

1. **Cross-sectional design.** No temporal variation; "causal-grade" here
   means "survived falsification and confounder controls," not experimental
   identification.
2. **Multiple testing.** Thousands of models were run across this program.
   The paper's claims rest on pre-stated falsification logic and pattern
   coherence, not on any single p-value; a Benjamini–Hochberg pass within
   the headline table is planned for the final write-up.
3. **Tobit normality.** All key results were cross-checked against
   detection-margin LPMs (assumption-light); agreement is uniform.
4. **Mean-of-detects concentration.** Location concentrations average only
   detected samples; a location detecting once in six samples looks like its
   one detection. Alternatives (max; event-level models) untested.
5. **UCMR5 completeness.** ~5% of results were outstanding at the January
   2026 release; small systems (<3,300) are a stratified sample, not a
   census, so bucket B is representative but not exhaustive of small-system
   America.
6. **EWG-derived coordinates** are approximate by their own documentation;
   all military exposure indices inherit this.

---

*Scripts: `UCMR5_full_rebuild.R` (data), `full_tobit_clustered.R` (baseline
grids), `placebo_industrial.R` / `placebo_military.R` (placebos),
`popdensity_tests.R` (density), `industrial_deepdive_pfoa.R` (offshoots),
`diagnose_pfhxs.R` (censoring diagnosis), `naics_tiers.R` (shared IDW
machinery and NAICS tiers).*
