# Known Issues & Sampling Options — Working Notes

> **STATUS UPDATE (2026-07-16):** items 1.1 (clustering), 1.2 (official
> MRLs), 1.8 (any-detect selection), and 1.9 (Excel truncation) are now
> ADDRESSED by the full-UCMR5 rebuild and clustered re-estimation:
> `UCMR5_full_rebuild.R` + `full_tobit_clustered.R`, results in
> `results_military_full_clustered.csv` / `results_industrial_full_clustered.csv`
> (buckets: A_anydetect = old design, B_all = all tested locations).
> Sample grew from 2,032 locations / 1,060 systems to 26,260 / 10,151.
> Still open: 1.3 (Tobit alternatives), 1.4 (multiple testing),
> 1.5 (location error), 1.6 (urbanization confound), 1.7 (bookkeeping).
>
> Headline outcomes of the re-run: (i) the PFBS "robust negative" was a
> selection artifact exactly as 1.8 predicted — it flips to significantly
> POSITIVE in bucket B (airport: -0.063** in A vs +0.098*** in B);
> (ii) bucket B strengthens nearly every exposure-compound pair and kills
> nearly all wrong-signed results; (iii) BUT in bucket B every compound
> responds to every source family, which is the signature of a shared
> urbanization/density confound acting through the detection margin —
> issue 1.6 is now the single most important open item before any causal
> claim from bucket B. Differentiation still exists (milsusp/milfire
> dominate PFOS/PFHxS; PFOA strongest for industry), which argues against
> pure confounding, but bucket-B levels should not be quoted until 1.6
> is addressed.

Status notes for the censoring-aware analysis (`model_outputs_military/`,
`model_outputs_censored/`). Nothing here invalidates the headline findings —
the AFFF fingerprint pattern (PFOS everywhere, PFHxS military-only, PFOA
regional, PFBS absent) is consistent across margins and specifications — but
each item below either needs fixing before the paper or is a design choice
that should be made deliberately rather than by default.

---

## Part 1 — Potential errors / things to fix

### 1.1 Standard errors are not clustered (HIGHEST PRIORITY)

The analysis dataset has **2,032 rows but only 1,060 unique water systems**
(`PWSID`). Rows are sampling points / facilities within systems, and
observations within a system share source water, treatment, and location.
Every regression so far treats them as independent, which overstates
precision.

- **Fix:** cluster SEs by `PWSID`. For `lm()` use
  `sandwich::vcovCL(fit, cluster = ~PWSID)` + `lmtest::coeftest`. For the
  Tobit (`survreg`), use `robust = TRUE` with `+ cluster(PWSID)` in the
  formula, or aggregate to one row per system first (see 2.3).
- **Expected impact:** with ~2 rows/system, p-values weaken but the strong
  results (p < 1e-6) will survive. Borderline results (p ≈ 0.01–0.05) may
  not — re-check PFOA and the marginal PFHpA/PFHxA findings after fixing.

### 1.2 Reporting limits are inferred, not official

The censored models set each compound's MRL to the **minimum detected value**
in the sample (e.g., PFHxS 3 ng/L, PFOS 4 ng/L). These match the published
UCMR5 MRLs, but the correct approach is to take the official MRL per
compound from the UCMR5 documentation (they are uniform national values in
UCMR5, so this is a constants table, not a data problem).

- **Fix:** hard-code the UCMR5 MRL table in `military_afff_margins.R` /
  `industrial_margins.R` instead of `min(conc, na.rm = TRUE)`.
- **Risk if unfixed:** trivial for well-detected compounds; matters only if a
  compound's sample minimum happens to exceed its true MRL.

### 1.3 Tobit distributional assumption

The Tobit margin assumes conditional log-normality, including below the
reporting limit. The detection LPM (assumption-light) agrees with the Tobit
everywhere important, which is the main defense. Additional cheap robustness:

- Substitution OLS at MRL/sqrt(2) on all rows (the environmental-lit
  convention; two-line addition).
- A logit version of the detection margin (LPM and logit should agree).

### 1.4 Multiple testing

~2,900 models have been run across the two grids. The defense is not any
single p-value but the *pattern*: compound-by-source-by-distance signatures
that match known chemistry (documented in the run logs and chat notes).
For the paper: designate the Tobit + covariates spec at 10 km as primary,
report everything else as robustness, and consider Romano-Wolf or
Benjamini-Hochberg adjustment within the primary table.

### 1.5 Location measurement error (both sides of the distance)

- **Water systems** are geocoded to ONE ECHO facility coordinate per PWSID
  (`ECHO_EXPORTER.csv` FAC_LAT/FAC_LONG — a real administrative/facility
  point, not a ZIP centroid as earlier drafts of these notes claimed).
  Still: every sampling point in a system shares that single coordinate,
  and the actual intake can be elsewhere — for surface-water systems the
  intake is what matters.
- **Military installations** are centroids; on a large base the burn pit can
  be 10+ km from the centroid. This is why military effects strengthen at
  wider IDW decay (25 km) — the wide kernel absorbs geocoding error. Fine to
  note; not fixable without installation-level site data.
- **EWG-derived coordinates** (fire-training sites, suspected sites) are
  approximate by EWG's own documentation ("determined using DoD addresses").

### 1.6 Confounding not yet addressed: urbanization
### (UPDATE 2026-07-16: the PFOA deep dive makes this concrete — see
### results_pfoa_deepdive_*.csv. In a joint tobit, EQUAL-weight industrial
### density beats the tier-weighted index (tier: -0.04 n.s.; equal: +0.09,
### p=2e-5), tier-3 "junk" sectors (wholesale, printing) predict PFOA as
### strongly as tier-1 sectors, and every sector loads near-identically on
### PFOA and PFOS. That is the signature of generic facility/population
### density, not PFAS-specific industrial discharge. The industrial results
### should be treated as UNIDENTIFIED pending a real density control or a
### non-PFAS-industry placebo IDW. The military results are not implicated —
### they passed the analogous specificity tests.)
###
### (PLACEBO RESULT, same day — results_industrial_placebo.csv: an IDW from
### 11,867 food/wood/beverage/furniture facilities [same FRS filters and
### construction]. PFOA PASSES decisively: jointly, the real PFAS-industry
### index keeps its full coefficient (0.066, p=3e-8) while the placebo dies
### (0.002, p=0.89) despite corr=0.79 between the indices. PFOS FAILS: real
### and placebo split the signal ~50/50 (0.044 vs 0.040, both sig).
### Interpretation: industrial PFOA is genuinely PFAS-industry-specific;
### industrial PFOS is substantially generic density — consistent with the
### chemistry (PFOA = manufacturing chemical; PFOS = AFFF, whose causal
### source is the military grid). Quote industrial effects for PFOA;
### treat industrial PFOS as descriptive only.)
###
### (MILITARY PLACEBO, same day — results_military_placebo.csv and
### results_military_placebo_bysource.csv; joint tobits vs placebo and vs
### placebo+industry, bucket B, 10 km, clustered:
###   milsusp  PASSES: PFOS 0.42->0.41 (p=.03-.04), PFHxS 0.55->0.54
###            (p=.014-.019); coefficients barely move, placebo dies for
###            PFHxS. The core AFFF result stands.
###   airport  PFHxS PASSES strongly (0.15, p=4e-4 in the triple race,
###            placebo+industry both die); PFOS survives placebo alone
###            (p=.03) but not the triple (p=.21).
###   milbase  PARTIAL: survives placebo (p~.03-.04) but marginal in the
###            triple (p~.06); per-SD the placebo outguns it for PFOS.
###   milfire  FAILS in bucket B joint races (PFOS p=.18/.28) — its
###            bucket-B standalone significance is substantially density.
### Pattern: the MOST AFFF-specific source lists survive falsification,
### the generic installation lists do not — quote milsusp (and
### airport-PFHxS); treat milbase/milfire bucket-B levels as descriptive.)
###
### (POPULATION-DENSITY CONTROL, same day — results_popdensity_tests.csv:
### adding log county population density (Census POPESTIMATE2023 / ALAND,
### spatially joined) to the 10 km races, bucket B, clustered tobit.
### Density is overwhelming (beta ~0.17, p ~ 1e-13..1e-25 in every model)
### and it ABSORBS almost everything:
###   industrial PFOA  0.067*** -> 0.001 (p=.90)   DIES (tierweighted too)
###   milsusp PFOS     0.42**   -> 0.10  (p=.55)   DIES
###   milsusp PFHxS    0.55**   -> 0.12  (p=.45)   DIES
###   milfire/milbase                              DIE
###   airport PFHxS    0.17     -> 0.13 (p=.002 alone; 0.12, p=.009 in the
###                    full triple+density race)   *** SURVIVES — the only one
###   airport PFOS                                 dies
### Interpretation cautions BOTH ways: (i) county density is coarse and
### metro counties genuinely contain diffuse PFAS sources (WWTPs, landfills,
### consumer wastewater), so this may partly OVER-control — the truth for
### milsusp/industrial-PFOA likely lies between the placebo-passing estimate
### and zero; (ii) but no causal claim except airport->PFHxS survives the
### conservative test, and the paper must say so. Next steps that could
### separate signal from density: tract-level (not county) density; a
### within-county design (county FE leaves ample within-county exposure
### variation, unlike the pathological ZCTA FE); re-running the races in
### bucket A where the detection margin is absent; and the Part 139 vs
### general-aviation placebo to harden the surviving airport-PFHxS result.)

The covariate battery is water-treatment variables only. Airports, industry,
and population co-locate; military bases less so (which is itself a useful
contrast). Before the paper: add population served (in UCMR5), county/CBSA
population density, and consider the Part 139 vs general-aviation placebo
design for the airport exposure.

### 1.7 Smaller bookkeeping items

- The 2.5 km cross-source dedup threshold in combined location sets is
  arbitrary; check 1 km / 5 km sensitivity (one constant in
  `military_afff_margins.R`).
- Per-SD standardized coefficients are computed on the fly
  (`compare_persd.R` in scratchpad), not stored; move into the repo and save
  a standardized results table.
- EWG data: email their materials-request form before publication; cite as
  the geocoding source for the 2019 crash-sites layer and 2025 suspected
  layer regardless.
- 7 fire-training installations remain ungeocoded (6 non-CONUS + Gentile AFS,
  Kettering OH — hand-code if desired).

### 1.8 The analysis sample is ALREADY conditioned on any-PFAS detection
### (discovered 2026-07; changes interpretation of everything)

Verified directly: all 2,032 analysis rows have >=1 PFAS detection; the
construction filter (`clean_data_construction.R` line ~82) dropped every
all-non-detect location. In the raw UCMR5 file on disk, only **3,274 of
15,663** tested locations (21%) have any PFAS detection — so ~12,400
all-ND locations were excluded before any modeling.

Consequences:
- Every model (including the Tobit/detect margins) is estimated on the
  "at least one compound detected" subsample — option 2.1(c) below was
  already, unknowingly, the design.
- **Effects are likely UNDERSTATED**: the excluded all-ND locations are
  disproportionately low-exposure, and cutting them removes the cleanest
  control observations. Positive findings are conservative.
- **Possible explanation for the robust PFBS negatives**: conditioning on
  any-detection is a collider. Among locations that detected *something*,
  those near military/industry detected PFOS et al.; distant locations
  that still entered the sample got in by detecting something else
  (often PFBS, the most-detected compound). That can manufacture a
  negative exposure-PFBS correlation that does not exist in the full
  population. Re-test PFBS after adding the all-ND locations before
  interpreting its negative sign as real.
- **Fix**: rebuild with all ~15,663 tested locations (subject to
  geocoding), non-detects censored at MRL. This both removes the
  selection and roughly septuples the sample.

### 1.9 The raw UCMR5_All.csv on disk is TRUNCATED by Excel

`PFAS_Project_Data/ucmr5/UCMR5_All.csv` contains exactly **1,048,575 data
rows — the Excel row limit** (2^20 - 1 + header). The file was opened/saved
through Excel at some point and silently lost every row beyond the limit.
The full UCMR5 (data through 2025) is roughly 3x that size. Everything
downstream inherits this truncation, and it is not random — UCMR5 files are
sorted, so entire states/regions/late collection dates may be missing.

- **Fix**: re-download UCMR5 occurrence data from EPA
  (https://www.epa.gov/dwucmr/occurrence-data-unregulated-contaminant-
  monitoring-rule) and never open the CSV in Excel (use R/fread directly).
  Combined with 1.8, the honest ceiling is ~5,800+ systems / ~15,000+
  locations from the file on disk, and more from the complete download.

---

## Part 2 — Sampling options (deliberate design choices)

### 2.1 Which observations enter the estimation sample?

Three candidate samples, with different interpretations:

**(a) All sampling locations (CURRENT, and the right default).**
All 2,032 observations; non-detects enter as censored. This is the only
sample that identifies "does exposure raise PFAS?" without conditioning on
an outcome. Keep as primary.

**(b) Detects-only per compound (the ORIGINAL spec — now known to be biased).**
Kept in the results tables as the `intensive` margin purely to document the
censoring artifact. Do not use for inference.

**(c) Systems where AT LEAST ONE PFAS compound was detected.**
A middle ground worth adding: condition on "PFAS is present at all"
(any-compound detection), then model individual compounds within that
subsample (still with censoring handled for the specific compound).

- *What it buys:* removes systems that plausibly have no PFAS pathway at all
  (deep confined aquifers, pristine watersheds). Within the remaining
  "contaminated or contaminable" systems, the question becomes *which
  sources shape the compound mix* — a cleaner fingerprinting design, less
  driven by the extensive margin of any-contamination.
- *What it costs:* it IS selection on an outcome — any-detection correlates
  with total exposure, so coefficients within the subsample will be
  attenuated relative to (a), and the subsample over-represents exposed
  systems. Never interpret (c) coefficients as total exposure effects.
- *How to use it honestly:* report (c) as a companion to (a). If a source
  predicts PFHxS specifically *among already-contaminated systems*, that is
  strong fingerprint evidence independent of the any-detection margin.
  A natural extension is a two-part / hurdle model: Part 1 = any-PFAS
  detection ~ exposure (all systems); Part 2 = compound composition ~ source
  type (contaminated systems).

### 2.2 Expand the sample (likely the highest-value data work)

Current sample: 1,060 systems, from three restrictive filters in
`clean_data_construction.R`:

1. **Rolling UCMR5 releases** — data downloaded mid-project; EPA kept
   publishing through 2025-26. Re-pull the final UCMR5 file.
2. **ECHO-coordinate geocoding** — systems whose PWSID has no ECHO
   FAC_LAT/FAC_LONG are dropped. (Correction to earlier drafts: coordinates
   come from ECHO facility points, not ZIP centroids.) In the full-UCMR5
   rebuild this turned out to be a minor constraint: 99.2% of tested
   locations geocoded. Upgrade path remains EPA's Community Water System
   Service Area Boundaries (polygons).
3. **Any-detect filter** — see 1.8; the dominant sample restriction was
   dropping all-non-detect locations, not geocoding.

Realistic ceiling: UCMR5 covers ~7,000+ systems, so the sample could roughly
triple. Biggest payoff for the low-detection compounds (PFHpA 12%,
PFBA 29%) where the censored models need the most data.

### 2.3 Unit-of-analysis options

- **Aggregate to one row per PWSID** (max or mean concentration across
  sampling points, detect = any point detected): sidesteps clustering
  entirely, cleanest inference, small information loss.
- **Stay at sampling-point level with clustered SEs** (1.1): uses all data,
  standard practice.
- Either is defensible; do 2.3(a) as a robustness check on whichever is
  primary.

### 2.4 Weighting options

- **Population-served weights**: current models weight a 500-person system
  the same as a 500,000-person one. Unweighted = "effect on the average
  system" (fine for the science); population-weighted = "effect on the
  average person" (better for the policy framing). Report primary unweighted,
  weighted as robustness.

---

## Suggested order of operations (when there is time)

1. Cluster SEs by PWSID (or aggregate per 2.3) and re-run both margin grids
   — an afternoon, and it firms up everything already found.
2. Hard-code official UCMR5 MRLs — minutes.
3. Add the any-detection subsample (2.1c) as a companion spec — an afternoon.
4. Add urbanization controls + the Part 139 vs GA airport placebo — a few days.
5. Re-pull final UCMR5 + service-area-boundary geocoding to triple the
   sample — the big lift, likely worth it before submission.
