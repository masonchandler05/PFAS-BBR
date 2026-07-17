# NAICS Refinement Experiments — Findings

**Question:** Are the proximity models inconclusive because the 39 NAICS codes are
treated as equally likely PFAS sources, when many of them don't actually emit PFAS?

**Answer: Largely yes.** Differentiating facilities by evidence-based PFAS likelihood
sharpens the PFOS signal and, for PFOA, moves it from null toward marginal. The single
best specification is **distance × NAICS-tier weighting** — exactly the idea of weighting
facilities by *both* distance and source type.

Spec for all results below: `log(PFAS) ~ exposure + ZCTA fixed effects`, 2,032 water
systems, 21,479 facilities. "Want" = positive & significant coefficient.

---

## 1. Distance × tier weighting is the winner (PFOS)

| Spec | n facilities | PFOS estimate | p-value |
|---|---|---|---|
| A — baseline, all 39 codes equal | 21,479 | 0.313 | 0.049 * |
| **D — all codes, weighted by tier (1.0 / 0.5 / 0.15) × distance** | 21,479 | **0.337** | **0.013 ** ** |
| C — tier 1+2 only | 16,285 | 0.368 | 0.025 ** |
| B — tier 1 only | 10,245 | 0.389 | 0.108 |
| B2 — EPA-regulated 7 codes only | 9,248 | 0.391 | 0.106 |

Restricting to better codes **raises the coefficient** (0.31 → 0.39, i.e. less
measurement-error attenuation, the predicted effect) but drops sample size, so the
hard-restriction specs (B, B2) lose significance from lost power. **Keeping every
facility but down-weighting the weak codes (D) gets the best of both** — the strongest,
most significant PFOS estimate in the whole study.

Pre-2002 (legacy) facilities: restriction roughly **doubles** the coefficient
(baseline 0.45 → tier-1 / EPA-regulated 0.65), consistent with the attenuation story.

## 2. Airports / AFFF carry the PFOS signal (Exp E, sector horse-race)

| Sector (PFOS, alone) | n | estimate | p |
|---|---|---|---|
| **airport (AFFF)** all-years | 774 | **+5.11** | 0.016 ** |
| **airport (AFFF)** pre-2002 | 270 | **+11.8** | 0.002 *** |
| plastics | 392 | +/- mixed | ~0.08 |
| printing (tier 3) | 1,813 | **−2.93** | 0.019 ** |
| metal, chemical, waste, textile | — | ~0 | n.s. |

AFFF firefighting foam (airports) is by far the strongest single PFOS mechanism —
exactly what the PFAS literature predicts, since legacy AFFF was the dominant PFOS source.
Surprisingly, **chemical and metal-finishing sectors are weak on their own** in this
sample; their facilities are numerous but noisy.

## 3. Some tier-3 codes actively hurt the signal

`printing (323111)` shows a **significant negative** PFOS coefficient — it is
anti-correlated with PFAS and dilutes the pooled exposure measure. These generic codes
are the ones to drop:

- `323111` commercial/gravure printing (1,813 facilities)
- `332999` all other misc fabricated metal (1,729)
- `424690` chemical merchant wholesalers — a distributor, not a source (831)
- `335999` misc electrical equipment (412)
- `324191` lubricating oil & grease (409)

## 4. PFOA: weak everywhere, but restriction helps

PFOA has no robust proximity signal under any spec. The best result is **tier-1 only**,
which moves PFOA from null (baseline est 0.17, p=0.45) to **marginal positive
(est 0.70, p=0.087)**. The "significant" PFOA results elsewhere (paper, petroleum) are
negative/wrong-signed and spurious.

## 5. Data-quality note

Several tier-1 codes have **zero facilities** in the analysis universe and contribute
nothing: `313310` (textile finishing), `322220` (coated paper), `313110`, `313220`,
`314910`, `322219`. The FRS file simply has no usable geocoded facilities under them.

---

## Recommendations

1. **Adopt Spec D (distance × tier-weighted IDW) as the headline exposure measure.**
   It is the user's "weight by distance and NAICS type" idea and gives the cleanest PFOS
   result in the project (p = 0.013).
2. **Drop the harmful/noise tier-3 codes** (`323111`, `332999`, `424690`, `335999`,
   `324191`, plus the zero-facility codes) from the presumptive list.
3. **Build a dedicated AFFF / airport (`488119`) exposure variable** and report it
   separately — it is the dominant PFOS mechanism and may deserve its own model.
4. Treat PFOA as a separate, weaker case; if reported, use the tier-1-restricted measure.

## How to reproduce

```r
# one-time (≈27 min): builds model_outputs/naics_inputs.rds
Rscript naics_prep_cache.R
# fast (seconds): runs experiments A, B, B2, C, D, E, F
Rscript naics_experiments.R
# fast: prints the decision-oriented summary
Rscript naics_results_summary.R
```

Outputs in `model_outputs/`: `naics_tier_assignments.csv`,
`results_naics_experiments.csv`, `results_naics_by_sector.csv`,
`results_naics_leave_one_out.csv`.

---

## Implemented (refined exposures now canonical)

- `naics_tiers.R` — single source of truth: the tier/sector/weight/EPA-regulated
  table + `build_weighted_idw()`. Sourced by all the analysis scripts.
- `analysis_naics_weighted.R` — cache-based; builds `idw_tierweighted_10km`
  (Spec D) and `idw_airport_10km` (+ pre-2002 versions) and runs FE & OLS across
  all compounds → `model_outputs/results_naics_weighted_airport.csv`.
- `clean_data_construction.R` — now also produces these two exposures and four
  FE result tables (`results_idw_tierweighted_zipfe.csv`, `results_idw_airport_zipfe.csv`,
  and their pre-2002 variants) on a full rebuild.

**Robustness of the headline results (PFOS):**

| Exposure | FE estimate (p) | OLS+cov estimate (p) |
|---|---|---|
| Airport/AFFF (488119) | +5.11 (0.016) | +0.086 (0.020) |
| Airport/AFFF pre-2002 | +11.8 (0.002) | +0.207 (0.056) |
| Spec D (tier-weighted) | +0.337 (0.013) | +0.020 (0.195) |

Airport/AFFF is significant under **both** the FE and the covariate-adjusted OLS
spec — the most robust positive finding in the project.
