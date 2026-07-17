# Simple (No-Fixed-Effects) Models — Plain-English Readout

**Why this file exists.** The fixed-effects (ZCTA) models absorb ~100% of the
exposure variation, so an FE "significant" result can be a statistical artifact
built on ~10% of the sample. These **simple models strip all of that out**. Each
exposure is run two ways, with nothing that can soak up the variation:

- **`simple`** — `log(PFAS) ~ exposure` and *nothing else*. The raw relationship.
- **`plus_cov`** — adds the water-treatment covariates only (still **no** ZCTA FE).

**Sign convention.** Distance measures should be **negative** (farther = less
PFAS). IDW / buffer / count measures should be **positive** (more exposure = more
PFAS). A "wrong sign" that is statistically significant is the *proximity
paradox* — water nearer to industry testing *cleaner*, usually because of
treatment investment or because rural/remote systems are both far from industry
and have other contamination.

Full numbers: `model_outputs/results_simple_models.csv`.

---

## The one-sentence takeaway

**Only the airport/AFFF exposure survives once the fixed effects are removed.**
It predicts higher PFOS strongly and robustly with no FE at all. Every broad
"industrial proximity" measure is either null or *paradoxically wrong-signed* in
the raw data — which means the earlier tier-weighted (Spec D) and equal-weight
results were largely fixed-effects artifacts, exactly the concern that motivated
this check.

---

## Exposure-by-exposure

### 1. Distance to nearest facility (`nearest_distance_m`) — expect NEGATIVE
- **PFOS:** β = +5.9e-8, **p = 0.017** — significant but the **WRONG sign**
  (systems *farther* from a facility test *higher* PFOS). This is the proximity
  paradox, not support.
- Everyone else: null, or PFHpA wrong-signed-significant.
- **Verdict:** does **not** support the hypothesis; the raw distance relationship
  runs backwards for PFOS.

### 2. Distance to nearest PRE-2002 facility (`nearest_distance_pre2002_m`) — expect NEGATIVE
- Essentially identical to #1 (the legacy facilities dominate the "nearest"
  calculation). PFOS again wrong-signed and significant (p = 0.017).
- **Verdict:** same as #1 — no support.

### 3. IDW exposure, all 39 NAICS codes equal-weight (`idw_exposure_10km`) — expect POSITIVE
- **PFOS:** β = +0.007, **p = 0.44** — null. The headline "Model 7" result in the
  report **disappears** without fixed effects.
- **PFBS** (p = 1e-5) and **PFHxS** (p = 0.007) are significant but **negative**
  (wrong sign).
- **Verdict:** the equal-weight industrial-proximity measure has **no real raw
  signal** for PFOS, and contradicts for the short-chains.

### 4. IDW exposure, pre-2002 facilities (`idw_pre2002_exposure_10km`) — expect POSITIVE
- **PFOS:** β = +0.013, **p = 0.15** — null (its FE significance does not survive).
- PFBS again wrong-signed significant.
- **Verdict:** no real support.

### 5. IDW exposure, NAICS tier-weighted — "Spec D" (`idw_tierweighted_10km`) — expect POSITIVE
- **PFOS:** simple **p = 0.15**, plus_cov **p = 0.21** — **null both ways.**
- **This is the important correction:** Spec D looked like the best model *only*
  under ZCTA fixed effects (p = 0.013). With nothing absorbing the variation it is
  not significant. So **the tier-weighting "win" was a fixed-effects artifact** —
  your instinct was right to distrust it.
- **Verdict:** no support in the raw data.

### 6. IDW exposure, airports / AFFF only (`idw_airport_10km`) — expect POSITIVE ✅
- **PFOS:** simple β = +0.146, **p = 4.7e-6 \*\*\***; plus_cov β = +0.085,
  **p = 0.021 \*\***. Strong, correct sign, and **robust to having no fixed
  effects** — it does not need the FE crutch.
- **PFBA:** p = 7e-13 / 1e-4 (very strong); **PFPeA:** p = 0.023 / 0.047. Several
  PFAS compounds line up with airport proximity.
- **PFOA:** positive but n.s. (p = 0.65).
- **Verdict:** the **only genuinely supported exposure**. AFFF/airports → higher
  PFOS (and several short-chains) is a real relationship, not an artifact.

### 7. Within 5 km of a facility (`within_5km`) — expect POSITIVE
- **PFOS:** β = −0.069, p = 0.13 — null and pointing the wrong way.
- PFBA positive-significant (p = 0.03); PFHxS/PFBS negative-significant (paradox).
- **Verdict:** no support for PFOS.

### 8. Within 10 km of a facility (`within_10km`) — expect POSITIVE
- **PFOS:** β = −0.111, **p = 0.057** — borderline and **negative** (within 10 km =
  *lower* PFOS). Proximity paradox again.
- Most compounds wrong-signed; PFBA the lone positive.
- **Verdict:** contradicts more than it supports.

---

## What to do with this

1. **Lead with the airport/AFFF → PFOS finding.** It is significant with no FE
   (p ≈ 5e-6 raw, 0.02 with covariates) and earlier was strongest under *state*
   fixed effects (p = 0.006). It is consistent across every honest specification —
   that is your real, defensible result.
2. **Demote the equal-weight and tier-weighted IDW proximity claims.** They are
   null without fixed effects; their FE significance is a thin-slice artifact.
3. **Treat the wrong-signed distance/buffer results as the confounding story**
   (treatment investment + rural/remote contamination), not as evidence against
   PFAS proximity per se.
4. **PFOA:** genuinely no proximity signal in this sample, by any measure.

Reproduce: `Rscript simple_models.R` (reads the cache; runs in seconds).
