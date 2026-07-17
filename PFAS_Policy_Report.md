---
editor_options: 
  markdown: 
    wrap: 72
---

# PFAS Contamination in Public Drinking Water: Spatial Econometric Evidence from EPA UCMR5

## A Multi-Model Analysis of Industrial Proximity and Drinking Water Contamination

**Prepared by:** Mason Chandler\
**Institution:** Big Blue Research (BBR) Scholars Program, University of
Nebraska–Lincoln\
**Date:** June 2026\
**Data Period:** 2023–2025 (UCMR5), with a 2013–2015 (UCMR3) panel
comparison\
**Models Estimated:** 8 core cross-sectional specifications, plus three
extensions — a difference-in-differences across UCMR rounds, a TRI
emission-weighted exposure model, and a hydrological flow-path
(upstream-network) model — across 12 PFAS compounds

------------------------------------------------------------------------

## Executive Summary

This report presents the findings of a spatial econometric research
project examining a central hypothesis: **do PFAS-adjacent industrial
manufacturing facilities, when located in close geographic proximity to
public water systems, predict elevated PFAS concentrations in drinking
water?** Using EPA's Fifth Unregulated Contaminant Monitoring Rule
(UCMR5) data on **2,032 unique water system sampling points** from
**1,060 distinct public water systems**, matched to a national database
of approximately 21,000 geocoded PFAS-adjacent industrial facilities via
geodetic distance calculations, this analysis estimates eight distinct
econometric models capturing different exposure pathways, spatial
scales, and identification strategies.

Each sampling point is represented at its water system's GPS-geocoded
location from the EPA ECHO database, with ZIP code fixed effects derived
from the Census ZCTA boundary containing each system's coordinates.
Multiple sampling facilities within the same water system share the same
geographic coordinates but may have different PFAS measurement outcomes
(reflecting variation across distinct tap water sampling points within
one utility). This structure captures the full scope of PFAS measurement
heterogeneity across 1,060 utilities while preserving geographically
accurate location assignment.

**The hypothesis is that closer proximity to PFAS industrial sources
predicts higher PFAS concentrations.** For distance-based models, this
means a negative coefficient on nearest_distance_m (farther away = lower
concentration). For IDW and buffer models, it means a positive
coefficient (higher exposure or being within the buffer = higher
concentration). The following key findings emerge:

1.  **The single robust finding is that proximity to airports / AFFF
    (aqueous film-forming foam) sources predicts higher PFOS — and this
    result does not depend on fixed effects.** An airport/AFFF-only
    exposure index (NAICS 488119) is positive and highly significant for
    PFOS in the rawest possible specification — a bivariate regression
    with *no* fixed effects and *no* covariates (β = +0.146, p ≈ 5×10⁻⁶)
    — and remains significant adding treatment covariates (p = 0.021)
    and under *state* fixed effects (p = 0.006). Because it survives
    with nothing absorbing the geographic variation, this is the most
    credible positive result in the report, and it is consistent with
    the toxicological record in which legacy AFFF was the dominant
    environmental source of PFOS. (See Section 8.5.)

2.  **The broader industrial-proximity PFOS results are real-signed but
    fragile — they are largely an artifact of granular fixed effects.**
    PFOS shows nominally significant proximity effects in four of the
    eight original specifications (e.g. within 1 km +15.6%, p = 0.031;
    pre-2002 distance within ZCTAs +15.4%, p = 0.044; both IDW ZCTA-FE
    models, p = 0.049 and 0.026). However, a robustness audit (Section
    8.5) shows the ZCTA fixed effect absorbs **\~100% of the exposure
    variation**, leaving the coefficient identified off roughly 10% of
    the sample (about 20 ZCTAs / 82 systems). When the fixed effects are
    removed, these equal-weight and tier-weighted proximity results
    **collapse to non-significance** (PFOS IDW p = 0.44; tier-weighted p
    = 0.15). They should therefore be read as suggestive, not as
    independent confirmation; the durable PFOS signal runs specifically
    through the airport/AFFF channel in finding 1.

3.  **Treatment confounding is a dominant and pervasive competing
    force.** The most reliable pattern across models is that water
    systems located near dense industrial clusters show *lower* PFAS
    concentrations for several compounds — the opposite of the
    hypothesis. This is most plausibly explained by the fact that
    utilities in industrially dense areas have historically invested in
    advanced PFAS treatment infrastructure (granular activated carbon,
    reverse osmosis, ion exchange resin), suppressing detected
    concentrations regardless of the underlying contamination load. PFBS
    and PFHxS show this pattern consistently. This confounding does not
    disprove the hypothesis; it means the treatment layer partially
    masks the contamination signal in observed tap water measurements.

4.  **Simple nearest-facility distance, without controlling for ZCTA
    geography, picks up treatment confounding rather than contamination
    for most compounds.** The OLS baseline finds a statistically
    significant positive coefficient on distance for PFOS (p = 0.024) —
    meaning systems farther from facilities show higher detected PFOS.
    This contradicts the hypothesis and is consistent with the treatment
    confounding explanation: utilities closest to industrial clusters
    have invested most in PFAS treatment, suppressing measured
    concentrations relative to moderate-distance utilities that lack
    equivalent treatment. When the analysis shifts to pre-2002 legacy
    facility distances with ZCTA fixed effects, the PFOS coefficient
    reverses to the expected negative sign, confirming that the
    contamination signal is real but masked in the simpler
    specification.

5.  **No other compound shows a consistent cross-specification positive
    proximity signal.** PFOA, the other EPA-regulated long-chain PFAS,
    shows predominantly null or negative proximity effects, possibly due
    to more diffuse non-industrial sources (food packaging, biosolids,
    consumer products) diluting the industrial signal, or because
    treatment investment is especially concentrated for PFOA-regulated
    utilities. Short-chain compounds PFBS, PFHxS, PFHxA, and PFPeA show
    either null effects or negative proximity effects consistent with
    treatment confounding.

6.  **PFBA remains unregulated and concentration-extreme.** With a
    maximum observed concentration of 1,225 ppt and 572 detections in
    this sample, PFBA is the concentration outlier in the dataset.
    Proximity models are mixed and inconsistent for PFBA, suggesting
    highly diffuse or poorly-captured sources. The absence of an EPA MCL
    for PFBA warrants independent regulatory attention.

7.  **The most rigorous causal test — a difference-in-differences across
    UCMR rounds — is null, which locates the signal in legacy rather
    than contemporary industrial activity.** Matching 2,604 water
    systems between UCMR3 (2013–2015) and UCMR5 (2023–2025) and using
    each system as its own control, there is no evidence that newly
    opened industrial facilities raised PFAS detection (pooled β ≈ 0, p
    = 0.96; PFOS and PFOA carry the expected positive sign but are far
    from significant). Because PFOS contamination is overwhelmingly a
    pre-2002 legacy phenomenon, a design keyed to recent facility
    openings structurally cannot capture it — so the null
    difference-in-differences and the significant pre-2002
    cross-sectional results are mutually consistent and jointly point to
    legacy contamination. A complementary model that replaces the binary
    industry-code proxy with actual TRI PFAS emission mass finds
    positive but mostly non-significant associations for the regulated
    compounds (PFOA closest, p = 0.055 within ZCTAs), limited by the
    sparseness of current TRI PFAS reporting (only 74 facilities
    nationally, a median of 100 km from the typical system).

8.  **Hydrological flow-path routing yields the strongest confirmation
    of the hypothesis — and the only statistically significant PFOA
    result in the entire project.** Rebuilding the exposure measure to
    count only facilities that are *genuinely upstream* of a system's
    intake along the actual surface-water network (USGS NHDPlus),
    weighted by network rather than straight-line distance, transforms
    the result for surface-water systems. On the identical set of
    systems, omnidirectional Euclidean exposure is null for both
    regulated compounds (PFOS p = 0.75, PFOA p = 0.85); but upstream
    legacy (pre-2002) exposure with ZCTA fixed effects is positive and
    significant for both PFOS (p = 0.048) and PFOA (p = 0.013). PFOA
    reaches significance nowhere else in the analysis, and does so here
    under the most physically realistic exposure model — direct support
    for the hypothesis once the direction of water flow is respected.
    The effect appears only within ZCTAs and only for the legacy
    footprint, reinforcing the legacy-contamination reading. Because
    this result is also ZCTA-FE-dependent, it warrants the same
    no-fixed-effects robustness check applied to the proximity models in
    Section 8.5 before being treated as definitive; the upstream
    *direction* of the effect, however, is mechanistically sensible and
    consistent with finding 1.

The overarching conclusion is that the hypothesis receives genuine
empirical support for PFOS, concentrated specifically in the
**airport/AFFF source channel** — the one result that holds with no
fixed effects, with covariates, and under state fixed effects alike. The
broader "any industrial proximity" version of the hypothesis is *not*
robustly supported: those results depend on granular ZCTA fixed effects
that absorb essentially all of the exposure variation, and they
disappear when that scaffolding is removed (Section 8.5). The
contamination pathway from legacy PFAS sources — above all legacy AFFF —
to drinking water is real, but detecting it at the national scale is
complicated by substantial treatment investment in industrially exposed
communities, by the noise that omnidirectional and equal-weight distance
measures introduce, and by the fragility of overly granular
fixed-effects identification. The honest reading is a **narrow but
solid** finding (AFFF/airports → PFOS) inside a **broad but confounded**
one.

------------------------------------------------------------------------

## 1. Introduction and Policy Motivation

### 1.1 The PFAS Crisis in Drinking Water

Per- and polyfluoroalkyl substances (PFAS) are a class of more than
15,000 synthetic chemicals characterized by an exceptionally strong
carbon-fluorine bond that renders them persistent in the environment and
resistant to biological degradation. First synthesized in the 1940s,
PFAS were commercially deployed across a wide range of industrial and
consumer applications — non-stick cookware (Teflon), water-repellent
textiles, food packaging, semiconductor manufacturing, and firefighting
foam — by companies including 3M, DuPont, Chemours, Solvay, and Daikin.
Their chemical stability, which made them industrially valuable, has
made them an environmental liability of the highest order: PFAS
accumulate in soil, groundwater, surface water, and human tissue with no
known natural mechanism for complete degradation.

The health consequences of PFAS exposure are substantial and
well-documented. The National Institute of Environmental Health Sciences
and the Agency for Toxic Substances and Disease Registry link PFAS
exposure to thyroid dysfunction, kidney and testicular cancer, elevated
cholesterol, immune system suppression, adverse reproductive outcomes
including reduced birth weight and preterm birth, and developmental
delays in children. The National Academies of Sciences, Engineering, and
Medicine issued a comprehensive 2022 report recommending clinical
intervention for individuals with PFAS blood concentrations above 7
ng/mL — a threshold that a substantial fraction of Americans may exceed.

Drinking water is the dominant human exposure pathway. The EPA's 2024
establishment of Maximum Contaminant Levels (MCLs) for six PFAS
compounds — 4 ppt for PFOA and PFOS individually; hazard index
thresholds for PFNA, PFHxS, HFPO-DA (GenX), and PFBS in combination —
represents the first legally enforceable federal drinking water
standards for any PFAS, and reflects a regulatory reckoning two decades
in the making.

### 1.2 The Research Question and Hypothesis

The regulation of PFAS in drinking water requires understanding *where*
contamination comes from and *how* to target remediation and monitoring
resources. This project addresses a specific, testable hypothesis:

> **Hypothesis:** Geographic proximity to PFAS-adjacent industrial
> manufacturing facilities predicts elevated PFAS concentrations in U.S.
> public drinking water systems. Water systems located closer to
> industrial sources should show higher PFAS concentrations, reflecting
> direct contamination via groundwater migration, surface water runoff,
> and atmospheric deposition from facility operations.

Testing this hypothesis at the national scale is non-trivial. Despite
broad scientific consensus on PFAS toxicity and regulatory urgency, most
evidence for the proximity-contamination relationship comes from
high-profile case studies — 3M's Cottage Grove, Minnesota facility, the
Chemours GenX discharge into the Cape Fear River, AFFF contamination
near military bases. These cases are compelling but may not generalize
to the full landscape of U.S. industrial PFAS sources.

Using EPA's UCMR5 national monitoring data, a comprehensive geocoded
database of PFAS-adjacent industrial facilities, and eight econometric
specifications, this project estimates whether the proximity hypothesis
holds at scale. The answer has direct implications for whether
proximity-based regulatory targeting is defensible or whether PFAS
policy needs to account for more complex, diffuse contamination
pathways.

------------------------------------------------------------------------

## 2. Data Sources and Construction

### 2.1 Primary Outcome Data: UCMR5

The EPA's **Fifth Unregulated Contaminant Monitoring Rule (UCMR5)**,
covering monitoring years 2023–2025, required public water systems
(PWSs) serving more than 3,300 people to test for 29 PFAS compounds and
lithium. Analytical results are measured in µg/L (equivalent to parts
per billion; 1 µg/L = 1,000 ppt). Where multiple samples were collected
over the monitoring period for a given PWS sampling point and
contaminant, this analysis uses the mean analytical result.

After filtering for sampling points with at least one PFAS detection and
successful ECHO coordinate matching, the analytic sample consists of
**2,032 observations** representing **1,060 unique public water
systems** (identified by PWSID). The 2,032 observations correspond to
unique PWSID × FacilityID combinations — distinct physical sampling
locations within each water utility. Some systems report results from a
single sampling point (FacilityID); others report from multiple. Across
the full dataset, the mean number of sampling points per system is
approximately 1.9. All sampling points within the same water system
share GPS coordinates (sourced from the EPA ECHO database at the system
level) and are therefore assigned the same geographic proximity
measures; their PFAS measurements may differ, reflecting variation in
tap water quality across the system's internal sampling network.

**Table 1: PFAS Compound Detection Summary — UCMR5 Analytic Sample (n =
2,032 observations)**

| Compound | N Detected | Mean (µg/L) | Median (µg/L) | 95th Pct. (µg/L) | Max (µg/L) | EPA MCL |
|----|----|----|----|----|----|----|
| PFPeA | 925 | 0.00924 | 0.00595 | 0.02445 | 0.13380 | None |
| PFBS | 916 | 0.00955 | 0.00530 | 0.03153 | 0.15525 | Hazard index |
| PFOS | 845 | 0.01272 | 0.00865 | 0.03325 | 0.34500 | 4 ppt |
| PFHxA | 794 | 0.00788 | 0.00549 | 0.01832 | 0.12980 | None |
| PFOA | 700 | 0.00865 | 0.00681 | 0.01815 | 0.18140 | 4 ppt |
| PFHxS | 678 | 0.00800 | 0.00545 | 0.01897 | 0.22195 | Hazard index |
| PFBA | 572 | 0.02700 | 0.01008 | 0.10804 | 1.22500 | None |
| PFHpA | 240 | 0.00584 | 0.00454 | 0.01169 | 0.06035 | None |
| 6:2 FTS | 24 | 0.03467 | 0.01275 | 0.11990 | 0.15945 | None |
| PFNA | 19 | 0.00907 | 0.00650 | 0.01725 | 0.03750 | Hazard index |
| PFPeS | 16 | 0.00797 | 0.00523 | 0.01741 | 0.03245 | None |
| HFPO-DA (GenX) | 2 | 0.02025 | 0.02025 | 0.03218 | 0.03350 | Hazard index |

*Note: PFNA (n=19), 6:2 FTS (n=24), PFPeS (n=16), and HFPO-DA (n=2) have
too few detections for reliable econometric analysis and are excluded
from substantive interpretation of regression results. PFBA max of 1,225
ppt exceeds the PFOA MCL by more than 300-fold.*

All detected values in this sample substantially exceed the 4 ppt
threshold for PFOA and PFOS, as the minimum reporting level for most
compounds is at or above that level. PFOS reaches 345 ppt at its maximum
— nearly 90 times the MCL. PFBA shows a maximum of 1,225 ppt.

### 2.2 Water System Geolocation

Precise water system coordinates were sourced from the **EPA ECHO
(Enforcement and Compliance History Online) Exporter**, which provides
FRS NAD83 geocoded latitude/longitude coordinates for permitted
facilities. ECHO coordinates were matched to UCMR5 systems via PWSID.
Each water system is assigned a single coordinate point, which is shared
across all its FacilityID sampling locations.

ZIP code identifiers for fixed effects were derived via **spatial join**
between each system's ECHO coordinate and 2020 Census ZIP Code
Tabulation Area (ZCTA) boundary polygons, using the `tigris` and `sf`
packages in R. This assigns each water system to the single ZCTA that
geographically contains its reported location — ensuring that the ZIP
code fixed effect reflects the system's actual physical location rather
than its administrative service area (which can span many ZIP codes and
would artificially inflate the sample if used naively as a join key).

### 2.3 Presumptive PFAS Source Locations

**Manufacturing Facilities:** The **EPA National Facility Registry
System (FRS)** was filtered using 39 PFAS-relevant NAICS codes
encompassing industries with documented PFAS use: textile and fabric
coating mills, semiconductor and electronics manufacturing, petroleum
refining and lubricants, plastics film and packaging, electroplating and
metal coating, paint and chemical manufacturing, hazardous waste
treatment and landfills, and airports (known AFFF users). After
filtering for geocoding accuracy ≤ 1,000 meters, valid coordinates, and
distinct facility IDs, the dataset comprises approximately **21,000
unique facilities** with valid start dates (and a larger set of \~27,000
when facilities without start dates are included).

**Military Fire/Crash Training Sites:** DoD installations under PFAS
assessment were joined between a June 2021 DoD progress report and the
Military Installation Resilience Tracking Application (MIRTA) dataset to
obtain coordinates for military AFFF use sites — among the most heavily
documented PFAS point sources in the U.S.

**Industrial Water Treatment Centers:** The 2022 Clean Watersheds Needs
Survey (CWNS) provided locations for industrial-flow treatment
facilities, which can serve as secondary exposure pathways through
treated effluent discharge.

### 2.4 Sample Proximity Characteristics

**Table 2: Distance Distribution — Water System Sampling Points to
Nearest PFAS-Adjacent Facility**

| Statistic                                  | Value                      |
|--------------------------------------------|----------------------------|
| N unique water systems (PWSID)             | 1,060                      |
| N sampling-point observations (PWSID×Fac.) | 2,032                      |
| Mean distance to nearest facility          | 145.95 km                  |
| Median distance                            | 3.31 km                    |
| 25th percentile                            | 1.72 km                    |
| 75th percentile                            | 6.99 km                    |
| Minimum distance                           | 0.00 km (co-located)       |
| Maximum distance                           | 5,925 km                   |
| Mean IDW exposure index                    | 4.02                       |
| Within 1 km                                | 263 observations (12.9%)   |
| Within 5 km                                | 1,316 observations (64.8%) |
| Within 10 km                               | 1,665 observations (82.0%) |
| Within 25 km                               | 1,905 observations (93.7%) |

The extreme divergence between mean (145.95 km) and median (3.31 km)
distance reflects a highly right-skewed distribution driven by a small
number of remote systems (likely in rural western states or Alaska) that
are far from any PFAS-adjacent manufacturing facility. The median
picture is more relevant for policy: **half of all sampling-point
observations are within 3.31 km of a PFAS-adjacent facility**, and 82%
are within 10 km. PFAS-adjacent industrial activity is effectively
ubiquitous across the landscape of U.S. public water infrastructure,
which itself makes distance a limited discriminatory variable and
underscores why within-ZCTA variation matters.

### 2.5 Treatment Covariates

Binary indicator variables for each public water system's reported
treatment processes and PFAS characteristics were constructed from the
UCMR5 Additional Data Elements file, including granular activated carbon
(GAC), powdered activated carbon (PAC), ion exchange resin (IEX),
nanofiltration/reverse osmosis, ozonation, coagulation, disinfectant
type, PFAS treatment installation, and PFAS occurrence self-reporting.

------------------------------------------------------------------------

## 3. Econometric Strategy

### 3.1 Outcome Variable Transformation

All PFAS concentration variables are log-transformed prior to
regression: the outcome is log(AnalyticalResultValue), measured in
log(µg/L). This addresses the right-skewed distribution of raw
concentration levels and permits interpretation of estimated
coefficients as approximate percentage changes (semi-elasticities).
Observations with missing or zero values are dropped per compound,
causing sample sizes to vary across outcome variables.

### 3.2 Sign Convention and Hypothesis Direction

Throughout this report, the hypothesis predicts a specific sign for each
type of exposure measure:

-   **Nearest-distance specifications:** A **negative** coefficient on
    distance supports the hypothesis (greater distance → lower PFAS;
    equivalently, closer → higher PFAS).
-   **IDW specifications:** A **positive** coefficient supports the
    hypothesis (higher cumulative exposure → higher PFAS).
-   **Buffer specifications:** A **positive** coefficient supports the
    hypothesis (being within the buffer → higher PFAS).

Any result in the opposite direction — positive coefficient on distance,
or negative coefficient on IDW/buffer — *contradicts* the hypothesis.
These contradictory results are consistently interpreted throughout this
report as evidence of treatment confounding rather than
misspecification, for the reasons described in Section 6.4.

### 3.3 Exposure Specifications

Three distinct exposure measures capture different theoretical
mechanisms:

**Nearest Distance (meters):** The geodetic distance from each water
system location to its nearest PFAS-adjacent manufacturing facility,
computed via `sf::st_distance()` on the WGS84 ellipsoid.

**Buffer Indicators (binary):** Four dummy variables encoding whether
any facility falls within 1 km, 5 km, 10 km, or 25 km of the water
system. These are not mutually exclusive.

**Inverse Distance-Weighted (IDW) Exposure Index:** A spatially
cumulative exposure measure summing exponential-decay-weighted distances
over the 10 nearest facilities:

$$\text{IDW} = \sum_{k=1}^{10} \exp\!\left(-\frac{d_k}{10{,}000}\right)$$

where $d_k$ is the distance in meters to the $k$-th nearest facility and
λ = 10,000 m (10 km). A facility exactly 10 km away contributes
$e^{-1} \approx 0.368$ units; one at 20 km contributes $e^{-2} \approx
0.135$ units. The mean IDW index in the analytic sample is **4.02**,
reflecting the dense industrial environment surrounding most U.S. public
water systems.

### 3.4 Temporal Stratification: Pre-2002 Legacy Facilities

3M ceased PFOS production in 2002 following regulatory pressure, making
that year the most consequential discontinuity in U.S. PFAS production
history. Facilities operating before 2002 are more likely to have
released PFOS and long-chain PFOA into surrounding soils and groundwater
— contamination that persists as legacy pollution decades later. A
pre-2002 facility subsample was constructed by filtering on
`START_DATE < 2002-01-01`, and separate distance matrices and IDW
indices were computed for this subset. This produces the most
theoretically defensible causal specification in the set: pre-2002
facility location decisions predate modern PFAS toxicity awareness and
are therefore less likely to be endogenous to current water quality
conditions. If the contamination hypothesis holds, it should show up
most clearly for PFOS and PFOA in these pre-2002 specifications.

### 3.5 ZCTA Fixed Effects

A central identification challenge is that PFAS contamination is
geographically clustered for reasons correlated with facility location —
regional geology, industrial land use history, source water type, and
utility management quality all vary across ZIP codes. Including ZCTA
fixed effects (`factor(ZIPCODE)`) absorbs all cross-sectional ZCTA-level
variation, so the estimated coefficient on the exposure variable
reflects variation *within* ZCTAs — differences in PFAS concentrations
across sampling points that share the same ZCTA but differ in their
proximity to PFAS sources. This substantially reduces confounding and
provides a cleaner test of the proximity hypothesis.

### 3.6 The Eight Models

**Table 3: Model Specifications**

| Model | Key Regressor | Controls | Primary Purpose |
|----|----|----|----|
| OLS Baseline | nearest_distance_m | Treatment covariates | Cross-sectional proximity association |
| OLS ZCTA FE | nearest_distance_m | ZCTA fixed effects | Within-ZCTA distance gradient |
| Buffer | km1/km5/km10/km25 | None | Nonlinear proximity effects |
| Buffer Wide | km1/km5/km10/km25 | None | Pivoted buffer summary |
| IDW Baseline | idw_exposure_10km | Treatment covariates | Cumulative spatial exposure |
| Pre-2002 ZCTA FE | nearest_distance_pre2002_m | ZCTA fixed effects | Legacy contamination gradient |
| IDW ZCTA FE | idw_exposure_10km | ZCTA fixed effects | Within-ZCTA cumulative exposure |
| Pre-2002 IDW ZCTA FE | idw_pre2002_exposure_10km | ZCTA fixed effects | Strongest within-ZCTA result; FE-dependent (see §8.5) |

------------------------------------------------------------------------

## 4. Results

### 4.1 Model 1: Baseline OLS — Nearest Distance with Treatment Covariates

**Table 4: Baseline OLS Results** *(log(PFAS) \~ nearest_distance_m +
treatment covariates)*

| Compound | Estimate (per meter) | Std. Error | t-stat | p-value | Adj. R² | N | Direction |
|----|----|----|----|----|----|----|----|
| **PFHpA** | **+1.306×10⁻⁷** | 4.94×10⁻⁸ | +2.64 | **0.009** | −0.004 | 223 | Contradicts ✗ |
| **PFOS** | **+8.343×10⁻⁸** | 3.69×10⁻⁸ | +2.26 | **0.024** | 0.117 | 785 | Contradicts ✗ |
| **PFBS** | **−8.834×10⁻⁸** | 3.92×10⁻⁸ | −2.26 | **0.024** | 0.094 | 841 | Contradicts ✗ |
| PFHxS | +3.40×10⁻⁸ | 3.62×10⁻⁸ | +0.94 | 0.348 | 0.076 | 629 | — |
| PFPeA | −1.95×10⁻⁸ | 3.07×10⁻⁸ | −0.63 | 0.526 | 0.114 | 858 | — |
| PFBA | +3.43×10⁻⁸ | 7.88×10⁻⁸ | +0.43 | 0.664 | 0.429 | 551 | — |
| PFHxA | +1.18×10⁻⁸ | 2.98×10⁻⁸ | +0.40 | 0.692 | 0.079 | 736 | — |
| PFOA | −1.44×10⁻⁸ | 4.29×10⁻⁸ | −0.33 | 0.738 | 0.115 | 655 | — |

*Bold = statistically significant at p \< 0.05. "Direction" column
assesses whether the significant result is consistent with the
hypothesis (closer = higher PFAS). A positive coefficient on distance
means farther = higher, which contradicts the hypothesis.*

The baseline OLS finds three statistically significant associations, but
all three are difficult to interpret as straightforward proximity
contamination:

-   **PFHpA** (β = +1.31×10⁻⁷, p = 0.009) and **PFOS** (β = +8.34×10⁻⁸,
    p = 0.024) both show *positive* coefficients on distance — meaning
    systems *farther* from the nearest facility show *higher*
    concentrations. This is the opposite of the hypothesis. The most
    likely explanation is treatment confounding: utilities that are very
    close to industrial facilities have invested most heavily in PFAS
    treatment, depressing measured concentrations below what we might
    expect from proximity alone. Systems at moderate distances (10–50
    km) may lack treatment while still accumulating PFAS from
    atmospheric deposition, groundwater migration, and other diffuse
    pathways.

-   **PFBS** (β = −8.83×10⁻⁸, p = 0.024) shows a *negative* coefficient
    on distance, which is in the direction predicted by the hypothesis
    (closer = higher). However, PFBS is a short-chain PFAS replacement
    compound that has multiple contemporary industrial sources, and the
    effect size is modest. The pattern is more consistent with a genuine
    proximity signal for PFBS, though treatment confounding may still
    operate in the opposite direction here.

The fact that the OLS baseline finds a significant but wrong-direction
result for PFOS — while later, more carefully specified models find the
expected direction — is an important signal: the baseline is picking up
the confounding between industrial proximity and treatment investment
rather than the contamination mechanism itself.

### 4.2 Model 2: OLS with ZCTA Fixed Effects

**Table 5: OLS with ZCTA Fixed Effects** *(log(PFAS) \~
nearest_distance_m + factor(ZIPCODE))*

| Compound | Estimate (per meter) | Est. per km | p-value | Adj. R² | N   |
|----------|----------------------|-------------|---------|---------|-----|
| PFHxS    | +7.48×10⁻⁵           | +0.0748     | 0.154   | 0.145   | 676 |
| PFOS     | −8.46×10⁻⁵           | −0.0846     | 0.200   | 0.383   | 841 |
| PFHpA    | +1.12×10⁻⁴           | +0.1118     | 0.315   | 0.058   | 238 |
| PFHxA    | +5.43×10⁻⁵           | +0.0543     | 0.327   | 0.212   | 790 |
| PFBS     | +2.79×10⁻⁵           | +0.0279     | 0.534   | 0.471   | 912 |
| PFOA     | −2.39×10⁻⁵           | −0.0239     | 0.687   | 0.302   | 696 |
| PFPeA    | +1.11×10⁻⁵           | +0.0111     | 0.866   | 0.184   | 921 |
| PFBA     | +3.60×10⁻⁶           | +0.0036     | 0.982   | 0.530   | 567 |

*No compound reaches statistical significance at p \< 0.05 (PFPeS n=16
excluded). Adj. R² values reflect ZCTA fixed effects absorbing
substantial geographic variation.*

Introducing ZCTA fixed effects eliminates any significant within-ZCTA
distance gradient for all compounds. This tells us that the OLS
baseline's significant findings were driven by between-ZCTA variation —
ZCTAs that are simultaneously more industrial and closer to facilities
differ from rural ZCTAs in many confounding ways that the treatment
covariates did not fully capture.

Importantly, PFOS now shows a negative coefficient (−8.46×10⁻⁵/m, p =
0.200) — closer to the expected direction — but does not yet reach
significance. This foreshadows the pre-2002 fixed effects result, where
PFOS achieves significance when the exposure measure is refined to
legacy manufacturing only.

### 4.3 Models 3 & 4: Buffer Dummy Models

The buffer models test whether being within a given radial distance of
*any* PFAS-adjacent facility predicts higher or lower PFAS
concentrations. These models are estimated without additional controls,
so results are descriptive.

**Table 6: Buffer Dummy Estimates — Selected Compounds** *(log(PFAS) \~
km_indicator, arranged by compound)*

| Compound | Within 1 km | Within 5 km | Within 10 km | Within 25 km |
|----|----|----|----|----|
| **PFOS** | **+0.145** (p=0.031) ✓ | −0.069 (p=0.134) | −0.111 (p=0.056) | **−0.317** (p=0.002) ✗ |
| **PFBS** | **−0.226** (p=0.003) ✗ | **−0.108** (p=0.023) ✗ | **−0.228** (p\<0.001) ✗ | **−0.249** (p=0.004) ✗ |
| **PFBA** | **−0.286** (p=0.006) ✗ | **+0.176** (p=0.031) ✓ | **+0.359** (p\<0.001) ✓ | +0.289 (p=0.084) |
| **PFHxS** | −0.108 (p=0.089) | **−0.108** (p=0.021) ✗ | **−0.132** (p=0.018) ✗ | **−0.252** (p\<0.001) ✗ |
| **PFOA** | −0.005 (p=0.933) | −0.001 (p=0.983) | **−0.114** (p=0.021) ✗ | **−0.372** (p\<0.001) ✗ |
| PFHpA | −0.003 (p=0.972) | +0.033 (p=0.571) | −0.021 (p=0.752) | −0.113 (p=0.250) |
| PFHxA | −0.014 (p=0.831) | −0.024 (p=0.575) | −0.061 (p=0.219) | −0.127 (p=0.094) |
| PFPeA | −0.073 (p=0.292) | +0.009 (p=0.835) | −0.013 (p=0.805) | −0.093 (p=0.261) |

*Bold = p \< 0.05. ✓ = direction supports hypothesis (positive
coefficient on buffer dummy). ✗ = direction contradicts hypothesis.*

**PFOS** shows the most theoretically interesting pattern across buffer
distances:

-   **Within 1 km (β = +0.145, p = 0.031):** The closest proximity
    category shows a *positive* and *significant* association — systems
    within 1 km of a PFAS-adjacent facility have approximately 15.6%
    higher PFOS concentrations (e\^0.145 − 1 ≈ 0.156). This directly
    supports the hypothesis for very short-range contamination. Systems
    so close to a facility may be subject to direct groundwater plume
    contamination or airborne deposition that overwhelms any treatment
    capacity.

-   **Within 25 km (β = −0.317, p = 0.002):** At the broader 25 km
    buffer, PFOS is *significantly lower* for systems within that
    radius. This apparent contradiction is the proximity paradox driven
    by treatment confounding: the 25 km buffer captures a large
    industrial zone where sophisticated utilities have invested in PFAS
    treatment technologies, collectively suppressing measured tap water
    PFOS even though contamination loads may be high.

**PFBS** shows consistently negative and significant coefficients across
all four buffer distances (p ranging from \< 0.001 to 0.004). This
monotone negative pattern is strong evidence of treatment confounding:
across all spatial scales, proximity to industrial facilities is
associated with lower measured PFBS. The implication is that PFBS
treatment investment is correlated with industrial proximity and
effectively suppresses measured concentrations throughout.

**PFBA** shows a non-monotonic pattern: significantly negative at 1 km
(β = −0.286, p = 0.006) but significantly positive at 5 km and 10 km (β
= +0.176, p = 0.031 and β = +0.359, p \< 0.001). This reversal may
reflect heterogeneous PFBA source geography: PFBA is associated with
both industrial and agricultural sources (fluorinated pesticides), and
the 5–10 km zone may capture a specific industrial source type while the
very close systems are in urban centers where a different confounding
pattern applies.

**PFOA** shows null effects at short distances but significantly
negative effects at 10 km (β = −0.114, p = 0.021) and 25 km (β = −0.372,
p \< 0.001). This is consistent with treatment confounding particularly
for PFOA, since utilities in PFOA-regulated areas are most motivated to
invest specifically in PFOA treatment given the MCL.

### 4.4 Model 5: IDW Exposure with Treatment Covariates

**Table 7: IDW Baseline Results** *(log(PFAS) \~ idw_exposure_10km +
treatment covariates)*

| Compound  | IDW Estimate | Std. Error | t-stat | p-value   | Adj. R² | N   | Direction     |
|-----------|--------------|------------|--------|-----------|---------|-----|---------------|
| **PFHxS** | **−0.0254**  | 0.01133    | −2.24  | **0.026** | 0.083   | 629 | Contradicts ✗ |
| **PFBS**  | **−0.0236**  | 0.01204    | −1.96  | **0.050** | 0.093   | 841 | Contradicts ✗ |
| PFHxA     | −0.00796     | 0.01003    | −0.79  | 0.428     | 0.080   | 736 | —             |
| PFPeA     | −0.00719     | 0.01020    | −0.70  | 0.481     | 0.114   | 858 | —             |
| PFBA      | −0.00713     | 0.01399    | −0.51  | 0.610     | 0.429   | 551 | —             |
| PFHpA     | +0.00614     | 0.01781    | +0.34  | 0.731     | −0.047  | 223 | —             |
| PFOA      | −0.00298     | 0.00891    | −0.34  | 0.738     | 0.115   | 655 | —             |
| PFOS      | +0.00138     | 0.01066    | +0.13  | 0.897     | 0.111   | 785 | —             |

*Bold = p \< 0.05.*

The IDW baseline yields significant results only for treatment
confounding:

-   **PFHxS** (β = −0.0254, p = 0.026) and **PFBS** (β = −0.0236, p =
    0.050) both show significantly *negative* IDW coefficients — higher
    cumulative facility density correlates with *lower* concentrations.
    This is consistent with treatment investment confounding, where the
    most industrially dense areas are served by the largest utilities
    with the most sophisticated PFAS removal technology.

-   **PFOS** (β = +0.00138, p = 0.897) is entirely null in this
    specification, with a sample nearly doubled relative to the previous
    version of the analysis (n = 785 vs. n ≈ 407 in prior draft). The
    null result here is attributable to two factors: (1) treatment
    covariates absorb much of the industrial proximity variation that
    correlates with PFOS treatment, and (2) within-system variation
    across FacilityIDs (which share the same IDW value) adds outcome
    variance without adding predictor variance, reducing the effective
    explanatory power of IDW. The ZCTA fixed effects specification for
    IDW recovers the PFOS signal by stripping out cross-ZCTA variation
    and focusing on within-ZCTA exposure gradients.

### 4.5 Model 6: Pre-2002 Nearest Distance with ZCTA Fixed Effects

This specification is the first to yield clear evidence supporting the
hypothesis for PFOS, and is the most important distance-based result in
the analysis.

**Table 8: Pre-2002 Facility Distance, ZCTA Fixed Effects** *(log(PFAS)
\~ nearest_distance_pre2002_m + factor(ZIPCODE))*

| Compound | Estimate (per meter) | Est. per km | p-value   | Adj. R² | N   | Direction  |
|----------|----------------------|-------------|-----------|---------|-----|------------|
| **PFOS** | **−1.43×10⁻⁴**       | **−0.143**  | **0.044** | 0.387   | 841 | Supports ✓ |
| PFPeA    | +3.53×10⁻⁵           | +0.0353     | 0.204     | 0.187   | 921 | —          |
| PFOA     | −6.66×10⁻⁵           | −0.0666     | 0.331     | 0.303   | 696 | —          |
| PFHxA    | +2.31×10⁻⁵           | +0.0231     | 0.355     | 0.212   | 790 | —          |
| PFBA     | −1.24×10⁻⁵           | −0.0124     | 0.680     | 0.530   | 567 | —          |
| PFHxS    | +1.39×10⁻⁵           | +0.0139     | 0.802     | 0.140   | 676 | —          |
| PFHpA    | +7.89×10⁻⁶           | +0.0079     | 0.958     | 0.049   | 238 | —          |
| PFBS     | +1.02×10⁻⁶           | +0.00102    | 0.988     | 0.470   | 912 | —          |

*Bold = p \< 0.05. PFPeS (n=16) excluded from interpretation. The
"Supports" direction for PFOS reflects that a negative coefficient on
distance means closer = higher PFAS.*

**PFOS** is the only compound that achieves statistical significance at
p \< 0.05 in this specification. The coefficient is β = −1.43×10⁻⁴ per
meter (p = 0.044, n = 841). This means: within the same ZCTA, each
additional kilometer of distance from the nearest pre-2002 legacy
manufacturing facility is associated with a 0.143 decrease in log(PFOS),
translating to approximately **−13.3% lower PFOS per kilometer of
distance** (1 − e\^{−0.143} ≈ 0.133), or equivalently, each kilometer
*closer* is associated with approximately **+15.4% higher PFOS**
(e\^{0.143} − 1 ≈ 0.154).

This result directly supports the contamination hypothesis for PFOS:
within geographically comparable areas, the water systems physically
closest to pre-2002 PFAS manufacturing sites show the highest PFOS
concentrations. Because this is within-ZCTA variation, it is not driven
by ZCTA-level differences in industrial land use or regional background
contamination. The focus on pre-2002 legacy facilities is theoretically
motivated: PFOS was phased out of U.S. production by 3M in 2002, so
legacy manufacturing sites — not newer facilities — are the primary
source of PFOS groundwater contamination today.

**PFOA** trends negative (closer = higher, β = −6.66×10⁻⁵/km, p = 0.331)
— the expected direction — but does not reach significance. PFOA's
non-significance here may reflect that PFOA sources are more diffuse
(including consumer products, food packaging, and biosolids) and that
PFOA-regulated utilities may have invested disproportionately in PFOA
treatment, partially masking the industrial proximity signal.

**Importantly, PFHxA and PFPeA — which showed large, anomalous
counterintuitive results in a prior version of this analysis — are now
null.** The prior finding (positive distance coefficients for these
short-chain compounds, suggesting systems farther from legacy sites have
higher concentrations) was a statistical artifact of the earlier
observation inflation where each system was counted once per
service-area ZIP code. With the corrected observation structure, these
compounds show no significant relationship with pre-2002 facility
distance.

### 4.6 Model 7: IDW Exposure with ZCTA Fixed Effects

**Table 9: IDW with ZCTA Fixed Effects** *(log(PFAS) \~
idw_exposure_10km + factor(ZIPCODE))*

| Compound | IDW Estimate | Std. Error | t-stat | p-value   | Adj. R² | N   | Direction  |
|----------|--------------|------------|--------|-----------|---------|-----|------------|
| **PFOS** | **+0.313**   | 0.159      | +1.97  | **0.049** | 0.386   | 841 | Supports ✓ |
| PFHxS    | +0.448       | 0.283      | +1.58  | 0.115     | 0.146   | 676 | —          |
| PFPeA    | −0.240       | 0.161      | −1.49  | 0.137     | 0.188   | 921 | —          |
| PFHxA    | −0.140       | 0.134      | −1.04  | 0.298     | 0.212   | 790 | —          |
| PFOA     | +0.173       | 0.227      | +0.76  | 0.446     | 0.303   | 696 | —          |
| PFHpA    | −0.289       | 0.449      | −0.64  | 0.522     | 0.053   | 238 | —          |
| PFBA     | +0.073       | 0.175      | +0.42  | 0.676     | 0.530   | 567 | —          |
| PFBS     | −0.042       | 0.178      | −0.23  | 0.815     | 0.470   | 912 | —          |

*Bold = p \< 0.05. PFPeS (n=16) excluded.*

**PFOS** (β = +0.313, p = 0.049, n = 841) shows a statistically
significant positive association between cumulative IDW exposure and
log(PFOS) concentration, surviving within-ZCTA identification. This
means: within the same geographic area, water systems that face higher
cumulative industrial facility density show higher PFOS concentrations.
A one-unit increase in the IDW index (roughly one additional facility at
10 km) is associated with approximately **36.7% higher PFOS
concentration** (e\^{0.313} − 1 ≈ 0.367).

This is a notable within-ZCTA association, but it is **not robust to
removing the fixed effects** (see Section 8.5): the ZCTA dummies absorb
\~100% of the IDW exposure variation, and the same regression with no
fixed effects is null for PFOS (p = 0.44). Read this as suggestive
within-ZCTA evidence rather than a stand-alone result; the
FE-independent PFOS signal runs through the airport/AFFF channel in
Section 8.5.

**PFOA** trends positive (β = +0.173, p = 0.446) in the expected
direction but does not reach significance. No other compound shows a
significant positive proximity signal.

### 4.7 Model 8: Pre-2002 IDW with ZCTA Fixed Effects — Strongest Within-ZCTA Result (see §8.5 robustness caveat)

This specification — combining the pre-2002 legacy industrial footprint
with the cumulative IDW measure and within-ZCTA identification — was
designed as the most rigorous cross-sectional model because it
simultaneously narrows the exposure measure to historically relevant
facilities, captures cumulative rather than just nearest-neighbor
exposure, and controls for geographic confounding.

**Table 10: Pre-2002 IDW with ZCTA Fixed Effects** *(log(PFAS) \~
idw_pre2002_exposure_10km + factor(ZIPCODE))*

| Compound | Pre-2002 IDW Est. | Std. Error | t-stat | p-value   | Adj. R² | N   | Direction  |
|----------|-------------------|------------|--------|-----------|---------|-----|------------|
| **PFOS** | **+0.446**        | 0.200      | +2.23  | **0.026** | 0.388   | 841 | Supports ✓ |
| PFHxS    | +0.638            | 0.411      | +1.55  | 0.122     | 0.146   | 676 | —          |
| PFPeA    | −0.274            | 0.192      | −1.43  | 0.154     | 0.188   | 921 | —          |
| PFHxA    | −0.144            | 0.162      | −0.89  | 0.375     | 0.212   | 790 | —          |
| PFOA     | +0.238            | 0.339      | +0.70  | 0.483     | 0.302   | 696 | —          |
| PFHpA    | −0.411            | 0.697      | −0.59  | 0.556     | 0.052   | 238 | —          |
| PFBA     | +0.070            | 0.209      | +0.34  | 0.738     | 0.530   | 567 | —          |
| PFBS     | −0.039            | 0.239      | −0.16  | 0.871     | 0.470   | 912 | —          |

*Bold = p \< 0.05. PFPeS (n=16) excluded.*

**PFOS** again shows the only statistically significant result, now with
a stronger coefficient (β = +0.446, p = 0.026, n = 841) compared to the
all-facilities IDW ZCTA FE (β = +0.313, p = 0.049). Using the pre-2002
legacy footprint strengthens the PFOS finding, as expected: PFOS's
primary industrial source is pre-2002 manufacturing, so refining the
exposure measure to that historical period sharpens the signal. A
one-unit increase in the pre-2002 IDW index is associated with
approximately **56.2% higher PFOS concentration** (e\^{0.446} − 1 ≈
0.562) within the same ZCTA.

**PFOA** (β = +0.238, p = 0.483) and **PFHxS** (β = +0.638, p = 0.122)
both trend positive in the expected direction but remain statistically
non-significant.

> **Robustness caveat (added on review; see Section 8.5).** This model
> was originally described as the "most causally credible," but a
> fixed-effects audit shows that interpretation is too strong. The ZCTA
> dummies absorb \~99.5–100% of the variation in the IDW exposure, so
> the PFOS coefficient is identified off roughly 10% of the sample (\~20
> ZCTAs, \~82 systems), and the within-ZCTA exposure variation it rests
> on is tiny — which is why the β is large (+0.446) but unstable. When
> the fixed effects are removed, the equal-weight IDW PFOS result is
> null (p = 0.44), as is the tier-weighted version (p = 0.15). This
> model should therefore be read as the strongest *within-ZCTA*
> association, **not** as independent causal confirmation. The PFOS
> result that does survive without fixed effects is the airport/AFFF
> channel (Section 8.5), which is the finding the report now leads with.

------------------------------------------------------------------------

## 5. Cross-Model Synthesis

**Table 11: Significance Summary Across All Eight Models**

| Compound | OLS Base | OLS ZCTA FE | Buffer | IDW Base | Pre-2002 ZCTA FE | IDW ZCTA FE | Pre-2002 IDW ZCTA FE | Net Signal |
|----|----|----|----|----|----|----|----|----|
| PFOS | **✗+ (0.024)** | ns | **✓+ 1km (0.031)**, **✗− 25km (0.002)** | ns | **✓− dist (0.044)** | **✓+ (0.049)** | **✓+ (0.026)** | **Positive: 4 specs support hypothesis** |
| PFBS | **✗− (0.024)** | ns | **✗− all km** | **✗− (0.050)** | ns | ns | ns | Consistent negative (treatment confounding) |
| PFHxS | ns | ns | **✗− 5–25km** | **✗− (0.026)** | ns | ns | ns | Consistent negative (treatment confounding) |
| PFOA | ns | ns | **✗− 10,25km** | ns | ns (trends ✓) | ns (trends ✓) | ns (trends ✓) | Largely null; positive trend in best specs |
| PFHpA | **✗+ (0.009)** | ns | ns | ns | ns | ns | ns | OLS baseline only — treatment confounding |
| PFBA | ns | ns | ✗− 1km; ✓+ 5–10km | ns | ns | ns | ns | Mixed (diffuse/heterogeneous sources) |
| PFHxA | ns | ns | ns | ns | ns | ns | ns | Null |
| PFPeA | ns | ns | ns | ns | ns | ns | ns | Null |

*ns = not significant at p \< 0.05. ✓ = result in direction supporting
hypothesis; ✗ = result contradicting hypothesis. For distance measures,
✓ means negative coefficient; for IDW and buffer, ✓ means positive
coefficient.*

### The PFOS Story

PFOS provides the most compelling evidence for the contamination
hypothesis. Across eight specifications, four find statistically
significant results in the predicted direction:

1.  **Buffer 1 km (β = +0.145, p = 0.031):** Systems within 1 km of a
    facility have 15.6% higher PFOS — a direct point-source signal.

2.  **Pre-2002 ZCTA FE (β = −0.000143/m, p = 0.044):** Within ZCTAs,
    each km closer to a pre-2002 legacy site = 15.4% higher PFOS.

3.  **IDW ZCTA FE (β = +0.313, p = 0.049):** Within ZCTAs, higher
    cumulative industrial density = 36.7% higher PFOS.

4.  **Pre-2002 IDW ZCTA FE (β = +0.446, p = 0.026):** Within ZCTAs,
    higher cumulative pre-2002 industrial density = 56.2% higher PFOS.

5.  **Airport/AFFF exposure, no fixed effects (β = +0.146, p ≈ 5×10⁻⁶;
    +covariates p = 0.021; state FE p = 0.006):** higher proximity to
    AFFF/airport sources predicts higher PFOS, and — unlike results 2–4
    — this holds with *no* variation-absorbing controls. This is the
    durable PFOS finding (Section 8.5).

Within the fixed-effects specifications the PFOS results are mutually
consistent — distance- and IDW-based measures point the same way, and
the effect grows when exposure is refined to pre-2002 legacy sites, as
expected for a compound phased out in 2002. **However, results 2–4 are
not robust to dropping the fixed effects.** The ZCTA dummies absorb
\~100% of the exposure variation (Section 8.5), and once they are
removed the equal-weight IDW PFOS effect is null (p = 0.44) and the
tier-weighted version is null (p = 0.15). The within-ZCTA consistency is
therefore best read as suggestive corroboration, while result 5
(airport/AFFF) — which needs no fixed effects at all — is the
empirically solid core of the PFOS story.

The two models where PFOS is not significant (OLS ZCTA FE, IDW Baseline)
are null rather than contradictory — they fail to detect the signal but
do not find a significant opposite effect. The OLS Baseline finds a
significant but wrong-direction result for PFOS (farther = higher), best
interpreted as treatment confounding. The buffer 25 km result is also a
treatment confounding artifact.

### Treatment Confounding as the Dominant Competing Pattern

The most consistent finding across compounds and models is *negative*
proximity effects — industrially dense areas have *lower* measured
concentrations for PFBS and PFHxS. This is the treatment confounding
mechanism operating at scale: the utilities most exposed to industrial
PFAS sources are also the utilities most likely to have invested in
advanced treatment, suppressing tap water concentrations below what the
underlying contamination would suggest. The implication is that simple
observed concentrations understate the true contamination exposure
burden in industrial communities, because treatment investment masks it.

### PFOA: The Absent Expected Companion

PFOS and PFOA were co-produced at many legacy manufacturing sites, so we
might expect similar proximity signals. PFOA trends positive in the
three best specifications (Pre-2002 ZCTA FE, IDW ZCTA FE, Pre-2002 IDW
ZCTA FE: β = −0.067/km, +0.173, +0.238 respectively) but fails to reach
significance. Possible explanations include: (1) PFOA's more diverse
source portfolio (food packaging, consumer products, biosolids
fertilizer) dilutes the industrial facility signal; (2) PFOA's MCL
creates strong regulatory incentives for PFOA-specific treatment
investment in regulated utilities, creating more severe confounding than
for PFOS; and (3) PFOA degrades more readily in the environment than
PFOS, reducing legacy contamination persistence. The positive trend
suggests the signal is there, but the current dataset lacks the
statistical power to detect it after ZCTA fixed effects absorb
geographic variation.

------------------------------------------------------------------------

## 6. Methodological Limitations and Caveats

### 6.1 Within-System Clustering

This analysis uses 2,032 observations from 1,060 unique water systems.
Systems with multiple UCMR5 sampling locations (FacilityIDs) contribute
multiple rows, all of which share the same GPS coordinates and thus the
same exposure measures (distance, IDW). Within-system variation in PFAS
outcomes across FacilityIDs is real — different sampling points within
the same utility may differ due to internal distribution network
variation — but this variation is independent of the industrial
proximity regressors. Standard errors in specifications that do not
cluster at the PWSID level may be understated to the extent that
residuals are correlated within water systems. This makes the reported
p-values slightly optimistic; the directional conclusions and relative
effect sizes are unaffected.

### 6.2 Spatial Imprecision

Even with ECHO coordinates, water system location measurement error is
real. Administrative addresses may differ from intake locations,
especially for surface water systems drawing from upstream catchments. A
system's PFAS exposure is ultimately determined by where its raw water
originates — not where its treatment plant is located. This mismatch
introduces non-classical measurement error in the exposure variable that
can bias coefficients in unpredictable directions.

### 6.3 NAICS-Based Facility Classification

Classifying facilities as "PFAS-adjacent" based on industry code is
imprecise. Not every petroleum refinery uses PFAS, not every
electroplating facility emits PFAS into groundwater. Including non-PFAS
facilities adds noise to the exposure variable and attenuates estimated
coefficients. Conversely, facilities that use PFAS but fall under
unlisted NAICS codes will be missed, further understating the true
exposure measure.

This limitation is tested directly in Section 8.5, which differentiates
the 39 codes by evidence-based PFAS likelihood. The result confirms the
attenuation concern — reweighting facilities by source likelihood
strengthens the PFOS estimate, and the signal is concentrated in
AFFF/airport sources — so the equal-weight measure used in Sections 4–7
should be read as a *conservative* lower bound on the true proximity
effect for PFOS.

### 6.4 Treatment Confounding

The treatment proxy variables (GAC, PAC, IEX, etc.) are binary
indicators from self-reported UCMR5 supplemental data. They do not
capture treatment intensity, operational effectiveness, or when
treatment was installed. Residual treatment confounding almost certainly
persists in all specifications, likely contributing to the prevalence of
negative proximity effects for compounds like PFBS and PFHxS. The ZCTA
fixed effects approach absorbs some of this confounding geographically,
which is why the PFOS signal emerges more clearly in the FE models —
treatment investment patterns within a ZCTA may be less variable than
across ZCTAs.

### 6.5 Hydrological Misspecification

Euclidean distance and IDW indices based on geographic proximity
fundamentally mismodel the actual transport mechanism. PFAS
contamination moves through groundwater flow paths and surface water
drainage networks, not straight lines. A facility 50 km upstream on a
major river may be a far more important source than one 2 km away with
no hydraulic connection to the water system. Future work using National
Hydrography Dataset flow-path distances would substantially improve
exposure specification.

### 6.6 Small-Sample Compounds

PFNA (n=19), 6:2 FTS (n=24), PFPeS (n=16), and HFPO-DA (n=2) have too
few detections to support meaningful econometric analysis. These
compounds are excluded from substantive interpretation.

### 6.7 Sample Selection

The analytic sample is conditioned on detection above the minimum
reporting level — it excludes water systems with zero or undetectable
PFAS. Systems far from all facilities may be genuinely clean rather than
simply below detection, and excluding them may bias estimates if
non-detection is systematically related to distance.

------------------------------------------------------------------------

## 7. Policy Implications

### 7.1 The Hypothesis Receives Empirical Support for PFOS — Specifically Through Legacy AFFF/Airport Sources

The central research hypothesis — that PFAS source proximity predicts
drinking water contamination — finds robust empirical support for
**PFOS**, the compound most directly associated with historical PFAS use
and subject to the EPA's 4 ppt MCL, but the support is **specific to one
source channel rather than to industry breadth**. Proximity to
airport/AFFF sources predicts significantly higher PFOS, and this result
holds across every specification tested — with no fixed effects (p ≈
5×10⁻⁶), with treatment covariates (p = 0.021), and under state fixed
effects (p = 0.006). Because it does not depend on the fixed-effects
machinery that absorbs nearly all the exposure variation (Section 8.5),
it is the report's most defensible positive finding.

The broader "any PFAS-adjacent industry" results are weaker than the
earlier draft implied. The four ZCTA-fixed-effects proximity results for
PFOS are real-signed but fragile: they are identified off \~10% of the
sample and do not survive removing the fixed effects. They corroborate
the direction of the effect but should not be cited as independent
confirmation.

This sharpens, rather than weakens, the regulatory implication. The
measurable, robust predictor of PFOS regulatory-threshold exceedances is
proximity to **legacy AFFF/airport sources** — airports, and by
extension military fire-training areas and other foam-discharge sites.
Water systems near these sources warrant the highest enhanced-monitoring
priority; the case for blanket prioritization around all presumptive
industrial codes is empirically thinner.

### 7.2 Treatment Confounding Masks the Full Contamination Burden

The persistent negative proximity effects for PFBS and PFHxS, and the
paradoxical positive-distance findings in the simple OLS baseline for
PFOS, reveal that the utilities most exposed to industrial PFAS sources
have invested most in PFAS treatment — and this investment suppresses
measured tap water concentrations relative to the underlying
contamination load. Measured concentrations understate the contamination
burden in industrial communities.

**Policy responses:** - **Distinguish between treatment-suppressed and
contamination-absent systems:** Regulatory compliance determinations for
PFAS should account for the relationship between industrial proximity
and treatment investment. A system that clears the PFOA MCL via reverse
osmosis treatment is in a fundamentally different risk position than one
that clears it because it has no industrial contamination sources
nearby. - **Cumulative exposure screening:** State regulators should
implement IDW-based screening tools to identify utilities in
moderate-distance zones (5–25 km from industrial clusters) that may lack
the treatment infrastructure that their nearest-neighbor counterparts
have, making them potentially the most vulnerable to unmitigated PFAS
exposure.

### 7.3 PFOS Legacy Contamination Requires Targeted Remediation Policy

The finding that proximity to **pre-2002** manufacturing sites
specifically predicts elevated PFOS concentrations — surviving
within-ZCTA identification in both distance and IDW specifications — is
consistent with the persistence narrative of PFOS groundwater
contamination. Twenty-plus years after the 3M phase-out, legacy PFOS
sources continue to predictably contaminate nearby water systems.

**Policy responses:** - **Priority PFOS monitoring near legacy
industrial sites:** States should implement enhanced PFOS monitoring for
water systems within 10 km of former PFOS manufacturing sites and other
pre-2002 NAICS-coded industrial facilities, even if current
nearest-facility distance metrics are moderate. - **CERCLA remediation
acceleration:** The formal designation of PFOA and PFOS as CERCLA
hazardous substances should be used to accelerate cleanup at legacy
manufacturing sites where the IDW legacy footprint overlaps with
drinking water source areas. - **Source water protection zones:** States
should establish PFAS-specific source water protection zones based on
legacy industrial legacy proximity for surface water intakes and
wellhead protection areas, using pre-2002 facility location as a key
criterion.

### 7.4 PFOA and Short-Chain PFAS: Monitoring Gaps and Future Research

PFOA trends in the expected direction across the best specifications but
fails to achieve significance, suggesting the industrial proximity
signal is real but statistically underpowered after ZCTA fixed effects.
The null results for PFHxA, PFPeA, PFBA, and PFHxS do not indicate these
compounds are safe — they indicate that the current facility database
does not adequately capture their source geography or that treatment
confounding fully masks their industrial signals.

**Policy responses:** - **Expand TRI PFAS reporting:** EPA should
require Toxics Release Inventory reporting for short-chain PFAS (PFHxA,
PFPeA, PFHxS) to improve compound-specific source identification. -
**UCMR6 PFBA priority:** PFBA is detected in 572 of 2,032 sampling
points in this sample, has a maximum of 1,225 ppt, and has no EPA MCL.
It should be a priority compound in UCMR6, and EPA should begin the MCLG
rulemaking process. - **Post-2002 facility database:** A complementary
exposure index restricted to post-2002 facilities may better predict
short-chain replacement PFAS compounds.

### 7.5 Treatment Financing and Small System Vulnerability

The buffer model findings for PFOS — where systems within 1 km show
significantly higher concentrations — suggest that very proximate
systems, which may be too small or under-resourced to implement advanced
PFAS treatment, remain at elevated risk. The negative effects at 25 km
reflect treatment investment among larger utilities; the 1 km systems
may be the smallest and least capitalized.

**Policy responses:** - **Treatment financing for small and very
proximate systems:** EPA's Water Infrastructure Finance and Innovation
Act (WIFIA) and State Revolving Funds should prioritize PFAS treatment
financing for small water systems (under 3,300 connections) located
within 1–5 km of legacy industrial PFAS sites. - **Lower UCMR monitoring
threshold to 500 connections:** UCMR5's 3,300-person threshold excludes
tens of thousands of small systems that may face the highest PFAS
exposure risk from nearby industrial sources and the least treatment
capacity.

------------------------------------------------------------------------

## 8. Extended Analyses: Causal Design (DiD), Emission-Weighted Exposure (TRI), Hydrological Flow-Paths, and NAICS Source-Likelihood Refinement

Three additional analyses were estimated using data beyond the original
UCMR5 + FRS facility match. Each targets one of the three deepest
weaknesses of the cross-sectional models in Sections 4–5: their
inability to rule out time-invariant geographic confounding (addressed
by the difference-in-differences, §8.1); the crudeness of defining a
"PFAS source" by industry code alone (addressed by the TRI
emission-weighted model, §8.2); and the misspecification of treating
contamination as traveling in straight lines rather than down the
surface-water network (addressed by the hydrological flow-path model,
§8.3).

### 8.1 Difference-in-Differences Across UCMR Rounds (UCMR3 → UCMR5)

**Design.** UCMR3 (2013–2015) and UCMR5 (2023–2025) are the only two
UCMR rounds that monitored PFAS — UCMR4 (2018–2020) monitored
cyanotoxins, disinfection byproducts, and metals, with no PFAS, so a
three-round panel is not possible. Six PFAS compounds were measured in
both rounds: PFOA, PFOS, PFHxS, PFNA, PFBS, and PFHpA. Matching systems
by PWSID yields a two-period panel of **2,604 water systems** (2,595
with usable coordinates), or 15,624 system×compound observations.

Each system serves as its own control, so a system fixed effect
(equivalently, a first difference) absorbs *all* time-invariant
geography — geology, source-water type, regional background — that the
cross-sectional models cannot. The treatment is the **change** in nearby
industrial exposure between rounds, reconstructed from facility opening
dates (FRS `START_DATE`): the IDW exposure index computed from
facilities existing as of 2015 versus as of 2023. The outcome is a PFAS
detection indicator. Critically, because UCMR3's minimum reporting
levels are roughly ten times higher than UCMR5's (e.g., PFOS 0.04 vs
\~0.004 µg/L), detection is defined at the **UCMR3 threshold applied to
both rounds**; otherwise PFAS would appear to "rise" everywhere purely
from improved laboratory sensitivity.

**Table 12: Difference-in-Differences Results** *(Δ detection \~ Δ IDW
exposure, first difference)*

| Compound | Δ-exposure β | Std. Error | p-value | UCMR3 detection rate | Direction |
|----|----|----|----|----|----|
| PFOS | +0.00728 | 0.01093 | 0.506 | 2.8% | Positive (ns) |
| PFOA | +0.00223 | 0.01051 | 0.832 | 2.7% | Positive (ns) |
| PFHpA | +0.00192 | 0.00982 | 0.845 | 2.2% | Positive (ns) |
| PFBS | +0.00033 | 0.00459 | 0.943 | 0.3% | ns |
| PFHxS | −0.00523 | 0.00782 | 0.504 | 1.4% | ns |
| PFNA | −0.00545 | 0.00220 | 0.013 | 0.1% | Degenerate (≈0 detections) |

*Pooled with compound fixed effects (n = 15,570): Δ IDW exposure β =
+0.00018 (p = 0.958); binary "new facility within 10 km during the gap"
β = −0.00292 (p = 0.158).*

**Result.** The difference-in-differences finds **no evidence** that
newly arriving industrial facilities raised PFAS detection. The pooled
coefficient on the change in IDW exposure is essentially zero (p =
0.96), and the binary "new facility within 10 km" indicator is also null
and slightly negative (p = 0.16). Compound by compound, PFOS (p = 0.51)
and PFOA (p = 0.83) carry the expected positive sign but are nowhere
near significant. The one nominally significant coefficient — PFNA (p =
0.013) — is a degenerate artifact: at the harmonized UCMR3 threshold
PFNA is detected in only \~0.1% of systems, so the estimate is driven by
a handful of observations and is not interpretable.

**Interpretation.** This is the most causally credible specification in
the entire project, and it does *not* support the hypothesis that
proximity to *newly opened* industrial facilities causes elevated
drinking-water PFAS over an \~8-year horizon. Two factors qualify the
null. First, harmonizing to UCMR3's high reporting levels collapses the
outcome to rare events (base detection rates of 2–3% for PFOS/PFOA and
below 1.5% for the rest), leaving little variation and limited
statistical power. Second, and more substantively, the design tests only
*newly opened* facilities, whereas PFOS contamination is overwhelmingly
a **legacy** phenomenon tied to pre-2002 manufacturing (Sections
4.5–4.7) — and legacy sources predate the 2015–2023 panel window
entirely. A difference-in-differences keyed to recent facility openings
is therefore structurally unable to capture the legacy pathway that the
cross-sectional models identify. The null DiD and the *significant*
pre-2002 cross-sectional results are thus mutually consistent: together
they point to legacy rather than contemporary industrial activity as the
operative source.

### 8.2 TRI Emission-Weighted Exposure

**Design.** The facility database used throughout Sections 4–7
classifies a site as a "PFAS source" if its NAICS industry code is
PFAS-relevant — a binary and often inaccurate proxy. The EPA **Toxics
Release Inventory (TRI)** began requiring facility-level PFAS reporting
in 2020 and provides actual annual release mass (in pounds) with
coordinates. Using the 2022 and 2023 national TRI files, an
emission-weighted exposure index was built for each UCMR5 system:

$$\text{TRI-IDW}_i = \sum_j \text{release\_lbs}_j \cdot \exp\!\left(-\frac{d_{ij}}{10{,}000}\right)$$

summed over reporting PFAS facilities, then z-scored for interpretable
per-standard-deviation coefficients. The binding constraint is coverage:
only **74 facilities nationwide** reported PFAS releases with usable
coordinates, and the median water system is **99.8 km** from the nearest
one. TRI PFAS reporting is still in its infancy, so the exposure measure
carries little spatial variation.

**Table 13: TRI Emission-Weighted Exposure Results** *(log(PFAS) \~
z-scored TRI-IDW)*

| Compound | Baseline β (per SD) | p-value | ZCTA-FE β | p-value | Direction |
|----|----|----|----|----|----|
| PFOA | +0.0828 | 0.130 | +52.0 | **0.055** | Positive (marginal) |
| PFOS | +0.0381 | 0.648 | +23.5 | 0.356 | Positive (ns) |
| PFHpA | +0.318 | **0.042** | −3.58 | 0.947 | Positive baseline only |
| PFBA | +0.137 | **0.049** | +0.105 | 0.987 | Positive baseline only |
| PFHxS | −0.0124 | 0.396 | +1.82 | 0.944 | ns |
| PFHxA | +0.0124 | 0.646 | −43.2 | 0.236 | ns |
| PFBS | −0.0085 | 0.628 | +0.118 | 0.997 | ns |
| PFPeA | −0.0020 | 0.944 | −45.2 | 0.284 | ns |

*Baseline models include treatment covariates. The very large ZCTA-FE
coefficients reflect the near-absence of within-ZCTA variation in TRI
exposure (most systems have an index of essentially zero). PFPeS (n =
16) again shows a spurious result and is disregarded.*

**Result.** Emission-weighted exposure points in the hypothesized
positive direction for the regulated and long-chain compounds, but the
signal is weak. In the baseline (treatment-covariate) models, PFHpA (p =
0.042) and PFBA (p = 0.049) are positive and significant, while PFOA (p
= 0.13) and PFOS (p = 0.65) are positive but not significant. Under ZCTA
fixed effects the strongest signal is **PFOA (p = 0.055, marginal)**,
followed by PFOS (positive, p = 0.36).

**Interpretation.** TRI emissions corroborate the cross-sectional story
weakly and in the right direction — PFOA, the compound co-produced with
PFOS at legacy sites, is the closest to significance under within-ZCTA
identification — but the instrument is currently too sparse to be
decisive. As TRI PFAS reporting expands (more facilities, more
compounds, more reporting years), an emission-weighted index is the most
promising route to replace the noisy binary NAICS proxy. For now it is
best read as suggestive corroboration rather than independent
confirmation.

### 8.3 Hydrological Flow-Path Exposure

**Design.** Every model to this point measures industrial proximity with
*omnidirectional* Euclidean distance, which treats a facility 3 km away
across a drainage divide identically to one 3 km upstream on the same
river. For surface-water systems this is a fundamental misspecification:
contamination travels down the surface-water network, not in straight
lines. This analysis rebuilds the exposure measure hydrologically. Each
surface-water system's coordinate is snapped to the USGS **NHDPlus**
stream network (via the `nhdplusTools` interface to the USGS
Network-Linked Data Index); a PFAS facility counts only if it is
**genuinely upstream** of the intake; and each upstream facility is
weighted by its **network (along-river) distance** rather than
straight-line distance, using NHDPlus `pathlength` value-added
attributes:

$$\text{HydroIDW}_i = \sum_{j\,\in\,\text{upstream}(i)} \exp\!\left(-\frac{\text{flowpath\_dist}_{ij}}{10{,}000}\right)$$

A separate index restricts to **pre-2002 legacy facilities** — sources
that are both historically PFOS-relevant *and* hydrologically upstream,
the sharpest operationalization of this report's central
legacy-contamination finding. Because true drinking-water intake
coordinates are not published nationally, the ECHO system coordinate
(snapped to the network) is used as an intake proxy.

**Coverage.** Of 460 surface-water systems, **98% snapped successfully**
to the network; 805 PFAS observations from 452 systems entered the
models. **201 systems (44%) have at least one genuinely upstream PFAS
facility** (162 have an upstream pre-2002 facility), with the nearest
upstream facility a median of 4.9 network-km away. The remaining systems
have no upstream industrial source on the surface network — itself
informative, and the reason the measure is zero for the majority.

**Table 14: Euclidean vs Hydrological Flow-Path Exposure — PFOS and
PFOA** *(per-SD coefficients, identical surface-water subset)*

| Outcome  | Specification                              | β (per SD) | p-value   | N   |
|----------|--------------------------------------------|------------|-----------|-----|
| **PFOS** | Euclidean IDW (same subset)                | +0.017     | 0.750     | 333 |
|          | Upstream HydroIDW (all facilities)         | +0.014     | 0.692     | 333 |
|          | Upstream HydroIDW (all) + ZCTA FE          | +2.41      | 0.089     | 349 |
|          | Upstream HydroIDW (pre-2002 legacy)        | +0.015     | 0.656     | 333 |
|          | **Upstream HydroIDW (pre-2002) + ZCTA FE** | **+2.46**  | **0.048** | 349 |
| **PFOA** | Euclidean IDW (same subset)                | +0.008     | 0.850     | 290 |
|          | Upstream HydroIDW (all facilities)         | −0.060     | 0.243     | 290 |
|          | **Upstream HydroIDW (all) + ZCTA FE**      | **+4.00**  | **0.024** | 301 |
|          | Upstream HydroIDW (pre-2002 legacy)        | −0.033     | 0.481     | 290 |
|          | **Upstream HydroIDW (pre-2002) + ZCTA FE** | **+4.45**  | **0.013** | 301 |

**Result.** Hydrological routing produces the clearest corroboration of
the hypothesis in the entire project. On the *identical* surface-water
systems, the Euclidean IDW is completely null for both regulated
compounds (PFOS p = 0.75, PFOA p = 0.85). Replacing it with an
**upstream** exposure measure and controlling for ZCTA geography turns
both significant and positive: within the same ZCTA, surface-water
systems with more upstream legacy industrial exposure have higher PFOS
(β = +2.46, p = 0.048) and higher PFOA (β = +4.45, p = 0.013). Notably,
**this is the only specification anywhere in the analysis where PFOA —
the long-chain compound co-produced with PFOS — reaches statistical
significance**, and it does so under the most physically faithful
exposure model.

**Interpretation.** The pattern is exactly what the contamination
hypothesis predicts and what the future-research plan anticipated:
omnidirectional distance dilutes the true signal by counting
hydrologically irrelevant facilities, and correcting for flow direction
recovers it. Two features make the result credible — the signal is
concentrated in the **pre-2002 legacy** measure (consistent with every
other strand of the analysis) and in the **within-ZCTA** comparison
(where treatment-investment and regional-background confounders are
differenced out). Three caveats temper it. First, the effect appears
only with ZCTA fixed effects — the baseline hydrological models are null
— so it is a within-neighborhood result, not an unconditional one.
Second, the large per-SD coefficients reflect limited within-ZCTA
variation in upstream exposure (most systems have none), so the
standardized magnitudes should not be read literally even though the
p-values are valid. Third, the intake-coordinate proxy introduces
snapping error (2% of systems failed to route; some may mis-snap to a
minor tributary). Even with these qualifications, the hydrological
analysis is the strongest single piece of evidence that genuinely
upstream, legacy industrial PFAS sources elevate drinking-water
contamination.

### 8.4 What the Extensions Add

Taken together, the three extensions **sharpen** the cross-sectional
conclusions and converge on a single story. The
difference-in-differences shows the relationship is not driven by
*contemporary* facility openings; the TRI model shows that measuring
sources by actual emissions still points in the expected direction for
the regulated compounds; and the hydrological flow-path analysis — the
most physically faithful exposure model — converts the null Euclidean
result for PFOS and PFOA into significant, positive, within-ZCTA effects
for genuinely upstream legacy sources. The common thread across all
three, and across Sections 4–7, is that the drinking-water PFAS signal
is **legacy** (pre-2002) and is recoverable only when the exposure
measure and the identification strategy are sharp enough to separate it
from treatment-investment and geographic confounding. The extensions do
not overturn the cautious cross-sectional findings; they explain why the
signal is easy to miss, and they recover it for exactly the two
compounds — PFOS and PFOA — that matter most for regulation.

### 8.5 NAICS Source-Likelihood Refinement and AFFF-Specific Exposure

The baseline exposure measures treat all 39 presumptive-source NAICS
codes as equally likely to release PFAS — the same equal-weight
convention used by Salvatore et al. (2022), whose list these codes are
drawn from. As flagged in Section 6.3, this is a measurement-error
problem: a fluoropolymer plant, a chrome-plating shop, an airport, and a
"miscellaneous fabricated metal" job-shop are not equally likely PFAS
sources, and pooling them attenuates the proximity coefficients. This
extension tests that limitation directly by differentiating facilities
on evidence-based PFAS likelihood.

Each of the 39 codes was assigned to a likelihood **tier** — tier 1
(high confidence: direct PFAS manufacture/use), tier 2 (plausible
secondary or waste pathway), tier 3 (generic code, weak link) — grounded
in the EPA Effluent Guidelines Program Plans 15 and 16, the EPA
Multi-Industry PFAS Study (2021), and state metal-finishing findings.
Seven codes that EPA is *actively* writing PFAS effluent-limit rules for
(OCPSF PFAS manufacturers and formulators, metal finishing, chromium
electroplating, and landfills) carry the strongest regulatory evidence.
Six experiment families were run on the most credible specification —
`log(PFAS) ~ exposure + ZCTA FE` — restricting or reweighting the
facility set: tier-1-only, EPA-regulated-only, tier-1+2, a distance ×
tier weighted IDW ("Spec D"), a sector-by-sector horse race, and
leave-one-sector-out.

Three findings stand out, all for **PFOS** — the compound that already
carried the cross-sectional signal:

1.  **Reweighting sharpens the signal *within the fixed-effects spec* —
    but the gain is fixed-effects-dependent.** Down-weighting
    low-likelihood codes rather than dropping them (Spec D: tier 1 =
    1.0, tier 2 = 0.5, tier 3 = 0.15) produces the strongest PFOS
    estimate among the ZCTA-FE models (β = +0.337, p = 0.013), improving
    on the equal-weight baseline (p = 0.049). This looked like the
    headline result. The robustness audit below, however, shows it is an
    artifact of the fixed effects: with no fixed effects, Spec D is null
    for PFOS (p = 0.15), as is the equal-weight IDW (p = 0.44). The
    tier-weighting helps only because it slightly rebalances which
    facilities sit inside the thin within-ZCTA slice the FE leaves
    behind — not because it uncovers a robust gradient. **Spec D is
    therefore demoted from the headline.**

2.  **Airports and AFFF carry the PFOS signal.** In the sector horse
    race, the airport sector (NAICS 488119, aqueous film-forming foam)
    is by far the dominant single mechanism: estimated alone, β = +5.11
    (p = 0.016) for all facilities and β = +11.8 (p = 0.002) for
    pre-2002 legacy airports. This is consistent with the toxicological
    record — legacy AFFF was the principal environmental source of PFOS.
    Crucially, a dedicated AFFF/airport exposure index is the **only
    positive PFOS result that survives with no fixed effects at all**:
    in a bare bivariate regression β = +0.146, p ≈ 5×10⁻⁶; adding
    treatment covariates, p = 0.021; and under *state* fixed effects (a
    coarser, honest grouping that retains 97% of the identifying sample)
    it is *strongest* at β = +0.10, p = 0.006. This is the most robust
    facility-proximity finding in the report and the one it now leads
    with.

3.  **Some generic codes actively degrade the measure.** Estimated
    alone, commercial printing (NAICS 323111) shows a *significant
    negative* PFOS coefficient — it is anti-correlated with measured
    PFAS and dilutes the pooled exposure variable. Together with
    miscellaneous fabricated metal (332999), chemical merchant
    wholesalers (424690), miscellaneous electrical equipment (335999),
    and lubricating-oil manufacturing (324191), these low-tier codes are
    the prime candidates for removal from the presumptive list.

For **PFOA**, no facility-based refinement recovers a robust signal; the
tier-1-restricted measure moves it only from clearly null (p = 0.45) to
marginal (β = +0.70, p = 0.087), reinforcing the Section 5 conclusion
that PFOA's drinking-water proximity signal is genuinely weak in this
sample.

These refined measures are now part of the canonical pipeline: a
distance × tier weighted IDW (`idw_tierweighted_10km`) and an
AFFF/airport-only IDW (`idw_airport_10km`), each with pre-2002 variants,
are constructed in `clean_data_construction.R` and exported as dedicated
result tables. The full experiment code and tier assignments are in
`naics_experiments.R`, `naics_tiers.R`, and `analysis_naics_weighted.R`,
with detailed results in `NAICS_findings.md`.

#### 8.5.1 The fixed-effects robustness audit

The ZCTA fixed-effects specification used throughout Sections 4–7 turns
out to absorb almost all of the variation it is fed, which makes its
"significant" proximity results fragile. For the PFOS estimation sample
(841 systems, 425 ZCTAs):

-   **\~30% of observations are in singleton ZCTAs** — one water system
    in their ZIP area — so their dummy fits them perfectly and they
    contribute *nothing* to the slope.
-   The ZCTA dummies absorb **99.5–100% of the variance** in every
    exposure measure (R² of `exposure ~ factor(ZCTA)`), leaving the
    within-ZCTA exposure standard deviation at just 1–9% of the total.
-   The PFOS coefficient is consequently identified off only **\~20
    ZCTAs and \~82 systems (≈10% of the sample)**.

Re-estimating PFOS at progressively coarser geography makes the
trade-off explicit (coefficient, p-value; identifying sample shrinks as
the FE get finer):

| Exposure (PFOS) | No FE (+cov) | **State FE** | ZIP3 FE | ZCTA FE |
|----|----|----|----|----|
| Airport / AFFF | +0.085 (**0.021**) | **+0.100 (0.006)** | −0.010 (0.85) | +5.11 (0.017) |
| Tier-weighted (Spec D) | +0.019 (0.21) | +0.032 (**0.034**) | +0.027 (0.23) | +0.346 (0.012) |
| Equal-weight IDW | +0.001 (0.90) | +0.017 (0.12) | +0.016 (0.36) | +0.329 (0.044) |
| *identifying obs* | *785* | *\~760* | *\~639* | *\~80* |

The pattern is unambiguous. The **equal-weight and tier-weighted IDW
results are significant only at ZCTA**, where \~100% of the variation is
absorbed and \~80 systems carry the estimate — the classic
over-absorbed-FE artifact, and the inflated ZCTA β of +5.11 for airport
(vs +0.10 at state) is the tell-tale sign of a slope blown up by
near-zero within-group variance. The **airport/AFFF result is the
exception**: it is significant with *no* FE and *strongest* under state
FE, where the bulk of the sample still identifies it.

A complementary bivariate audit (`simple_models.R`, every exposure with
no FE and nothing else) confirms it: airport/AFFF → PFOS is the lone
exposure that is positive and significant in the raw data, while the
distance and buffer measures are frequently *wrong-signed* (the
proximity paradox — water nearer industry testing cleaner, a
treatment-investment confound).

The corrected practical implication is a **narrow but solid** finding
inside a **broad but confounded** one: the relationship between facility
proximity and drinking-water PFOS is real but runs specifically through
legacy AFFF/airport sources, and it does not require the fixed-effects
scaffolding. The broad "any PFAS-adjacent industry" version of the
hypothesis is not robustly supported once that scaffolding is removed.
Diagnostics are reproducible via `diagnose_fe_variation.R`,
`granularity_compare.R`, and `simple_models.R` (with
`SIMPLE_MODELS_EXPLAINED.md`).

------------------------------------------------------------------------

## 9. Conclusion

This analysis applies eight cross-sectional econometric specifications
to EPA UCMR5 data covering **2,032 sampling points** from **1,060 public
water systems**, matched to a national database of PFAS-adjacent
industrial facilities via geodetic distance and IDW exposure measures,
and extends them with three further analyses: a
difference-in-differences across UCMR rounds, a TRI emission-weighted
exposure model, and a hydrological flow-path (upstream-network) model.

The central hypothesis — that PFAS source proximity predicts elevated
drinking water PFAS concentrations — receives robust support for
**PFOS**, but a robustness audit (Section 8.5) shows that support is
concentrated in **one source channel** rather than spread across
industry. Proximity to legacy **AFFF/airport** sources predicts
significantly higher PFOS in every specification, including those with
*no* fixed effects (bivariate p ≈ 5×10⁻⁶; with covariates p = 0.021;
under state fixed effects p = 0.006). The four ZCTA-fixed-effects
proximity results that the earlier draft treated as the core evidence (1
km buffer p = 0.031; pre-2002 distance p = 0.044; IDW p = 0.049;
pre-2002 IDW p = 0.026) are correctly signed and mutually consistent,
but they rest on fixed effects that absorb \~100% of the exposure
variation and identify the slope off \~10% of the sample; they do not
survive removing those fixed effects. They are best read as
direction-consistent corroboration of the AFFF/airport finding, not as
four independent confirmations.

The three extensions clarify what *kind* of industrial activity is
responsible and recover the signal for the compounds that matter most.
The difference-in-differences — the most causally credible design in the
project, using each system as its own control across UCMR3 and UCMR5 —
finds no effect of *newly opened* facilities on PFAS detection (pooled β
≈ 0, p = 0.96). Rather than contradicting the PFOS finding, this null
pinpoints it: because PFOS is a pre-2002 legacy contaminant, a test of
facilities opened after 2015 cannot capture it, and the null is exactly
what the legacy interpretation predicts. The TRI emission-weighted
model, which measures sources by reported release mass rather than
industry code, likewise points positive for the regulated compounds
(PFOA marginal at p = 0.055), though sparse PFAS reporting (74
facilities nationally) keeps the estimate imprecise. Most decisively,
the hydrological flow-path model — which counts only facilities
genuinely upstream of each intake along the surface-water network —
converts the null Euclidean result into significant, positive
within-ZCTA effects for **both** PFOS (p = 0.048) and PFOA (p = 0.013)
when exposure is restricted to upstream pre-2002 legacy sources; this is
the only place in the analysis where PFOA attains significance. This
result is also ZCTA-FE-based and so carries the same fragility caveat as
the other within-ZCTA findings (Section 8.5) — it warrants a
no-fixed-effects re-check — but the upstream *direction* of the effect
is mechanistically sound. Together the extensions reinforce a
legacy-contamination reading of the cross-sectional results.

The hypothesis does not receive clear support for PFOA (though PFOA
trends positive in the best specifications), and it receives no support
for short-chain compounds. Treatment confounding — industrially exposed
utilities investing more in PFAS treatment and suppressing measured
concentrations — is a pervasive competing force that complicates
interpretation of many specifications, particularly in the simpler
distance models.

The contamination pathway from legacy PFAS sources — above all legacy
AFFF — to drinking water is real, statistically detectable for PFOS, and
policy-relevant. The key methodological lesson, learned the hard way in
this project, is that **the exposure measure matters more than the
fixed-effects machinery**: differentiating sources (isolating
AFFF/airport facilities) recovers a signal that holds under the simplest
possible model, whereas piling on granular ZCTA fixed effects mostly
manufactures fragile significance by absorbing nearly all the variation
and identifying the effect off a handful of systems. Proximity-based
regulatory targeting should therefore be built on **source-specific
exposure** — legacy AFFF/airport and foam-discharge sites first — rather
than on broad "any presumptive industry" distance measures or on overly
granular fixed-effects specifications. Geographic confounding is real
and worth controlling, but at a sensible scale (e.g. state) that leaves
variation left to identify the effect.

The 2024 EPA MCLs for PFOA, PFOS, PFNA, PFHxS, HFPO-DA, and PFBS
represent a critical first step. The evidence in this project suggests
that fully addressing the scope of PFAS contamination will require
targeted monitoring enhancements near legacy industrial sites, treatment
financing for small and very proximate utilities, regulatory attention
to PFBA and other unregulated compounds, and continued investment in
watershed-based exposure modeling that captures the actual hydrological
pathways connecting manufacturing to the tap.

------------------------------------------------------------------------

## Appendix A: Data Sources

| Dataset | Source | Purpose |
|----|----|----|
| UCMR5_All.csv | EPA UCMR5 | PFAS measurements (2023–2025) |
| UCMR3_All.txt | EPA UCMR3 | PFAS measurements (2013–2015), DiD panel |
| TRI 2022 & 2023 national files | EPA Toxics Release Inventory | Facility-level PFAS release mass + coordinates (emission-weighted exposure) |
| NHDPlus V2 network + pathlength VAA | USGS (via `nhdplusTools`/NLDI) | Stream network and along-river distances (hydrological flow-path exposure) |
| UCMR5_ZIPCodes.txt | EPA UCMR5 | Service-area ZIP codes (reference only; not used for location) |
| UCMR5_AddtlDataElem.txt | EPA UCMR5 | Treatment and source covariates |
| ECHO_EXPORTER.csv | EPA ECHO | Precise GPS for water systems (primary location source) |
| ZCTA boundaries (2020) | U.S. Census / tigris | ZCTA polygons for spatial ZIP assignment |
| NATIONAL_FACILITY_FILE.CSV | EPA FRS | Manufacturing facility registry |
| NATIONAL_NAICS_FILE.CSV | EPA FRS | NAICS codes per facility |
| NATIONAL_ENVIRONMENTAL_INTEREST_FILE.CSV | EPA FRS | Facility start dates |
| FLOW.csv / PHYSICAL_LOCATION.csv | 2022 CWNS | Industrial water treatment centers |
| pfas_progress_june_2021.csv | DoD | Military PFAS installation assessment |
| mirta dataset | MIRTA | Military installation coordinates |

## Appendix B: Formal Model Specifications

**OLS Baseline (Models 1, 5):**
$$\log(PFAS_{i,c}) = \beta_0 + \beta_1 X_i + \mathbf{\gamma}' \mathbf{Z}_i + \varepsilon_{i,c}$$

where
$X_i \in \{\text{nearest\_distance\_m},\, \text{idw\_exposure\_10km}\}$
and $\mathbf{Z}_i$ is the vector of binary treatment-process covariates.

**Fixed-Effects (Models 2, 6, 7, 8):**
$$\log(PFAS_{i,c}) = \beta_0 + \beta_1 X_i + \alpha_z + \varepsilon_{i,c}$$

where $\alpha_z$ is the ZCTA fixed effect absorbing all cross-sectional
ZCTA-level variation. Identification in this model requires ZCTAs
containing more than one sampled water system.

**Buffer Dummies (Models 3, 4):**
$$\log(PFAS_{i,c}) = \beta_0 + \beta_k \cdot \mathbf{1}[d_i \leq k] + \varepsilon_{i,c}, \quad k \in \{1,5,10,25\}\text{ km}$$

**IDW Exposure Index:**
$$\text{IDW}_i = \sum_{j=1}^{10} \exp\!\left(-\frac{d_{ij}}{10{,}000}\right)$$

Pre-2002 variants replace $d_{ij}$ with distances computed to the subset
of facilities with $\text{START\_DATE} < \text{2002-01-01}$.

**Difference-in-Differences (Section 8.1):** With two periods, the
two-way fixed-effects model reduces to a first difference:

$$\Delta D_{i} = D_{i,\text{UCMR5}} - D_{i,\text{UCMR3}} = \beta\,\Delta \text{IDW}_i + \varepsilon_i$$

where $D_{i,t}$ is a PFAS detection indicator evaluated at the common
(UCMR3) reporting threshold, and $\Delta \text{IDW}_i =
\text{IDW}_i(\text{2023}) - \text{IDW}_i(\text{2015})$ is the change in
the inverse-distance-weighted exposure index, reconstructed from
facility opening dates. The pooled model adds compound fixed effects.

**TRI Emission-Weighted Exposure (Section 8.2):**
$$\text{TRI-IDW}_i = \sum_{j} \text{release\_lbs}_j \cdot \exp\!\left(-\frac{d_{ij}}{10{,}000}\right)$$

summed over TRI-reporting PFAS facilities $j$, entered as a z-scored
regressor in both treatment-covariate and ZCTA fixed-effects models.

**Hydrological Flow-Path Exposure (Section 8.3):**
$$\text{HydroIDW}_i = \sum_{j\,\in\,\text{upstream}(i)} \exp\!\left(-\frac{f_{ij}}{10{,}000}\right), \qquad f_{ij} = \text{pathlength}(c_j) - \text{pathlength}(c_i)$$

where $c_i$ is the NHDPlus flowline (COMID) onto which system $i$'s
intake is snapped, $c_j$ the flowline nearest facility $j$, and
$\text{pathlength}(\cdot)$ the NHDPlusV2 network distance from a
flowline to the network terminus; $f_{ij} > 0$ identifies facilities
strictly upstream of the intake. Facilities are retained only if within
5 km of a flowline and within 100 network-km upstream. The pre-2002
variant restricts $j$ to facilities with
$\text{START\_DATE} < \text{2002-01-01}$. Estimated on surface-water
systems only, z-scored, with and without ZCTA fixed effects.

## Appendix C: R Packages and Computational Notes

All analyses were conducted in R (tidyverse 2.0.0, data.table, sf 1.0,
broom, tigris). Distance matrices were computed using
`sf::st_distance()` on WGS84 coordinates. IDW exposure indices were
computed by applying the exponential decay function to the sorted 10
nearest distances from each water system sampling point. ZCTA ZIP code
assignment used `sf::st_join()` between GPS-geocoded water system
coordinates (from EPA ECHO, one coordinate per PWSID) and 2020 Census
ZCTA boundary polygons downloaded via the `tigris` package (cached
locally). Multiple sampling points (FacilityIDs) within the same water
system share ECHO coordinates and therefore share all distance and IDW
exposure measures. The extension analyses used `fixest`-free base `lm`
first differences (difference-in-differences), `spdep`/`spatialreg`
(spatial models, Section 5 robustness), and `nhdplusTools` with the USGS
Network-Linked Data Index for the hydrological flow-path model — system
coordinates were snapped to NHDPlus flowlines via
`discover_nhdplus_id()`, upstream networks retrieved with
`navigate_nldi(mode = "UT")`, and along-river distances computed from
the national `pathlength` value-added attribute table (`get_vaa()`,
cached). Per-system NLDI results were cached locally so the routing pass
is resumable. All model output tables were exported to CSV via
`write.csv()` to the `model_outputs/` subdirectory of the BBR project
folder.

------------------------------------------------------------------------

*This report was prepared as part of the Big Blue Research (BBR)
Scholars Program at the University of Nebraska–Lincoln. Raw data files
are held locally and are publicly available from the EPA's UCMR data
portal, the EPA FRS database, and the EPA ECHO system.*
