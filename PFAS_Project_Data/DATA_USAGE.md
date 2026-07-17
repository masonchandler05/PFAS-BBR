# PFAS Project — How Each Data Source Is Used

This file explains the role of every data source in the analysis: which script
reads it, what is extracted, how it is transformed, and where it enters the
econometric models. See `DATA_SOURCES.md` for citations and download URLs, and
the repository `README.md` for the run order.

The analysis pipeline:

```
clean_data_construction.R   ← builds the core 2,032-observation dataset + 8 models
   │  (sourced by each extension below)
   ├── analysis_improvements.R     post-2002 index · compound-specific NAICS · spatial models
   ├── analysis_external_data.R    difference-in-differences (UCMR3→UCMR5) · TRI emissions
   └── analysis_hydrology.R        upstream hydrological flow-path exposure
```

---

## 1. UCMR5 (`ucmr5/`) — primary outcome
**Read by:** `clean_data_construction.R`, and re-read for the DiD in
`analysis_external_data.R`.

- `UCMR5_All.csv` — the PFAS measurements. Pivoted to one row per
  PWSID × FacilityID × contaminant (mean `AnalyticalResultValue` over repeat
  samples). The log of each PFAS concentration is the **outcome variable** in
  every regression. `FacilityWaterType` (SW/GU/MX vs GW) defines the
  surface-water subset for the hydrology model.
- `UCMR5_AddtlDataElem.txt` — treatment/PFAS-source self-reports, reshaped into
  binary covariates (GAC, IEX, reverse osmosis, disinfectant type, etc.). These
  are the **treatment-covariate controls** in the baseline models.
- `UCMR5_ZIPCodes.txt` — service-area ZIP codes. Retained only for reference;
  **not** used for location (an earlier draft's row-duplication bug). Location
  comes from ECHO instead.

## 2. UCMR3 (`ucmr3/`) — difference-in-differences panel
**Read by:** `analysis_external_data.R`.

- `UCMR3_All.txt` — 2013–2015 PFAS measurements for the six compounds common to
  UCMR3 and UCMR5 (PFOA, PFOS, PFHxS, PFNA, PFBS, PFHpA). Matched to UCMR5 by
  PWSID to form a two-period panel. Detection is defined at UCMR3's (higher)
  minimum reporting level applied to **both** rounds so the change in detection
  is not an artifact of UCMR5's better sensitivity. Feeds the first-difference
  DiD (Δ detection ~ Δ upstream exposure).

## 3. ECHO Exporter (`echo/`) — water-system location
**Read by:** `clean_data_construction.R`.

- `ECHO_EXPORTER.csv` — only `FAC_LAT`, `FAC_LONG`, `SDWA_IDS` are read.
  Multi-PWSID rows are split so each PWSID gets one coordinate. These GPS points
  are the **canonical location** of every water system: they drive all
  facility-distance calculations, the spatial join to ZCTAs, and (snapped to the
  stream network) the intake location in the hydrology model.

## 4. FRS National Combined (`frs/`) — PFAS source facilities
**Read by:** `clean_data_construction.R`.

- `NATIONAL_NAICS_FILE.CSV` — filtered to 39 PFAS-relevant NAICS codes to flag
  PFAS-adjacent industries.
- `NATIONAL_FACILITY_FILE.CSV` — joined to the PFAS-NAICS list to get facility
  coordinates (`LATITUDE83`, `LONGITUDE83`) and accuracy.
- `NATIONAL_ENVIRONMENTAL_INTEREST_FILE.CSV` — supplies `START_DATE`, the
  facility operating-start date used to build the **pre-2002 legacy** vs
  **post-2002** facility subsets.

Together these become the ~21,000-facility set from which all
nearest-distance, buffer, and IDW exposure measures are computed, and the
pre-2002 subset that recurs as the project's central "legacy" measure.

## 5. TRI 2022 & 2023 (`tri/`) — emission-weighted exposure
**Read by:** `analysis_external_data.R`.

- `tri_2022_us.csv`, `tri_2023_us.csv` — filtered to true PFAS chemicals
  (per/poly-fluoroalkyl names), keeping facility coordinates and `TOTAL RELEASES`
  (lbs). Used to build a release-weighted IDW exposure index — a continuous
  source-intensity measure that replaces the binary NAICS proxy.

## 6. CWNS 2022 (`cwns/`) — industrial wastewater centers
**Read by:** `clean_data_construction.R`.

- `FLOW.csv` — filtered to `FLOW_TYPE == "Industrial Flow"`.
- `PHYSICAL_LOCATION.csv` — supplies coordinates for those facilities.
  Added to the combined PFAS-source location set as a secondary exposure pathway.

## 7. DoD PFAS progress + 8. MIRTA (`military/`) — military AFFF sites
**Read by:** `clean_data_construction.R`.

- `pfas_progress_june_2021.csv` — DoD installations under PFAS assessment.
- `mirta_-223606765265040761.csv` — installation coordinates, joined to the DoD
  list by installation name. Adds military fire/crash-training sites (heavy AFFF
  users) to the combined PFAS-source location set.

## 9. USGS NHDPlus / NLDI — hydrological routing (no stored file)
**Used by:** `analysis_hydrology.R` via `nhdplusTools`.

For each surface-water system: the ECHO coordinate is snapped to the NHDPlus
stream network (`discover_nhdplus_id`), the upstream network is retrieved
(`navigate_nldi`, mode `UT`), and the national `pathlength` attribute table
(`get_vaa`) supplies along-river distances. PFAS facilities are kept only if
genuinely upstream, yielding the upstream HydroIDW exposure measures. Per-system
results are cached in `hydro_cache/`.

## 10. Census ZCTA 2020 — fixed effects (no stored file)
**Used by:** `clean_data_construction.R` via `tigris`.

2020 ZCTA boundary polygons are downloaded and each water system's ECHO
coordinate is spatially joined to its containing ZCTA. The resulting ZIP code is
the **fixed effect** (`factor(ZIPCODE)`) that absorbs cross-neighborhood
confounding in the FE specifications.
