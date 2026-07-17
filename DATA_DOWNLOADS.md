# DATA_DOWNLOADS.md — Everything you need to download to run this repo

The repo tracks all scripts, all model outputs, all small/derived datasets,
and the exposure caches. What it does NOT track (see `.gitignore`) are the
large public raw datasets. This file lists **every external download**, where
to put it, and which scripts need it — so a fresh `git clone` can reproduce
everything.

**Quick triage:** the current (2026) analysis pipeline —
`UCMR5_full_rebuild.R` → `full_tobit_clustered.R` → placebo/density/deep-dive
scripts — needs only downloads **1, 2, 3** below. Items 4–6 are only for the
legacy/archived analyses. Item 0 requires no download at all.

---

## 0. Runs with NO downloads (caches are committed)

Because `model_outputs_censored/ucmr5_full_inputs.rds`,
`ucmr5_full_exposures.rds`, `ucmr5_placebo_exposures.rds`, and
`ucmr5_sector_exposures.rds` are committed, these run from a fresh clone
as-is:

- `full_tobit_clustered.R` (baseline grids — skips the exposure build when
  the cache exists)
- `placebo_military.R`
- `popdensity_tests.R` (Census county file is committed at
  `PFAS_Project_Data/census/co-est2024-alldata.csv`; county boundaries are
  auto-fetched by the `tigris` package)
- `industrial_deepdive_pfoa.R` (sector cache committed)

To *rebuild* those caches from raw data you need items 1–3.

---

## 1. UCMR5 complete occurrence data  — REQUIRED for the current pipeline

- **What:** EPA UCMR5 occurrence results (Jan 2026 release, ~1.93M rows),
  additional data elements (covariates), ZIP codes.
- **URL:** https://www.epa.gov/system/files/other-files/2023-08/ucmr5-occurrence-data.zip
  (EPA updates this file in place; landing page:
  https://www.epa.gov/dwucmr/occurrence-data-unregulated-contaminant-monitoring-rule)
- **Size:** 13 MB zip → ~330 MB extracted.
- **Place at:** `PFAS_Project_Data/ucmr5_full/`
  (must contain `UCMR5_All.txt`, `UCMR5_AddtlDataElem.txt`, `UCMR5_ZIPCodes.txt`)
- **Used by:** `UCMR5_full_rebuild.R`, `full_tobit_clustered.R` (Size class
  merge in `industrial_deepdive_pfoa.R`).
- **⚠ NEVER open these files in Excel.** Excel silently truncates at
  1,048,575 rows and corrupted an earlier copy of this dataset. Read with
  `data.table::fread` only.

```sh
mkdir -p PFAS_Project_Data/ucmr5_full
curl -L -o PFAS_Project_Data/ucmr5_full/ucmr5.zip \
  https://www.epa.gov/system/files/other-files/2023-08/ucmr5-occurrence-data.zip
unzip -o PFAS_Project_Data/ucmr5_full/ucmr5.zip -d PFAS_Project_Data/ucmr5_full/
```

## 2. EPA FRS national combined files — REQUIRED to rebuild exposures

- **What:** Facility Registry Service national files: facility coordinates,
  NAICS codes, environmental-interest start dates. Source of the 21,479
  PFAS-industry facilities, the 774 airports, and the placebo facilities.
- **URL:** https://www.epa.gov/frs/epa-state-combined-csv-download-files →
  "National Combined" (`national_combined.zip`)
- **Size:** ~1.5 GB zip → ~5 GB extracted.
- **Place at:** `PFAS_Project_Data/frs/` (needs `NATIONAL_FACILITY_FILE.CSV`,
  `NATIONAL_NAICS_FILE.CSV`, `NATIONAL_ENVIRONMENTAL_INTEREST_FILE.CSV`)
- **Used by:** `clean_data_construction.R`, `naics_prep_cache.R` (which
  builds `model_outputs/naics_inputs.rds` — the facility table read by
  `full_tobit_clustered.R`'s exposure builder), `placebo_industrial.R`.

## 3. EPA ECHO exporter — REQUIRED to rebuild water-system coordinates

- **What:** ECHO facility export; supplies one lat/lon per PWSID via
  `SDWA_IDS` (`FAC_LAT`, `FAC_LONG`).
- **URL:** https://echo.epa.gov/files/echodownloads/echo_exporter.zip
  (landing page: https://echo.epa.gov/tools/data-downloads)
- **Size:** ~250 MB zip → ~2 GB extracted.
- **Place at:** `PFAS_Project_Data/echo/ECHO_EXPORTER.csv`
- **Used by:** `UCMR5_full_rebuild.R`, `clean_data_construction.R`.

## 4. UCMR5 legacy copy — legacy scripts only

- **What/where:** same download as item 1, but the old pipeline
  (`clean_data_construction.R`, `naics_prep_cache.R`) reads
  `PFAS_Project_Data/ucmr5/UCMR5_All.csv` (a CSV conversion) plus the two
  `.txt` files. If you rebuild it, extract item 1's zip here and rename/
  convert `UCMR5_All.txt` accordingly — or better, port those scripts to
  the `.txt`. Historical note: the old CSV was Excel-truncated, which is
  why `ucmr5_full/` exists.

## 5. UCMR3 occurrence data — legacy DiD only

- **URL:** https://www.epa.gov/dwucmr/occurrence-data-unregulated-contaminant-monitoring-rule
  → "UCMR 3 Occurrence Data" zip
- **Size:** ~180 MB extracted. **Place at:** `PFAS_Project_Data/ucmr3/`
- **Used by:** `UCMR_DiD_Dataset_Building.R` (archived; the DiD was null).

## 6. EPA TRI basic data files (2022, 2023) — legacy TRI models only

- **URL:** https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-present
- **Size:** ~60 MB each. **Place at:** `PFAS_Project_Data/tri/tri_2022_us.csv`,
  `tri_2023_us.csv`
- **Used by:** `analysis_external_data.R` (archived).

---

## Auto-downloaded by packages (no manual step)

- **Census county boundaries (2023 cartographic)** and **ZCTA boundaries
  (2020)** — fetched by the `tigris` package on first use and cached
  (`options(tigris_use_cache = TRUE)`). Used by `popdensity_tests.R`,
  `clean_data_construction.R`.

## Already committed in the repo (no download)

- `PFAS_Project_Data/census/co-est2024-alldata.csv` — Census county
  population estimates 2020–2024 (source:
  https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/counties/totals/co-est2024-alldata.csv)
- `PFAS_Project_Data/military/` — MIRTA installation points; DoD PFAS
  progress list; EWG 2019 crash-sites GeoJSON and 2025 suspected-sites
  GeoJSON (⚠ EWG asks that reuse in publications go through their materials
  request form); `fire_training_sites_geocoded.csv` (derived — rebuildable
  with `geocode_fire_training_sites.py` from the committed PDF).
- `PFAS_Project_Data/cwns/` — CWNS flow/location extracts.
- `List-of-military-fire-and-crash-training-sites-2014.pdf` and the DoD
  installations-assessment PDF.
- All `model_outputs*` result CSVs and the four `ucmr5_*` exposure/input
  caches (see item 0).

## Rebuilt locally, never downloaded

- `model_outputs/naics_inputs.rds` (148 MB; gitignored) — built by
  `naics_prep_cache.R` (~27 min) from items 2–4. Needed only to REBUILD
  exposures or run the legacy NAICS scripts; the current model scripts run
  from the committed caches without it.

## R packages

`tidyverse, data.table, sf, survival, sandwich, broom, jsonlite, tigris`
(Python, only for re-geocoding: `pandas, pypdf, rapidfuzz`).

---

*Suggested order for a full from-scratch rebuild: download 1–3 → run
`UCMR5_full_rebuild.R` → delete the four `ucmr5_*` caches in
`model_outputs_censored/` if you want exposures recomputed → run
`full_tobit_clustered.R` → the placebo/density/deep-dive scripts in any
order.*
