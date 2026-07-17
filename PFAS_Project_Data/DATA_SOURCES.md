# PFAS Project — Data Sources

All datasets used in this project are publicly available from U.S. federal
agencies. Each entry below gives a citation, the official download location, the
local path within this folder, the approximate size, and whether the file is
included in the Git repository or fetched by `download_data.R`.

Accessed: June 2026. Citation style approximates APA with agency-as-author.

---

## 1. EPA UCMR5 — Fifth Unregulated Contaminant Monitoring Rule (2023–2025)
**Primary PFAS outcome data.**

> U.S. Environmental Protection Agency. (2025). *Fifth Unregulated Contaminant
> Monitoring Rule (UCMR 5) Occurrence Data* [Data set]. Office of Water.
> https://www.epa.gov/dwucmr/occurrence-data-unregulated-contaminant-monitoring-rule

- Download: https://www.epa.gov/system/files/other-files/2023-08/ucmr5-occurrence-data.zip
- Local: `ucmr5/UCMR5_All.csv`, `ucmr5/UCMR5_ZIPCodes.txt`, `ucmr5/UCMR5_AddtlDataElem.txt`
- Size: ~178 MB (zip ~13 MB) · **Fetched by `download_data.R`**

## 2. EPA UCMR3 — Third Unregulated Contaminant Monitoring Rule (2013–2015)
**Earlier-period PFAS data for the difference-in-differences panel.**

> U.S. Environmental Protection Agency. (2016). *Third Unregulated Contaminant
> Monitoring Rule (UCMR 3) Occurrence Data* [Data set]. Office of Water.
> https://www.epa.gov/dwucmr/occurrence-data-unregulated-contaminant-monitoring-rule

- Download: https://www.epa.gov/system/files/other-files/2024-04/ucmr3-occurrence-data.zip
- Local: `ucmr3/UCMR3_All.txt`
- Size: ~177 MB (zip ~9 MB) · **Fetched by `download_data.R`**
- Note: UCMR4 (2018–2020) is **not** used — it monitored no PFAS.

## 3. EPA ECHO Exporter — Facility coordinates
**GPS coordinates for public water systems (intake/facility locations).**

> U.S. Environmental Protection Agency. (2026). *ECHO Exporter* [Data set].
> Enforcement and Compliance History Online (ECHO).
> https://echo.epa.gov/tools/data-downloads

- Download: https://echo.epa.gov/files/echodownloads/echo_exporter.zip
- Local: `echo/ECHO_EXPORTER.csv`
- Size: ~2.0 GB · **Fetched by `download_data.R`**
- Columns used: `FAC_LAT`, `FAC_LONG`, `SDWA_IDS`.

## 4. EPA FRS — Facility Registry Service, National Combined files
**Industrial facility registry, NAICS industry codes, and operating start dates.**

> U.S. Environmental Protection Agency. (2026). *Facility Registry Service (FRS)
> National Combined Files* [Data set]. https://www.epa.gov/frs/epa-frs-facilities-state-single-file-csv-download

- Download: https://ordsext.epa.gov/FLA/www3/state_files/national_combined.zip
- Local: `frs/NATIONAL_FACILITY_FILE.CSV`, `frs/NATIONAL_NAICS_FILE.CSV`,
  `frs/NATIONAL_ENVIRONMENTAL_INTEREST_FILE.CSV`
- Size: ~3.2 GB combined · **Fetched by `download_data.R`**

## 5. EPA TRI — Toxics Release Inventory Basic Data Files (2022, 2023)
**Facility-level PFAS release mass for the emission-weighted exposure model.**

> U.S. Environmental Protection Agency. (2024). *Toxics Release Inventory (TRI)
> Basic Data Files, Reporting Years 2022 and 2023* [Data set].
> https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-present

- Download: https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/2022_US/csv
  and `.../2023_US/csv`
- Local: `tri/tri_2022_us.csv`, `tri/tri_2023_us.csv`
- Size: ~120 MB combined · **Fetched by `download_data.R`**
- Columns used: FRS ID, LATITUDE, LONGITUDE, CHEMICAL, TOTAL RELEASES.

## 6. EPA CWNS — 2022 Clean Watersheds Needs Survey
**Industrial-flow wastewater treatment center locations.**

> U.S. Environmental Protection Agency. (2024). *2022 Clean Watersheds Needs
> Survey (CWNS): National Data* [Data set]. Office of Wastewater Management.
> https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-2022-report-and-data

- Portal (no static zip URL): https://sdwis.epa.gov/ords/sfdw_pub/r/sfdw/cwns_pub/data-download
- Local: `cwns/FLOW.csv`, `cwns/PHYSICAL_LOCATION.csv`
- Size: ~6.4 MB · **Included in repository** (no scriptable direct URL).

## 7. U.S. DoD — PFAS Cleanup Progress (June 2021)
**Military installations under PFAS assessment.**

> U.S. Department of Defense. (2021). *Addressing PFAS at DoD Installations:
> Progress as of June 2021* [Data set]. Office of the Assistant Secretary of
> Defense (Sustainment).

- Local: `military/pfas_progress_june_2021.csv`
- Size: ~44 KB · **Included in repository.**

## 8. U.S. DoD — MIRTA (Military Installations, Ranges, and Training Areas)
**Coordinates for military installations (joined to the DoD PFAS list).**

> U.S. Department of Defense. (2020). *Military Installations, Ranges, and
> Training Areas (MIRTA)* [Data set]. https://catalog.data.gov/dataset/military-installations-ranges-and-training-areas

- Local: `military/mirta_-223606765265040761.csv`
- Size: ~96 KB · **Included in repository.**

---

## Programmatically fetched (no stored file)

## 9. USGS NHDPlus V2 / Network-Linked Data Index (NLDI)
**Stream network and along-river (pathlength) distances for the hydrological
flow-path model.** Fetched on demand by the `nhdplusTools` R package; the national
`pathlength` value-added-attribute table is downloaded once and cached by the
package.

> U.S. Geological Survey. (2019). *NHDPlus Version 2 / Network-Linked Data Index
> (NLDI)* [Data service]. https://www.usgs.gov/national-hydrography
> Accessed via Blodgett, D. (2023). *nhdplusTools: Tools for Accessing and Working
> with the NHDPlus.* R package. https://doi.org/10.5066/P97AS8JD

## 10. U.S. Census Bureau — ZIP Code Tabulation Areas (2020)
**ZCTA boundary polygons for the ZIP-code fixed effects.** Fetched on demand by
the `tigris` R package.

> U.S. Census Bureau. (2020). *2020 ZIP Code Tabulation Areas (ZCTAs)*
> [Data set, cartographic boundary files]. Accessed via the `tigris` R package.
