# PFAS Industrial Proximity & Drinking-Water Contamination

Spatial-econometric analysis of whether proximity to PFAS-adjacent industrial
facilities predicts PFAS concentrations in U.S. public drinking water, using EPA
UCMR5 monitoring data. Eight cross-sectional models plus four extensions
(post-2002 stratification, spatial regression, a UCMR3→UCMR5 difference-in-
differences, TRI emission-weighted exposure, and a hydrological flow-path model).

Full write-up: **[`PFAS_Policy_Report.md`](PFAS_Policy_Report.md)**.

## Repository layout

```
clean_data_construction.R     Builds the core 2,032-obs dataset + the 8 base models
analysis_improvements.R       Post-2002 index · compound-specific NAICS · spatial models
analysis_external_data.R      Difference-in-differences (UCMR3→UCMR5) · TRI emissions
analysis_hydrology.R          Upstream hydrological flow-path exposure (NHDPlus)
download_data.R               Fetches the large public datasets from EPA
future_directions.qmd         Research-extension roadmap
PFAS_Policy_Report.md         Final report (all results + interpretation)
model_outputs/                Result tables (CSV) — committed
hydro_cache/                  Cached per-system NLDI results — committed (delete to refresh)
PFAS_Project_Data/            Data folder (see DATA_SOURCES.md / DATA_USAGE.md)
```

## Data

The large raw files (~5.5 GB: ECHO, FRS, UCMR3/5, TRI) are **not** stored in Git —
they exceed GitHub's 100 MB file limit. They are all public and are fetched from
their official EPA sources by `download_data.R`. The small files that have no
scriptable download (CWNS, DoD, MIRTA) are included in the repo.

- **[`PFAS_Project_Data/DATA_SOURCES.md`](PFAS_Project_Data/DATA_SOURCES.md)** —
  citations and download URLs for every source.
- **[`PFAS_Project_Data/DATA_USAGE.md`](PFAS_Project_Data/DATA_USAGE.md)** —
  how each source enters the analysis.

## Reproduce

From the repository root:

```bash
# 1. Install R packages
Rscript -e 'install.packages(c("tidyverse","data.table","sf","broom","tigris",
  "spdep","spatialreg","nhdplusTools"), repos="https://cloud.r-project.org")'

# 2. Download the large public datasets (~5.5 GB; needs curl + unzip)
Rscript download_data.R

# 3. Run the analysis (each extension re-sources clean_data_construction.R)
Rscript clean_data_construction.R     # core dataset + 8 models
Rscript analysis_improvements.R       # post-2002 · NAICS · spatial
Rscript analysis_external_data.R      # difference-in-differences · TRI
Rscript analysis_hydrology.R          # hydrological flow-path (NHDPlus web calls; slow)
```

All result tables are written to `model_outputs/`. NHDPlus (`nhdplusTools`) and
Census ZCTAs (`tigris`) download what they need on first use. The hydrology step
makes ~900 calls to the USGS NLDI service and takes ~40 min; its per-system cache
in `hydro_cache/` is committed, so it returns instantly unless you delete the
cache to force a fresh run.

## Notes / limitations

- Run scripts from the repository root (paths are relative to it).
- `UCMR4` is intentionally unused — it monitored no PFAS.
- The hydrology model uses ECHO coordinates as an intake proxy (true intakes are
  not published nationally); see the report's §8.3 for caveats.
