# =============================================================================
# analysis_external_data.R
# -----------------------------------------------------------------------------
# Implements the two future_directions.qmd extensions whose required data has
# now been downloaded:
#
#   Direction 1 — Difference-in-Differences across UCMR rounds
#                 (UCMR3 2013-2015  ->  UCMR5 2023-2025)
#   Direction 4 — Facility-level TRI PFAS emission-weighted exposure
#                 (TRI Basic Data Files 2022 + 2023)
#
# IMPORTANT CORRECTION to the plan in future_directions.qmd: UCMR4 (2018-2020)
# did NOT monitor any PFAS compounds (its panel was cyanotoxins, HAAs, metals,
# pesticides, and alcohols). The DiD is therefore a TWO-period panel built from
# the only two PFAS-monitoring rounds: UCMR3 and UCMR5.
#
# Directions 2 (hydrological flow paths) and 3 (state source-water monitoring)
# remain unimplemented: NHDPlus is an on-demand web service rather than a single
# downloadable file and lacks geocoded drinking-water intake points, and state
# pre-treatment monitoring has no unified national download.
#
# Data downloaded for this script:
#   PFAS_Project_Data/ucmr3/UCMR3_All.txt   (tab-delimited)
#   PFAS_Project_Data/tri/tri_2022_us.csv              (TRI national basic file)
#   PFAS_Project_Data/tri/tri_2023_us.csv
# =============================================================================

library(tidyverse)
library(data.table)
library(sf)
library(broom)

# Rebuild base objects: UCMR_5 (full UCMR5 table), echo_coords (PWSID lat/lon),
# Pfas_dataset_with_dates (facilities + START_DATE_FORMAT), df2_sf, analysis_data,
# idw_10, log_vars, covariate_cols.
source("clean_data_construction.R", chdir = TRUE)

out_dir <- "model_outputs"

# Local regression helpers (same conventions as the base script).
run_zipfe <- function(data, exposure, outcomes) {
  bind_rows(lapply(outcomes, function(var) {
    md <- data %>% select(all_of(c(var, exposure, "ZIPCODE"))) %>%
      filter(!is.na(.data[[var]]), !is.na(.data[[exposure]]), !is.na(ZIPCODE))
    if (nrow(md) < 10) return(NULL)
    fit <- lm(as.formula(paste0("`", var, "` ~ ", exposure, " + factor(ZIPCODE)")), data = md)
    bind_cols(tidy(fit) %>% filter(term == exposure) %>% mutate(outcome = var),
              glance(fit) %>% select(adj.r.squared, nobs))
  })) %>% select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs) %>%
    arrange(p.value)
}
run_baseline <- function(data, exposure, outcomes, covs) {
  cov_string <- paste(covs, collapse = " + ")
  bind_rows(lapply(outcomes, function(var) {
    md <- data %>% select(all_of(c(var, exposure, covs))) %>%
      filter(!is.na(.data[[var]]), !is.na(.data[[exposure]]))
    if (nrow(md) < 10) return(NULL)
    fit <- lm(as.formula(paste0("`", var, "` ~ ", exposure, " + ", cov_string)), data = md)
    bind_cols(tidy(fit) %>% filter(term == exposure) %>% mutate(outcome = var),
              glance(fit) %>% select(adj.r.squared, nobs))
  })) %>% select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs) %>%
    arrange(p.value)
}

# =============================================================================
# DIRECTION 1 — DIFFERENCE-IN-DIFFERENCES (UCMR3 -> UCMR5)
# -----------------------------------------------------------------------------
# Identification: each water system is its own control over time. A system FE
# absorbs all time-invariant geography; the treatment is the CHANGE in nearby
# industrial PFAS exposure between rounds, reconstructed from facility opening
# dates (FRS START_DATE). With two periods this reduces to a first-difference:
#
#     Delta(detect)_i = beta * Delta(IDW exposure)_i + e_i
#
# CRITICAL HARMONIZATION: UCMR3 minimum reporting levels are ~10x higher than
# UCMR5 (e.g., PFOS 0.04 vs ~0.004 ug/L). A naive levels comparison would show
# PFAS "rising" everywhere purely because UCMR5 can detect lower concentrations.
# We therefore define a DETECTION indicator at the *UCMR3* MRL applied to BOTH
# rounds, so the outcome is comparable across time.
# =============================================================================

# UCMR3 minimum reporting levels (ug/L) for the 6 PFAS common to UCMR3 & UCMR5.
ucmr3_mrl <- c(PFOA = 0.02, PFOS = 0.04, PFHxS = 0.03,
               PFNA = 0.02, PFBS = 0.09, PFHpA = 0.01)
thr_dt <- data.table(Contaminant = names(ucmr3_mrl), thr = as.numeric(ucmr3_mrl))

ucmr3 <- fread("PFAS_Project_Data/ucmr3/UCMR3_All.txt")

# Detection at the harmonized (UCMR3) threshold, max over each system's samples.
detect_round <- function(dt) {
  d <- merge(dt[Contaminant %in% names(ucmr3_mrl),
                .(PWSID, Contaminant, AnalyticalResultValue)],
             thr_dt, by = "Contaminant")
  d[, det := as.integer(!is.na(AnalyticalResultValue) & AnalyticalResultValue >= thr)]
  d[, .(detect = max(det)), by = .(PWSID, Contaminant)]
}

det_u3 <- detect_round(ucmr3)  %>% rename(detect3 = detect)
det_u5 <- detect_round(UCMR_5) %>% rename(detect5 = detect)

panel <- inner_join(det_u3, det_u5, by = c("PWSID", "Contaminant")) %>%
  mutate(delta_detect = detect5 - detect3)

cat(sprintf("\n[Direction 1] Panel: %d system x compound pairs across %d systems and %d compounds.\n",
            nrow(panel), n_distinct(panel$PWSID), n_distinct(panel$Contaminant)))

# Reconstruct nearby industrial exposure at each round from facility open dates.
panel_sys <- panel %>% distinct(PWSID) %>%
  inner_join(as_tibble(echo_coords), by = "PWSID") %>%
  filter(!is.na(lon), !is.na(lat))

panel_sf <- st_as_sf(panel_sys, coords = c("lon", "lat"), crs = 4326)
fac_sf    <- st_as_sf(Pfas_dataset_with_dates, coords = c("LONGITUDE83", "LATITUDE83"), crs = 4326)
fac_dates <- Pfas_dataset_with_dates$START_DATE_FORMAT

Dpanel <- st_distance(panel_sf, fac_sf)   # n_systems x n_facilities (meters)

# IDW over the 10 nearest facilities that EXISTED as of a given cutoff date.
idw_asof <- function(distrow, cutoff, lambda = 10000) {
  keep <- !is.na(fac_dates) & fac_dates < cutoff
  if (!any(keep)) return(0)
  d <- sort(as.numeric(distrow[keep]))[seq_len(min(10L, sum(keep)))]
  sum(exp(-d / lambda))
}

panel_sys <- panel_sys %>%
  mutate(
    idw_2015     = apply(Dpanel, 1, idw_asof, cutoff = as.Date("2016-01-01")),
    idw_2023     = apply(Dpanel, 1, idw_asof, cutoff = as.Date("2023-01-01")),
    delta_idw    = idw_2023 - idw_2015,
    # Binary: a new facility opened within 10 km during the inter-round gap.
    new_fac_10km = apply(Dpanel, 1, function(dr)
      as.integer(any(dr <= 10000 & !is.na(fac_dates) &
                     fac_dates >= as.Date("2016-01-01") &
                     fac_dates <  as.Date("2023-01-01"))))
  )

cat(sprintf("[Direction 1] Systems with coords: %d | mean delta_IDW = %.3f | got a new facility within 10km: %d\n",
            nrow(panel_sys), mean(panel_sys$delta_idw), sum(panel_sys$new_fac_10km)))

panel_did <- panel %>% inner_join(panel_sys, by = "PWSID")

# First-difference DiD per compound: change in detection ~ change in exposure.
did_by_compound <- bind_rows(lapply(names(ucmr3_mrl), function(cmp) {
  md <- panel_did %>% filter(Contaminant == cmp)
  if (nrow(md) < 20) return(NULL)
  fit <- lm(delta_detect ~ delta_idw, data = md)
  tidy(fit) %>% filter(term == "delta_idw") %>%
    transmute(compound = cmp, estimate, std.error, statistic, p.value,
              n = nrow(md),
              base_detect_rate_u3 = mean(md$detect3),
              new_detect_share    = mean(md$delta_detect == 1))
})) %>% arrange(p.value)

# Pooled DiD with compound fixed effects (continuous treatment = delta IDW).
fit_pool_idw <- lm(delta_detect ~ delta_idw + factor(Contaminant), data = panel_did)
fit_pool_bin <- lm(delta_detect ~ new_fac_10km + factor(Contaminant), data = panel_did)

did_pooled <- bind_rows(
  tidy(fit_pool_idw) %>% filter(term == "delta_idw")    %>% mutate(model = "pooled: delta IDW exposure"),
  tidy(fit_pool_bin) %>% filter(term == "new_fac_10km") %>% mutate(model = "pooled: new facility <=10km (binary)")
) %>% transmute(model, term, estimate, std.error, statistic, p.value, n = nrow(panel_did))

write.csv(did_by_compound, file.path(out_dir, "results_did_by_compound.csv"), row.names = FALSE)
write.csv(did_pooled,      file.path(out_dir, "results_did_pooled.csv"),      row.names = FALSE)

cat("\n[Direction 1] DiD by compound (Delta detection ~ Delta IDW exposure):\n")
print(did_by_compound)
cat("\n[Direction 1] Pooled DiD (compound FE):\n")
print(did_pooled)

# =============================================================================
# DIRECTION 4 — TRI PFAS EMISSION-WEIGHTED EXPOSURE
# -----------------------------------------------------------------------------
# Replaces the binary "PFAS-adjacent facility" indicator with actual reported
# PFAS release mass. TRI began requiring PFAS reporting in 2020. We build an
# emission-weighted exposure index for each UCMR5 system:
#
#     TRI_IDW_i = sum_j  release_lbs_j * exp(-d_ij / lambda)
#
# and regress log(PFAS) on it. Because reported release mass spans many orders
# of magnitude and TRI PFAS reporting is still sparse, we also use a z-scored
# version of the index for interpretable per-SD coefficients.
# =============================================================================

read_tri_pfas <- function(path) {
  x <- fread(path, select = c(3, 12, 13, 37, 107))
  setnames(x, c("frs", "lat", "lon", "chemical", "total_releases"))
  # True PFAS: per/poly-fluoroalkyl names. Excludes HF, CFCs/HCFCs/Halons,
  # fluorine gas, and fluorinated pesticides/pharmaceuticals.
  pfas_rx <- regex(
    "perfluoro|polyfluoro|fluorotelomer|hexafluoropropylene oxide|heptadecafluoro|difluoromethylene|fluorooctyl|fluorobutane",
    ignore_case = TRUE
  )
  x[str_detect(chemical, pfas_rx) & !is.na(lat) & !is.na(lon)]
}

tri <- rbindlist(list(read_tri_pfas("PFAS_Project_Data/tri/tri_2022_us.csv"),
                      read_tri_pfas("PFAS_Project_Data/tri/tri_2023_us.csv")))

# One row per facility: mean total PFAS release across the reported years.
tri_fac <- tri[, .(total_releases = mean(total_releases, na.rm = TRUE)),
               by = .(frs, lat, lon)][total_releases >= 0]

cat(sprintf("\n[Direction 4] TRI PFAS point sources: %d facility records -> %d unique facilities with coords.\n",
            nrow(tri), nrow(tri_fac)))
cat("[Direction 4] PFAS chemicals matched in TRI:\n")
print(sort(unique(tri$chemical)))

tri_sf <- st_as_sf(tri_fac, coords = c("lon", "lat"), crs = 4326)
Dtri   <- st_distance(df2_sf, tri_sf)   # n_analysis x n_tri (meters)

emis_idw <- function(distrow, rel, lambda = 10000) {
  sum(rel * exp(-as.numeric(distrow) / lambda), na.rm = TRUE)
}

analysis_data <- analysis_data %>%
  mutate(
    tri_emission_idw   = apply(Dtri, 1, emis_idw, rel = tri_fac$total_releases),
    tri_emission_idw_z = as.numeric(scale(tri_emission_idw)),
    nearest_tri_m      = apply(Dtri, 1, function(dr) min(as.numeric(dr)))
  )

cat(sprintf("[Direction 4] Mean TRI emission IDW = %.1f | median nearest TRI PFAS facility = %.1f km\n",
            mean(analysis_data$tri_emission_idw),
            median(analysis_data$nearest_tri_m) / 1000))

# Regressions: log(PFAS) on TRI emission exposure, baseline (treatment covs) and ZCTA FE.
results_tri_baseline <- run_baseline(analysis_data, "tri_emission_idw_z", log_vars, covariate_cols)
results_tri_zipfe    <- run_zipfe(analysis_data,    "tri_emission_idw_z", log_vars)

write.csv(results_tri_baseline, file.path(out_dir, "results_tri_baseline.csv"), row.names = FALSE)
write.csv(results_tri_zipfe,    file.path(out_dir, "results_tri_zipfe.csv"),    row.names = FALSE)

cat("\n[Direction 4] TRI emission exposure, baseline (treatment covariates):\n")
print(results_tri_baseline)
cat("\n[Direction 4] TRI emission exposure, ZCTA fixed effects:\n")
print(results_tri_zipfe)

# =============================================================================
cat("\n=============================================================\n")
cat("analysis_external_data.R complete. New outputs in model_outputs/:\n")
cat(" - results_did_by_compound.csv\n")
cat(" - results_did_pooled.csv\n")
cat(" - results_tri_baseline.csv\n")
cat(" - results_tri_zipfe.csv\n")
cat("=============================================================\n")
