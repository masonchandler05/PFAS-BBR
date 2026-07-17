# =============================================================================
# analysis_improvements.R
# -----------------------------------------------------------------------------
# Implements the "no new data required" extensions from future_directions.qmd
# to improve on the cross-sectional analysis in clean_data_construction.R:
#
#   Direction 5 — Post-2002 facility index  (temporal source stratification)
#   Direction 7 — Compound-specific NAICS exposure (reduce exposure noise)
#   Direction 6 — Spatial regression / spillovers (correct spatial dependence)
#
# Directions 1 (DiD), 2 (hydrology), 3 (source water) and 4 (TRI emissions)
# are NOT implemented here because they require external data that is not
# present locally (UCMR3/UCMR4, NHDPlus + intake locations, state source-water
# monitoring, and the TRI PFAS release database, respectively).
#
# This script rebuilds the base objects by sourcing clean_data_construction.R,
# then adds the three extensions and writes their outputs to model_outputs/.
# =============================================================================

library(tidyverse)
library(sf)
library(broom)

# -----------------------------------------------------------------------------
# Rebuild the base analysis objects (analysis_data, df2_sf, Pfas_dataset_with_dates,
# idw_10, log_vars, covariate_cols, ...). This re-runs the full pipeline so the
# extensions below operate on exactly the same 2,032-observation sample.
# -----------------------------------------------------------------------------
source("clean_data_construction.R", chdir = TRUE)

out_dir <- "model_outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# Shared regression helpers (mirror the conventions in the base script)
# -----------------------------------------------------------------------------

# ZCTA fixed-effects regression of each log-PFAS outcome on one exposure column.
run_zipfe <- function(data, exposure, outcomes) {
  bind_rows(lapply(outcomes, function(var) {
    md <- data %>%
      select(all_of(c(var, exposure, "ZIPCODE"))) %>%
      filter(!is.na(.data[[var]]), !is.na(.data[[exposure]]), !is.na(ZIPCODE))
    if (nrow(md) < 10) return(NULL)
    fit <- lm(as.formula(paste0("`", var, "` ~ ", exposure, " + factor(ZIPCODE)")),
              data = md)
    bind_cols(
      tidy(fit)   %>% filter(term == exposure) %>% mutate(outcome = var),
      glance(fit) %>% select(adj.r.squared, nobs)
    )
  })) %>%
    select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs) %>%
    arrange(p.value)
}

# Baseline regression with treatment covariates (no fixed effects).
run_baseline <- function(data, exposure, outcomes, covs) {
  cov_string <- paste(covs, collapse = " + ")
  bind_rows(lapply(outcomes, function(var) {
    md <- data %>%
      select(all_of(c(var, exposure, covs))) %>%
      filter(!is.na(.data[[var]]), !is.na(.data[[exposure]]))
    if (nrow(md) < 10) return(NULL)
    fit <- lm(as.formula(paste0("`", var, "` ~ ", exposure, " + ", cov_string)),
              data = md)
    bind_cols(
      tidy(fit)   %>% filter(term == exposure) %>% mutate(outcome = var),
      glance(fit) %>% select(adj.r.squared, nobs)
    )
  })) %>%
    select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs) %>%
    arrange(p.value)
}

# =============================================================================
# DIRECTION 5 — POST-2002 FACILITY INDEX
# -----------------------------------------------------------------------------
# The base script already builds a *pre*-2002 distance matrix and IDW index to
# capture the legacy (PFOS/PFOA) manufacturing footprint. Here we build the
# complementary *post*-2002 index to capture the contemporary replacement-
# chemistry footprint (short-chain PFPeA, PFHxA, PFBS). If the source-
# substitution interpretation is correct, post-2002 exposure should predict the
# short-chain compounds that the pre-2002 measure does not.
# =============================================================================

Pfas_dataset_post2002 <- Pfas_dataset_with_dates %>%
  filter(!is.na(START_DATE_FORMAT), START_DATE_FORMAT >= as.Date("2002-01-01"))

cat("\n[Direction 5] Pre-2002 facilities: ",
    sum(Pfas_dataset_with_dates$START_DATE_FORMAT <  as.Date("2002-01-01"), na.rm = TRUE),
    " | Post-2002 facilities: ", nrow(Pfas_dataset_post2002), "\n", sep = "")

df1_sf_post2002      <- st_as_sf(Pfas_dataset_post2002,
                                 coords = c("LONGITUDE83", "LATITUDE83"), crs = 4326)
dist_matrix_post2002 <- st_distance(df2_sf, df1_sf_post2002)   # N_water × N_mfg_post2002

# Attach post-2002 nearest distance and IDW to the analysis frame (row order of
# df2_sf matches analysis_data, both derived from UCMR5_wide_filter_multiple_zip_codes).
analysis_data <- analysis_data %>%
  mutate(
    nearest_distance_post2002_m = apply(dist_matrix_post2002, 1, min),
    idw_post2002_exposure_10km  = apply(dist_matrix_post2002, 1, idw_10)
  )

# Post-2002 models (mirror the pre-2002 specifications for direct comparison)
results_post2002_dist_zipfe <- run_zipfe(analysis_data, "nearest_distance_post2002_m", log_vars) %>%
  mutate(estimate_in_km = round(1000 * estimate, 5))
results_idw_post2002_zipfe  <- run_zipfe(analysis_data, "idw_post2002_exposure_10km", log_vars)

# Side-by-side pre vs post comparison of the IDW ZCTA-FE coefficient per compound.
pre_vs_post_idw <- results_idw_pre2002 %>%
  select(outcome, pre2002_est = estimate, pre2002_p = p.value, nobs) %>%
  full_join(
    results_idw_post2002_zipfe %>% select(outcome, post2002_est = estimate, post2002_p = p.value),
    by = "outcome"
  ) %>%
  mutate(
    pre2002_sig  = ifelse(pre2002_p  < 0.05, "*", ""),
    post2002_sig = ifelse(post2002_p < 0.05, "*", "")
  ) %>%
  arrange(post2002_p)

write.csv(results_post2002_dist_zipfe,
          file.path(out_dir, "results_post2002_dist_zipfe.csv"), row.names = FALSE)
write.csv(results_idw_post2002_zipfe,
          file.path(out_dir, "results_idw_post2002_zipfe.csv"),  row.names = FALSE)
write.csv(pre_vs_post_idw,
          file.path(out_dir, "results_pre_vs_post2002_idw.csv"), row.names = FALSE)

cat("\n[Direction 5] Pre- vs Post-2002 IDW (ZCTA FE) coefficients:\n")
print(pre_vs_post_idw, n = 30)

# =============================================================================
# DIRECTION 7 — COMPOUND-SPECIFIC NAICS EXPOSURE
# -----------------------------------------------------------------------------
# The base exposure measures pool all 39 PFAS-adjacent NAICS codes as if every
# industry emits every compound. Different industries emit different compound
# profiles. Here each compound is regressed on an exposure index built ONLY from
# the NAICS codes documented to use that compound's chemistry, sharpening the
# signal-to-noise ratio. Mapping follows future_directions.qmd Section 7.
# =============================================================================

compound_naics <- list(
  PFOS  = c("332812", "332813", "334413", "488119"),  # electroplating, airports/AFFF
  PFOA  = c("313320", "325510", "325612"),            # textile coating, paint/varnish
  PFHxA = c("334413", "334419"),                       # semiconductor manufacturing
  PFPeA = c("334413", "334419"),                       # semiconductor manufacturing
  PFBS  = c("325199", "325998"),                       # industrial / replacement chemicals
  PFBA  = c("322220", "323111")                        # food packaging, printing
)

# Build one compound-specific exposure column per compound, then regress that
# compound's log outcome on its own exposure (nearest distance + IDW, ZCTA FE).
cs_rows <- list()
for (cmp in names(compound_naics)) {
  outcome <- paste0("log_", cmp)
  if (!outcome %in% log_vars) next

  fac_sub <- Pfas_dataset_with_dates %>% filter(NAICS_CODE %in% compound_naics[[cmp]])
  if (nrow(fac_sub) < 5) {
    cat(sprintf("[Direction 7] %s: only %d facilities for its NAICS subset — skipped.\n",
                cmp, nrow(fac_sub)))
    next
  }

  fac_sf <- st_as_sf(fac_sub, coords = c("LONGITUDE83", "LATITUDE83"), crs = 4326)
  dmat   <- st_distance(df2_sf, fac_sf)

  tmp <- analysis_data %>%
    mutate(
      cs_nearest = apply(dmat, 1, min),
      cs_idw     = apply(dmat, 1, idw_10)
    )

  dist_fit <- run_zipfe(tmp, "cs_nearest", outcome)
  idw_fit  <- run_zipfe(tmp, "cs_idw",     outcome)

  cs_rows[[cmp]] <- tibble(
    compound       = cmp,
    n_facilities   = nrow(fac_sub),
    naics_used     = paste(compound_naics[[cmp]], collapse = ";"),
    dist_estimate  = if (nrow(dist_fit)) dist_fit$estimate[1] else NA_real_,
    dist_p         = if (nrow(dist_fit)) dist_fit$p.value[1]  else NA_real_,
    idw_estimate   = if (nrow(idw_fit))  idw_fit$estimate[1]  else NA_real_,
    idw_p          = if (nrow(idw_fit))  idw_fit$p.value[1]   else NA_real_,
    nobs           = if (nrow(idw_fit))  idw_fit$nobs[1]      else NA_integer_
  )
}

results_compound_specific <- bind_rows(cs_rows) %>%
  mutate(
    dist_supports = !is.na(dist_p) & dist_p < 0.05 & dist_estimate < 0,  # closer = higher
    idw_supports  = !is.na(idw_p)  & idw_p  < 0.05 & idw_estimate  > 0   # more exposure = higher
  ) %>%
  arrange(idw_p)

write.csv(results_compound_specific,
          file.path(out_dir, "results_compound_specific_naics.csv"), row.names = FALSE)

cat("\n[Direction 7] Compound-specific NAICS exposure (ZCTA FE):\n")
print(results_compound_specific, n = 30)

# =============================================================================
# DIRECTION 6 — SPATIAL REGRESSION AND GEOGRAPHIC SPILLOVERS
# -----------------------------------------------------------------------------
# Standard OLS assumes independent observations; PFAS is spatially correlated.
# We (1) test for spatial autocorrelation in log(PFOS) with Moran's I, and
# (2) estimate a spatial lag (SAR) and spatial error (SEM) model. PFOS is used
# as the focal outcome because it is the compound with the clearest proximity
# signal in the base analysis. Runs only if spdep/spatialreg are installed.
# =============================================================================

have_spatial <- requireNamespace("spdep", quietly = TRUE) &&
                requireNamespace("spatialreg", quietly = TRUE)

if (!have_spatial) {
  cat("\n[Direction 6] spdep/spatialreg not installed — spatial models skipped.\n",
      "  Install with: install.packages(c('spdep','spatialreg'))\n", sep = "")
} else {
  suppressPackageStartupMessages({
    library(spdep); library(spatialreg)
  })

  # One row per PFOS observation with coordinates. De-duplicate exactly co-located
  # points (multiple FacilityIDs share PWSID coordinates) by jittering is avoided;
  # instead we collapse to unique coordinate points to build a valid neighbour graph.
  spat <- analysis_data %>%
    filter(!is.na(log_PFOS), !is.na(lon), !is.na(lat)) %>%
    group_by(lon, lat) %>%
    summarise(
      log_PFOS           = mean(log_PFOS, na.rm = TRUE),
      nearest_distance_m = first(nearest_distance_m),
      idw_exposure_10km  = first(idw_exposure_10km),
      .groups = "drop"
    )

  cat(sprintf("\n[Direction 6] Spatial sample: %d unique PFOS coordinate points.\n",
              nrow(spat)))

  spat_sf <- st_as_sf(spat, coords = c("lon", "lat"), crs = 4326)
  coords  <- st_coordinates(spat_sf)

  # k-nearest-neighbour graph (k = 5) → row-standardised spatial weights.
  knn   <- knearneigh(coords, k = 5, longlat = TRUE)
  nb    <- knn2nb(knn)
  listw <- nb2listw(nb, style = "W")

  # (1) Moran's I — is log(PFOS) spatially autocorrelated?
  moran <- moran.test(spat$log_PFOS, listw, zero.policy = TRUE)
  cat("\n[Direction 6] Moran's I for log(PFOS):\n")
  print(moran)

  # (2) Spatial lag (SAR) and spatial error (SEM) models with the distance and
  #     IDW exposure measures as regressors.
  f <- log_PFOS ~ nearest_distance_m + idw_exposure_10km

  ols_sp <- lm(f, data = spat)
  sar    <- spatialreg::lagsarlm(f, data = spat, listw = listw, zero.policy = TRUE)
  sem    <- spatialreg::errorsarlm(f, data = spat, listw = listw, zero.policy = TRUE)

  cat("\n[Direction 6] OLS (non-spatial) for reference:\n"); print(summary(ols_sp))
  cat("\n[Direction 6] Spatial Lag (SAR):\n");                print(summary(sar))
  cat("\n[Direction 6] Spatial Error (SEM):\n");              print(summary(sem))

  spatial_summary <- tibble(
    model      = c("OLS", "SAR (spatial lag)", "SEM (spatial error)"),
    idw_coef   = c(coef(ols_sp)[["idw_exposure_10km"]],
                   coef(sar)[["idw_exposure_10km"]],
                   coef(sem)[["idw_exposure_10km"]]),
    dist_coef  = c(coef(ols_sp)[["nearest_distance_m"]],
                   coef(sar)[["nearest_distance_m"]],
                   coef(sem)[["nearest_distance_m"]]),
    rho_lambda = c(NA, sar$rho, sem$lambda),
    AIC        = c(AIC(ols_sp), AIC(sar), AIC(sem)),
    moran_I    = c(moran$estimate[["Moran I statistic"]], NA, NA),
    moran_p    = c(moran$p.value, NA, NA)
  )
  write.csv(spatial_summary,
            file.path(out_dir, "results_spatial_pfos.csv"), row.names = FALSE)

  cat("\n[Direction 6] Spatial model comparison (focal outcome: log PFOS):\n")
  print(spatial_summary)
}

# =============================================================================
cat("\n=============================================================\n")
cat("analysis_improvements.R complete. New outputs in model_outputs/:\n")
cat(" - results_post2002_dist_zipfe.csv\n")
cat(" - results_idw_post2002_zipfe.csv\n")
cat(" - results_pre_vs_post2002_idw.csv\n")
cat(" - results_compound_specific_naics.csv\n")
if (have_spatial) cat(" - results_spatial_pfos.csv\n")
cat("=============================================================\n")
