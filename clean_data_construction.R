library(tidyverse)
library(data.table)
library(sf)
library(broom)
library(tigris)

# =============================================================================
# UCMR5 water sampling data
# =============================================================================

UCMR_5          <- fread("~/Downloads/ucmr5-occurrence-data/UCMR5_All.csv")
UCMR_5_locations <- fread("~/Downloads/ucmr5-occurrence-data/UCMR5_ZIPCodes.txt")
UCMR_5_other_data <- fread("~/Downloads/ucmr5-occurrence-data/UCMR5_AddtlDataElem.txt")

# Build wide covariate indicators (one row per PWSID)
dt <- as.data.table(UCMR_5_other_data)
dt[, varname := fifelse(
  AdditionalDataElement == "DisinfectantType",       paste0("disinf_",      Response),
  fifelse(AdditionalDataElement == "TreatmentInformation",   paste0("treat_",       Response),
  fifelse(AdditionalDataElement == "LithiumTreatment",       paste0("lith_treat_",  Response),
  fifelse(AdditionalDataElement == "PFASTreatment",          paste0("pfas_treat_",  Response),
  fifelse(AdditionalDataElement == "PotentialPFASSourcesDetail", paste0("pfas_source_", Response),
  fifelse(AdditionalDataElement == "LithiumOccurrence",      paste0("lithium_occ_", Response),
  fifelse(AdditionalDataElement == "PFASOccurrence",         paste0("pfas_occ_",    Response),
  fifelse(AdditionalDataElement == "PotentialPFASSources",   paste0("pfas_sources_",Response),
    NA_character_))))))))]
dt <- dt[!is.na(varname)]
dt[, value := 1]

UCMR5_covariates <- dcast(dt, PWSID ~ varname, value.var = "value",
                          fun.aggregate = max, fill = 0)

# Zip codes per PWSID — kept for ZIPCODE fixed effects, not for coordinates
UCMR_5_zipcodes <- UCMR_5_locations %>%
  mutate(ZIPCODE = sprintf("%05s", ZIPCODE)) %>%
  select(PWSID, ZIPCODE)

# Mean PFAS measurement per testing location
UCMR5_wide <- UCMR_5 %>%
  pivot_wider(
    id_cols   = c(PWSName, PWSID, FacilityID),
    names_from  = Contaminant,
    values_from = AnalyticalResultValue,
    values_fn   = mean
  )

# =============================================================================
# EPA ECHO exact coordinates for public water systems
# Source: https://echo.epa.gov/tools/data-downloads (ECHO_EXPORTER.csv)
# FAC_LAT / FAC_LONG are FRS NAD83 geocoded points — one per PWS facility.
# SDWA_IDS can contain multiple space-separated PWSIDs for multi-permit sites.
# =============================================================================

echo_raw <- fread(
  "~/Downloads/epa_pws_coordinates/ECHO_EXPORTER.csv",
  select = c("FAC_LAT", "FAC_LONG", "SDWA_IDS")
)
echo_raw <- echo_raw[SDWA_IDS != "" & !is.na(FAC_LAT) & !is.na(FAC_LONG)]

# Expand multi-PWSID rows so every PWSID gets its own coordinate row
echo_coords <- echo_raw[
  , .(PWSID = unlist(strsplit(trimws(SDWA_IDS), "\\s+"))),
  by = .(FAC_LAT, FAC_LONG)
][PWSID != ""][
  , .(lat = first(FAC_LAT), lon = first(FAC_LONG)), by = PWSID
]

# Spatially derive one ZIP code per water system from its ECHO coordinate.
# Uses ZCTA boundaries so the ZIP reflects the system's actual physical location,
# not its service-area coverage (which caused row duplication).
options(tigris_use_cache = TRUE)
zctas_sf     <- zctas(year = 2020, cb = TRUE) %>% st_transform(crs = 4326)
echo_sf_zip  <- st_as_sf(as_tibble(echo_coords), coords = c("lon", "lat"), crs = 4326)
echo_with_zip <- st_join(echo_sf_zip, zctas_sf %>% select(ZIPCODE = ZCTA5CE20), left = TRUE) %>%
  st_drop_geometry() %>%
  distinct(PWSID, .keep_all = TRUE)

# UCMR5 locations with at least one PFAS sample, joined to exact coords and covariates
UCMR5_wide_filter_multiple_zip_codes <- UCMR5_wide %>%
  filter(!if_all(-c(PWSName, PWSID, FacilityID, lithium), is.na)) %>%
  left_join(as_tibble(echo_coords), by = "PWSID") %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  left_join(UCMR5_covariates, by = "PWSID") %>%
  left_join(echo_with_zip %>% select(PWSID, ZIPCODE), by = "PWSID") %>%
  distinct(PWSID, .keep_all = TRUE)

# =============================================================================
# Manufacturing site locations
# =============================================================================

facilities    <- fread("~/Downloads/national_combined/NATIONAL_FACILITY_FILE.CSV")
naics_codes   <- fread("~/Downloads/national_combined/NATIONAL_NAICS_FILE.CSV")
enviornmental <- fread("~/Downloads/national_combined/NATIONAL_ENVIRONMENTAL_INTEREST_FILE.CSV")

enviornmental1 <- enviornmental %>%
  filter(str_length(START_DATE) > 5, !is.na(START_DATE))

pfas_naics_codes <- c(
  "313320", "325510", "322220", "313210", "322121", "332813", "324110",
  "325612", "334413", "326113", "332812", "333318", "334419", "562212",
  "325199", "323111", "313110", "314110", "316110", "325211", "324191",
  "325998", "562211", "562213", "313310", "322219", "323120", "313220",
  "313230", "322130", "332999", "424690", "314910", "326112", "335999",
  "562112", "562219", "325611", "488119"
)

naics_pfas <- naics_codes %>% filter(NAICS_CODE %in% pfas_naics_codes)

pfas_facilities <- facilities %>%
  inner_join(naics_pfas, by = "REGISTRY_ID") %>%
  left_join(
    enviornmental1 %>% select(REGISTRY_ID, START_DATE, START_DATE_QUALIFIER),
    by = "REGISTRY_ID"
  )

pfas_dataset <- pfas_facilities %>%
  filter(!is.na(LATITUDE83), !is.na(LONGITUDE83), ACCURACY_VALUE <= 1000, !is.na(START_DATE)) %>%
  distinct(REGISTRY_ID, .keep_all = TRUE) %>%
  select(REGISTRY_ID, PRIMARY_NAME, LATITUDE83, LONGITUDE83, ACCURACY_VALUE,
         START_DATE, START_DATE_QUALIFIER, STATE_CODE, COUNTY_NAME, CITY_NAME,
         FIPS_CODE, NAICS_CODE)

pfas_final_with_and_without_start_date <- pfas_facilities %>%
  filter(!is.na(LATITUDE83), !is.na(LONGITUDE83), ACCURACY_VALUE <= 1000) %>%
  distinct(REGISTRY_ID, .keep_all = TRUE)

Pfas_dataset_with_dates <- pfas_dataset %>%
  mutate(START_DATE_FORMAT = as.Date(START_DATE, format = "%d-%b-%y"))

# =============================================================================
# Industrial water treatment locations
# =============================================================================

water_flow                <- read.csv("~/Downloads/2022CWNS_NATIONAL_APR2024/FLOW.csv")
water_treatment_locations <- read.csv("~/Downloads/2022CWNS_NATIONAL_APR2024/PHYSICAL_LOCATION.csv")

industrial_water_treatment_centers <- water_flow %>%
  filter(FLOW_TYPE == "Industrial Flow") %>%
  select(FACILITY_ID, FLOW_TYPE)

water_treatment_industrial_final_dataset <- water_treatment_locations %>%
  select(FACILITY_ID, LATITUDE, LONGITUDE) %>%
  inner_join(industrial_water_treatment_centers, by = "FACILITY_ID")

# =============================================================================
# Military fire site locations
# =============================================================================

dod   <- read.csv("pfas_progress_june_2021.csv")
mirta <- read.csv("~/Downloads/mirta_-223606765265040761.csv")

fireSite <- mirta %>%
  mutate(Installation_Name = Site.Name) %>%
  inner_join(dod, by = "Installation_Name") %>%
  select(Installation_Name, OBJECTID, x, y)

# =============================================================================
# Combined location dataset
# =============================================================================

Pfas_dataset_all <- bind_rows(
  pfas_final_with_and_without_start_date %>%
    transmute(ID = REGISTRY_ID, lat = LATITUDE83, lon = LONGITUDE83, type = "Manufacturing Locations"),
  water_treatment_industrial_final_dataset %>%
    transmute(ID = FACILITY_ID, lat = LATITUDE, lon = LONGITUDE, type = "Water Treatment Center"),
  fireSite %>%
    transmute(ID = OBJECTID, lat = x, lon = y, type = "Military Fire Sites")
)

# =============================================================================
# Distances from water systems to nearest manufacturing site
# =============================================================================

df1_sf <- st_as_sf(Pfas_dataset_with_dates,              coords = c("LONGITUDE83", "LATITUDE83"), crs = 4326)
df2_sf <- st_as_sf(UCMR5_wide_filter_multiple_zip_codes, coords = c("lon", "lat"),                crs = 4326)

dist_matrix  <- st_distance(df1_sf, df2_sf)   # N_mfg  × N_water
dist_matrix2 <- st_distance(df2_sf, df1_sf)   # N_water × N_mfg

Pfas_dataset_with_dates$nearest_distance_m              <- apply(dist_matrix,  1, min)
UCMR5_wide_filter_multiple_zip_codes$nearest_distance_m <- apply(dist_matrix2, 1, min)

# Distances to nearest pre-2002 manufacturing site
Pfas_dataset_with_dates_before_2002 <- Pfas_dataset_with_dates %>%
  filter(!is.na(START_DATE_FORMAT), START_DATE_FORMAT < as.Date("2002-01-01"))

df1_sf_2002      <- st_as_sf(Pfas_dataset_with_dates_before_2002, coords = c("LONGITUDE83", "LATITUDE83"), crs = 4326)
dist_matrix_2002 <- st_distance(df2_sf, df1_sf_2002)   # N_water × N_mfg_pre2002

UCMR5_wide_filter_multiple_zip_codes$nearest_distance_pre2002_m <- apply(dist_matrix_2002, 1, min)

# =============================================================================
# Analysis datasets
# =============================================================================

# Exponential inverse distance weight summed over the 10 closest facilities.
# lambda = 10 000 m (10 km) decay parameter; larger lambda = slower decay.
idw_10 <- function(dists, lambda = 10000) {
  d <- sort(as.numeric(dists))[seq_len(min(10L, length(dists)))]
  sum(exp(-d / lambda))
}

analysis_data <- UCMR5_wide_filter_multiple_zip_codes %>%
  mutate(across(
    -c(PWSName, PWSID, FacilityID, ZIPCODE, lat, lon),
    log,
    .names = "log_{.col}"
  )) %>%
  mutate(
    idw_exposure_10km        = apply(dist_matrix2,    1, idw_10),
    idw_pre2002_exposure_10km = apply(dist_matrix_2002, 1, idw_10)
  )

analysis_data_buffer <- analysis_data %>%
  mutate(
    km1  = as.integer(nearest_distance_m <= 1000),
    km5  = as.integer(nearest_distance_m <= 5000),
    km10 = as.integer(nearest_distance_m <= 10000),
    km25 = as.integer(nearest_distance_m <= 25000)
  )

# =============================================================================
# Econometric model result dataframes
# =============================================================================

# PFAS outcome variables (log-transformed) and treatment covariates
log_vars <- setdiff(
  grep("^log_", names(analysis_data), value = TRUE),
  grep("^log_(nearest|idw|km|log_|disinf_|treat_|pfas_|lithium|lith)", names(analysis_data), value = TRUE)
)

covariate_cols   <- grep("^(pfas_treat_|treat_|disinf_|pfas_occ_|pfas_source_|lithium_occ_|lith_treat_)",
                         names(analysis_data), value = TRUE)
covariate_string <- paste(covariate_cols, collapse = " + ")

# OLS: log(PFAS) ~ nearest_distance_m + treatment covariates
results <- bind_rows(lapply(log_vars, function(var) {
  md <- analysis_data %>%
    select(all_of(c(var, "nearest_distance_m", covariate_cols))) %>%
    filter(!is.na(.data[[var]]), !is.na(nearest_distance_m))
  if (nrow(md) < 10) return(NULL)
  fit <- lm(as.formula(paste0("`", var, "` ~ nearest_distance_m + ", covariate_string)), data = md)
  bind_cols(
    tidy(fit)   %>% filter(term == "nearest_distance_m") %>% mutate(outcome = var),
    glance(fit) %>% select(r.squared, adj.r.squared, nobs)
  )
})) %>%
  select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs) %>%
  arrange(p.value)

# OLS with ZIPCODE fixed effects: log(PFAS) ~ nearest_distance_m + factor(ZIPCODE)
results_fixed_effects <- bind_rows(lapply(log_vars, function(var) {
  md <- analysis_data %>%
    select(all_of(c(var, "nearest_distance_m", "ZIPCODE"))) %>%
    filter(!is.na(.data[[var]]), !is.na(nearest_distance_m), !is.na(ZIPCODE))
  if (nrow(md) < 10) return(NULL)
  fit <- lm(as.formula(paste0("`", var, "` ~ nearest_distance_m + factor(ZIPCODE)")), data = md)
  bind_cols(
    tidy(fit)   %>% filter(term == "nearest_distance_m") %>% mutate(outcome = var),
    glance(fit) %>% select(r.squared, adj.r.squared, nobs)
  )
})) %>%
  select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs) %>%
  mutate(estimate_in_km = round(1000 * estimate, 5)) %>%
  arrange(p.value)

# Buffer dummy models: log(PFAS) ~ km buffer indicator
km_vars      <- c("km1", "km5", "km10", "km25")
pfas_log_vars <- setdiff(
  grep("^log_", names(analysis_data_buffer), value = TRUE),
  grep("^log_(nearest|idw|km|log_|disinf_|treat_|pfas_|lithium|lith)", names(analysis_data_buffer), value = TRUE)
)

results_buffer <- bind_rows(lapply(pfas_log_vars, function(pfas) {
  bind_rows(lapply(km_vars, function(km) {
    md <- analysis_data_buffer %>%
      select(all_of(c(pfas, km))) %>%
      filter(!is.na(.data[[pfas]]), !is.na(.data[[km]]))
    if (nrow(md) < 5) return(NULL)
    fit <- lm(as.formula(paste0("`", pfas, "` ~ ", km)), data = md)
    tidy(fit) %>%
      filter(term == km) %>%
      mutate(PFAS = pfas, buffer = km) %>%
      select(PFAS, buffer, estimate, std.error, p.value)
  }))
}))

results_wide <- results_buffer %>%
  pivot_wider(names_from = buffer, values_from = estimate) %>%
  mutate(across(any_of(km_vars), ~ round(., 5)))

# IDW exposure models: log(PFAS) ~ idw_exposure_10km + treatment covariates
results_idw <- bind_rows(lapply(log_vars, function(var) {
  md <- analysis_data %>%
    select(all_of(c(var, "idw_exposure_10km", covariate_cols))) %>%
    filter(!is.na(.data[[var]]), !is.na(idw_exposure_10km))
  if (nrow(md) < 10) return(NULL)
  fit <- lm(as.formula(paste0("`", var, "` ~ idw_exposure_10km + ", covariate_string)), data = md)
  bind_cols(
    tidy(fit)   %>% filter(term == "idw_exposure_10km") %>% mutate(outcome = var),
    glance(fit) %>% select(r.squared, adj.r.squared, nobs)
  )
})) %>%
  select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs) %>%
  arrange(p.value)

# Pre-2002 manufacturing distance with ZIPCODE fixed effects
results_pre2002 <- bind_rows(lapply(log_vars, function(var) {
  md <- analysis_data %>%
    select(all_of(c(var, "nearest_distance_pre2002_m", "ZIPCODE"))) %>%
    filter(!is.na(.data[[var]]), !is.na(nearest_distance_pre2002_m), !is.na(ZIPCODE))
  if (nrow(md) < 10) return(NULL)
  fit <- lm(as.formula(paste0("`", var, "` ~ nearest_distance_pre2002_m + factor(ZIPCODE)")), data = md)
  bind_cols(
    tidy(fit)   %>% filter(term == "nearest_distance_pre2002_m") %>% mutate(outcome = var),
    glance(fit) %>% select(r.squared, adj.r.squared, nobs)
  )
})) %>%
  select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs) %>%
  mutate(estimate_in_km = round(1000 * estimate, 5)) %>%
  arrange(p.value)

# IDW with ZIPCODE fixed effects: log(PFAS) ~ idw_exposure_10km + factor(ZIPCODE)
results_idw_fixed_effects <- bind_rows(lapply(log_vars, function(var) {
  md <- analysis_data %>%
    select(all_of(c(var, "idw_exposure_10km", "ZIPCODE"))) %>%
    filter(!is.na(.data[[var]]), !is.na(idw_exposure_10km), !is.na(ZIPCODE))
  if (nrow(md) < 10) return(NULL)
  fit <- lm(as.formula(paste0("`", var, "` ~ idw_exposure_10km + factor(ZIPCODE)")), data = md)
  bind_cols(
    tidy(fit)   %>% filter(term == "idw_exposure_10km") %>% mutate(outcome = var),
    glance(fit) %>% select(r.squared, adj.r.squared, nobs)
  )
})) %>%
  select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs) %>%
  arrange(p.value)

# Pre-2002 IDW with ZIPCODE fixed effects: log(PFAS) ~ idw_pre2002_exposure_10km + factor(ZIPCODE)
results_idw_pre2002 <- bind_rows(lapply(log_vars, function(var) {
  md <- analysis_data %>%
    select(all_of(c(var, "idw_pre2002_exposure_10km", "ZIPCODE"))) %>%
    filter(!is.na(.data[[var]]), !is.na(idw_pre2002_exposure_10km), !is.na(ZIPCODE))
  if (nrow(md) < 10) return(NULL)
  fit <- lm(as.formula(paste0("`", var, "` ~ idw_pre2002_exposure_10km + factor(ZIPCODE)")), data = md)
  bind_cols(
    tidy(fit)   %>% filter(term == "idw_pre2002_exposure_10km") %>% mutate(outcome = var),
    glance(fit) %>% select(r.squared, adj.r.squared, nobs)
  )
})) %>%
  select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs) %>%
  arrange(p.value)

# =============================================================================
# Export all model result tables to CSV
# =============================================================================

out_dir <- "~/Downloads/BBR/model_outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(results,                   file.path(out_dir, "results_ols_baseline.csv"),          row.names = FALSE)
write.csv(results_fixed_effects,     file.path(out_dir, "results_ols_zipfe.csv"),              row.names = FALSE)
write.csv(results_buffer,            file.path(out_dir, "results_buffer.csv"),                 row.names = FALSE)
write.csv(results_wide,              file.path(out_dir, "results_buffer_wide.csv"),            row.names = FALSE)
write.csv(results_idw,               file.path(out_dir, "results_idw_baseline.csv"),           row.names = FALSE)
write.csv(results_pre2002,           file.path(out_dir, "results_pre2002_zipfe.csv"),          row.names = FALSE)
write.csv(results_idw_fixed_effects, file.path(out_dir, "results_idw_zipfe.csv"),              row.names = FALSE)
write.csv(results_idw_pre2002,       file.path(out_dir, "results_idw_pre2002_zipfe.csv"),      row.names = FALSE)

# Summary of water system sample
sample_summary <- UCMR5_wide_filter_multiple_zip_codes %>%
  summarise(
    n_systems          = n_distinct(PWSID),
    n_observations     = n(),
    mean_dist_km       = mean(nearest_distance_m / 1000, na.rm = TRUE),
    median_dist_km     = median(nearest_distance_m / 1000, na.rm = TRUE),
    p25_dist_km        = quantile(nearest_distance_m / 1000, 0.25, na.rm = TRUE),
    p75_dist_km        = quantile(nearest_distance_m / 1000, 0.75, na.rm = TRUE),
    min_dist_km        = min(nearest_distance_m / 1000, na.rm = TRUE),
    max_dist_km        = max(nearest_distance_m / 1000, na.rm = TRUE),
    mean_idw           = mean(analysis_data$idw_exposure_10km, na.rm = TRUE),
    n_within_1km       = sum(nearest_distance_m <= 1000,  na.rm = TRUE),
    n_within_5km       = sum(nearest_distance_m <= 5000,  na.rm = TRUE),
    n_within_10km      = sum(nearest_distance_m <= 10000, na.rm = TRUE),
    n_within_25km      = sum(nearest_distance_m <= 25000, na.rm = TRUE)
  )
write.csv(sample_summary, file.path(out_dir, "sample_summary.csv"), row.names = FALSE)

# Detection rates and mean concentrations per PFAS compound
pfas_raw_cols <- gsub("^log_", "", log_vars)
pfas_summary <- bind_rows(lapply(pfas_raw_cols, function(v) {
  if (!v %in% names(UCMR5_wide_filter_multiple_zip_codes)) return(NULL)
  x <- UCMR5_wide_filter_multiple_zip_codes[[v]]
  tibble(
    compound        = v,
    n_detected      = sum(!is.na(x)),
    mean_ug_L       = mean(x, na.rm = TRUE),
    median_ug_L     = median(x, na.rm = TRUE),
    p95_ug_L        = quantile(x, 0.95, na.rm = TRUE),
    max_ug_L        = max(x, na.rm = TRUE),
    pct_above_4ppt  = mean(x > 0.000004, na.rm = TRUE) * 100
  )
}))
write.csv(pfas_summary, file.path(out_dir, "pfas_compound_summary.csv"), row.names = FALSE)

cat("\nAll model outputs written to:", path.expand(out_dir), "\n")
cat("Files written:\n")
cat(paste(" -", list.files(path.expand(out_dir))), sep = "\n")
