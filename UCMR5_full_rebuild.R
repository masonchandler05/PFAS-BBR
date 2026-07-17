# =============================================================================
# UCMR5_full_rebuild.R
# -----------------------------------------------------------------------------
# Rebuild the location-level analysis dataset from the COMPLETE UCMR5 release
# (Jan 2026, 1.93M results; PFAS_Project_Data/ucmr5_full/) fixing two problems
# documented in ISSUES_AND_SAMPLING_OPTIONS.md:
#   1.9  the old UCMR5_All.csv was Excel-truncated at 1,048,575 rows
#   1.8  the old sample kept only locations with >=1 PFAS detection
#
# Output: one location-level table covering ALL PFAS-tested locations, with:
#   det_<compound>   1 = detected at least once at this location, 0 = never
#                    (NA = this location never sampled that compound)
#   conc_<compound>  mean of DETECTED sample values (NA if never detected)
#   any_pfas_detect  1 = location detected at least one PFAS compound
#                    (filtering on this reproduces the old "bucket A" design)
#   lon/lat          ECHO exact PWS facility coordinate (one per PWSID,
#                    same source & logic as clean_data_construction.R)
#   covariates       same dcast of UCMR5_AddtlDataElem as the original build
# plus a per-compound official MRL table (from the MRL column).
#
# Saves: model_outputs_censored/ucmr5_full_inputs.rds
# Run:   Rscript UCMR5_full_rebuild.R   (~2-5 min)
# =============================================================================

suppressMessages({ library(tidyverse); library(data.table) })

u <- fread("PFAS_Project_Data/ucmr5_full/UCMR5_All.txt", sep = "\t",
           select = c("PWSID", "PWSName", "FacilityID", "Contaminant", "MRL",
                      "AnalyticalResultsSign", "AnalyticalResultValue"))
cat("full UCMR5 result rows:", nrow(u), "\n")
u <- u[Contaminant != "lithium"]

# official reporting limit per compound (uniform in UCMR5; min is defensive)
mrl_table <- u[, .(mrl = min(MRL, na.rm = TRUE)), by = Contaminant]

# location x compound: detected ever? mean of detected values
loc_cmp <- u[, .(
  det  = as.integer(any(AnalyticalResultsSign == "=")),
  conc = mean(AnalyticalResultValue[AnalyticalResultsSign == "="], na.rm = TRUE)
), by = .(PWSID, PWSName, FacilityID, Contaminant)]
loc_cmp[det == 0, conc := NA_real_]

wide_det  <- dcast(loc_cmp, PWSID + PWSName + FacilityID ~ Contaminant,
                   value.var = "det")
wide_conc <- dcast(loc_cmp, PWSID + PWSName + FacilityID ~ Contaminant,
                   value.var = "conc")
setnames(wide_det,  setdiff(names(wide_det),  c("PWSID","PWSName","FacilityID")),
         paste0("det_",  setdiff(names(wide_det),  c("PWSID","PWSName","FacilityID"))))
setnames(wide_conc, setdiff(names(wide_conc), c("PWSID","PWSName","FacilityID")),
         paste0("conc_", setdiff(names(wide_conc), c("PWSID","PWSName","FacilityID"))))
locations <- merge(wide_det, wide_conc, by = c("PWSID","PWSName","FacilityID"))

det_cols <- grep("^det_", names(locations), value = TRUE)
locations[, any_pfas_detect := as.integer(rowSums(.SD == 1, na.rm = TRUE) > 0),
          .SDcols = det_cols]
cat("PFAS-tested locations:", nrow(locations),
    "| systems:", uniqueN(locations$PWSID), "\n")
cat("any-detect locations (old design):", sum(locations$any_pfas_detect), "\n")

# ---- ECHO exact coordinates (identical logic to clean_data_construction.R) ---
echo_raw <- fread("PFAS_Project_Data/echo/ECHO_EXPORTER.csv",
                  select = c("FAC_LAT", "FAC_LONG", "SDWA_IDS"))
echo_raw <- echo_raw[SDWA_IDS != "" & !is.na(FAC_LAT) & !is.na(FAC_LONG)]
echo_coords <- echo_raw[
  , .(PWSID = unlist(strsplit(trimws(SDWA_IDS), "\\s+"))),
  by = .(FAC_LAT, FAC_LONG)
][PWSID != ""][
  , .(lat = first(FAC_LAT), lon = first(FAC_LONG)), by = PWSID
]
locations <- merge(locations, echo_coords, by = "PWSID", all.x = TRUE)
cat("locations with ECHO coordinates:", sum(!is.na(locations$lon)),
    "of", nrow(locations), "\n")

# ---- covariates (identical dcast logic to clean_data_construction.R) ---------
ade <- fread("PFAS_Project_Data/ucmr5_full/UCMR5_AddtlDataElem.txt", sep = "\t")
ade[, varname := fifelse(
  AdditionalDataElement == "DisinfectantType",           paste0("disinf_",      Response),
  fifelse(AdditionalDataElement == "TreatmentInformation",   paste0("treat_",       Response),
  fifelse(AdditionalDataElement == "LithiumTreatment",       paste0("lith_treat_",  Response),
  fifelse(AdditionalDataElement == "PFASTreatment",          paste0("pfas_treat_",  Response),
  fifelse(AdditionalDataElement == "PotentialPFASSourcesDetail", paste0("pfas_source_", Response),
  fifelse(AdditionalDataElement == "LithiumOccurrence",      paste0("lithium_occ_", Response),
  fifelse(AdditionalDataElement == "PFASOccurrence",         paste0("pfas_occ_",    Response),
  fifelse(AdditionalDataElement == "PotentialPFASSources",   paste0("pfas_sources_",Response),
    NA_character_))))))))]
ade <- ade[!is.na(varname)]
ade[, value := 1]
covariates <- dcast(ade, PWSID ~ varname, value.var = "value",
                    fun.aggregate = max, fill = 0)
# sanitize column names for formula use
setnames(covariates, make.names(names(covariates)))
locations <- merge(locations, covariates, by = "PWSID", all.x = TRUE)
cov_cols <- setdiff(names(covariates), "PWSID")
for (cc in cov_cols) set(locations, which(is.na(locations[[cc]])), cc, 0)

geocoded <- locations[!is.na(lon) & !is.na(lat)]
cat("\nFINAL geocoded location table:", nrow(geocoded), "rows,",
    uniqueN(geocoded$PWSID), "systems\n")
cat("  bucket A (any-detect):", sum(geocoded$any_pfas_detect), "rows,",
    uniqueN(geocoded$PWSID[geocoded$any_pfas_detect == 1]), "systems\n")
cat("  bucket B (all tested): ", nrow(geocoded), "rows\n")

dir.create("model_outputs_censored", showWarnings = FALSE)
saveRDS(list(locations = as_tibble(geocoded),
             mrl_table = as_tibble(mrl_table),
             cov_cols  = cov_cols),
        "model_outputs_censored/ucmr5_full_inputs.rds")
cat("\nsaved model_outputs_censored/ucmr5_full_inputs.rds\n")
