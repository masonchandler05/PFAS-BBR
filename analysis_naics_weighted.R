# =============================================================================
# analysis_naics_weighted.R
# -----------------------------------------------------------------------------
# Two refined exposure measures motivated by NAICS_findings.md:
#   idw_tierweighted_10km  "Spec D" -- inverse-distance weight over the 10 nearest
#                          facilities, each scaled by its NAICS PFAS-likelihood
#                          weight (tier1=1.0, tier2=0.5, tier3=0.15). The best
#                          PFOS specification found in the experiments.
#   idw_airport_10km       AFFF/airport-only IDW (NAICS 488119) -- the dominant
#                          single PFOS mechanism.
# Both also built over the pre-2002 (legacy) facility subset.
#
# Runs from the cache (model_outputs/naics_inputs.rds) in seconds. Run:
#   Rscript naics_prep_cache.R          # once, if cache absent
#   Rscript analysis_naics_weighted.R
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(broom)
})

source("naics_tiers.R", chdir = TRUE)

out_dir    <- "model_outputs"
cache_file <- file.path(out_dir, "naics_inputs.rds")
if (!file.exists(cache_file))
  stop("cache not found -- run `Rscript naics_prep_cache.R` first.")

cat("[wexp] loading cached inputs\n")
.cache                  <- readRDS(cache_file)
dist_matrix2            <- .cache$dist_matrix2
analysis_data           <- .cache$analysis_data
Pfas_dataset_with_dates <- .cache$Pfas_dataset_with_dates
log_vars                <- .cache$log_vars
stopifnot("ZIPCODE" %in% names(analysis_data),
          ncol(dist_matrix2) == nrow(Pfas_dataset_with_dates))

# -----------------------------------------------------------------------------
# Align per-facility weights to the columns of dist_matrix2
# -----------------------------------------------------------------------------
fac_code    <- as.character(Pfas_dataset_with_dates$NAICS_CODE)
fac_pre2002 <- !is.na(Pfas_dataset_with_dates$START_DATE_FORMAT) &
               Pfas_dataset_with_dates$START_DATE_FORMAT < as.Date("2002-01-01")

idx        <- match(fac_code, naics_meta$code)
fac_weight <- naics_meta$weight[idx];          fac_weight[is.na(fac_weight)] <- 0
fac_sector <- naics_meta$sector[idx]
fac_airport <- as.numeric(!is.na(fac_sector) & fac_sector == "airport")

# -----------------------------------------------------------------------------
# Build the two exposures (full period + pre-2002 legacy)
# -----------------------------------------------------------------------------
analysis_data$idw_tierweighted_10km         <- build_weighted_idw(dist_matrix2, fac_weight)
analysis_data$idw_airport_10km              <- build_weighted_idw(dist_matrix2, fac_airport)
analysis_data$idw_tierweighted_pre2002_10km <- build_weighted_idw(dist_matrix2, fac_weight  * fac_pre2002)
analysis_data$idw_airport_pre2002_10km      <- build_weighted_idw(dist_matrix2, fac_airport * fac_pre2002)

covariate_cols <- grep("^(pfas_treat_|treat_|disinf_|pfas_occ_|pfas_source_|lithium_occ_|lith_treat_)",
                       names(analysis_data), value = TRUE)
covariate_string <- paste(covariate_cols, collapse = " + ")

# -----------------------------------------------------------------------------
# Model runners: ZCTA fixed-effects (primary) and covariate-adjusted OLS
# -----------------------------------------------------------------------------
run_models <- function(exposure) {
  bind_rows(lapply(log_vars, function(var) {
    md <- analysis_data %>%
      select(all_of(c(var, exposure, "ZIPCODE", covariate_cols))) %>%
      filter(!is.na(.data[[var]]), !is.na(.data[[exposure]]), !is.na(ZIPCODE))
    if (nrow(md) < 10) return(NULL)
    fe  <- lm(as.formula(paste0("`", var, "` ~ ", exposure, " + factor(ZIPCODE)")), data = md)
    ols <- lm(as.formula(paste0("`", var, "` ~ ", exposure, " + ", covariate_string)), data = md)
    bind_rows(
      tidy(fe)  %>% filter(term == exposure) %>% mutate(spec = "zcta_fe",  outcome = var, nobs = nobs(fe)),
      tidy(ols) %>% filter(term == exposure) %>% mutate(spec = "ols_cov", outcome = var, nobs = nobs(ols))
    )
  })) %>%
    transmute(outcome, spec, exposure, estimate, std.error, statistic, p.value, nobs)
}

results <- bind_rows(
  run_models("idw_tierweighted_10km"),
  run_models("idw_tierweighted_pre2002_10km"),
  run_models("idw_airport_10km"),
  run_models("idw_airport_pre2002_10km")
) %>%
  mutate(sig = case_when(p.value < .01 ~ "***", p.value < .05 ~ "**",
                         p.value < .10 ~ "*",  TRUE ~ "")) %>%
  arrange(exposure, spec, p.value)

write.csv(results, file.path(out_dir, "results_naics_weighted_airport.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Console summary
# -----------------------------------------------------------------------------
cat("\n=== Refined exposures: all compounds (sorted by p within exposure/spec) ===\n")
results %>%
  mutate(estimate = signif(estimate, 3), p.value = signif(p.value, 3)) %>%
  select(exposure, spec, outcome, estimate, p.value, sig, nobs) %>%
  print(n = 200)

cat("\n=== Headline PFOS / PFOA ===\n")
results %>%
  filter(grepl("PFOS|PFOA", outcome)) %>%
  mutate(estimate = signif(estimate, 3), p.value = signif(p.value, 3)) %>%
  select(exposure, spec, outcome, estimate, p.value, sig, nobs) %>%
  arrange(outcome, exposure, spec) %>%
  print(n = 100)

cat("\n[wexp] written:", file.path(out_dir, "results_naics_weighted_airport.csv"), "\n")
