# =============================================================================
# diagnose_fe_variation.R
# -----------------------------------------------------------------------------
# Is the ZCTA fixed effect absorbing (almost) all of the exposure variation,
# leaving the proximity coefficient identified off a tiny within-ZCTA slice?
# For the PFOS estimation sample this script reports, per exposure measure:
#   * how many ZCTAs are SINGLETONS (one water system -> contribute 0 to FE id)
#   * % of the variance the ZCTA dummies absorb  (R^2 of exposure ~ factor(ZCTA))
#   * within-ZCTA SD vs total SD of the exposure
#   * the EFFECTIVE sample: obs in multi-system ZCTAs that actually have
#     within-ZCTA variation in the exposure (the rows that identify the slope)
#
# Run from the cache:  Rscript diagnose_fe_variation.R
# =============================================================================

suppressMessages({ library(tidyverse) })
source("naics_tiers.R", chdir = TRUE)

.c <- readRDS("model_outputs/naics_inputs.rds")
dist_matrix2 <- .c$dist_matrix2; analysis_data <- .c$analysis_data
Pfas_dataset_with_dates <- .c$Pfas_dataset_with_dates; log_vars <- .c$log_vars

# rebuild the refined exposures (same as analysis_naics_weighted.R)
fc  <- as.character(Pfas_dataset_with_dates$NAICS_CODE)
idx <- match(fc, naics_meta$code)
w   <- naics_meta$weight[idx]; w[is.na(w)] <- 0
ap  <- as.numeric(!is.na(naics_meta$sector[idx]) & naics_meta$sector[idx] == "airport")
analysis_data$idw_tierweighted_10km <- build_weighted_idw(dist_matrix2, w)
analysis_data$idw_airport_10km      <- build_weighted_idw(dist_matrix2, ap)

exposures <- c("idw_exposure_10km", "idw_tierweighted_10km", "idw_airport_10km")
OUTCOME   <- grep("PFOS", log_vars, value = TRUE)[1]

cat("Outcome for the diagnostic sample:", OUTCOME, "\n")

diag_one <- function(expo) {
  d <- analysis_data %>%
    select(y = all_of(OUTCOME), x = all_of(expo), z = ZIPCODE) %>%
    filter(!is.na(y), !is.na(x), !is.na(z))
  n      <- nrow(d)
  nz     <- n_distinct(d$z)
  sizes  <- d %>% count(z, name = "m")
  sing   <- sizes %>% filter(m == 1)
  d2     <- d %>% left_join(sizes, by = "z")           # group sizes
  multi  <- d2 %>% filter(m >= 2)

  # variance absorbed by the ZCTA dummies = R^2 of x ~ factor(z)
  r2_abs <- summary(lm(x ~ factor(z), data = d))$r.squared

  # within-ZCTA SD (demean x by ZCTA, over multi-system groups only)
  wsd <- multi %>% group_by(z) %>% mutate(xd = x - mean(x)) %>% ungroup() %>% pull(xd) %>% sd()
  tsd <- sd(d$x)

  # ZCTAs (and obs) that actually carry within variation in x
  varying <- multi %>% group_by(z) %>% summarise(v = sd(x), mm = n(), .groups = "drop") %>%
    filter(v > 0)
  eff_obs <- sum(varying$mm)

  tibble(
    exposure          = expo,
    n_obs             = n,
    n_zcta            = nz,
    pct_obs_singleton = round(100 * nrow(sing) / n, 1),     # singleton obs (=#singleton ZCTAs)
    pct_var_absorbed  = round(100 * r2_abs, 1),             # <- the key number
    within_to_total_sd = round(wsd / tsd, 3),
    n_zcta_identifying = nrow(varying),
    eff_obs_identifying = eff_obs,
    pct_obs_identifying = round(100 * eff_obs / n, 1),
    resid_df          = n - nz - 1
  )
}

res <- bind_rows(lapply(exposures, diag_one))
cat("\n================= FE absorption diagnostic (PFOS sample) =================\n")
print(as.data.frame(res), row.names = FALSE)

cat("\nReading guide:\n",
    " pct_var_absorbed   = % of the exposure's variance eaten by ZCTA dummies.\n",
    "                      ~100 means the FE leaves almost nothing to identify the slope.\n",
    " within_to_total_sd = how big within-ZCTA exposure swings are vs overall. Small = weak id.\n",
    " pct_obs_identifying= share of the sample sitting in ZCTAs that actually have\n",
    "                      within-ZCTA variation in the exposure (the rows doing the work).\n",
    " resid_df           = residual degrees of freedom after the dummies.\n", sep = "")
