# =============================================================================
# granularity_compare.R
# -----------------------------------------------------------------------------
# The ZCTA fixed effect absorbs ~100% of the exposure variation (see
# diagnose_fe_variation.R). This script re-estimates the headline models at
# COARSER geographic granularity, where each group holds more water systems and
# therefore retains more within-group exposure variation to identify the slope:
#
#   none   OLS + treatment covariates (uses ALL variation, between + within)
#   state  state fixed effects   (PWSID first 2 chars)
#   zip3   3-digit ZIP fixed effects
#   zcta   full ZCTA fixed effects (the report's current spec)
#
# For each (exposure x outcome x granularity) it reports the coefficient and,
# crucially, the % of exposure variation the chosen FE absorbs and the effective
# identifying sample, so the bias/variance trade-off is explicit.
#
# Run:  Rscript granularity_compare.R
# =============================================================================

suppressMessages({ library(tidyverse); library(broom) })
source("naics_tiers.R", chdir = TRUE)

.c <- readRDS("model_outputs/naics_inputs.rds")
dist_matrix2 <- .c$dist_matrix2; analysis_data <- .c$analysis_data
Pfas_dataset_with_dates <- .c$Pfas_dataset_with_dates; log_vars <- .c$log_vars

fc  <- as.character(Pfas_dataset_with_dates$NAICS_CODE)
idx <- match(fc, naics_meta$code)
w   <- naics_meta$weight[idx]; w[is.na(w)] <- 0
ap  <- as.numeric(!is.na(naics_meta$sector[idx]) & naics_meta$sector[idx] == "airport")
analysis_data$idw_tierweighted_10km <- build_weighted_idw(dist_matrix2, w)
analysis_data$idw_airport_10km      <- build_weighted_idw(dist_matrix2, ap)

analysis_data <- analysis_data %>%
  mutate(state = substr(PWSID, 1, 2),
         zip3  = substr(ZIPCODE, 1, 3))

cov_cols <- grep("^(pfas_treat_|treat_|disinf_|pfas_occ_|pfas_source_|lithium_occ_|lith_treat_)",
                 names(analysis_data), value = TRUE)
cov_str  <- paste(cov_cols, collapse = " + ")

exposures <- c("idw_exposure_10km", "idw_tierweighted_10km", "idw_airport_10km")
outcomes  <- grep("PFOS|PFOA", log_vars, value = TRUE)
grans     <- c("none", "state", "zip3", "zcta")
gvar      <- c(none = NA, state = "state", zip3 = "zip3", zcta = "ZIPCODE")

fit_one <- function(expo, outc, gran) {
  g <- gvar[[gran]]
  keep <- c(outc, expo, cov_cols, if (!is.na(g)) g)
  d <- analysis_data %>% select(all_of(keep)) %>%
    filter(if_all(everything(), ~ !is.na(.)))
  if (nrow(d) < 20) return(NULL)

  if (gran == "none") {
    f <- paste0("`", outc, "` ~ ", expo, " + ", cov_str)
    absorbed <- NA_real_; ngrp <- NA_integer_; eff <- nrow(d)
  } else {
    f <- paste0("`", outc, "` ~ ", expo, " + factor(", g, ")")
    absorbed <- summary(lm(reformulate(paste0("factor(", g, ")"), expo), data = d))$r.squared
    sz   <- d %>% count(.data[[g]], name = "m")
    mult <- d %>% inner_join(filter(sz, m >= 2), by = g)
    vary <- mult %>% group_by(.data[[g]]) %>% summarise(v = sd(.data[[expo]]), mm = n(), .groups="drop") %>%
      filter(v > 0)
    ngrp <- nrow(vary); eff <- sum(vary$mm)
  }
  fit <- lm(as.formula(f), data = d)
  co  <- tidy(fit) %>% filter(term == expo)
  if (nrow(co) == 0) return(NULL)
  tibble(outcome = outc, exposure = expo, granularity = gran,
         estimate = co$estimate, p.value = co$p.value, nobs = nrow(d),
         pct_var_absorbed = round(100 * absorbed, 1),
         n_grp_identifying = ngrp, eff_obs_identifying = eff)
}

res <- bind_rows(lapply(exposures, function(e)
  bind_rows(lapply(outcomes, function(o)
    bind_rows(lapply(grans, function(g) fit_one(e, o, g)))))))

res <- res %>%
  mutate(sig = case_when(p.value < .01 ~ "***", p.value < .05 ~ "**",
                         p.value < .10 ~ "*", TRUE ~ ""),
         estimate = signif(estimate, 3), p.value = signif(p.value, 3)) %>%
  arrange(outcome, exposure, factor(granularity, levels = grans))

write.csv(res, "model_outputs/results_granularity_compare.csv", row.names = FALSE)
cat("\n========== Coefficient & FE-absorption by geographic granularity ==========\n")
print(as.data.frame(res), row.names = FALSE)
cat("\n(coarser FE = more within-group exposure variation retained = lower",
    "pct_var_absorbed and larger identifying sample, at the cost of weaker",
    "confounding control.)\n")
