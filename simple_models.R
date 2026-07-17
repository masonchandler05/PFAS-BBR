# =============================================================================
# simple_models.R
# -----------------------------------------------------------------------------
# Purpose: cut through the fixed-effects / covariate machinery and show the RAW
# relationship between each exposure measure and each PFAS compound.
#
# The ZCTA fixed effect absorbs ~100% of the exposure variation (see
# diagnose_fe_variation.R), so an FE "significant" result can be a thin-slice
# artifact. This script runs, for EVERY exposure, two transparent specs that let
# nothing soak up the variation:
#
#   simple        log(PFAS) ~ exposure            (bivariate, nothing else)
#   plus_cov      log(PFAS) ~ exposure + treatment covariates   (no FE)
#
# It prints the coefficient, p-value, R^2, and a plain-English verdict for each,
# using the project's sign convention so "does it support the hypothesis?" is
# obvious at a glance.
#
# Run:  Rscript simple_models.R
# =============================================================================

suppressMessages({ library(tidyverse); library(broom) })
source("naics_tiers.R", chdir = TRUE)

.c <- readRDS("model_outputs/naics_inputs.rds")
dist_matrix2 <- .c$dist_matrix2; analysis_data <- .c$analysis_data
Pfas_dataset_with_dates <- .c$Pfas_dataset_with_dates; log_vars <- .c$log_vars

# ---- rebuild the NAICS-aware + buffer exposures so all live in one frame ------
fc  <- as.character(Pfas_dataset_with_dates$NAICS_CODE)
idx <- match(fc, naics_meta$code)
w   <- naics_meta$weight[idx]; w[is.na(w)] <- 0
ap  <- as.numeric(!is.na(naics_meta$sector[idx]) & naics_meta$sector[idx] == "airport")
analysis_data$idw_tierweighted_10km <- build_weighted_idw(dist_matrix2, w)
analysis_data$idw_airport_10km      <- build_weighted_idw(dist_matrix2, ap)
analysis_data <- analysis_data %>%
  mutate(within_5km  = as.integer(nearest_distance_m <= 5000),
         within_10km = as.integer(nearest_distance_m <= 10000))

cov_cols <- grep("^(pfas_treat_|treat_|disinf_|pfas_occ_|pfas_source_|lithium_occ_|lith_treat_)",
                 names(analysis_data), value = TRUE)
cov_str  <- paste(cov_cols, collapse = " + ")

# ---- exposures + the sign we EXPECT if proximity drives contamination --------
#  distance measures: farther away  -> less PFAS  => expect NEGATIVE coefficient
#  idw / buffer / count measures: more exposure  -> more PFAS => expect POSITIVE
exposures <- tribble(
  ~exposure,                      ~expect, ~label,
  "nearest_distance_m",            "neg",  "Distance to nearest facility (m)",
  "nearest_distance_pre2002_m",    "neg",  "Distance to nearest pre-2002 facility (m)",
  "idw_exposure_10km",             "pos",  "IDW exposure, all 39 codes equal-weight",
  "idw_pre2002_exposure_10km",     "pos",  "IDW exposure, pre-2002 facilities",
  "idw_tierweighted_10km",         "pos",  "IDW exposure, NAICS tier-weighted (Spec D)",
  "idw_airport_10km",              "pos",  "IDW exposure, airports/AFFF only",
  "within_5km",                    "pos",  "Within 5 km of a facility (0/1)",
  "within_10km",                   "pos",  "Within 10 km of a facility (0/1)"
)

# well-detected compounds only (skip n<30 noise: PFNA, PFPeS, 6:2 FTS, HFPO-DA)
keep_out <- c("log_PFOS","log_PFOA","log_PFHxS","log_PFBS",
              "log_PFHxA","log_PFPeA","log_PFBA","log_PFHpA")
outcomes <- intersect(keep_out, log_vars)

fit_one <- function(expo, expect, outc, spec) {
  rhs  <- if (spec == "simple") expo else paste0(expo, " + ", cov_str)
  d <- analysis_data %>% select(all_of(c(outc, expo, cov_cols))) %>%
    filter(!is.na(.data[[outc]]), !is.na(.data[[expo]]))
  if (nrow(d) < 20) return(NULL)
  fit <- lm(as.formula(paste0("`", outc, "` ~ ", rhs)), data = d)
  co  <- tidy(fit) %>% filter(term == expo)
  if (nrow(co) == 0) return(NULL)
  est <- co$estimate; p <- co$p.value
  # does the sign match the hypothesis, and is it significant?
  right_sign <- (expect == "neg" & est < 0) | (expect == "pos" & est > 0)
  verdict <- if (p < .05 & right_sign)  "SUPPORTS  (sig, expected sign)"
        else if (p < .05 & !right_sign) "CONTRADICTS (sig, wrong sign)"
        else if (right_sign)            "weak support (expected sign, n.s.)"
        else                            "no/▼ support (wrong sign, n.s.)"
  tibble(outcome = sub("^log_", "", outc), exposure = expo, spec = spec,
         estimate = est, p.value = p, r2 = summary(fit)$r.squared,
         nobs = nrow(d), expected = expect, verdict = verdict)
}

res <- bind_rows(lapply(seq_len(nrow(exposures)), function(i)
  bind_rows(lapply(outcomes, function(o)
    bind_rows(lapply(c("simple","plus_cov"), function(s)
      fit_one(exposures$exposure[i], exposures$expect[i], o, s)))))))

res_print <- res %>%
  mutate(sig = case_when(p.value<.01~"***", p.value<.05~"**", p.value<.10~"*", TRUE~""),
         estimate = signif(estimate, 3), p.value = signif(p.value, 3),
         r2 = round(r2, 3))

write.csv(res_print, "model_outputs/results_simple_models.csv", row.names = FALSE)

cat("\n############ SIMPLE (no-FE) MODELS — RAW RELATIONSHIPS ############\n")
cat("Sign convention: distance measures expect NEGATIVE; idw/buffer expect POSITIVE.\n")
for (ex in exposures$exposure) {
  lab <- exposures$label[exposures$exposure == ex]
  cat("\n----", lab, "(", ex, ") ----\n")
  res_print %>% filter(exposure == ex) %>%
    select(outcome, spec, estimate, p.value, sig, r2, nobs, verdict) %>%
    arrange(outcome, spec) %>% as.data.frame() %>% print(row.names = FALSE)
}

cat("\nSaved: model_outputs/results_simple_models.csv\n")
