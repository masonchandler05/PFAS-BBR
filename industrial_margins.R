# =============================================================================
# industrial_margins.R
# -----------------------------------------------------------------------------
# Re-estimate the ORIGINAL industrial-proximity exposures (the simple_models.R
# set -- NOT the military/airport grid) on three margins, to test whether the
# null/wrong-signed results were a detection-censoring artifact all along,
# as they were for PFHxS in the military grid (see diagnose_pfhxs.R).
#
#   detect     P(detected) ~ exposure       LPM, all 2,032 systems
#   tobit      log(conc) ~ exposure, left-censored at the reporting limit
#              (survreg gaussian), all systems -- headline margin
#   intensive  log(conc) ~ exposure, detects only  -- the original spec
#
# Exposures (no FE anywhere; ZCTA FE results are excluded by design):
#   IDW families x lambda {5,10,25} km:  all 39 NAICS equal-weight, pre-2002,
#     post-2002, NAICS tier-weighted (Spec D)
#   plus: nearest facility distance (all / pre-2002), within 5 km, within 10 km
#
# Sign convention: distance measures expect NEGATIVE (farther = less PFAS,
# and for the detect margin farther = less likely detected); IDW/buffer
# measures expect POSITIVE. `supports` = p < .05 with the expected sign.
#
# Output: model_outputs_censored/results_industrial_all_margins.csv
# Run:    Rscript industrial_margins.R
# =============================================================================

suppressMessages({ library(tidyverse); library(broom); library(survival) })
source("naics_tiers.R", chdir = TRUE)

OUTDIR <- "model_outputs_censored"
dir.create(OUTDIR, showWarnings = FALSE)

.c <- readRDS("model_outputs/naics_inputs.rds")
analysis_data           <- .c$analysis_data
Pfas_dataset_with_dates <- .c$Pfas_dataset_with_dates
dist_matrix2            <- .c$dist_matrix2   # 2032 water x 21479 facilities

# ---- facility weight vectors for each IDW family -----------------------------
fac   <- Pfas_dataset_with_dates
w_all  <- rep(1, nrow(fac))
w_pre  <- as.numeric(!is.na(fac$START_DATE_FORMAT) &
                     fac$START_DATE_FORMAT <  as.Date("2002-01-01"))
w_post <- as.numeric(!is.na(fac$START_DATE_FORMAT) &
                     fac$START_DATE_FORMAT >= as.Date("2002-01-01"))
idx    <- match(as.character(fac$NAICS_CODE), naics_meta$code)
w_tier <- naics_meta$weight[idx]; w_tier[is.na(w_tier)] <- 0

families <- list(all_equal = w_all, pre2002 = w_pre,
                 post2002 = w_post, tierweighted = w_tier)

for (fam in names(families)) {
  for (lam in c(5, 10, 25) * 1000) {
    nm <- paste0("idw__", fam, "__", lam / 1000, "km")
    analysis_data[[nm]] <- build_weighted_idw(dist_matrix2, families[[fam]],
                                              lambda = lam)
  }
}

# non-IDW measures (no decay variants); distances in km so coefs are readable
analysis_data <- analysis_data %>%
  mutate(nearest_km         = nearest_distance_m / 1000,
         nearest_pre2002_km = if ("nearest_distance_pre2002_m" %in% names(.))
                                nearest_distance_pre2002_m / 1000 else NA_real_,
         within_5km  = as.integer(nearest_distance_m <= 5000),
         within_10km = as.integer(nearest_distance_m <= 10000))

exposures <- tribble(
  ~exposure,            ~expect, ~radius_km,
  "nearest_km",          "neg",   NA,
  "nearest_pre2002_km",  "neg",   NA,
  "within_5km",          "pos",   NA,
  "within_10km",         "pos",   NA)
for (fam in names(families)) for (r in c(5, 10, 25))
  exposures <- add_row(exposures, exposure = paste0("idw__", fam, "__", r, "km"),
                       expect = "pos", radius_km = r)
exposures <- exposures %>%
  filter(map_lgl(exposure, ~ !all(is.na(analysis_data[[.x]]))))

# ---- three-margin fitting ------------------------------------------------------
cov_cols <- grep("^(pfas_treat_|treat_|disinf_|pfas_occ_|pfas_source_|lithium_occ_|lith_treat_)",
                 names(analysis_data), value = TRUE)
compounds <- c("PFOS", "PFOA", "PFHxS", "PFBS", "PFHxA", "PFPeA", "PFBA", "PFHpA")

fit_margins <- function(cmp, expo, spec) {
  conc <- analysis_data[[cmp]]
  mrl  <- min(conc, na.rm = TRUE)
  d <- analysis_data %>%
    mutate(det  = as.integer(!is.na(.data[[cmp]])),
           ylog = ifelse(det == 1, log(.data[[cmp]]), log(mrl))) %>%
    select(all_of(c("det", "ylog", expo, if (spec == "cov") cov_cols))) %>%
    drop_na()
  rhs <- if (spec == "raw") expo else paste(c(expo, cov_cols), collapse = " + ")

  out <- list()
  f <- lm(as.formula(paste("det ~", rhs)), data = d)
  co <- tidy(f) %>% filter(term == expo)
  out$detect <- c(co$estimate, co$std.error, co$p.value, nobs(f))
  out$tobit <- tryCatch({
    ft <- survreg(as.formula(paste("Surv(ylog, det, type='left') ~", rhs)),
                  data = d, dist = "gaussian")
    s <- summary(ft)$table
    c(s[expo, "Value"], s[expo, "Std. Error"], s[expo, "p"], nrow(d))
  }, error = function(e) rep(NA_real_, 4))
  di <- d %>% filter(det == 1)
  out$intensive <- if (nrow(di) >= 20) {
    f2 <- lm(as.formula(paste("ylog ~", rhs)), data = di)
    co2 <- tidy(f2) %>% filter(term == expo)
    c(co2$estimate, co2$std.error, co2$p.value, nobs(f2))
  } else rep(NA_real_, 4)

  map_dfr(names(out), function(m)
    tibble(compound = cmp, exposure = expo, spec = spec, margin = m,
           estimate = out[[m]][1], std.error = out[[m]][2],
           p.value = out[[m]][3], nobs = out[[m]][4],
           detect_rate = mean(d$det), mrl_ug_l = mrl))
}

grid <- expand_grid(cmp = compounds, expo = exposures$exposure,
                    spec = c("raw", "cov"))
res <- pmap_dfr(grid, function(cmp, expo, spec) fit_margins(cmp, expo, spec)) %>%
  left_join(exposures, by = "exposure") %>%
  mutate(sig = case_when(p.value < .01 ~ "***", p.value < .05 ~ "**",
                         p.value < .10 ~ "*", TRUE ~ ""),
         supports    = !is.na(p.value) & p.value < .05 &
                       ((expect == "pos" & estimate > 0) |
                        (expect == "neg" & estimate < 0)),
         contradicts = !is.na(p.value) & p.value < .05 & !supports) %>%
  arrange(compound, exposure, spec, margin)

write.csv(res, file.path(OUTDIR, "results_industrial_all_margins.csv"),
          row.names = FALSE)
cat(nrow(res), "model rows saved to",
    file.path(OUTDIR, "results_industrial_all_margins.csv"), "\n")

# ---- summaries ------------------------------------------------------------------
cat("\n#### supports (expected sign, p<.05) / contradicts (sig, wrong sign) ####\n")
res %>% group_by(compound, margin) %>%
  summarise(support = sum(supports), contradict = sum(contradicts),
            n = n(), .groups = "drop") %>%
  pivot_wider(names_from = margin, values_from = c(support, contradict, n)) %>%
  select(compound, ends_with("_detect"), ends_with("_tobit"),
         ends_with("_intensive")) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n#### exposure families x margin: PFOS + PFOA (tobit vs intensive, 10km/base) ####\n")
res %>%
  filter(compound %in% c("PFOS", "PFOA"),
         exposure %in% c("idw__all_equal__10km", "idw__pre2002__10km",
                         "idw__post2002__10km", "idw__tierweighted__10km",
                         "nearest_km", "within_10km")) %>%
  mutate(estimate = signif(estimate, 3), p.value = signif(p.value, 2)) %>%
  select(compound, exposure, spec, margin, estimate, p.value, sig) %>%
  arrange(compound, exposure, spec, margin) %>%
  as.data.frame() %>% print(row.names = FALSE)
