# =============================================================================
# placebo_industrial.R
# -----------------------------------------------------------------------------
# Falsification test for the industrial-proximity results (see the PFOA deep
# dive, results_pfoa_deepdive_*.csv, which found tier-weighting adds nothing
# over generic facility density).
#
# PLACEBO exposure: IDW built from FRS facilities in NAICS sectors with NO
# plausible PFAS connection -- food mfg (311), beverage/tobacco (312), wood
# products (321), furniture (337) -- with the SAME filters as the real PFAS
# facility set (geocoded, ACCURACY_VALUE <= 1000, valid START_DATE), randomly
# subsampled to the same facility count (21,479) so the index has the same
# construction and comparable scale.
#
# Logic: if the placebo predicts PFOA/PFOS as well as the real PFAS-industry
# index, the "industrial effect" is generic facility/population density.
# If the real index beats its placebo head-to-head, there is PFAS-specific
# signal underneath.
#
# Models (bucket B, tobit at official MRL, cov spec, clustered by PWSID):
#   each index alone; real + placebo jointly. Compounds: PFOA, PFOS.
# Output: model_outputs_censored/results_industrial_placebo.csv
# Run:   Rscript placebo_industrial.R    (~15 min: one chunked distance pass)
# =============================================================================

suppressMessages({ library(tidyverse); library(data.table); library(sf)
                   library(survival);  library(sandwich) })
source("naics_tiers.R", chdir = TRUE)

CHUNK <- 2000
set.seed(42)

inp <- readRDS("model_outputs_censored/ucmr5_full_inputs.rds")
loc <- inp$locations; mrl_table <- inp$mrl_table
expo <- readRDS("model_outputs_censored/ucmr5_full_exposures.rds")
dat <- bind_cols(loc, expo %>% select(starts_with("idw__ind_")))

# ---- build placebo facility set from FRS --------------------------------------
placebo_cache <- "model_outputs_censored/ucmr5_placebo_exposures.rds"
if (file.exists(placebo_cache)) {
  plc <- readRDS(placebo_cache)
} else {
  facilities  <- fread("PFAS_Project_Data/frs/NATIONAL_FACILITY_FILE.CSV")
  naics_codes <- fread("PFAS_Project_Data/frs/NATIONAL_NAICS_FILE.CSV")
  env         <- fread("PFAS_Project_Data/frs/NATIONAL_ENVIRONMENTAL_INTEREST_FILE.CSV")
  env <- env %>% filter(str_length(START_DATE) > 5, !is.na(START_DATE))

  pfas_naics <- c("313320","325510","322220","313210","322121","332813","324110",
                  "325612","334413","326113","332812","333318","334419","562212",
                  "325199","323111","313110","314110","316110","325211","324191",
                  "325998","562211","562213","313310","322219","323120","313220",
                  "313230","322130","332999","424690","314910","326112","335999",
                  "562112","562219","325611","488119")
  plc_naics <- naics_codes %>%
    filter(substr(as.character(NAICS_CODE), 1, 3) %in% c("311","312","321","337"),
           !(as.character(NAICS_CODE) %in% pfas_naics))

  plc_fac <- facilities %>%
    inner_join(plc_naics, by = "REGISTRY_ID") %>%
    left_join(env %>% select(REGISTRY_ID, START_DATE), by = "REGISTRY_ID") %>%
    filter(!is.na(LATITUDE83), !is.na(LONGITUDE83),
           ACCURACY_VALUE <= 1000, !is.na(START_DATE)) %>%
    distinct(REGISTRY_ID, .keep_all = TRUE)
  cat("placebo facility pool:", nrow(plc_fac), "\n")
  n_target <- 21479
  if (nrow(plc_fac) > n_target)
    plc_fac <- plc_fac[sample(nrow(plc_fac), n_target), ]
  cat("placebo facilities used:", nrow(plc_fac),
      "| sectors:", paste(names(sort(table(substr(plc_fac$NAICS_CODE, 1, 3)),
                                     decreasing = TRUE)), collapse = ","), "\n")

  loc_sf <- st_as_sf(loc, coords = c("lon", "lat"), crs = 4326)
  plc_sf <- st_as_sf(plc_fac %>% transmute(lon = LONGITUDE83, lat = LATITUDE83),
                     coords = c("lon", "lat"), crs = 4326)
  n <- nrow(loc); starts <- seq(1, n, by = CHUNK)
  parts <- vector("list", length(starts))
  for (ci in seq_along(starts)) {
    rows <- starts[ci]:min(starts[ci] + CHUNK - 1, n)
    dmp <- unclass(st_distance(loc_sf[rows, ], plc_sf))
    out <- data.frame(row_id = rows)
    for (lam in c(10000, 25000))
      out[[paste0("idw_placebo_", lam/1000, "km")]] <-
        build_weighted_idw(dmp, rep(1, ncol(dmp)), lambda = lam)
    parts[[ci]] <- out; rm(dmp); gc(verbose = FALSE)
    cat("placebo chunk", ci, "/", length(starts), "\n")
  }
  plc <- bind_rows(parts)
  saveRDS(plc, placebo_cache)
}
dat <- bind_cols(dat, plc %>% select(-row_id))

# ---- models ---------------------------------------------------------------------
cov_cols <- grep("^(pfas_treat_|treat_|disinf_|pfas_occ_|pfas_source_|lithium_occ_|lith_treat_)",
                 names(dat), value = TRUE)
cov_cols <- cov_cols[sapply(cov_cols, function(v) length(unique(dat[[v]])) > 1)]
X <- as.matrix(dat[, cov_cols]); q <- qr(cbind(1, X))
cov_cols <- cov_cols[q$pivot[seq_len(q$rank)] - 1][-1] |> na.omit() |> as.character()

tobit_fit <- function(rhs_terms, cmp) {
  mrl <- mrl_table$mrl[mrl_table$Contaminant == cmp]
  d <- dat
  d$det  <- d[[paste0("det_", cmp)]]
  d$ylog <- ifelse(d$det == 1, log(pmax(d[[paste0("conc_", cmp)]], mrl/2)), log(mrl))
  d <- d %>% filter(!is.na(det))
  rhs <- paste(c(rhs_terms, cov_cols), collapse = " + ")
  ft <- survreg(as.formula(paste("Surv(ylog, det, type='left') ~", rhs,
                                 "+ cluster(PWSID)")),
                data = d, dist = "gaussian", robust = TRUE)
  s <- summary(ft)$table
  tibble(term = rownames(s)) %>% filter(term %in% rhs_terms) %>%
    mutate(estimate = s[term, "Value"], std.error = s[term, "Std. Err"],
           p.value = s[term, "p"],
           beta_perSD = estimate * sapply(term, function(t) sd(d[[t]], na.rm = TRUE)),
           compound = cmp, nobs = nrow(d))
}

REAL <- "idw__ind_all_equal__10km"; PLB <- "idw_placebo_10km"
res <- bind_rows(
  tobit_fit(REAL, "PFOA") %>% mutate(model = "real PFAS-industry alone"),
  tobit_fit(PLB,  "PFOA") %>% mutate(model = "placebo alone"),
  tobit_fit(c(REAL, PLB), "PFOA") %>% mutate(model = "joint"),
  tobit_fit(REAL, "PFOS") %>% mutate(model = "real PFAS-industry alone"),
  tobit_fit(PLB,  "PFOS") %>% mutate(model = "placebo alone"),
  tobit_fit(c(REAL, PLB), "PFOS") %>% mutate(model = "joint"),
  tobit_fit(c("idw__ind_tierweighted__10km", PLB), "PFOA") %>%
    mutate(model = "joint (tierweighted vs placebo)"),
  tobit_fit(c("idw__ind_tierweighted__10km", PLB), "PFOS") %>%
    mutate(model = "joint (tierweighted vs placebo)"))

write.csv(res, "model_outputs_censored/results_industrial_placebo.csv",
          row.names = FALSE)
cat("\n#### PLACEBO TEST (10km, bucket B, tobit+cov, clustered) ####\n")
print(as.data.frame(res %>%
  mutate(across(c(estimate, std.error, p.value, beta_perSD), ~signif(., 3))) %>%
  select(compound, model, term, estimate, beta_perSD, p.value)), row.names = FALSE)
cat("\ncorr(real, placebo) =",
    round(cor(dat[[REAL]], dat[[PLB]], use = "complete.obs"), 3), "\n")
cat("Saved: model_outputs_censored/results_industrial_placebo.csv\n")
