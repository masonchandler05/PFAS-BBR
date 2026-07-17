# =============================================================================
# industrial_deepdive_pfoa.R
# -----------------------------------------------------------------------------
# Deep dive into the strongest industrial result from the full-UCMR5 clustered
# re-estimation: PFOA vs the tier-weighted industrial IDW ("Spec D").
# Recap (bucket B, tobit, cov, clustered): 10km beta=0.083 p=5e-14.
#
# Four questions:
#   1. DOSE-RESPONSE  Is the relationship monotone across exposure quintiles,
#      or driven by a top-tail blip? (detection rates + quintile-dummy tobit)
#   2. HORSE RACE     Does PFAS-likelihood weighting carry signal BEYOND
#      generic industrial density? (tierweighted + equal-weight IDW jointly;
#      if tierweighted survives, the effect is PFAS-specific, not
#      "factories = cities" confounding)
#   3. SECTORS        Which industries drive it? (sector-specific IDWs,
#      each alone, PFOA + PFOS for contrast)
#   4. SIZE CONTROL   Robust to the UCMR5 system-size class (L/S), the
#      cheapest available urbanization proxy?
#
# All models: bucket B (all tested locations), tobit left-censored at official
# MRL, treatment covariates, SEs clustered by PWSID. No FE.
# Outputs: model_outputs_censored/results_pfoa_deepdive_*.csv
# Run:    Rscript industrial_deepdive_pfoa.R   (~15 min; recomputes sector IDWs)
# =============================================================================

suppressMessages({ library(tidyverse); library(data.table); library(sf)
                   library(survival);  library(sandwich) })
source("naics_tiers.R", chdir = TRUE)

CHUNK <- 2000
inp <- readRDS("model_outputs_censored/ucmr5_full_inputs.rds")
loc <- inp$locations; mrl_table <- inp$mrl_table
expo <- readRDS("model_outputs_censored/ucmr5_full_exposures.rds")
dat <- bind_cols(loc, expo %>% select(starts_with("idw__ind_"), nearest_km))

# ---- sector IDWs (10 & 25 km), chunked distance pass -------------------------
sector_cache <- "model_outputs_censored/ucmr5_sector_exposures.rds"
if (file.exists(sector_cache)) {
  sect <- readRDS(sector_cache)
} else {
  .c <- readRDS("model_outputs/naics_inputs.rds")
  fac <- .c$Pfas_dataset_with_dates; rm(.c); gc()
  fac$sector <- naics_meta$sector[match(as.character(fac$NAICS_CODE),
                                        naics_meta$code)]
  sectors <- setdiff(unique(na.omit(fac$sector)), "airport")
  loc_sf <- st_as_sf(loc, coords = c("lon", "lat"), crs = 4326)
  fac_sf <- st_as_sf(fac %>% transmute(lon = LONGITUDE83, lat = LATITUDE83),
                     coords = c("lon", "lat"), crs = 4326)
  n <- nrow(loc); starts <- seq(1, n, by = CHUNK)
  parts <- vector("list", length(starts))
  for (ci in seq_along(starts)) {
    rows <- starts[ci]:min(starts[ci] + CHUNK - 1, n)
    dmi <- unclass(st_distance(loc_sf[rows, ], fac_sf))
    out <- data.frame(row_id = rows)
    for (s in sectors) {
      w <- as.numeric(fac$sector == s & !is.na(fac$sector))
      for (lam in c(10000, 25000))
        out[[paste0("idw_sect_", s, "_", lam/1000, "km")]] <-
          build_weighted_idw(dmi, w, lambda = lam)
    }
    parts[[ci]] <- out; rm(dmi); gc(verbose = FALSE)
    cat("sector chunk", ci, "/", length(starts), "\n")
  }
  sect <- bind_rows(parts)
  saveRDS(sect, sector_cache)
}
dat <- bind_cols(dat, sect %>% select(-row_id))

# ---- system size (L/S) from raw UCMR5 ----------------------------------------
size_tbl <- fread("PFAS_Project_Data/ucmr5_full/UCMR5_All.txt", sep = "\t",
                  select = c("PWSID", "Size"))[, .(Size = first(Size)), by = PWSID]
dat <- dat %>% left_join(as_tibble(size_tbl), by = "PWSID") %>%
  mutate(size_large = as.integer(Size == "L"))

# ---- shared model machinery ---------------------------------------------------
cov_cols <- grep("^(pfas_treat_|treat_|disinf_|pfas_occ_|pfas_source_|lithium_occ_|lith_treat_)",
                 names(dat), value = TRUE)
cov_cols <- cov_cols[sapply(cov_cols, function(v) length(unique(dat[[v]])) > 1)]
X <- as.matrix(dat[, cov_cols]); q <- qr(cbind(1, X))
cov_cols <- cov_cols[q$pivot[seq_len(q$rank)] - 1][-1] |> na.omit() |> as.character()

tobit_fit <- function(d, rhs_terms, cmp = "PFOA") {
  mrl <- mrl_table$mrl[mrl_table$Contaminant == cmp]
  d$det  <- d[[paste0("det_", cmp)]]
  d$ylog <- ifelse(d$det == 1, log(pmax(d[[paste0("conc_", cmp)]], mrl/2)), log(mrl))
  d <- d %>% filter(!is.na(det))
  rhs <- paste(c(rhs_terms, cov_cols), collapse = " + ")
  ft <- survreg(as.formula(paste("Surv(ylog, det, type='left') ~", rhs,
                                 "+ cluster(PWSID)")),
                data = d, dist = "gaussian", robust = TRUE)
  s <- summary(ft)$table
  tibble(term = rownames(s), estimate = s[, "Value"],
         std.error = s[, "Std. Err"], p.value = s[, "p"],
         nobs = nrow(d), compound = cmp) %>%
    filter(term %in% rhs_terms | grepl("^expo_q", term))
}

E10 <- "idw__ind_tierweighted__10km"; E25 <- "idw__ind_tierweighted__25km"

# ---- 1. dose-response: quintiles ----------------------------------------------
dat$expo_q <- cut(dat[[E10]], breaks = quantile(dat[[E10]], probs = seq(0,1,.2),
                  na.rm = TRUE), include.lowest = TRUE, labels = paste0("Q",1:5))
dose_detect <- dat %>% filter(!is.na(det_PFOA)) %>% group_by(expo_q) %>%
  summarise(n = n(), pfoa_detect_rate = mean(det_PFOA),
            pfos_detect_rate = mean(det_PFOS, na.rm = TRUE), .groups = "drop")
cat("\n#### 1a. PFOA/PFOS detection rate by tier-weighted exposure quintile ####\n")
print(as.data.frame(dose_detect), row.names = FALSE)

dose_tobit <- tobit_fit(dat, "expo_q")   # Q1 = reference
cat("\n#### 1b. quintile-dummy tobit (latent log PFOA vs Q1) ####\n")
print(as.data.frame(dose_tobit %>% mutate(across(where(is.numeric), ~signif(.,3)))),
      row.names = FALSE)
write.csv(bind_rows(dose_tobit), "model_outputs_censored/results_pfoa_deepdive_dose.csv",
          row.names = FALSE)

# ---- 2. horse race: tier-weighted vs equal-weight ------------------------------
hr <- bind_rows(
  tobit_fit(dat, E10) %>% mutate(model = "tierweighted alone"),
  tobit_fit(dat, "idw__ind_all_equal__10km") %>% mutate(model = "equal-weight alone"),
  tobit_fit(dat, c(E10, "idw__ind_all_equal__10km")) %>% mutate(model = "both jointly"))
cat("\n#### 2. horse race (10km): PFAS-likelihood weighting vs generic density ####\n")
print(as.data.frame(hr %>% mutate(across(where(is.numeric), ~signif(.,3)))),
      row.names = FALSE)
write.csv(hr, "model_outputs_censored/results_pfoa_deepdive_horserace.csv",
          row.names = FALSE)

# ---- 3. sector decomposition ----------------------------------------------------
sector_cols <- grep("^idw_sect_.*_10km$", names(dat), value = TRUE)
sec <- bind_rows(lapply(sector_cols, function(sc) bind_rows(
  tobit_fit(dat, sc, "PFOA"), tobit_fit(dat, sc, "PFOS")))) %>%
  mutate(sector = gsub("idw_sect_|_10km", "", term))
cat("\n#### 3. sector-specific IDWs (10km, each alone): PFOA vs PFOS ####\n")
print(as.data.frame(sec %>% select(sector, compound, estimate, p.value) %>%
  mutate(across(where(is.numeric), ~signif(.,2))) %>%
  pivot_wider(names_from = compound, values_from = c(estimate, p.value))),
  row.names = FALSE)
write.csv(sec, "model_outputs_censored/results_pfoa_deepdive_sectors.csv",
          row.names = FALSE)

# ---- 4. size-class control -------------------------------------------------------
sz <- bind_rows(
  tobit_fit(dat, E10) %>% mutate(model = "baseline"),
  tobit_fit(dat, c(E10, "size_large")) %>% mutate(model = "+ system size (L/S)"),
  tobit_fit(dat %>% filter(size_large == 1), E10) %>% mutate(model = "large systems only"),
  tobit_fit(dat %>% filter(size_large == 0), E10) %>% mutate(model = "small systems only"))
cat("\n#### 4. tier-weighted 10km PFOA effect vs system-size control ####\n")
print(as.data.frame(sz %>% filter(term == E10) %>%
  select(model, estimate, std.error, p.value, nobs) %>%
  mutate(across(where(is.numeric), ~signif(.,3)))), row.names = FALSE)
write.csv(sz, "model_outputs_censored/results_pfoa_deepdive_size.csv",
          row.names = FALSE)
cat("\nDone.\n")
