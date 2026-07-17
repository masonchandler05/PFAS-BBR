# =============================================================================
# full_tobit_clustered.R
# -----------------------------------------------------------------------------
# Definitive re-estimation on the COMPLETE UCMR5 (Jan 2026 release; see
# UCMR5_full_rebuild.R), fixing every issue documented in
# ISSUES_AND_SAMPLING_OPTIONS.md:
#   - full data (no Excel truncation)            [issue 1.9]
#   - two sampling buckets                       [issue 1.8]
#       A_anydetect : locations with >=1 PFAS detection (the old design)
#       B_all       : ALL PFAS-tested locations (removes the selection)
#   - official per-compound MRLs from the data   [issue 1.2]
#   - SEs clustered by PWSID                     [issue 1.1]
# No fixed effects anywhere.
#
# Margins: tobit  (left-censored at MRL, survreg + cluster(PWSID), robust)
#          detect (LPM, sandwich::vcovCL clustered by PWSID)
# Grids:   military/airport: 15 source combos x lambda {5,10,25} km
#          industrial: 4 IDW families x lambda {5,10,25} km + nearest/within
# Specs:   raw, cov (UCMR5 treatment covariates)
#
# Outputs:
#   model_outputs_censored/ucmr5_full_exposures.rds  (cached exposures)
#   model_outputs_military/results_military_full_clustered.csv
#   model_outputs_censored/results_industrial_full_clustered.csv
# Run:  Rscript full_tobit_clustered.R   (exposures ~10 min, models ~1-2 h)
# =============================================================================

suppressMessages({ library(tidyverse); library(data.table); library(sf)
                   library(jsonlite);  library(survival);   library(sandwich)
                   library(broom) })
source("naics_tiers.R", chdir = TRUE)

DEDUP_M <- 2500
LAMBDAS <- c(5, 10, 25) * 1000
CHUNK   <- 2000

inp <- readRDS("model_outputs_censored/ucmr5_full_inputs.rds")
loc <- inp$locations; mrl_table <- inp$mrl_table

# =============================================================================
# 1. EXPOSURES (cached)
# =============================================================================
expo_cache <- "model_outputs_censored/ucmr5_full_exposures.rds"
if (file.exists(expo_cache)) {
  expo <- readRDS(expo_cache)
  cat("loaded cached exposures:", ncol(expo) - 2, "columns\n")
} else {
  .c  <- readRDS("model_outputs/naics_inputs.rds")
  fac <- .c$Pfas_dataset_with_dates
  rm(.c); gc()

  # ---- industrial facility weights ----
  w_ind <- list(
    ind_all_equal    = rep(1, nrow(fac)),
    ind_pre2002      = as.numeric(!is.na(fac$START_DATE_FORMAT) &
                                  fac$START_DATE_FORMAT <  as.Date("2002-01-01")),
    ind_post2002     = as.numeric(!is.na(fac$START_DATE_FORMAT) &
                                  fac$START_DATE_FORMAT >= as.Date("2002-01-01")),
    ind_tierweighted = { i <- match(as.character(fac$NAICS_CODE), naics_meta$code)
                         w <- naics_meta$weight[i]; w[is.na(w)] <- 0; w })

  # ---- military/airport points + dedup sets (same as military_afff_margins.R) ----
  ap <- fac %>% filter(as.character(NAICS_CODE) == "488119") %>%
    transmute(source = "airport", lon = LONGITUDE83, lat = LATITUDE83)
  mf <- read.csv("PFAS_Project_Data/military/fire_training_sites_geocoded.csv") %>%
    filter(!is.na(lon)) %>% transmute(source = "milfire", lon, lat)
  sr <- fromJSON("PFAS_Project_Data/military/ewg_suspected_sites_2025AUG01.geojson")
  ms <- tibble(source = "milsusp", lon = sr$features$properties$Longitude,
               lat = sr$features$properties$Latitude)
  mb <- read.csv("PFAS_Project_Data/military/mirta_-223606765265040761.csv") %>%
    filter(!is.na(x), !is.na(y)) %>% transmute(source = "milbase", lon = x, lat = y)
  mil_pts <- bind_rows(ap, mf, ms, mb) %>% filter(!is.na(lon), !is.na(lat))

  mil_sf  <- st_as_sf(mil_pts, coords = c("lon", "lat"), crs = 4326)
  dpp     <- unclass(st_distance(mil_sf))
  PRIORITY <- c("airport", "milfire", "milsusp", "milbase")
  combos <- unlist(lapply(1:4, function(k) combn(PRIORITY, k, simplify = FALSE)),
                   recursive = FALSE)
  dedup_indices <- function(sources) {
    sources <- PRIORITY[PRIORITY %in% sources]
    kept <- integer(0)
    for (s in sources) {
      idx <- which(mil_pts$source == s)
      if (length(kept) > 0 && length(sources) > 1) {
        mind <- apply(dpp[idx, kept, drop = FALSE], 1, min)
        idx  <- idx[mind > DEDUP_M]
      }
      kept <- c(kept, idx)
    }
    kept
  }
  mil_sets <- setNames(lapply(combos, dedup_indices),
                       sapply(combos, paste, collapse = "+"))

  # ---- chunked distance computation over water locations ----
  loc_sf <- st_as_sf(loc, coords = c("lon", "lat"), crs = 4326)
  fac_sf <- st_as_sf(fac %>% transmute(lon = LONGITUDE83, lat = LATITUDE83),
                     coords = c("lon", "lat"), crs = 4326)

  n <- nrow(loc); starts <- seq(1, n, by = CHUNK)
  expo_rows <- vector("list", length(starts))
  t0 <- Sys.time()
  for (ci in seq_along(starts)) {
    rows <- starts[ci]:min(starts[ci] + CHUNK - 1, n)
    dmi <- unclass(st_distance(loc_sf[rows, ], fac_sf))     # industrial
    dmm <- unclass(st_distance(loc_sf[rows, ], mil_sf))     # military
    out <- data.frame(row_id = rows)
    for (fm in names(w_ind)) for (lam in LAMBDAS)
      out[[paste0("idw__", fm, "__", lam/1000, "km")]] <-
        build_weighted_idw(dmi, w_ind[[fm]], lambda = lam)
    out$nearest_km <- apply(dmi, 1, min) / 1000
    pre_cols <- which(w_ind$ind_pre2002 > 0)
    out$nearest_pre2002_km <- apply(dmi[, pre_cols, drop = FALSE], 1, min) / 1000
    for (sn in names(mil_sets)) {
      cols <- mil_sets[[sn]]
      sub  <- dmm[, cols, drop = FALSE]
      for (lam in LAMBDAS)
        out[[paste0("idw__mil_", gsub("\\+", ".", sn), "__", lam/1000, "km")]] <-
          build_weighted_idw(sub, rep(1, ncol(sub)), lambda = lam)
    }
    expo_rows[[ci]] <- out
    rm(dmi, dmm); gc(verbose = FALSE)
    cat(sprintf("chunk %d/%d done (%.1f min elapsed)\n", ci, length(starts),
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  expo <- bind_rows(expo_rows) %>%
    mutate(within_5km  = as.integer(nearest_km <= 5),
           within_10km = as.integer(nearest_km <= 10),
           PWSID = loc$PWSID[row_id], FacilityID = loc$FacilityID[row_id])
  saveRDS(expo, expo_cache)
  cat("saved exposure cache\n")
}

stopifnot(nrow(expo) == nrow(loc))
dat <- bind_cols(loc, expo %>% select(-row_id, -PWSID, -FacilityID))

# =============================================================================
# 2. MODELS: tobit (clustered, robust) + detect LPM (clustered), 2 buckets
# =============================================================================
cov_cols <- grep("^(pfas_treat_|treat_|disinf_|pfas_occ_|pfas_source_|lithium_occ_|lith_treat_)",
                 names(dat), value = TRUE)
compounds <- c("PFOS", "PFOA", "PFHxS", "PFBS", "PFHxA", "PFPeA", "PFBA", "PFHpA")

mil_expos <- grep("^idw__mil_", names(dat), value = TRUE)
ind_expos <- c(grep("^idw__ind_", names(dat), value = TRUE),
               "nearest_km", "nearest_pre2002_km", "within_5km", "within_10km")

buckets <- list(A_anydetect = dat %>% filter(any_pfas_detect == 1),
                B_all       = dat)

# drop covariates that are constant or aliased within each bucket (once)
usable_covs <- lapply(buckets, function(b) {
  cc <- cov_cols[sapply(cov_cols, function(v) length(unique(b[[v]])) > 1)]
  X  <- as.matrix(b[, cc])
  q  <- qr(cbind(1, X))
  keep <- q$pivot[seq_len(q$rank)] - 1          # drop intercept slot
  cc[keep[keep > 0]]
})

fit_cell <- function(b, bucket_name, expo, cmp, spec) {
  mrl <- mrl_table$mrl[mrl_table$Contaminant == cmp]
  dcol <- paste0("det_", cmp); ccol <- paste0("conc_", cmp)
  covs <- if (spec == "cov") usable_covs[[bucket_name]] else character(0)
  d <- b %>%
    select(PWSID, det = all_of(dcol), conc = all_of(ccol),
           expo_v = all_of(expo), all_of(covs)) %>%
    filter(!is.na(det), !is.na(expo_v))
  if (nrow(d) < 50 || sum(d$det) < 10) return(NULL)
  d$ylog <- ifelse(d$det == 1, log(pmax(d$conc, mrl/2)), log(mrl))
  rhs <- paste(c("expo_v", covs), collapse = " + ")

  # detect LPM, clustered
  f1 <- lm(as.formula(paste("det ~", rhs)), data = d)
  v1 <- tryCatch(vcovCL(f1, cluster = d$PWSID), error = function(e) NULL)
  det_row <- if (!is.null(v1)) {
    est <- coef(f1)["expo_v"]; se <- sqrt(v1["expo_v", "expo_v"])
    c(est, se, 2 * pnorm(-abs(est / se)))
  } else rep(NA_real_, 3)

  # tobit, clustered robust
  tb_row <- tryCatch({
    ft <- survreg(as.formula(paste("Surv(ylog, det, type='left') ~", rhs,
                                   "+ cluster(PWSID)")),
                  data = d, dist = "gaussian", robust = TRUE)
    s <- summary(ft)$table
    c(s["expo_v", "Value"], s["expo_v", "Std. Err"], s["expo_v", "p"])
  }, error = function(e) rep(NA_real_, 3))

  map2_dfr(list(det_row, tb_row), c("detect", "tobit"), function(r, m)
    tibble(compound = cmp, exposure = expo, spec = spec, margin = m,
           bucket = bucket_name, estimate = r[1], std.error = r[2],
           p.value = r[3], nobs = nrow(d), n_systems = n_distinct(d$PWSID),
           detect_rate = mean(d$det)))
}

run_grid <- function(expos, label) {
  grid <- expand_grid(bucket = names(buckets), expo = expos,
                      cmp = compounds, spec = c("raw", "cov"))
  cat(sprintf("\n[%s] %d cells x 2 margins\n", label, nrow(grid)))
  t0 <- Sys.time(); done <- 0
  res <- pmap_dfr(grid, function(bucket, expo, cmp, spec) {
    r <- fit_cell(buckets[[bucket]], bucket, expo, cmp, spec)
    done <<- done + 1
    if (done %% 100 == 0)
      cat(sprintf("  %d/%d cells (%.1f min)\n", done, nrow(grid),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    r
  })
  res %>% mutate(sig = case_when(p.value < .01 ~ "***", p.value < .05 ~ "**",
                                 p.value < .10 ~ "*", TRUE ~ ""))
}

res_mil <- run_grid(mil_expos, "military/airport")
write.csv(res_mil, "model_outputs_military/results_military_full_clustered.csv",
          row.names = FALSE)

res_ind <- run_grid(ind_expos, "industrial")
write.csv(res_ind, "model_outputs_censored/results_industrial_full_clustered.csv",
          row.names = FALSE)

# =============================================================================
# 3. console summary
# =============================================================================
summarize_grid <- function(res, dist_expect_neg = "^nearest") {
  res %>%
    mutate(expect_neg = grepl(dist_expect_neg, exposure),
           supports = !is.na(p.value) & p.value < .05 &
             ((!expect_neg & estimate > 0) | (expect_neg & estimate < 0)),
           contradicts = !is.na(p.value) & p.value < .05 & !supports) %>%
    group_by(compound, bucket, margin) %>%
    summarise(sup = sum(supports), con = sum(contradicts), n = n(),
              .groups = "drop")
}
cat("\n#### MILITARY grid: support/contradict by compound x bucket x margin ####\n")
print(as.data.frame(summarize_grid(res_mil) %>%
  pivot_wider(names_from = c(bucket, margin), values_from = c(sup, con, n))),
  row.names = FALSE)
cat("\n#### INDUSTRIAL grid ####\n")
print(as.data.frame(summarize_grid(res_ind) %>%
  pivot_wider(names_from = c(bucket, margin), values_from = c(sup, con, n))),
  row.names = FALSE)
cat("\nDone.\n")
