# =============================================================================
# diagnose_pfhxs.R
# -----------------------------------------------------------------------------
# Why is PFHxS -- the canonical second AFFF marker -- null/negative in the
# military/airport combination grid, when theory says it should track PFOS?
#
# Hypothesis under test: LEFT-CENSORING + SELECTION. All grid models condition
# on detection (non-detects are NA), and PFHxS is only detected in ~33% of
# systems with values starting right at the 3 ng/L reporting limit. If AFFF
# exposure mainly moves systems ACROSS the detection threshold, the
# conditional-on-detection (intensive) regression can be null or sign-flipped
# even when the true exposure effect is positive.
#
# Three looks, per compound x exposure source (no fixed effects anywhere):
#   detect   P(detect) ~ exposure            [LPM, raw and +covariates]
#   tobit    left-censored log regression at each compound's reporting limit
#            (survreg, dist = "gaussian"), all 2032 systems
#   intensive log(PFAS) ~ exposure, detects only  [the grid's spec, reference]
#
# Output: model_outputs_military/results_pfhxs_diagnosis.csv
# Run:    Rscript diagnose_pfhxs.R
# =============================================================================

suppressMessages({ library(tidyverse); library(broom); library(sf)
                   library(jsonlite);  library(survival) })
source("naics_tiers.R", chdir = TRUE)

OUTDIR <- "model_outputs_military"
dir.create(OUTDIR, showWarnings = FALSE)

.c <- readRDS("model_outputs/naics_inputs.rds")
analysis_data           <- .c$analysis_data
Pfas_dataset_with_dates <- .c$Pfas_dataset_with_dates

# ---- exposures: the four sources + full deduped combo, lambda 10km & 25km ----
airport_fac <- Pfas_dataset_with_dates %>%
  filter(as.character(NAICS_CODE) == "488119") %>%
  transmute(source = "airport", lon = LONGITUDE83, lat = LATITUDE83)
milfire <- read.csv("PFAS_Project_Data/military/fire_training_sites_geocoded.csv") %>%
  filter(!is.na(lon)) %>% transmute(source = "milfire", lon, lat)
susp_raw <- fromJSON("PFAS_Project_Data/military/ewg_suspected_sites_2025AUG01.geojson")
milsusp <- tibble(source = "milsusp",
                  lon = susp_raw$features$properties$Longitude,
                  lat = susp_raw$features$properties$Latitude)
milbase <- read.csv("PFAS_Project_Data/military/mirta_-223606765265040761.csv") %>%
  filter(!is.na(x), !is.na(y)) %>% transmute(source = "milbase", lon = x, lat = y)

points <- bind_rows(airport_fac, milfire, milsusp, milbase)
water_sf  <- st_as_sf(analysis_data, coords = c("lon", "lat"), crs = 4326)
points_sf <- st_as_sf(points, coords = c("lon", "lat"), crs = 4326)
dmat <- unclass(st_distance(water_sf, points_sf))
dpp  <- unclass(st_distance(points_sf))

PRIORITY <- c("airport", "milfire", "milsusp", "milbase")
kept <- integer(0)
for (s in PRIORITY) {                      # same 2.5 km dedup as the grid
  idx <- which(points$source == s)
  if (length(kept) > 0)
    idx <- idx[apply(dpp[idx, kept, drop = FALSE], 1, min) > 2500]
  kept <- c(kept, idx)
}
sets <- c(as.list(setNames(PRIORITY, PRIORITY)), list(all_combined = NA))
expo_defs <- list()
for (nm in names(sets)) {
  cols <- if (nm == "all_combined") kept else which(points$source == nm)
  for (lam in c(10000, 25000)) {
    key <- paste0(nm, "_", lam / 1000, "km")
    analysis_data[[key]] <- build_weighted_idw(
      dmat[, cols, drop = FALSE], rep(1, length(cols)), lambda = lam)
    expo_defs[[key]] <- key
  }
}

# ---- outcomes: PFHxS + comparators --------------------------------------------
compounds <- c("PFHxS", "PFOS", "PFOA", "PFBS")
cov_cols <- grep("^(pfas_treat_|treat_|disinf_|pfas_occ_|pfas_source_|lithium_occ_|lith_treat_)",
                 names(analysis_data), value = TRUE)
cov_str  <- paste(cov_cols, collapse = " + ")

rows <- list()
for (cmp in compounds) {
  conc <- analysis_data[[cmp]]
  mrl  <- min(conc, na.rm = TRUE)                 # reporting limit = min detect
  det  <- as.integer(!is.na(conc))
  ylog <- ifelse(det == 1, log(conc), log(mrl))   # censored obs pinned at MRL
  analysis_data$..det  <- det
  analysis_data$..ylog <- ylog

  for (ek in names(expo_defs)) {
    for (spec in c("raw", "cov")) {
      rhs <- if (spec == "raw") ek else paste0(ek, " + ", cov_str)

      # 1) extensive margin: P(detect) ~ exposure (LPM on all systems)
      f1 <- lm(as.formula(paste("..det ~", rhs)), data = analysis_data)
      c1 <- tidy(f1) %>% filter(term == ek)

      # 2) tobit: left-censored at the reporting limit, all systems
      dt <- analysis_data %>%
        select(all_of(c("..ylog", "..det", ek, if (spec == "cov") cov_cols))) %>%
        drop_na()
      tb <- tryCatch({
        ft <- survreg(as.formula(paste("Surv(..ylog, ..det, type='left') ~", rhs)),
                      data = dt, dist = "gaussian")
        s  <- summary(ft)$table
        s[ek, c("Value", "Std. Error", "p")]
      }, error = function(e) c(NA, NA, NA))

      # 3) intensive margin (the grid's spec), detects only -- reference
      di <- analysis_data %>% filter(..det == 1)
      f3 <- lm(as.formula(paste("..ylog ~", rhs)), data = di)
      c3 <- tidy(f3) %>% filter(term == ek)

      rows[[length(rows) + 1]] <- tibble(
        compound = cmp, exposure = ek, spec = spec,
        margin   = c("detect_LPM", "tobit_censored", "intensive_detects"),
        estimate = c(c1$estimate, tb[1], c3$estimate),
        p.value  = c(c1$p.value,  tb[3], c3$p.value),
        nobs     = c(nobs(f1), nrow(dt), nobs(f3)),
        detect_rate = mean(det))
    }
  }
  analysis_data$..det <- NULL; analysis_data$..ylog <- NULL
}

res <- bind_rows(rows) %>%
  mutate(sig = case_when(p.value < .01 ~ "***", p.value < .05 ~ "**",
                         p.value < .10 ~ "*", TRUE ~ ""))
write.csv(res, file.path(OUTDIR, "results_pfhxs_diagnosis.csv"), row.names = FALSE)

# ---- selection check: exposure levels, detects vs non-detects ------------------
cat("\n#### mean 10km all-combined exposure: detects vs non-detects ####\n")
for (cmp in compounds) {
  det <- !is.na(analysis_data[[cmp]])
  e   <- analysis_data$all_combined_10km
  tt  <- t.test(e[det], e[!det])
  cat(sprintf("%-6s detect %.3f  vs nondetect %.3f   p = %.2g\n",
              cmp, mean(e[det]), mean(e[!det]), tt$p.value))
}

cat("\n#### PFHxS vs PFOS across margins (10km exposures) ####\n")
res %>%
  filter(grepl("10km", exposure), compound %in% c("PFHxS", "PFOS")) %>%
  mutate(estimate = signif(estimate, 3), p.value = signif(p.value, 2)) %>%
  select(compound, exposure, spec, margin, estimate, p.value, sig) %>%
  arrange(compound, exposure, spec, margin) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\nSaved:", file.path(OUTDIR, "results_pfhxs_diagnosis.csv"), "\n")
