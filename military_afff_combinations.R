# =============================================================================
# military_afff_combinations.R
# -----------------------------------------------------------------------------
# Full combination grid of AFFF/military exposure sources vs UCMR5 PFAS.
#
# Location sources:
#   airport  FRS facilities, NAICS 488119 (airport operations / AFFF)
#   milfire  DoD fire/crash training installations, 2014 KBCRS list, geocoded
#   milsusp  EWG "suspected AFFF discharge" military sites (2025 map layer)
#   milbase  MIRTA military installations (all DoD sites w/ coordinates)
#
# Grid: every non-empty combination of the 4 sources (15 sets)
#       x IDW decay lambda in {5, 10, 25} km  (same exponential IDW as
#         build_weighted_idw(); "idw_*_10km" elsewhere = lambda 10 km)
#       x spec in {raw, cov}  (log(PFAS) ~ exposure [+ treatment covariates],
#         no fixed effects -- see SIMPLE_MODELS_EXPLAINED.md for why)
#       x the 8 well-detected compounds.
#
# Duplicates: single-source sets keep every location. Combined sets are
# deduplicated by priority (airport > milfire > milsusp > milbase): a
# lower-priority point within DEDUP_M of an already-kept point is dropped,
# so e.g. an ANG fire-training site at a civilian airport isn't counted twice.
#
# Outputs (kept separate from model_outputs/):
#   model_outputs_military/results_military_afff_combinations.csv
#   model_outputs_military/location_set_sizes.csv
#
# Run:  Rscript military_afff_combinations.R   (seconds; uses the cache)
# =============================================================================

suppressMessages({ library(tidyverse); library(broom); library(sf); library(jsonlite) })
source("naics_tiers.R", chdir = TRUE)

DEDUP_M <- 2500   # meters: cross-source duplicate threshold in combined sets
LAMBDAS <- c(5, 10, 25) * 1000
OUTDIR  <- "model_outputs_military"
dir.create(OUTDIR, showWarnings = FALSE)

.c <- readRDS("model_outputs/naics_inputs.rds")
analysis_data           <- .c$analysis_data
Pfas_dataset_with_dates <- .c$Pfas_dataset_with_dates
log_vars                <- .c$log_vars

# ---- 1. assemble the four location sources as one point table ---------------
airport_fac <- Pfas_dataset_with_dates %>%
  filter(as.character(NAICS_CODE) == "488119") %>%
  transmute(source = "airport", name = PRIMARY_NAME,
            lon = LONGITUDE83, lat = LATITUDE83)

milfire <- read.csv("PFAS_Project_Data/military/fire_training_sites_geocoded.csv") %>%
  filter(!is.na(lon)) %>%
  transmute(source = "milfire", name = installation, lon, lat)

susp_raw <- fromJSON("PFAS_Project_Data/military/ewg_suspected_sites_2025AUG01.geojson")
milsusp <- tibble(source = "milsusp",
                  name = susp_raw$features$properties$FACIL_NAME,
                  lon  = susp_raw$features$properties$Longitude,
                  lat  = susp_raw$features$properties$Latitude)

milbase <- read.csv("PFAS_Project_Data/military/mirta_-223606765265040761.csv") %>%
  filter(!is.na(x), !is.na(y)) %>%
  transmute(source = "milbase", name = Site.Name, lon = x, lat = y)

points <- bind_rows(airport_fac, milfire, milsusp, milbase) %>%
  filter(!is.na(lon), !is.na(lat))
cat("points by source:\n"); print(table(points$source))

# ---- 2. distance matrices (water systems x points; points x points) ---------
water_sf  <- st_as_sf(analysis_data, coords = c("lon", "lat"), crs = 4326)
points_sf <- st_as_sf(points,        coords = c("lon", "lat"), crs = 4326)

dmat_wp <- st_distance(water_sf, points_sf)  |> unclass()  # meters
dmat_pp <- st_distance(points_sf)            |> unclass()

# ---- 3. the 15 location sets, deduped by priority ----------------------------
PRIORITY <- c("airport", "milfire", "milsusp", "milbase")
combos <- unlist(lapply(1:4, function(k)
  combn(PRIORITY, k, simplify = FALSE)), recursive = FALSE)

dedup_indices <- function(sources) {
  sources <- PRIORITY[PRIORITY %in% sources]      # enforce priority order
  kept <- integer(0)
  for (s in sources) {
    idx <- which(points$source == s)
    if (length(kept) > 0 && length(sources) > 1) {
      mind <- apply(dmat_pp[idx, kept, drop = FALSE], 1, min)
      idx  <- idx[mind > DEDUP_M]
    }
    kept <- c(kept, idx)
  }
  kept
}

set_sizes <- map_dfr(combos, function(srcs) {
  kept <- dedup_indices(srcs)
  tibble(location_set = paste(srcs, collapse = "+"),
         n_sources    = length(srcs),
         n_points_raw = sum(points$source %in% srcs),
         n_points     = length(kept),
         n_deduped    = sum(points$source %in% srcs) - length(kept))
})
write.csv(set_sizes, file.path(OUTDIR, "location_set_sizes.csv"), row.names = FALSE)
cat("\nlocation sets:\n"); print(as.data.frame(set_sizes), row.names = FALSE)

# ---- 4. exposures: every set x every lambda ----------------------------------
for (i in seq_along(combos)) {
  kept <- dedup_indices(combos[[i]])
  sub  <- dmat_wp[, kept, drop = FALSE]
  for (lam in LAMBDAS) {
    nm <- paste0("idw__", paste(combos[[i]], collapse = "."), "__", lam / 1000, "km")
    analysis_data[[nm]] <- build_weighted_idw(sub, rep(1, ncol(sub)), lambda = lam)
  }
}

# ---- 5. regressions: raw + covariates, 8 well-detected compounds -------------
cov_cols <- grep("^(pfas_treat_|treat_|disinf_|pfas_occ_|pfas_source_|lithium_occ_|lith_treat_)",
                 names(analysis_data), value = TRUE)
cov_str  <- paste(cov_cols, collapse = " + ")
outcomes <- intersect(c("log_PFOS","log_PFOA","log_PFHxS","log_PFBS",
                        "log_PFHxA","log_PFPeA","log_PFBA","log_PFHpA"), log_vars)

expo_cols <- grep("^idw__", names(analysis_data), value = TRUE)

fit_one <- function(expo, outc, spec) {
  rhs <- if (spec == "raw") expo else paste0(expo, " + ", cov_str)
  d <- analysis_data %>% select(all_of(c(outc, expo, cov_cols))) %>%
    filter(!is.na(.data[[outc]]), !is.na(.data[[expo]]))
  if (nrow(d) < 20) return(NULL)
  fit <- lm(as.formula(paste0("`", outc, "` ~ ", rhs)), data = d)
  co  <- tidy(fit) %>% filter(term == expo)
  if (nrow(co) == 0) return(NULL)
  parts <- str_match(expo, "^idw__(.*)__(\\d+)km$")
  tibble(location_set = gsub("\\.", "+", parts[2]),
         radius_km    = as.integer(parts[3]),
         spec         = spec,
         outcome      = sub("^log_", "", outc),
         estimate     = co$estimate, std.error = co$std.error,
         statistic    = co$statistic, p.value = co$p.value,
         r2           = summary(fit)$r.squared, nobs = nrow(d))
}

res <- bind_rows(lapply(expo_cols, function(e)
  bind_rows(lapply(outcomes, function(o)
    bind_rows(lapply(c("raw", "cov"), function(s) fit_one(e, o, s)))))))

res <- res %>%
  left_join(set_sizes %>% select(location_set, n_points), by = "location_set") %>%
  mutate(sig = case_when(p.value < .01 ~ "***", p.value < .05 ~ "**",
                         p.value < .10 ~ "*",   TRUE ~ ""),
         supports = p.value < .05 & estimate > 0) %>%
  arrange(outcome, location_set, radius_km, spec)

write.csv(res, file.path(OUTDIR, "results_military_afff_combinations.csv"),
          row.names = FALSE)

# ---- 6. console summary -------------------------------------------------------
cat("\n", nrow(res), "regressions saved to",
    file.path(OUTDIR, "results_military_afff_combinations.csv"), "\n")

cat("\n############ SIGNIFICANT + POSITIVE (p<.05, supports hypothesis) ############\n")
res %>% filter(supports) %>%
  mutate(across(c(estimate, p.value), ~signif(., 3))) %>%
  select(outcome, location_set, radius_km, spec, estimate, p.value, sig, nobs) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n############ SIGNIFICANT but WRONG-SIGNED (p<.05, negative) ############\n")
res %>% filter(p.value < .05, estimate < 0) %>%
  mutate(across(c(estimate, p.value), ~signif(., 3))) %>%
  select(outcome, location_set, radius_km, spec, estimate, p.value, sig, nobs) %>%
  as.data.frame() %>% print(row.names = FALSE)
