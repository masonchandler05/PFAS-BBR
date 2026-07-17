# =============================================================================
# military_afff_margins.R
# -----------------------------------------------------------------------------
# The full military/airport combination grid (see military_afff_combinations.R)
# re-estimated on THREE margins per model, motivated by diagnose_pfhxs.R which
# showed the detects-only spec buries signals for heavily-censored compounds:
#
#   detect     P(detected) ~ exposure      LPM on all systems (assumption-light)
#   tobit      log(conc) ~ exposure, left-censored at the compound's reporting
#              limit (survreg gaussian), all systems -- the headline margin
#   intensive  log(conc) ~ exposure, detects only -- the original grid spec
#
# Grid: 15 location sets (all combos of airport/milfire/milsusp/milbase,
#       2.5 km priority dedup in combined sets) x lambda {5,10,25} km
#       x spec {raw, cov} x 8 compounds x 3 margins = 2,160 models.
# No fixed effects anywhere.
#
# Output: model_outputs_military/results_military_afff_all_margins.csv
# Run:    Rscript military_afff_margins.R
# =============================================================================

suppressMessages({ library(tidyverse); library(broom); library(sf)
                   library(jsonlite);  library(survival) })
source("naics_tiers.R", chdir = TRUE)

DEDUP_M <- 2500
LAMBDAS <- c(5, 10, 25) * 1000
OUTDIR  <- "model_outputs_military"
dir.create(OUTDIR, showWarnings = FALSE)

.c <- readRDS("model_outputs/naics_inputs.rds")
analysis_data           <- .c$analysis_data
Pfas_dataset_with_dates <- .c$Pfas_dataset_with_dates

# ---- location sources (identical to military_afff_combinations.R) ------------
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

points <- bind_rows(airport_fac, milfire, milsusp, milbase) %>%
  filter(!is.na(lon), !is.na(lat))

water_sf  <- st_as_sf(analysis_data, coords = c("lon", "lat"), crs = 4326)
points_sf <- st_as_sf(points, coords = c("lon", "lat"), crs = 4326)
dmat_wp <- unclass(st_distance(water_sf, points_sf))
dmat_pp <- unclass(st_distance(points_sf))

PRIORITY <- c("airport", "milfire", "milsusp", "milbase")
combos <- unlist(lapply(1:4, function(k)
  combn(PRIORITY, k, simplify = FALSE)), recursive = FALSE)

dedup_indices <- function(sources) {
  sources <- PRIORITY[PRIORITY %in% sources]
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

for (i in seq_along(combos)) {
  kept <- dedup_indices(combos[[i]])
  sub  <- dmat_wp[, kept, drop = FALSE]
  for (lam in LAMBDAS) {
    nm <- paste0("idw__", paste(combos[[i]], collapse = "."), "__", lam / 1000, "km")
    analysis_data[[nm]] <- build_weighted_idw(sub, rep(1, ncol(sub)), lambda = lam)
  }
}
expo_cols <- grep("^idw__", names(analysis_data), value = TRUE)

# ---- models -------------------------------------------------------------------
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
  # detect LPM (all systems)
  f <- lm(as.formula(paste("det ~", rhs)), data = d)
  co <- tidy(f) %>% filter(term == expo)
  out$detect <- c(co$estimate, co$std.error, co$p.value, nobs(f))
  # tobit (all systems, left-censored at MRL)
  tb <- tryCatch({
    ft <- survreg(as.formula(paste("Surv(ylog, det, type='left') ~", rhs)),
                  data = d, dist = "gaussian")
    s <- summary(ft)$table
    c(s[expo, "Value"], s[expo, "Std. Error"], s[expo, "p"], nrow(d))
  }, error = function(e) rep(NA_real_, 4))
  out$tobit <- tb
  # intensive (detects only) -- the original grid spec
  di <- d %>% filter(det == 1)
  out$intensive <- if (nrow(di) >= 20) {
    f2 <- lm(as.formula(paste("ylog ~", rhs)), data = di)
    co2 <- tidy(f2) %>% filter(term == expo)
    c(co2$estimate, co2$std.error, co2$p.value, nobs(f2))
  } else rep(NA_real_, 4)

  parts <- str_match(expo, "^idw__(.*)__(\\d+)km$")
  map_dfr(names(out), function(m)
    tibble(compound = cmp,
           location_set = gsub("\\.", "+", parts[2]),
           radius_km = as.integer(parts[3]), spec = spec, margin = m,
           estimate = out[[m]][1], std.error = out[[m]][2],
           p.value = out[[m]][3], nobs = out[[m]][4],
           detect_rate = mean(d$det), mrl_ug_l = mrl))
}

grid <- expand_grid(cmp = compounds, expo = expo_cols, spec = c("raw", "cov"))
res <- pmap_dfr(grid, function(cmp, expo, spec) fit_margins(cmp, expo, spec)) %>%
  mutate(sig = case_when(p.value < .01 ~ "***", p.value < .05 ~ "**",
                         p.value < .10 ~ "*", TRUE ~ ""),
         supports = !is.na(p.value) & p.value < .05 & estimate > 0) %>%
  arrange(compound, location_set, radius_km, spec, margin)

write.csv(res, file.path(OUTDIR, "results_military_afff_all_margins.csv"),
          row.names = FALSE)
cat(nrow(res), "model rows saved to",
    file.path(OUTDIR, "results_military_afff_all_margins.csv"), "\n")

# ---- summaries ------------------------------------------------------------------
cat("\n#### count of positive-significant (p<.05) models out of 90 per cell ####\n")
res %>% group_by(compound, margin) %>%
  summarise(n_support = sum(supports), n_sig_neg = sum(!is.na(p.value) &
            p.value < .05 & estimate < 0), .groups = "drop") %>%
  pivot_wider(names_from = margin, values_from = c(n_support, n_sig_neg)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n#### single-source sets, 10 km, tobit margin ####\n")
res %>%
  filter(margin == "tobit", radius_km == 10, !grepl("\\+", location_set)) %>%
  mutate(estimate = signif(estimate, 3), p.value = signif(p.value, 2)) %>%
  select(compound, location_set, spec, estimate, p.value, sig) %>%
  pivot_wider(names_from = spec, values_from = c(estimate, p.value, sig)) %>%
  as.data.frame() %>% print(row.names = FALSE)
