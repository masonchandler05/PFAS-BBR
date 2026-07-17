# =============================================================================
# analysis_hydrology.R
# -----------------------------------------------------------------------------
# Direction 2 of future_directions.qmd: HYDROLOGICAL FLOW-PATH DISTANCES.
#
# Replaces omnidirectional Euclidean distance with a hydrologically routed
# exposure measure. For each SURFACE-WATER public water system, the system's
# coordinate is snapped to the USGS NHDPlus stream network; PFAS-adjacent
# facilities are kept only if they are genuinely UPSTREAM of the intake; and
# each upstream facility is weighted by its NETWORK (along-river) distance to
# the intake rather than straight-line distance:
#
#     HydroIDW_i = sum_{j in upstream(i)} exp( -flowpath_dist_ij / lambda )
#
#     flowpath_dist_ij = pathlength(facility_flowline_j) - pathlength(intake_i)
#
# where pathlength is the NHDPlusV2 value-added attribute giving network
# distance from a flowline down to the network terminus; the difference between
# a facility's flowline and the intake's flowline is the along-network distance
# between them (positive => facility is upstream).
#
# COMBINED WITH DIRECTION 5 (legacy stratification): a separate upstream index
# is built from pre-2002 facilities only -- "legacy PFAS sources that are
# actually upstream" -- the sharpest test of the project's central finding that
# PFOS contamination is a pre-2002 legacy phenomenon.
#
# Data: NHDPlus is fetched on demand from the USGS Network-Linked Data Index
# (NLDI) via nhdplusTools; the national pathlength VAA table is downloaded once
# and cached by the package. True drinking-water intake coordinates are not
# published nationally, so the ECHO PWS coordinate (snapped to the network) is
# used as an intake proxy -- an acknowledged source of measurement error
# (occasional mis-snapping to a minor tributary; see snap diagnostics in output).
#
# The NLDI pass is cached per system under hydro_cache/ and is
# fully resumable: re-running skips systems already computed.
# =============================================================================

suppressMessages({
  library(tidyverse); library(data.table); library(sf)
  library(broom);     library(nhdplusTools)
})
options(timeout = 600)
sf_use_s2(TRUE)

# Rebuild base objects (echo_coords, Pfas_dataset_with_dates, analysis_data,
# UCMR_5, idw_10, log_vars, covariate_cols, ZIPCODE).
source("clean_data_construction.R", chdir = TRUE)

out_dir   <- "model_outputs"
cache_dir <- "hydro_cache"
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

LAMBDA       <- 10000      # 10 km decay (matches the Euclidean IDW in the base script)
NAV_KM       <- 100        # upstream network search radius (>5*lambda; far beyond it weights ~0)
SNAP_KM      <- 5          # max facility-to-flowline offset to count a facility as "on" a stream
PROJ_CRS     <- 5070       # CONUS Albers Equal Area (meters) for planar distance ops

# National pathlength VAA (downloaded once, cached by nhdplusTools).
# Use a named-vector lookup keyed by COMID: a data.table key lookup whose
# lookup variable is also named `comid` silently returns the whole column.
vaa       <- as.data.table(get_vaa(atts = c("pathlength")))   # columns: comid, pathlength (km)
pl_lookup <- setNames(vaa$pathlength, as.character(vaa$comid))

# -----------------------------------------------------------------------------
# Surface-water systems that are in the analytic sample (have ECHO coordinates).
# -----------------------------------------------------------------------------
sw_pws <- UCMR_5[FacilityWaterType %in% c("SW", "GU", "MX"), unique(PWSID)]

sys <- analysis_data %>%
  distinct(PWSID, lon, lat) %>%
  filter(PWSID %in% sw_pws, !is.na(lon), !is.na(lat))

cat(sprintf("\n[Hydrology] Surface-water systems to route: %d\n", nrow(sys)))

# Facilities (PFAS-adjacent) as projected points, with open dates.
fac_dt <- as.data.table(Pfas_dataset_with_dates)[
  !is.na(LONGITUDE83) & !is.na(LATITUDE83),
  .(LONGITUDE83, LATITUDE83, START_DATE_FORMAT)]
fac_sf_ll  <- st_as_sf(fac_dt, coords = c("LONGITUDE83", "LATITUDE83"), crs = 4326)
fac_sf     <- st_transform(fac_sf_ll, PROJ_CRS)
fac_xy     <- st_coordinates(fac_sf)
fac_pre02  <- !is.na(fac_dt$START_DATE_FORMAT) & fac_dt$START_DATE_FORMAT < as.Date("2002-01-01")

# -----------------------------------------------------------------------------
# Per-system hydrological exposure (cached & resumable).
# -----------------------------------------------------------------------------
compute_one <- function(pwsid, lon, lat) {
  cf <- file.path(cache_dir, paste0(gsub("[^A-Za-z0-9]", "_", pwsid), ".rds"))
  if (file.exists(cf)) return(readRDS(cf))

  res <- tryCatch({
    pt    <- st_sfc(st_point(c(lon, lat)), crs = 4326)
    comid <- suppressMessages(discover_nhdplus_id(point = pt))
    if (length(comid) == 0 || is.na(comid)) stop("no comid")
    intake_pl <- unname(pl_lookup[as.character(as.integer(comid))])
    if (length(intake_pl) == 0 || is.na(intake_pl)) stop("intake not in VAA")

    nav <- suppressMessages(navigate_nldi(
      list(featureSource = "comid", featureID = as.character(comid)),
      mode = "UT", distance_km = NAV_KM, data_source = "flowlines"))
    fl <- nav$UT_flowlines
    if (is.null(fl) || nrow(fl) == 0) stop("no upstream flowlines")

    idcol <- if ("comid" %in% names(fl)) "comid" else "nhdplus_comid"
    fl$comid <- suppressWarnings(as.integer(fl[[idcol]]))
    fl$pathlength <- unname(pl_lookup[as.character(fl$comid)])
    fl <- fl[!is.na(fl$pathlength), ]
    if (nrow(fl) == 0) stop("no flowline pathlengths")
    fl <- st_transform(fl, PROJ_CRS)

    # Restrict facilities to a bounding box around the intake (network dist >=
    # Euclidean dist, so anything beyond NAV_KM straight-line cannot qualify).
    deg <- (NAV_KM + 20) / 111
    inbox <- which(abs(fac_dt$LONGITUDE83 - lon) <= deg / cos(lat * pi / 180) &
                   abs(fac_dt$LATITUDE83  - lat) <= deg)
    if (length(inbox) == 0) stop("no nearby facilities")
    sub <- fac_sf[inbox, ]

    ni       <- st_nearest_feature(sub, fl)
    snap_m   <- as.numeric(st_distance(sub, fl[ni, ], by_element = TRUE))
    fac_pl   <- fl$pathlength[ni]
    flow_km  <- fac_pl - intake_pl                    # >0 => upstream
    keep     <- snap_m <= SNAP_KM * 1000 & flow_km > 0 & flow_km <= NAV_KM
    keep[is.na(keep)] <- FALSE

    w_all <- exp(-(flow_km[keep] * 1000) / LAMBDA)
    pre   <- keep & fac_pre02[inbox]
    w_pre <- exp(-(flow_km[pre] * 1000) / LAMBDA)

    list(PWSID = pwsid, comid = as.integer(comid), intake_pathlength = intake_pl,
         n_upstream = sum(keep), n_upstream_pre2002 = sum(pre),
         hydro_idw_all     = sum(w_all),
         hydro_idw_pre2002 = sum(w_pre),
         nearest_up_km     = if (any(keep)) min(flow_km[keep]) else NA_real_,
         ok = TRUE)
  }, error = function(e)
    list(PWSID = pwsid, comid = NA_integer_, intake_pathlength = NA_real_,
         n_upstream = NA_integer_, n_upstream_pre2002 = NA_integer_,
         hydro_idw_all = NA_real_, hydro_idw_pre2002 = NA_real_,
         nearest_up_km = NA_real_, ok = FALSE))

  saveRDS(res, cf)
  Sys.sleep(0.25)   # be polite to the NLDI service
  res
}

cat("[Hydrology] Routing systems through NLDI (cached/resumable)...\n")
hydro_list <- vector("list", nrow(sys))
for (i in seq_len(nrow(sys))) {
  hydro_list[[i]] <- compute_one(sys$PWSID[i], sys$lon[i], sys$lat[i])
  if (i %% 25 == 0) cat(sprintf("  ... %d / %d done\n", i, nrow(sys)))
}
hydro_df <- bind_rows(hydro_list)

ok_rate <- mean(hydro_df$ok)
cat(sprintf("\n[Hydrology] Routed %d systems | NLDI success %.0f%% | with >=1 upstream facility: %d | with upstream pre-2002: %d\n",
            nrow(hydro_df), 100 * ok_rate,
            sum(hydro_df$n_upstream > 0, na.rm = TRUE),
            sum(hydro_df$n_upstream_pre2002 > 0, na.rm = TRUE)))
cat("[Hydrology] Snap diagnostics — intake pathlength (km) quantiles:\n")
print(round(quantile(hydro_df$intake_pathlength, c(.1,.25,.5,.75,.9), na.rm = TRUE), 1))

write.csv(hydro_df, file.path(out_dir, "hydro_exposure_by_system.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Merge hydrological exposure onto the surface-water analytic observations and
# compare against the Euclidean IDW on the IDENTICAL subset.
# -----------------------------------------------------------------------------
sw_data <- analysis_data %>%
  filter(PWSID %in% sys$PWSID) %>%
  left_join(hydro_df %>% filter(ok) %>%
              select(PWSID, hydro_idw_all, hydro_idw_pre2002, n_upstream), by = "PWSID") %>%
  filter(!is.na(hydro_idw_all)) %>%
  mutate(
    euclid_idw_z   = as.numeric(scale(idw_exposure_10km)),
    hydro_all_z    = as.numeric(scale(hydro_idw_all)),
    hydro_pre02_z  = as.numeric(scale(hydro_idw_pre2002))
  )

cat(sprintf("\n[Hydrology] Surface-water analytic observations with hydrological exposure: %d (from %d systems)\n",
            nrow(sw_data), n_distinct(sw_data$PWSID)))

# Regression helpers (z-scored exposures => per-SD coefficients, comparable).
run_base <- function(data, exposure, outcomes, covs) {
  cs <- paste(covs, collapse = " + ")
  bind_rows(lapply(outcomes, function(v) {
    md <- data %>% select(all_of(c(v, exposure, covs))) %>%
      filter(!is.na(.data[[v]]), !is.na(.data[[exposure]]))
    if (nrow(md) < 15) return(NULL)
    fit <- lm(as.formula(paste0("`", v, "` ~ ", exposure, " + ", cs)), data = md)
    bind_cols(tidy(fit) %>% filter(term == exposure) %>% mutate(outcome = v),
              glance(fit) %>% select(adj.r.squared, nobs))
  })) %>% select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs) %>%
    arrange(p.value)
}
run_fe <- function(data, exposure, outcomes) {
  bind_rows(lapply(outcomes, function(v) {
    md <- data %>% select(all_of(c(v, exposure, "ZIPCODE"))) %>%
      filter(!is.na(.data[[v]]), !is.na(.data[[exposure]]), !is.na(ZIPCODE))
    if (nrow(md) < 15) return(NULL)
    fit <- lm(as.formula(paste0("`", v, "` ~ ", exposure, " + factor(ZIPCODE)")), data = md)
    bind_cols(tidy(fit) %>% filter(term == exposure) %>% mutate(outcome = v),
              glance(fit) %>% select(adj.r.squared, nobs))
  })) %>% select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs) %>%
    arrange(p.value)
}

res_euclid    <- run_base(sw_data, "euclid_idw_z",  log_vars, covariate_cols)
res_hydro_all <- run_base(sw_data, "hydro_all_z",   log_vars, covariate_cols)
res_hydro_pre <- run_base(sw_data, "hydro_pre02_z", log_vars, covariate_cols)
res_hydro_all_fe <- run_fe(sw_data, "hydro_all_z",   log_vars)
res_hydro_pre_fe <- run_fe(sw_data, "hydro_pre02_z", log_vars)

write.csv(res_euclid,       file.path(out_dir, "results_hydro_euclid_baseline.csv"), row.names = FALSE)
write.csv(res_hydro_all,    file.path(out_dir, "results_hydro_upstream_baseline.csv"), row.names = FALSE)
write.csv(res_hydro_pre,    file.path(out_dir, "results_hydro_upstream_pre2002_baseline.csv"), row.names = FALSE)
write.csv(res_hydro_all_fe, file.path(out_dir, "results_hydro_upstream_zipfe.csv"), row.names = FALSE)
write.csv(res_hydro_pre_fe, file.path(out_dir, "results_hydro_upstream_pre2002_zipfe.csv"), row.names = FALSE)

# Head-to-head summary for the regulated long-chain compounds (PFOS, PFOA).
headline <- function(tbl, label) {
  tbl %>% filter(outcome %in% c("log_PFOS", "log_PFOA")) %>%
    transmute(model = label, outcome, estimate, p.value, nobs)
}
hydro_headline <- bind_rows(
  headline(res_euclid,       "Euclidean IDW (same SW subset)"),
  headline(res_hydro_all,    "Upstream HydroIDW (all facilities)"),
  headline(res_hydro_pre,    "Upstream HydroIDW (pre-2002 legacy)"),
  headline(res_hydro_all_fe, "Upstream HydroIDW (all) + ZCTA FE"),
  headline(res_hydro_pre_fe, "Upstream HydroIDW (pre-2002) + ZCTA FE")
) %>% arrange(outcome, model)
write.csv(hydro_headline, file.path(out_dir, "results_hydro_headline_pfos_pfoa.csv"), row.names = FALSE)

cat("\n[Hydrology] PFOS / PFOA head-to-head (Euclidean vs upstream hydrological):\n")
print(hydro_headline, n = 40)
cat("\n[Hydrology] Upstream HydroIDW (all) baseline — full table:\n");        print(res_hydro_all, n = 30)
cat("\n[Hydrology] Upstream HydroIDW (pre-2002 legacy) baseline — full:\n");  print(res_hydro_pre, n = 30)

cat("\n=============================================================\n")
cat("analysis_hydrology.R complete. New outputs in model_outputs/:\n")
cat(" - hydro_exposure_by_system.csv\n")
cat(" - results_hydro_euclid_baseline.csv\n")
cat(" - results_hydro_upstream_baseline.csv\n")
cat(" - results_hydro_upstream_pre2002_baseline.csv\n")
cat(" - results_hydro_upstream_zipfe.csv\n")
cat(" - results_hydro_upstream_pre2002_zipfe.csv\n")
cat(" - results_hydro_headline_pfos_pfoa.csv\n")
cat("=============================================================\n")
