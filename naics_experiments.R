# =============================================================================
# naics_experiments.R
# -----------------------------------------------------------------------------
# Question:  the base analysis (clean_data_construction.R) treats all 39
# PFAS-adjacent NAICS codes as EQUALLY likely PFAS sources -- every facility
# counts the same in the nearest-distance and inverse-distance-weighted (IDW)
# exposure measures. But a chrome-plating shop, a fluoropolymer plant, an
# airport (AFFF) and a "miscellaneous fabricated metal" job-shop are NOT equally
# likely to release PFAS. Treating them identically adds measurement error to
# the exposure variable and ATTENUATES the estimated proximity coefficients
# (the report itself flags this in section 6.3).
#
# This script tests whether differentiating facilities by their evidence-based
# PFAS likelihood sharpens the signal. It runs six experiments, all on the
# project's most credible specification -- log(PFAS) ~ exposure + ZCTA FE --
# focused on the headline regulated compounds (PFOS, PFOA) plus a few short-
# chain compounds.
#
#   Exp A  Baseline: all facilities, equal weight (reproduces the base IDW).
#   Exp B  Tier-1 only: keep only high-confidence PFAS-source industries.
#   Exp B2 EPA-regulated only: the tightest set -- only the 7 codes EPA is
#          actively writing PFAS effluent limits for (OCPSF mfrs/formulators,
#          metal finishing, chromium electroplating, landfills).
#   Exp C  Tier-1+2 only: drop the low-confidence (Tier-3) industries.
#   Exp D  Tier-weighted IDW: keep all, but weight each facility by PFAS
#          likelihood (Tier1=1.0, Tier2=0.5, Tier3=0.15).
#   Exp E  Sector-specific exposure: a separate IDW for each mechanism
#          (chemical mfg, metal finishing, airports, waste, textiles, ...)
#          to see WHICH source type carries the PFOS/PFOA signal.
#   Exp F  Leave-one-sector-out: drop each sector from the full set in turn to
#          see whose removal most weakens the coefficient.
#
# Tier / sector assignments are grounded in:
#   * Salvatore et al. 2022, "Presumptive Contamination" (Env Sci Tech Lett) --
#     three source classes: AFFF (airports/military), industrial, and waste.
#   * EPA Multi-Industry PFAS Study (2021) -- organic chemicals, plastics &
#     resins (fluoropolymers), textiles, metal finishing/electroplating, pulp &
#     paper, airports.
#   * State metal-finishing findings (MN PCA, MI EGLE IPP) -- chrome plating
#     PFOS mist suppressants are the dominant metal-finishing source.
#
# All exposure measures reuse dist_matrix2 from clean_data_construction.R
# (water systems x facilities; its columns are aligned 1:1 with the rows of
# Pfas_dataset_with_dates, which carries NAICS_CODE and START_DATE_FORMAT).
# No distance matrix is recomputed.
#
# Outputs (model_outputs/):
#   naics_tier_assignments.csv         the code -> sector/tier/weight lookup + counts
#   results_naics_experiments.csv      every experiment x outcome, long format
#   results_naics_by_sector.csv        Exp E only (sector comparison)
#   results_naics_leave_one_out.csv    Exp F only
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(broom)
})

# Load the base objects (analysis_data, dist_matrix2, Pfas_dataset_with_dates,
# log_vars) from the cache built by naics_prep_cache.R. Same 2,032-obs sample as
# the report. If the cache is missing, fall back to rebuilding from scratch (slow:
# reads the 2 GB ECHO + 1.9 GB FRS files and recomputes the distance matrices).
out_dir    <- "model_outputs"
cache_file <- file.path(out_dir, "naics_inputs.rds")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

if (file.exists(cache_file)) {
  cat("[exp] loading cached inputs from", cache_file, "\n")
  .cache                  <- readRDS(cache_file)
  dist_matrix2            <- .cache$dist_matrix2
  analysis_data           <- .cache$analysis_data
  Pfas_dataset_with_dates <- .cache$Pfas_dataset_with_dates
  log_vars                <- .cache$log_vars
} else {
  cat("[exp] cache not found; rebuilding from clean_data_construction.R (slow)\n")
  source("clean_data_construction.R", chdir = TRUE)
  dist_matrix2 <- matrix(as.numeric(dist_matrix2),
                         nrow = nrow(dist_matrix2), ncol = ncol(dist_matrix2))
}

# analysis_data carries ZIPCODE under that exact name; the FE spec needs it.
stopifnot("ZIPCODE" %in% names(analysis_data))

LAMBDA <- 10000   # 10 km decay, matches the base idw_10()
KNN    <- 10      # 10 nearest facilities, matches the base idw_10()

# -----------------------------------------------------------------------------
# 1. NAICS -> sector / PFAS-likelihood tier / weight lookup
# -----------------------------------------------------------------------------
# tier 1 = high confidence (direct PFAS manufacture/use, strong evidence)
# tier 2 = medium (plausible secondary user or waste pathway)
# tier 3 = low / speculative (generic code, weak or no direct PFAS link)
naics_meta <- tribble(
  ~code,     ~sector,       ~tier, ~description,
  "325199",  "chemical",        1L, "All other basic organic chemical mfg (fluorochemicals)",
  "325211",  "chemical",        1L, "Plastics material & resin mfg (fluoropolymers, PTFE)",
  "325998",  "chemical",        1L, "All other misc chemical product & prep (PFAS formulation)",
  "325510",  "chemical",        1L, "Paint & coating mfg (fluoropolymer coatings)",
  "325611",  "chemical",        2L, "Soap & detergent mfg",
  "325612",  "chemical",        2L, "Polish & other sanitation good mfg",
  "332813",  "metal",           1L, "Electroplating, plating, anodizing (chrome PFOS mist suppressant)",
  "332812",  "metal",           1L, "Metal coating, engraving, heat treating",
  "332999",  "metal",           3L, "All other misc fabricated metal product mfg",
  "488119",  "airport",         1L, "Other airport operations (AFFF firefighting foam)",
  "562212",  "waste",           1L, "Solid waste landfill (leachate reservoir)",
  "562211",  "waste",           2L, "Hazardous waste treatment & disposal",
  "562213",  "waste",           2L, "Solid waste combustors & incinerators",
  "562112",  "waste",           2L, "Hazardous waste collection",
  "562219",  "waste",           2L, "Other nonhazardous waste treatment & disposal",
  "314110",  "textile",         1L, "Carpet & rug mills (stain-repellent treatments)",
  "313320",  "textile",         1L, "Fabric coating mills (water/stain repellents)",
  "313310",  "textile",         1L, "Textile & fabric finishing mills",
  "313210",  "textile",         2L, "Broadwoven fabric mills",
  "313230",  "textile",         2L, "Nonwoven fabric mills",
  "313110",  "textile",         2L, "Fiber, yarn & thread mills",
  "313220",  "textile",         2L, "Narrow fabric mills",
  "314910",  "textile",         2L, "Textile bag & canvas mills",
  "316110",  "textile",         2L, "Leather & hide tanning & finishing (water repellents)",
  "322121",  "paper",           2L, "Paper (except newsprint) mills (grease-resistant)",
  "322130",  "paper",           2L, "Paperboard mills",
  "322220",  "paper",           1L, "Paper bag & coated/treated paper mfg (direct grease-proofing application)",
  "322219",  "paper",           2L, "Other paperboard container mfg",
  "326113",  "plastics",        2L, "Unlaminated plastics film & sheet mfg",
  "326112",  "plastics",        2L, "Plastics packaging film & sheet mfg",
  "334413",  "electronics",     2L, "Semiconductor & related device mfg (fluorinated etchants)",
  "334419",  "electronics",     2L, "Other electronic component mfg",
  "335999",  "electronics",     3L, "All other misc electrical equipment mfg",
  "324110",  "petroleum",       2L, "Petroleum refineries",
  "324191",  "petroleum",       3L, "Petroleum lubricating oil & grease mfg",
  "323111",  "printing",        3L, "Commercial / gravure printing",
  "323120",  "printing",        3L, "Support activities for printing",
  "333318",  "other",           3L, "Other commercial & service industry machinery mfg",
  "424690",  "wholesale",       3L, "Other chemical & allied products merchant wholesalers"
)

tier_weight <- c(`1` = 1.0, `2` = 0.5, `3` = 0.15)

# Codes EPA is ACTIVELY writing PFAS effluent-limit rulemakings for (strongest
# possible regulatory evidence of real PFAS discharge to water):
#   * OCPSF PFAS manufacturers & formulators (40 CFR 414) -> 325199/325211/325998/325510
#   * Metal finishing (40 CFR 433) & chromium electroplating (40 CFR 413) -> 332812/332813
#   * Landfills (40 CFR 445) -> 562212
# Source: EPA Effluent Guidelines Program Plans 15 & 16; Multi-Industry PFAS Study.
epa_regulated_codes <- c("325199", "325211", "325998", "325510",
                         "332812", "332813", "562212")

naics_meta <- naics_meta %>%
  mutate(weight        = tier_weight[as.character(tier)],
         epa_regulated = code %in% epa_regulated_codes)

# -----------------------------------------------------------------------------
# 2. Align the lookup to the facility universe behind dist_matrix2
#    (columns of dist_matrix2 == rows of Pfas_dataset_with_dates).
# -----------------------------------------------------------------------------
fac_code   <- as.character(Pfas_dataset_with_dates$NAICS_CODE)
fac_pre2002 <- !is.na(Pfas_dataset_with_dates$START_DATE_FORMAT) &
               Pfas_dataset_with_dates$START_DATE_FORMAT < as.Date("2002-01-01")

idx          <- match(fac_code, naics_meta$code)
fac_sector   <- naics_meta$sector[idx]
fac_tier     <- naics_meta$tier[idx]
fac_weight   <- naics_meta$weight[idx]
fac_epa      <- naics_meta$epa_regulated[idx]
fac_epa[is.na(fac_epa)] <- FALSE
fac_weight[is.na(fac_weight)] <- 0      # any unmapped code contributes nothing

stopifnot(ncol(dist_matrix2) == length(fac_code))

# Facility counts per code actually present in the analysis universe.
code_counts <- as.data.frame(table(code = fac_code), stringsAsFactors = FALSE) %>%
  rename(n_facilities = Freq)
naics_assign <- naics_meta %>%
  left_join(code_counts, by = "code") %>%
  mutate(n_facilities = replace_na(n_facilities, 0L),
         n_pre2002    = map_int(code, ~ sum(fac_code == .x & fac_pre2002))) %>%
  arrange(tier, desc(n_facilities))
write.csv(naics_assign, file.path(out_dir, "naics_tier_assignments.csv"), row.names = FALSE)

cat("\nNAICS tier assignments (facilities present in analysis universe):\n")
print(naics_assign %>% select(code, sector, tier, weight, epa_regulated, n_facilities, n_pre2002), n = 40)

# -----------------------------------------------------------------------------
# 3. Exposure builder: weighted IDW + nearest distance over a facility subset
# -----------------------------------------------------------------------------
# dmat    water x facilities distance matrix (dist_matrix2)
# keep    logical vector over facilities (columns) to include
# weights numeric vector over facilities (per-facility weight; 1 = equal)
build_exposure <- function(dmat, keep, weights = NULL, lambda = LAMBDA, k = KNN) {
  cols <- which(keep)
  if (length(cols) == 0) return(NULL)
  sub <- dmat[, cols, drop = FALSE]
  w   <- if (is.null(weights)) rep(1, length(cols)) else weights[cols]
  n   <- nrow(sub)
  idw     <- numeric(n)
  nearest <- numeric(n)
  for (i in seq_len(n)) {
    d <- as.numeric(sub[i, ])
    o <- order(d)[seq_len(min(k, length(d)))]
    idw[i]     <- sum(w[o] * exp(-d[o] / lambda))
    nearest[i] <- min(d)
  }
  list(idw = idw, nearest = nearest, n_fac = length(cols))
}

# -----------------------------------------------------------------------------
# 4. Regression helper: ZCTA fixed-effects, one exposure column, many outcomes
# -----------------------------------------------------------------------------
run_zipfe <- function(data, exposure, outcomes) {
  bind_rows(lapply(outcomes, function(var) {
    md <- data %>%
      select(all_of(c(var, exposure, "ZIPCODE"))) %>%
      filter(!is.na(.data[[var]]), !is.na(.data[[exposure]]), !is.na(ZIPCODE))
    if (nrow(md) < 10) return(NULL)
    fit <- lm(as.formula(paste0("`", var, "` ~ ", exposure, " + factor(ZIPCODE)")),
              data = md)
    co <- tidy(fit) %>% filter(term == exposure)
    if (nrow(co) == 0) return(NULL)
    co %>%
      mutate(outcome = var) %>%
      bind_cols(glance(fit) %>% select(adj.r.squared, nobs)) %>%
      select(outcome, estimate, std.error, statistic, p.value, adj.r.squared, nobs)
  }))
}

# Run one exposure set against the FE spec for both the full-period and the
# pre-2002 legacy facility subset, returning a tidy labelled table.
eval_set <- function(label, keep, weights, outcomes) {
  out <- list()
  ex_now <- build_exposure(dist_matrix2, keep, weights)
  if (!is.null(ex_now)) {
    ad <- analysis_data; ad$exp_idw <- ex_now$idw
    out$now <- run_zipfe(ad, "exp_idw", outcomes) %>%
      mutate(experiment = label, period = "all_years", n_facilities = ex_now$n_fac)
  }
  ex_leg <- build_exposure(dist_matrix2, keep & fac_pre2002, weights)
  if (!is.null(ex_leg)) {
    ad <- analysis_data; ad$exp_idw <- ex_leg$idw
    out$leg <- run_zipfe(ad, "exp_idw", outcomes) %>%
      mutate(experiment = label, period = "pre2002", n_facilities = ex_leg$n_fac)
  }
  bind_rows(out)
}

# -----------------------------------------------------------------------------
# 5. Outcome sets
# -----------------------------------------------------------------------------
panel    <- grep("PFOS|PFOA|PFHxS|PFBS|PFNA", log_vars, value = TRUE)  # Exp A-D
headline <- grep("PFOS|PFOA", log_vars, value = TRUE)                  # Exp E-F
all_keep <- fac_weight > 0 | !is.na(fac_tier)                          # any mapped code

cat("\nOutcomes (panel):", paste(panel, collapse = ", "), "\n")
cat("Total facilities in universe:", ncol(dist_matrix2),
    "| pre-2002:", sum(fac_pre2002), "\n")

# -----------------------------------------------------------------------------
# 6. Experiments A-D (tier restriction & weighting)
# -----------------------------------------------------------------------------
main_results <- bind_rows(
  eval_set("A_baseline_all_equal", all_keep,                         NULL,       panel),
  eval_set("B_tier1_only",         !is.na(fac_tier) & fac_tier == 1L, NULL,       panel),
  eval_set("B2_epa_regulated_only", fac_epa,                         NULL,       panel),
  eval_set("C_tier1_2_only",       !is.na(fac_tier) & fac_tier <= 2L, NULL,       panel),
  eval_set("D_tier_weighted_all",  all_keep,                          fac_weight, panel)
)

# -----------------------------------------------------------------------------
# 7. Experiment E: sector-specific exposure (which mechanism drives the signal)
# -----------------------------------------------------------------------------
sectors <- sort(unique(na.omit(fac_sector)))
by_sector <- bind_rows(lapply(sectors, function(s) {
  eval_set(paste0("E_sector_", s), !is.na(fac_sector) & fac_sector == s, NULL, headline)
}))
write.csv(by_sector, file.path(out_dir, "results_naics_by_sector.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# 8. Experiment F: leave-one-sector-out (whose removal weakens the signal)
# -----------------------------------------------------------------------------
leave_one <- bind_rows(lapply(sectors, function(s) {
  eval_set(paste0("F_drop_", s), all_keep & (is.na(fac_sector) | fac_sector != s), NULL, headline)
}))
write.csv(leave_one, file.path(out_dir, "results_naics_leave_one_out.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# 9. Combine, round, write
# -----------------------------------------------------------------------------
all_results <- bind_rows(main_results, by_sector, leave_one) %>%
  mutate(sig = case_when(
    p.value < 0.01 ~ "***",
    p.value < 0.05 ~ "**",
    p.value < 0.10 ~ "*",
    TRUE           ~ ""
  )) %>%
  select(experiment, period, outcome, n_facilities, estimate, std.error,
         statistic, p.value, sig, adj.r.squared, nobs)
write.csv(all_results, file.path(out_dir, "results_naics_experiments.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# 10. Console summary: headline compounds, sorted by significance
# -----------------------------------------------------------------------------
cat("\n=== PFOS / PFOA results across experiments (FE spec) ===\n")
all_results %>%
  filter(grepl("PFOS|PFOA", outcome)) %>%
  arrange(outcome, p.value) %>%
  mutate(estimate = signif(estimate, 3), p.value = signif(p.value, 3)) %>%
  select(outcome, experiment, period, n_facilities, estimate, p.value, sig) %>%
  print(n = 200)

cat("\nAll NAICS experiment outputs written to", path.expand(out_dir), "\n")
