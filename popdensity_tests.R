# =============================================================================
# popdensity_tests.R
# -----------------------------------------------------------------------------
# The direct version of the confounding test the placebos ran indirectly:
# add COUNTY POPULATION DENSITY as a covariate to the key 10 km races.
#
# Density: Census county population estimates (co-est2024-alldata.csv,
# POPESTIMATE2023) / county land area (tigris cb counties ALAND), spatially
# joined to each water-system coordinate; enters as log(1 + persons/km2).
#
# Models (bucket B, tobit at official MRL, cov spec + log density, clustered):
#   industrial: real all-equal IDW and tier-weighted IDW alone; real+placebo
#               joint          (compounds: PFOA, PFOS)
#   military:   milsusp/milfire/milbase/airport alone; each + placebo +
#               industry joint (compounds: PFOS, PFHxS)
# Output: model_outputs_censored/results_popdensity_tests.csv
# Run:   Rscript popdensity_tests.R   (~10 min)
# =============================================================================

suppressMessages({ library(tidyverse); library(data.table); library(sf)
                   library(survival);  library(tigris) })
options(tigris_use_cache = TRUE)

inp <- readRDS("model_outputs_censored/ucmr5_full_inputs.rds")
loc <- inp$locations; mrl_table <- inp$mrl_table
expo <- readRDS("model_outputs_censored/ucmr5_full_exposures.rds")
plc  <- readRDS("model_outputs_censored/ucmr5_placebo_exposures.rds")
dat <- bind_cols(loc, expo %>% select(starts_with("idw__")),
                 plc %>% select(starts_with("idw_placebo")))

# ---- county population density -------------------------------------------------
cty <- counties(cb = TRUE, year = 2023) %>%
  st_transform(4326) %>% select(GEOID, ALAND)
loc_sf <- st_as_sf(dat %>% select(PWSID, FacilityID, lon, lat),
                   coords = c("lon", "lat"), crs = 4326)
joined <- st_join(loc_sf, cty, left = TRUE) %>% st_drop_geometry()
pop <- fread("PFAS_Project_Data/census/co-est2024-alldata.csv",
             select = c("STATE", "COUNTY", "POPESTIMATE2023")) %>%
  mutate(GEOID = sprintf("%02d%03d", STATE, COUNTY))
dens <- joined %>% left_join(pop, by = "GEOID") %>%
  mutate(popden = POPESTIMATE2023 / (ALAND / 1e6),
         log_popden = log1p(popden))
dat$log_popden <- dens$log_popden
cat("locations with density:", sum(!is.na(dat$log_popden)), "of", nrow(dat), "\n")
cat("density quartiles (persons/km2):",
    paste(round(quantile(dens$popden, c(.25,.5,.75,.95), na.rm = TRUE)), collapse=", "), "\n")

# ---- model machinery -------------------------------------------------------------
cov_cols <- grep("^(pfas_treat_|treat_|disinf_|pfas_occ_|pfas_source_|lithium_occ_|lith_treat_)",
                 names(dat), value = TRUE)
cov_cols <- cov_cols[sapply(cov_cols, function(v) length(unique(dat[[v]])) > 1)]
X <- as.matrix(dat[, cov_cols]); q <- qr(cbind(1, X))
cov_cols <- c(cov_cols[q$pivot[seq_len(q$rank)] - 1][-1] |> na.omit() |> as.character(),
              "log_popden")

tfit <- function(rhs_terms, cmp, label) {
  mrl <- mrl_table$mrl[mrl_table$Contaminant == cmp]
  d <- dat
  d$det  <- d[[paste0("det_", cmp)]]
  d$ylog <- ifelse(d$det == 1, log(pmax(d[[paste0("conc_", cmp)]], mrl/2)), log(mrl))
  d <- d %>% filter(!is.na(det), !is.na(log_popden))
  rhs <- paste(c(rhs_terms, cov_cols), collapse = " + ")
  ft <- survreg(as.formula(paste("Surv(ylog, det, type='left') ~", rhs,
                                 "+ cluster(PWSID)")),
                data = d, dist = "gaussian", robust = TRUE)
  s <- summary(ft)$table
  tibble(term = rownames(s)) %>%
    filter(term %in% c(rhs_terms, "log_popden")) %>%
    mutate(estimate = s[term, "Value"], std.error = s[term, "Std. Err"],
           p.value = s[term, "p"], compound = cmp, model = label, nobs = nrow(d))
}

PLB <- "idw_placebo_10km"; IND <- "idw__ind_all_equal__10km"
TIER <- "idw__ind_tierweighted__10km"

res <- bind_rows(
  # industrial
  bind_rows(lapply(c("PFOA", "PFOS"), function(cmp) bind_rows(
    tfit(IND, cmp,  "industry alone + density"),
    tfit(TIER, cmp, "tierweighted alone + density"),
    tfit(c(IND, PLB), cmp, "industry + placebo + density")))),
  # military
  bind_rows(lapply(c("milsusp", "milfire", "milbase", "airport"), function(src) {
    M <- paste0("idw__mil_", src, "__10km")
    bind_rows(lapply(c("PFOS", "PFHxS"), function(cmp) bind_rows(
      tfit(M, cmp, paste(src, "alone + density")),
      tfit(c(M, PLB, IND), cmp, paste(src, "+ placebo + industry + density")))))
  })))

write.csv(res, "model_outputs_censored/results_popdensity_tests.csv", row.names = FALSE)
cat("\n#### WITH COUNTY POPULATION DENSITY (10km, bucket B, tobit, clustered) ####\n")
print(as.data.frame(res %>%
  mutate(across(c(estimate, std.error, p.value), ~signif(., 2))) %>%
  select(compound, model, term, estimate, p.value)), row.names = FALSE)
