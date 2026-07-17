# =============================================================================
# placebo_military.R
# -----------------------------------------------------------------------------
# Same falsification logic as placebo_industrial.R, applied to the military
# exposure with the largest source count and strong full-sample results:
# milbase = all 737 MIRTA installations (10 km IDW).
#
# Races (bucket B, tobit at official MRL, cov spec, clustered by PWSID):
#   milbase alone
#   placebo alone                (11,867 food/wood/beverage/furniture facilities)
#   milbase + placebo            (does military survive generic density?)
#   milbase + placebo + real industry (does it survive BOTH density and
#                                      PFAS-industry co-location?)
# Compounds: PFOS, PFHxS, PFOA, PFBS.
# Output: model_outputs_censored/results_military_placebo.csv
# Run:   Rscript placebo_military.R   (uses cached exposures; ~5 min)
# =============================================================================

suppressMessages({ library(tidyverse); library(survival); library(sandwich) })

inp <- readRDS("model_outputs_censored/ucmr5_full_inputs.rds")
loc <- inp$locations; mrl_table <- inp$mrl_table
expo <- readRDS("model_outputs_censored/ucmr5_full_exposures.rds")
plc  <- readRDS("model_outputs_censored/ucmr5_placebo_exposures.rds")
dat <- bind_cols(loc,
                 expo %>% select(starts_with("idw__")),
                 plc  %>% select(starts_with("idw_placebo")))

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

MIL <- "idw__mil_milbase__10km"
PLB <- "idw_placebo_10km"
IND <- "idw__ind_all_equal__10km"

res <- bind_rows(lapply(c("PFOS", "PFHxS", "PFOA", "PFBS"), function(cmp) bind_rows(
  tobit_fit(MIL, cmp)              %>% mutate(model = "milbase alone"),
  tobit_fit(PLB, cmp)              %>% mutate(model = "placebo alone"),
  tobit_fit(c(MIL, PLB), cmp)      %>% mutate(model = "milbase + placebo"),
  tobit_fit(c(MIL, PLB, IND), cmp) %>% mutate(model = "milbase + placebo + industry"))))

write.csv(res, "model_outputs_censored/results_military_placebo.csv",
          row.names = FALSE)
cat("#### MILITARY PLACEBO TEST (milbase 10km, bucket B, tobit+cov, clustered) ####\n")
print(as.data.frame(res %>%
  mutate(across(c(estimate, beta_perSD, p.value), ~signif(., 2))) %>%
  select(compound, model, term, estimate, beta_perSD, p.value)), row.names = FALSE)
cat("\ncorr(milbase, placebo) =",
    round(cor(dat[[MIL]], dat[[PLB]], use = "complete.obs"), 3),
    "| corr(milbase, industry) =",
    round(cor(dat[[MIL]], dat[[IND]], use = "complete.obs"), 3), "\n")
