# =============================================================================
# naics_results_summary.R
# -----------------------------------------------------------------------------
# Reads the CSVs produced by naics_experiments.R and prints a compact,
# decision-oriented summary answering the project question:
#   "Does restricting / reweighting facilities by NAICS PFAS-likelihood sharpen
#    the proximity signal vs treating all 39 codes equally?"
#
# Run AFTER naics_experiments.R:  Rscript naics_results_summary.R
# =============================================================================

suppressMessages(library(tidyverse))
out_dir <- "model_outputs"

rd <- function(f) {
  p <- file.path(out_dir, f)
  if (!file.exists(p)) { cat("[summary] missing:", p, "\n"); return(NULL) }
  as_tibble(read.csv(p, stringsAsFactors = FALSE))
}

main   <- rd("results_naics_experiments.csv")
sector <- rd("results_naics_by_sector.csv")
l1o    <- rd("results_naics_leave_one_out.csv")
assign <- rd("naics_tier_assignments.csv")

star <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))

if (!is.null(main)) {
  cat("\n================ PFOS / PFOA across exposure definitions ================\n")
  cat("(FE spec: log(PFAS) ~ exposure + ZCTA FE. Positive estimate = more/closer\n",
      " PFAS facilities -> higher measured PFAS. Want: positive, significant.)\n\n", sep = "")

  tab <- main %>%
    filter(grepl("PFOS|PFOA", outcome)) %>%
    mutate(est = signif(estimate, 3), p = signif(p.value, 3), sig = star(p.value)) %>%
    arrange(outcome, period, p.value) %>%
    select(outcome, period, experiment, n_facilities, est, p, sig)
  print(tab, n = 200)

  # Which experiment most improves PFOS/PFOA vs the A baseline, all_years?
  base <- main %>% filter(experiment == "A_baseline_all_equal", period == "all_years",
                          grepl("PFOS|PFOA", outcome)) %>%
    select(outcome, base_p = p.value, base_est = estimate)
  comp <- main %>% filter(period == "all_years", grepl("PFOS|PFOA", outcome)) %>%
    left_join(base, by = "outcome") %>%
    mutate(p_drop = base_p - p.value) %>%
    filter(experiment != "A_baseline_all_equal") %>%
    group_by(experiment) %>%
    summarise(mean_p = mean(p.value), n_sig05 = sum(p.value < .05),
              mean_p_improvement = mean(p_drop), .groups = "drop") %>%
    arrange(mean_p)
  cat("\n---- Which specification sharpens PFOS/PFOA most (all_years) ----\n")
  print(comp)
}

if (!is.null(sector)) {
  cat("\n================ Exp E: which SECTOR carries the signal ================\n")
  sector %>%
    filter(period == "all_years") %>%
    mutate(est = signif(estimate, 3), p = signif(p.value, 3), sig = star(p.value)) %>%
    arrange(outcome, p.value) %>%
    select(outcome, experiment, n_facilities, est, p, sig) %>%
    print(n = 200)
}

if (!is.null(l1o)) {
  cat("\n================ Exp F: whose removal WEAKENS the signal most ==========\n")
  cat("(Bigger p.value after dropping a sector = that sector was carrying signal)\n")
  l1o %>%
    filter(period == "all_years") %>%
    mutate(est = signif(estimate, 3), p = signif(p.value, 3), sig = star(p.value)) %>%
    arrange(outcome, desc(p.value)) %>%
    select(outcome, experiment, n_facilities, est, p, sig) %>%
    print(n = 200)
}

cat("\n[summary] done.\n")
