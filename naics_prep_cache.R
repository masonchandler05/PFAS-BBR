# =============================================================================
# naics_prep_cache.R
# -----------------------------------------------------------------------------
# Runs the expensive base pipeline (clean_data_construction.R) ONCE and caches
# only the objects the NAICS experiments need, so naics_experiments.R does not
# have to re-read the 2 GB ECHO + 1.9 GB FRS files or recompute the distance
# matrices on every run.
#
# Cached objects (model_outputs/naics_inputs.rds):
#   dist_matrix2             water-system x facility distance matrix (meters)
#   analysis_data            per-water-system outcomes + ZIPCODE + covariates
#   Pfas_dataset_with_dates  facility table; rows align 1:1 with cols of dist_matrix2
#   log_vars                 names of log-PFAS outcome columns
#
# Run:  Rscript naics_prep_cache.R     (only needed once, or after data changes)
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(data.table)
  library(sf)
  library(broom)
  library(tigris)
})

cat("[prep] sourcing clean_data_construction.R (this is the slow part)...\n")
t0 <- Sys.time()
source("clean_data_construction.R", chdir = TRUE)
cat(sprintf("[prep] base pipeline built in %.1f min\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

# dist_matrix2 is a 'units' matrix from st_distance; strip to plain numeric so
# the experiment script can index it without unit arithmetic surprises.
dist_matrix2_plain <- matrix(as.numeric(dist_matrix2),
                             nrow = nrow(dist_matrix2),
                             ncol = ncol(dist_matrix2))

stopifnot(ncol(dist_matrix2_plain) == nrow(Pfas_dataset_with_dates))
stopifnot(nrow(dist_matrix2_plain) == nrow(analysis_data))

out_dir <- "model_outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

saveRDS(
  list(
    dist_matrix2            = dist_matrix2_plain,
    analysis_data           = analysis_data,
    Pfas_dataset_with_dates = Pfas_dataset_with_dates,
    log_vars                = log_vars
  ),
  file.path(out_dir, "naics_inputs.rds")
)

cat(sprintf("[prep] cache written: %s (%d water systems x %d facilities)\n",
            file.path(out_dir, "naics_inputs.rds"),
            nrow(dist_matrix2_plain), ncol(dist_matrix2_plain)))
