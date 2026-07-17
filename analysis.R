library(sf)
library(dplyr)
library(broom)
library(tidyr)

# ============================================================================
# STEP 1: Fix date parsing in the original dataset FIRST
# ============================================================================

# Parse dates in the original dataframe (this is the key fix)
Pfas_dataset_with_dates <- Pfas_dataset_with_dates %>%
  mutate(
    START_DATE_FORMAT = as.Date(START_DATE, format = "%d-%b-%y")
  )

# Verify dates parsed correctly
cat("Date parsing verification:\n")
cat("Total factories:", nrow(Pfas_dataset_with_dates), "\n")
cat("Valid dates:", sum(!is.na(Pfas_dataset_with_dates$START_DATE_FORMAT)), "\n")
cat("Date range:", range(Pfas_dataset_with_dates$START_DATE_FORMAT, na.rm = TRUE), "\n")

# ============================================================================
# STEP 2: Create pre-2002 subset and its distance matrix
# ============================================================================

cat("\nCreating pre-2002 subset...\n")

# Filter to pre-2002 factories (dates now properly parsed)
Pfas_dataset_with_dates_before_2002 <- Pfas_dataset_with_dates %>%
  filter(!is.na(START_DATE_FORMAT),  # Remove rows with missing dates
         START_DATE_FORMAT < as.Date("2002-01-01"))

cat("Pre-2002 factories:", nrow(Pfas_dataset_with_dates_before_2002), "\n")
cat("Post-2002 factories:", nrow(Pfas_dataset_with_dates) - nrow(Pfas_dataset_with_dates_before_2002), "\n")

# ============================================================================
# STEP 3: Create spatial object for pre-2002 subset ONLY (if needed)
# ============================================================================

# Check if you already have df1_sf_2002 in your environment
if(!exists("df1_sf_2002")) {
  cat("\nCreating spatial object for pre-2002 factories...\n")
  df1_sf_2002 <- st_as_sf(Pfas_dataset_with_dates_before_2002, 
                          coords = c("LONGITUDE83", "LATITUDE83"), 
                          crs = 4326)
} else {
  cat("\nUsing existing df1_sf_2002 object\n")
}

# ============================================================================
# STEP 4: Compute pre-2002 distance matrix ONLY if needed
# ============================================================================

# Check if you already have dist_matrix_2002
if(!exists("dist_matrix_2002")) {
  cat("\nComputing pre-2002 distance matrix (this will take ~5 minutes)...\n")
  
  # Make sure df2_sf exists
  if(!exists("df2_sf")) {
    df2_sf <- st_as_sf(UCMR5_wide_filter_multiple_zip_codes, 
                       coords = c("lng", "lat"), 
                       crs = 4326)
  }
  
  dist_matrix_2002 <- st_distance(df2_sf, df1_sf_2002)
} else {
  cat("\nUsing existing dist_matrix_2002 object\n")
}

# ============================================================================
# STEP 5: Create NEW column for pre-2002 distances (don't overwrite!)
# ============================================================================

# Extract nearest distance to pre-2002 factories
UCMR5_wide_filter_multiple_zip_codes$nearest_distance_pre2002_m <- apply(dist_matrix_2002, 1, min)

# Verify you still have the original distances
if(is.null(UCMR5_wide_filter_multiple_zip_codes$nearest_distance_m)) {
  cat("\nWARNING: Original nearest_distance_m column is missing!\n")
  cat("You may need to reload your original data or recompute full distances.\n")
} else {
  cat("\nOriginal distances (nearest_distance_m) still present.\n")
}

# ============================================================================
# STEP 6: Verify distances are different
# ============================================================================

cat("\nVerifying distance matrices:\n")
cat("Full distances - range:", range(UCMR5_wide_filter_multiple_zip_codes$nearest_distance_m, na.rm = TRUE), "\n")
cat("Pre-2002 distances - range:", range(UCMR5_wide_filter_multiple_zip_codes$nearest_distance_pre2002_m, na.rm = TRUE), "\n")

# Check correlation - should NOT be 1 if filtering worked
dist_correlation <- cor(UCMR5_wide_filter_multiple_zip_codes$nearest_distance_m, 
                        UCMR5_wide_filter_multiple_zip_codes$nearest_distance_pre2002_m, 
                        use = "complete.obs")
cat("Correlation between full and pre-2002 distances:", dist_correlation, "\n")

if(dist_correlation > 0.99) {
  cat("WARNING: Distances are nearly identical! Date filter may not be working as expected.\n")
  cat("Check if most factories opened before 2002, or if date parsing failed.\n")
} else {
  cat("SUCCESS: Distances are different! Pre-2002 filter changed the nearest factories.\n")
}

# ============================================================================
# STEP 7: Run regressions comparing full vs pre-2002
# ============================================================================

cat("\n=== REGRESSION COMPARISON ===\n")

# Full model (all factories)
model_full <- lm(PFOS ~ nearest_distance_m + factor(ZIPCODE), 
                 data = UCMR5_wide_filter_multiple_zip_codes)
cat("\nFull Model (all factories):\n")
print(summary(model_full))

# Pre-2002 model (only older factories)
model_pre2002 <- lm(PFOS ~ nearest_distance_pre2002_m + factor(ZIPCODE), 
                    data = UCMR5_wide_filter_multiple_zip_codes)
cat("\nPre-2002 Model (factories opened before 2002):\n")
print(summary(model_pre2002))

# Compare coefficients
cat("\n=== COEFFICIENT COMPARISON ===\n")
full_coef <- coef(model_full)["nearest_distance_m"]
pre2002_coef <- coef(model_pre2002)["nearest_distance_pre2002_m"]
cat("Full model coefficient:", full_coef, "\n")
cat("Pre-2002 model coefficient:", pre2002_coef, "\n")

if(abs(full_coef - pre2002_coef) < 0.001) {
  cat("\nWARNING: Coefficients are almost identical!\n")
  cat("This suggests the pre-2002 filter didn't change the nearest factories.\n")
  cat("Possible reasons:\n")
  cat("  1. Most factories opened before 2002\n")
  cat("  2. Date parsing still not working correctly\n")
  cat("  3. The nearest factory for each site is the same in both datasets\n")
}

# ============================================================================
# STEP 8: Optional - Check which factories are being used
# ============================================================================

# Find which factories are the nearest for each site
nearest_indices_full <- apply(dist_matrix_full, 1, which.min)
nearest_indices_pre2002 <- apply(dist_matrix_2002, 1, which.min)

# Check how often the nearest factory is the same
if(exists("dist_matrix_full")) {
  same_nearest <- sum(nearest_indices_full == nearest_indices_pre2002)
  cat("\nNearest factory is the same for", same_nearest, "out of", length(nearest_indices_full), "sites\n")
  cat("Percentage same:", 100 * same_nearest / length(nearest_indices_full), "%\n")
}

cat("\nDone! The pre-2002 distances are now in column: nearest_distance_pre2002_m\n")