library(sf)
library(dplyr)

# Convert to spatial objects
df1_sf <- st_as_sf(Pfas_dataset_with_dates, coords = c("LONGITUDE83", "LATITUDE83"), crs = 4326)
df2_sf <- st_as_sf(UCMR5_wide_filter_multiple_zip_codes, coords = c("lng", "lat"), crs = 4326)

# Compute distance matrix
dist_matrix <- st_distance(df1_sf, df2_sf)
dist_matrix2 <- st_distance(df2_sf, df1_sf)

# Extract nearest distance
Pfas_dataset_with_dates$nearest_distance_m <- apply(dist_matrix, 1, min)

# Extract nearest distance 
UCMR5_wide_filter_multiple_zip_codes$nearest_distance_m <- apply(dist_matrix2, 1, min)

# creates log columns for all pfas 
UCMR5_logtransformed <- UCMR5_wide_filter_multiple_zip_codes %>%
  mutate(across(
    -c(PWSName, PWSID, FacilityID, ZIPCODE, lat, lng),
    log,
    .names = "log_{.col}"
  ))

# SLR models 
model <- lm(log_PFOA ~ nearest_distance_m, data = UCMR5_wide_filter_multiple_zip_codes)
summary(model)

model2 <- lm(log_PFBS ~ nearest_distance_m, data = analysis_data)
summary(model2)


model3 <- lm(PFNA ~ nearest_distance_m, data = UCMR5_wide_filter_multiple_zip_codes)
summary(model3)

model4 <- lm(log_PFPeA ~ nearest_distance_m, data = UCMR5_logtransformed)
summary(model4)

model5 <- lm(log_PFBS ~ log_nearest_distance_m, data = UCMR5_logtransformed)
summary(model5)

mod <- lm(log_PFBS ~ nearest_distance_m + pfas_occ_Yes, data = analysis_data)
summary(mod)

mod <- lm(
  log_PFPeA ~ 
    nearest_distance_m +
    pfas_occ_Yes +
    pfas_treat_GAC +
    pfas_treat_PAC +
    pfas_treat_IEX +
    pfas_treat_NRO +
    pfas_treat_MFL +
    pfas_treat_OZN +
    treat_GAC +
    treat_MFL +
   # treat_RO +
    treat_CON,
  data = analysis_data
)

summary(mod)

#checks for correlation between dummy var 
car::vif(mod)


# models gets relevant data for each MLR 
library(dplyr)
library(broom)

results_list <- lapply(log_vars, function(var) {
  
  model_data <- analysis_data %>%
    select(all_of(c(var, "nearest_distance_m", covariates))) %>%
    filter(
      !is.na(.data[[var]]),
      !is.na(nearest_distance_m)
    )
  
  if(nrow(model_data) < 10) return(NULL)
  
  formula <- as.formula(
    paste0("`", var, "` ~ nearest_distance_m + ", covariate_string)
  )
  
  model <- lm(formula, data = model_data)
  
  # Extract coefficient + model fit stats
  coef_part <- tidy(model) %>%
    filter(term == "nearest_distance_m") %>%
    mutate(outcome = var)
  
  fit_part <- glance(model) %>%
    select(r.squared, adj.r.squared, nobs)
  
  bind_cols(coef_part, fit_part)
})

# Combine results
results <- bind_rows(results_list) %>%
  select(outcome, estimate, std.error, statistic, p.value, adj.r.squared) %>%
  arrange(p.value)

# creates dummy variables for if there is a company within xKm 
analysis_data_buffer <- analysis_data %>%
  mutate(
    km1 = as.integer(nearest_distance_m <= 1000),
    km5  = as.integer(nearest_distance_m <= 5000),
    km10 = as.integer(nearest_distance_m <= 10000),
    km25 = as.integer(nearest_distance_m <= 25000)
  )
  
lm(log_PFBS ~ nearest_distance_m , data = analysis_data_buffer)
lm(log_PFBA ~ km1, data = analysis_data_buffer)
lm(log_PFBA ~ km5, data = analysis_data_buffer)
lm(log_PFBS ~ km10, data = analysis_data_buffer)
lm(log_PFOA ~ km25, data = analysis_data_buffer)

head(analysis_data_buffer)
colnames(analysis_data_buffer)
lm(log_PFBA ~ nearest_distance_m + km1, data = analysis_data_buffer)

summary(analysis_data$nearest_distance_m)

# creates df of regressions of dummy km var 
library(dplyr)
library(broom)

pfas_vars <- grep("^log_", names(analysis_data_buffer), value = TRUE)
pfas_vars <- pfas_vars[!pfas_vars %in% c("log_nearest_distance_m","log_log_PFOA","log_log_PFBS")]

km_vars <- c("km1","km5","km10","km25")

results_buffer <- data.frame()

for(pfas in pfas_vars){
  for(km in km_vars){
    
    model_data <- analysis_data_buffer %>%
      select(all_of(c(pfas, km))) %>%
      filter(!is.na(.data[[pfas]]),
             !is.na(.data[[km]]))
    
    if(nrow(model_data) < 5) next
    
    formula <- as.formula(paste0("`", pfas, "` ~ ", km))
    
    model <- lm(formula, data = model_data)
    
    coef_val <- tidy(model) %>%
      filter(term == km) %>%
      mutate(PFAS = pfas,
             buffer = km) %>%
      select(PFAS, buffer, estimate, p.value)
    
    results_buffer <- bind_rows(results_buffer, coef_val)
    
  }
}

library(tidyverse)
results_wide <- results_buffer %>%
  pivot_wider(
    names_from = buffer,   
    values_from = estimate 
  ) %>%
  mutate(across(km1:km25, ~ round(., 5)))





 b <- lm(log_PFBA ~ nearest_distance_m + factor(ZIPCODE), data = analysis_data_buffer)
 a <- lm(log_PFPeS ~ I(1000 * nearest_distance_m) + factor(ZIPCODE), data = analysis_data_buffer)
 
 summary(a)
 summary(b)
 
 
 c <- lm(log_PFPeA ~ nearest_distance_m + factor(ZIPCODE), data = analysis_data_buffer)
 summary(c)
 library(dplyr)
 library(broom)
 
 results_list <- lapply(log_vars, function(var) {
   
   # Select only the outcome, nearest_distance_m, and ZIPCODE
   model_data <- analysis_data %>%
     select(all_of(c(var, "nearest_distance_m", "ZIPCODE"))) %>%
     filter(
       !is.na(.data[[var]]),
       !is.na(nearest_distance_m),
       !is.na(ZIPCODE)
     )
   
   # Skip if too few rows
   if(nrow(model_data) < 10) return(NULL)
   
   # Build formula with nearest_distance_m + ZIPCODE fixed effects
   formula <- as.formula(
     paste0("`", var, "` ~ nearest_distance_m + factor(ZIPCODE)")
   )
   
   # Run linear model
   model <- lm(formula, data = model_data)
   
   # Extract nearest_distance_m coefficient
   coef_part <- tidy(model) %>%
     filter(term == "nearest_distance_m") %>%
     mutate(outcome = var)
   
   # Extract model fit statistics
   fit_part <- glance(model) %>%
     select(r.squared, adj.r.squared, nobs)
   
   # Combine coefficient and fit stats
   bind_cols(coef_part, fit_part)
 })
 
 # Combine all results into one data frame
 results_fixed_effects <- bind_rows(results_list) %>%
   select(outcome, estimate, std.error, statistic, p.value, adj.r.squared) %>%
   arrange(p.value) %>%
   mutate(
     estimate_in_km = round(1000 * estimate, 5)
   )
 
 library(dplyr)
 library(tidyr)
 
 # List of PFAS variables
 pfas_vars <- c("log_PFHxA", "log_PFOS", "log_PFPeA", "log_PFPeS")
 
 # Count number of non-missing observations (n) for each PFAS variable
 pfas_n <- analysis_data %>%
   summarise(across(all_of(pfas_vars), ~ sum(!is.na(.)))) %>%
   pivot_longer(cols = everything(), names_to = "PFAS_type", values_to = "n")
 
 pfas_n
 
 
 
# PFOS may have negative slope since older plants produced PFOS but around 2002 this was phased out
 # filtering dates of factories opening after 2002 and running regression 
 
 
 # DID SOMETHING WRONG - Regression is the same as before so there must be an error somewhere 
 # likely did not add nearest locations correctly but I could have done something wrong with df 
 Pfas_dataset_with_dates_before_2002 <- Pfas_dataset_with_dates %>%
   mutate(
     START_DATE_FORMAT = as.Date(START_DATE, format = "%d-%b-%y")
   ) %>%
   filter(START_DATE_FORMAT < as.Date("2002-01-01"))
 
 df1_sf_2002 <- st_as_sf(Pfas_dataset_with_dates_before_2002, coords = c("LONGITUDE83", "LATITUDE83"), crs = 4326)

 # Compute distance matrix
 dist_matrix_2002  <- st_distance(df2_sf, df1_sf_2002)


 # Extract nearest distance 
 UCMR5_wide_filter_multiple_zip_codes$nearest_distance_m <- apply(dist_matrix_2002, 1, min)

 
 model <- lm(log(PFOS) ~ nearest_distance_m + factor(ZIPCODE), data = UCMR5_wide_filter_multiple_zip_codes)
 summary(model)
 