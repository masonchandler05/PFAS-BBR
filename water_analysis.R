library(tidyverse)
library(data.table)
library(sp)
library(zipcodeR)
library(dplyr)

# loads in datasets 
UCMR_5 <- fread("~/Downloads/ucmr5-occurrence-data/UCMR5_All.csv")
UCMR_5_locations <- fread("~/Downloads/ucmr5-occurrence-data/UCMR5_ZIPCodes.txt")
UCMR_5_other_data <- fread("~/Downloads/ucmr5-occurrence-data/UCMR5_AddtlDataElem.txt")

head(UCMR_5_other_data)



# AI GENERATED CODE
# creates dummy variables to add as covariates to the dataset 
library(data.table)

dt <- as.data.table(UCMR_5_other_data)

# create variable names like disinf_CLOF, treat_CWL, etc
dt[, varname := fifelse(
  AdditionalDataElement == "DisinfectantType",
  paste0("disinf_", Response),
  fifelse(
    AdditionalDataElement == "TreatmentInformation",
    paste0("treat_", Response),
    fifelse(
      AdditionalDataElement == "LithiumTreatment",
      paste0("lith_treat_", Response),
      fifelse(
        AdditionalDataElement == "PFASTreatment",
        paste0("pfas_treat_", Response),
        fifelse(
          AdditionalDataElement == "PotentialPFASSourcesDetail",
          paste0("pfas_source_", Response),
          fifelse(
            AdditionalDataElement == "LithiumOccurrence",
            paste0("lithium_occ_", Response),
            fifelse(
              AdditionalDataElement == "PFASOccurrence",
              paste0("pfas_occ_", Response),
              fifelse(
                AdditionalDataElement == "PotentialPFASSources",
                paste0("pfas_sources_", Response),
                NA_character_
              )
            )
          )
        )
      )
    )
  )
)]

# remove rows that don't generate covariates
dt <- dt[!is.na(varname)]

# set indicator
dt[, value := 1]

# widen to one row per PWSID
UCMR5_covariates <- dcast(
  dt,
  PWSID ~ varname,
  value.var = "value",
  fun.aggregate = max,
  fill = 0
)




# adds zip code data 
UCMR_5_locations_update <- UCMR_5_locations %>%
  mutate(
    ZIPCODE = sprintf("%05s", ZIPCODE)        # pad to 5 characters
  ) %>%
  left_join(
    zip_code_db %>% select(zipcode, lat, lng),
    by = c("ZIPCODE" = "zipcode")
  )
  

# shows the Minimum reporting value of all PFAS 
UCMR_5_pivot <- pivot_wider(UCMR_5,
                            names_from = Contaminant,
                            values_from = MRL)
  
# shows the mean pfas measurement from each testing location (there could be multiple tests over a certain time period)
UCMR5_wide <- UCMR_5 %>%
  pivot_wider(
    id_cols = c(PWSName,PWSID, FacilityID),
    names_from = Contaminant,
    values_from = AnalyticalResultValue,
    values_fn = mean
  ) 

# Filters so only locations with at least 1 pfas sample above the minimum reporting value and includes every zip code that its located in 
UCMR5_wide_filter_multiple_zip_codes <- UCMR5_wide %>%
  filter(!if_all(-c(PWSName, PWSID, FacilityID, lithium), is.na))%>%
  left_join(UCMR_5_locations_update, by = "PWSID") %>%
  filter(!is.na(lng) & !is.na(lat)) %>%
  left_join(UCMR5_covariates, by = "PWSID")



# Filters so only locations with at least 1 pfas sample above the minimum reporting value
UCMR5_wide_filter <- UCMR5_wide %>%
  filter(!if_all(-c(PWSName, PWSID, FacilityID, lithium), is.na))


