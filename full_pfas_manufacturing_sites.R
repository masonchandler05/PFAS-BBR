# loading in libraries 
library(tidyverse)
library(data.table)
library(sp)

# start manufacturing plant locations 

# loading in datasets 
facilities <- fread("~/Downloads/national_combined/NATIONAL_FACILITY_FILE.CSV")
naics_codes <- fread("~/Downloads/national_combined/NATIONAL_NAICS_FILE.CSV")
enviornmental <- fread("~/Downloads/national_combined/NATIONAL_ENVIRONMENTAL_INTEREST_FILE.CSV") 

#filtering enviornmental dataset to only include locations with a start date 
enviornmental1 <- enviornmental %>%
  filter(str_length(START_DATE) > 5) %>%
  filter(!is.na(START_DATE))
  
# The complete list of 38 PFAS-relevant NAICS codes
pfas_naics_codes <- c(
  "313320",  # Fabric Coating Mills
  "325510",  # Paint and Coating Manufacturing
  "322220",  # Paper Bag Manufacturing
  "313210",  # Broadwoven Fabric Mills
  "322121",  # Paper Mills
  "332813",  # Electroplating
  "324110",  # Petroleum Refineries
  "325612",  # Polish Manufacturing
  "334413",  # Semiconductor Manufacturing
  "326113",  # Plastics Film Manufacturing
  "332812",  # Metal Coating
  "333318",  # Service Industry Machinery
  "334419",  # Electronic Component Manufacturing
  "562212",  # Solid Waste Landfill
  "325199",  # Basic Organic Chemical Manufacturing
  "323111",  # Commercial Printing
  "313110",  # Fiber/Yarn Mills
  "314110",  # Carpet Mills
  "316110",  # Leather Tanning
  "325211",  # Plastics Material Manufacturing
  "324191",  # Petroleum Lubricating Oil
  "325998",  # Miscellaneous Chemical Manufacturing
  "562211",  # Hazardous Waste Treatment
  "562213",  # Waste Incinerators
  "313310",  # Textile Finishing Mills
  "322219",  # Paperboard Container Manufacturing
  "323120",  # Support for Printing
  "313220",  # Narrow Fabric Mills
  "313230",  # Nonwoven Fabric Mills
  "322130",  # Paperboard Mills
  "332999",  # Miscellaneous Metal Manufacturing
  "424690",  # Chemical Wholesalers
  "314910",  # Textile Bag Mills
  "326112",  # Plastics Packaging Manufacturing
  "335999",  # Electrical Equipment Manufacturing
  "562112",  # Hazardous Waste Collection
  "562219",  # Waste Treatment
  "325611", # Soap Manufacturing
  "488119" # Airports 
)

#filters for only the relevant factories 
naics_pfas <- naics_codes %>%
  filter(NAICS_CODE %in% pfas_naics_codes)
#checks how many locations fall into this 
nrow(naics_pfas)


#joining together df by resgistry_id 
pfas_facilities <- facilities %>%
  inner_join(naics_pfas, by = "REGISTRY_ID") %>%
  # joins just the start data to the df by registry ID 
  left_join(enviornmental1 %>% select(REGISTRY_ID, START_DATE, START_DATE_QUALIFIER),
            by = "REGISTRY_ID")
#checking rows to see if it combined correctly 
nrow(pfas_facilities)

#filtering out missing locations and innacurrate locaions
pfas_final <- pfas_facilities %>%
  filter(!is.na(LATITUDE83)) %>%
  filter(!is.na(LONGITUDE83)) %>%
  filter(ACCURACY_VALUE <= 1000) %>%
  filter(!is.na(START_DATE)) %>%
  distinct(REGISTRY_ID, .keep_all = TRUE)
# left with ~27,000 locations compared to their 49,000
# what filtering modifications did I make compared to them?
# after filtering for start date, it only has ~21,000

# getting only relevant columns for a simplified dataset 
pfas_dataset <- pfas_final %>%
  select(
    REGISTRY_ID, 
    PRIMARY_NAME,
    LATITUDE83, 
    LONGITUDE83, 
    ACCURACY_VALUE, 
    START_DATE,
    START_DATE_QUALIFIER,
    STATE_CODE, 
    COUNTY_NAME, 
    CITY_NAME, 
    FIPS_CODE, 
    NAICS_CODE
  )

#assigning a new df that takes in the pfas dataset so I can use it as spacial coordinates 
geospace_pfas_dataset <- pfas_dataset

# makes geospatial points of all locations 
coordinates(geospace_pfas_dataset) = c("LONGITUDE83", "LATITUDE83")
crs.geo1 = CRS("+proj=longlat")
proj4string(geospace_pfas_dataset) = crs.geo1
plot(geospace_pfas_dataset, pch = 20, col = "steelblue")

# checking how many of each naics code is found 
naics_count_summary <- pfas_dataset %>%
  group_by(NAICS_CODE) %>%
  summarise(
    factory_count = n()
  ) 

# end manufacturing plants locations 


# start water treatment center locations 

# pulling in data 
water_flow <- read.csv("~/Downloads/2022CWNS_NATIONAL_APR2024/FLOW.csv")
water_treatment_locations <- read.csv("~/Downloads/2022CWNS_NATIONAL_APR2024/PHYSICAL_LOCATION.csv")

# filtering water flow to only industrial flow 
industrial_water_treatment_centers <- water_flow %>%
  filter(FLOW_TYPE == "Industrial Flow") %>%
  select(
    FACILITY_ID,
    FLOW_TYPE
  )

# making a final simplified data set
water_treatment_industrial_final_dataset <- water_treatment_locations %>%
  select(
    FACILITY_ID, 
    LATITUDE, 
    LONGITUDE
  ) %>%
  inner_join(industrial_water_treatment_centers, by = "FACILITY_ID")

# making new df to set as geospacial points 
geospace_water_treatments <- water_treatment_industrial_final_dataset

# makes geospatial points of all locations 
coordinates(geospace_water_treatments) = c("LONGITUDE", "LATITUDE")
crs.geo1 = CRS("+proj=longlat")
proj4string(geospace_water_treatments) = crs.geo1
plot(geospace_water_treatments, pch = 20, col = "steelblue")
# END water locations 

# BEGIN Military Locations 
library(dplyr)

#load in military locations 
dod <- read.csv("pfas_progress_june_2021.csv")
mirta <- read.csv("~/Downloads/mirta_-223606765265040761.csv")

# merge together all of the same locations 
fireSite <- mirta %>%
  mutate(
    Installation_Name = Site.Name
  ) %>%
  inner_join(dod, by = "Installation_Name") %>%
  select(
    Installation_Name,
    OBJECTID,
    x,
    y
  )

# END Military Locations 


# Dataset of manufacturing locations that include those without a start date 

pfas_final_with_and_without_start_date <- pfas_facilities %>%
  filter(!is.na(LATITUDE83)) %>%
  filter(!is.na(LONGITUDE83)) %>%
  filter(ACCURACY_VALUE <= 1000) %>%
  distinct(REGISTRY_ID, .keep_all = TRUE)




# Adding together datasets 

Pfas_dataset_with_dates <- pfas_dataset

# pfas dataset of all locations 
Pfas_dataset_all <- bind_rows(
  pfas_final_with_and_without_start_date %>%
    transmute(
      ID   = REGISTRY_ID,
      lat  = LATITUDE83,
      lon  = LONGITUDE83,
      type = "Manufacturing Locations"
    ),
  water_treatment_industrial_final_dataset %>%
    transmute(
      ID   = FACILITY_ID,
      lat  = LATITUDE,
      lon  = LONGITUDE,
      type = "Water Treatment Center"
    ), 
  fireSite %>%
    transmute(
      ID = OBJECTID,
      lat = x, 
      lon = y, 
      type = "Military Fire Sites"
    )
)






