# Load Packages 
library(tidyverse)
library(data.table)
library(sp)
library(zipcodeR)
library(dplyr)
library(tidygeocoder)

# Load in df
EPA_Locations <- fread("~/Downloads/SDWA_latest_downloads (2)/SDWA_PUB_WATER_SYSTEMS.csv")

# Create function to get coordinates
get_coords <- function(addr1, addr2, city, state, zip) {
  # Try ADDRESS_LINE1
  if(!is.na(addr1) && addr1 != "") {
    geo <- tryCatch(geocode(paste(addr1, city, state, zip, sep = ", "), method = 'osm'),
                    error = function(e) tibble(lon = NA, lat = NA))
    if(!is.na(geo$lon)) return(tibble(lon = geo$lon, lat = geo$lat))
  }
  
  # Try ADDRESS_LINE2
  if(!is.na(addr2) && addr2 != "") {
    geo <- tryCatch(geocode(paste(addr2, city, state, zip, sep = ", "), method = 'osm'),
                    error = function(e) tibble(lon = NA, lat = NA))
    if(!is.na(geo$lon)) return(tibble(lon = geo$lon, lat = geo$lat))
  }
  
  # Fall back to ZIP_CODE
  if(!is.na(zip) && zip != "") {
    zip_info <- tryCatch(reverse_zipcode(zip),
                         error = function(e) tibble(lng = NA, lat = NA))
    return(tibble(lon = zip_info$lng, lat = zip_info$lat))
  }
  
  return(tibble(lon = NA, lat = NA))
}

# Apply efficiently
EPA_Exact_Location <- EPA_Locations %>%
  mutate(PWSID = str_extract(PWSID, "\\d+")) %>%
  mutate(coords = pmap(list(ADDRESS_LINE1, ADDRESS_LINE2, CITY_NAME, STATE_CODE, ZIP_CODE), get_coords)) %>%
  unnest_wider(coords) %>%
  select(PWSID, lon, lat)