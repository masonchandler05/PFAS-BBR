# loading in packages 
library(tidyverse)
library(data.table)
library(sp)
library(zipcodeR)
library(dplyr)

## loading in datasets 
# UCMR5: 2023 - current 
UCMR_5 <- fread("PFAS_Project_Data/ucmr5/UCMR5_All.csv")
UCMR_5_locations <- fread("PFAS_Project_Data/ucmr5/UCMR5_ZIPCodes.txt")
UCMR_5_other_data <- fread("PFAS_Project_Data/ucmr5/UCMR5_AddtlDataElem.txt")

# UCMR4: 2018 - 2020
UCMR_4 <- fread("PFAS_Project_Data/ucmr5/UCMR5_All.csv")
UCMR_4_locations <- fread("PFAS_Project_Data/ucmr5/UCMR5_ZIPCodes.txt")
UCMR_4_other_data <- fread("PFAS_Project_Data/ucmr5/UCMR5_AddtlDataElem.txt")

# UCMR3: 2013-2015
UCMR_5 <- fread("PFAS_Project_Data/ucmr5/UCMR5_All.csv")
UCMR_5_locations <- fread("PFAS_Project_Data/ucmr5/UCMR5_ZIPCodes.txt")
UCMR_5_other_data <- fread("PFAS_Project_Data/ucmr5/UCMR5_AddtlDataElem.txt")

# UCMR2: 2008-2010
UCMR_5 <- fread("PFAS_Project_Data/ucmr5/UCMR5_All.csv")
UCMR_5_locations <- fread("PFAS_Project_Data/ucmr5/UCMR5_ZIPCodes.txt")
UCMR_5_other_data <- fread("PFAS_Project_Data/ucmr5/UCMR5_AddtlDataElem.txt")

# UCMR1: 2001-2005
UCMR_5 <- fread("PFAS_Project_Data/ucmr5/UCMR5_All.csv")
UCMR_1_locations <- fread("~/Downloads/ucmr1-occurrence-data/UCMR1_All.txt")
UCMR_1_other_data <- fread("~/Downloads/ucmr1-occurrence-data/UCMR1_Aeromonas_AddtlDataElem.txt")

# UCM_State Rounds 1/2: 1988-1997