## Getting Typology Variables

library(tmap)
library(sf)
library(tidyverse)
library(data.table)
library(tidycensus)
options(scipen = 999)
source("Source Code/Helper Functions - Typology.R")
tempdir()
dir.create(tempdir())

## Read in the patterns
pats <- fread("Output Data/Patterns/july_2021_patterns.csv")

typ <- prep4typology("NV", pats)
rc <- get_rc("NV")
rc <- rc %>%
  select(rcID, rcName)
rc_typ <- merge(rc, typ, by = "rcID")
st_write(rc_typ, "rc_typ.gpkg")
## Get variables

### North-East
ne_out <- lapply(ne, prep4typology)
ne_out_df <- do.call(rbind, ne_out)
#write.csv(ne_out_df, "Output Data/Typology/Regional Variables/NE_typ.csv")

### South
s_out <- lapply(s, prep4typology)
s_out_df <- do.call(rbind, s_out)
write.csv(s_out_df, "Output Data/Typology/Regional Variables/S_typ.csv")

### West
w_out <- lapply(w, prep4typology)
w_out_df <- do.call(rbind, w_out)
write.csv(w_out_df, "Output Data/Typology/Regional Variables/W_typ.csv")

### Mid-West
mw_out <- lapply(mw, prep4typology)
mw_out_df <- do.call(rbind, mw_out)
write.csv(mw_out_df, "Output Data/Typology/Regional Variables/MW_typ.csv")

### US
us_typ <- rbind(ne_out_df, s_out_df, w_out_df, mw_out_df)
write.csv(us_typ, "Output Data/Typology/US_Typology_EXTRA_Variables.csv")
write.csv(us_typ, "Output Data/Typology/US_Typology_Variables_minPts50.csv")


### Assemble Final 
us_typ_orig <- read.csv("Output Data/Typology/US_Typology_Variables_minPts50.csv")
us_full <- merge(us_typ_orig, us_typ, by = "rcID")

us_full <- us_full %>%
  select(-c(X)) %>%
  select(rcID, 
         propClothingandFootwear, propDIYandHousehold, propElectrical, propRecreational,
         propChemist, propCTNandGasoline, propFood, propOffLicence,
         propBars, propRestaurant, propFastFood, propEntertainment, propFitness,
         propConsumerService, propHouseholdService, propBusinessService,
         propIndependent, propSmallMultiple, propNationalChain, propPopularBrands, nationalCatDiversity, localCatDiversity,
         nUnits, roeckScore, distanceTravelled, retailDensity, residentialDensity, employmentDensity, retailemploymentDensity, roadDensity, transitDistance, propDiscount, propAnchor,
         lowIncome, retailService, totalVisits, medianDwell)
write.csv(us_full, "Output Data/Typology/US_Typology_Variables_minPts50_NEW.csv")

us_typ100 <- us_full %>%
  filter(nUnits >= 100)
write.csv(us_typ100, "Output Data/Typology/US_Typology_Variables_minPts100_NEW.csv")
