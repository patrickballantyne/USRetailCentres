## Naming Centres

library(tidyverse)
library(sf)
library(tigris)
library(tmap)
library(hereR)
tmap_mode("view")
source("Source Code/Helper Functions - Delineation.R")
source("Source Code/Helper Functions - Naming Strategy.R")
set_key("j9XT_bIs1olds-HkJ0x4z_1nG3maemirCpWyfsqcNP8")

## CHECK: API Key is working first!

## Then supply list of states you want to extract the names for:
state_ls <- list("DC", "RI")
## Run function across list - it will write out the boundaries w/ names for you, 
## no need to assign objects
lapply(state_ls, get_geocoded_names)

get_geocoded_names("DC")

ms <- t %>% filter()












st_write(t, "t.gpkg")


#### Testing mall code
identifier = "WY"
rc <- st_read(paste0("Output Data/Retail Centres/", identifier, "_RC_Boundaries.gpkg"))
rc <- st_transform(rc, 4326)

## Read in all places and filter to Malls
# all_places <- st_read("Output Data/SafeGraph_Cleaned_Places_US.gpkg")
# malls <- all_places %>%
#   filter(sub_category == "Malls")
# st_write(malls, "Output Data/US_Malls.gpkg")

malls <- st_read("Output Data/SafeGraph_Cleaned_Places_US.gpkg")
malls_sub <- malls %>% filter(region == identifier) %>%
  select(location_name, sub_category) %>%
  st_transform(4326)

st_crs(rc)

rc_malls <- st_intersection(malls_sub, rc)
