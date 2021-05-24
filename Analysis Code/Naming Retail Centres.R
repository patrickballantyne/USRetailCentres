## Naming Centres

library(tidyverse)
library(sf)
library(tigris)
library(tmap)
tmap_mode("view")
source("Source Code/Helper Functions - Delineation.R")
source("Source Code/Helper Functions - Naming Strategy.R")

get_names("ID")


## Read in some centres
rc <- st_read("Output Data/Retail Centres/MT_RC_Boundaries.gpkg")
rc_clean <- rc %>%
  select(rcID)

## Tigris - get the geography we want to aggregate these to
places <- tigris::places("MT")
places <- places %>%
  select(NAMELSAD) %>%
  rename(place_name = NAMELSAD) %>%
  mutate(place_name = gsub("CDP", '', place_name)) %>%
  mutate(place_name = gsub("city", 'City', place_name)) %>%
  mutate(place_name = gsub("town", 'Town', place_name)) %>%
  st_transform(4326)
  
## Read in the POIs
pts <- read_points("MT")
pts <- pts %>% select(safegraph_place_id, street_address, city, region, postal_code)

## Clean the addresses
pts_clean <- pts
pts_clean$street_address_clean <- sub(".*? ", "", pts_clean$street_address) 
pts_clean <- pts_clean %>%
  select(-c(street_address))

## Perform intersection
m <- st_intersection(pts_clean, rc)
m$street_address_clean <- as.factor(m$street_address_clean)

## Count number of individual street appearances per retail centre
streets <- m %>% 
  as.data.frame() %>%
  select(street_address_clean, rcID) %>%
  group_by(rcID) %>%
  count(street_address_clean) %>%
  top_n(n = 1) %>%
  group_by(rcID) %>%
  summarise(street_address_clean = paste0(street_address_clean, collapse = " / "), n = n) %>%
  distinct(street_address_clean) %>%
  rename(street_name = street_address_clean)

## Merge on street names
rc_clean <- merge(rc_clean, streets, by = "rcID")

## Merge on place names
pa <- st_intersection(rc_clean, places)
pa <- pa %>%
  as.data.frame() %>%
  group_by(rcID) %>%
  summarise(place = paste0(place_name, collapse = " / "), street = street_name) %>%
  distinct(place)

## Merge on place names
rc_clean <- merge(rc_clean, pa, by = "rcID")

## Merge on county and state
rc_m <- rc %>%
  as.data.frame() %>%
  select(rcID, State, County) %>%
  rename(state = State, county = County)
rc_clean <- merge(rc_clean, rc_m, by = "rcID")

## Pull out unique ID 
rc_clean <- rc_clean %>%
  rename(rcID_full = rcID) %>%
  mutate(rcID = substr(rcID_full, 8, 12:13)) %>%
  mutate(rcID = gsub("_", "", rcID)) %>%
  rename(street = street_name) %>%
  select(rcID_full, rcID, street, place, county, state) 


## Adjust and create retail centre name
rc_clean$rcName <- as.factor(with(rc_clean, paste("(",rcID,")", street, ",", place, ",", county, ",", state)))
rc_clean <- rc_clean %>%
  rename(identifier = rcID_full) %>%
  select(identifier, rcID, rcName, street, place, county, state)
st_write(rc_clean, "names.gpkg")
