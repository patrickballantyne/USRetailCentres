## Catchment Development

library(sf)
library(tidyverse)
library(tmap)
library(SafeGraphR)
source("Source Code/Helper Functions - Delineation.R")
tmap_mode("view")


## Data
rc <- st_read("Output Data/Retail Centres/MT_RC_Boundaries.gpkg")
rc <- rc %>% select(rcID)
ptns <- st_read("Output Data/Patterns/MT_Patterns_July2021.gpkg")
all_ptns <- ptns %>% select(placekey)

## How to clean a set of patterns
# full <- data.table::fread("Output Data/Patterns/july_2021_patterns.csv")
# ptns <- full %>%
#   filter(region == "MT")
# pts <- read_points("MT")
# pts <- pts %>% select(placekey)
# mt_ptns <- merge(pts, ptns, by = "placekey", all.x = TRUE)
# st_write(mt_ptns, "Output Data/Patterns/MT_Patterns_July2021.gpkg")


# 1. Clean the patterns ---------------------------------------------------

## Convert all empties to NA, and fix extra speechmarks in patterns
ptns <- ptns %>%
  select(placekey, parent_placekey, raw_visit_counts, raw_visitor_counts,
         poi_cbg, visitor_home_cbgs, visitor_daytime_cbgs, visitor_home_aggregation) %>%
  mutate_all(na_if, "") %>%
  mutate(visitor_home_cbgs = gsub('"""', '', visitor_home_cbgs)) %>%
  mutate(visitor_daytime_cbgs = gsub('"""', '', visitor_daytime_cbgs)) %>%
  mutate(visitor_home_aggregation = gsub('"""', '', visitor_home_aggregation))


## Clean parents - pulling in all the variables at parent level
parents <- ptns %>%
  as.data.frame() %>%
  select(-c(geom, placekey)) %>%  
  filter(!is.na(parent_placekey))

## Filter out the parents from the main dataset
ptns_noparents <- ptns %>%
  filter(!parent_placekey %in% parents$parent_placekey)

## Expand data for pois - SafeGraphR
poi_home_cbgs <- expand_cat_json(ptns_noparents, 
                                    expand = "visitor_home_cbgs",
                                    index = "visitor_home_cbg",
                                    by = "placekey")
poi_home_tracts <- expand_cat_json(ptns_noparents, 
                                      expand = "visitor_home_aggregation",
                                      index = "visitor_home_tract",
                                      by = "placekey")

## Expand data for parents - SafeGraphR
parent_home_cbgs <- expand_cat_json(parents, 
                     expand = "visitor_home_cbgs",
                     index = "visitor_home_cbg",
                     by = "parent_placekey")
parent_home_cbgs <- parent_home_cbgs %>% rename(placekey= parent_placekey)
parent_home_tracts <- expand_cat_json(parents, 
                                      expand = "visitor_home_aggregation",
                                      index = "visitor_home_tract",
                                      by = "parent_placekey")
parent_home_tracts <- parent_home_tracts %>% rename(placekey= parent_placekey)

## Join POI and Parents together
home_cbgs <- rbind(poi_home_cbgs, parent_home_cbgs)
home_tracts <- rbind(poi_home_tracts, parent_home_tracts)


# 2. Cleaning the Census Geographies --------------------------------------

## Read in the geodemographics shapefile, to get tracts
geodemo <- st_read("Input Data/Geodemographics/US_Geodemographic_Classification.gpkg")
geodemo <- geodemo %>%
  select(GEOID10) %>%
  rename(Census_Tract = GEOID10)

## NEED TO FIND A CBG SHAPEFILE FOR THE US?





# 3. Identifying Patterns in Retail Centres  ---------------------------------

## Identify those in retail centres
p_rc <- st_intersection(all_ptns, rc)

## Merge on the visit counts by tract
p_rc <- merge(p_rc, home_tracts, by = "placekey", all.x = TRUE)
p_rc <- p_rc %>%
  rename(n.visits = visitor_home_aggregation) %>%
  drop_na("n.visits")

## Group - calculating total visits to each tract by retail centre
grp <- p_rc %>%
  as.data.frame() %>%
  select(-c(geometry)) %>%
  group_by(rcID, visitor_home_tract) %>%
  summarise(total.visits = sum(n.visits))



# 4. Visualising Catchments -----------------------------------------------

## Extract a retail centre
test <- grp %>%
  filter(rcID == "30_111_RC_6") %>%
  rename(Census_Tract = visitor_home_tract)
test_rc <- rc %>%
  filter(rcID == "30_111_RC_6")

## Merge onto tracts
tract_test <- merge(geodemo, test, by = "Census_Tract", all.y = TRUE)

## Map full geographical reach 
tm_shape(tract_test) +
  tm_fill(col = "total.visits") +
  tm_shape(test_rc) +
  tm_fill(col = "black")


# 5. Extracting Refined Catchments ----------------------------------------

## Calculate total visits in each RC
grp_totals <- grp %>%
  group_by(rcID) %>%
  summarise(total.rc.visits = sum(total.visits))

## Merge on totals
tract_test <- merge(tract_test, grp_totals, by = "rcID", all.x = TRUE)

## Compute proportion visits occupied by each tract, sort and calculate cumulatives
out <- tract_test %>%
  mutate(prop_visits = (total.visits / total.rc.visits) * 100) %>%
  arrange(desc(prop_visits)) %>%
  mutate(cumulative = cumsum(prop_visits)) %>%
  filter(cumulative <= 70)

## Map 
tm_shape(test_rc) +
  tm_fill(col = "black") +
  tm_shape(out) +
  tm_fill(col = "orange", alpha = 0.2) +
  tm_borders(col = "orange")
