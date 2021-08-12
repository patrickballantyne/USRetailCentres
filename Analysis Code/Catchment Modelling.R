## Catchment Modelling 

library(sf)
library(tidyverse)
library(tmap)
library(hereR)
source("Source Code/Helper Functions - Catchments.R")
options(connectionObserver = NULL)
mydb <- dbConnect(duckdb::duckdb(), "Output Data/Patterns/Patterns.duckdb")
options(scipen = 999)
tempdir()
dir.create(tempdir())
set_key("5zgYrNtYojJ0DPRRAnufXo_dLijIAav_a6-3j-bg768")

## Read in the retail centres for a state
rc <- get_rc("ME")
rc_out <- rc %>%
  as.data.frame() %>%
  select(rcID, N.pts)

## Get the patterns 
ptns <- get_patterns("ME", mydb, "july2021")

## Get points
pts <- get_pts("ME")
pts <- pts %>% select(top_category, sub_category) %>% st_set_crs(4326)

# 1. Create an attractiveness indicator -----------------------------------

### OUR ATTRACTIVENESS INDICATOR WILL BE COMPRISED OF THREE ELEMENTS - 
### 1) SIZE (n.pts) 2) RETAIL OFFER (diversity of categories) and 
### 3) TOTAL VISITS

## Get attractiveness scores for ME centres
attr <- get_attractive("IL")

# 2. Euclidean Distances (CBG -> Retail Centre) -------------------------------------

## Get the Census Blocks
cbg <- tigris::block_groups("ME")
cbg <- cbg %>%
  select(GEOID) %>%
  rename(Census_Block_Group = GEOID) %>%
  st_transform(4326)

## Retail Centres
get_rc("ME")

## Calculate euclidean distances between all Census Blocks and Retail Centres
euc <- get_euclidean(rc, cbg)



# 3. Network Distances ----------------------------------------------------

## Use HereR to calculate routes between them 
dist <- get_network(rc[1:10, ], cbg)

## Attach to the Census Blocks to check they're correct
cbg_dist <- merge(cbg, dist, by = "Census_Block_Group", all.y = TRUE)


# 4. Parameter Calibration  -----------------------------------------------



# 5. Huff Modelling -------------------------------------------------------

## Merge datasets together
rc_out <- merge(rc_out, dist, by = "rcID", all.y = TRUE)
rc_out <- merge(rc_out, attr, by = "rcID", all.x = TRUE)


## Prepare for experimentation on huff model
huff_input <- rc_out %>% 
  select(rcID, Census_Block_Group, attr_score, Distance) %>%
  mutate(Distance = ifelse(Distance == 0, 0.01, Distance))

## Run huff model
huff_probs <- get_huff(huff_input, 1 , 2)

## Merge onto shapefile
huff_cbg <- merge(cbg, huff_probs, by = "Census_Block_Group", all.y = TRUE)
st_write(huff_cbg, "huff.gpkg")
