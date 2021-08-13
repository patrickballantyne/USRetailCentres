## Catchment Modelling 

library(sf)
library(tidyverse)
library(tmap)
library(hereR)
library(SafeGraphR)

source("Source Code/Helper Functions - Catchments.R")
options(connectionObserver = NULL)
mydb <- dbConnect(duckdb::duckdb(), "Output Data/Patterns/Patterns.duckdb")
options(scipen = 999)
tempdir()
dir.create(tempdir())]
tmap_mode("view")
set_key("5zgYrNtYojJ0DPRRAnufXo_dLijIAav_a6-3j-bg768")

## Read in the retail centres for a state
rc <- get_rc("NE")
rc_out <- rc %>%
  as.data.frame() %>%
  select(rcID, N.pts)

# ## Get the patterns 
# ptns <- get_patterns("ME", mydb, "july2021")
# 
# ## Get points
# pts <- get_pts("ME")
# pts <- pts %>% select(top_category, sub_category) %>% st_set_crs(4326)

# 1. Create an attractiveness indicator -----------------------------------

### OUR ATTRACTIVENESS INDICATOR WILL BE COMPRISED OF THREE ELEMENTS - 
### 1) SIZE (n.pts) 2) RETAIL OFFER (diversity of categories) and 
### 3) TOTAL VISITS

## Get attractiveness scores for ME centres
attr <- get_attractive("NE")

# 2. Euclidean Distances (CBG -> Retail Centre) -------------------------------------

## Get the Census Blocks
cbg <- tigris::block_groups("NE")
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
dist <- get_network(rc, cbg)

## Attach to the Census Blocks to check they're correct
cbg_dist <- merge(cbg, dist, by = "Census_Block_Group", all.y = TRUE)


# 4. Parameter Calibration  -----------------------------------------------

## Get the patterns
ne_ptns <- get_patterns("NE", mydb, "july2021")
ptns_clean <- ne_ptns %>%
  as.data.frame() %>%
  select(placekey, raw_visit_counts, visitor_home_cbgs) %>%
  mutate_all(na_if, "") 

## Clean the patterns
poi_home_cbgs <- expand_cat_json(ptns_clean,
                                 expand = "visitor_home_cbgs",
                                 index = "visitor_home_cbg",
                                 by = "placekey")

## Get the retail centers, and list the intersecting places
rc_clean <- rc %>% select(rcID)
ne_pts <- get_pts("NE")
ne_pts <- ne_pts %>%
  select(placekey) %>%
  st_set_crs(4326)
int <- st_intersection(ne_pts, rc_clean)
int <- int %>% 
  as.data.frame() %>%
  select(rcID, placekey)

## Merge the rc names onto the list of pattern
test <- merge(poi_home_cbgs, int, by = "placekey", all.y = TRUE)
test_g <- test %>%
  group_by(rcID, visitor_home_cbg) %>%
  summarise(CBG_RC_visits = sum(visitor_home_cbgs)) %>%
  setNames(c("rcID", "Census_Block_Group", "Total_Visits_RC"))

test_cbg <- test %>%
  group_by(visitor_home_cbg) %>%
  summarise(CBG_visits = sum(visitor_home_cbgs)) %>%
  setNames(c("Census_Block_Group", "Total_Visits_CBG"))


cbg_merge <- merge(cbg, test_cbg, by = "Census_Block_Group", all.x = TRUE)
cbg_merge <- merge(cbg_merge, test_g, by = "Census_Block_Group", all.x = TRUE)
cbg_merge <- cbg_merge %>%
  select(Census_Block_Group, rcID, Total_Visits_CBG, Total_Visits_RC) %>%
  mutate(Prop_Visits_RC = (Total_Visits_RC / Total_Visits_CBG) * 100)


## check it? 
calib <- cbg_merge %>%
  filter(rcID == "31_157_RC_49")
tm_shape(calib) +
  tm_fill(col = "Prop_Visits_RC")

st_write(calib, "calib.gpkg")


# 5. Huff Modelling -------------------------------------------------------

## Merge datasets together
rc_out <- rc
rc_out <- merge(rc_out, dist, by = "rcID", all.y = TRUE)
rc_out <- merge(rc_out, attr, by = "rcID", all.x = TRUE)


## Prepare for experimentation on huff model
huff_input <- rc_out %>% 
  as.data.frame() %>%
  select(rcID, Census_Block_Group, attr_score, Distance) %>%
  mutate(Distance = ifelse(Distance == 0, 0.01, Distance))

## Run huff model
huff_probs <- get_huff(huff_input, 1 , 2)
summary(huff_probs)
## Merge onto shapefile
huff_cbg <- merge(cbg, huff_probs, by = "Census_Block_Group", all.y = TRUE)

head(rc)
## Extract subset
subset <- huff_cbg %>%
  filter(rcID == "31_055_RC_3" | rcID == "31_157_RC_49" | rcID == "31_101_RC_50" | rcID == "31_079_RC_4")
st_write(subset, "huff.gpkg")




# 6. RSME and Pearson's R -------------------------------------------------

## Join observed and modelled together
calib <- calib %>%
  as.data.frame() %>%
  select(rcID, Census_Block_Group, Prop_Visits_RC)
cbg_stat <- subset %>%
  select(rcID, Census_Block_Group, huff_probability) %>%
  rename(Model_Huff_Probability = huff_probability)
cbg_stat <- merge(cbg_stat, calib, by = c("Census_Block_Group", "rcID"), all.x = TRUE)
cbg_stat <- cbg_stat %>%
  rename(Observed_Huff_Probability = Prop_Visits_RC) %>%
  mutate(Observed_Huff_Probability = replace_na(Observed_Huff_Probability, 0))
cbg_stat$Observed_Huff_Probability <- scales::rescale(cbg_stat$Observed_Huff_Probability, to = c(0, 1))


## Compute RMSE
rmse <- sqrt(mean((cbg_stat$Observed_Huff_Probability - cbg_stat$Model_Huff_Probability)^2))

## Compute Pearson's R
pearson <- cor.test(cbg_stat$Observed_Huff_Probability, cbg_stat$Model_Huff_Probability,
                    method = "pearson")
pearson
