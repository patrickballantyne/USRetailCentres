## Catchment Modelling 

library(sf)
library(tidyverse)
library(tmap)
library(hereR)
library(vroom)
library(SafeGraphR)
source("Source Code/Helper Functions - Catchments.R")
tmap_mode("view")
options(scipen = 999)
set_key("5zgYrNtYojJ0DPRRAnufXo_dLijIAav_a6-3j-bg768")


# 1. Observed Patronage - SafeGraph 'weekly patterns' ---------------------

## Pull in census blocks for calibration area
w_tracts <- st_read("Output Data/Catchments/West_Tracts.gpkg")

## Patterns
july2021 <- get_patterns("July2021")

## Extract and upack those for calibration area
july2021_w <- july2021 %>%
  filter(state %in% w)

###### Compute by census tract ################################

## Expand using SafeGraphR
july2021_w <- expand_cat_json(july2021_w,
                             expand = "visitor_home_aggregation",
                             index = "CensusTract",
                             by = c("placekey", "raw_visit_counts"))

## Compute total visits to each census tract
tract_total_visits <- july2021_w %>%
  select(CensusTract, raw_visit_counts) %>%
  group_by(CensusTract) %>%
  summarise(TotalVisitsTract = sum(raw_visit_counts))

## Compute total visitors from census tracts
tract_total_visitors <- july2021_w %>%
  select(CensusTract, visitor_home_aggregation) %>%
  rename(totalVisits = visitor_home_aggregation) %>%
  group_by(CensusTract) %>%
  summarise(totalVisitorsTract = sum(totalVisits))

## Join together to calculate average visits
join <- merge(tract_total_visitors, tract_total_visits, by = "CensusTract")
join$averageVisits <- join$TotalVisitsTract / join$totalVisitorsTract

## Merge onto census block groups, and replace NAs
out <- merge(w_tracts, join, by = "CensusTract", all.x = TRUE)
out <- out %>% mutate(totalVisitsTract = replace_na(totalVisitsTract, 0))


###### Now you need to compute by retail centre and census tract ################

## Pull in retail centres for calibration zone
rc <- st_read("Output Data/Retail Centres/US Retail Centres/US_RC_minPts50.gpkg")
rc <- rc %>%
  filter(State %in% w) %>%
  select(rcID) %>%
  st_transform(4326)

## Read in the points for those states
pts <- lapply(w, get_pts)
pts <- do.call(rbind, pts)
pts <- pts %>%
  select(placekey) %>%
  st_set_crs(4326)

## Perform intersection
int <- st_join(pts, rc)
int <- int %>%
  as.data.frame() %>%
  select(placekey, rcID) %>%
  filter(!is.na(rcID))

## Merge on the census tract counts
ptn_out <- merge(july2021_w, int, by = "placekey", all.y = TRUE)
ptn_out <- ptn_out %>%
  setNames(c("placekey", "CensusTract", "totalVisits", "totalVisitors", "rcID"))

## Merge on the total census tract counts
ptn_out_merge <- merge(ptn_out, tract_total_visits, by = "CensusTract", all.x = TRUE)
ptn_out_merge <- ptn_out_merge %>% filter(!is.na(TotalVisitsTract))

## Compute total visits to each retail centre from each tract
rc_total <- ptn_out_merge %>%
  group_by(rcID, CensusTract, TotalVisitsTract) %>%
  summarise(totalVisitorsRC = sum(totalVisitors)) %>%
  mutate(averageVisits = TotalVisitsTract / totalVisitorsRC)


rc_sub <- rc_total %>%
  filter(rcID == "02_020_RC_1")
m <- merge(w_tracts, rc_sub, by = "CensusTract", all.x = TRUE)
st_write(m, "m.gpkg")
# 1. Create an attractiveness indicator -----------------------------------

### OUR ATTRACTIVENESS INDICATOR WILL BE COMPRISED OF THREE ELEMENTS - 
### 1) SIZE (n.pts) 2) RETAIL OFFER (diversity of categories) and 
### 3) TOTAL VISITS

## Get attractiveness scores for ME centres
attr <- get_attractive("NM")

# 2. Euclidean Distances (CBG -> Retail Centre) -------------------------------------

# ## Get the Census Blocks
# cbg <- tigris::block_groups("NE")
# cbg <- cbg %>%
#   select(GEOID) %>%
#   rename(Census_Block_Group = GEOID) %>%
#   st_transform(4326)
# 
# ## Retail Centres
# get_rc("ME")
# 
# ## Calculate euclidean distances between all Census Blocks and Retail Centres
# euc <- get_euclidean(rc, cbg)

# 3. Network Distances ----------------------------------------------------

## Get network distances for all census block groups within a 50km radius of the centre
dist <- get_network(rc_sub, state = "NM")

## Attach to the Census Blocks to check they're correct
#cbg_dist <- merge(cbg, dist, by = "Census_Block_Group", all.y = TRUE)


# 4. Huff Modelling (Predicted Behaviour)  -----------------------------------------------

# ## Pull data in together
# rc_out <- rc %>%
#   as.data.frame() %>%
#   select(-c(geom)) %>%
#   select(rcID)
# rc_out <- merge(rc_out, attr, by = "rcID")
# rc_out <- merge(rc_out, dist, by = "rcID", all.y= TRUE)
# 
# ## Run once you have all distances/attractiveness data- static alpha & beta
# huff_static <- get_huff(rc_out, alpha = 1, beta = 2)

## Run huff model with varying params
# huff_flexi <- huff_experiment(huff_static)

## DO NOT USE!!!
### Run all functions together
huff_together <- get_predicted_patronage("AK")

# 5. Observed Patronage -------------------------------------------------------

## Get the observed patronage - based on SafeGraph patterns
test <- get_observed_patronage("AK")


# 6. RSME and Pearson's R -------------------------------------------------

## Join observed and modelled together
cbg_stat <- merge(cbg, huff_together, by = "Census_Block_Group", all.y = TRUE)
cbg_stat <- merge(cbg_stat, test, by = "Census_Block_Group", all.x = TRUE)
cbg_stat <- cbg_stat %>%
  select(Census_Block_Group, rcID.x, Prop_Visits_RC, huff_probability) %>%
  rename(rcID = rcID.x, observed_probability = Prop_Visits_RC, predicted_probability = huff_probability) %>%
  mutate(observed_probability = replace_na(observed_probability, 0))
cbg_stat$observed_probability <- scales::rescale(cbg_stat$observed_probability, to = c(0, 1))


## Compute RMSE
rmse <- sqrt(mean((cbg_stat$Observed_Huff_Probability - cbg_stat$Model_Huff_Probability)^2))

## Compute Pearson's R
pearson1 <- cor.test(cbg_stat$Observed_Huff_Probability, cbg_stat$Model_Huff_Probability,
                    method = "pearson")
pearson2 <- cor.test(cbg_stat$Observed_Huff_Probability, cbg_stat$Model_Huff_Probability,
                     method = "pearson")


pearson1$estimate
pearson2$estimate

t <- rbind(pearson1$estimate, pearson2$estimate)
#####

## Get huff values with different a & b values
o <- huff_experiment(huff_input)

## Function 
test <- split(o,list(o$alpha,o$beta))



###################################
## old code - observed patterns w/ buffers

## Merge onto census blocks
cbg <- st_read("Input Data/Census Block Groups/US_Census_Block_Groups.gpkg")
cbg <- cbg %>% rename(Census_Block_Group = CBG_ID) %>% st_transform(32616)
cbg_ptn <- merge(cbg, ptn_out, by.x = "Census_Block_Group", by.y = "visitor_home_cbg", all.y = TRUE)

## For each retail centre, identify only those blocks within the 50km radius
rc <- rc %>% st_transform(32616)
rc_ls <- split(rc, seq(nrow(rc)))
main_blocks <- lapply(rc_ls, function(x) {
  
  ## Create a buffer for each retail centre (50km)
  rc_buffer <- st_transform(st_buffer(x, 50000))
  
  ## Get blocks in the buffer
  blocks_sub <- st_intersection(cbg_ptn, rc_buffer)
  blocks_sub })
main_blocks <- do.call(rbind, main_blocks)

## Compute total visits by retail centre & census block group
ptn_out_group <- main_blocks %>%
  as.data.frame() %>%
  select(rcID, Census_Block_Group, visitor_home_cbgs) %>%
  group_by(rcID, Census_Block_Group) %>%
  summarise(CBG_RC_visits = sum(visitor_home_cbgs)) %>%
  setNames(c("rcID", "Census_Block_Group", "Total_Visits_RC"))
## Compute total visits by census block group
ptn_out_cbg <- main_blocks %>%
  as.data.frame() %>%
  select(Census_Block_Group, visitor_home_cbgs) %>%
  group_by(Census_Block_Group) %>%
  summarise(CBG_visits = sum(visitor_home_cbgs)) %>%
  setNames(c("Census_Block_Group", "Total_Visits_CBG"))


cbg_merge <- merge(cbg, ptn_out_group, by = "Census_Block_Group", all.y = TRUE)
cbg_merge <- merge(cbg_merge, ptn_out_cbg, by = "Census_Block_Group", all.x = TRUE)
cbg_merge <- cbg_merge %>%
  mutate(prop_visits = (Total_Visits_RC / Total_Visits_CBG) * 100) %>%
  arrange(rcID)

head(cbg_merge)

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

