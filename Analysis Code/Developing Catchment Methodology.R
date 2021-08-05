## Catchment Development

library(sf)
library(tidyverse)
library(tmap)
library(units)
library(SafeGraphR)
library(data.table)
library(Rcpp)
library(DBI)
library(duckdb)
source("Source Code/Helper Functions - Catchments.R")
options(connectionObserver = NULL)

## Setting up an SQL database
mydb <- dbConnect(duckdb::duckdb(), "Output Data/Patterns/Patterns.duckdb")

## Testing function
t <- get_patterns("DC", mydb, "july2021")


pts_query <- paste0("select* from SafeGraph_Cleaned_Places_US where region = '", state, "'") 
pts <- st_read("Output Data/SafeGraph_Cleaned_Places_US.gpkg", query = pts_query)


st_write(t, "t.gpkg")
st_write(pts, "pts.gpkg")



## Read in a test dataset to work with
duckdb::duckdb_read_csv(mydb, "july2021", "Output Data/Patterns/july_2021_patterns.csv")

## Play around with get query
test <- dbGetQuery(mydb, "SELECT * FROM july2021 WHERE region = 'MT'")
## Disconnect
dbDisconnect(mydb)





############ TESTING CENSUS BLOCK GROUPS

mt <- get_patterns(state = "MT", duckdb = mydb, week = "july2021")


ptns_clean <- mt %>%
  select(placekey, parent_placekey, raw_visit_counts, raw_visitor_counts,
         poi_cbg, visitor_home_cbgs, visitor_daytime_cbgs, visitor_home_aggregation) %>%
  mutate_all(na_if, "") %>%
  mutate(visitor_home_cbgs = gsub('"""', '', visitor_home_cbgs)) %>%
  mutate(visitor_daytime_cbgs = gsub('"""', '', visitor_daytime_cbgs)) %>%
  mutate(visitor_home_aggregation = gsub('"""', '', visitor_home_aggregation))
parents <- ptns_clean %>%
  as.data.frame() %>%
  select(-c(geometry, placekey)) %>%  
  filter(!is.na(parent_placekey))
ptns_noparents <- ptns_clean %>%
  filter(!parent_placekey %in% parents$parent_placekey)


poi_home_cbgs <- expand_cat_json(ptns_noparents, 
                                   expand = "visitor_home_cbgs",
                                   index = "visitor_home_cbg",
                                   by = "placekey")
parent_home_cbgs <- expand_cat_json(parents, 
                                      expand = "visitor_home_cbgs",
                                      index = "visitor_home_cbg",
                                      by = "parent_placekey")
parent_home_cbgs <- parent_home_cbgs %>% rename(placekey = parent_placekey)
home_cbgs <- rbind(poi_home_cbgs, parent_home_cbgs)

rc <- st_read("Output Data/Retail Centres/MT_RC_Boundaries.gpkg")

ptns_clean <- st_set_crs(ptns_clean, 4326)
p_rc <- st_intersection(ptns_clean, rc)
p_rc <- merge(p_rc, home_cbgs, by = "placekey", all.x = TRUE)
p_rc <- p_rc %>%
  rename(n.visits = visitor_home_cbgs.y) %>%
  drop_na("n.visits") %>%
  as.data.frame() %>%
  select(visitor_home_cbg, rcID, n.visits) %>%
  rename(Census_CBG = visitor_home_cbg) %>%
  distinct() %>%
  group_by(Census_CBG, rcID) %>%
  summarise(n.visits = sum(n.visits))


st_crs(rc)










t <- extract_catchments("MT")

out_df <- t %>%
  as.data.frame() %>%
  select(-c(geometry)) %>%
  filter(rcID == "30_111_RC_3") %>%
  mutate_if(is.character, as.factor)



t_map <- t %>% filter(rcID == "30_031_RC_15")
rc_map <- rc %>% filter(rcID == "30_031_RC_15")




tmap_mode("view")
tm_shape(t_map) +
  tm_fill(col = "orange", alpha = 0.4) +
  tm_shape(rc_map) +
  tm_fill(col = "black")


  
st_write(t, "t.gpkg")
rc <- st_read("Output Data/Retail Centres/MT_RC_Boundaries.gpkg")
rc <- rc %>% filter(rcID == "30_031_RC_15")

t_cent <- st_centroid(t)
t_cent <- t_cent %>% select(Census_Tract)
rc_cent <- st_centroid(rc)
rc_cent <- rc_cent %>% select(rcID)

m <- st_distance(rc_cent, t_cent)
rownames(m) <- rc_cent$rcID
colnames(m) <- t_cent$Census_Tract
m <- set_units(m, "km")
m <- as.data.frame(m)
rownames(m) <- rc_cent$rcID
colnames(m) <- t_cent$Census_Tract


m_long <- m %>%
  rownames_to_column() %>%
  rename(rcID = rowname) %>%
  gather(Census_Tract, Distance, -rcID, factor_key = FALSE) %>%
  mutate(Distance = gsub("[km]", "", Distance)) %>%
  mutate(Census_Tract = gsub("\\..*", "", Census_Tract)) %>%
  distinct() %>%
  arrange(rcID, Distance)


r <- merge(t, m_long, by = c("rcID", "Census_Tract"), all.x = TRUE)
rs <- r %>% filter(rcID == "30_001_RC_24") %>% as.data.frame() %>% select(-c(geometry))## Data


rc <- st_read("Output Data/Retail Centres/MT_RC_Boundaries.gpkg")
rc <- rc %>% select(rcID)
ptns <- st_read("Output Data/Patterns/MT_Patterns_July2021.gpkg")
all_ptns <- ptns %>% select(placekey)

## How to clean a set of patterns
full <- data.table::fread("Output Data/Patterns/july_2021_patterns.csv")
full_pts <- st_read("Output Data/SafeGraph_Cleaned_Places_US.gpkg")
full_pts <- full_pts %>% select(placekey)
full_pts_ptns <- merge(full_pts, full, by = "placekey", all.x = TRUE)

st_write(full_pts_ptns, "Output Data/Patterns/SafeGraph_Patterns_July2021.gpkg")

# 
# ptns <- full %>%
#   filter(region == "MT")
# pts <- read_points("MT")
# pts <- pts %>% select(placekey)
# mt_ptns <- merge(pts, ptns, by = "placekey", all.x = TRUE)
# st_write(mt_ptns, "Output Data/Patterns/MT_Patterns_July2021.gpkg")




