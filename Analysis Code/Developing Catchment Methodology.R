## Catchment Development

library(sf)
library(tidyverse)
library(tmap)
library(units)
library(scales)
library(SafeGraphR)
library(data.table)
library(Rcpp)
library(DBI)
library(duckdb)
source("Source Code/Helper Functions - Catchments.R")
options(connectionObserver = NULL)
mydb <- dbConnect(duckdb::duckdb(), "Output Data/Patterns/Patterns.duckdb")
options(scipen = 999)


## Get a retail centre & patterns to work with
rc <- st_read("Output Data/Retail Centres/WA_RC_Boundaries.gpkg")
rc <- rc %>% filter(rcID == "53_033_RC_1")
ptns <- get_patterns("WA", mydb, "july2021")

## Test function
m <- extract_catchments(rc, ptns, "WA", "block")

## Scale them
m_df <- m %>% as.data.frame() %>% select(rcID, Census_Tract, prop_visits, Distance)
m_df$visits <- rescale(m_df$visits, to = c(0, 1))
m_df$Distance <- rescale(m_df$Distance, to = c(0, 1))
m_df <- m_df %>%
  setNames(c("rcID", "Census_Tract", "r_Visits", "r_Distance"))

## Merge
final <- merge(m, m_df, by = c("rcID", "Census_Tract"))
final <- final %>% distinct() %>% mutate_at(c("r_Distance"), funs(. * -1))

## Create score
final$catchment <- final$r_Visits + final$r_Distance

final_df <- final %>%
  arrange(desc(catchment)) %>%
  mutate(t_catchment = sum(catchment)) %>%
  mutate(prop_catchment = (catchment / t_catchment) * 100) %>%
  mutate(cum_catch = cumsum(prop_catchment)) #%>%
  select(rcID, r_Visits, r_Distance, catchment, t_catchment, prop_catchment, cum_catch)


final_df$cum_catch <- rescale(final_df$cum_catch, to = c(0, 100))
map <- final_df %>% filter(cum_catch < 50) %>% filter(Distance < 30)

head(map)

tmap_mode("plot")
rc$newName <- "Seattle, WA"


m %>%
  filter(Distance < 200) %>%
  tm_shape() +
  tm_fill(col = "n.visits", palette = "Blues", n = 3, title = "Visits by Tract") +
  tm_shape(cbg_vt) +
  tm_fill(col = "white", alpha = 0.2) +
  tm_borders(col = "gray", alpha = 0.3) +
  tm_shape(rc) +
  tm_dots(size = 0.05) +
  tm_text("newName", fontface = "bold") +
  tm_layout(frame = FALSE)


tmap_mode("view")
tm_shape(map) +
  tm_fill(col = "orange", alpha = 0.5) +
  tm_add_legend("fill", labels = "Catchment", col = "orange", border.col = "orange") +
  tm_shape(cbg_vt) +
  tm_fill(col = "white", alpha = 0.2) +
  tm_borders(col = "gray", alpha = 0.3) +
  tm_shape(rc) +
  tm_dots(size = 0.05, alpha = 0.01) +
  tm_text("newName", fontface = "bold")
  

cbg_vt <- tigris::block_groups(state = "WA")
## Testing Census Block Variant
test <- extract_catchments("MT", "Block")
wtr <- st_read("Output Data/Water/WA/NHDArea.shp")
wtr2 <- st_read("Output Data/Water/WA/NHDWaterbody.shp")

tmap_mode("view")
tm_shape(map) +
  tm_fill(col = "orange", alpha = 0.5) +
  tm_shape(wtr) +
  tm_fill(col = "blue", alpha = 0.3) +
  tm_shape(wtr2) +
  tm_fill(col = "blue", alpha = 0.3)



to <- test[1:100, ]
to <- to %>%
  distinct()


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

dbListTables(mydb)



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


cbgs <- st_read("Input Data/Census Block Groups/US_Census_Block_Groups.gpkg")
cbgs <- cbgs %>%
  select(CBG_ID) %>%
  rename(Census_CBG = CBG_ID)


m <- merge(cbgs, p_rc, by = "Census_CBG", all.y = TRUE)
m <- m[!st_is_empty(m),,drop = FALSE]
m <- st_cast(m, "POLYGON")

m_df <- m %>%
  as.data.frame() %>%
  select(-c(geometry)) %>%
  mutate_if(is.character, as.factor) %>%
  distinct()

gd_cbgs <- merge(cbgs, m_df, by = "Census_CBG", all.y = TRUE)

grp_totals <- gd_cbgs %>%
  as.data.frame() %>%
  select(-c(geometry)) %>%
  group_by(rcID) %>%
  summarise(total.rc.visits = sum(n.visits)) %>%
  mutate_if(is.character, as.factor)

gd_cbgs <- merge(gd_cbgs, grp_totals, by = "rcID", all.x = TRUE)
gd_cbgs <- gd_cbgs %>%
  filter(n.visits > 4) %>%
  mutate(prop = (n.visits / total.rc.visits) * 100)

t <- gd_cbgs %>%
  filter(rcID == "30_049_RC_4")
st_write(t, "o.gpkg")

tmap_mode("view")
tm_shape(out_df) +
  tm_fill(col = "Catchment")


t <- extract_catchments("MT")

out_df <- t %>%
  arrange(desc(prop)) %>%
  mutate(Catchment = cumsum(prop)) %>%
  filter(Catchment <= 50)
  



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




