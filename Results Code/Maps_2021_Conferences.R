## Maps for 2021 Conferences

library(tmap)
install.packages("tmap")
library(tidyverse)
library(sf)
install.packages("tmaptools")
library(ceramic)
library(extrafont)
source("Source Code/Helper Functions - Delineation.R")
Sys.setenv(MAPBOX_API_KEY="pk.eyJ1Ijoic2dwYmFsbGEiLCJhIjoiY2tncnZxc3FqMGhzaTJ6bzcxNTk3bDNldCJ9.tYEAVCBey8y5tzXx9i0lPw")
windowsFonts("Times" = windowsFont("Times New Roman"))
tmap_mode("plot")
## Read in the dataset
typ <- st_read("Output Data/Typology/NE_Typology.gpkg")


# 1. Methods Maps ---------------------------------------------------------

## Read in data
pts <- read_points("NH")
bdgs <- st_read("Output Data/Buildings/NH_Retail_Buildings.gpkg")


# 1. Group 1 Maps ---------------------------------------------------------

## Boston, MA
make_case_study_maps <- function(state, rc_list, typology = FALSE) {
  
  ## Pull out the retail centres we want to map
  typ <- st_read("Output Data/Typology/NE_Typology.gpkg")
  typ <- typ %>% filter(rcID %in% rc_list)
  typ <- typ %>%
    mutate(group_name = case_when(group_id == 1 ~ "1. Inner City & Historic Centres",
                                  group_id == 2 ~ "2. Neighbourhood Centres",
                                  group_id == 3 ~ "3. Comparison Destinations")) %>%
    mutate(type_name = case_when(type_id == 1.1 ~ "1.2. Secondary Inner City/Historic Retail Centres",
                                 type_id == 1.2 ~ "1.1. Primary Inner City/Historic Retail Centres",
                                 type_id == 2.1 ~ "2.1. Diverse Neighbourhood Centres",
                                 type_id == 2.2 ~ "2.2. Neighbourhood Convenience Centres",
                                 type_id == 3.1 ~ "3.2. Secondary Comparison Destinations",
                                 type_id == 3.2 ~ "3.1. Leading Comparison Destinations")) %>%
    mutate_if(is.character, as.factor)
  
  ## Bbox 
  in_bbox <- typ %>% summarise()
  in_bbox <- st_transform(in_bbox, crs = "+init=epsg:4326")
  out_bbox <- st_as_sfc(st_bbox(typ, n = 1))
  sp <- as(out_bbox, 'Spatial')
  
  ## Basemap
  basemap <- cc_location(loc = sp, zoom = 14,
                         base_url = "https://basemaps.cartocdn.com/rastertiles/light_all/{zoom}/{x}/{y}.png")
  
  ## Read in features
  pts <- read_points(state = state)
  bdgs <- st_read(paste0("Output Data/Buildings/", state, "_Retail_Buildings.gpkg"))
  
  ## Clip 
  in_pts <- st_intersection(pts, in_bbox)
  in_bdgs <- st_intersection(bdgs, in_bbox)
  in_bdgs <- in_bdgs %>%
    summarise()
  
  out_pts <- st_intersection(pts, out_bbox)
  out_pts <- out_pts %>%
    filter(!safegraph_place_id %in% in_pts$safegraph_place_id)
  out_bdgs <- st_intersection(bdgs, out_bbox)
  out_bdgs <- out_bdgs %>%
    filter(!safegraph_place_id %in% in_bdgs$safegraph_place_id) %>%
    summarise()
  
  
  ## Map
  if(typology == FALSE) {

    p1 <- tm_shape(basemap) +
      tm_rgb() +
      tm_shape(typ) +
      tm_fill(col = "gray", alpha = 0.3) +
      tm_borders(col = "orange", lwd = 2.0) +
      tm_add_legend("fill", col = "white", border.col = "orange", labels = "Retail Centres") +
      tm_shape(in_bdgs) +
      tm_fill(col = "black", alpha = 0.15) +
      tm_add_legend("fill", col = "gray", border.col = "gray", labels = "Retail Buildings") +
      tm_shape(in_pts) +
      tm_dots(col = "black", size = 0.005, alpha = 0.25) +
      tm_add_legend("symbol", col = "black", size = 0.3, labels = "Retail Points") +
      # tm_shape(out_bdgs) +
      # tm_fill(col = "orange", alpha = 0.15) +
      # tm_add_legend("fill", "orange", labels = "Retail Buildings") +
      # tm_shape(out_pts) +
      # tm_dots(col = "red", size = 0.005, alpha = 0.3) +
      # tm_add_legend("symbol", col = "orange", size = 0.3, labels = "Retail Points") +
      tm_layout(frame = FALSE, legend.frame = FALSE, legend.bg.color = "white", legend.position = c("right", "top"),
                legend.outside = FALSE, legend.title.fontface = "bold", legend.text.size = 0.75, legend.title.size = 0.5)
    return(p1)
  } else if(typology == "Group"){
    
    typ_pal_groups <- c("#4e91dc", "#f7aa2c",  "#ac77e8")
    p1 <- tm_shape(basemap) +
      tm_rgb() +
      tm_shape(typ) +
      tm_fill(col = "group_name", alpha = 0.75,  palette = typ_pal_groups, title = "Retail Centre Groups") +
      tm_borders(col = "black", lwd = 2.0) +
      tm_shape(in_bdgs) +
      tm_fill(col = "black", alpha = 0.15) +
      tm_layout(frame = TRUE, legend.outside.position = c("right", "top"), legend.outside = TRUE,
                legend.frame = TRUE, legend.text.size = 0.75, legend.title.size = 1.0, legend.title.fontface = "bold")
      
    return(p1)
  } else if(typology == "Type"){
    
    typ_pal_types <- c("#4e91dc", "#a7d2FF", "#f7aa2c", "#ac77e8", "#cfb8e8")
    p1 <- tm_shape(basemap) +
      tm_rgb() +
      tm_shape(typ) +
      tm_fill(col = "type_name", alpha = 0.75,  palette = typ_pal_types, title = "Retail Centre Types", drop.levels = FALSE) +
      tm_borders(col = "black", lwd = 2.0) +
      tm_shape(in_bdgs) +
      tm_fill(col = "black", alpha = 0.15) +
      tm_layout(frame = TRUE, legend.outside.position = c("right", "top"), legend.outside = TRUE,
                legend.frame = TRUE, legend.text.size = 0.75, legend.title.size = 1.0, legend.title.fontface = "bold")
    
    return(p1)
    
  }

  return(p1)
  
    
}

## Maps

### Boston
boston_ls <- c("MA_025_1", "MA_025_27", "MA_025_75", "MA_025_559", "MA_025_290", 
               "MA_025_104", "MA_025_113", "MA_025_33", "MA_017_2", "MA_025_38", "MA_025_74")
boston_map <- make_case_study_maps("MA", boston_ls)
boston_map

### King of Prussia Mall
kp_ls <- c("PA_091_5", "PA_091_721", "PA_091_444", "PA_091_260", "PA_029_492",
           "PA_091_613", "PA_091_238", "PA_091_474",
           "PA_091_267", "PA_091_1195", "PA_091_446", "PA_091_1097")
kp_map <- make_case_study_maps("PA", kp_ls, typology = TRUE)
kp_map


### New Hampshire - Manchester
nh_ls <- c("NH_011_1", "NH_011_101", "NH_011_60", "NH_011_91", "NH_011_107", "NH_011_2", "NH_011_11")
nh_map <- make_case_study_maps("NH", nh_ls, typology = "Group")
nh_map

db <- typ %>% filter(rcID == "NH_011_2")
db_sp <- as(db, 'Spatial')
bm <- cc_location(loc = db_sp, zoom = 16,
                             base_url = "https://basemaps.cartocdn.com/rastertiles/dark_all/{zoom}/{x}/{y}.png") 

tm_shape(bm) +
  tm_rgb() +
  tm_shape(db) +
  tm_fill(col = "orange", alpha = 0.3) +
  tm_borders(col = "orange")
