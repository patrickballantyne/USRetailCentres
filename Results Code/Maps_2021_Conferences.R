## Maps for 2021 Conferences

library(tmap)
library(tidyverse)
library(sf)
library(ceramic)
library(extrafont)
source("Source Code/Helper Functions - Delineation.R")
Sys.setenv(MAPBOX_API_KEY="pk.eyJ1Ijoic2dwYmFsbGEiLCJhIjoiY2tncnZxc3FqMGhzaTJ6bzcxNTk3bDNldCJ9.tYEAVCBey8y5tzXx9i0lPw")
windowsFonts("Times" = windowsFont("Times New Roman"))
tmap_mode("plot")

## Read in the dataset
typ <- st_read("Output Data/Typology/NE_Typology.gpkg")



# 1. Group 1 Maps ---------------------------------------------------------

## Boston, MA
make_case_study_maps <- function(state, rc_list) {
  
  ## Pull out the retail centres we want to map
  typ <- st_read("Output Data/Typology/NE_Typology.gpkg")
  typ <- typ %>% filter(rcID %in% rc_list)
  
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
  p1 <- tm_shape(basemap) +
    tm_rgb() +
    tm_shape(typ) +
    tm_fill(col = "gray", alpha = 0.3) +
    tm_borders(col = "orange", lwd = 1.75) +
    tm_add_legend("fill", col = "white", border.col = "orange", labels = "Retail Centres") +
    tm_shape(in_bdgs) +
    tm_fill(col = "black", alpha = 0.15) +
    tm_add_legend("fill", col = "gray", border.col = "gray", labels = "Retail Buildings") +
    tm_shape(in_pts) +
    tm_dots(col = "black", size = 0.005, alpha = 0.5) +
    tm_add_legend("symbol", col = "black", size = 0.3, labels = "Retail Points") +
    # tm_shape(out_bdgs) +
    # tm_fill(col = "orange", alpha = 0.15) +
    # tm_add_legend("fill", "orange", labels = "Retail Buildings") +
    tm_shape(out_pts) +
    tm_dots(col = "black", size = 0.005) +
    # tm_add_legend("symbol", col = "orange", size = 0.3, labels = "Retail Points") +
    tm_layout(fontfamily = "Times", legend.position = c("right", "bottom"), 
              legend.outside = FALSE, legend.frame = TRUE, legend.title.fontface = "bold", legend.text.size = 0.75,
              legend.title.size = 0.5)
  return(p1)
    
}

## Maps

### Boston
boston_ls <- c("MA_025_1", "MA_025_27", "MA_025_75", "MA_025_559", "MA_025_290", 
               "MA_025_104", "MA_025_113", "MA_025_33", "MA_017_2", "MA_025_38", "MA_025_74")
boston_map <- make_case_study_maps("MA", boston_ls)
boston_map

### King of Prussia Mall
kp_ls <- c("PA_091_5", "PA_091_721", "PA_091_444", "PA_091_260")
kp_map <- make_case_study_maps("PA", kp_ls)
kp_map


### New Hampshire - Manchester
nh_ls <- c("NH_011_1", "NH_011_101", "NH_011_91", "NH_011_107", "NH_011_2", "NH_011_11")
nh_map <- make_case_study_maps("NH", nh_ls)
nh_map





typ <- st_read("Output Data/Typology/NE_Typology.gpkg")
typ <- typ %>% filter(rcID %in% ls)
typ_t <- as(typ, "Spatial")
bm <- cc_location(loc = typ_t, zoom = 7,
                  base_url = "https://basemaps.cartocdn.com/rastertiles/dark_all/{zoom}/{x}/{y}.png")
