## H3 Method Figure

library(tmap)
library(sf)
library(tidyverse)
library(h3jsr)
library(igraph)
library(tidygraph)
source("Source Code/Helper Functions - Delineation.R")
windowsFonts("Times" = windowsFont("Times New Roman"))

## Pull in datasets
pts <- st_transform(read_points("PA"), 4326)
buildings <- st_read("Output Data/Buildings/PA_Retail_Buildings.gpkg")
lu <- st_read("Output Data/Land-Use/Processed/PA_LU.gpkg")
h3 <- st_read("Output Data/Retail H3/PA_Retail_H3.gpkg")
rdsa <- st_transform(st_read("Outputs and Figures/Data/Trans_RoadSegment4.shp"), 4326)
rdsb <- st_transform(st_read("Outputs and Figures/Data/Trans_RoadSegment5.shp"), 4326)
rds <- rbind(rdsa, rdsb)

## Case Study Area
cs <- st_read("Outputs and Figures/Data/H3_Method_CS.shp")

## Clip to Case Study Area
pts <- pts[cs, op = st_intersects]
bdg <- buildings[cs, op = st_intersects]
lu <- lu[cs, op = st_intersects]
h3 <- h3[cs, op = st_intersects]
rds <- rds[cs, op = st_intersects]

## Running Algorithm

## Adjacent tracts
adj_tracts <- get_h3_clusters(h3, 20, "PA")
adj_tracts_d <- adj_tracts %>%
  group_by(tractID) %>%
  st_union()
## Connections
connections <- get_h3_clusters(h3, 20, "PA")
ct <- connections %>%
  filter(!h3_address %in% adj_tracts$h3_address)
o <- ct %>%
  as.data.frame() %>%
  select(h3_address) %>%
  distinct()
ct <- ct %>%
  filter(h3_address %in% o$h3_address)
## Connected tracts
con_tracts <- get_h3_clusters(h3, 20, "PA")
con_tracts_d <- con_tracts %>%
  group_by(tractID) %>%
  st_union()
## Final tracts 
final_tracts <- con_tracts %>% filter(n.pts >= 20)

## Template
tm_shape(rds) +
  tm_lines(col = "gray", alpha = 0.75) +
  tm_shape(bdg) +
  tm_fill(col = "gray") +
  tm_shape(pts) +
  tm_dots(col = "black", size = 0.075) +
  tm_shape(lu) +
  tm_fill(col = NA, alpha = 0.5) +
  tm_borders(col = "red", lwd = 1.5) +
  tm_add_legend("fill", col = "gray", size = 0.8, labels = "Retail Places (Buildings)") +
  tm_add_legend("symbol", col = "black", size = 0.8, labels = "Retail Places (Points)") +
  tm_add_legend("fill", col = "white", border.col = "red", size = 0.8, labels = "Retail Land-Use") +
  tm_scale_bar(position = c("right", "bottom"), breaks = c(0, 0.25, 0.5),
               text.size = 0.75, bg.color = "white", bg.alpha = 0.15) +
  tm_layout(title = "A", title.position = c("left", "top"), 
            frame = TRUE,
            fontfamily = "Times", title.fontface = "bold", title.size = 2,
            legend.position = c("left", "bottom"), legend.frame = TRUE, legend.show = TRUE,
            legend.text.size = 0.75, legend.title.size = 1, legend.bg.color = "white")

## Map 1 - All Features
p1 <- tm_shape(rds) +
  tm_lines(col = "gray", alpha = 0.75) +
  tm_shape(bdg) +
  tm_fill(col = "gray") +
  tm_shape(pts) +
  tm_dots(col = "black", size = 0.075) +
  tm_shape(lu) +
  tm_fill(col = NA, alpha = 0.5) +
  tm_borders(col = "red", lwd = 1.5) +
  tm_add_legend("fill", col = "gray", size = 0.8, labels = "Retail Places (Buildings)") +
  tm_add_legend("symbol", col = "black", size = 0.8, labels = "Retail Places (Points)") +
  tm_add_legend("fill", col = "white", border.col = "red", size = 0.8, labels = "Retail Land-Use") +
  tm_scale_bar(position = c("right", "bottom"), breaks = c(0, 0.5, 1.0),
               text.size = 0.75, bg.color = "white", bg.alpha = 0.15) +
  tm_layout(title = "A", title.position = c("left", "top"), 
            frame = FALSE,
            fontfamily = "Times", title.fontface = "bold", title.size = 2,
            legend.position = c("left", "bottom"), legend.frame = TRUE, legend.show = TRUE,
            legend.text.size = 0.65, legend.title.size = 1, legend.bg.color = "white")

  

## Map 2 - All Features -> H3
p2 <- tm_shape(rds) +
  tm_lines(col = "gray", alpha = 0.75) +
  tm_shape(bdg) +
  tm_fill(col = "gray") +
  tm_shape(pts) +
  tm_dots(col = "black", size = 0.075) +
  tm_shape(h3) +
  tm_fill(col = "orange", alpha = 0.5) +
  tm_borders(col = "orange") +
  tm_add_legend("fill", col = "orange", border.col = "orange", size = 0.8, labels = "Retail H3 Addresses") +
  tm_add_legend("fill", col = "gray", size = 0.8, labels = "Retail Places (Buildings)") +
  tm_add_legend("symbol", col = "black", size = 0.8, labels = "Retail Places (Points)") +
  tm_scale_bar(position = c("right", "bottom"), breaks = c(0, 0.5, 1.0),
               text.size = 0.75, bg.color = "white", bg.alpha = 0.15) +
  tm_layout(title = "B", title.position = c("left", "top"), 
            frame = FALSE,
            fontfamily = "Times", title.fontface = "bold", title.size = 2,
            legend.position = c("left", "bottom"), legend.frame = TRUE, legend.show = TRUE,
            legend.text.size = 0.65, legend.title.size = 1, legend.bg.color = "white")


## Map 3 - Adjacent Tracts
p3 <- tm_shape(rds) +
  tm_lines(col = "gray", alpha = 0.75) +
  tm_shape(bdg) +
  tm_fill(col = "gray") +
  tm_shape(pts) +
  tm_dots(col = "black", size = 0.075) +
  #tm_shape(lu) +
  #tm_fill(col = NA, alpha = 0.5) +
  #tm_borders(col = "red", lwd = 1.5) +
  tm_shape(adj_tracts) +
  tm_fill(col = "tractID", alpha = 0.75, legend.show = FALSE) +
  tm_borders(col = NA, alpha = 0.001) +
  tm_shape(adj_tracts_d) +
  tm_fill(col = NA, alpha = 0.001) +
  tm_borders(col = "black", lwd = 1.75) +
  tm_add_legend("fill", col = "#ccfdcc", size = 0.8, labels = "Tracts of Adjacent H3's") +
  tm_add_legend("fill", col = "gray", size = 0.8, labels = "Retail Places (Buildings)") +
  tm_add_legend("symbol", col = "black", size = 0.8, labels = "Retail Places (Points)") +
  tm_scale_bar(position = c("right", "bottom"), breaks = c(0, 0.5, 1.0),
               text.size = 0.75, bg.color = "white", bg.alpha = 0.15) +
  tm_layout(title = "C", title.position = c("left", "top"), 
            frame = FALSE,
            fontfamily = "Times", title.fontface = "bold", title.size = 2,
            legend.position = c("left", "bottom"), legend.frame = TRUE, legend.show = TRUE,
            legend.text.size = 0.65, legend.title.size = 1, legend.bg.color = "white")

## Map 4 - Connections
p4 <- tm_shape(rds) +
  tm_lines(col = "gray", alpha = 0.75) +
  tm_shape(bdg) +
  tm_fill(col = "gray") +
  tm_shape(pts) +
  tm_dots(col = "black", size = 0.075) +
  #tm_shape(lu) +
  #tm_fill(col = NA, alpha = 0.5) +
  #tm_borders(col = "red", lwd = 1.5) +
  tm_shape(adj_tracts) +
  tm_fill(col = "tractID", alpha = 0.75, legend.show = FALSE) +
  tm_borders(col = NA, alpha = 0.001) +
  tm_shape(ct) +
  tm_fill(col = "red", alpha = 0.5) +
  tm_borders(col = "red", lwd = 1.5) +
  tm_add_legend("fill", col = "red", border.col = "red", size = 0.8, labels = "Tract-Connecting H3's") +
  tm_add_legend("fill", col = "#ccfdcc", size = 0.8, labels = "Tracts of Adjacent H3's ") +
  tm_add_legend("fill", col = "gray", size = 0.8, labels = "Retail Places (Buildings)") +
  tm_add_legend("symbol", col = "black", size = 0.8, labels = "Retail Places (Points)") +
  tm_scale_bar(position = c("right", "bottom"), breaks = c(0, 0.5, 1.0),
               text.size = 0.75, bg.color = "white", bg.alpha = 0.15) +
  tm_layout(title = "D", title.position = c("left", "top"), 
            frame = FALSE,
            fontfamily = "Times", title.fontface = "bold", title.size = 2,
            legend.position = c("left", "bottom"), legend.frame = TRUE, legend.show = TRUE,
            legend.text.size = 0.65, legend.title.size = 1, legend.bg.color = "white")


## Map 5 - Connected Tracts
p5 <- tm_shape(rds) +
  tm_lines(col = "gray", alpha = 0.75) +
  tm_shape(bdg) +
  tm_fill(col = "gray") +
  tm_shape(pts) +
  tm_dots(col = "black", size = 0.075) +
  tm_shape(con_tracts) +
  tm_fill(col = "tractID", alpha = 0.75, legend.show = FALSE) +
  tm_shape(con_tracts_d) +
  tm_fill(col = NA, alpha = 0.001) +
  tm_borders(col = "black", lwd = 1.75) +
  tm_add_legend("fill", col = "#cae8dc", size = 0.8, labels = "Connected Tracts ") +
  tm_add_legend("fill", col = "gray", size = 0.8, labels = "Retail Places (Buildings)") +
  tm_add_legend("symbol", col = "black", size = 0.8, labels = "Retail Places (Points)") +
  tm_scale_bar(position = c("right", "bottom"), breaks = c(0, 0.5, 1.0),
               text.size = 0.75, bg.color = "white", bg.alpha = 0.15) +
  tm_layout(title = "E", title.position = c("left", "top"), 
            frame = FALSE,
            fontfamily = "Times", title.fontface = "bold", title.size = 2,
            legend.position = c("left", "bottom"), legend.frame = TRUE, legend.show = TRUE,
            legend.text.size = 0.65, legend.title.size = 1, legend.bg.color = "white")


## Map 6 - Final Tracts & Names
p6 <- tm_shape(rds) +
  tm_lines(col = "gray", alpha = 0.75) +
  tm_shape(bdg) +
  tm_fill(col = "gray") +
  tm_shape(pts) +
  tm_dots(col = "black", size = 0.075) +
  tm_shape(final_tracts) +
  tm_fill(col = "tractID", alpha = 0.75, legend.show = FALSE) +
  tm_borders(col = "black") +
  tm_text("rcID", size = 0.8, bg.color = "white", fontface = "bold", bg.alpha = 0.75) +
  #tm_shape(lu) +
  #tm_fill(col = NA, alpha = 0.5) +
  #tm_borders(col = "red", lwd = 1.5) +
  tm_add_legend("fill", col = "#cae8dc", size = 0.8, labels = "Major Retail Centres ") +
  tm_add_legend("text", labels = "Retail Centre Name", text = "PA2", size = 5, col = "black") +
  tm_add_legend("fill", col = "gray", size = 0.8, labels = "Retail Places (Buildings)") +
  tm_add_legend("symbol", col = "black", size = 0.8, labels = "Retail Places (Points)") +
  #tm_add_legend("fill", col = "white", border.col = "red", size = 0.8, labels = "Retail Land-Use") +
  tm_scale_bar(position = c("right", "bottom"), breaks = c(0, 0.5, 1.0),
               text.size = 0.75, bg.color = "white", bg.alpha = 0.15) +
  tm_layout(title = "F", title.position = c("left", "top"), 
            frame = FALSE,
            fontfamily = "Times", title.fontface = "bold", title.size = 2,
            legend.position = c("left", "bottom"), legend.frame = TRUE, legend.show = TRUE,
            legend.text.size = 0.65, legend.title.size = 1, legend.bg.color = "white")

tmap_arrange(p1, p2, p3, nrow = 1)
tmap_arrange(p4, p5, p6, nrow = 1)
