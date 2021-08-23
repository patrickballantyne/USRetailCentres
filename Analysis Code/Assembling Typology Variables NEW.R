## Getting Typology Variables

library(tmap)
library(sf)
library(tidyverse)
library(data.table)
library(tidycensus)
options(scipen = 999)
source("Source Code/Helper Functions - Typology.R")
tempdir()
dir.create(tempdir())

## Test it 
state_list <- c("DC")
out_ls <- lapply(state_list, prep4typology)

## Rbind and write out
out_df <- do.call(rbind, out_ls)
write.csv(out_df, "Output Data/Typology/US_Typology_Variables_minPts100.csv")


### tEST ON ONE
n <- prep4typology("DC")

boundaries <- get_rc("DC")
pts <- get_pts("DC")
pts <- st_set_crs(pts, 4326)
boundaries <- st_transform(boundaries, 4326)

b <- boundaries[1, ]
int <- st_intersection(b, pts)
