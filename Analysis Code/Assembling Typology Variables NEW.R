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

## Read in the patterns
pats <- fread("Output Data/Patterns/july_2021_patterns.csv")

## Get variables

### North-East
ne_out <- lapply(ne, prep4typology, patterns = pats)
ne_out_df <- do.call(rbind, ne_out)

### South
s_out <- lapply(s, prep4typology, patterns = pats)
s_out_df <- do.call(rbind, s_out)

### West
w_out <- lapply(w, prep4typology, patterns = pats)
w_out_df <- do.call(rbind, w_out)

### Mid-West
mw_out <- lapply(mw, prep4typology, patterns = pats)
mw_out_df <- do.call(rbind, mw_out)

### US
us_typ <- rbind(ne_out_df, s_out_df, w_out_df, mw_out_df)
write.csv(us_typ, "Output Data/Typology/US_Typology_Variables_minPts100.csv")



