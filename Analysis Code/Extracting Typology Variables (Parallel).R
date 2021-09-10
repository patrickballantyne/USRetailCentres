## Extracting Typology Variables (FAST)

library(sf)
library(tmap)
library(tidyverse)
library(data.table)
library(parallel)
library(tigris)
library(tidycensus)
library(vroom)
library(dtplyr)
options(scipen = 999)
census_api_key("d67aed54e1801dbbc75b7f2361388350fba41631")
source("source_code/Helper Functions - Typology (Parallel).R")

## Test application of methodology on one state
typ <- prep4typology("IL")

## Apply across regions

### Mid-West
mw_out <- mclapply(mw, prep4typology, mc.cores = 12)
mw_out_df <- do.call(rbind, mw_out)

### North-East
ne_out <- mclapply(ne, prep4typology, mc.cores = 12)
ne_out_df <- do.call(rbind, ne_out)

### West
w_out <- mclapply(w, prep4typology, mc.cores = 12)
w_out_df <- do.call(rbind, w_out)

### South
s_out <- mclapply(s, prep4typology, mc.cores = 12)
s_out_df <- do.call(rbind, s_out)

### US
us_typ <- rbind(mw_out_df, ne_out_df, w_out_df, s_out_df)
us_typ <- us_typ %>%
  arrange(rcID)
head(us_typ)
write.csv(us_typ, "output_data/TypologyVariables/Typ50.csv")


