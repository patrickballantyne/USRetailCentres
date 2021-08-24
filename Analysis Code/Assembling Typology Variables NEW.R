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

## Read in the pattern
pats <- fread("Output Data/Patterns/july_2021_patterns.csv")

## Test it 
state_list <- c("DC")
out_ls <- lapply(state_list, prep4typology, patterns = pats)

exce## Rbind and write out
out_df <- do.call(rbind, out_ls)
write.csv(out_df, "Output Data/Typology/US_Typology_Variables_minPts100.csv")

### Test on one state - for troubleshooting
n <- prep4typology("MT", pats)


