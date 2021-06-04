## Naming Centres

library(tidyverse)
library(sf)
library(tigris)
library(tmap)
library(hereR)
tmap_mode("view")
source("Source Code/Helper Functions - Delineation.R")
source("Source Code/Helper Functions - Naming Strategy.R")
set_key("5zgYrNtYojJ0DPRRAnufXo_dLijIAav_a6-3j-bg768")

## CHECK: API Key is working first!
## Then supply list of states you want to extract the names for:
state_ls <- list("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                 "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                 "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                 "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
                 "UT", "VA", "VT", "WA", "WI", "WV", "WY")
## Run function across list - it will write out the boundaries w/ names for you, 
## no need to assign objects
lapply(state_ls, get_geocoded_names)


