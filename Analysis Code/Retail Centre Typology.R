## Retail Centre Typology

library(sf)
library(tidyverse)
library(factoextra)
library(fpc)
library(cluster)
options(scipen=999)
source("Source Code/Helper Functions - Typology.R")

## Get typology variables for list of retail centres
ne_typ <- lapply(ne, prep4typology)
# ne_typ_un <- do.call(rbind, ne_typ)
# st_write(ne_typ_un, "Output Data/Typology/NE_Typ.gpkg")


# 1. Preparing for Typology -----------------------------------------------

## Read in the dataset
typ <- st_read("Output Data/Typology/NE_Typ.gpkg")

## Prep for PAM - convert to df, remove ID's and select only the variables we want
typ_df <- typ %>%
  as.data.frame() %>%
  select(-c(rcID, rcName, n.hexes, n.bdgs, top_category_diversity, total_visitors, Housing_Count, geom)) %>% 
  mutate_if(is.integer, as.numeric)

## Scale
typ_s <- as.data.frame(scale(typ_df, center = TRUE, scale = TRUE))

# 2. Selecting K Value ----------------------------------------------------

## Calculate Average Silhouette Scores
get_silhouette_scores(typ_s, 125)

## Clustergram
fviz_nbclust(typ_sub, cluster::pam, method = "wss") +
  labs(subtitle = "Elbow Method")


# 3. Running the Clustering -----------------------------------------------
pm <- run_typology(typ_sub, 5)

# 4. Examining Cluster Profiles -------------------------------------------

## Plot medoids
plot_medoids(pm)
