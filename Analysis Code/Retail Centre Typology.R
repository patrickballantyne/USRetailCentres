## Retail Centre Typology

library(sf)
library(tidyverse)
library(factoextra)
library(fpc)
library(cluster)
source("Source Code/Helper Functions - Typology.R")

## Get typology variables for list of retail centres
# ne_typ <- lapply(ne, prep4typology)
# ne_typ <- do.call(rbind, ne_typ)
# write.csv(ne_typ, "Output Data/Typology/NE_TYP.csv")


# 1. Preparing for Typology -----------------------------------------------

## Read in the dataset
typ <- read.csv("Output Data/Typology/NE_TYP.csv")
typ <- typ %>% select(-c("X"))

## Select variables - remove multicollinear ones (n.hexes, pct_Chain)
typ <- typ %>%
  select(n.units, pct_Comparison, pct_Convenience, pct_Service, pct_Leisure, pct_Chain, sub_category_diversity,
         roeck, total_visits, median_distance, median_dwell)

## Scale
typ_s <- as.data.frame(scale(typ, center = TRUE, scale = TRUE))
typ_sub <- typ_s[1:100,]

# 2. Selecting K Value ----------------------------------------------------

## Calculate Average Silhouette Scores
get_silhouette_scores(typ_s, 123)

## Elbow Plot
fviz_nbclust(typ_s, cluster::pam, method = "wss", k.max = 10)


# 3. Running the Clustering -----------------------------------------------
pm <- run_typology(typ_s, 3)

pm[[2]]
