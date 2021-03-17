## Retail Centre Typology

library(sf)
library(tidyverse)
library(factoextra)
library(fpc)
library(cluster)
options(scipen=999)
source("Source Code/Helper Functions - Typology.R")


me <- prep4typology("ME")


## Get typology variables for list of retail centres
# ne_typ <- lapply(ne, prep4typology)
# ne_typ <- do.call(rbind, ne_typ)
# write.csv(ne_typ, "Output Data/Typology/NE_TYP.csv")

# 1. Preparing for Typology -----------------------------------------------

## Read in the dataset
typ <- read.csv("Output Data/Typology/NE_TYP.csv")
typ <- typ %>% select(-c("X"))

## Select variables - remove multicollinear ones (n.hexes, pct_Chain)
typ_f <- me %>%
  select(-c(rcID, rcName))

## Scale
typ_s <- as.data.frame(scale(typ_f, center = TRUE, scale = TRUE))

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
