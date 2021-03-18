## Retail Centre Typology

library(sf)
library(tidyverse)
library(factoextra)
library(fpc)
library(cluster)
library(e1071)
library(ggplot2)
options(scipen=999)
source("source_code/Helper Functions - Typology (Parallel).R")

# 1. Data Preparation -----------------------------------------------------

## Read in and prepare the dataset
typ <- st_read("output_data/Typology/NE_Typ.gpkg")
typ_df <- typ %>%
  as.data.frame() %>%
  select(-c(rcID, rcName, geom)) %>% 
  mutate_if(is.integer, as.numeric) %>%
  mutate(total_visits = replace_na(total_visits, 0), 
         total_visitors = replace_na(total_visitors, 0),
         median_distance = replace_na(median_distance, 0),
         median_dwell = replace_na(median_dwell, 0))
typ_s <- as.data.frame(scale(typ_df, center = TRUE, scale = TRUE))

# 2. Checking skew of variables -------------------------------------------

skew <- as.data.frame(sapply(typ_s, function(x) skewness(x)))
skew <- skew %>%
  rownames_to_column("variable") %>%
  set_names(c("Variable", "Skew"))

# 3. Checking collinearity of variables -----------------------------------

## Convert to correlation matrix
cor_m <- cor(typ_s)

## Compute p vals
p.mat <- cor.mtest(typ_s)

## Plot - marking off insignificant correlations 
corrplot::corrplot(cor_m, type = "upper", tl.srt = 45, method = "number",
                   p.mat = p.mat, sig.level = 0.01, insig = "blank")

## Remove highly linear variables
typ_s <- typ_s %>%
  select(-c(n.units, n.bdgs, area, top_category_diversity, pct_Independent, total_visitors, Housing_Count))

# 3. Identifying K Value --------------------------------------------------

## Print average silhouette scores
get_silhouette_scores(typ_s)

## Plot the clustergram
fviz_nbclust(typ_s,  cluster::pam, method = "wss", k.max = 10) +
  labs(subtitle = "Elbow Method")

## Run typology
pm <- run_typology(typ_s, 2)
pm_6 <- run_typology(typ_s, 6)

## Plot medoids
plot_medoids(pm)
plot_medoids(pm_6)


