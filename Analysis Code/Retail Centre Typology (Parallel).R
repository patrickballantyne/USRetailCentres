## Paralellising Retail Centre Typology

library(sf)
library(tidyverse)
library(data.table)
library(parallel)
library(factoextra)
library(FactoMineR)
library(fpc)
library(corrplot)
library(cluster)
library(e1071)
library(ggplot2)
options(scipen = 999)
source("source_code/Helper Functions - Typology (Parallel).R")

# 1. Read in the data -----------------------------------------------------

## Vars
typ <- read.csv("output_data/TypologyVariables/Typ50.csv")

## Extract Vars
typ_vars <- typ %>%
  select(-c(X)) %>%
  select(2:43) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_all(~replace(., is.na(.), 0))

## Scale
### Z-scores
typ_z <- as.data.frame(scale(typ_vars, center = TRUE, scale = TRUE))

### 0 - 1
# scaleit <- function(x) (scales::rescale(x, to = c(0 , 1)))
# typ_n <- typ_vars %>% 
#   mutate_all(., scaleit)


# 2. Skew -----------------------------------------------------------------

## Calculate & Format Skew
skew <- as.data.frame(sapply(typ_vars, function(x) skewness(x)))
skew <- skew %>%
  rownames_to_column("variable") %>%
  set_names(c("Variable", "Skew"))


# 3. Collinearity ---------------------------------------------------------

## Build correlation matrix
cor_m <- cor(typ_z)

## P vals
p.mat <- cor.mtest(typ_z)

## Corr plot
corrplot(cor_m, type = "upper", method = "color",
         insig = "blank", tl.srt = 90, tl.col = "black", p.mat = p.mat$p)

## Extract correlations over 70%
any_over_70 <- function(x) any(x > .7, na.rm = TRUE)
any_over_neg70 <- function(x) any(x < -0.7, na.rm = TRUE)

## High correlations
high <- cor_m %>% focus_if(any_over_70, mirror = TRUE)
## High negative correlations
low <- cor_m %>% focus_if(any_over_neg70, mirror = TRUE)


## Remove the highly linear variables
typ_z <- typ_z %>%
  select(-c(propGeneralMerchandise, nationalRetailDiversity, nationalServiceDiversity,
            totalPopulation, retailService, propNationalChain))
typ_vars <- typ_vars %>%
  select(-c(propGeneralMerchandise, nationalRetailDiversity, nationalServiceDiversity,
            totalPopulation, retailService, propNationalChain))


# 4. Descriptives ---------------------------------------------------------

## Median Vals
median_vals <- typ_vars %>%
  summarise_if(is.numeric, median) %>%
  gather()

## Std Dev Vals
st_dev_vals <- typ_vars %>%
  summarise_if(is.numeric, sd) %>%
  gather()

# 5. Dimensionality Reduction ---------------------------------------------

## Compute PCA
pca <- PCA(typ_z, graph = FALSE)

## Eigenvals
eig.vals <- get_eigenvalue(pca)
eig.vals

## Scree plot
fviz_eig(pca, addlabels = TRUE)

## Active vars
var <- get_pca_var(pca)
var$contrib

## Contribution to Dim 1&2
fviz_contrib(pca, choice = "var", axes = 1:5)

# 6. Identifying K -----------------------------------------------------------

## Print average Silhouette Scores
s_scores <- get_silhouette_scores(typ_z, 123)
s_scores


# 7. Typology - Groups ----------------------------------------------------

## Get out groups
pm <- run_typology(typ_z, 4)
pm_out <- pm[[1]]
plot_medoids(pm)


# 8. Typology - Types -----------------------------------------------------

# 8.1 Group 1 -------------------------------------------------------------

## Run PCA on the groups to identify insignificant vars
pca1 <- run_type_pca(pm_out, 1)
pca1

## Get variables for PCA
group1_vars <- pm_out %>%
  filter(cluster == 1) %>%
  select(propIndependent, propRestaurant, localRetailDiversity, retailDensity,
         propRecreational, propBars, propConsumerService, nUnits, residentialDensity,
         propEntertainment, nCompeting, medianIncome, totalVisits)

## Calculate k parameters for the types
get_silhouette_scores(group1_vars)

## Run PCA
pm_group1 <- run_typology(group1_vars, 2)
plot_medoids(pm_group1)

pm_group1_df <- pm_group1[[1]]
pm_group1_df <- pm_group1_df %>%
  select(cluster) %>%
  rename(type_cluster = cluster)




# 8.2 Group 2 ---------------------------------------------------------------------

## Run PCA on the groups to identify insignificant vars
pca2 <- run_type_pca(pm_out, 2)
pca2

## Get variables for PCA
group2_vars <- pm_out %>%
  filter(cluster == 2) %>%
  select(nUnits, localRetailDiversity, localServiceDiversity, propPopularComparisonBrands,
         propDiscount, totalVisits, retailDensity, propIndependent, propPopularConvenienceBrands,
         propConsumerService, roadDensity, residentialDensity, nCompeting)

## Calculate k parameters for the types
get_silhouette_scores(group2_vars)

## Run PCA
pm_group2 <- run_typology(group2_vars, 2)
plot_medoids(pm_group2)

pm_group2_df <- pm_group2[[1]]
pm_group2_df <- pm_group2_df %>%
  select(cluster) %>%
  rename(type_cluster = cluster)



# 8.3. Group 3 ------------------------------------------------------------

## Run PCA on the groups to identify insignificant vars
pca3 <- run_type_pca(pm_out, 3)
pca3

## Get variables for PCA
group3_vars <- pm_out %>%
  filter(cluster == 3) %>%
  select(propClothingandFootwear, propPopularComparisonBrands, localServiceDiversity,
         propPopularConvenienceBrands, propDiscount, localRetailDiversity, retailDensity,
         propConsumerService, propIndependent, propSmallMultiple, propChemist,
         propPremiumBrand)

## Calculate k parameters for the types
get_silhouette_scores(group3_vars)

## Run PCA
pm_group3 <- run_typology(group3_vars, 2)
plot_medoids(pm_group3)

pm_group3_df <- pm_group3[[1]]
pm_group3_df <- pm_group3_df %>%
  select(cluster) %>%
  rename(type_cluster = cluster)



# 8.4 Group 4 -------------------------------------------------------------

## Run PCA on the groups to identify insignificant vars
pca4 <- run_type_pca(pm_out, 4)
pca4

## Get variables for PCA
group4_vars <- pm_out %>%
  filter(cluster == 4) %>%
  select(propIndependent, propRestaurant, residentialDensity, roadDensity,
         localServiceDiversity, propBars, propFoodandDrink, totalVisits, 
         localRetailDiversity, propSmallMultiple, nUnits, 
         propClothingandFootwear, propChemist, propEntertainment)

## Calculate k parameters for the types
get_silhouette_scores(group4_vars)

## Run PCA
pm_group4 <- run_typology(group4_vars, 2)
plot_medoids(pm_group4)

pm_group4_df <- pm_group4[[1]]
pm_group4_df <- pm_group4_df %>%
  select(cluster) %>%
  rename(type_cluster = cluster)



# Exporting for checks ----------------------------------------------------

out_df <- cbind(typ$rcID, pm_out)
colnames(out_df)[1] <- "rcID"



out_df4 <- out_df %>%
  select(rcID, cluster) %>%
  filter(cluster == 4)
out_df3 <- out_df %>%
  select(rcID, cluster) %>%
  filter(cluster == 3)
out_df2 <- out_df %>%
  select(rcID, cluster) %>%
  filter(cluster == 2)
out_df1 <- out_df %>%
  select(rcID, cluster) %>%
  filter(cluster == 1)
out_df4 <- cbind(out_df4, pm_group4_df)
out_df3 <- cbind(out_df3, pm_group3_df)
out_df2 <- cbind(out_df2, pm_group2_df)
out_df1 <- cbind(out_df1, pm_group1_df)
out_df_all <- rbind(out_df1, out_df2, out_df3, out_df4)

## Reclassify the groups & types
out_recl <- out_df_all %>%
  rename(orig_group = cluster, orig_type = type_cluster) %>%
  mutate(group = case_when(orig_group == 1 ~ 2,
                           orig_group == 2 ~ 4,
                           orig_group == 3 ~ 3,
                           orig_group == 4 ~ 1)) %>%
  mutate(type = case_when(orig_group == 1 & orig_type == 2 ~ 2.1,
                          orig_group == 1 & orig_type == 1 ~ 2.2,
                          orig_group == 2 & orig_type == 2 ~ 4.1,
                          orig_group == 2 & orig_type == 1 ~ 4.2,
                          orig_group == 3 & orig_type == 2 ~ 3.1,
                          orig_group == 3 & orig_type == 1 ~ 3.2,
                          orig_group == 4 & orig_type == 2 ~ 1.1,
                          orig_group == 4 & orig_type == 1 ~ 1.2)) %>%
  setNames(c("rcID", "orig_groupID", "orig_typeID", "groupID", "typeID")) %>%
  mutate_if(is.numeric, as.factor) %>%
  arrange(typeID)

## Merge onto main dataset
rc <- st_read("output_data/US_RC_minPts50.gpkg")
rc <- rc %>%
  select(rcID, State)
rc_out <- merge(rc, out_recl, by = "rcID")
st_write(rc_out, "output_data/rcTyp50.gpkg")






out_df3 <- cbind(out_df3, pm_group3_df)
rc_out3 <- merge(rc, out_df3, by = "rcID", all.y = TRUE)
st_write(rc_out3, "rc_out3.gpkg")
