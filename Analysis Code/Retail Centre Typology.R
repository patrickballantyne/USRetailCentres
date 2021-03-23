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
ne_typ_un <- do.call(rbind, ne_typ)
st_write(ne_typ_un, "Output Data/Typology/NE_Typ.gpkg")


### This is all done on a Linux machine - won't run on Windows (mclapply)

# 1. Data Preparation -----------------------------------------------------

## Read in and prepare the dataset
typ <- st_read("output_data/Typology/NE_Typ.gpkg")

## Variables in df
typ_v <- typ %>%
  as.data.frame() %>%
  select(-c(geom)) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_all(~replace(., is.na(.), 0))

typ_s <- as.data.frame(scale(typ_v[, 3:33], center = TRUE, scale = TRUE))

# 2. Checking skew of variables -------------------------------------------

skew <- as.data.frame(sapply(typ_s, function(x) skewness(x)))
skew <- skew %>%
  rownames_to_column("variable") %>%
  set_names(c("Variable", "Skew"))
skew

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
  select(-c(n.hexes, n.bdgs, area, 
            top_category_diversity, pct_Independent, Tot_Res_Count)) %>%
  mutate_all(~replace(., is.na(.), 0))

## Remake corrplot
cor_m <- cor(typ_s)
p.mat <- cor.mtest(typ_s)
corrplot::corrplot(cor_m, type = "upper", tl.srt = 45, method = "number",
                   p.mat = p.mat, sig.level = 0.01, insig = "blank")


# 3. Dimensionality Reduction ---------------------------------------------

## Compute PCA
pca <- PCA(typ_s, graph = FALSE)

## Examine eigenvalues
eig.vals <- get_eigenvalue(pca)
eig.vals

## Scree plot
fviz_eig(pca, addlabels = TRUE)

## Extract results for active variables
var <- get_pca_var(pca)
var$contrib

## Extract and plot representation of variables - contrib to Dim 1 & 2
## least contributions from median dwell, convenience and median distance travelled
fviz_cos2(pca, choice = "var", axes = 1:2)

## Plot for each Dim
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)

## Remove variables for simplicity - Geodemographics
typ_s_no_geodemo <- typ_s %>%
  select(-c(GroupA_NE_prop, GroupB_NE_prop, GroupC_NE_prop, GroupD_NE_prop,
            GroupE_NE_prop, GroupF_NE_prop, GroupG_NE_prop, GroupH_NE_prop, 
            GroupI_NE_prop, GroupJ_NE_prop))

## Most significant geodemographics
geodems <- typ_s %>%
  select(GroupH_NE_prop, GroupE_NE_prop, GroupC_NE_prop, GroupB_NE_prop, GroupA_NE_prop)

# 4. Identifying K Value --------------------------------------------------

## Print average silhouette scores
get_silhouette_scores(typ_s_no_geodemo)

## Plot the clustergram
fviz_nbclust(typ_s_no_geodemo,  cluster::pam, method = "wss", k.max = 10) +
  labs(subtitle = "Elbow Method")


# 5. Running Typology -----------------------------------------------------

## Run typology
pm_2 <- run_typology(typ_s_no_geodemo, 2)
pm_3 <- run_typology(typ_s_no_geodemo, 3)

## Plot medoids
plot_medoids(pm_2)
plot_medoids(pm_3)


# 6. Extracting Results ---------------------------------------------------

## Extract cluster vals and attach rcID/rcName
pm_out <- pm_3[[1]]
pm_out <- cbind(typ[,1:2], pm_out)
tidy <- pm_out %>% select(rcID, rcName, cluster) %>% dplyr::rename(group_id = cluster)
#st_write(pm_out, "output_data/Typology/test.gpkg")

## Save medoids
group_medoids <- plot_medoids(pm_3)
group_medoids

## Reattach geodemographic variables
groups <- cbind(pm_out, geodems)

# 7. Extracting Nested Types (with geodemographics) ----------------------------------------------

## Get number of distinct groups
n <- c(unique(groups$cluster))

## Apply typology functions across number of groups
types <- lapply(n, function(x) get_nested_types(groups, cl = x, medoids = FALSE))
types_un <- do.call(rbind, types)

medoids <- lapply(n, function(x) get_nested_types(groups, cl = x, medoids = TRUE))
medoids_un <- do.call(rbind, medoids)
type_medoids <- plot_type_medoids(medoids_un)


group_medoids
type_medoids

# 7.1 Extracting Nested Types (without geodemographics) -------------------

## Get number of distinct groups
n <- c(unique(pm_out$cluster))


## Apply typology functions across number of groups
types <- lapply(n, function(x) get_nested_types(pm_out, cl = x, medoids = FALSE))
types_un <- do.call(rbind, types)


## Get medoids
medoids <- lapply(n, function(x) get_nested_types(pm_out, cl = x, medoids = TRUE))
medoids_un <- do.call(rbind, medoids)
type_medoids <- plot_type_medoids(medoids_un)
type_medoids

# 8. Writing Out ----------------------------------------------------------
st_write(types_un, "output_data/Typology/NE_RC_Typology.gpkg")



