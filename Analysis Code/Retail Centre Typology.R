## Retail Centre Typology

library(sf)
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(fpc)
library(cluster)
library(e1071)
library(corrplot)
library(RColorBrewer)
library(lares)
options(scipen=999)
source("Source Code/Helper Functions - Typology.R")


# 1. Data Preparation -----------------------------------------------------

## Read in and prepare the dataset
typ <- read.csv("Output Data/Typology/US_Typology_Variables_minPts50_NEW.csv")

## Read in the centres
rc <- st_read("Output Data/Retail Centres/US Retail Centres/US_RC_minPts50.gpkg")

## Variables in df
typ_v <- typ %>%
  select(-c(X)) %>%
  select(2:38) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_all(~replace(., is.na(.), 0))

## Scale to Z scores
typ_z <- as.data.frame(scale(typ_v, center = TRUE, scale = TRUE))

## Scale to 0 - 1
scale2 <- function(x) (scales::rescale(x, to = c(0 , 1)))
typ_s <- typ_v %>% mutate_all(., scale2)


# ## Apply weights
# typ_s_w <- typ_s %>%
#   mutate(propClothingandFootwear = propClothingandFootwear * 0.017,
#          propDIYandHousehold = propDIYandHousehold * 0.017,
#          propRecreational = propRecreational * 0.017,
#          propFood = propFood * 0.017,
#          propCTNandGasoline = propCTNandGasoline * 0.017,
#          propOffLicence = propOffLicence * 0.017,
#          propChemist = propChemist * 0.017,
#          propBars = propBars * 0.017,
#          propRestaurant = propRestaurant * 0.017,
#          propFastFood = propFastFood * 0.017, 
#          propEntertainment = propEntertainment * 0.017,
#          propFitness = propFitness * 0.017,
#          propConsumerService = propConsumerService * 0.017, 
#          propHouseholdService = propHouseholdService * 0.017, 
#          propBusinessService = propBusinessService * 0.017,
#          
#          propIndependent = propIndependent * 0.063,
#          propSmallMultiple = propSmallMultiple * 0.063,
#          propPopularBrands = propPopularBrands * 0.063,
#          localCatDiversity = localCatDiversity * 0.063,
#          
#          nUnits = nUnits * 0.025,
#          roeckScore = roeckScore * 0.025,
#          distanceTravelled = distanceTravelled * 0.025,
#          retailDensity = retailDensity * 0.025,
#          residentialDensity = residentialDensity * 0.025,
#          employmentDensity = employmentDensity * 0.025,
#          retailemploymentDensity = retailemploymentDensity * 0.025,
#          roadDensity = roadDensity * 0.025,
#          propAnchor = propAnchor * 0.025,
#          propDiscount = propDiscount * 0.025,
#          
#          totalVisits = totalVisits * 0.125,
#          medianDwell = medianDwell * 0.125)


# 2. Checking skew of variables -------------------------------------------

skew <- as.data.frame(sapply(typ_s_w, function(x) skewness(x)))
skew <- skew %>%
  rownames_to_column("variable") %>%
  set_names(c("Variable", "Skew"))



# 2.1 Descriptive Statistics ----------------------------------------------

## Median vals
median_vals <- typ_v %>%
  summarise_if(is.numeric, median) %>%
  gather()

## Standard Deviations
st_dev_vals <- typ_v %>%
  summarise_if(is.numeric, sd) %>%
  gather()

# 3. Checking collinearity of variables -----------------------------------

## Convert to correlation matrix
cor_m <- cor(typ_s)

## Compute p vals
p.mat <- cor.mtest(typ_s)

## Plot - marking off insignificant correlations
corrplot(cor_m, type = "upper", method = "color",
         insig = "blank", tl.srt = 90, tl.col = "black", p.mat = p.mat$p,
         col = brewer.pal(n = 8, name = "Greys"))

## Extract top 10 most correlated variables
corr_cross(typ_s, max_pvalue = 0.05, top = 10)

## Remove highly linear variables
typ_s_clean <- typ_s %>%
  select(-c(nationalCatDiversity, propNationalChain, retailService)) %>%
  mutate_all(~replace(., is.na(.), 0))


# 3. Dimensionality Reduction ---------------------------------------------

## Compute PCA
pca <- PCA(typ_s_clean, graph = FALSE)

## Examine eigenvalues
eig.vals <- get_eigenvalue(pca)
eig.vals

## Scree plot
fviz_eig(pca, addlabels = TRUE, ncp = 15)

## Extract results for active variables
var <- get_pca_var(pca)
var$contrib

## Extract and plot representation of variables - contrib to Dim 1 & 2
## least contributions from median dwell, convenience and median distance travelled
fviz_cos2(pca, choice = "var", axes = 1:5)

## Remove insignificant variables
typ_p <- typ_s_clean %>%
  select(-c(transitDistance))
  

# 4. Identifying K Value --------------------------------------------------

## Weight the variables
typ_p_w <- typ_p %>%
  mutate(propClothingandFootwear = propClothingandFootwear * 0.016,
         propDIYandHousehold = propDIYandHousehold * 0.016,
         propElectrical = propElectrical * 0.016,
         propRecreational = propRecreational * 0.016,
         propFood = propFood * 0.016,
         propOffLicence = propOffLicence * 0.016,
         propCTNandGasoline = propCTNandGasoline * 0.016,
         propChemist = propChemist * 0.016,
         propBars = propBars * 0.016,
         propRestaurant = propRestaurant * 0.016,
         propFastFood = propFastFood * 0.016, 
         propEntertainment = propEntertainment * 0.016,
         propFitness = propFitness * 0.016,
         propConsumerService = propConsumerService * 0.016, 
         propHouseholdService = propHouseholdService * 0.016, 
         propBusinessService = propBusinessService * 0.016,
         
         propIndependent = propIndependent * 0.063,
         propSmallMultiple = propSmallMultiple * 0.063,
         propPopularBrands = propPopularBrands * 0.063,
         localCatDiversity = localCatDiversity * 0.063,
         
         nUnits = nUnits * 0.025,
         roeckScore = roeckScore * 0.025,
         retailDensity = retailDensity * 0.025,
         distanceTravelled = distanceTravelled * 0.025,
         residentialDensity = residentialDensity * 0.025,
         employmentDensity = employmentDensity * 0.025,
         retailemploymentDensity = retailemploymentDensity * 0.025,
         roadDensity = roadDensity * 0.025,
         propAnchor = propAnchor * 0.025,
         propDiscount = propDiscount * 0.025,
         
         totalVisits = totalVisits * 0.083,
         medianDwell = medianDwell * 0.083,
         lowIncome = lowIncome * 0.083)
  

## Print average silhouette scores
sil_scores <- get_silhouette_scores(typ_p_w)
sil_scores

## Silhouette plot
sil_plot <- fviz_nbclust(typ_p_w, pam, method="silhouette") +
  theme_classic()
sil_plot

## Within sum of squares
wss_plot <- fviz_nbclust(typ_p_w, cluster::pam, method = "wss", k.max = 10) + 
  theme_classic()
wss_plot

## Ecluster plot
eclust<- eclust(typ_p, FUNcluster = "pam", k = 2, hc_metric = "euclidean")
eclust

# 5. Running Typology -----------------------------------------------------

## Run typology
pm_2 <- run_typology(typ_p, 2)
pm_3 <- run_typology(typ_p_w, 3)
pm_4 <- run_typology(typ_p, 4)
pm_5 <- run_typology(typ_p, 5)

## Plot medoids
plot_medoids(pm_2)
plot_medoids(pm_3)
plot_medoids(pm_5)


# 6. Extracting Results ---------------------------------------------------

## Extract cluster vals and attach rcID/rcName
pm_out <- pm_3[[1]]
pm_out <- as.data.frame(cbind(typ_v[,1], pm_out))
tidy <- pm_out %>% 
  select(1:2) %>%
  setNames(c("rcID", "cluster"))

rc_merge <- merge(rc, tidy, by = "rcID")
st_write(rc_merge, "t3.gpkg")

## Save medoids
group_medoids <- plot_medoids(pm_2)
group_medoids

## Reattach geodemographic variables
groups <- cbind(pm_out, geodems)

# 7. Extracting Nested Types  ----------------------------------------------

## Get number of distinct groups
n <- c(unique(pm_out$cluster))

## Apply typology functions across number of groups
colnames(pm_out)[1] <- "rcID"
types <- lapply(n, function(x) get_nested_types(pm_out, cl = x, medoids = FALSE))
types_un <- do.call(rbind, types)

## Get medoids
medoids <- lapply(n, function(x) get_nested_types(pm_out, cl = x, medoids = TRUE))
medoids_un <- do.call(rbind, medoids)
type_medoids <- plot_type_medoids(medoids_un)
type_medoids

# 8. Writing Out ----------------------------------------------------------

rc_out <- rc %>%
  select(rcID)
rc_out <- merge(rc_out, types_un, by = "rcID")

st_write(rc_out, "Output Data/Typology/US_RC_Typology.gpkg")


