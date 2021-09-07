
### DON'T RUN - DESIGNED FOR LINUX

library(sf)
library(tidyverse)
library(data.table)
library(parallel)
library(factoextra)
library(fpc)
library(corrplot)
library(cluster)
library(e1071)
library(ggplot2)
library(corrr)
install.packages("corrr")
options(scipen = 999)
source("source_code/Helper Functions - Typology (Parallel).R")


# 1. Read in the data -----------------------------------------------------

## Vars
typ <- read.csv("input_data/US_Typology_Variables_minPts50_NEW.csv")

## Extract Vars
typ_vars <- typ %>%
  select(-c(X)) %>%
  select(2:38) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_all(~replace(., is.na(.), 0))

## Scale
### Z-scores
typ_z <- as.data.frame(scale(typ_vars, center = TRUE, scale = TRUE))

### 0 - 1
scaleit <- function(x) (scales::rescale(x, to = c(0 , 1)))
typ_n <- typ_vars %>% 
  mutate_all(., scaleit)


# 2. Skew -----------------------------------------------------------------

## Calculate & Format Skew
skew <- as.data.frame(sapply(typ_vars, function(x) skewness(x)))
skew <- skew %>%
  rownames_to_column("variable") %>%
  set_names(c("Variable", "Skew"))


# 3. Descriptives ---------------------------------------------------------

## Median Vals
median_vals <- typ_vars %>%
  summarise_if(is.numeric, median) %>%
  gather()

## Std Dev Vals
st_dev_vals <- typ_vars %>%
  summarise_if(is.numeric, sd) %>%
  gather()


# 4. Collinearity ---------------------------------------------------------

## Build correlation matrix
cor_m <- cor(typ_n)

## P vals
p.mat <- cor.mtest(typ_n)

## Corr plot
corrplot(cor_m, type = "upper", method = "color",
         insig = "blank", tl.srt = 90, tl.col = "black", p.mat = p.mat$p)

## Extract correlations over 70%
any_over_70 <- function(x) any(x > .7, na.rm = TRUE)
any_over_neg70 <- function(x) any(x < -0.7, na.rm = TRUE)

## High correlations
rs %>% focus_if(any_over_70, mirror = TRUE)
## High negative correlations
rs %>% focus_if(any_over_neg70, mirror = TRUE)


## Remove the highly linear variables
typ_z <- typ_z %>%
  select(-c(propConsumerService, propNationalChain, nationalCatDiversity))


# 5. Dimensionality Reduction ---------------------------------------------


# 6. Identifying K -----------------------------------------------------------

## Print average Silhouette Scores
s_scores <- get_silhouette_scores(typ_z, 123)
s_scores




# 7. Typology - Groups ----------------------------------------------------

## Get out groups
pm <- run_typology(typ_z, 2)
pm_out <- pm[[1]]

## Plot medoids
plot_medoids(pm)


# 8. Typology - Types -----------------------------------------------------

## Run PCA on the groups to identify insignificant vars
pca1 <- run_type_pca(pm_out, 1)
pm

## Calculate k parameters for the types
k-vals <- 
  