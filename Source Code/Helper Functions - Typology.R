## Helper Functions (2) - Retail Centre Typology


## Function that reads in SafeGraph retail points for a state of interest
read_points <- function(state = "AL") {
  
  ## Read in the points for the selected state
  query <- paste0("select* from SafeGraph_Retail_Places_US where region = '", state, "'")
  pts <- st_read("Output Data/SafeGraph_Retail_Places_US.gpkg", query = query)
  pts <- st_transform(pts, 4326)
  return(pts)
}

## Function that reads in the points for NE 
read_region_points <- function(region = "NE") {
  
  ## Lapply function on states within the region 
  ls <- c("CT", "DC", "DE", "MA", "MD", "ME", "NH", "NJ", "NY", "PA", "RI", "VT")
  ls_pts <- lapply(ls, read_points)
  ls_pts <- do.call(rbind, ls_pts)
  return(ls_pts)
  
}

## Region State Lists
ne <- c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT")
mw <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
s <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV",
       "AL", "KY", "MS", "TN",
       "AR", "LA", "OK", "TX")
w <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
       "AK", "CA", "HI", "OR", "WA")


## Function that extracts all the variables needed to run the typology for the retail centres
prep4typology <- function(state) {
  
  ## Read in the datasets we need for this
  boundaries <- st_read(paste0("Output Data/Retail Centres/", state, "_RC_Boundaries.gpkg"))
  hexes <- st_read(paste0("Output Data/Retail Centres/", state, "_RC_Hexes.gpkg"))
  pts <- read_points(state = state)
  bdgs <- st_read(paste0("Output Data/Buildings/", state, "_Retail_Buildings.gpkg"))
  
  ## Drop out the retail centres with < 20 pts
  boundaries <- boundaries %>% filter(n.pts >= 20)
  hexes <- hexes %>% filter(rcID %in% boundaries$rcID)
  
  ## 1. n.features & area ########################
  boundaries <- boundaries %>%
    dplyr::rename(n.units = n.pts)
  
  ## Count number of H3 hexes
  hex_count <- st_intersection(boundaries, hexes)
  rc_grouped <- hex_count %>%
    as.data.frame() %>%
    group_by(rcID, rcName, n.units) %>%
    dplyr::summarise(n.hexes = n())
  
  ## Count number of Buildings
  bdg_count <- boundaries %>%
    st_join(bdgs) %>%
    as.data.frame() %>%
    select(rcID, rcName) %>%
    group_by(rcID, rcName) %>%
    dplyr::summarise(n.bdgs = n())
  rc_grouped <- merge(rc_grouped, bdg_count, by = c("rcID", "rcName"), all.x = TRUE)
  
  ## Calculate Area
  boundaries$area <- st_area(boundaries)
  
  ## Calculate Retail Building Density
  densities <- merge(boundaries, bdg_count, by = c("rcID", "rcName"), all.x = TRUE)
  densities <- densities %>%
    as.data.frame() %>%
    select(rcID, rcName, n.bdgs, area) %>%
    mutate(bdg_density = n.bdgs/area) %>%
    select(-c(n.bdgs)) %>%
    mutate_at(vars(area, bdg_density), as.numeric)
  rc_grouped <- merge(rc_grouped, densities, by = c("rcID", "rcName"), all.x = TRUE)
  
  ## 2. proportions of comparison, convenience, leisure and service retail ##########################
  pt_count <- st_intersection(boundaries, pts)
  cat_props<- pt_count %>%
    group_by(rcID, rcName) %>%
    dplyr::count(ldc_aggregation) %>%
    as.data.frame() %>%
    select(-c(geom)) %>%
    spread(ldc_aggregation, n)  %>%
    mutate(SERVICES = replace(SERVICES, is.na(SERVICES), 0)) %>%
    mutate(LEISURE = replace(LEISURE, is.na(LEISURE), 0)) %>%
    mutate(COMPARISON = replace(COMPARISON, is.na(COMPARISON), 0)) %>%
    mutate(CONVENIENCE = replace(CONVENIENCE, is.na(CONVENIENCE), 0)) %>%
    mutate(TOTAL = COMPARISON + CONVENIENCE + SERVICES + LEISURE) %>%
    mutate(pct_Comparison = (COMPARISON / TOTAL) * 100) %>%
    mutate(pct_Convenience = (CONVENIENCE / TOTAL) * 100) %>%
    mutate(pct_Service = (SERVICES / TOTAL) * 100) %>%
    mutate(pct_Leisure = (LEISURE / TOTAL) * 100) %>%
    select(-c(TOTAL, COMPARISON, CONVENIENCE, LEISURE, SERVICES))
  rc_grouped <- merge(rc_grouped, cat_props, by = c("rcID", "rcName"), all.x = TRUE)
  
  ## 3. diversity ########################
  distinct_cats <- pt_count %>%
    as.data.frame() %>%
    group_by(rcID, rcName) %>%
    dplyr::summarise(top_category_total = n_distinct(top_category),
                     sub_category_total = n_distinct(sub_category))
  ### we know that there are 176 distinct top categories in the US, and 357 distinct sub categories
  distinct_cat <- distinct_cats %>%
    mutate(top_category_diversity = (top_category_total / 176)*100, 
           sub_category_diversity = (sub_category_total / 357)*100) %>%
    select(rcID, rcName, top_category_diversity, sub_category_diversity)
  rc_grouped <- merge(rc_grouped, distinct_cat, by = c("rcID", "rcName"), all.x = TRUE)
  
  
  ## 4. chains vs independents #####################
  chain_or_ind <- data.table::fread("Output Data/Typology/SafeGraph_Places_US_Updated_Chains.csv")
  pt_count <- merge(pt_count, chain_or_ind, by = c("safegraph_place_id", "location_name"), all.x = TRUE)
  diversity <- pt_count %>%
    group_by(rcID, rcName) %>%
    dplyr::count(chain_or_independent) %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    spread(chain_or_independent, n) %>%
    select("rcID", "rcName", "Independent", "National Chain", "Small Multiple") %>%
    set_names(c("rcID", "rcName", "Independent", "NationalChain", "SmallMultiple")) %>%
    mutate(Independent = replace(Independent, is.na(Independent), 0)) %>%
    mutate(NationalChain = replace(NationalChain, is.na(NationalChain), 0)) %>%
    mutate(SmallMultiple = replace(SmallMultiple, is.na(SmallMultiple), 0)) %>%
    mutate(total = Independent + NationalChain + SmallMultiple) %>%
    mutate(pct_Independent = (Independent / total) * 100) %>%
    mutate(pct_National_Chain = (NationalChain / total) * 100) %>%
    mutate(pct_Small_Multiple = (SmallMultiple / total) * 100) %>%
    select(c(rcID, rcName, pct_National_Chain, pct_Small_Multiple, pct_Independent))
  rc_grouped <- merge(rc_grouped, diversity, by = c("rcID", "rcName"), all.x = TRUE)
  
  
  ## 5. centre compactness ###################
  boundaries$area <- st_area(boundaries)
  boundaries_area <- boundaries %>%
    select(rcID, rcName, area)
  circ <- lwgeom::st_minimum_bounding_circle(boundaries_area)
  circ$area <- st_area(circ)
  circ <- circ %>%
    as.data.frame() %>%
    select(rcID, rcName, area)
  roeck <- merge(boundaries, circ, by = c("rcID", "rcName"), all.x = TRUE)
  roeck$roeck <- as.numeric(roeck$area.x / roeck$area.y)
  roeck$roeck <- scales::rescale(roeck$roeck)
  roeck <- roeck %>%
    as.data.frame() %>%
    select(rcID, rcName, roeck)
  rc_grouped <- merge(rc_grouped, roeck, by = c("rcID", "rcName"), all.x = TRUE)
  
  ## 6. geodemographics ######################
  
  ## Read in Geodemographic variables
  geodemo <- data.table::fread("Output Data/Typology/NE_Geodemographics.csv")
  geodemo <- geodemo %>%
    select(-c(V1))
  
  ## Merge onto main dataset
  rc_grouped <- merge(rc_grouped, geodemo, by = c("rcID", "rcName"), all.x = TRUE)
  
  ## BELOW IS HOW TO OBTAIN NE_GEODEMOGRAPHICS TABLE:
  
  # ## Build centroids for the centres
  # centroids <- st_centroid(boundaries)
  # centroids <- st_transform(centroids, 32616)
  # 
  # ## Build a buffer to extract features from 
  # catchment <- st_buffer(centroids, 1000)
  # catchment <- catchment %>% select(rcID, rcName)
  # 
  # ## Read in the Geodemographic/Population dataset
  # gd_pop <- st_read("Input Data/Geodemographics/NE_Geodemographic_Population.gpkg")
  # gd_pop <- gd_pop %>%
  #   select(GEOID, US_GeoDemo_Lookup_Spielman_Singleton_Group, Tot_Pop_2019) %>%
  #   dplyr::rename(Spielman_Singleton_Group = US_GeoDemo_Lookup_Spielman_Singleton_Group)
  # 
  # ## Calculate total pop for each of the Geodemographic categories
  # ne_pop <- gd_pop %>%
  #   as.data.frame() %>%
  #   select(Spielman_Singleton_Group, Tot_Pop_2019) %>%
  #   group_by(Spielman_Singleton_Group) %>%
  #   dplyr::summarise(gd_total = sum(Tot_Pop_2019))
  # ne_pop$ne_total <- sum(ne_pop$gd_total)
  # ne_pop$gd_prop <- (ne_pop$gd_total / ne_pop$ne_total) * 100
  # 
  # ## Calculation total pop in each of the Retail Centres
  # rc_pop <- catchment %>%
  #   st_transform(4326) %>%
  #   st_join(gd_pop) %>%
  #   as.data.frame() %>%
  #   select(rcID, rcName, Tot_Pop_2019) %>%
  #   group_by(rcID, rcName) %>%
  #   dplyr::summarise(total_rc_pop = sum(Tot_Pop_2019))
  # 
  # ## Calculate total population belonging to each geodemographic group, in each retail centre
  # rc_gd <- catchment %>%
  #   st_transform(4326) %>%
  #   st_join(gd_pop) %>%
  #   as.data.frame() %>%
  #   select(-c(geom)) %>%
  #   group_by(rcID, rcName, Spielman_Singleton_Group) %>%
  #   dplyr::summarise(total_gd_pop = sum(Tot_Pop_2019)) %>%
  #   spread(Spielman_Singleton_Group, total_gd_pop, fill = 0) 
  # 
  # 
  # ## Add in any extra columns that may be missing 
  # cols <- c("A: Hispanic & Kids" = NA_real_, "B: Wealthy Nuclear Families" = NA_real_,
  #           "C: Middle Income, Single Family Homes" = NA_real_, "D: Native American" = NA_real_,
  #           "E: Wealthy Urbanites" = NA_real_, "F: Low Income and Diverse" = NA_real_,
  #           "G: Old, Wealthy White" = NA_real_, "H: Low Income, Minority Mix" = NA_real_,
  #           "I: African American Adversity" = NA_real_, "J: Residential Institutions, Young People" = NA_real_)
  # col_list <- c("rcID", "rcName", "A: Hispanic & Kids", "B: Wealthy Nuclear Families", "C: Middle Income, Single Family Homes" ,
  #               "D: Native American", "E: Wealthy Urbanites", "F: Low Income and Diverse", "G: Old, Wealthy White",
  #               "H: Low Income, Minority Mix", "I: African American Adversity", "J: Residential Institutions, Young People")
  # rc_gd<- add_column(rc_gd, !!!cols[setdiff(names(cols), names(rc_gd))])
  # rc_gd <- rc_gd[,col_list]
  # 
  # ## Convert to proportions, and divide by national level proportions
  # rc_gd_pop <- merge(rc_gd, rc_pop, by = c("rcID", "rcName"))
  # colnames(rc_gd_pop) <- c("rcID", "rcName", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "total_rc_pop")
  # rc_gd_pop <- rc_gd_pop %>%
  #   group_by(rcID, rcName) %>%
  #   mutate(GroupA_prop = (A/total_rc_pop) * 100, GroupB_prop = (B/total_rc_pop) * 100, GroupC_prop = (C/total_rc_pop) * 100,
  #          GroupD_prop = (D/total_rc_pop) * 100, GroupE_prop = (E/total_rc_pop) * 100, GroupF_prop = (F/total_rc_pop) * 100, 
  #          GroupG_prop = (G/total_rc_pop) * 100, GroupH_prop = (H/total_rc_pop) * 100, GroupI_prop = (I/total_rc_pop) * 100,
  #          GroupJ_prop = (J/total_rc_pop) * 100) %>%
  #   select(rcID, rcName, GroupA_prop, GroupB_prop, GroupC_prop, GroupD_prop, GroupE_prop, 
  #          GroupF_prop,GroupG_prop, GroupH_prop, GroupI_prop, GroupJ_prop) %>%
  #   mutate(GroupA_NE_prop = GroupA_prop / 5.62, GroupB_NE_prop = GroupB_prop / 35.6, GroupC_NE_prop = GroupC_prop / 28.7,
  #          GroupD_NE_prop = GroupD_prop / 0.0327, GroupE_NE_prop = GroupE_prop / 4.67, GroupF_NE_prop = GroupF_prop / 2.15, 
  #          GroupG_NE_prop = GroupG_prop / 3.31, GroupH_NE_prop = GroupH_prop / 15.6, GroupI_NE_prop = GroupI_prop / 1.48, 
  #          GroupJ_NE_prop = GroupJ_prop / 2.79) %>%
  #   select(rcID, rcName,
  #          GroupA_NE_prop, GroupB_NE_prop, GroupC_NE_prop, GroupD_NE_prop,
  #          GroupE_NE_prop, GroupF_NE_prop, GroupG_NE_prop, GroupH_NE_prop,
  #          GroupI_NE_prop, GroupJ_NE_prop)
  # 
  # ## Merge onto main dataset
  # rc_grouped <- merge(rc_grouped, rc_gd_pop, by = c("rcID", "rcName"), all.x = TRUE)
  
  
  ## 7. economic performance
  
  # ## Read in Patterns
  # ptns <- data.table::fread("Input Data/Patterns/NorthEast_Patterns_08_06_2020.csv")
  # ptns <- ptns %>%
  #   select(safegraph_place_id, raw_visit_counts, raw_visitor_counts, distance_from_home, median_dwell) %>%
  #   drop_na(raw_visit_counts, raw_visitor_counts, distance_from_home, median_dwell)
  # 
  # ## Merge onto main dataset
  # cl_ptns <- merge(pt_count, ptns, by = "safegraph_place_id", all.x = TRUE)
  # cl_ptns <- cl_ptns %>%
  #   as.data.frame() %>%
  #   select(rcID, rcName, raw_visit_counts, raw_visitor_counts, distance_from_home, median_dwell) %>%
  #   drop_na(raw_visit_counts, raw_visitor_counts, distance_from_home, median_dwell) %>%
  #   group_by(rcID, rcName) %>%
  #   dplyr::summarise(rc_visits = sum(raw_visit_counts), rc_visitors = sum(raw_visitor_counts), 
  #          rc_distance_travelled = median(distance_from_home), rc_median_dwell = median(median_dwell)) %>%
  #   select(rcID, rcName, rc_visits, rc_visitors, rc_distance_travelled, rc_median_dwell) %>% 
  #   dplyr::rename(total_visits = rc_visits, total_visitors = rc_visitors, median_distance = rc_distance_travelled, median_dwell = rc_median_dwell) %>%
  #   unique()
  # ## Merge
  # rc_grouped <- merge(rc_grouped, cl_ptns, by = c("rcID", "rcName"), all.x = TRUE)
  

  # 8. urban morphology -----------------------------------------------------
  
  ## Read in the Smart Location data for the NorthEast
  sl <- st_read("Input Data/Smart Location/NorthEast_SL.gpkg")
  
  ## Join data with retail centres
  sl_rc <- st_join(boundaries, sl)
  
  ## Fix numbers in Dist_to_Transit column - values of -9999 are allocated when distance to transit is > 1200m, so we allocate these as
  ## 1225m (just bigger than 3/4 of a mile)
  sl_rc$Dist_to_Transit[sl_rc$Dist_to_Transit == -99999] <- 1225
  
  ## Compute variables
  sl_out <- sl_rc %>%
    as.data.frame() %>%
    group_by(rcID, rcName) %>%
    summarise(Tot_Res_Count = sum(Res_Count), Med_Res_Density = median(Res_Density), Med_Emp_Density = median(Emp_Density),
              Med_Retail_Emp_Density = median(Retail_Emp_Density), Med_Road_Density = median(Road_Density),
              Med_Dist_to_Transit = median(Dist_to_Transit))
  
  ## Join 
  rc_grouped <- merge(rc_grouped, sl_out, by = c("rcID", "rcName"))
  
  
  ## 9. return ######################
  
  ## Merge dataframe onto retail centre boundaries
  boundaries_out <- boundaries %>%
    select(rcID, rcName)
  boundaries_out <- merge(boundaries_out, rc_grouped, by = c("rcID", "rcName"), all.x = TRUE)
  return(boundaries_out)
  
  # ## Return a list - dataframe containing variables for analysis, and boundaries with variables attached
  # out <- list(rc_grouped, boundaries_out)
  
  ## Write them out
  # write.csv(rc_grouped, "Output Data/Typology/NE_typ.csv")
  # st_write(boundaries_out, "Output Data/Typology/NE_typ.gpkg")
  
}


## Function that computes average silhouette scores across values of K, for determing K value used in PAM
get_silhouette_scores <- function(db, seed = 123) {
  
  ## Params
  set.seed(seed)
  k_ls <- c(2:10)
  
  ## Run PAM
  cl <- lapply(k_ls, function(x) pam(db, k = x))
  cl <- lapply(cl, function(x) {
    X <- x$clustering
    return(X)
  })
  
  cl_out <- as.data.frame(do.call(cbind, cl))
  colnames(cl_out) <- c("k=2", "k=3", "k=4", "k=5", "k=6", "k=7", "k=8", "k=9", "k=10")
  #db_cl <- cbind(db, cl_out)
  
  ## Computation of silhouette scores
  col_ls <- c(1:9)
  ss <- lapply(col_ls, function(x) silhouette(cl_out[, x], dist(db)))
  
  ## Computation of average silhouette scores
  avg_ss <- lapply(ss, function(x) mean(x[, 3]))
  avg_ss <- as.data.frame(do.call(rbind, avg_ss))
  avg_ss$k <- c(2:10)
  avg_ss$k <- paste0("k= ", avg_ss$k)
  colnames(avg_ss) <- c("avg_silhouette_score", "k")
  avg_ss <- avg_ss %>% select(k, avg_silhouette_score)
  avg_ss ## Print output
  
}

## Function for extracting cluster id's and cluster medoids from PAM
run_typology <- function(db, k = 3) {
  
  ## Run it
  pm <- pam(x = db, k = k, metric = "euclidean")
  
  ## Extract Clustering 
  cl <- as.data.frame(pm$clustering)
  cl <- cbind(cl, db)
  colnames(cl)[1] <- "cluster"
  
  ## Extract medoids
  med <- as.data.frame(pm$medoids)
  med$cluster <- seq_along(med[,1])
  med <- med %>%
    gather(variable, cluster_vals, -c(cluster)) %>%
    mutate(pos = cluster_vals >= 0)
  
  out <- list(cl, med)
  return(out)
}

## Function for re-running the typology, to get nested types 
get_nested_types <- function(groups, cl = 1, medoids = FALSE) {
  
  ## Filter to cluster
  groups <- groups %>%
    dplyr::filter(cluster == cl)
  
  ## Remove names/id's
  groups_df <- groups %>%
    as.data.frame() %>%
    select(-c(rcID, rcName, cluster, geom))
  
  ## Get the silhouette scores
  ss <- get_silhouette_scores(groups_df)
  
  ## Extract max
  ss_max <- ss[which.max(ss$avg_silhouette_score),]
  
  ## Run typology
  pm <- run_typology(groups_df, k = ss_max$k)
  
  ## Formatting for output
  clustering <- pm[[1]]
  clustering <- clustering %>%
    select(cluster) %>%
    dplyr::rename(type = cluster)
  
  ## Join with original groups and merge cluster and type columns together
  out <- cbind(groups, clustering)
  out <- out %>%
    dplyr::rename(group = cluster) %>%
    select(rcID, rcName, group, type) %>%
    transform(typ_id = paste(group, type, sep = "."))
  
  
  if (medoids == FALSE) {
    return(out)
  } else if (medoids == TRUE) {
    
    ## Get id's to merge
    type_ids <- out %>%
      as.data.frame() %>%
      select(group, type, typ_id) %>%
      unique()
    
    ## Extract medoids
    medoids <- pm[[2]]
    
    ## Attach new ID's 
    medoids <- merge(medoids, type_ids, by.x = "cluster", by.y = "type", all.x = TRUE)
    medoids <- medoids %>%
      select(typ_id, variable, cluster_vals, pos) %>%
      dplyr::rename(cluster = typ_id)
    return(medoids)
  }
  
}


## Plotting of Clustergram
## Clustergram
plot_clustergram <- function(Data, k.range = 2:10 , 
                        clustering.function = clustergram.kmeans,
                        clustergram.plot = clustergram.plot.matlines, 
                        line.width = .004, add.center.points = T)
{
  # Data - should be a scales matrix.  Where each column belongs to a different dimension of the observations
  # k.range - is a vector with the number of clusters to plot the clustergram for
  # clustering.function - this is not really used, but offers a bases to later extend the function to other algorithms 
  #			Although that would  more work on the code
  # line.width - is the amount to lift each line in the plot so they won't superimpose eachother
  # add.center.points - just assures that we want to plot points of the cluster means
  Data <- as.matrix(Data)
  n <- dim(Data)[1]
  
  PCA.1 <- Data %*% princomp(Data)$loadings[,1]	# first principal component of our data
  
  if(require(colorspace)) {
    COL <- heat_hcl(n)[order(PCA.1)]	# line colors
  } else {
    COL <- rainbow(n)[order(PCA.1)]	# line colors
    warning('Please consider installing the package "colorspace" for prittier colors')
  }
  
  line.width <- rep(line.width, n)
  
  Y <- NULL	# Y matrix
  X <- NULL	# X matrix
  
  centers.points <- list()
  
  for(k in k.range)
  {
    k.clusters <- clustering.function(Data, k)
    
    clusters.vec <- k.clusters$cluster
    # the.centers <- apply(cl$centers,1, mean)
    the.centers <- k.clusters$centers 
    
    noise <- unlist(tapply(line.width, clusters.vec, cumsum))[order(seq_along(clusters.vec)[order(clusters.vec)])]	
    # noise <- noise - mean(range(noise))
    y <- the.centers[clusters.vec] + noise
    Y <- cbind(Y, y)
    x <- rep(k, length(y))
    X <- cbind(X, x)
    
    centers.points[[k]] <- data.frame(y = the.centers , x = rep(k , k))	
    #	points(the.centers ~ rep(k , k), pch = 19, col = "red", cex = 1.5)
  }
  
  
  x.range <- range(k.range)
  y.range <- range(c(-5,10))
  
  clustergram.plot(X,Y, k.range, 
                   x.range, y.range , COL, 
                   add.center.points , centers.points)
  
  
}

## Plot medoids from PAM
plot_medoids <- function(pm) {
  
  ## Extract medoids from clustering object
  md <- pm[[2]]
  
  ## Split by cluster
  md_ls <- split(md, md$cluster)
  
  ## Plot all at once
  plots <- lapply(md_ls, function(x) ggplot(data = x) +
                    aes(x = reorder(variable, -cluster_vals), y = cluster_vals, fill = pos) +
                    geom_col(position = "identity", size = 0.25, colour = "black") + 
                    scale_fill_manual(values = c("#FFDDDD", "#CCEEFF"), guide = FALSE) +
                    xlab("Variable") +
                    ylab("Median Values") +
                    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.ticks = element_line(),
                          axis.line = element_line(colour = "black"), 
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank()))
  ggpubr::ggarrange(plotlist = plots, labels = md$cluster)
}



## Plot type medoids 
plot_type_medoids <- function(medoids) {
  
  ## Split by cluster
  md_ls <- split(medoids, medoids$cluster)
  
  ## Plot all at once
  plots <- lapply(md_ls, function(x) ggplot(data = x) +
                    aes(x = reorder(variable, -cluster_vals), y = cluster_vals, fill = pos) +
                    geom_col(position = "identity", size = 0.25, colour = "black") + 
                    scale_fill_manual(values = c("#FFDDDD", "#CCEEFF"), guide = FALSE) +
                    xlab("Variable") +
                    ylab("Median Values") +
                    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.ticks = element_line(),
                          axis.line = element_line(colour = "black"), 
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank()))
  ggpubr::ggarrange(plotlist = plots, labels = unique(medoids$cluster))
  
}





## Dependency function of Clustergram
clustergram.kmeans <- function(Data, k, ...)
{
  # this is the type of function that the clustergram
  # 	function takes for the clustering.
  # 	using similar structure will allow implementation of different clustering algorithms
  
  #	It returns a list with two elements:
  #	cluster = a vector of length of n (the number of subjects/items)
  #				indicating to which cluster each item belongs.
  #	centers = a k dimensional vector.  Each element is 1 number that represent that cluster
  #				In our case, we are using the weighted mean of the cluster dimensions by 
  #				Using the first component (loading) of the PCA of the Data.
  
  cl <- kmeans(Data, k,...)
  
  cluster <- cl$cluster
  centers <- cl$centers %*% princomp(Data)$loadings[,1]	# 1 number per center
  # here we are using the weighted mean for each
  
  return(list(
    cluster = cluster,
    centers = centers
  ))
}	

## Dependency function of Clustergram
clustergram.plot.matlines <- function(X,Y, k.range, 
                                      x.range, y.range , COL, 
                                      add.center.points , centers.points)
{
  plot(0,0, col = "white", xlim = x.range, ylim = y.range,
       axes = F,
       xlab = "Number of clusters (k)", ylab = "PCA weighted Mean of the clusters")
  axis(side =1, at = k.range)
  axis(side =2)
  abline(v = k.range, col = "grey")
  
  matlines(t(X), t(Y), pch = 19, col = COL, lty = 1, lwd = 1.5)
  
  if(add.center.points)
  {
    require(plyr)
    
    xx <- ldply(centers.points, rbind)
    points(xx$y~xx$x, pch = 19, col = "red", cex = 1.3)
    
    # add points	
    # temp <- l_ply(centers.points, function(xx) {
    # with(xx,points(y~x, pch = 19, col = "red", cex = 1.3))
    # points(xx$y~xx$x, pch = 19, col = "red", cex = 1.3)
    # return(1)
    # })
    # We assign the lapply to a variable (temp) only to suppress the lapply "NULL" output
  }	
}
