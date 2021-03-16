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
  
  ## Drop out the retail centres with < 20 pts
  boundaries <- boundaries %>% filter(n.pts >= 20)
  hexes <- hexes %>% filter(rcID %in% boundaries$rcID)
  
  ## 1. n.units and n.hexes ########################
  boundaries <- boundaries %>%
    rename(n.units = n.pts)
  hex_count <- st_intersection(boundaries, hexes)
  rc_grouped <- hex_count %>%
    as.data.frame() %>%
    group_by(rcID, rcName, n.units) %>%
    summarise(n.hexes = n())
  
  ## 2. proportions of comparison, convenience, leisure and service retail ##########################
  pt_count <- st_intersection(boundaries, pts)
  cat_props<- pt_count %>%
    group_by(rcID, rcName) %>%
    count(ldc_aggregation) %>%
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
  chain_or_ind <- data.table::fread("Output Data/SafeGraph_Places_US_Updated_Chains.csv")
  pt_count <- merge(pt_count, chain_or_ind, by = c("safegraph_place_id", "location_name"), all.x = TRUE)
  diversity <- pt_count %>%
    group_by(rcID, rcName) %>%
    dplyr::count(chain_or_independent) %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    spread(chain_or_independent, n) %>%
    mutate(Chain = replace(Chain, is.na(Chain), 0)) %>%
    mutate(Independent = replace(Independent, is.na(Independent), 0)) %>%
    mutate(total = Chain + Independent) %>%
    mutate(pct_Independent = (Independent / total) * 100) %>%
    mutate(pct_Chain = (Chain / total) * 100) %>%
    select(c(rcID, rcName, pct_Independent, pct_Chain))
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
  
  
  ## 7. economic performance
  
  ## Read in Patterns
  ptns <- data.table::fread("Input Data/Patterns/NorthEast_Patterns_08_06_2020.csv")
  ptns <- ptns %>%
    select(safegraph_place_id, raw_visit_counts, raw_visitor_counts, distance_from_home, median_dwell) %>%
    drop_na(raw_visit_counts, raw_visitor_counts, distance_from_home, median_dwell)
  
  ## Merge onto main dataset
  cl_ptns <- merge(pt_count, ptns, by = "safegraph_place_id", all.x = TRUE)
  cl_ptns <- cl_ptns %>%
    as.data.frame() %>%
    select(rcID, rcName, raw_visit_counts, raw_visitor_counts, distance_from_home, median_dwell) %>%
    drop_na(raw_visit_counts, raw_visitor_counts, distance_from_home, median_dwell) %>%
    group_by(rcID, rcName) %>%
    mutate(rc_visits = sum(raw_visit_counts), rc_visitors = sum(raw_visitor_counts), 
           rc_distance_travelled = median(distance_from_home), rc_median_dwell = median(median_dwell)) %>%
    select(rcID, rcName, rc_visits, rc_visitors, rc_distance_travelled, rc_median_dwell) %>% 
    rename(total_visits = rc_visits, total_visitors = rc_visitors, median_distance = rc_distance_travelled, median_dwell = rc_median_dwell) %>%
    unique()
  ## Merge
  rc_grouped <- merge(rc_grouped, cl_ptns, by = c("rcID", "rcName"), all.x = TRUE)
  
  ## 8. return ######################
  return(rc_grouped)
  
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
