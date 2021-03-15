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
  db_cl <- cbind(db, cl_out)
  
  ## Computation of silhouette scores
  col_ls <- c(12:20)
  ss <- lapply(col_ls, function(x) silhouette(db_cl[, x], dist(db_cl[, 1:11])))
  
  ## Computation of average silhouette scores
  avg_ss <- lapply(ss, function(x) mean(x[, 3]))
  avg_ss <- as.data.frame(do.call(rbind, avg_ss))
  avg_ss$k <- c(2:10)
  avg_ss <- avg_ss %>% rename(avg_score = V1)
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
  med$cluster <- c(1:3)
  med <- med %>%
    gather(variable, cluster_vals, -c(cluster))
  
  out <- list(cl, med)
  return(out)
}