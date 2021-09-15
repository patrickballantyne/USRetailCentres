## Function for pulling in the patterns for a state
# get_patterns <- function(state = "AL", duckdb = mydb, week = "july2021") {
#   
#   ## Read in the patterns from the duck database
#   ptns <- dbGetQuery(mydb, paste0("SELECT * FROM", " ", week, " ", " WHERE region = '", state, "'"))
#   print("PATTERNS EXTRACTED")
#   
#   ## Read in the points to match 
#   pts_query <- paste0("select* from SafeGraph_Cleaned_Places_US where region = '", state, "'") 
#   pts <- st_read("Output Data/SafeGraph_Cleaned_Places_US.gpkg", query = pts_query)
#   pts <- pts %>% select(placekey)
#   print("POINTS EXTRACTED")
#   
#   ## Merge
#   ptns_sf <- merge(pts, ptns, by = "placekey", all.x = TRUE)
#   print(paste0(week, "PATTERNS CLEANED"))
#   return(ptns_sf)
# }

## State to Region Lookup
ne <- c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT")
mw <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
s <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV",
       "AL", "KY", "MS", "TN",
       "AR", "LA", "OK", "TX")
w <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
       "AK", "CA", "HI", "OR", "WA")


## Function for reading in the patterns
get_patterns <- function(iteration = "May2021") {
  
  ## Read in
  ptn <- vroom(paste0("Output Data/Catchments/Patterns/", iteration, ".tsv"))
  ptn <- ptn %>%
    select(placekey, region, raw_visit_counts, visitor_home_cbgs, visitor_home_aggregation) %>%
    rename(state = region)
  return(ptn)
}
## Get retail centres
get_rc <- function(state = "AL") {
  
  rc_query <- paste0("select* from US_RC_minPts50 where State = '", state, "'") 
  rc <- st_read("Output Data/Retail Centres/US Retail Centres/US_RC_minPts50.gpkg", query = rc_query)
  return(rc)
}

## Function that reads in SafeGraph retail points for a state of interest
get_pts <- function(state = "AL") {
  
  ## Read in the points for the selected state
  query <- paste0("select* from SafeGraph_Cleaned_Places_US where region = '", state, "'")
  pts <- st_read("Output Data/SafeGraph_Cleaned_Places_US.gpkg", query = query)
  return(pts)
}

## Function that obtains Huff Probabilities for all Retail Centres in a state
get_predicted_patronage <- function(state = "AL") {
  
  ## First load up the attractiveness scores 
  attr <- get_attractive(state = state)
  print("ATTRACTIVENESS SCORES COMPUTED")
  
  ## Then compute the distances
  dist <- get_network(state = state)
  print("NETWORK DISTANCES COMPUTED")
  
  ## Prepare data for Huff model
  rc <- get_rc(state)
  rc_out <- rc %>% as.data.frame() %>% select(rcID)
  rc_out <- merge(rc_out, attr, by = "rcID", all.x = TRUE)
  rc_out <- merge(rc_out, dist, by = "rcID", all.y = TRUE)
  
  ## Run huff model
  huff <- get_huff(rc_out, alpha = 1, beta = 2)
  print("HUFF MODEL COMPLETE")
  return(huff)
}

## Function that uses SafeGraph patterns to construct 'observed patronage' probabilities
get_observed_patronage <- function(state = "AL") {
  
  ## Pull in patterns for state
  ptns <- vroom("Output Data/Catchments/Patterns/May2021.tsv")
  ptns <- ptns %>%
    filter(region == state) %>%
    as.data.frame() %>%
    mutate_all(na_if, "")
  
  ## Clean out the information about census block groups
  poi_home_cbgs <- expand_cat_json(ptns,
                                   expand = "visitor_home_cbgs",
                                   index = "visitor_home_cbg",
                                   by = "placekey")
  
  ## Pull in list of intersecting places w/ retail centres
  rc <- get_rc(state = state) 
  rc <- rc %>% 
    select(rcID) %>%
    st_transform(4326)
  
  pts <- get_pts(state = state)
  pts <- pts %>%
    select(placekey) %>%
    st_set_crs(4326)
  
  int <- st_intersection(pts, rc)
  int <- int %>%
    as.data.frame() %>%
    select(placekey, rcID)
  
  ## Merge intersection onto patterns
  ptn_out <- merge(poi_home_cbgs, int, by = "placekey", all.y = TRUE)
  ptn_out <- ptn_out %>%
    setNames(c("placekey", "census_block_group", "totalVisits", "rcID")) %>%
    filter(!is.na(totalVisits))
  
  ## Compute totals per RC and RC/CBG
  rc_total <- ptn_out %>%
    group_by(rcID, census_block_group) %>%
    summarise(CBG_RC_visits = sum(totalVisits)) %>%
    setNames(c("rcID", "census_block_group", "totalVisitsRC"))
  cbg_total <- ptn_out %>%
    group_by(census_block_group) %>%
    summarise(CBG_visits = sum(totalVisits)) %>%
    setNames(c("census_block_group", "totalVisitsCBG"))
  
  ## Merge onto the Census Blocks
  cbg <- st_read("Input Data/Census Block Groups/US_Census_Block_Groups.gpkg")
  cbg <- cbg %>%
    select(CBG_ID) %>%
    rename(census_block_group = CBG_ID)
  
  cbg_merge <- merge(cbg, rc_total, by = "census_block_group", all.x = TRUE)
  cbg_merge <- merge(cbg_merge, cbg_total, by = "census_block_group", all.x = TRUE)
  cbg_merge <- cbg_merge %>%
    select(census_block_group, rcID, totalVisitsRC, totalVisitsCBG) %>%
    mutate(propVisits = (totalVisitsRC / totalVisitsCBG) * 100) %>%
    select(census_block_group, rcID, totalVisitsRC, totalVisitsCBG, propVisits) %>%
    mutate_if(is.character, as.factor)
  return(cbg_merge)
  
}

## Get attractiveness score for a set of retail centres
get_attractive <- function(rc, state = "AL") {
  
  ## Read in centres
  #rc <- get_rc(state = state)
  rc_out <- rc %>% 
    as.data.frame() %>%
    select(rcID, rcName, N.pts)
  
  ## Get points and patterns
  pts <- get_pts(state = state)
  pts <- pts %>% select(top_category, sub_category) %>% st_set_crs(4326)
  ptns <- get_patterns(state = state, mydb, "july2021")
  ptns <- ptns %>% select(placekey, raw_visit_counts) %>% st_set_crs(4326)
  
  ## Intersections
  pts_int <- st_intersection(pts, rc)
  ptns_int <- st_intersection(ptns, rc)
  
  ## Calculate category diversity
  topcat_total <- 190 ## n. of distinct safegraph top categories
  subcat_total <- 378 ## n. of distinct safegraphg sub categories
  div <- pts_int %>%
    as.data.frame() %>%
    select(rcID, top_category, sub_category) %>%
    group_by(rcID) %>%
    summarise(n_topcat = n_distinct(top_category),
              n_subcat = n_distinct(sub_category)) %>%
    mutate(top_cat_prop = n_topcat / topcat_total, 
           sub_cat_prop = n_subcat / subcat_total)
  
  ## Calculate total visits
  visits <- ptns_int %>%
    as.data.frame() %>%
    select(rcID, raw_visit_counts) %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    group_by(rcID) %>%
    summarise(total_visits = sum(raw_visit_counts))
  
  ## Merge and calculate better version of total visits
  attr <- merge(rc_out, div, by = "rcID")
  attr <- merge(attr, visits, by = "rcID")
  attr$visits_prop <- attr$total_visits / attr$N.pts
  
  ## Rescale
  attr$r_N.pts <- scales::rescale(attr$N.pts, to = c(0, 1))
  attr$r_top_cat_prop <- scales::rescale(attr$top_cat_prop, to = c(0, 1))
  attr$r_visits_prop <- scales::rescale(attr$visits_prop, to = c(0, 1))
  
  ## Build indicator
  attr <- attr %>%
    select(rcID, r_N.pts, r_top_cat_prop, r_visits_prop) %>%
    mutate(attr_score = r_N.pts + r_top_cat_prop + r_visits_prop) %>%
    select(rcID, attr_score)
  return(attr)
  
}

## Function returning the euclidean distances between two objects - designed for CBGs and Retail Centres
get_euclidean <- function(x, y) {
  
  ## Row and column names
  cols <- as.vector(y$rcID)
  rows <- as.vector(x$Census_Block_Group)
  
  ## Run distance calculation 
  dist_df <- as.data.frame(st_distance(x, y))
  
  ## Tidy up output
  colnames(dist_df) <- cols
  dist_df$Census_Block_Group <- rows
  dist_df <- dist_df %>%
    gather(rcID, Distance, -Census_Block_Group) %>%
    mutate(Distance = Distance / 1000) %>%
    mutate(Distance = gsub("[km]", "", Distance)) %>%
    mutate(Distance = as.numeric(Distance)) 
  
  return(dist_df)
}

## Function for returning the network distances between two objects - designed for CBGs and Retail Centres
get_network <- function(state = "AL") {
  
  ## Read in the Census Blocks 
  blocks <- st_transform(st_read("Input Data/Census Block Groups/US_Census_Block_Groups.gpkg"), 32616)
  blocks <- blocks %>%
    rename(Census_Block_Group = CBG_ID)
  
  ## Read in the retail centres
  rc <- get_rc(state = state)
  rc <- st_transform(rc, 32616)
  
  ## Split by retail centre & compute distances 
  rc_ls <- split(rc, seq(nrow(rc)))
  distances <- lapply(rc_ls, function(x) {
    
    ## Create a buffer for each retail centre (50km)
    rc_buffer <- st_transform(st_buffer(x, 50000))
    
    ## Get blocks in the buffer
    blocks_sub <- blocks[rc_buffer, op = st_within]
    
    ## Centroids for distances
    rc_cent <- st_centroid(x)
    cbg_cent <- st_centroid(blocks_sub)
    
    ## Compute distances
    dist <- route_matrix(rc_cent, cbg_cent, routing_mode = "fast", transport_mode = "car")
    dist$rcID <- x$rcID
    dist$Census_Block_Group <- cbg_cent$Census_Block_Group
    dist})
  
  dist_df <- do.call(rbind, distances)
  dist_df <- dist_df %>%
    as.data.frame() %>%
    select(rcID, Census_Block_Group, distance) %>%
    rename(Distance = distance) %>%
    mutate(Distance = Distance / 1000)
  return(dist_df)

}

## Huff Model Specification - as input we need to have the retail centre ID, and then the different parameters we want to use
## in the model - attractiveness scores, distances and then alpha & beta values
get_huff <- function(huff_inputs, alpha = 1, beta = 2) {
  
  ## Pull out features we want
  cl <- huff_inputs %>%
    select(rcID, Census_Block_Group, attr_score, Distance) %>%
    mutate(Distance = ifelse(Distance == 0, 0.01, Distance))
    
 ## Numerator 
 numerator <- cl %>%
   mutate(numerator = (attr_score ^ alpha) / (Distance ^ beta))
 
 ## Denominator
 denominator <- numerator %>%
   group_by(Census_Block_Group) %>%
   summarise(denominator = sum(numerator))
 
 ## Calculate Huff Probability 
 huff_probs <- merge(numerator, denominator, by = "Census_Block_Group", all.x = TRUE)
 huff_probs$huff_probability <- huff_probs$numerator / huff_probs$denominator
 huff_probs$alpha <- alpha
 huff_probs$beta <- beta
 return(huff_probs)  
    
}

## Function that iterates through a list of values
huff_experiment <- function(huff_inputs) {
  
  ## Pull out features we want
  cl <- huff_inputs %>%
    select(rcID, Census_Block_Group, attr_score, Distance) %>%
    mutate(Distance = ifelse(Distance == 0, 0.01, Distance))
  
  ## Create the parameters we want to use
  
  ### Set alpha & beta range
  alpha_ls <- c(0.1, 0.5, 1.0, 2.0, 5.0)
  beta_ls <- c(0.1, 0.5, 1.0, 2.0, 5.0)
  
  ### Convert to format that fits lapply
  params <- expand.grid(alpha_ls, beta_ls)
  params <- params %>%
    setNames(c("alpha", "beta"))
  params <- split(params, seq(nrow(params)))
  
  ## Numerator 
  test <- lapply(params, function(o) {
    numerator <- cl %>%
      mutate(numerator = (attr_score ^ o$alpha) / (Distance ^ o$beta))
    denominator <- numerator %>%
      group_by(Census_Block_Group) %>%
      summarise(denominator = sum(numerator))
    huff_probs <- merge(numerator, denominator, by = "Census_Block_Group", all.x = TRUE)
    huff_probs$huff_probability <- huff_probs$numerator / huff_probs$denominator
    huff_probs$alpha <- o$alpha
    huff_probs$beta <- o$beta
    huff_probs
  })
  
  out_df <- do.call(rbind, test)
  
}

## Function that splits the different iterations of huff (a & b vals) and merges them onto the main dataset
clean_huff_experiment <- function(huff_experiment, observed_probs) {
  
  ## Get out variables we want
  huff_experiment <- huff_experiment %>%
    select(Census_Block_Group, rcID, huff_probability, alpha, beta) %>%
    rename(Predicted_Probability = huff_probability)
  
  ## Iterate through each set of values (unique alphas & betas) and perform join
  he_ls <- split(huff_experiment, list(huff_experiment$alpha, huff_experiment$beta))
  join <- lapply(huff_experiment, function(n) {
    
    ## Join onto CBGs
    cbg_join <- merge(cbg, n, by = "Census_Block_Group", all.y = TRUE)
    
    ## Join on the observed probabilities
    cbg_join <- merge(cbg_join, observed_probs, by = "Census_Block_Group", all.x = TRUE)
    
    ## Correlating the two
    p <- cor.test(cbg_join$Observed_Huff_Probability, cbg_stat$Predicted_Probability,
                  method = "pearson")
  })
  
}







## Function for extracting list of census tracts and total visits to each 
extract_catchments <- function(rc, ptns, state = "AL", geography = "Block") {
  
  ## Read in the patterns
  #ptns <- get_patterns(state)
  #ptns <- st_read("Output Data/Patterns/MT_Patterns_July2021.gpkg")
  
  ## Read in the retail centres
  #rc <- get_retail_centres(state)
  #rc <- st_read("Output Data/Retail Centres/MT_RC_Boundaries.gpkg")
  

  # 1. Cleaning -------------------------------------------------------------

  ## Strip out the corrupted features
  ptns_clean <- ptns %>%
    select(placekey, parent_placekey, raw_visit_counts, raw_visitor_counts,
           poi_cbg, visitor_home_cbgs, visitor_daytime_cbgs, visitor_home_aggregation) %>%
    mutate_all(na_if, "") %>%
    mutate(visitor_home_cbgs = gsub('"""', '', visitor_home_cbgs)) %>%
    mutate(visitor_daytime_cbgs = gsub('"""', '', visitor_daytime_cbgs)) %>%
    mutate(visitor_home_aggregation = gsub('"""', '', visitor_home_aggregation))
  
  ## Strip out the patterns associated w/ parents
  parents <- ptns_clean %>%
    as.data.frame() %>%
    select(-c(geometry, placekey)) %>%  
    filter(!is.na(parent_placekey))
  ptns_noparents <- ptns_clean %>%
    filter(!parent_placekey %in% parents$parent_placekey)
  

  # 2. Pulling out Census Tracts & Census Block Groups --------------------------------------------
  
  ## Using SafeGraphR to get Census Tracts
  poi_home_tracts <- expand_cat_json(ptns_noparents,
                                     expand = "visitor_home_aggregation",
                                     index = "visitor_home_tract",
                                     by = "placekey")
  parent_home_tracts <- expand_cat_json(parents,
                                        expand = "visitor_home_aggregation",
                                        index = "visitor_home_tract",
                                        by = "parent_placekey")
  parent_home_tracts <- parent_home_tracts %>% rename(placekey = parent_placekey)
  home_tracts <- rbind(poi_home_tracts, parent_home_tracts)
  # 
  ## Using SafeGraphR to get Census Block Groups
  # poi_home_cbgs <- expand_cat_json(ptns_noparents, 
  #                                  expand = "visitor_home_cbgs",
  #                                  index = "visitor_home_cbg",
  #                                  by = "placekey")
  # parent_home_cbgs <- expand_cat_json(parents, 
  #                                     expand = "visitor_home_cbgs",
  #                                     index = "visitor_home_cbg",
  #                                     by = "parent_placekey")
  # parent_home_cbgs <- parent_home_cbgs %>% rename(placekey = parent_placekey)
  # home_cbgs <- rbind(poi_home_cbgs, parent_home_cbgs)
  

  # 3. Patterns in Retail Centres -------------------------------------------
  
  ## CRS
  ptns <- st_set_crs(ptns, 4326)
  
  ## Identify
  p_rc <- st_intersection(ptns, rc)
  p_rc_tracts <- merge(p_rc, home_tracts, by = "placekey", all.x = TRUE)
  #p_rc_blocks <- merge(p_rc, home_cbgs, by = "placekey", all.x = TRUE)
  
  p_rc_tracts <- p_rc_tracts %>%
    rename(n.visits = visitor_home_aggregation.y) %>%
    drop_na("n.visits") %>%
    as.data.frame() %>%
    select(visitor_home_tract, rcID, n.visits) %>%
    rename(Census_Tract = visitor_home_tract) %>%
    distinct() %>%
    group_by(Census_Tract, rcID) %>%
    summarise(n.visits = sum(n.visits))
  # p_rc_blocks <- p_rc_blocks %>%
  #   rename(n.visits = visitor_home_cbgs.y) %>%
  #   drop_na("n.visits") %>%
  #   as.data.frame() %>%
  #   select(visitor_home_cbg, rcID, n.visits) %>%
  #   rename(Census_CBG = visitor_home_cbg) %>%
  #   distinct() %>%
  #   group_by(Census_CBG, rcID) %>%
  #   summarise(n.visits = sum(n.visits))

    
  
  # 4. Attaching to Tracts/Blocks --------------------------------------------------
  
  ## Tracts shapefile from Spielman & Singleton (2015)
  geodemo <- st_read("Input Data/Geodemographics/US_Geodemographic_Classification.gpkg")
  geodemo <- geodemo %>%
    select(GEOID10) %>%
    rename(Census_Tract = GEOID10)
  
  ## Blocks shapefile processed from Tiger
  blocks <- st_read("Input Data/Census Block Groups/US_Census_Block_Groups.gpkg")
  blocks <- blocks %>%
    select(CBG_ID) %>%
    rename(Census_CBG = CBG_ID)
  
  ## Attach count
  tracts <- merge(geodemo, p_rc_tracts, by = "Census_Tract", all.y = TRUE)
  tracts <- tracts[!st_is_empty(tracts),,drop = FALSE]
  tracts <- st_cast(tracts, "POLYGON")
  
  # blocks <- merge(blocks, p_rc_blocks, by = "Census_CBG", all.y = TRUE)
  # blocks <- blocks[!st_is_empty(blocks),,drop = FALSE]
  # blocks <- st_cast(blocks, "POLYGON")
  
  ## Clean out repeated rows 
  tracts_df <- tracts %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    mutate_if(is.character, as.factor) %>%
    distinct()
  # blocks_df <- blocks %>%
  #   as.data.frame() %>%
  #   select(-c(geometry)) %>%
  #   mutate_if(is.character, as.factor) %>%
  #   distinct()
  
  ## Re attach
  gd_tracts <- merge(geodemo, tracts_df, by = "Census_Tract", all.y = TRUE)
  # gd_blocks <- merge(blocks, blocks_df, by = "Census_CBG", all.y = TRUE)
  # gd_blocks <- gd_blocks %>% select(-c(rcID.y, n.visits.y)) %>% rename(rcID = rcID.x, n.visits = n.visits.x) %>% distinct()

  ## Total visits in each retail centre
  tract_grp_totals <- gd_tracts %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    group_by(rcID) %>%
    summarise(total.rc.visits = sum(n.visits))
  # block_grp_totals <- gd_blocks %>%
  #   as.data.frame() %>%
  #   select(-c(geometry)) %>%
  #   group_by(rcID) %>%
  #   summarise(total.rc.visits = sum(n.visits)) %>%
  #   mutate_if(is.character, as.factor)
  
  
  ## Merge on
  gd_tracts <- merge(gd_tracts, tract_grp_totals, by = "rcID", all.x = TRUE)
  #gd_blocks <- merge(gd_blocks, block_grp_totals, by = "rcID", all.x = TRUE)
  

  # 5. Refining Catchments --------------------------------------------------


  # 5.1 Primary Catchment Calculation ---------------------------------------
  
  ## Compute proportion visits occupied by each tract, sort and calculate cumulatives
  gd_tracts <- gd_tracts %>%
    mutate(prop_visits = (n.visits / total.rc.visits) * 100) %>%
    arrange(rcID, -prop_visits) %>%
    mutate_if(is.character, as.factor)
  # gd_blocks <- gd_blocks %>%
  #   mutate(prop_visits = (n.visits / total.rc.visits) * 100) %>%
  #   arrange(rcID, -prop_visits) %>%
  #   mutate_if(is.character, as.factor)
    
  # 5.2 Distance from Retail Centre ------------------------------------------
  
  
  ## Build up datasets for calculation
  t_cent <- st_centroid(gd_tracts)
  t_cent <- t_cent %>% select(Census_Tract)
  # b_cent <- st_transform(st_centroid(gd_blocks), 4326)
  # b_cent <- b_cent %>% select(Census_CBG)
  rc_cent <- st_centroid(rc)
  rc_cent <- rc_cent %>% select(rcID)
  
  ## Calculate & tidy distances
  
  ### Tracts first 
  m <- st_distance(rc_cent, t_cent)
  rownames(m) <- rc_cent$rcID
  colnames(m) <- t_cent$Census_Tract
  m <- set_units(m, "km")
  m <- as.data.frame(m)
  rownames(m) <- rc_cent$rcID
  colnames(m) <- t_cent$Census_Tract
  
  ### Blocks second
  # n <- st_distance(rc_cent, b_cent)
  # rownames(n) <- rc_cent$rcID
  # colnames(n) <- b_cent$Census_CBG
  # n <- set_units(n, "km")
  # n <- as.data.frame(n)
  # rownames(n) <- rc_cent$rcID
  # colnames(n) <- b_cent$Census_CBG
  
  ## Convert format - each row has rcID, census tract and distance in kms
  m_long <- m %>%
    rownames_to_column() %>%
    rename(rcID = rowname) %>%
    gather(Census_Tract, Distance, -rcID, factor_key = FALSE) %>%
    mutate(Distance = gsub("[km]", "", Distance)) %>%
    mutate(Distance = as.numeric(Distance)) %>%
    mutate(Census_Tract = gsub("\\..*", "", Census_Tract)) %>%
    distinct() %>%
    arrange(rcID, Distance)
  # n_long <- n %>%
  #   rownames_to_column() %>%
  #   rename(rcID = rowname) %>%
  #   gather(Census_CBG, Distance, -rcID, factor_key = FALSE) %>%
  #   mutate(Distance = gsub("[km]", "", Distance)) %>%
  #   mutate(Distance = as.numeric(Distance)) %>%
  #   mutate(Census_CBG = gsub("\\..*", "", Census_CBG)) %>%
  #   distinct() %>%
  #   arrange(rcID, Distance)
  
  ## Merge distances on
  gd_tracts <- merge(gd_tracts, m_long, by = c("rcID", "Census_Tract"), all.x = TRUE)
  #gd_blocks <- merge(gd_blocks, n_long, by = c("rcID", "Census_CBG"), all.x = TRUE)
  return(gd_tracts)
  
  # 3.3 Refinement Strategy -------------------------------------------------
  
  ## Tidy up, arrange by Distance (low-high) and Visits (high-low) before computing cumulative visits
  # out_tracts <- gd_tracts %>% 
  #   select(rcID, Census_Tract, Distance, prop_visits, n.visits, total.rc.visits) %>%
  #   rename(TractVisits = n.visits, TotalVisits = total.rc.visits, PropVisits = prop_visits) %>%
  #   arrange(rcID, desc(PropVisits), Distance) %>%
  #   group_by(rcID) %>%
  #   mutate(Catchment = cumsum(PropVisits)) %>%
  #   ungroup() %>%
  #   mutate_if(is.character, as.factor) %>%
  #   filter(Catchment <= 50)
  #out_tracts <- st_cast(out_tracts, "POLYGON")
  
  out_blocks <- gd_blocks %>%
    select(rcID, Census_CBG, Distance, prop_visits, n.visits, total.rc.visits) %>%
    rename(BlockVisits = n.visits, TotalVisits = total.rc.visits, PropVisits = prop_visits) %>%
    arrange(rcID, desc(PropVisits), Distance) %>%
    group_by(rcID) %>%
    mutate(Catchment = cumsum(PropVisits)) %>%
    ungroup() %>%
    mutate_if(is.character, as.factor) %>%
    filter(Catchment <= 50)
  out_blocks <- st_cast(out_blocks, "POLYGON")
  
  if (geography == "Tract") {
    return(out_tracts)
  } else if (geography == "Block") {
    return(out_blocks)
  }
  
}