## Function for pulling in the patterns for a state
get_patterns <- function(state = "AL", duckdb = mydb, week = "july2021") {
  
  ## Read in the patterns from the duck database
  ptns <- dbGetQuery(mydb, paste0("SELECT * FROM", " ", week, " ", " WHERE region = '", state, "'"))
  print("PATTERNS EXTRACTED")
  
  ## Read in the points to match 
  pts_query <- paste0("select* from SafeGraph_Cleaned_Places_US where region = '", state, "'") 
  pts <- st_read("Output Data/SafeGraph_Cleaned_Places_US.gpkg", query = pts_query)
  pts <- pts %>% select(placekey)
  print("POINTS EXTRACTED")
  
  ## Merge
  ptns_sf <- merge(pts, ptns, by = "placekey", all.x = TRUE)
  print(paste0(week, "PATTERNS CLEANED"))
  return(ptns_sf)
}

## Function for extracting list of census tracts and total visits to each 
extract_catchments <- function(state = "AL") {
  
  ## Read in the patterns
  #ptns <- get_patterns(state)
  ptns <- st_read("Output Data/Patterns/MT_Patterns_July2021.gpkg")
  
  ## Read in the retail centres
  #rc <- get_retail_centres(state)
  rc <- st_read("Output Data/Retail Centres/MT_RC_Boundaries.gpkg")
  

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
    select(-c(geom, placekey)) %>%  
    filter(!is.na(parent_placekey))
  ptns_noparents <- ptns_clean %>%
    filter(!parent_placekey %in% parents$parent_placekey)
  

  # 2. Pulling out Census Tracts --------------------------------------------
  
  ## Using SafeGraphR to expand the json columns
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
  
  

  # 3. Patterns in Retail Centres -------------------------------------------
  
  ## Identify
  p_rc <- st_intersection(ptns, rc)
  p_rc <- merge(p_rc, home_tracts, by = "placekey", all.x = TRUE)
  p_rc <- p_rc %>%
    rename(n.visits = visitor_home_aggregation.y) %>%
    drop_na("n.visits") %>%
    as.data.frame() %>%
    select(visitor_home_tract, rcID, n.visits) %>%
    rename(Census_Tract = visitor_home_tract) %>%
    distinct() %>%
    group_by(Census_Tract, rcID) %>%
    summarise(n.visits = sum(n.visits))

    
  
  # 4. Attaching to Tracts --------------------------------------------------
  
  ## Tracts shapefile from Spielman & Singleton (2015)
  geodemo <- st_read("Input Data/Geodemographics/US_Geodemographic_Classification.gpkg")
  geodemo <- geodemo %>%
    select(GEOID10) %>%
    rename(Census_Tract = GEOID10)
  
  ## Attach count
  tracts <- merge(geodemo, p_rc, by = "Census_Tract", all.y = TRUE)
  tracts <- tracts[!st_is_empty(tracts),,drop = FALSE]
  tracts <- st_cast(tracts, "POLYGON")
  
  ## Clean out repeated rows 
  tracts_df <- tracts %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    mutate_if(is.character, as.factor) %>%
    distinct()
  
  ## Re attach
  gd_tracts <- merge(geodemo, tracts_df, by = "Census_Tract", all.y = TRUE)

  ## Total visits in each retail centre
  grp_totals <- gd_tracts %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    group_by(rcID) %>%
    summarise(total.rc.visits = sum(n.visits))
  
  ## Merge on
  gd_tracts <- merge(gd_tracts, grp_totals, by = "rcID", all.x = TRUE)
  

  # 5. Refining Catchments --------------------------------------------------


  # 5.1 Primary Catchment Calculation ---------------------------------------
  
  ## Compute proportion visits occupied by each tract, sort and calculate cumulatives
  gd_tracts <- gd_tracts %>%
    mutate(prop_visits = (n.visits / total.rc.visits) * 100) %>%
    arrange(rcID, -prop_visits) %>%
    mutate_if(is.character, as.factor)
  

  # 5.2 Distance from Retail Centre ------------------------------------------
  
  
  ## Build up datasets for calculation
  t_cent <- st_centroid(gd_tracts)
  t_cent <- t_cent %>% select(Census_Tract)
  rc_cent <- st_centroid(rc)
  rc_cent <- rc_cent %>% select(rcID)
  
  ## Calculate & tidy distances
  m <- st_distance(rc_cent, t_cent)
  rownames(m) <- rc_cent$rcID
  colnames(m) <- t_cent$Census_Tract
  m <- set_units(m, "km")
  m <- as.data.frame(m)
  rownames(m) <- rc_cent$rcID
  colnames(m) <- t_cent$Census_Tract
  
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
  
  ## Merge distances on
  gd_tracts <- merge(gd_tracts, m_long, by = c("rcID", "Census_Tract"), all.x = TRUE)
  
  # 3.3 Refinement Strategy -------------------------------------------------
  
  ## Tidy up, arrange by Distance (low-high) and Visits (high-low) before computing cumulative visits
  out_tracts <- gd_tracts %>% 
    select(rcID, Census_Tract, Distance, prop_visits, n.visits, total.rc.visits) %>%
    rename(TractVisits = n.visits, TotalVisits = total.rc.visits, PropVisits = prop_visits) %>%
    arrange(rcID, desc(PropVisits), Distance) %>%
    group_by(rcID) %>%
    mutate(Catchment = cumsum(PropVisits)) %>%
    ungroup() %>%
    mutate_if(is.character, as.factor) %>%
    filter(Catchment <= 50)
  out_tracts <- st_cast(out_tracts, "POLYGON")
  return(out_tracts)
  
  
  
  
  
}