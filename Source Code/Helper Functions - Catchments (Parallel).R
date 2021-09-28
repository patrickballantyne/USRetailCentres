
## Function for reading in the patterns
get_patterns <- function(iteration = "May2021") {
  
  ## Read in
  ptn <- vroom(paste0("output_data/Catchments/", iteration, ".tsv"))
  ptn <- ptn %>%
    select(placekey, region, raw_visit_counts, visitor_home_cbgs, visitor_home_aggregation) %>%
    rename(state = region)
  return(ptn)
}

## Get retail centres
get_rc <- function(state = "AL") {
  
  rc_query <- paste0("select* from US_RC_minPts50 where State = '", state, "'") 
  rc <- st_read("output_data/US_RC_minPts50.gpkg", query = rc_query)
  rc <- rc %>%
    st_transform(4326)
  return(rc)
}

## Function that reads in SafeGraph retail points for a state of interest
get_pts <- function(state = "AL") {
  
  ## Read in the points for the selected state
  query <- paste0("select* from SafeGraph_Cleaned_Places_US where region = '", state, "'")
  pts <- st_read("output_data/SafeGraph_Cleaned_Places_US.gpkg", query = query)
  return(pts)
}

## Get the intersections
calcObserved <- function(x) {
  
  ## Get points in retail centre
  int <- st_join(x, pts)
  int_clean <- int %>%
    as.data.frame() %>%
    select(placekey, rcID) %>%
    filter(!is.na(rcID))
  
  ## Attach patterns
  int_patterns <- merge(july2021, int_clean, by = "placekey", all.y = TRUE)
  int_patterns <- int_patterns %>%
    select(rcID, placekey, raw_visit_counts, visitor_home_aggregation) %>%
    filter(visitor_home_aggregation != "{}") %>%
    filter(visitor_home_aggregation != "")
  
  ## Expand patterns to census tracts
  ptn_out <- expand_cat_json(int_patterns,
                             expand = "visitor_home_aggregation",
                             index = "CensusTract",
                             by = c("rcID", "placekey", "raw_visit_counts"))
  
  ## Compute total visits for census tracts by retail centre
  out_df <- ptn_out %>%
    select(rcID, CensusTract, visitor_home_aggregation) %>%
    group_by(rcID, CensusTract) %>%
    summarise(totalVisitors = sum(visitor_home_aggregation)) %>%
    ungroup()
  
  ## Tidy up
  tract_merge <- merge(w_tracts_df, out_df, by = "CensusTract", all.x = TRUE)
  tract_merge <- tract_merge %>%
    mutate(totalVisitors = replace_na(totalVisitors, 0)) %>%
    select(-c(rcID))
  tract_merge$rcID <- x$rcID
  tract_merge <- tract_merge %>%
    mutate_if(is.character, as.factor)
  return(tract_merge)
}

## Function for cleaning output of calcObserved()
clean_ca <- function(out) {
  
  ## unlist
  out_df <- rbindlist(out)
  
  ## write out
  vroom_write(out_df, "output_data/Catchments/Observed Patronage/CA.tsv", append = TRUE)
  rm(out)
  gc()
  
}


## Function that calculates observed patronage for each state's worth of retail 
getObserved <- function(state = "AL") {
  
  ## Pull in retail centres for calibration zone
  rc <- get_rc(state)
  if (state == "CA") {
    
    rc <- rc %>%
      filter(rcID != "06_059_RC_982") %>%
      filter(rcID != "06_085_RC_1553")
    rc_ls <- split(rc, seq(nrow(rc)))
  } else {
    rc_ls <- split(rc, seq(nrow(rc)))
  }
  
  ## Pull in the points
  pts <- get_pts(state)
  pts <- pts %>%
    select(placekey) %>%
    st_set_crs(4326)
  
  ## Pull in the patterns
  july2021 <- get_patterns("July2021")
  july2021 <- july2021 %>%
    filter(state == state)
  
  ## Pull in the census block groups
  w_tracts <- st_read("output_data/Catchments/West_Tracts.gpkg")
  w_tracts_df <- w_tracts %>%
    as.data.frame() %>%
    select(CensusTract)
  
  ## in function
  calcObserved <- function(x) {
    
    ## Get points in retail centre
    int <- st_join(x, pts)
    int_clean <- int %>%
      as.data.frame() %>%
      select(placekey, rcID) %>%
      filter(!is.na(rcID))
    
    ## Attach patterns
    int_patterns <- merge(july2021, int_clean, by = "placekey", all.y = TRUE)
    int_patterns <- int_patterns %>%
      select(rcID, placekey, raw_visit_counts, visitor_home_aggregation) %>%
      filter(visitor_home_aggregation != "{}") %>%
      filter(visitor_home_aggregation != "")
    
    ## Expand patterns to census tracts
    ptn_out <- expand_cat_json(int_patterns,
                               expand = "visitor_home_aggregation",
                               index = "CensusTract",
                               by = c("rcID", "placekey", "raw_visit_counts"))
    
    ## Compute total visits for census tracts by retail centre
    out_df <- ptn_out %>%
      select(rcID, CensusTract, visitor_home_aggregation) %>%
      group_by(rcID, CensusTract) %>%
      summarise(totalVisitors = sum(visitor_home_aggregation)) %>%
      ungroup()
    
    ## Tidy up
    tract_merge <- merge(w_tracts_df, out_df, by = "CensusTract", all.x = TRUE)
    tract_merge <- tract_merge %>%
      mutate(totalVisitors = replace_na(totalVisitors, 0)) %>%
      select(-c(rcID))
    tract_merge$rcID <- x$rcID
    tract_merge <- tract_merge %>%
      mutate_if(is.character, as.factor)
    return(tract_merge)
  }
  
  ## Format out of lapply
  out <- mclapply(rc_ls, calcObserved, mc.cores = 16)
  out_out <- data.table::rbindlist(out, use.names = TRUE)
  
  ## Read in the census tract visitors for western region
  w_visitors <- st_read("output_data/Catchments/W_Tract_Visitors.gpkg")
  w_visitors <- w_visitors %>%
    as.data.frame() %>%
    select(CensusTract, totalVisitors) %>%
    rename(totalTractVisitors = totalVisitors)
  
  ## Merge on and compute proportions
  out_merge <- merge(out_out, w_visitors, by = "CensusTract", all.x = TRUE)
  out_merge$state <- state
  out_merge <- out_merge %>%
    as.data.frame() %>%
    mutate(propVisitors = (totalVisitors / totalTractVisitors) * 100) %>%
    mutate(propVisitors = replace_na(propVisitors, 0)) %>%
    select(CensusTract, rcID, state, totalVisitors, totalTractVisitors, propVisitors)
  vroom_write(out_merge, paste0("output_data/Catchments/Observed Patronage/", state, "_ObservedPatronage.tsv"), append = FALSE)
  print(paste0(state, " ", "Observed Patronage Extracted"))
}

## Function for computing network distances
getNetwork <- function(rc, tracts) {
  
  ## Create list of tracts to loop through
  tracts_ls <- split(tracts, seq(nrow(tracts)))
  
  ## Compute distances
  dist <- lapply(tracts_ls, function(x)  {
    
    dist <- route_matrix(rc, x, routing_mode = "fast", transport_mode = "car")
    dist$rcID <- rc$rcID
    dist$CensusTract <- x$CensusTract
    dist
  })
  
  dist_out <- do.call(rbind, dist)
  dist_out <- dist_out %>%
    as.data.frame(rcID, CensusTract, distance) %>%
    rename(Distance = distance) %>%
    mutate(Distance = Distance/ 1000)
  
}

## Function for getting euclidean distances
getEuclidean <- function(tracts, rc) {
  
  tracts <- tracts %>% st_transform(4269)
  rc <- rc %>% st_transform(4269)
  
  ## Split
  rc_ls <- split(rc, seq(nrow(rc)))
  
  dist <- mclapply(rc_ls, function(i) {
    
    cols <- as.vector(i$rcID)
    rows <- as.vector(tracts$CensusTract)
    
    dist_df <- as.data.frame(st_distance(tracts, i))
    colnames(dist_df) <- cols
    dist_df$CensusTract <- rows
    dist_df <- dist_df %>%
      gather(rcID, Distance, -CensusTract) %>%
      mutate(Distance = Distance / 1000) %>%
      mutate(Distance = gsub("[km]", "", Distance)) %>%
      mutate(Distance = as.numeric(Distance)) 
    
    rc_attr <- rc[, c("rcID", "attractivenessScore")]
    dist_df <- merge(dist_df, rc_attr, by = "rcID", all.x = TRUE)
    dist_df <- dist_df %>%
      as.data.frame() %>%
      select(rcID, CensusTract, Distance, attractivenessScore) %>%
      rename(Attractiveness = attractivenessScore)
    dist_df
  }, mc.cores = 16)
  
}

## Huff Model Specification - as input we need to have the retail centre ID, and then the different parameters we want to use
## in the model - attractiveness scores, distances and then alpha & beta values
getHuff <- function(huff_inputs, alpha = 1, beta = 2) {
  
  ## Pull out features we want
  cl <- huff_inputs %>%
    select(rcID, CensusTract, Distance, Attractiveness) %>%
    mutate(Distance = ifelse(Distance == 0, 0.01, Distance))
  
  ## Numerator 
  numerator <- cl %>%
    mutate(numerator = (Attractiveness ^ alpha) / (Distance ^ beta))
  
  ## Denominator
  denominator <- numerator %>%
    group_by(CensusTract) %>%
    summarise(denominator = sum(numerator))
  
  ## Calculate Huff Probability 
  huff_probs <- merge(numerator, denominator, by = "CensusTract", all.x = TRUE)
  huff_probs$huff_probability <- huff_probs$numerator / huff_probs$denominator
  huff_probs$alpha <- alpha
  huff_probs$beta <- beta
  huff_probs <- huff_probs %>%
    select(CensusTract, rcID, huff_probability) %>%
    arrange(rcID)
  return(huff_probs)  
  
}


ne <- c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT")
mw <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
s <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV",
       "AL", "KY", "MS", "TN",
       "AR", "LA", "OK", "TX")
w <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
       "AK", "HI", "OR", "WA", "CA")
