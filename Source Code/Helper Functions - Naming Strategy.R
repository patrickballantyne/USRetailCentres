

## Geocode the names
get_geocoded_names <- function(identifier = "AL") {

  
  ## Retail Centres
  rc <- st_read(paste0("Output Data/Retail Centres/", identifier, "_RC_Boundaries.gpkg"))
  
  # 1. Stage 1. Geocoding - getting city, county and state ----------

  ## Get centroids
  cent <- st_centroid(rc)
  ## Reverse geocode
  g <- reverse_geocode(cent, 1, FALSE)
  
  if(is.null(g) == TRUE) {
    print("API Key Expired")
  } else {
    
    ## Clean
    clean_rc <- rc %>% select(rcID, rcName, n.pts, County, State)
    rc_clean <- cbind(clean_rc, g)
    rc_clean <- rc_clean %>%
      select(-c(id, rank, street, address, type, house_number, postal_code, district, county, state, country,
                distance, lng_access, lat_access, lng_position, lat_position))  %>%
      rename(county = County, state = State)
    
    # 2. Getting street names for centres (based on most the street mo --------
    
    ## Read in points and intersect w/ retail centres
    pts <- read_points(identifier)
    pts_clean <- pts %>% 
      select(safegraph_place_id, street_address, location_name, sub_category, city, region, postal_code) %>%
      mutate(street_address_clean = sub(".*? ", "", street_address)) %>%
      select(-c(street_address))
    m <- st_intersection(pts_clean, rc)
  
    ## Identify the 'major' street of each retail centre
    streets <- m %>% 
      as.data.frame() %>%
      select(street_address_clean, rcID) %>%
      group_by(rcID) %>%
      count(street_address_clean) %>%
      top_n(n = 1) %>%
      group_by(rcID) %>%
      summarise(street_address_clean = paste0(street_address_clean, collapse = " / "), n = n, .groups = "keep") %>%
      distinct(street_address_clean) %>%
      rename(street_name = street_address_clean)
    rc_clean <- merge(rc_clean, streets, by = "rcID")
    

    # 3. Stage 3. Identifying the Malls -----------------------------------------------------------------------
    
    ## Extract malls for the AoI
    malls <- st_read("Output Data/US_Malls.gpkg")
    malls <- malls %>% 
      filter(region == identifier) %>%
      select(location_name, sub_category) %>%
      st_transform(4326)
    
    ## Identify the rc's that contain a mall
    rc_malls <- st_intersection(malls, rc)
    rc_malls <- rc_malls %>%
      as.data.frame() %>%
      select(rcID, location_name)
    rc_clean <- merge(rc_clean, rc_malls, by = "rcID", all.x = TRUE)
    
    ## Create identifier as to whether a centre is a mall or not
    rc_clean <- rc_clean %>%
      mutate(mall_identifier = case_when(is.na(location_name) ~ "NO MALL",
                                         !is.na(location_name) ~ "MALL"))
    

    # 4. Identifying the Major Centres in each place --------------------------
    
    ## 
    places <- rc_clean %>%
      as.data.frame() %>%
      select(rcID, city, n.pts, mall_identifier)
    
    ## Drop out the malls
    ms <- places %>% 
      filter(mall_identifier == "MALL")
    ns <- places %>% 
      filter(mall_identifier == "NO MALL")
    
    ## Create identifier if largest centre in city
    tops <- ns %>%
      group_by(city) %>%
      top_n(1, wt = n.pts) %>%
      mutate(hier_identifier = "LARGEST") %>%
      select(rcID, hier_identifier)
    rc_clean <- merge(rc_clean, tops, by = "rcID", all.x = TRUE)
    
    # 4. Stage 5. Creating Names  ----------------------
    
    ## Get a retail centre name, no street names for large centres
    by_size <- rc_clean %>%
      # mutate(size = case_when(n.pts < 100 ~ "SMALL",
      #                         n.pts >= 100 ~ "LARGE")) %>%
      # mutate(rcName_NEW = case_when(size == "LARGE" ~ paste0(city,","," ",county,","," ",state),
      #                               size == "SMALL" ~ paste0(street_name,","," ",city,","," ",county,","," ",state))) %>%
      rename(rcID_full = rcID, city = city.x) %>%
      select(-c(city.y)) %>%
      mutate(rcID = substr(rcID_full, 10, 12:13)) %>%
      mutate(rcID = gsub("_", "", rcID)) %>%
      # group_by(rcName_NEW) %>%
      # add_count(rcName_NEW) %>%
      # group_by(rcName_NEW, n) %>%
      # mutate(uniqueID = sequence(n())) %>%
      # ungroup() %>%
      mutate(rcName_test = case_when(mall_identifier == "MALL" ~ paste0(location_name, ",", " ", city, ",", " ", state),
                                     (hier_identifier == "LARGEST" & mall_identifier == "NO MALL") ~ paste0(city,",", " ", state),
                                     (is.na(hier_identifier) & mall_identifier == "NO MALL") ~ paste0(street_name, ",", " ", city, ",", " ", state))) %>%
                                     # (size == "LARGE" & n < 2 & mall_identifier == "NO MALL") ~ paste0(street_name, ",", " ", city, ",", " ", state),
                                     # (size == "SMALL" & n < 2 & mall_identifier == "NO MALL") ~ paste0(street_name, ",", " ", city, ",", " ", state))) 
      select(-c(rcName, mall_identifier, hier_identifier)) %>%
      rename(rcName = rcName_test, street = street_name, place = city) %>%
      select(rcID_full, rcID, rcName, n.pts, street, place, county, state) %>%
      mutate(rcID = as.integer(rcID)) %>%
      arrange(rcID)
      
    

    # 4. Write Out ------------------------------------------------------------
    st_write(by_size, paste0("Output Data/Retail Centres/Named/", identifier, "_RC.gpkg"), append = FALSE)
    
  }
}

