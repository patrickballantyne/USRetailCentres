

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
      select(safegraph_place_id, street_address, city, region, postal_code) %>%
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
    
    
    # 3. Stage 3. Identifying the large centres (no street names) ----------------------
    
    ## Get a retail centre name, no street names for large centres
    by_size <- rc_clean %>%
      mutate(size = case_when(n.pts < 100 ~ "SMALL",
                              n.pts >= 100 ~ "LARGE")) %>%
      mutate(rcName_NEW = case_when(size == "LARGE" ~ paste0(city,","," ",county,","," ",state),
                                    size == "SMALL" ~ paste0(street_name,","," ",city,","," ",county,","," ",state))) %>%
      group_by(rcName_NEW) %>%
      add_count(rcName_NEW) %>%
      group_by(rcName_NEW, n) %>%
      mutate(uniqueID = sequence(n())) %>%
      ungroup() %>%
      mutate(rcName_test = case_when((size == "LARGE" &  n > 1) ~ paste0(city, " ", "(", uniqueID, ")",",", " ", county, ",", state),
                                     (size == "SMALL" & n > 1) ~ paste0(street_name, " ", "(", uniqueID, ")", ",", " ", city, ",", " ", county, ",", " ", state),
                                     (size == "LARGE" & n < 2) ~ paste0(city,",", " ",county, ",", state),
                                     (size == "SMALL" & n < 2) ~ paste0(street_name,",", " ", city, ",", " ", county, ",", " ", state))) %>%
      select(-c(rcName, rcName_NEW)) %>%
      rename(rcID_full = rcID, rcName = rcName_test, street = street_name, place = city) %>%
      mutate(rcID = substr(rcID_full, 10, 12:13)) %>%
      mutate(rcID = gsub("_", "", rcID)) %>%
      select(rcID_full, rcID, rcName, n.pts, street, place, county, state) %>%
      mutate(rcID = as.integer(rcID)) %>%
      arrange(rcID)
    return(by_size)
    

    # 4. Write Out ------------------------------------------------------------
    st_write(by_size, paste0("Output Data/Retail Centres/Named/", identifier, "_RC.gpkg"), append = FALSE)
    
  }
}







  
  
}
