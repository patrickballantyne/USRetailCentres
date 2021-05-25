
get_names <- function(identifier = "AL") {
  
  ## Read in data
  rc <- st_read(paste0("Output Data/Retail Centres/", identifier, "_RC_Boundaries.gpkg"))
  pts <- read_points(identifier)
  places <- tigris::places(state = identifier)
  
  ## Clean
  rc_clean <- rc %>%
    dplyr::select(rcID)
  places <- places %>%
    select(NAMELSAD) %>%
    rename(place_name = NAMELSAD) %>%
    mutate(place_name = gsub("CDP", '', place_name)) %>%
    mutate(place_name = gsub("city", 'City', place_name)) %>%
    mutate(place_name = gsub("town", 'Town', place_name)) %>%
    st_transform(4326)
  pts_clean <- pts %>% 
    select(safegraph_place_id, street_address, city, region, postal_code) %>%
    mutate(street_address_clean = sub(".*? ", "", street_address)) %>%
    select(-c(street_address))
  
  ## Get pts in rc (for addresses)
  m <- st_intersection(pts_clean, rc)
  
  ## Identify the street with the most pts for each rc
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
  
  ## Merge on place names
  pa <- st_intersection(rc_clean, places)
  pa <- pa %>%
    as.data.frame() %>%
    group_by(rcID) %>%
    summarise(place = paste0(place_name, collapse = " / "), street = street_name) %>%
    distinct(place)
  rc_clean <- merge(rc_clean, pa, by = "rcID")
  
  ## Merge on county and state
  rc_m <- rc %>%
    as.data.frame() %>%
    select(rcID, State, County, n.pts) %>%
    rename(state = State, county = County)
  rc_clean <- merge(rc_clean, rc_m, by = "rcID")
  
  ## Pull out largest centre in each place
  t <- rc_clean %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    group_by(place) %>%
    arrange(desc(n.pts)) %>%
    top_n(n = 1) 
  
  large <- rc_clean %>%
    filter(rcID %in% t$rcID) %>%
    mutate(largest_in_place = "TRUE")
  small <- rc_clean %>%
    filter(!rcID %in% t$rcID) %>%
    mutate(largest_in_place = "FALSE")
  
  ## Join together and create rcName
  rc_out <- rbind(large, small)
  
  ## Create name depending on whether the centre is largest in place or not
  rc_out <- rc_out %>%
    mutate(rcName = case_when(largest_in_place == "TRUE" ~ paste0(place,",",county,",",state),
                              largest_in_place == "FALSE" ~ paste0(street_name,",",place,",",county,",",state)))
  
  ## Pull out unique ID 
  rc_out <- rc_out %>%
    rename(rcID_full = rcID) %>%
    mutate(rcID = substr(rcID_full, 10, 12:13)) %>%
    mutate(rcID = gsub("_", "", rcID)) %>%
    rename(street = street_name) %>%
    select(rcID_full, rcID, rcName, n.pts, street, place, county, state) %>%
    mutate(rcID = as.integer(rcID)) %>%
    arrange(rcID)
  
  ## Create final retail centre name
  # rc_clean$rcName <- as.factor(with(rc_clean, paste("(",rcID,")", street, ",", place, ",", county, ",", state)))
  # rc_clean <- rc_clean %>%
  #   rename(identifier = rcID_full) %>%
  #   select(identifier, rcID, rcName, n.pts, street, place, county, state)
  st_write(rc_out, paste0("Output Data/Retail Centres/Named/", identifier, "_RC.gpkg"), append = FALSE)
  
}
