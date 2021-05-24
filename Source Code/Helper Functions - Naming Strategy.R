
get_names <- function(identifier = "AL") {
  
  ## Read in data
  rc <- st_read(paste0("Output Data/Retail Centres/", identifier, "_RC_Boundaries.gpkg"))
  pts <- read_points(identifier)
  places <- tigris::places(state = identifier)
  
  ## Clean
  rc_clean <- rc %>%
    select(rcID)
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
    summarise(street_address_clean = paste0(street_address_clean, collapse = " / "), n = n) %>%
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
  
  ## Pull out unique ID 
  rc_clean <- rc_clean %>%
    rename(rcID_full = rcID) %>%
    mutate(rcID = substr(rcID_full, 8, 12:13)) %>%
    mutate(rcID = gsub("_", "", rcID)) %>%
    rename(street = street_name) %>%
    select(rcID_full, rcID, n.pts, street, place, county, state) 
  
  ## Create final retail centre name
  rc_clean$rcName <- as.factor(with(rc_clean, paste("(",rcID,")", street, ",", place, ",", county, ",", state)))
  rc_clean <- rc_clean %>%
    rename(identifier = rcID_full) %>%
    select(identifier, rcID, rcName, n.pts, street, place, county, state)
  st_write(rc_clean, paste0("Output Data/Retail Centres/Named/", identifier, "_RC.gpkg"))
  
}
