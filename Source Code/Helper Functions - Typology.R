## Helper Functions (2) - Retail Centre Typology


## Function that reads in SafeGraph retail points for a state of interest
read_points <- function(state = "AL") {
  
  ## Read in the points for the selected state
  query <- paste0("select* from SafeGraph_Retail_Places_US where region = '", state, "'")
  pts <- st_read("Output Data/SafeGraph_Retail_Places_US.gpkg", query = query)
  pts <- st_transform(pts, 4326)
  return(pts)
}

## Function that extracts all the variables needed to run the typology for the retail centres
prep4typology <- function(state) {
  
  ## Read in the datasets we need for this
  boundaries <- st_read(paste0("Output Data/Retail Centres/", state, "_RC_Boundaries.gpkg"))
  hexes <- st_read(paste0("Output Data/Retail Centres/", state, "_RC_Hexes.gpkg"))
  pts <- read_points(state = state)
  
  
  ## 1. n.units and n.hexes ########################
  boundaries <- boundaries %>%
    rename(n.units = n.pts)
  hex_count <- st_intersection(boundaries, hexes)
  rc_grouped <- hex_count %>%
    as.data.frame() %>%
    group_by(rcID, tractID, n.units) %>%
    summarise(n.hexes = n())
  
  ## 2. proportions of comparison, convenience, leisure and service retail ##########################
  pt_count <- st_intersection(boundaries, pts)
  cat_props<- pt_count %>%
    group_by(rcID, tractID) %>%
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
  rc_grouped <- merge(rc_grouped, cat_props, by = c("rcID", "tractID"), all.x = TRUE)
  
  ## 3. diversity ########################
  distinct_cats <- pt_count %>%
    as.data.frame() %>%
    group_by(rcID, tractID) %>%
    dplyr::summarise(top_category_total = n_distinct(top_category),
                     sub_category_total = n_distinct(sub_category))
  ### we know that there are 176 distinct top categories in the US, and 357 distinct sub categories
  distinct_cat <- distinct_cats %>%
    mutate(top_category_diversity = (top_category_total / 176)*100, 
           sub_category_diversity = (sub_category_total / 357)*100)
  
  
  ## 4. chains vs independents #####################
  chain_or_ind <- data.table::fread("Output Data/SafeGraph_Places_US_Updated_Chains.csv")
  pt_count <- merge(pt_count, chain_or_ind, by = c("safegraph_place_id", "location_name"), all.x = TRUE)
  diversity <- pt_count %>%
    group_by(rcID, tractID) %>%
    dplyr::count(chain_or_independent) %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    spread(chain_or_independent, n) %>%
    mutate(Chain = replace(Chain, is.na(Chain), 0)) %>%
    mutate(Independent = replace(Independent, is.na(Independent), 0)) %>%
    mutate(total = Chain + Independent) %>%
    mutate(pct_Independent = (Independent / total) * 100) %>%
    mutate(pct_Chain = (Chain / total) * 100) %>%
    select(c(rcID, tractID, pct_Independent, pct_Chain))
  rc_grouped <- merge(rc_grouped, diversity, by = c("rcID", "tractID"), all.x = TRUE)
  
  
  ## 5. centre compactness ###################
  boundaries$area <- st_area(boundaries)
  boundaries_area <- boundaries %>%
    select(rcID, tractID, area)
  circ <- lwgeom::st_minimum_bounding_circle(boundaries_area)
  circ$area <- st_area(circ)
  circ <- circ %>%
    as.data.frame() %>%
    select(rcID, tractID, area)
  roeck <- merge(boundaries, circ, by = c("rcID", "tractID"), all.x = TRUE)
  roeck$roeck <- as.numeric(roeck$area.x / roeck$area.y)
  roeck$roeck <- scales::rescale(roeck$roeck)
  roeck <- roeck %>%
    as.data.frame() %>%
    select(rcID, tractID, roeck)
  rc_grouped <- merge(rc_grouped, roeck, by = c("rcID", "tractID"), all.x = TRUE)
  
  ## 6. geodemographics ######################
  
  
  ## 7. return ######################
  return(rc_grouped)
  
}


m <- prep4typology("DC")
