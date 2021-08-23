## Helper Functions (2) - Retail Centre Typology


# ## Function that reads in SafeGraph retail points for a state of interest
# read_points <- function(state = "AL") {
#   
#   ## Read in the points for the selected state
#   query <- paste0("select* from SafeGraph_Retail_Places_US where region = '", state, "'")
#   pts <- st_read("Output Data/SafeGraph_Retail_Places_US.gpkg", query = query)
#   pts <- st_transform(pts, 4326)
#   return(pts)
# }
# 
# ## Function that reads in the points for NE 
# read_region_points <- function(region = "NE") {
#   
#   ## Lapply function on states within the region 
#   ls <- c("CT", "DC", "DE", "MA", "MD", "ME", "NH", "NJ", "NY", "PA", "RI", "VT")
#   ls_pts <- lapply(ls, read_points)
#   ls_pts <- do.call(rbind, ls_pts)
#   return(ls_pts)
#   
# }

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


## Region State Lists
ne <- c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT")
mw <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
s <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV",
       "AL", "KY", "MS", "TN",
       "AR", "LA", "OK", "TX")
w <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
       "AK", "CA", "HI", "OR", "WA")


## Clean the retail centres - extract those w/ over 100 pts only
subset_centres <- function(min_pts = 100) {
  
  ## Read them in 
  ls <- list.files(paste0("Output Data/Retail Centres/Named"), pattern = paste0("_RC_Named.gpkg$"), full.names = TRUE)
  centres <- lapply(ls, st_read)
  centres <- do.call(rbind, centres)
  
  ## Clean columns
  centres <- centres %>%
    select(rcID_full, rcName, street, place, county, state, n.pts) %>%
    setNames(c("rcID", "rcName", "Street", "Place", "County", "State", "N.pts", "geom")) %>%
    dplyr::filter(N.pts >= min_pts)
  
  ## Write out 
  st_write(centres, paste0("Output Data/Retail Centres/US Retail Centres/US_RC", "_minPts", min_pts, ".gpkg"))
  print("RETAIL CENTRES CLEANED")
}


## Function that extracts all the variables needed to run the typology for the retail centres
prep4typology <- function(state, patterns) {
  
  ## Read in the datasets we need for this
  boundaries <- get_rc(state)
  pts <- get_pts(state = state)
  bdgs <- st_read(paste0("Output Data/Buildings/", state, "_Retail_Buildings.gpkg"))
  
  ## Merge on the new aggregations
  ldc <- data.table::fread("Output Data/SafeGraph_Places_Categories_LDC.csv")
  pts <- merge(pts, ldc, by = c("top_category", "sub_category"))

  ## Merge
  rc_grouped <- boundaries %>%
    as.data.frame() %>%
    select(rcID, rcName, State, N.pts) %>%
    rename(n.units = N.pts)
  
  ## 1. n.features & area ########################

  ## Count number of Buildings
  bdg_count <- boundaries %>%
    st_join(bdgs) %>%
    as.data.frame() %>%
    select(rcID) %>%
    group_by(rcID) %>%
    dplyr::summarise(n.bdgs = n())
  rc_grouped <- merge(rc_grouped, bdg_count, by = "rcID", all.x = TRUE)
  
  ## Calculate Area
  boundaries$area <- st_area(boundaries)
  
  ## Calculate Retail Building Density
  densities <- merge(boundaries, bdg_count, by = "rcID", all.x = TRUE)
  densities <- densities %>%
    as.data.frame() %>%
    select(rcID, n.bdgs, area) %>%
    mutate(bdg_density = n.bdgs/area) %>%
    select(-c(n.bdgs)) %>%
    mutate_at(vars(area, bdg_density), as.numeric)
  rc_grouped <- merge(rc_grouped, densities, by = "rcID", all.x = TRUE)
  
  ## 2. Proportions different types of retail ##########################
  
  ### Intersect
  pts <- st_set_crs(pts, 4326)
  boundaries <- st_transform(boundaries, 4326)
  pt_count <- st_intersection(boundaries, pts)
  
  
  ### First, the comparison categories
  comp_types <- pt_count %>%
    as.data.frame() %>%
    select(-c(geom)) %>%
    select(rcID, ldc_aggregation, typology_aggregation) %>%
    filter(ldc_aggregation == "COMPARISON")  %>%
    group_by(rcID) %>%
    count(typology_aggregation) %>%
    spread(typology_aggregation, n)
  comp_types <- merge(comp_types, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  comp_types <- comp_types %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(propClothingandFootwear = (ClothingandFootwear / n.units) * 100,
           propDIYandHousehold = (DIYandHousehold / n.units) * 100,
           propElectrical = (Electrical / n.units) * 100,
           propRecreational = (Recreational / n.units) * 100,
           propOtherComparison = (OtherComparison / n.units) * 100) %>%
    select(rcID, propClothingandFootwear, propDIYandHousehold, propElectrical,
           propRecreational, propOtherComparison)
  rc_grouped <- merge(rc_grouped, comp_types, by = "rcID", all.x = TRUE)
  
  ## Second the comparison categories
  conv_types <- pt_count %>%
    as.data.frame() %>%
    select(-c(geom)) %>%
    select(rcID, ldc_aggregation, typology_aggregation) %>%
    filter(ldc_aggregation == "CONVENIENCE")  %>%
    group_by(rcID) %>%
    count(typology_aggregation) %>%
    spread(typology_aggregation, n)
  conv_types <- merge(conv_types, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  conv_types <- conv_types %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(propChemist = (Chemist / n.units) * 100,
           propCTNandGasoline = (CTNandGasoline / n.units) * 100,
           propFood = (Food / n.units) * 100,
           propOffLicence = (OffLicence / n.units) * 100,
           propOtherConvenience = (OtherConvenience / n.units) * 100) %>%
    select(rcID, propChemist, propCTNandGasoline, propFood, 
           propOffLicence, propOtherConvenience)
  rc_grouped <- merge(rc_grouped, conv_types, by = "rcID", all.x = TRUE)
  
  ## Third, the leisure categories
  leisure_types <- pt_count %>%
    as.data.frame() %>%
    select(-c(geom)) %>%
    select(rcID, ldc_aggregation, typology_aggregation) %>%
    filter(ldc_aggregation == "LEISURE")  %>%
    group_by(rcID) %>%
    count(typology_aggregation) %>%
    spread(typology_aggregation, n)
  leisure_types <- merge(leisure_types, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  leisure_types <- leisure_types %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(propBars= (Bars / n.units) * 100,
           propRestaurant = (Restaurant / n.units) * 100,
           propFastFood = (FastFood / n.units) * 100,
           propEntertainment = (Entertainment / n.units) * 100,
           propFitness = (Fitness / n.units) * 100,
           propOtherLeisure = (OtherLeisure / n.units) * 100) %>%
    select(rcID, propBars, propRestaurant, propFastFood, 
           propEntertainment, propFitness, propOtherLeisure)
  rc_grouped <- merge(rc_grouped, leisure_types, by = "rcID", all.x = TRUE)
  
  ## Fourth, the service categories
  service_types <- pt_count %>%
    as.data.frame() %>%
    select(-c(geom)) %>%
    select(rcID, ldc_aggregation, typology_aggregation) %>%
    filter(ldc_aggregation == "SERVICES")  %>%
    group_by(rcID) %>%
    count(typology_aggregation) %>%
    spread(typology_aggregation, n)
  service_types <- merge(service_types, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  service_types <- service_types %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(propConsumerService = (ConsumerService / n.units) * 100,
           propHouseholdService = (HouseholdService / n.units) * 100,
           propBusinessService = (BusinessService/ n.units) * 100) %>%
    select(rcID, propConsumerService, propHouseholdService,
           propBusinessService)
  rc_grouped <- merge(rc_grouped, service_types, by = "rcID", all.x = TRUE)

  
  ## 3. Diversity ########################
  
  ## First the Diversity Indexes 
  ### Count number of distinct categories
  distinct_cats <- pt_count %>%
    as.data.frame() %>%
    select(rcID, top_category, sub_category) %>%
    group_by(rcID) %>%
    dplyr::summarise(top_category_total = n_distinct(top_category))
  
  ### Compute national diversity index
  ### we know that there are 176 distinct top categories in the US, and 357 distinct sub categories
  nat_cat <- distinct_cats %>%
    mutate(nationalCatDiversity = (top_category_total / 176)*100) %>%
    select(rcID, top_category_total, nationalCatDiversity)
    
  ### Compute state diversity index
  local_cat <- pt_count %>%
    as.data.frame() %>%
    select(top_category) %>%
    summarise(local_top_category_total = n_distinct(top_category))
  
  ### Bring two together
  nat_cat$localCatDiversity <- (nat_cat$top_category_total / local_cat$local_top_category_total) * 100
  nat_cat <- nat_cat %>% select(rcID, nationalCatDiversity, localCatDiversity)
  rc_grouped <- merge(rc_grouped, nat_cat, by = "rcID", all.x = TRUE)
  
  
  ## Next the the proportions of different ownership 
  
  ### Popular brands
  popularbrands <- read.csv("Output Data/Typology/PopularBrands.csv")
  p_brands <- pt_count %>%
    as.data.frame() %>%
    select(-c(geom)) %>%
    filter(brands %in% popularbrands$brands) %>%
    group_by(rcID) %>%
    count() 
  p_brands <- merge(p_brands, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  p_brands <- p_brands %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(propPopularBrands = (n / n.units) * 100) %>%
    select(rcID, propPopularBrands) 
  rc_grouped <- merge(rc_grouped, p_brands, by = "rcID", all.x = TRUE)
  rc_grouped <- rc_grouped %>%
    mutate_if(is.numeric, ~replace_na(., 0))
  
  ### Proportions of Independents, Small Multiples and National Chains
  
  ## Read in the classification
  cl_pts <- data.table::fread("Output Data/Typology/OwnershipClassification.csv", header = TRUE)
  cl_pts <- cl_pts %>%
    filter(region == state) %>%
    select(placekey, brand_identifier)
  
  ## Merge on
  pts_recl <- merge(pt_count, cl_pts, by = "placekey", all.x = TRUE)
  
  ## Spread and calculate number of independents, small multiples and national chains
  div_types <- pts_recl %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    filter(ldc_aggregation == "COMPARISON" | ldc_aggregation == "CONVENIENCE" | ldc_aggregation == "SERVICE" | ldc_aggregation == "LEISURE") %>%
    select(rcID, brand_identifier) %>%
    group_by(rcID) %>%
    count(brand_identifier) %>%
    spread(brand_identifier, n)
  div_types <- merge(div_types, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  div_types <- div_types %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(propIndependent = (INDEPENDENT / n.units) * 100,
           propSmallMultiple = (`SMALL MULTIPLE` / n.units) * 100,
           propNationalChain = (`NATIONAL CHAIN` / n.units) * 100) %>%
    select(rcID, propIndependent, propSmallMultiple, propNationalChain)
  rc_grouped <- merge(rc_grouped, div_types, by = "rcID", all.x = TRUE)
  
  
  ## 5. Size and Function Variables ###################
  
  ### Compute roeck scroe
  boundaries$area <- st_area(boundaries)
  boundaries_area <- boundaries %>%
    select(rcID, area)
  circ <- lwgeom::st_minimum_bounding_circle(boundaries_area)
  circ$area <- st_area(circ)
  circ <- circ %>%
    as.data.frame() %>%
    select(rcID, area)
  roeck <- merge(boundaries, circ, by = "rcID", all.x = TRUE)
  roeck$roeck <- as.numeric(roeck$area.x / roeck$area.y)
  roeck$roeck <- scales::rescale(roeck$roeck)
  roeck <- roeck %>%
    as.data.frame() %>%
    select(rcID,roeck)
  rc_grouped <- merge(rc_grouped, roeck, by = "rcID", all.x = TRUE)
  
  ### Compute proportion Anchor
  anchors <-  pt_count %>%
    as.data.frame() %>%
    select(-c(geom)) %>%
    select(rcID, sub_category) %>%
    filter(sub_category == "Department Stores")  %>%
    group_by(rcID) %>%
    count() 
  anchors <- merge(anchors, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  anchors <- anchors %>% 
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(propAnchor = (n / n.units) * 100) %>%
    select(rcID, propAnchor)
  rc_grouped <- merge(rc_grouped, anchors, by = "rcID", all.x = TRUE)
  rc_grouped <- rc_grouped %>%
    mutate_if(is.numeric, ~replace_na(., 0))
  
  ### Compute proportion Mass and Value stores
  discounters <- read.csv("Output Data/Typology/Discounters.csv")
  present_discounters <- pt_count %>%
    as.data.frame() %>%
    select(-c(geom)) %>%
    filter(brands %in% discounters$brands) %>%
    group_by(rcID) %>%
    count() 
  present_discounters <- merge(present_discounters, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  present_discounters <- present_discounters %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(propDiscount = (n / n.units) * 100) %>%
    select(rcID, propDiscount) 
  rc_grouped <- merge(rc_grouped, present_discounters, by = "rcID", all.x = TRUE)
  rc_grouped <- rc_grouped %>%
    mutate_if(is.numeric, ~replace_na(., 0))
  
  ## SmartLocation Variables 
  
  ### Read in the dataset
  query <- paste0("select* from SmartLocation where StateName = '", state, "'")
  sl <- st_read("Input Data/Smart Location/SmartLocation.gpkg", query = query)
  
  ## Join data with retail centres
  sl_rc <- st_join(boundaries, sl)
  ## Compute variables
  sl_out <- sl_rc %>%
    as.data.frame() %>%
    group_by(rcID) %>%
    summarise(Median_Res_Density = median(ResidentialDensity), Median_Emp_Density = median(EmploymentDensity),
              Median_Retail_Emp_Density = median(RetailEmploymentDensity), Median_Road_Density = median(RoadDensity),
              Median_Distance_to_Transit = median(TransitDistance)) %>%
    mutate(Median_Distance_to_Transit = case_when(Median_Distance_to_Transit == -99999 ~ 1000,
                                                  TRUE ~ Median_Distance_to_Transit))
  rc_grouped <- merge(rc_grouped, sl_out, by = "rcID", all.x = TRUE)
  
  
  ## 6. Economic Performance ##########################
  

  ### Low Income Population
  sl_out2 <- sl_rc %>%
    as.data.frame() %>%
    group_by(rcID) %>%
    summarise(Median_LowIncome_Prop = median(LowIncome)) %>%
    select(rcID, Median_LowIncome_Prop)
  rc_grouped <- merge(rc_grouped, sl_out2, by = "rcID", all.x = TRUE)
  
  ### Tenancy Mix 
  tenancy_mix <- pt_count %>%
    as.data.frame() %>%
    select(-c(geom)) %>%
    select(rcID, ldc_aggregation) %>%
    group_by(rcID) %>%
    count(ldc_aggregation) %>%
    spread(ldc_aggregation, n)
  tenancy_mix <- merge(tenancy_mix, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  tenancy_mix <- tenancy_mix %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(Retail = COMPARISON + CONVENIENCE,
           PropRetail = (Retail / n.units) * 100,
           PropService = (SERVICES / n.units) * 100,
           RetailtoService = PropRetail - PropService) %>%
    select(rcID, RetailtoService)
  rc_grouped <- merge(rc_grouped, tenancy_mix, by = "rcID", all.x = TRUE)
  print(paste0(rc_grouped$State, "Variables Extracted"))
  return(rc_grouped)
  
  ### Patterns Variables
  # pt_patterns <- merge(pts, patterns, by = "placekey", all.x = TRUE)
  # 
  # ## Calculate Retail Centre variables
  # ptn_int <- st_intersection(boundaries, ptns)
  # return(ptn_int)
  
  
  
  # ## Read in Patterns
  # patterns <- st_read(paste0("E:/SafeGraph Patterns/WEEKLY PATTERNS/2021/STATE PATTERNS/", state, "_RC_Patterns.gpkg"))
  # patterns <- patterns %>%
  #   select(safegraph_place_id, parent_safegraph_place_id, 
  #          raw_visit_counts, raw_visitor_counts, distance_from_home, median_dwell) %>%
  #   rename(visits = raw_visit_counts, visitors = raw_visitor_counts, distance = distance_from_home, dwell = median_dwell) %>%
  #   replace(is.na(.), 0) %>%
  #   st_transform(4326)
  # 
  # ## Intersect w/ boundaries
  # int <- st_intersection(patterns, boundaries)
  # int_df <- int %>%
  #   as.data.frame() %>%
  #   select(rcID, rcName, visits, visitors, distance, dwell) %>%
  #   group_by(rcID, rcName) %>%
  #   summarise(total_visits = sum(visits), total_visitors = sum(visitors),
  #             median_distance = median(distance), median_dwell = median(dwell))
  # ## Merge
  # rc_grouped <- merge(rc_grouped, int_df, by = c("rcID", "rcName"), all.x = TRUE)
  # 
  
  
  ## 6. geodemographics ######################
  
  # ## Read in Geodemographic variables
  # geodemo <- data.table::fread("Output Data/Typology/NE_Geodemographics.csv")
  # geodemo <- geodemo %>%
  #   select(-c(V1))
  # 
  # ## Merge onto main dataset
  # rc_grouped <- merge(rc_grouped, geodemo, by = c("rcID", "rcName"), all.x = TRUE)
  
  ## BELOW IS HOW TO OBTAIN NE_GEODEMOGRAPHICS TABLE:
  
  # ## Build centroids for the centres
  # centroids <- st_centroid(boundaries)
  # centroids <- st_transform(centroids, 32616)
  # 
  # ## Build a buffer to extract features from 
  # catchment <- st_buffer(centroids, 1000)
  # catchment <- catchment %>% select(rcID, rcName)
  # 
  # ## Read in the Geodemographic/Population dataset
  # gd_pop <- st_read("Input Data/Geodemographics/NE_Geodemographic_Population.gpkg")
  # gd_pop <- gd_pop %>%
  #   select(GEOID, US_GeoDemo_Lookup_Spielman_Singleton_Group, Tot_Pop_2019) %>%
  #   dplyr::rename(Spielman_Singleton_Group = US_GeoDemo_Lookup_Spielman_Singleton_Group)
  # 
  # ## Calculate total pop for each of the Geodemographic categories
  # ne_pop <- gd_pop %>%
  #   as.data.frame() %>%
  #   select(Spielman_Singleton_Group, Tot_Pop_2019) %>%
  #   group_by(Spielman_Singleton_Group) %>%
  #   dplyr::summarise(gd_total = sum(Tot_Pop_2019))
  # ne_pop$ne_total <- sum(ne_pop$gd_total)
  # ne_pop$gd_prop <- (ne_pop$gd_total / ne_pop$ne_total) * 100
  # 
  # ## Calculation total pop in each of the Retail Centres
  # rc_pop <- catchment %>%
  #   st_transform(4326) %>%
  #   st_join(gd_pop) %>%
  #   as.data.frame() %>%
  #   select(rcID, rcName, Tot_Pop_2019) %>%
  #   group_by(rcID, rcName) %>%
  #   dplyr::summarise(total_rc_pop = sum(Tot_Pop_2019))
  # 
  # ## Calculate total population belonging to each geodemographic group, in each retail centre
  # rc_gd <- catchment %>%
  #   st_transform(4326) %>%
  #   st_join(gd_pop) %>%
  #   as.data.frame() %>%
  #   select(-c(geom)) %>%
  #   group_by(rcID, rcName, Spielman_Singleton_Group) %>%
  #   dplyr::summarise(total_gd_pop = sum(Tot_Pop_2019)) %>%
  #   spread(Spielman_Singleton_Group, total_gd_pop, fill = 0) 
  # 
  # 
  # ## Add in any extra columns that may be missing 
  # cols <- c("A: Hispanic & Kids" = NA_real_, "B: Wealthy Nuclear Families" = NA_real_,
  #           "C: Middle Income, Single Family Homes" = NA_real_, "D: Native American" = NA_real_,
  #           "E: Wealthy Urbanites" = NA_real_, "F: Low Income and Diverse" = NA_real_,
  #           "G: Old, Wealthy White" = NA_real_, "H: Low Income, Minority Mix" = NA_real_,
  #           "I: African American Adversity" = NA_real_, "J: Residential Institutions, Young People" = NA_real_)
  # col_list <- c("rcID", "rcName", "A: Hispanic & Kids", "B: Wealthy Nuclear Families", "C: Middle Income, Single Family Homes" ,
  #               "D: Native American", "E: Wealthy Urbanites", "F: Low Income and Diverse", "G: Old, Wealthy White",
  #               "H: Low Income, Minority Mix", "I: African American Adversity", "J: Residential Institutions, Young People")
  # rc_gd<- add_column(rc_gd, !!!cols[setdiff(names(cols), names(rc_gd))])
  # rc_gd <- rc_gd[,col_list]
  # 
  # ## Convert to proportions, and divide by national level proportions
  # rc_gd_pop <- merge(rc_gd, rc_pop, by = c("rcID", "rcName"))
  # colnames(rc_gd_pop) <- c("rcID", "rcName", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "total_rc_pop")
  # rc_gd_pop <- rc_gd_pop %>%
  #   group_by(rcID, rcName) %>%
  #   mutate(GroupA_prop = (A/total_rc_pop) * 100, GroupB_prop = (B/total_rc_pop) * 100, GroupC_prop = (C/total_rc_pop) * 100,
  #          GroupD_prop = (D/total_rc_pop) * 100, GroupE_prop = (E/total_rc_pop) * 100, GroupF_prop = (F/total_rc_pop) * 100, 
  #          GroupG_prop = (G/total_rc_pop) * 100, GroupH_prop = (H/total_rc_pop) * 100, GroupI_prop = (I/total_rc_pop) * 100,
  #          GroupJ_prop = (J/total_rc_pop) * 100) %>%
  #   select(rcID, rcName, GroupA_prop, GroupB_prop, GroupC_prop, GroupD_prop, GroupE_prop, 
  #          GroupF_prop,GroupG_prop, GroupH_prop, GroupI_prop, GroupJ_prop) %>%
  #   mutate(GroupA_NE_prop = GroupA_prop / 5.62, GroupB_NE_prop = GroupB_prop / 35.6, GroupC_NE_prop = GroupC_prop / 28.7,
  #          GroupD_NE_prop = GroupD_prop / 0.0327, GroupE_NE_prop = GroupE_prop / 4.67, GroupF_NE_prop = GroupF_prop / 2.15, 
  #          GroupG_NE_prop = GroupG_prop / 3.31, GroupH_NE_prop = GroupH_prop / 15.6, GroupI_NE_prop = GroupI_prop / 1.48, 
  #          GroupJ_NE_prop = GroupJ_prop / 2.79) %>%
  #   select(rcID, rcName,
  #          GroupA_NE_prop, GroupB_NE_prop, GroupC_NE_prop, GroupD_NE_prop,
  #          GroupE_NE_prop, GroupF_NE_prop, GroupG_NE_prop, GroupH_NE_prop,
  #          GroupI_NE_prop, GroupJ_NE_prop)
  # 
  # ## Merge onto main dataset
  # rc_grouped <- merge(rc_grouped, rc_gd_pop, by = c("rcID", "rcName"), all.x = TRUE)
  


  ## 9. return ######################
  
  ## Merge dataframe onto retail centre boundaries
  boundaries_out <- boundaries %>%
    select(rcID, rcName)
  boundaries_out <- merge(boundaries_out, rc_grouped, by = c("rcID", "rcName"), all.x = TRUE)
  return(boundaries_out)
  
  # ## Return a list - dataframe containing variables for analysis, and boundaries with variables attached
  # out <- list(rc_grouped, boundaries_out)
  
  ## Write them out
  # write.csv(rc_grouped, "Output Data/Typology/NE_typ.csv")
  # st_write(boundaries_out, "Output Data/Typology/NE_typ.gpkg")
  
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

## Function for re-running the typology, to get nested types 
get_nested_types <- function(groups, cl = 1, medoids = FALSE) {
  
  ## Filter to cluster
  groups <- groups %>%
    dplyr::filter(cluster == cl)
  
  ## Remove names/id's
  groups_df <- groups %>%
    as.data.frame() %>%
    select(-c(rcID, rcName, cluster, geom))
  
  ## Get the silhouette scores
  ss <- get_silhouette_scores(groups_df)
  
  ## Extract max
  ss_max <- ss[which.max(ss$avg_silhouette_score),]
  
  ## Run typology
  pm <- run_typology(groups_df, k = ss_max$k)
  
  ## Formatting for output
  clustering <- pm[[1]]
  clustering <- clustering %>%
    select(cluster) %>%
    dplyr::rename(type = cluster)
  
  ## Join with original groups and merge cluster and type columns together
  out <- cbind(groups, clustering)
  out <- out %>%
    dplyr::rename(group = cluster) %>%
    select(rcID, rcName, group, type) %>%
    transform(typ_id = paste(group, type, sep = "."))
  
  
  if (medoids == FALSE) {
    return(out)
  } else if (medoids == TRUE) {
    
    ## Get id's to merge
    type_ids <- out %>%
      as.data.frame() %>%
      select(group, type, typ_id) %>%
      unique()
    
    ## Extract medoids
    medoids <- pm[[2]]
    
    ## Attach new ID's 
    medoids <- merge(medoids, type_ids, by.x = "cluster", by.y = "type", all.x = TRUE)
    medoids <- medoids %>%
      select(typ_id, variable, cluster_vals, pos) %>%
      dplyr::rename(cluster = typ_id)
    return(medoids)
  }
  
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



## Plot type medoids 
plot_type_medoids <- function(medoids) {
  
  ## Split by cluster
  md_ls <- split(medoids, medoids$cluster)
  
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
  ggpubr::ggarrange(plotlist = plots, labels = unique(medoids$cluster))
  
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
