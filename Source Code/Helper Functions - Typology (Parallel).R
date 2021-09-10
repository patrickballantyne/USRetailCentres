## Helper Functions - Typology (Parallel)


## Function that reads in retail centres for a state of interest
get_rc <- function(state = "AL") {
  
  rc_query <- paste0("select* from US_RC_minPts50 where State = '", state, "'") 
  rc <- st_read("output_data/US_RC_minPts50.gpkg", query = rc_query)
  return(rc)
}

## Function that reads in points for a state of interest
get_pts <- function(state = "AL") {
  
  ## Read in the points for the selected state
  query <- paste0("select* from SafeGraph_Cleaned_Places_US where region = '", state, "'")
  pts <- st_read("output_data/SafeGraph_Cleaned_Places_US.gpkg", query = query)
  return(pts)
}

## Region State Lists
ne <- c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT")
mw <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
s <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV",
       "AL", "KY", "MS", "TN",
       "AR", "LA", "OK", "TX")
w <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
       "AK", "CA", "HI", "OR", "WA")

## Build a variale distance buffer for extraction of catchment characteristics
var_buffer <- function(rc) {
  
  rc <- st_transform(rc, 32616)
  rc_a <- rc %>% filter(N.pts <= 80)
  rc_b <- rc %>% filter(N.pts > 80)
  
  buf_a <- st_buffer(rc_a, 24000)
  buf_b <- st_buffer(rc_b, 40000)
  
  buf <- rbind(buf_a, buf_b)
  buf <- st_transform(buf, 4326)
  return(buf) ## Return
}

## Function that pulls in all the variables for the typology
prep4typology <- function(state, patterns) {
  
  ## Read in the datasets we need for this
  boundaries <- get_rc(state)
  pts <- get_pts(state = state)
  bdgs <- st_read(paste0("output_data/", state, "_Retail_Buildings.gpkg"))
  
  ## Merge on the new aggregations
  ldc <- read.csv("output_data/SafeGraph_Places_Categories_LDC.csv", header = TRUE)
  pts <- merge(pts, ldc, by = c("top_category", "sub_category"), all.x = TRUE)
  
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
  boundaries$area <- boundaries$area / 1000
  
  ## Calculate Retail Building Density
  densities <- merge(boundaries, bdg_count, by = "rcID", all.x = TRUE)
  densities <- densities %>%
    as.data.frame() %>%
    select(rcID, n.bdgs, area) %>%
    mutate(bdg_density = n.bdgs/area) %>%
    select(-c(n.bdgs)) %>%
    mutate_at(vars(area, bdg_density), as.numeric) %>%
    rename(retailDensity = bdg_density)
  rc_grouped <- merge(rc_grouped, densities, by = "rcID", all.x = TRUE)
  
  ## 2. Proportions different types of retail ##########################
  
  ### Intersect
  pts <- st_set_crs(pts, 4326)
  boundaries <- st_transform(boundaries, 4326)
  pt_count <- st_intersection(pts, boundaries)
  
  ### First, the comparison categories
  comp_types <- pt_count %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
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
           propRecreational = (Recreational / n.units) * 100) %>%
    select(rcID, propClothingandFootwear, propDIYandHousehold, propElectrical,
           propRecreational)
  rc_grouped <- merge(rc_grouped, comp_types, by = "rcID", all.x = TRUE)
  
  ## Second the comparison categories
  conv_types <- pt_count %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
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
           propFoodandDrink = (FoodandDrink / n.units) * 100,
           propGeneralMerchandise = (GeneralMerchandise / n.units) * 100) %>%
    select(rcID, propChemist, propCTNandGasoline, propFoodandDrink, propGeneralMerchandise)
  rc_grouped <- merge(rc_grouped, conv_types, by = "rcID", all.x = TRUE)
  
  ## Third, the leisure categories
  leisure_types <- pt_count %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
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
           propFitness = (Fitness / n.units) * 100) %>%
    select(rcID, propBars, propRestaurant, propFastFood, 
           propEntertainment, propFitness)
  rc_grouped <- merge(rc_grouped, leisure_types, by = "rcID", all.x = TRUE)
  
  ## Fourth, the service categories
  service_types <- pt_count %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
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
  print("COMPOSITION VARIABLES EXTRACTED")
  
  ## 3. Diversity ########################
  

  # 3.1 Retail Diversity Index ----------------------------------------------

  ## Count number of distinct retail categories in each retail centre
  retail_distinct_cats <- pt_count %>%
    as.data.frame() %>%
    select(rcID, top_category, sub_category, ldc_aggregation) %>%
    filter(ldc_aggregation == "COMPARISON" | ldc_aggregation == "CONVENIENCE") %>%
    group_by(rcID) %>%
    dplyr::summarise(top_category_total = n_distinct(top_category))
  

  ### Compute national diversity index
  ### we know that there are 117 distinct retail top categories in the US
  nat_retail_cat <- retail_distinct_cats %>%
    mutate(nationalRetailDiversity = (top_category_total / 117)*100) %>%
    select(rcID, top_category_total, nationalRetailDiversity)
  
  ### Compute state diversity index
  local_retail_cat <- pt_count %>%
    as.data.frame() %>%
    filter(ldc_aggregation == "COMPARISON" | ldc_aggregation == "CONVENIENCE") %>%
    select(top_category) %>%
    summarise(local_top_category_total = n_distinct(top_category))
  
  ### Bring two together
  nat_retail_cat$localRetailDiversity <- (nat_retail_cat$top_category_total / local_retail_cat$local_top_category_total) * 100
  nat_retail_cat <- nat_retail_cat %>% select(rcID, nationalRetailDiversity, localRetailDiversity)
  

  # 3.2 Service Diversity Index ---------------------------------------------

  ## Count number of distinct service categories in each retail centre
  service_distinct_cats <- pt_count %>%
    as.data.frame() %>%
    select(rcID, top_category, sub_category, ldc_aggregation) %>%
    filter(ldc_aggregation == "SERVICES") %>%
    group_by(rcID) %>%
    dplyr::summarise(top_category_total = n_distinct(top_category))
  
  
  ### Compute national diversity index
  ### we know that there are 74 distinct service top categories in the US
  nat_service_cat <- service_distinct_cats %>%
    mutate(nationalServiceDiversity = (top_category_total / 74)*100) %>%
    select(rcID, top_category_total, nationalServiceDiversity)
  
  ### Compute state diversity index
  local_service_cat <- pt_count %>%
    as.data.frame() %>%
    filter(ldc_aggregation == "SERVICES") %>%
    select(top_category) %>%
    summarise(local_top_category_total = n_distinct(top_category))
  
  ### Bring two together
  nat_service_cat$localServiceDiversity <- (nat_service_cat$top_category_total / local_service_cat$local_top_category_total) * 100
  nat_service_cat <- nat_service_cat %>% select(rcID, nationalServiceDiversity, localServiceDiversity) 
  
  rc_grouped <- merge(rc_grouped, nat_retail_cat, by = "rcID", all.x = TRUE)
  rc_grouped <- merge(rc_grouped, nat_service_cat, by = "rcID", all.x = TRUE)

  
  
  ## Next the the proportions of different ownership 
  
  ### Popular brands - Comparison
  popularCompBrands <- read.csv("output_data/PopularComparisonBrands.csv")
  p_comparison_brands <- pt_count %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    filter(brands %in% popularCompBrands$brands) %>%
    group_by(rcID) %>%
    count() 
  p_comparison_brands <- merge(p_comparison_brands, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  p_comparison_brands <- p_comparison_brands %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(propPopularComparisonBrands = (n / n.units) * 100) %>%
    select(rcID, propPopularComparisonBrands) 
  rc_grouped <- merge(rc_grouped, p_comparison_brands, by = "rcID", all.x = TRUE)
  rc_grouped <- rc_grouped %>%
    mutate_if(is.numeric, ~replace_na(., 0))
  
  ### Popular brands - Convenience
  popularConvBrands <- read.csv("output_data/PopularConvenienceBrands.csv")
  p_convenience_brands <- pt_count %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    filter(brands %in% popularConvBrands$brands) %>%
    group_by(rcID) %>%
    count() 
  p_convenience_brands <- merge(p_convenience_brands, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  p_convenience_brands <- p_convenience_brands %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(propPopularConvenienceBrands = (n / n.units) * 100) %>%
    select(rcID, propPopularConvenienceBrands) 
  rc_grouped <- merge(rc_grouped, p_convenience_brands, by = "rcID", all.x = TRUE)
  rc_grouped <- rc_grouped %>%
    mutate_if(is.numeric, ~replace_na(., 0))
  
  ### Popular brands - Leisure
  popularLeisureBrands <- read.csv("output_data/PopularLeisureBrands.csv")
  p_leisure_brands <- pt_count %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    filter(brands %in% popularLeisureBrands$brands) %>%
    group_by(rcID) %>%
    count() 
  p_leisure_brands <- merge(p_leisure_brands, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  p_leisure_brands <- p_leisure_brands %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(propPopularLeisureBrands = (n / n.units) * 100) %>%
    select(rcID, propPopularLeisureBrands) 
  rc_grouped <- merge(rc_grouped, p_leisure_brands, by = "rcID", all.x = TRUE)
  rc_grouped <- rc_grouped %>%
    mutate_if(is.numeric, ~replace_na(., 0))
  
  ### Proportions of Independents, Small Multiples and National Chains
  
  ## Read in the classification
  cl_pts <- data.table::fread("output_data/OwnershipClassification.csv", header = TRUE)
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
  print("DIVERSITY VARIABLES EXTRACTED")
  
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
  anchor_ls <- read.csv("output_data/Anchors.csv")
  anchors <-  pt_count %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    filter(brands %in% anchor_ls$brands) %>%
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
  discounters <- read.csv("output_data/Discounters.csv")
  present_discounters <- pt_count %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
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
  
  ## Compute proportion Premium Brands
  premiumbrands <- read.csv("output_data/PremiumBrands.csv")
  present_premium <- pt_count %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    filter(brands %in% premiumbrands$brands) %>%
    group_by(rcID) %>%
    count() 
  present_premiums <- merge(present_premium, rc_grouped[, c("rcID", "n.units")], all.x = TRUE)
  present_premiums <- present_premiums %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(propPremiumBrand = (n / n.units) * 100) %>%
    select(rcID, propPremiumBrand) 
  rc_grouped <- merge(rc_grouped, present_premiums, by = "rcID", all.x = TRUE)
  rc_grouped <- rc_grouped %>%
    mutate_if(is.numeric, ~replace_na(., 0))
  
  
  ## SmartLocation Variables 
  
  ### Read in the dataset
  query <- paste0("select* from SmartLocation where StateName = '", state, "'")
  sl <- st_read("output_data/SmartLocation.gpkg", query = query)
  
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
  print("SIZE & FUNCTION VARIABLES EXTRACTED")
  
  ## 6. Economic Performance ##########################
  
  
  ## Download from tidycensus - median income & unemployment at census block group level
  census_vars <- get_acs(geography = "block group",
                         variables = c(total = "B23025_001",
                                       unemployed = "B23025_005",
                                       medincome = "B19013_001"),
                         state = state, 
                         year = 2018)
  census_vars_w <- census_vars %>%
    select(GEOID, NAME, variable, estimate) %>%
    spread(variable, estimate)
  
  cbg <- tigris::block_groups(state = state)
  cbg <- cbg %>%
    select(GEOID)
  cbg_census <- merge(cbg, census_vars_w, by = "GEOID")
  cbg_census <- st_transform(cbg_census, 4326)
  
  
  ## Build catchments for centres 
  buf <- var_buffer(boundaries)
  
  ## Extract those within RC and calculate median vals
  join <- st_join(st_transform(boundaries, 4326), cbg_census)

  income <- join %>%
    as.data.frame() %>%
    select(rcID, medincome) %>%
    group_by(rcID) %>%
    filter(!is.na(medincome)) %>%
    summarise(medianIncome = median(medincome)) 
  unemploy <- join %>%
    as.data.frame() %>%
    select(rcID, unemployed, total) %>%
    filter(!is.na(unemployed)) %>%
    mutate(propUnemployed = (unemployed/total) * 100) %>%
    group_by(rcID) %>%
    summarise(medianUnemployed = median(propUnemployed))
  totalpop <- join %>%
    as.data.frame() %>%
    filter(!is.na(total)) %>%
    select(rcID, total) %>%
    group_by(rcID) %>%
    summarise(totalPopulation = sum(total))
  rc_grouped <- merge(rc_grouped, income, by = "rcID", all.x = TRUE)
  rc_grouped <- merge(rc_grouped, unemploy, by = "rcID", all.x = TRUE)
  rc_grouped <- merge(rc_grouped, totalpop, by = "rcID", all.x = TRUE)

  
  ## Calculate number of competing centres
  com_int <- st_join(buf, boundaries)
  com_int_df_small <- com_int %>%
    as.data.frame() %>%
    select(rcID.x, N.pts.x) %>%
    filter(N.pts.x <= 80) %>%
    group_by(rcID.x) %>%
    count() %>%
    setNames(c("rcID", "nCompeting"))
  com_int_df_large <- com_int %>%
    as.data.frame() %>%
    select(rcID.x, N.pts.x) %>%
    filter(N.pts.x > 80) %>%
    group_by(rcID.x) %>%
    count() %>%
    setNames(c("rcID", "nCompeting"))
  com_out <- rbind(com_int_df_large, com_int_df_small)
  rc_grouped <- merge(rc_grouped, com_out)
    

  ### Tenancy Mix 
  tenancy_mix <- pt_count %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
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
  
  ### Patterns Variables
  
  ## First subset patterns to state
  pats_v <- vroom("output_data/july_2021_patterns.csv", delim = ",")
  patterns <- pats_v %>%
    filter(region == state) %>%
    select(placekey, raw_visit_counts, raw_visitor_counts, visitor_home_cbgs,
           distance_from_home, median_dwell)
  
  ## Join to main dataset
  pt_patterns <- merge(pt_count, patterns, by = "placekey", all.x = TRUE)
  
  ## Remove non-retail
  pt_patterns <- pt_patterns %>%
    filter(ldc_aggregation != "MISC") %>%
    filter(!is.na(ldc_aggregation))
  
  
  ## Calculate variables
  visits <- pt_patterns %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    filter(!is.na(raw_visit_counts)) %>%
    group_by(rcID) %>%
    summarise(totalVisits = sum(raw_visit_counts))
  distance <- pt_patterns %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
    filter(!is.na(distance_from_home)) %>%
    group_by(rcID) %>%
    summarise(medianDistance = median(distance_from_home)) %>%
    mutate(medianDistance = medianDistance / 1000)
  rc_grouped <- merge(rc_grouped, visits, by = "rcID", all.x = TRUE) 
  rc_grouped <- merge(rc_grouped, distance, by = "rcID", all.x = TRUE)
  print("ECONOMIC PERFORMANCE VARIABLES EXTRACTED")
  
  
  # 7. Tidying Up Output ----------------------------------------------------
  
  ## Tidying up 
  out_df <- rc_grouped %>%
    rename(nUnits = n.units, roeckScore = roeck,  residentialDensity = Median_Res_Density, employmentDensity = Median_Emp_Density, 
           retailemploymentDensity = Median_Retail_Emp_Density, roadDensity = Median_Road_Density, transitDistance = Median_Distance_to_Transit, retailService = RetailtoService) %>%
    select(rcID, 
           propClothingandFootwear, propDIYandHousehold, propElectrical, propRecreational,
           propChemist, propCTNandGasoline, propFoodandDrink, propGeneralMerchandise,
           propBars, propRestaurant, propFastFood, propEntertainment, propFitness,
           propConsumerService, propHouseholdService, propBusinessService,
           propIndependent, propSmallMultiple, propNationalChain, propPopularComparisonBrands, propPopularConvenienceBrands, propPopularLeisureBrands,
           nationalRetailDiversity, localRetailDiversity, nationalServiceDiversity, localServiceDiversity,
           nUnits, roeckScore, medianDistance, retailDensity, residentialDensity, retailemploymentDensity, roadDensity, propAnchor, propDiscount, propPremiumBrand,
           totalVisits, medianUnemployed, medianIncome, totalPopulation, retailService, nCompeting) #%>%
    #mutate(totalVisits = replace_na(totalVisits, 0),
          # medianDistance = replace_na(medianDistance, 0),
           #medianUnemployed = replace_na(medianUnemployed, 0),
          # medianIncome = replace_na(medianIncome, 0))
  print(paste0(state, " ", "Variables Extracted"))
  return(out_df)}
  

## Function that computes average silhouette scores across values of K, for determing K value used in PAM
get_silhouette_scores <- function(db, seed = 123) {
  
  ## Params
  set.seed(seed)
  k_ls <- c(2:10)
  
  ## Run PAM
  cl <- mclapply(k_ls, function(x) pam(db, k = x), mc.cores = 9)
  cl <- mclapply(cl, function(x) {
    X <- x$clustering
    return(X)
  }, mc.cores = 9)
  
  cl_out <- as.data.frame(do.call(cbind, cl))
  colnames(cl_out) <- c("k=2", "k=3", "k=4", "k=5", "k=6", "k=7", "k=8", "k=9", "k=10")
  #db_cl <- cbind(db, cl_out)
  
  ## Computation of silhouette scores
  col_ls <- c(1:9)
  ss <- mclapply(col_ls, function(x) silhouette(cl_out[, x], dist(db)), mc.cores = 9)
  
  ## Computation of average silhouette scores
  avg_ss <- lapply(ss, function(x) mean(x[, 3]))
  avg_ss <- as.data.frame(do.call(rbind, avg_ss))
  avg_ss$k <- c(2:10)
  colnames(avg_ss) <- c("avg_silhouette_score", "k")
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

## Function that performs PCA on each group to identify variables worth removing 
run_type_pca <- function(groups, cl = 1) {
  
  ## Filter and process
  groups <- groups %>%
    filter(cluster == cl) %>%
    as.data.frame() %>%
    select(-c(cluster))
  
  ## Run PCA
  pca <- PCA(groups, graph = FALSE)
  
  ## Visualise 
  fviz_contrib(pca, choice = "var", axes = 1:2)
}


## Function used to help identify best k value to split each group into types
identify_type_k <- function(groups, cl = 1, col_list) {
  
  ## Filter and process
  groups <- groups %>%
    filter(cluster == cl) %>%
    as.data.frame() %>%
    select(-c(rcID, rcName, cluster, geom))
  
  ## Drop insignificant vars
  groups = groups[,!(names(groups) %in% col_list)]
  
  ## Get the silhouette scores
  ss <- get_silhouette_scores(groups)
  
  ## Plot silhouettes
  silhouettes <- fviz_nbclust(groups,  cluster::pam, method = "silhouette", k.max = 10) +
    labs(subtitle = "Silhouette Method")
  
  ## Plot the elbow
  elbow <- fviz_nbclust(groups,  cluster::pam, method = "wss", k.max = 10) +
    labs(subtitle = "Elbow Method")
  
  ## Return
  ls <- list(ss, silhouettes, elbow)
  return(ls)
}


## Function for re-running the typology, to get nested types 
get_nested_types <- function(groups, cl = 1, k_vals, medoids = FALSE) {
  
  ## Filter to cluster
  groups <- groups %>%
    dplyr::filter(cluster == cl)
  
  ## Remove names/id's
  groups_df <- groups %>%
    as.data.frame() %>%
    select(-c(rcID, rcName, cluster, geom))
  
  ## Filter k vals to get only the cluster we want
  k <- k_vals %>%
    filter(cluster == cl)
  
  ## Run typology
  pm <- run_typology(groups_df, k = k$k)
  
  ## Formatting for output
  clustering <- pm[[1]]
  clustering <- clustering %>%
    select(cluster) %>%
    dplyr::rename(type = cluster)
  
  ## Join with original groups and merge cluster and type columns together
  out <- cbind(groups, clustering)
  return(out)
  out <- out %>%
    dplyr::rename(group = cluster) %>%
    select(rcID, rcName, group, type) %>%
    dplyr::rename(group_id = group) %>%
    transform(type_id = paste(group_id, type, sep = "."))
  
  
  if (medoids == FALSE) {
    out <- out %>% select(-c(type))
    return(out)
  } else if (medoids == TRUE) {
    
    ## Get id's to merge
    type_ids <- out %>%
      as.data.frame() %>%
      select(group_id, type, type_id) %>%
      unique()
    
    ## Extract medoids
    medoids <- pm[[2]]
    
    ## Attach new ID's 
    medoids <- merge(medoids, type_ids, by.x = "cluster", by.y = "type", all.x = TRUE)
    medoids <- medoids %>%
      select(type_id, variable, cluster_vals, pos) %>%
      dplyr::rename(cluster = type_id)
    return(medoids)
  }
  
}


## Compute p values for corrplot
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
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
                    ylab("Median Values") +
                    theme(axis.text.x = element_text(angle = 90), axis.ticks = element_line(),
                          axis.line = element_line(colour = "black"), axis.title.x = element_blank(),
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank()))
  ggpubr::ggarrange(plotlist = plots, vjust = 1.0, align = "hv")
  
}
