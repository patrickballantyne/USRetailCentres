## Helper Functions


# 1. H3 Functions ---------------------------------------------------------

## Function that takes points and aggregates them to H3 geometries
points2h3 <- function(pts, h3_res = 11) {
  
  ## Aggregate to H3
  h3 <- h3jsr::point_to_h3(pts, res = h3_res, simple = FALSE)
  h3_list <- unlist(h3$h3_resolution_11, use.names = TRUE)
  
  h3 <- h3jsr::h3_to_polygon(h3_list)
  h3 <- st_as_sf(h3)
  
  h3_poly <- cbind(h3, pts)
  h3_poly <- h3_poly %>%
    select(-c(geom))
  h3_poly <- cbind(h3_poly, h3_list)
  h3_poly <- h3_poly %>%
    select(h3_list, x) %>%
    rename(h3_address = h3_list, geom = x) %>%
    unique()
  return(h3_poly)
}

## Functions for turning polygons into H3
poly2h3 <- function(sf, h3_res = 11) {
  
  ## Polyfill
  h3 <- polyfill(sf, h3_res, simple = FALSE)
  ## Convert 
  poly_h3 <- h3_to_polygon(unlist(h3$h3_polyfillers), simple = FALSE)
  poly_h3 <- poly_h3 %>%
    select(h3_address) %>%
    unique() %>%
    drop_na()
  return(poly_h3)
}


## Modified version of poly2h3, to work on buildings/landuse polygons - fill at a lower resolution and then pull out parent
## H3's at resolution 11
buildings2h3 <- function(buildings, lower_res = 12) {
  
  ## Polyfill at the low resolution
  b <- poly2h3(buildings, h3_res = lower_res)
  
  ## Extract parent h3's at resolution 11
  p <- get_parent(b$h3_address, 11, FALSE)
  p <- p %>%
    select(h3_parent) %>%
    unique()
  
  ## Convert to polygon
  p_h3 <- h3_to_polygon(unlist(p$h3_parent), simple = FALSE)
  p_h3 <- p_h3 %>%
    select(h3_address, geometry) %>%
    rename(geom = geometry)
  return(p_h3)
}

## Function for turning lines to H3
lines2h3 <- function(sf, h3_res = 11) {
  
  ## Extract lines
  sf <- sf %>%
    filter(grepl("LINESTRING", st_geometry_type(geometry)))
  
  ## Convert to points
  n <- nrow(sf)
  sf <- st_as_sf(st_sample(sf, (n * 500), type = "regular"))
  sf <- st_cast(sf, "POINT")
  sf <- sf %>%
    rename(geom = x)
  
  ## Get H3 geometries
  sf_h3 <- points2h3(sf, h3_res, FALSE)
  sf_h3 <- sf_h3 %>%
    unique()
  return(sf_h3)
}

## Function that extracts the H3 addresses for all retail features in a state - SafeGraph Buildings, Points and Land-Use Polygons from OSM
extract_state_h3 <- function(state = "AL") {
  
  ## 1. Gather Land-Use
  landuse <- list.files(paste0("Output Data/Land-Use/Processed"), pattern = paste0(state, "_LU.gpkg$"), full.names = TRUE)
  landuse <- lapply(landuse, st_read)
  landuse <- do.call(rbind, landuse)
  
  ## 2. Buildings
  buildings <- list.files(paste0("Output Data/Buildings/"), pattern = paste0(state, "_Retail_Buildings.gpkg$"), full.names = TRUE)
  buildings <- lapply(buildings, st_read)
  buildings <- do.call(rbind, buildings)
  buildings_d <- buildings %>%
    summarise()
  
  ## 3. Points 
  query <- paste0("select* from SafeGraph_Retail_Places_US where region = '", state, "'")
  pts <- st_read("Output Data/SafeGraph_Retail_Places_US.gpkg", query = query)
  
  ## 4. Converting to H3
  
  ### Points
  pts_h3 <- points2h3(pts, 11)
  print("Points Converted")
  
  ### Buildings
  if (nrow(buildings) < 100000) {
    
    bdg_h3 <- buildings2h3(buildings_d, 12)
    print("Buildings Converted")
    #return(bdg_h3)
    
  } else {
    
    print("Too Many Features - Chunking...")
    
    ## Split into smaller subsets
    bdg_ls <- split(buildings, rep(1:ceiling(nrow(buildings)/50000), each=50000, length.out=nrow(buildings)))
    bdg_ls <- lapply(bdg_ls, function(x) summarise(x))
    
    ## Apply function on smaller subsets
    bdg_h3 <- lapply(bdg_ls, buildings2h3)
    bdg_h3 <- do.call(rbind, bdg_h3)
    bdg_h3 <- bdg_h3 %>%
      unique()
    print("Buildings Converted")
    #return(bdg_h3)
  }
  
  ### Land-Use
  if(is.null(landuse) == FALSE) {
    
    ## Convert 
    lu_h3 <- buildings2h3(landuse, 12)
    print("Land-Use Converted")
    
    ## Join together final features
    out_h3 <- rbind(pts_h3, bdg_h3, lu_h3)
    out_h3 <- out_h3 %>%
      unique()
    print("Returning H3's")
    
  } else {
    
    print("No Land-Use Data")
    
    ## Rbind just points and buildings
    out_h3 <- rbind(pts_h3, bdg_h3)
    print("Returning H3's")
  }
  
  
  ## 3. Write out
  st_write(out_h3, paste0("Output Data/Retail H3/", state, "_Retail_H3.gpkg"))
  
}



# 2. OSM Functions --------------------------------------------------------

## Function that splits an AoI into multiple smaller areas, to make the osmdata API work without crashing (Source: Jacob Macdonald)
bb.list <- function(BB, sq.N){
  x.n <- (BB[1,2] - BB[1,1])/sq.N
  y.n <- (BB[2,2] - BB[2,1])/sq.N
  x <- BB[1,1] + x.n*c(0:sq.N)
  y <- BB[2,1] + y.n*c(0:sq.N)
  
  bb.x <- list()
  bb.y <- list()
  for(i in 1:sq.N){
    x.1 <- cbind(x[i], x[i+1])
    y.1 <- cbind(y[i], y[i+1])
    
    bb.x[[i]] <- x.1    
    bb.y[[i]] <- y.1
  }
  bb.N <- unlist(lapply(bb.x, function(a) lapply(bb.y, function (b) rbind(a, b))), recursive=FALSE)
  bb.N <- lapply(bb.N, function(x) {dimnames(x) <- list(c("x", "y"), c("min", "max")); return(x)})
  return(bb.N)
}

## Function that extracts Retail Land-Use polygons from OSM, using the bb.list function above to ensure the API doesn't crash
get_osm_polygons <- function(bb = "Illinois, US", d = 15) {
  
  ## Landuse
  t <- lapply(bb.list(getbb(bb), sq.N = d), function(x) {
    
    opq(bbox = x) %>%
      add_osm_feature(key = "landuse", value = "retail") %>%
      osmdata_sf()})
  
  osm.retailLU <- t[which(do.call(rbind, lapply(t, function(y) dim(y$osm_polygons)[1]!=0)))]
  osm.retailLU <- lapply(osm.retailLU, function(x) x$osm_polygons)
  osm.retailLU <- lapply(osm.retailLU, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.retailLU, names))])
  osm.retailLU <- do.call(rbind, osm.retailLU)
  osm.retailLU <- unique(osm.retailLU)
  osm.retailLU <- osm.retailLU %>%
    select(osm_id, geometry)
  osm.retailLU$identifier <- "land-use"
  return(osm.retailLU)
  
}

# 3. Additional SafeGraph Functions --------------------------------------------------

## Function that reads in SafeGraph retail points for a state of interest
read_points <- function(state = "AL") {
  
  ## Read in the points for the selected state
  query <- paste0("select* from SafeGraph_Retail_Places_US where region = '", state, "'")
  pts <- st_read("output_data/SafeGraph_Retail_Places_US.gpkg", query = query)
  return(pts)
}

# 4. Urban Features Functions ---------------------------------------------

## Function that processes main roads for a state - Roads data came from the National Transportation Dataset (NTD)
## https://www.sciencebase.gov/catalog/item/4f70b1f4e4b058caae3f8e16
get_roads <- function(state = "AL") {
  
  # Variable
  fpsc_list <- c("S1100")
  
  ## List files in directory
  file_list <- list.files(paste0("Output Data/Roads/", state), pattern = "*shp$", full.names = TRUE)
  ## Read in & rbind
  shp_list <- lapply(file_list, st_read)
  rds <- do.call(rbind, shp_list)
  ## Filter to main roads only and project
  rds <- rds %>%
    filter(MTFCC_CODE %in% fpsc_list) %>%
    select(OBJECTID) %>%
    st_transform(4326)
  
  ## Convert to polygons
  rds = st_transform(rds,"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")
  rds <- rds %>%
    st_buffer(10) %>%
    st_transform(4326) %>%
    select(geometry)
  return(rds)
  
}

## Function that processes water for a state - Water data came from the National Hydrography Dataset (NHD)
## https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
get_water <- function(state = "AL") {
  
  ## List files
  file_list <- list.files(paste0("output_data/Water/", state), pattern = "*shp$", full.names = TRUE)
  ## Read in and rbind()
  shp_list <- lapply(file_list, st_read)
  wtr = lapply(shp_list, "[", c("OBJECTID", "FType", "FCode"))
  wtr<- plyr::rbind.fill(wtr)
  
  ## Clean up and project
  wtr <- wtr %>%
    st_as_sf() %>%
    filter(!FCode == 39010) %>%
    st_transform(4326) %>%
    st_zm(drop = TRUE) %>%
    select(geometry)
  return(wtr)
  
}

## Function for bringing together the roads and water shapefiles for a state
get_urban_features <- function(state = "AL") {
  
  ## Get Roads
  # rds <- get_roads(state)
  # print("ROADS PROCESSED")
  ## Get Water
  wtr <- get_water(state)
  print("WATER PROCESSED")
  ## Join 
  # print("JOINING AND DISSOLVING")
  # uf <- rbind(rds, wtr)
  return(wtr)
}


# 5. Retail Centre Boundaries Functions -----------------------------------

## Primary Function - takes Retail Features (H3) and builds tracts, delimited partially by urban features (roads, water), before
## using the original points and buildings datasets to define minimum parameters for a retail centre
get_h3_clusters <- function(retail_features, h3_resolution = 11, min_pts = 10, boundary = FALSE, identifier = "IL") {
  
  ## Start time
  
  ## 1. Compile a list of h3 addresses for points and polygons ##############################
  h3_list <- retail_features 

  ###########################################################################################
  
  ## 1. Do the first round of tracts ########################################################
  hex_graph <- as_tibble(do.call(rbind, mclapply(as.list(h3_list$h3_address), function(x) get_kring(x, 1), mc.cores = 26))) 
  hex_graph <- hex_graph %>%
      separate(V1, into = paste0("to_", 1:9)) %>%
      select(-c(1, 9)) %>%
      gather("neighbourN", "h3_neighbour", -to_2) %>%
      rename(h3_address = to_2) %>%
      select(h3_address, h3_neighbour) %>%
      arrange(h3_address, h3_neighbour) %>%
      filter(h3_neighbour %in% unique(h3_list$h3_address) & h3_address %in% unique(h3_list$h3_address)) %>%
      filter(!is.na(h3_address) | h3_address != "") %>%
      filter(!is.na(h3_neighbour) | h3_neighbour != "") %>%
      unique() 
  
  ## Create graph object
  i <- as_tbl_graph(hex_graph, directed = FALSE)
  i <- split(names(V(i)), components(i)$membership)
  
  ## Use graph object to build tracts and ID's
  hex_Graph.N <- mclapply(as.list(1:length(i)), function(x) paste0("tractID_", names(i[x])), mc.cores = 12)
  i.graph <- as_tibble(do.call(rbind, mclapply(as.list(1:length(i)), function(x) cbind(hex_Graph.N[[x]], i[[x]]), mc.cores = 12)))
  names(i.graph) <- c("tractID", "h3_address")
  
  ## Split by tractID 
  g <- i.graph$tractID
  igraph_ls <- mclapply(split(i.graph, g), function(x) {
    
    v <- x %>%
        filter(h3_address %in% unique(h3_list$h3_address)) %>%
        select(tractID, h3_address) %>%
        filter(!is.na(tractID) | tractID!="") %>%
        filter(!is.na(h3_address) | h3_address!="") %>%
        group_by(tractID) %>%
        select(tractID, h3_address) %>%
        unique()
    
    hl <- unlist(v$h3_address, use.names = TRUE)
    h <- h3_to_polygon(hl)
    h <- st_as_sf(h)
    h <- bind_cols(h, v)
    h <- h %>%
      rename(geometry = x) %>%
      select(h3_address, tractID, geometry)
    return(h)

  }, mc.cores = 12)
  
  igraph_clusters <- do.call(rbind_list, igraph_ls)
  
  ## Read in the urban features
  urban_features <- get_urban_features(state = identifier)

  ## New method - 20/01 for Urban Feature RemovaL
  hex_touch <- fast_intersect(igraph_clusters, urban_features)
  
  ## Remove
  igraph_clusters <- igraph_clusters %>%
    filter(!h3_address %in% hex_touch$h3_address)
  print("STAGE ONE COMPLETE - FIRST SET OF TRACTS EXTRACTED")
  
  #############################################################################################
  
  # 2. Grab the connecting hexagons ###########################################################
  
  ## Grab neighbouring hexagons again
  hex_connector <- as_tibble(do.call(rbind, mclapply(as.list(igraph_clusters$h3_address), function(x) get_kring(x, 1), mc.cores = 12)))
  hex_connector <- hex_connector %>%
      separate(V1, into = paste0("to_", 1:9)) %>%
      select(-c(1, 9)) %>%
      gather("neighbourN", "h3_connector", -to_2) %>%
      rename(h3_address = to_2) %>%
      select(h3_address, h3_connector) %>%
      left_join(igraph_clusters, by = "h3_address") %>%
      unique() %>%
      filter(!(h3_connector %in% unique(igraph_clusters$h3_address))) %>%
      filter(!is.na(h3_address) & h3_address != "") %>%
      filter(!is.na(h3_connector) & h3_connector != "")
  
  ## Format connector h3 geoms
  connections <- hex_connector %>%
    select(h3_connector, tractID) %>%
    filter(h3_connector %in% unique(.[["h3_connector"]][duplicated(.[["h3_connector"]])])) %>%
    unique() %>%
    group_by(h3_connector) %>%
    add_count() %>%
    arrange(h3_connector, tractID) %>%
    #filter(n > 1) %>%
    select(tractID, h3_connector) %>%
    unique()

  
  ## Format connecting hexagons for out
  hexes_list <- unlist(connections$h3_connector, use.names = TRUE)
  hexes <- h3_to_polygon(hexes_list)
  hexes <- st_as_sf(hexes)
  hexes <- bind_cols(hexes, connections)
  hexes <- hexes %>%
    rename(geometry = x, h3_address = h3_connector) %>%
    select(h3_address, tractID, geometry)
  hexes$connector <- "Y"
  igraph_clusters$connector <- "N"
  ls <- list(igraph_clusters, hexes)
  out <- rbind(igraph_clusters, hexes)
  
  ## Re-run urban features delineation 
  hex_touch2 <- fast_intersect(out, urban_features)
  
  ## Remove
  out <- out %>%
    filter(!h3_address %in% hex_touch2$h3_address)
  print("STAGE TWO COMPLETE - CONNECTING HEXAGONS EXTRACTED")
  
  #####################################################################################
  
  # 3. Re-run first step to get final set of tracts
  
  igraph_clusters <- out %>%
    select(h3_address)
  
  ## Get neighbouring hexagons
  hex_graph <- as_tibble(do.call(rbind, mclapply(as.list(igraph_clusters$h3_address), function(x) get_kring(x, 1), mc.cores = 12))) 
  hex_graph <- hex_graph %>%
    separate(V1, into = paste0("to_", 1:9)) %>%
    select(-c(1, 9)) %>%
    gather("neighbourN", "h3_neighbour", -to_2) %>%
    rename(h3_address = to_2) %>%
    select(h3_address, h3_neighbour) %>%
    arrange(h3_address, h3_neighbour) %>%
    filter(h3_neighbour %in% unique(igraph_clusters$h3_address) & h3_address %in% unique(igraph_clusters$h3_address)) %>%
    filter(!is.na(h3_address) | h3_address != "") %>%
    filter(!is.na(h3_neighbour) | h3_neighbour != "") %>%
    unique() 
  
  ## Build the graph object
  i <- as_tbl_graph(hex_graph, directed = FALSE)
  i <- split(names(V(i)), components(i)$membership)
  
  ## Use graph membership to build tracts and ID's
  hex_Graph.N <- mclapply(as.list(1:length(i)), function(x) paste0("tractID_", names(i[x])), mc.cores = 12)
  i.graph <- as_tibble(do.call(rbind, mclapply(as.list(1:length(i)), function(x) cbind(hex_Graph.N[[x]], i[[x]]), mc.cores = 12)))
  names(i.graph) <- c("tractID", "h3_address")
  i.graph <- i.graph %>%
    filter(h3_address %in% unique(igraph_clusters$h3_address)) %>%
    select(tractID, h3_address) %>%
    filter(!is.na(tractID) | tractID!="") %>%
    filter(!is.na(h3_address) | h3_address!="") %>% 
    group_by(tractID) %>%
    add_count(name = "tractN") %>%
    unique()
  
  ## Format for out
  tracts <- as_tibble(i.graph)
  tracts <- tracts %>%
    select(tractID, h3_address)
  hexes_list <- unlist(tracts$h3_address, use.names = TRUE)
  hexes <- h3_to_polygon(hexes_list)
  hexes <- st_as_sf(hexes)
  hexes <- bind_cols(hexes, tracts)
  hexes <- hexes %>%
    rename(geometry = x) %>%
    select(h3_address, tractID, geometry)
  print("STAGE THREE COMPLETE - FINAL TRACTS EXTRACTED")
  
  ##################################################################################
  
  ## 4. Additional Parameters - Boundaries, Min_Pts ################################
  
  ## Extract boundaries for tracts 
  retail_centre_boundaries <- hexes %>%
    select(tractID) %>%
    group_by(tractID) %>%
    summarise()
  
  # Read in points
  pts <- read_points(state = identifier)
  pts <- st_transform(pts, 4326)

  ## Get count of pts in each rc
  pts_cl <- do.call(rbind, fast_intersection(pts, retail_centre_boundaries))
  pts_cl <- pts_cl %>%
    as.data.frame() %>%
    select(safegraph_place_id, tractID) %>%
    unique() %>%
    group_by(tractID) %>%
    add_count() %>%
    select(tractID, n) %>%
    rename(n.pts = n) %>%
    unique()
  boundaries <- merge(retail_centre_boundaries, pts_cl, by = "tractID", all.x = TRUE)
  
  ## Get df version to merge onto individual hexes
  boundaries_df <- boundaries %>%
    as.data.frame() %>%
    select(tractID, n.pts)
  
  ## Merge on pt counts
  hexes <- merge(hexes, boundaries_df, by = "tractID", all.x = TRUE)

  ## Assign new IDs based on identifier and hierarchy (n.pts)
  rcb_m <- boundaries %>%
    arrange(desc(n.pts))
  rcb_m$ID <- seq.int(nrow(rcb_m))
  rcb_m$ID <- as.character(with(rcb_m, paste(identifier,rcb_m$ID, sep = "")))
  out_boundaries <- rcb_m %>%
    select(ID, tractID, n.pts, geometry) %>%
    rename(rcID = ID)
  
  cl_info <- out_boundaries %>%
    as.data.frame() %>%
    select(rcID, tractID)
  rf_cl <- merge(hexes, cl_info, by = "tractID", all.x = TRUE)
  out_hexes <- rf_cl %>%
    select(rcID, tractID, n.pts, geometry) %>%
    arrange(rcID)
  
  ## Create list
  ls <- list(out_boundaries, out_hexes)
  return(ls)
  
}

## Function that takes two simple features objects and intersects them, using mclapply()
fast_intersect <- function(sf1, sf2) {
  
  gv <- rep(1:27, length.out = nrow(sf1))
  split <- split(sf1, gv)
  
  mclapply(split, function(x) {
    
    x[sf2, op = st_intersects]}, mc.cores = 27)
}

## Function that takes two simple features objects and returns the intersection, using mclapply()
fast_intersection <- function(sf1, sf2) {
  
  gv <- rep(1:27, length.out = nrow(sf1))
  split <- split(sf1, gv)
  
  mclapply(split, function(x) {
    
    st_intersection(x, sf2)}, mc.cores = 27)
  
}

# ## Boundary or not
# if (boundary == TRUE) {
#   print("STAGE FOUR COMPLETE - RETAIL CENTRES (INDIVIDUAL HEXAGONS) RETURNED")
#   return(rcb_m)
#   
#   
# } else if (boundary == FALSE) {
#   
#   ## Merging New ID's onto individual hexagons
#   cl_info <- rcb_m %>%
#     as.data.frame() %>%
#     select(rcID, tractID)
#   rf_cl <- merge(hexes, cl_info, by = "tractID", all.x = TRUE)
#   rf_cl <- rf_cl %>%
#     select(rcID, tractID, n.pts, geometry) %>%
#     arrange(rcID)
#   print("STAGE FIVE COMPLETE - RETAIL CENTRES (BOUNDARIES) RETURNED")
#   return(rf_cl)
#   
#   
# }

