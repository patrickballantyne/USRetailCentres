# USRetailCentres

## American Retail Centre Geographies: Clusters, Characteristics and Catchments 

This repo contains all the code used for Paper Two of my PhD. In this paper, using data from SafeGraph, OSM and USGS we develop a comprehensive classification of retail centre space for the national extent of the US. The paper has three sections/aims:

1. Delineation of Retail Centre Extents
2. Constructing a Typology of Retail Centres
3. Building Catchments for the Retail Centres


---

### Part One - Delineation of Retail Centre Extents 

To delineate the extents of American Retail Centres, we use a methodology that is based on the hexagonal spatial indexing system; H3. In essense the methodology aggregates retail locations to hexagons, and then using graph objects we are able to delineate contiguous tracts of Retail locations based on how they interact with other hexagons in the dataset.

The retail locations are derived using a number of datasets:

- SafeGraph Retail Places - *points*
- SafeGraph Retail Places Building Geometries - *polygons*
- OSM Retail Land-Use - *polygons*

All the necessary functions can be found in [Helper Functions.R].

