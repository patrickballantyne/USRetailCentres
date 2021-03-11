# USRetailCentres

## American Retail Centre Geographies: Clusters, Characteristics and Catchments 

This repo contains all the code used for Paper Two of my PhD. In this paper, using data from SafeGraph, OSM and USGS we develop a comprehensive classification of retail centre space for the national extent of the US. The paper has three sections/aims:

1. Retail Centre Delineation
2. A Typology of American Retail Centres
3. Retail Centre Catchments


---

### Part One - Retail Centre Delineation

To delineate the extents of American Retail Centres, we use a methodology that is based on the hexagonal spatial indexing system; H3. In essense the methodology aggregates retail locations to hexagons, and then using graph objects we are able to delineate contiguous tracts of Retail locations based on how they interact with other hexagons in the dataset.

The retail locations are derived using a number of datasets:

- SafeGraph Retail Places - *points*
- SafeGraph Retail Places Building Geometries - *polygons*
- OSM Retail Land-Use - *polygons*

All the necessary functions can be found [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/Source%20Code/Helper%20Functions%20-%20Delineation.R), and you can follow this workflow to obtain a clustering with similar datasets:

- Assemble OSM retail landuse polygons - using the 'bb.list()' and 'get_osm_polygons()' functions.
- Convert all your retail locations datasets to H3 - using either the 'extract_state_h3()' function, or to assemble them individually you can use the 'points2hr()', 'lines2h3()', 'poly2h3()' and 'buildings2h3()' functions.
- Download the urban features (rails, roads, water) that you want to act as delimiters for your retail centre boundaries, and assemble them for the clustering using the 'get_urban_features()' function.
- Run the delineation - using the 'get_h3_clusters()' function, selecting the h3 resolution you want to use, the minimum number of points in a centre, the no. of krings and whether or not you want the boundary or individual hexagons returned.

For improved performance - particularly useful with larger states (CA, NJ, NY), there is also a set of the same functions written to utilise parallelisation, for improved performance. These can be found in [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/Source%20Code/Helper%20Functions%20-%20Delineation%20(Parallel).R), and you can use pretty much the same workflow as above to achieve the same results.


---

## Part Two - A Typology of American Retail 

---

## Part Three - Retail Centre Catchments



