# USRetailCentres

## American Retail Centre Geographies: Clusters, Characteristics and Catchments 

This repo contains all the code used for the paper: "American Retail Centre Geographies: Centres, Characteristics and Catchments". In this paper, using data from SafeGraph, OSM and USGS we develop a comprehensive classification of retail centre space for the national extent of the US. The paper has three sections/aims:

1. Retail Centre Delineation
2. A Typology of American Retail Centres
3. Retail Centre Catchments

To see an overview of the project, take a look at the slides for this repo, which were presented at the Virtual AAG 2021 Conference, available [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/AAG_2021_Slides.pptx)

---

### Part One - Retail Centre Delineation

To delineate the extents of American Retail Centres, we use a methodology that is based on the hexagonal spatial indexing system; H3. In essense the methodology aggregates retail locations to hexagons, and then using graph objects we are able to delineate contiguous tracts of Retail locations based on how they interact with other hexagons in the dataset.

The retail locations are derived using a number of datasets:

- SafeGraph Retail Places - *points*
- SafeGraph Retail Places Building Geometries - *polygons*
- OSM Retail Land-Use - *polygons*

All the necessary functions can be found [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/Source%20Code/Helper%20Functions%20-%20Delineation.R).

For improved performance - particularly useful with larger states (CA, NJ, NY), there is also a set of the same functions written to utilise parallelisation, for improved performance. These can be found in [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/Source%20Code/Helper%20Functions%20-%20Delineation%20(Parallel).R).


---

## Part Two - A Typology of American Retail 

To construct a typology of Retail Centres, we use unsupervised machine learning algorithm called PAM (partition around medoids) to identify groups of retail centres that share similar characteristics. 

The typology is currently constructed taking into account the following:

- Composition - proportions of Comparison, Convenience, Service and Leisure.
- Diversity - proportions of Independents vs National Chains, diversity of retail sub categories.
- Size & Function - number of retail locations, linearity of centres, surrounding road and building densities, employment densities.

At the moment, Geodemographics & Economic Performance have not yet been integrated, due to issues with data coverage and their wider integration into the typology.

All the necessary functions can be found [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/Source%20Code/Helper%20Functions%20-%20Typology.R).

For improved performance - particularly in extracting optimal k values and performing PCA, there is also a set of functions written to utilise parallelisation, for improved performance. These can be found [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/Source%20Code/Helper%20Functions%20-%20Typology%20(Parallel).R). 

---

## Part Three - Retail Centre Catchments



