# USRetailCentres

## The *Who, What* and *Where* of American Retail Centre Geographies.

This repo contains all the code used for the paper: The *Who, What* and *Where* of American Retail Centre Geographies. In this paper, using data from SafeGraph we develop a comprehensive understanding of the geographies of American retail centres. The paper has three sections/aims:

1. The '*Where*' - Delineating the spatial extent of American retail centre agglomerations.
2. The '*What*' - Developing a multidimensional American retail centre typology.
3. The '*Who*' - Building a calibrating Huff model to estimate retail centre catchments. 

The most recent update on this project can be found in the slides used to present at the 2021 RGS-IBG Annual International Conference. The slides are available [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/AAG_2021_Slides.pptx). 

---

### Part One - The '*Where*'

To delineate the spatial extents of American Retail Centres, we use a methodology that is based on the hexagonal spatial indexing system; H3. In essense the methodology aggregates retail locations to hexagons, and then using graph objects we are able to delineate contiguous tracts of retail locations based on how they interact with other hexagons in the dataset.

The retail locations are derived using a number of datasets:

- SafeGraph Retail Places - *points*
- SafeGraph Retail Places Building Geometries - *polygons*
- OSM Retail Land-Use - *polygons*

All the necessary functions can be found [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/Source%20Code/Helper%20Functions%20-%20Delineation.R).

For improved performance - particularly useful with larger states (CA, NJ, NY), there is also a set of the same functions written to utilise parallelisation, for improved performance. These can be found in [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/Source%20Code/Helper%20Functions%20-%20Delineation%20(Parallel).R).


---

## Part Two - The '*What*'

We adopt the framework first proposed by Dolega et al. (2019) in developing a multidimensional classification - the approach involves gathering a series of variables about the retail centres, dimensionality reduction and then clustering using PAM.

The typology accounts for four key domains of retail:

- Composition - proportions of different retail categories.
- Diversity - proportions of differential ownership (e.g. Independents), local/national category diversity.
- Size & Function - size, shape, density (retail, roads, residential, employment)
- Economic Health -  visit frequencies and dwell times, low income neighbourhoods.

All the necessary functions can be found [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/Source%20Code/Helper%20Functions%20-%20Typology.R).

For improved performance - particularly in extracting optimal k values and performing PCA, there is also a set of functions written to utilise parallelisation, for improved performance. These can be found [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/Source%20Code/Helper%20Functions%20-%20Typology%20(Parallel).R). 

---

## Part Three - The '*Who*'

To demarcate catchments for the centres we build a calibrated Huff model, utilising the SafeGraph 'weekly patterns' data - we use the data in calibrating the *alpha* and *beta* parameters of the model by extracting observed patronage probabilities for census block groups from the visitor_home_cbg variable, before fitting the model with attractiveness and distance as below:

- Attractiveness - size, diversity, total number of visits
- Distance - road network distances computed using the hereR API.

This section of the paper is still a work in progress, but working functions can be found [HERE](https://github.com/patrickballantyne/USRetailCentres/blob/main/Source%20Code/Helper%20Functions%20-%20Catchments.R)

