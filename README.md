# SppTrend: Analyzing Linear Trends in Species Occurrence Data

[![CRAN Status](https://www.r-pkg.org/badges/version/SppTrend)](https://cran.r-project.org/package=SppTrend)

![Estrategias](title.png)


`SppTrend` is an R package that provides a methodology to analyze trends in species occurrence data over time, with a particular focus on the influence of environmental factors such as temperature. This package facilitates the derivation of explanatory hypotheses about the effects of distribution changes in species assemblages, based on historical presence data that includes temporal and geographic information.

## Installation

You can install the released version of SppTrend from CRAN:

```{r}
install.packages("SppTrend")
```
Alternatively, you can install the development version from GitHub:
```{r}
install.packages("devtools")
library(devtools)
devtools::install_github("MarioMingarro/SppTrend")
```

## Overview of Key Features

The `SppTrend` package provides a methodology to derive explanatory hypotheses about the effects of distribution changes in species assemblages. 
It attempts to help researchers understand species responses to environmental change by analyzing historical occurrence data includes:

  - **Predictors** - Sampling date (year and preferably month and year)

  - **Responses** - Geographic location (latitude and longitude) and environmental factors (elevation and temperature).

The methodology assumes that the observed species occurrences represent a temporal sequence reflecting changes in their responses to these factors.



The `SppTrend` package provides a structured workflow for analyzing trends in species occurrence data:

1.  **Environmental Data Integration (Optional)**: Utilize functions like `get_era5_tme()` to incorporate temperature data and `get_dem_ele()` to add elevation information to your occurrence records. This step enriches the dataset with environmental context.
2.  **Overall Trend Estimation**: Employ the `overall_trend()` function to calculate the average temporal trend of the selected response variables across all species in your dataset. This provides a general baseline for comparison.
3.  **Individual Trend Analysis**: Use the `spp_trend()` function to determine the individual temporal trends for each species and response variable. This step compares individual species' responses with the overall trends.
4.  **Ecological Strategy Classification**: Apply the `spp_strategy()` function to categorize species into distinct ecological strategies based on the significance and direction of their individual trends relative to the overall trend.

### Detailed Steps

The `SppTrend` package is designed to analyze species presence records. To utilize the package effectively, your dataset must, at a minimum, include the following information for each occurrence:

* Species identification (e.g., 'species').
* Geographic coordinates: Latitude (e.g., 'Lat') and Longitude (e.g., 'Lon').
* Temporal information: Year (e.g., 'year'), and preferably Month (e.g., 'month').

Given its potential for global applications, the package assumes the use of the WGS84 (World Geodetic System 1984) coordinate reference system.

**It is essential that the column names in your input dataset either match the default names expected by the `SppTrend` functions (e.g., 'species', 'year', 'month', 'Lon', 'Lat', and environmental response variables such as 'Ele', 'Tme', 'Tmx', or 'Tmn').**

The following is an example illustrating the structure of a data frame containing 500 randomly generated presence records for 10 distinct species:

```r
data <- data.frame(
  species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
  year = sample(1950:2020, 500, replace = TRUE),
  month = sample(1:12, 500, replace = TRUE),
  lon = runif(500, -10, 20),
  lat = runif(500, 30, 70)
)
```

### Phase 1: Environmental Data Generation

The `SppTrend` package offers functions to enhance your species occurrence data with relevant environmental information. Currently, it supports the integration of temperature and elevation data.

#### ERA5 Temperature Data

ERA5 is the fifth generation European Centre for Medium-Range Weather Forecasts (ECMWF) reanalysis dataset for the global climate and weather. It provides comprehensive atmospheric, land, and ocean climate data from 1940 to the present, with high spatial and temporal resolution. This makes it a valuable resource for studying the impact of climate on species distributions.
You can explore the ERA5 daily statistics dataset on the Copernicus Climate Change Service (C3S) Climate Data Store (CDS) at: [ERA5 Daily Statistics Overview](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means). For more detailed information about the ERA5 dataset, please visit the [ECMWF website](https://confluence.ecmwf.int/display/CKB/The+family+of+ERA5+datasets).

The `SppTrend` package provides the following function to incorporate ERA5 temperature data:

`get_era5_tme()`: Allows users to obtain average temperature data (mean temperature of the environment) for species occurrences using ERA5 reanalysis data. 

*Notes: ERA5 data is available from 1940 onwards. The data must be in `.netcdf` format.*
**Example Usage:**

```{r}
nc_file <- "path/to/your/era5_data.nc"
data <- get_era5_tme(data, nc_file, month_col = "month")
```
#### Digital Elevation Model (DEM) Data

`get_dem_ele()`: This function can be used to retrieve Digital Elevation Model (DEM) data for the species occurrences, providing information about the elevation at which the species were recorded.
For obtaining elevation data for species occurrences, this example utilizes the WorldClim dataset ([WorldClim](https://www.worldclim.org/data/worldclim21.html)). However, users are encouraged to consider other Digital Elevation Models (DEMs) based on the specific resolution requirements of their analysis. For instance, the [EU-DEM dataset](https://dataspace.copernicus.eu/explore-data/data-collections/copernicus-contributing-missions/collections-description/COP-DEM) provides high-resolution elevation data for Europe.

Furthermore, it is highly recommended to utilize any existing elevation data already present within the user's occurrence dataset. This allows for a direct comparison and validation of the retrieved elevation values, potentially improving the accuracy and reliability of the analysis.
*Notes: DEM data must be in `.tif` format.*
```{r}
dem_file <- "path/to/your/dem.tif"
data <- get_dem_ele(data, dem_file)
```
### Phase 2: Estimation of the Overall Trend of Responses

The `overall_trend()` function calculates the Overall Trend (OT) for specified response variables across the entire dataset. 
This trend serves as a neutral reference to evaluate individual species' responses. It's important to consider potential biases in the data when interpreting the OT.

```{r}
overall_trend_result <- overall_trend(data, responses, predictor)
```

### Phase 3: Estimation of Individual Trends of Responses

The `spp_trend()` function calculates the individual temporal trend for each species and response variable, comparing it to the general trend observed in the data. It also handles longitude transformations and considers hemisphere-specific trends.

```{r}
general_trend_result <- spp_trend(data, spp, predictor, responses, n_min = 50)
```

### Phase 4: Analysis of Specific Species Responses

The `spp_strategy()` function analyzes the results from `spp_trend()` to classify species into different ecological strategies based on the significance and direction of their trends relative to the overall trend. 
This function incorporates logic for poleward shifts in latitude based on hemisphere and can also classify trends in elevation.

```{r}
spp_strategy_results <- spp_strategy(spp_trends_results, sig_level = 0.05, responses = c("Lat", "Lon", "Ele", "Tmx"))
```

### Ecological strategies

The `SppTrend` package identifies several spatial and thermal response strategies:

![Estrategias](strategies.png)

**Spatial Responses**

  - **Spatial Adaptation (SA)**: The species' presence shows a positive temporal trend, significantly different from the OT. 
 
  - **Spatial Discordance (SD)**: The species' presence shows a negative temporal trend, significantly different from the OT.

  - **Spatial Conformity (SC)**: The species' presence follows a temporal trend similar to the OT.

**Thermal Responses**

  - **Thermal Tolerance (TT)**: The species shows a positive response to temperature over time, significantly different from the OT.

  - **Thermal Conformity (TC)**: The species follows a thermal trend similar to the OT. 

  - **Thermal Adjustment (TA)**: The species shows a negative response to temperature over time, significantly different from the OT. 

**World Latitudinal Responses**
  - **Spatial Poleward (SP)**: This represents a significant positive temporal trend, indicating a shift towards higher latitudes (the pole in the Northern Hemisphere). 
  Also, it represents a significant negative temporal trend, also indicating a shift towards lower latitudes (the pole in Southern Hemisphere).
  - **Spatial Equatorward (SE)**: This represents a significant negative temporal trend, indicating a shift towards lower latitudes (the equator in the Northern Hemisphere). 
  Also, it represents a significant positive temporal trend, also indicating a shift towards higher latitudes (the equator in Southern Hemisphere).


### Applications and Limitations
`SppTrend` is a valuable tool for researchers investigating the impacts of environmental change on biodiversity. 
However, it's crucial to interpret results cautiously, considering potential biases inherent in species occurrence data. 
The overall trend serves as a reference but might not always represent the absolute species responses to warming or other environmental changes.

For more detailed information and examples, please refer to the package documentation within R:

```{r}
help(package = SppTrend)
# Or for a specific function:
help(general_trend)
```
## References
This package is based on the methodology described in:

Jorge M. Lobo, Mario Mingarro, Martin Godefroid, Emilio García-Roselló 2023. Taking advantage of opportunistically collected historical occurrence data to detect responses to climate change: The case of temperature and Iberian dung beetles. *Ecology and evolution*, 13(12) e10674. https://doi.org/10.1002/ece3.10674 

## Contact
For any questions or issues, please feel free to contact:

Mario Mingarro Lopez
mario_mingarro@mncn.csic.es

Jorge M. Lobo 
mcnj117@mncn.csic.es

Emilio García-Roselló 
egrosello@esei.uvigo.es
