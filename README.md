# SppTrend: Analyze Trends in Species Data

[![CRAN Status](https://www.r-pkg.org/badges/version/SppTrend)](https://cran.r-project.org/package=SppTrend)

**SppTrend** is an R package that provides a methodology to analyze trends in species occurrence data over time, with a particular focus on the influence of environmental factors such as temperature. This package facilitates the derivation of explanatory hypotheses about the effects of distribution changes in species assemblages, based on historical presence data that includes temporal and geographic information.

## Installation

You can install the released version of SppTrend from CRAN:

```{r}
install.packages("SppTrend")
```
Basic Usage
Here's a quick example demonstrating the use of the general_trend() function:
```{r}
library(SppTrend)

# Example data
data <- data.frame(
  year = 2000:2020,
  month = sample(1:12, 21, replace = TRUE),
  var1 = rnorm(21, mean = 10, sd = 2),
  var2 = rnorm(21, mean = 25, sd = 5)
)

# Combine year and month for a time predictor (example)
data$year_month <- data$year + data$month * (1/12)

# Analyze the trend of 'var1' over time
trend_result <- general_trend(
  Data = data,
  variables = c("var1", "var2"),
  predictors = c("year_month")
)

print(trend_result)
```
Overview of Key Features
The SppTrend package provides the following key functionalities:

Temperature Data Integration: The get_era5_tme() function allows users to obtain average temperature data for species occurrences from ERA5 datasets.
Overall Trend Analysis: The general_trend() function calculates the average trend (Overall Trend - OT) of different response variables across all species occurrences, providing a neutral reference point.
Species-Specific Trend Analysis: The spp_trend() function estimates the individual trend (SpeciesTrend - SppT) of different response variables for each species in the dataset.
Species Response Strategy Analysis: The spp_strategy() function compares individual species trends with the overall trend to identify specific response strategies (Spatial Adaptation, Spatial Discordance, Spatial Conformity, Thermal Tolerance, Thermal Adjustment, Thermal Conformity).
Methodology
The package follows a four-phase methodological approach:

Temperature Data Generation: Obtaining and integrating temperature data using get_era5_tme().
Overall Trend Estimation: Calculating the average trend across all occurrences using general_trend().
Individual Response Trend Estimation: Determining species-specific trends using spp_trend().
Species-Specific Response Analysis: Comparing individual trends with the overall trend to categorize species responses using spp_strategy().
Further Documentation
For more detailed information and examples, please refer to the package documentation within R:

```{r}
help(package = SppTrend)
# Or for a specific function:
help(general_trend)
```
Reference
This package is based on the methodology described in:

Contact
For any questions or issues, please feel free to contact:

Mario Mingarro Lopez
mario_mingarro@mncn.csic.es


## Estrategias de Respuesta Específicas

Basándonos en la documentación que proporcionaste, el paquete `SppTrend` identifica las siguientes estrategias de respuesta, divididas en espaciales y térmicas:

**Respuestas Espaciales:**

* **Adaptación Espacial (SA):** La presencia de la especie muestra una tendencia temporal positiva, que es significativamente diferente de la Tendencia General (OT). Esto sugiere que la especie está expandiendo su distribución en la dirección de la tendencia general.
* **Discordancia Espacial (SD):** La presencia de la especie muestra una tendencia temporal negativa, que es significativamente diferente de la Tendencia General (OT). Esto sugiere que la especie está contrayendo su distribución en la dirección opuesta a la tendencia general.
* **Conformidad Espacial (SC):** La presencia de la especie sigue una tendencia temporal similar a la Tendencia General (OT). Esto indica que la distribución de la especie está cambiando en sincronía con la tendencia general.

**Respuestas Térmicas:**

* **Tolerancia Térmica (TT):** La especie muestra una respuesta positiva a la temperatura a lo largo del tiempo, que es significativamente diferente de la Tendencia General (OT). Esto sugiere que la especie está favoreciendo condiciones de temperatura más altas con el tiempo.
* **Ajuste Térmico (TA):** La especie muestra una respuesta negativa a la temperatura a lo largo del tiempo, que es significativamente diferente de la Tendencia General (OT). Esto sugiere que la especie está favoreciendo condiciones de temperatura más bajas con el tiempo.
* **Conformidad Térmica (TC):** La especie sigue una tendencia térmica similar a la Tendencia General (OT). Esto indica que la respuesta de la especie a la temperatura está cambiando en sincronía con la tendencia general.


