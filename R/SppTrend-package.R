#' @keywords internal
#' @aliases SppTrend
"_PACKAGE"

#' @title SppTrend: Analyzing linear trends in species occurrence data
#' @description
#' The `SppTrend` package provides a methodology to analyze how species occurrences
#' change over time, particularly in relation to geographic and environmental factors.
#' It facilitates the development of explanatory hypotheses about the impact of
#' environmental shifts on species assemblages by analyzing historical presence
#' data that includes temporal and geographic information.
#'
#' @details
#' ## Methodology
#' `SppTrend` assumes that observed species occurrences reflect a temporal sequence of changes in response to environmental drivers.
#'
#' The analysis uses:
#'
#' - **Predictors**: Sampling date (e.g., year or year-month decimals).
#'
#' - **Responses**: Geographic location (latitude and longitude) and environmental factors (elevation and temperature).
#'
#' ## Workflow
#' `SppTrend` provides a structured workflow for analyzing these trends:
#'
#' 1. **Environmental data integration (optional)**: Enhance occurrence records
#'    with environmental context using functions like \code{\link{get_era5_tme}} (temperature)
#'    or \code{\link{extract_elevation}} (elevation).
#'
#' 2. **Overall trend estimation**: Calculate the average temporal trend of
#'    selected response variables across the entire dataset using \code{\link{overall_trend}}.
#'    This establishes a baseline for comparison.
#'
#' 3. **Individual trend analysis**: Determine specific temporal trends for each
#'    species and response variable using \code{\link{spp_trend}}. This compares
#'    individual species' responses to the overall trend via interaction models.
#'
#' 4. **Ecological strategy classification**: Categorize species into distinct
#'    ecological strategies based on the significance and direction of their
#'    trends relative to the baseline using \code{\link{spp_strategy}}.
#'
#' ## More details
#' Project home and source code: \url{https://github.com/MarioMingarro/SppTrend}
#'
#' @name SppTrend-package
NULL
