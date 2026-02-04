#' @keywords internal
#' @aliases SppTrend
"_PACKAGE"

#' @title SppTrend: Analyzing Linear Trends in Species Occurrence Data
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
#' 1. **Rapid diagnostic and visual summary**: Perform a quick visual diagnostic of the input data
#'    using \code{\link{get_fast_info}}.
#'
#' 2. **Environmental data integration (optional)**: Enhance occurrence records
#'    with environmental context using functions like \code{\link{get_era5_tme}} (temperature)
#'    or \code{\link{get_elevation}} (elevation).
#'
#' 3. **Overall trend estimation**: Calculate the overall temporal trend (OT) of
#'    selected response variables across the entire dataset using \code{\link{overall_trend}}.
#'    This serves as a neutral reference against which species-specific temporal trends are evaluated
#'
#' 4. **Individual trend analysis**: Estimate the species-specific temporal trends for each selected response variable
#'    using \code{\link{spp_trend}}. This compares individual species' responses to the overall trend via interaction models.
#'
#' 5. **Ecological strategy classification**: Classify species into distinct spatial or thermal response categories based on the
#'    direction and statistical significance of their species-specific trends relative to the overall trend using \code{\link{spp_strategy}}.
#'
#' ## More details
#' Source code: \url{https://github.com/MarioMingarro/SppTrend}
#'
#' @name SppTrend-package
NULL
