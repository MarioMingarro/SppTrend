#' @keywords internal
#' @aliases SppTrend
"_PACKAGE"

#' @title SppTrend: Analyzing Linear Trends in Species Occurrence Data
#'
#' @details
#' ## Methodology
#' `SppTrend` evaluates species-specific responses to climate change by
#' contrasting individual temporal trends against the overall trend estimated
#' from the complete dataset. This comparative approach explicitly accounts for
#' sampling bias shared across species within a taxonomic group.
#'
#' Species responses are decomposed into two complementary dimensions:
#'
#' - **Spatial**: Temporal shifts in geographic position (latitude and
#'   longitude), analysed jointly via Earth-Centred Earth-Fixed (ECEF) vector
#'   analysis.
#'
#' - **Environmental**: Temporal changes in temperature and elevation
#'   conditions associated with species occurrences.
#'
#' ## Response categories
#'
#' **Spatial** (from \code{\link{spp_trend_spatial_ecef}}):
#' - **SA** (Spatial Adaptation): movement faster than or directionally
#'   different from the global pool, in a poleward direction.
#' - **SD** (Spatial Discordance): movement slower than the global pool,
#'   or in a non-poleward direction.
#' - **SC** (Spatial Conformance): speed and direction indistinguishable
#'   from the global pattern.
#'
#' **Environmental** (from \code{\link{spp_trend_environmental}}):
#' - **TT** (Thermal Tolerance): significant positive trend in temperature.
#' - **TA** (Thermal Adjustment): significant negative trend in temperature.
#' - **TC** (Thermal Conformance): temperature trend not significantly
#'   different from the overall trend.
#' - **SA/SD/SC**: equivalent classes applied to elevation trends.
#'
#' ## Workflow
#' `SppTrend` provides a structured workflow for analysing these trends:
#'
#' 1. **Rapid diagnostic**: Quick visual check of spatial distribution and
#'    temperature trends using \code{\link{get_fast_info}}.
#'
#' 2. **Environmental data integration** (optional): Add ERA5 temperature
#'    values with \code{\link{get_era5_tme}} or elevation with
#'    \code{\link{get_elevation}}.
#'
#' 3. **Spatial trend analysis**: Estimate temporal changes in species
#'    geographic position using ECEF vector analysis with
#'    \code{\link{spp_trend_spatial_ecef}}.
#'
#' 4. **Environmental trend analysis**: Estimate temporal trends in
#'    temperature and elevation for each species using
#'    \code{\link{spp_trend_environmental}}.
#'
#' ## More details
#' Source code: \url{https://github.com/MarioMingarro/SppTrend}
#'
#' @name SppTrend-package
NULL
