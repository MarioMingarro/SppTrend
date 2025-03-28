#' Creates a summary table of significant and spatial/thermal trends.
#'
#' This function takes a table of trend indicators 'spp_trends' and transforms it
#' into a summary table that includes information about significance, trends,
#' differences, and spatial/thermal classification of species.
#'
#' @param spp_trends A data frame containing trend indicators per species.
#'                   It is expected to contain columns such as:
#'                   - "species": Species names.
#'                   - "Trend": Trend values.
#'                   - "t": t-statistic values.
#'                   - "pvalue": p-values.
#'                   - "Response": Response variable (e.g., Lat, Tmax).
#'                   - "Dif_t": Difference in t-statistic values.
#'                   - "Dif_pvalue": Difference in p-values.
#'                   - "n": Sample size.
#' @param bonferroni The Bonferroni adjusted significance threshold value.
#'
#' @return A data frame summarizing significant trends and spatial/thermal
#'         classification of species.
#' @importFrom dplyr select mutate case_when
#' @importFrom tidyr pivot_wider
#'
#' @examples
#'# spp_trend_result <- spp_trend(Data, spp, predictor, responses, n_min = 50)
#'# bonferroni = 0.05 # Recommended (0.05/length(spp))
#'# strategies <- spp_strategy(spp_trend_result, bonferroni)
#' @export
#'

spp_strategy <- function(spp_trends, bonferroni = 0.05) {
  required_cols <- c("species",
                     "Trend",
                     "t",
                     "pvalue",
                     "Response",
                     "Dif_t",
                     "Dif_pvalue",
                     "n")

  # Verificar si todas las columnas requeridas están presentes
  if (!all(required_cols %in% names(spp_trends))) {
    missing_cols <- setdiff(required_cols, names(spp_trends))
    stop(
      paste(
        "Error: The following columns were not found in 'spp_trends':",
        paste(missing_cols, collapse = ", "),
        ". The required columns are:",
        paste(required_cols, collapse = ", ")
      )
    )
  }

  strategies <- spp_trends %>%
    dplyr::select(all_of(required_cols)) %>%
    tidyr::pivot_wider(
      names_from = Response,
      values_from = c(Trend, t, pvalue, Dif_t, Dif_pvalue),
      names_sep = "_"
    )

  # Función para clasificar espacial o térmicamente
  classify <- function(p, dif_p, trend) {
    case_when(
      p > bonferroni ~ "SC",
      p <= bonferroni & dif_p <= bonferroni & trend > 0 ~ "SA",
      p <= bonferroni & dif_p <= bonferroni & trend < 0 ~ "SD",
      TRUE ~ "SC"
    )
  }

  classify_thermal <- function(p, dif_p, trend) {
    case_when(
      p > bonferroni ~ "TC",
      p <= bonferroni & dif_p <= bonferroni & trend < 0 ~ "TA",
      p <= bonferroni & dif_p <= bonferroni & trend > 0 ~ "TT",
      TRUE ~ "TC"
    )
  }

  if ("pvalue_Lat" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Spatial_Lat = classify(pvalue_Lat, Dif_pvalue_Lat, Trend_Lat))
  }

  if ("pvalue_Long" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Spatial_Lon = classify(pvalue_Long, Dif_pvalue_Long, Trend_Long))
  }

  if ("pvalue_Elevation" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Spatial_ele = classify(pvalue_Elevation, Dif_pvalue_Elevation, Trend_Elevation))
  }

  # Clasificación térmica dinámica
  if ("pvalue_Tmx" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Thermal_Tmx = classify_thermal(pvalue_TMAX, Dif_pvalue_TMAX, Trend_TMAX))
  }

  if ("pvalue_Tmn" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Thermal_Tmn = classify_thermal(pvalue_TMIN, Dif_pvalue_TMIN, Trend_TMIN))
  }

  if ("pvalue_Tme" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Thermal_Tme = classify_thermal(pvalue_TMED, Dif_pvalue_TMED, Trend_TMED))
  }

  return(strategies)
}
