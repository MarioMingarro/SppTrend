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
#'
#' @importFrom dplyr select mutate case_when
#' @importFrom tidyr pivot_wider
#'
#' @examples
#'
#'spp_trends_results <- data.frame(
#'   species = paste0("spp_", 1:10),
#'   response = rep("Lat", 10),
#'   trend = runif(10, -0.5, 0.5),
#'   t = runif(10, -2, 2),
#'   pvalue = runif(10, 0, 1),
#'   ci_95_max = runif(10, 0.05, 0.2),
#'   ci_95_min = runif(10, -0.1, 0.05),
#'   dif_t = runif(10, -1, 1.5),
#'   dif_pvalue = runif(10, 0.001, 0.9),
#'   n = runif(10, 40, 60))
#'spp <- unique(spp_trends_results$species)
#'bonferroni = 0.05/length(spp)
#'spp_strategy_results <- spp_strategy(spp_trends_results, bonferroni = 0.05)
#'
#' @export
#'

spp_strategy <- function(spp_trends, bonferroni = 0.05) {
  required_cols <- c("species",
                     "trend",
                     "t",
                     "pvalue",
                     "response",
                     "dif_t",
                     "dif_pvalue",
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
      names_from = response,
      values_from = c(trend, t, pvalue, dif_t, dif_pvalue),
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
      dplyr::mutate(Spatial_Lat = classify(pvalue_Lat, dif_pvalue_Lat, trend_Lat))
  }

  if ("pvalue_Lon" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Spatial_Lon = classify(pvalue_Lon, dif_pvalue_Lon, trend_Lon))
  }

  if ("pvalue_Ele" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Spatial_ele = classify(pvalue_Ele, dif_pvalue_Ele, trend_Ele))
  }

  # Clasificación térmica dinámica
  if ("pvalue_Tmx" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Thermal_Tmx = classify_thermal(pvalue_Tmx, dif_pvalue_Tmx, trend_Tmx))
  }

  if ("pvalue_Tmn" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Thermal_Tmn = classify_thermal(pvalue_Tmn, dif_pvalue_Tmn, trend_Tmn))
  }

  if ("pvalue_Tme" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Thermal_Tme = classify_thermal(pvalue_Tme, dif_pvalue_Tme, trend_Tme))
  }

  return(strategies)
}
