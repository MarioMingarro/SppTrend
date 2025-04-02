#' @title spp_strategy_poleward
#' @description Creates a summary table of significant and spatial/thermal trends,
#' incorporating poleward shift logic for latitude if hemisphere data is provided.
#' Assumes longitude trends may be based on coordinates with Antimeridian as 0.
#'
#' @param spp_trends A data frame containing trend indicators per species.
#'   It is expected to contain columns such as:
#'   - "species": Species names.
#'   - "trend": Trend values.
#'   - "t": t-statistic values.
#'   - "pvalue": p-values.
#'   - "response": Response variable (e.g., Lat, Lon, Tmax).
#'   - "dif_t": Difference in t-statistic values (e.g., from segmented regression).
#'   - "dif_pvalue": Difference in p-values (e.g., from segmented regression).
#'   - "n": Sample size.
#'   - "hemisphere": (Optional but required for poleward logic) Hemisphere info
#'                   ("N", "S", or "Both"). If absent, standard classification used.
#' @param bonferroni The Bonferroni adjusted significance threshold value.
#'
#' @return A data frame summarizing significant trends and spatial/thermal
#'   classification of species. Latitude classification will be 'Spatial_Lat_Poleward'
#'   if hemisphere info is provided and used. Longitude classification interpretation
#'   depends on whether input trends used transformed coordinates (Antimeridian=0).
#'
#' @importFrom dplyr select mutate case_when all_of lead
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#' spp_trends_results <- data.frame(
#'   species = paste0("spp_", 1:10),
#'   response = rep(c("Lat", "Lon"), each = 5),
#'   trend = runif(10, -0.5, 0.5),
#'   t = runif(10, -2, 2),
#'   pvalue = runif(10, 0, 0.1),
#'   ci_95_max = runif(10, 0.05, 0.2),
#'   ci_95_min = runif(10, -0.1, 0.05),
#'   dif_t = runif(10, -1, 1.5),
#'   dif_pvalue = runif(10, 0.001, 0.9),
#'   n = runif(10, 40, 60),
#'   hemisphere = sample(c("N", "S"), 10, replace = TRUE)
#' )
#' spp <- unique(spp_trends_results$species)
#' bonferroni_alpha = 0.05 / length(spp) # Correct Bonferroni calculation
#' spp_strategy_poleward_results <- spp_strategy_poleward(
#'   spp_trends_results,
#'   bonferroni = bonferroni_alpha
#'  )
#' print(spp_strategy_poleward_results)
#' }
#'
#' @export
spp_strategy_poleward <- function(spp_trends, bonferroni = 0.05) {

  required_cols <- c("species", "trend", "t", "pvalue", "response",
                     "dif_t", "dif_pvalue", "n", "hemisphere")

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
    dplyr::select(dplyr::all_of(intersect(required_cols, names(spp_trends)))) %>%
    tidyr::pivot_wider(
      names_from = response,
      values_from = c(trend, t, pvalue, dif_t, dif_pvalue),
      names_sep = "_"
    )
  classify_spatial_standard <- function(p, dif_p, trend) {
    dplyr::case_when(
      p > bonferroni ~ "SC",
      p <= bonferroni & dif_p <= bonferroni & trend > 0 ~ "SA",
      p <= bonferroni & dif_p <= bonferroni & trend < 0 ~ "SD",
      p <= bonferroni & trend > 0 ~ "SA_nsd",
      p <= bonferroni & trend < 0 ~ "SD_nsd",
      TRUE ~ "SC"
    )
  }

  classify_lat_poleward <- function(p, dif_p, trend, hemisphere) {
    dplyr::case_when(
      p > bonferroni ~ "SC",
      p <= bonferroni & dif_p <= bonferroni & ((hemisphere == "N" & trend > 0) | (hemisphere == "S" & trend < 0)) ~ "SP", # Sig Poleward
      p <= bonferroni & dif_p <= bonferroni & ((hemisphere == "N" & trend < 0) | (hemisphere == "S" & trend > 0)) ~ "SE", # Sig Equatorward
      p <= bonferroni & ((hemisphere == "N" & trend > 0) | (hemisphere == "S" & trend < 0)) ~ "SP_nsd",
      p <= bonferroni & ((hemisphere == "N" & trend < 0) | (hemisphere == "S" & trend > 0)) ~ "SE_nsd",
      hemisphere == "Both" ~ "SC_Both",
      TRUE ~ "SC_Unknown"
    )
  }

  classify_thermal <- function(p, dif_p, trend) {
    dplyr::case_when(
      p > bonferroni ~ "TC",
      p <= bonferroni & dif_p <= bonferroni & trend < 0 ~ "TA", # Thermal Adaptation
      p <= bonferroni & dif_p <= bonferroni & trend > 0 ~ "TT", # Thermal Tracking
      p <= bonferroni & trend < 0 ~ "TA_nsd",
      p <= bonferroni & trend > 0 ~ "TT_nsd",
      TRUE ~ "TC"
    )
  }


  if ("pvalue_Lat" %in% names(strategies)) {
    if (has_hemisphere_col && "hemisphere" %in% names(strategies)) {
      strategies <- strategies %>%
        dplyr::mutate(Spatial_Lat_Poleward = classify_lat_poleward(
          .data$pvalue_Lat, .data$dif_pvalue_Lat, .data$trend_Lat, .data$hemisphere
        )
        )
    } else {
      strategies <- strategies %>%
        dplyr::mutate(Spatial_Lat_Standard = classify_spatial_standard(
          .data$pvalue_Lat, .data$dif_pvalue_Lat, .data$trend_Lat
        )
        )
    }
  }

  if ("pvalue_Lon" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Spatial_Lon = classify_spatial_standard(
        .data$pvalue_Lon, .data$dif_pvalue_Lon, .data$trend_Lon
      )
      )
  }

  if ("pvalue_Ele" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Spatial_Ele = classify_spatial_standard(
        .data$pvalue_Ele, .data$dif_pvalue_Ele, .data$trend_Ele
      )
      )
  }

  if ("pvalue_Tmx" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Thermal_Tmx = classify_thermal(
        .data$pvalue_Tmx, .data$dif_pvalue_Tmx, .data$trend_Tmx
      )
      )
  }
  if ("pvalue_Tmn" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Thermal_Tmn = classify_thermal(
        .data$pvalue_Tmn, .data$dif_pvalue_Tmn, .data$trend_Tmn
      )
      )
  }
  if ("pvalue_Tme" %in% names(strategies)) {
    strategies <- strategies %>%
      dplyr::mutate(Thermal_Tme = classify_thermal(
        .data$pvalue_Tme, .data$dif_pvalue_Tme, .data$trend_Tme
      )
      )
  }

   strategies <- strategies %>% dplyr::select(species, n, starts_with("Spatial"), starts_with("Thermal"), everything())

  return(strategies)
}
