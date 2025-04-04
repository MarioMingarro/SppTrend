#' @title spp_trend_world
#' @description This function fits a linear model to analyze individual trends
#'   over time, comparing with general data trend, and includes longitude
#'   transformation and hemisphere detection.
#'
#' @param Data A data frame containing the variables for the model, including
#'   'species', 'year', 'month', 'Lon', 'Lat', 'Tmx', 'Tmn'.
#' @param responses A vector of response variable names (e.g., "Lat", "Lon",
#'   "Tmx").
#' @param predictor A vector of predictor variable names (e.g., "year_month").
#' @param spp A vector of species names.
#' @param n_min Minimum number of presences required for a species in each
#'   hemisphere.
#'
#' @return A data frame with trend statistics, including:
#'   - `species`: Species name.
#'   - `responses`: The name of the variable analyzed.
#'   - `trend`: The slope of the linear model.
#'   - `t`: The t-statistic of the model.
#'   - `pvalue`: The p-value of the trend.
#'   - `ci_95_max`: Upper bound of the 95% confidence interval.
#'   - `ci_95_min`: Lower bound of the 95% confidence interval.
#'   - `dif_t`: Difference in t-statistic values (comparison of species trend to general trend).
#'   - `dif_pvalue`: Difference in p-values (comparison of species trend to general trend).
#'   - `n`: Number of datapoints used for the specific species/hemisphere trend.
#'   - `hemisphere`: Detected hemisphere ("North" or "South").
#'
#' @details The function analyzes trends for each species in the provided data,
#'   considering different response variables and a specified predictor. It
#'   accounts for the antimeridian issue when analyzing longitude trends and
#'   separates the analysis by hemisphere. It also compares the trend of each
#'   species with a general trend calculated from the entire hemisphere's data.
#'
#' @importFrom stats as.formula confint formula lm coef summary
#' @importFrom dplyr %>% mutate filter
#'
#' @examples
#' \dontrun{
#' Data <- data.frame(
#'   species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
#'   year = sample(1900:2024, 500, replace = TRUE),
#'   month = sample(1:12, 500, replace = TRUE),
#'   Lon = runif(500, -10, 20),
#'   Lat = runif(500, 30, 70),
#'   Tmx = rnorm(500, 15, 10),
#'   Tmn = rnorm(500, 10, 8)
#' )
#' Data$year_month <- Data$year + Data$month * 0.075
#' predictor <- "year_month"
#' responses <- c("Lat", "Lon", "Tmx", "Tmn")
#' spp <- unique(Data$species)
#' general_trend_result <- spp_trend_world(Data, spp, predictor, responses, n_min = 50)
#' print(spp_trend_world_results)
#' }
#'
#' @export
spp_trend_world <- function(Data, spp, predictor, responses, n_min = 50) {
  transform_lon_antimeridian <- function(lon) {
    lon_transformed <- ifelse(lon > 180, lon - 360, ifelse(lon < -180, lon + 360, lon))
    return(lon_transformed)
  }
  Data$hemisphere <- ifelse(Data$Lat >= 0, "North", "South")
  results_list <- list()
  for (n in 1:length(spp)) {
    ind <- Data[Data$species == spp[n], ]
    ind_list <- split(ind, f = ind$hemisphere)
    for (h in names(ind_list)) {
      #"North" "South"
      ind_hemisphere <- ind_list[[h]]
      data_hemisphere <- Data[Data$hemisphere == h, ]
      if (nrow(ind_hemisphere) > n_min) {
        for (i in 1:length(responses)) {
          tryCatch({
            if (nrow(ind_hemisphere) > 0 && nrow(data_hemisphere) > 0) {
              ind_hemisphere$group <- "i"
              data_hemisphere$group <- "g"
              dat <- rbind(data_hemisphere, ind_hemisphere)
              if (responses[i] == "Lon") {
                ind_hemisphere$Lon_transformed <- transform_lon_antimeridian(ind_hemisphere$Lon)
                dat$Lon_transformed <- transform_lon_antimeridian(dat$Lon)
                model_i <- lm(formula(paste(
                  "Lon_transformed",
                  paste(predictor, collapse = "+"),
                  sep = " ~ "
                )), data = ind_hemisphere)
                model_int <- lm(formula(paste(
                  "Lon_transformed",
                  paste(predictor, "*group", collapse = "+"),
                  sep = " ~ "
                )), data = dat)
              } else {
                model_i <- lm(formula(paste(
                  responses[i],
                  paste(predictor, collapse = "+"),
                  sep = " ~ "
                )), data = ind_hemisphere)
                model_int <- lm(formula(paste(
                  responses[i],
                  paste(predictor, "*group", collapse = "+"),
                  sep = " ~ "
                )), data = dat)
              }
              trend <- coef(model_i)[2]
              t_value <- summary(model_i)$coefficients[2, 3]
              p_value <- summary(model_i)$coefficients[2, 4]
              ci <- confint(model_i, predictor, level = .95)[, ]
              dif_t <- summary(model_int)$coefficients[4, 3]
              dif_p <- summary(model_int)$coefficients[4, 4]
              if (responses[i] == "Lat" && h == "South") {
                trend <- trend
              }
              results_list[[length(results_list) + 1]] <- data.frame(
                species = spp[n],
                responses = responses[i],
                trend = trend,
                t = t_value,
                pvalue = p_value,
                ci_95_max = ci[2],
                ci_95_min = ci[1],
                dif_t = dif_t,
                dif_pvalue = dif_p,
                n = nrow(ind_hemisphere),
                hemisphere = h
              )
            } else {
              cat(
                paste0(
                  "WARNING: Specie ",
                  spp[n],
                  " response (",
                  responses[i],
                  ") has insufficient data (",
                  nrow(ind_hemisphere),
                  ").\n"
                )
              )
            }
          }, error = function(e) {
            cat(
              paste0(
                "WARNING: Specie ",
                ind_hemisphere[1, 1],
                " responses (",
                responses[i],
                ") has error: ",
                conditionMessage(e),
                "\n"
              )
            )
          })
        }
      } else {
        print(paste0(
          "WARNING: Specie ",
          spp[n],
          " has few data (",
          nrow(ind_hemisphere),
          ")in ",
          h,
          " hemisphere"
        ))
      }
    }
  }
  if (length(results_list) > 0) {
    spp_trend_result <- do.call(rbind, results_list)
  } else {
    spp_trend_result <- data.frame()
  }
  return(spp_trend_result)
}
