#' @title Individual trend analysis
#' @description This function fits a linear model to analyze individual trends over time, comparing with a general data trend (`overall_trend`),
#' and includes longitude transformation to handle the antimeridian and hemisphere detection. For species distributed across both hemispheres, it also compares their overall trend
#' with the global data trend. The comparison with the general trend is assessed using an interaction term in the linear model.
#'
#' @param data A `data frame` containing the variables for the model, including 'species', 'year', 'month', 'lon', 'lat', 'tmx' and/or similar.
#' @param predictor A `character`vector of predictor variable names representing a temporal variable (e.g., "year_month").
#' @param responses A `character` vector of response variable names to analyze (e.g., "lat"  for spatial trends, "tmx" for thermal trends).
#' @param spp A `character` vector of unique species names.
#' @param n_min Minimum `numeric` number of presences required for a species in each hemisphere (or globally for species in both hemispheres) to perform the analysis.
#'
#' @return A data frame with trend statistics, including:
#'    - `species`: Species name.
#'    - `responses`: The name of the variable analyzed.
#'    - `trend`: The slope of the linear model, indicating the direction and magnitude of the trend.
#'    - `t`: The t-statistic of the model, used to assess the significance of the trend.
#'    - `pvalue`: The p-value of the trend, indicating the probability of observing such a trend if there was no actual trend.
#'    - `ci_95_max`: Upper bound of the 95% confidence interval for the trend.
#'    - `ci_95_min`: Lower bound of the 95% confidence interval for the trend.
#'    - `dif_t`: Difference in t-statistic values (comparison of species trend to general trend).
#'    - `dif_pvalue`: Difference in p-values (comparison of species trend to general trend). A low p-value here suggests the species trend is significantly different from the general trend.
#'    - `n`: Number of datapoints used for the specific species/hemisphere trend.
#'    - `hemisphere`: Detected hemisphere ("North", "South", or "Both" for global comparison).
#'
#' @details The function analyzes trends for each species in the provided data,
#'    considering different response variables and a specified temporal predictor.
#'    It accounts for the antimeridian issue when analyzing longitude trends by
#'    transforming longitude values. The analysis is performed separately
#'    for each hemisphere to capture potential differences in ecological responses.
#'    The function compares the trend of each species within a hemisphere with a
#'    general trend calculated from the entire hemisphere's data. For species
#'    found in both the Northern and Southern Hemispheres, it additionally compares
#'    the trend of the species (using all occurrences) with the trend of the entire
#'    dataset (all hemispheres combined), providing a global perspective on the
#'    species' trend. Warnings are generated if a species has insufficient data
#'    (below `n_min`) in a given hemisphere or globally, or if errors occur during
#'    the linear model fitting process. This function typically follows the
#'    calculation of an overall trend using a function like `overall_trend()`.
#'
#' @importFrom stats as.formula confint formula lm coef
#' @importFrom dplyr %>% mutate filter
#'
#' @examples
#'
#' data <- data.frame(
#'    species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
#'    year = sample(1950:2020, 500, replace = TRUE),
#'    month = sample(1:12, 500, replace = TRUE),
#'    lon = runif(500, -10, 20),
#'    lat = runif(500, 30, 70),
#'    tme = rnorm(500, 15, 10)
#' )
#'
#' data$year_month <- data$year + data$month * 0.075
#'
#' predictor <- "year_month"
#' responses <- c("lat", "lon", "tme")
#'
#' spp <- unique(data$species)
#'
#' spp_trend_result <- spp_trend(data, spp, predictor, responses, n_min = 50)
#'
#' print(head(spp_trend_result))
#'
#' # Example interpretation: A positive 'trend' for 'lat' might indicate a
#' # northward shift in the Northern Hemisphere. A low 'pvalue' (e.g., < 0.05)
#' # suggests this trend is statistically significant. A low 'dif_pvalue'
#' # might indicate this trend is significantly different from the general trend.
#'
#' @export
#'
spp_trend <- function(data, spp, predictor, responses, n_min = 50) {
  data$hemisphere <- ifelse(data$lat >= 0, "North", "South")
  results_list <- list()
  for (n in 1:length(spp)) {
    ind <- data[data$species == spp[n], ]
    hemispheres_present <- unique(ind$hemisphere)
    ind_list <- split(ind, f = ind$hemisphere)
    for (h in names(ind_list)) {
      ind_hemisphere <- ind_list[[h]]
      data_hemisphere <- data[data$hemisphere == h, ]
      if (nrow(ind_hemisphere) > n_min) {
        for (i in 1:length(responses)) {
          tryCatch({
            current_resp <- responses[i]
            if (current_resp == "lon") {
              val_ind <- (ind_hemisphere$lon + 180) %% 360
              val_gen <- (data_hemisphere$lon + 180) %% 360
            } else {
              val_ind <- ind_hemisphere[[current_resp]]
              val_gen <- data_hemisphere[[current_resp]]
            }
            if (length(unique(ind_hemisphere[[predictor]])) > 1) {
              df_ind <- data.frame(y = val_ind, time = ind_hemisphere[[predictor]], group = "i")
              df_gen <- data.frame(y = val_gen, time = data_hemisphere[[predictor]], group = "g")
              dat_model <- rbind(df_ind, df_gen)
              model_i <- lm(y ~ time, data = df_ind)
              model_int <- lm(y ~ time * group, data = dat_model)
              sum_i <- summary(model_i)$coefficients
              sum_int <- summary(model_int)$coefficients
              ci <- confint(model_i, "time", level = .95)
              int_term <- "time:groupi"
              results_list[[length(results_list) + 1]] <- data.frame(
                species = spp[n],
                responses = current_resp,
                trend = coef(model_i)[2],
                t = sum_i[2, 3],
                pvalue = sum_i[2, 4],
                ci_95_max = ci[1, 2],
                ci_95_min = ci[1, 1],
                dif_t = if (int_term %in% rownames(sum_int)) sum_int[int_term, 3] else NA,
                dif_pvalue = if (int_term %in% rownames(sum_int)) sum_int[int_term, 4] else NA,
                n = nrow(ind_hemisphere),
                hemisphere = h,
                stringsAsFactors = FALSE
              )
            }
          }, error = function(e) {
            message(paste("Error en", spp[n], "-", responses[i], "(", h, "):", e$message))
          })
        }
      }
    }
    if (all(c("North", "South") %in% hemispheres_present)) {
      if (nrow(ind) > n_min) {
        for (i in 1:length(responses)) {
          tryCatch({
            current_resp <- responses[i]
            if (length(unique(ind[[predictor]])) > 1) {
              if (current_resp == "lon") {
                val_ind_g <- (ind$lon + 180) %% 360
                val_gen_g <- (data$lon + 180) %% 360
              } else {
                val_ind_g <- ind[[current_resp]]
                val_gen_g <- data[[current_resp]]
              }
              df_ind_g <- data.frame(y = val_ind_g, time = ind[[predictor]], group = "i")
              df_gen_g <- data.frame(y = val_gen_g, time = data[[predictor]], group = "g")
              dat_global <- rbind(df_ind_g, df_gen_g)
              model_i_g <- lm(y ~ time, data = df_ind_g)
              model_int_g <- lm(y ~ time * group, data = dat_global)
              sum_i_g <- summary(model_i_g)$coefficients
              sum_int_g <- summary(model_int_g)$coefficients
              ci_g <- confint(model_i_g, "time", level = .95)
              results_list[[length(results_list) + 1]] <- data.frame(
                species = spp[n],
                responses = current_resp,
                trend = coef(model_i_g)[2],
                t = sum_i_g[2, 3],
                pvalue = sum_i_g[2, 4],
                ci_95_max = ci_g[1, 2],
                ci_95_min = ci_g[1, 1],
                dif_t = if ("time:groupi" %in% rownames(sum_int_g)) sum_int_g["time:groupi", 3] else NA,
                dif_pvalue = if ("time:groupi" %in% rownames(sum_int_g)) sum_int_g["time:groupi", 4] else NA,
                n = nrow(ind),
                hemisphere = "Both",
                stringsAsFactors = FALSE
              )
            }
          }, error = function(e) {
            message(paste("Error global en", spp[n], "-", responses[i], ":", e$message))
          })
        }
      }
    }
  }
  if (length(results_list) > 0) return(do.call(rbind, results_list))
  else return(data.frame())
}
