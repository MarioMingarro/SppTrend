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
            if (nrow(ind_hemisphere) > 0 && nrow(data_hemisphere) > 0) {
              if (length(unique(ind_hemisphere[[predictor]])) > 1) {
                ind_hemisphere$group <- "i"
                data_hemisphere$group <- "g"
                dat <- rbind(data_hemisphere, ind_hemisphere)
                if (responses[i] == "lon") {
                  ind_hemisphere$lon_transformed <- (ind_hemisphere$lon + 180) %% 360
                  dat$lon_transformed <- (dat$lon + 180) %% 360
                  model_i <- lm(formula(paste(
                    "lon_transformed",
                    paste(predictor, collapse = "+"),
                    sep = " ~ "
                  )), data = ind_hemisphere)
                  model_int <- lm(formula(paste(
                    "lon_transformed",
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
                interaction_term <- paste0(predictor, ":groupi")
                dif_t <- if (interaction_term %in% rownames(summary(model_int)$coefficients)) {
                  summary(model_int)$coefficients[interaction_term, 3]
                } else {
                  NA
                }
                dif_p <- if (interaction_term %in% rownames(summary(model_int)$coefficients)) {
                  summary(model_int)$coefficients[interaction_term, 4]
                } else {
                  NA
                }
                if (responses[i] == "lat" && h == "South") {
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
                    ") in ",
                    h,
                    " hemisphere has insufficient variation in predictor (",
                    predictor,
                    ").\n"
                  )
                )
              }
            } else {
              cat(
                paste0(
                  "WARNING: Specie ",
                  spp[n],
                  " response (",
                  responses[i],
                  ") has insufficient data (",
                  nrow(ind_hemisphere),
                  ") in ",
                  h,
                  " hemisphere.\n"
                )
              )
            }
          }, error = function(e) {
            cat(
              paste0(
                "WARNING: Specie ",
                if (nrow(ind_hemisphere) > 0)
                  ind_hemisphere[1, 1]
                else
                  spp[n],
                " responses (",
                responses[i],
                ") in ",
                h,
                " hemisphere has error: ",
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
          ") in ",
          h,
          " hemisphere"
        ))
      }
    }

    if (all(c("North", "South") %in% hemispheres_present)) {
      for (i in 1:length(responses)) {
        tryCatch({
          if (nrow(ind) > n_min) {
            if (length(unique(ind[[predictor]])) > 1) {
              data$group_global <- "g"
              ind$group_global <- "i"
              dat_global <- rbind(data, ind)

              if (responses[i] == "lon") {
                ind$lon_transformed <- (ind$lon + 180) %% 360
                dat_global$lon_transformed <- (dat_global$lon + 180) %% 360
                model_i_global <- lm(formula(paste(
                  "lon_transformed",
                  paste(predictor, collapse = "+"),
                  sep = " ~ "
                )), data = ind)
                model_int_global <- lm(formula(paste(
                  "lon_transformed",
                  paste(predictor, "*group_global", collapse = "+"),
                  sep = " ~ "
                )), data = dat_global)
              } else {
                model_i_global <- lm(formula(paste(
                  responses[i],
                  paste(predictor, collapse = "+"),
                  sep = " ~ "
                )), data = ind)
                model_int_global <- lm(formula(paste(
                  responses[i],
                  paste(predictor, "*group_global", collapse = "+"),
                  sep = " ~ "
                )), data = dat_global)
              }

              trend_global <- coef(model_i_global)[2]
              t_value_global <- summary(model_i_global)$coefficients[2, 3]
              p_value_global <- summary(model_i_global)$coefficients[2, 4]
              ci_global <- confint(model_i_global, predictor, level = .95)[, ]
              interaction_term_global <- paste0(predictor, ":group_globali")
              dif_t_global <- if (interaction_term_global %in% rownames(summary(model_int_global)$coefficients)) {
                summary(model_int_global)$coefficients[interaction_term_global, 3]
              } else {
                NA
              }
              dif_p_global <- if (interaction_term_global %in% rownames(summary(model_int_global)$coefficients)) {
                summary(model_int_global)$coefficients[interaction_term_global, 4]
              } else {
                NA
              }

              results_list[[length(results_list) + 1]] <- data.frame(
                species = spp[n],
                responses = responses[i],
                trend = trend_global,
                t = t_value_global,
                pvalue = p_value_global,
                ci_95_max = ci_global[2],
                ci_95_min = ci_global[1],
                dif_t = dif_t_global,
                dif_pvalue = dif_p_global,
                n = nrow(ind),
                hemisphere = "Both"
              )
            } else {
              cat(
                paste0(
                  "WARNING: Specie ",
                  spp[n],
                  " response (",
                  responses[i],
                  ") has insufficient variation in predictor (",
                  predictor,
                  ") for global analysis.\n"
                )
              )
            }
          } else {
            cat(
              paste0(
                "WARNING: Specie ",
                spp[n],
                " response (",
                responses[i],
                ") has insufficient data (",
                nrow(ind),
                ") for global analysis.\n"
              )
            )
          }
        }, error = function(e) {
          cat(
            paste0(
              "WARNING: Specie ",
              if (nrow(ind) > 0)
                ind[1, 1]
              else
                spp[n],
              " responses (",
              responses[i],
              ") has error (global): ",
              conditionMessage(e),
              "\n"
            )
          )
        })
      }
    }
  }
  if (length(results_list) > 0) {
    spp_trend_result <- do.call(rbind, results_list)
    rownames(spp_trend_result) <- NULL
  } else {
    spp_trend_result <- data.frame()
  }
  return(spp_trend_result)
}
