#' @title spp_trend_world
#' @description This function fits a linear model to analyze individual trends
#'    over time, comparing with general data trend, and includes longitude
#'    transformation and hemisphere detection. For species distributed across
#'    both hemispheres, it also compares their overall trend with the global data trend.
#'
#' @param Data A data frame containing the variables for the model, including
#'    'species', 'year', 'month', 'Lon', 'Lat', 'Tmx', 'Tmn'.
#' @param responses A vector of response variable names (e.g., "Lat", "Lon",
#'    "Tmx").
#' @param predictor A vector of predictor variable names (e.g., "year_month").
#' @param spp A vector of species names.
#' @param n_min Minimum number of presences required for a species in each
#'    hemisphere (or globally for species in both hemispheres).
#'
#' @return A data frame with trend statistics, including:
#'    - `species`: Species name.
#'    - `responses`: The name of the variable analyzed.
#'    - `trend`: The slope of the linear model.
#'    - `t`: The t-statistic of the model.
#'    - `pvalue`: The p-value of the trend.
#'    - `ci_95_max`: Upper bound of the 95% confidence interval.
#'    - `ci_95_min`: Lower bound of the 95% confidence interval.
#'    - `dif_t`: Difference in t-statistic values (comparison of species trend to general trend).
#'    - `dif_pvalue`: Difference in p-values (comparison of species trend to general trend).
#'    - `n`: Number of datapoints used for the specific species/hemisphere trend.
#'    - `hemisphere`: Detected hemisphere ("North", "South", or "Both" for global comparison).
#'
#' @details The function analyzes trends for each species in the provided data,
#'    considering different response variables and a specified predictor. It
#'    accounts for the antimeridian issue when analyzing longitude trends and
#'    separates the analysis by hemisphere. It also compares the trend of each
#'    species with a general trend calculated from the entire hemisphere's data.
#'    For species found in both the Northern and Southern Hemispheres, the function
#'    additionally compares the trend of the species (using all occurrences) with
#'    the trend of the entire dataset (all hemispheres combined).
#'
#' @importFrom stats as.formula confint formula lm coef summary
#' @importFrom dplyr %>% mutate filter
#'
#' @examples
#' \dontrun{
#' Data <- data.frame(
#'    species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
#'    year = sample(1900:2024, 500, replace = TRUE),
#'    month = sample(1:12, 500, replace = TRUE),
#'    Lon = runif(500, -10, 20),
#'    Lat = runif(500, 30, 70),
#'    Tmx = rnorm(500, 15, 10),
#'    Tmn = rnorm(500, 10, 8)
#' )
#' Data$year_month <- Data$year + Data$month * 0.075
#' predictor <- "year_month"
#' responses <- c("Lat", "Lon", "Tmx", "Tmn")
#' spp <- unique(Data$species)
#' general_trend_result <- spp_trend_world(Data, spp, predictor, responses, n_min = 50)
#' print(general_trend_result)
#' }
#'
#' @export
spp_trend_world <- function(Data, spp, predictor, responses, n_min = 50) {
  Data$hemisphere <- ifelse(Data$Lat >= 0, "North", "South")
  results_list <- list()
  for (n in 1:length(spp)) {
    ind <- Data[Data$species == spp[n], ]
    hemispheres_present <- unique(ind$hemisphere)
    ind_list <- split(ind, f = ind$hemisphere)

    for (h in names(ind_list)) {
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
                ind_hemisphere$Lon_transformed <- (ind_hemisphere$Lon + 180) %% 360
                dat$Lon_transformed <- (dat$Lon + 180) %% 360
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
            Data$group_global <- "g"
            ind$group_global <- "i"
            dat_global <- rbind(Data, ind)

            if (responses[i] == "Lon") {
              ind$Lon_transformed <- (ind$Lon + 180) %% 360
              dat_global$Lon_transformed <- (dat_global$Lon + 180) %% 360
              model_i_global <- lm(formula(paste(
                "Lon_transformed",
                paste(predictor, collapse = "+"),
                sep = " ~ "
              )), data = ind)
              model_int_global <- lm(formula(paste(
                "Lon_transformed",
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
