#' Overall Trend Analysis
#'
#' This function fits a linear model to analyze the overall trends over time for multiple response variables, extracting statistics.
#' For 'lat' responses, the trend is calculated separately for each hemisphere.
#'
#' @param data A `data frame` containing the variables for the model, including 'species', 'year', 'month', 'lon', 'lat', 'tmx' and/or 'tmn'.
#' @param predictor A `character`vector of predictor variable names representing a temporal variable (e.g., "year_month").
#' @param responses A `character` vector of response variable names to analyze (e.g., "lat"  for spatial trends, "tmx" for thermal trends).
#'
#'@return A data frame with trend statistics, including:
#' - `responses`: The name of the variable analyzed.
#' - `trend`: The slope of the linear model.
#' - `t`: The t-statistic of the model.
#' - `pvalue`: The p-value of the trend.
#' - `ci_95_max`: Upper bound of the 95% confidence interval.
#' - `ci_95_min`: Lower bound of the 95% confidence interval.
#' - `n`: Number of datapoints used for the specific hemisphere trend.
#' - `hemisphere`: The geographical context of the trend ("Global", "North", or "South").
#'
#' @importFrom stats as.formula confint formula lm coef
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
#' overall_trend_result <- overall_trend(data, predictor, responses)
#'
#' print(overall_trend_result)
#'
#' @export
#'
overall_trend <- function(data, predictor, responses) {

  data$hemisphere <- ifelse(data$lat >= 0, "North", "South")

  groups_to_analyze <- list(
    Global = data,
    North = data[data$hemisphere == "North", ],
    South = data[data$hemisphere == "South", ]
  )

  results_list <- list()
  for (var in responses) {
    for (h_name in names(groups_to_analyze)) {
      data_set <- groups_to_analyze[[h_name]]
      current_var <- var

      if (var == "lon") {
        data_set$lon_transformed <- (data_set$lon + 180) %% 360
        current_var <- "lon_transformed"
      }
      if (nrow(data_set) > 0 && length(unique(data_set[[predictor]])) > 1) {
        formula_str <- paste(current_var, "~", paste(predictor, collapse = "+"))

        tryCatch({
          model_g <- lm(as.formula(formula_str), data = data_set)
          if (length(coef(model_g)) > 1) {
            summary_model <- summary(model_g)

            if (nrow(summary_model$coefficients) >= 2 && !is.na(summary_model$coefficients[2, 3])) {
              trend_value <- coef(model_g)[2]
              conf_int <- confint(model_g, level = 0.95)

              results_list[[length(results_list) + 1]] <- data.frame(
                responses = var,
                trend = trend_value,
                t = summary_model$coefficients[2, 3],
                pvalue = summary_model$coefficients[2, 4],
                ci_95_max = conf_int[predictor, 2],
                ci_95_min = conf_int[predictor, 1],
                n = nrow(data_set),
                hemisphere = h_name,
                stringsAsFactors = FALSE
              )
            }
          }
        }, error = function(e) {
          warning(paste("Error fitting model for response", var, "in", h_name, ":", conditionMessage(e)))
        })
      } else {
        if (h_name != "Global") {
          warning(paste("Insufficient data or predictor variation for response", var, "in", h_name, "hemisphere."))
        }
      }
    }
  }
  if (length(results_list) > 0) {
    overall_trend_result <- do.call(rbind, results_list)
    rownames(overall_trend_result) <- NULL
  } else {
    overall_trend_result <- data.frame(
      responses = character(),
      trend = numeric(),
      t = numeric(),
      pvalue = numeric(),
      ci_95_max = numeric(),
      ci_95_min = numeric(),
      n = integer(),
      hemisphere = character()
    )
  }

  return(overall_trend_result)
}
