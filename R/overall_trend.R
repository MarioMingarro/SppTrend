#' @title Overall trend analysis
#' @description This function fits a linear model to stimate trends over time to get a general data trend,
#' and includes longitude transformation to handle the antimeridian and hemisphere detection. For species distributed across both hemispheres, it generates their overall trend
#' for each hemisphere and for the global data trend. The comparison with the general trend is assessed using an interaction term in the linear model.
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
  col_names <- names(data)
  if (!all(responses %in% col_names)) {
    missing_in_call <- responses[!responses %in% col_names]
    stop(paste0("Critical Error: Response(s) '", paste(missing_in_call, collapse = ", "), "' not found in dataset."))
  }
  if (!predictor %in% col_names) stop(paste0("Critical Error: Predictor '", predictor, "' not found."))
  expected_responses <- c("tme", "ele", "tmx", "tmn")
  missing_vars <- expected_responses[!tolower(expected_responses) %in% tolower(col_names)]
  if (length(missing_vars) > 0) {
    warning(paste("Recommended variables missing from the dataset:", paste(missing_vars, collapse = ", ")))
  }

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
      if (nrow(data_set) > 0 && length(unique(data_set[[predictor]])) > 1) {
        tryCatch({
          current_resp <- var
          target_var <- var
          if (var == "lon") {
            data_set$lon_transformed <- (data_set$lon + 180) %% 360
            target_var <- "lon_transformed"
          }
          formula_str <- as.formula(paste(target_var, "~", predictor))
          model_g <- lm(formula_str, data = data_set)
          sum_g <- summary(model_g)$coefficients
          if (nrow(sum_g) >= 2) {
            trend_value <- coef(model_g)[2]
            t_value <- sum_g[2, 3]
            p_value <- sum_g[2, 4]
            conf_int <- confint(model_g, predictor, level = 0.95)
            results_list[[length(results_list) + 1]] <- data.frame(
              responses = var,
              trend = trend_value,
              t = t_value,
              pvalue = p_value,
              ci_95_max = conf_int[1, 2],
              ci_95_min = conf_int[1, 1],
              n = nrow(data_set),
              hemisphere = h_name,
              stringsAsFactors = FALSE
            )
          }
        }, error = function(e) {
          message(paste("ERROR in overall analysis for response", var, "in", h_name, ":", e$message))
        })
      } else {
        if (h_name != "Global") {
          message(paste("WARNING: Insufficient data or predictor variation for", var, "in", h_name))
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
      hemisphere = character(),
      stringsAsFactors = FALSE
    )
  }
  return(overall_trend_result)
}
