#' Overall Trend Analysis
#'
#' This function fits a linear model to analyze the overall trends over time.
#'
#' @param data A data frame containing the variables for the model.
#' @param predictor A vector of predictor variable names.
#' @param responses A vector of response variable names.
#'
#' @return A data frame with trend statistics, including:
#' - `Response`: The name of the variable analyzed.
#' - `Trend`: The slope of the linear model.
#' - `t`: The t-statistic of the model.
#' - `pvalue`: The p-value of the trend.
#' - `ci_95_max`: Upper bound of the 95% confidence interval.
#' - `ci_95_min`: Lower bound of the 95% confidence interval.
#'
#' @importFrom stats as.formula confint formula lm
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
  overall_trend_result <- data.frame(
    response = character(),
    trend = numeric(),
    t = numeric(),
    pvalue = numeric(),
    ci_95_max = numeric(),
    ci_95_min = numeric()
  )
  for (var in responses) {
    table <- data.frame(
      response = var,
      trend = NA_real_,
      t = NA_real_,
      pvalue = NA_real_,
      ci_95_max = NA_real_,
      ci_95_min = NA_real_
    )
    formula_str <- paste(var, "~", paste(predictor, collapse = "+"))
    tryCatch({
      model_g <- lm(as.formula(formula_str), data = data)
      if (length(coef(model_g)) > 1) {
        table$trend <- coef(model_g)[2]
        summary_model <- summary(model_g)
        if (!is.na(summary_model$coefficients[2, 3]) && !is.na(summary_model$coefficients[2, 4])) {
          table$t <- summary_model$coefficients[2, 3]
          table$pvalue <- summary_model$coefficients[2, 4]
          conf_int <- confint(model_g, level = 0.95)
          if (nrow(conf_int) > 1) {
            table$ci_95_max <- conf_int[2, 2]
            table$ci_95_min <- conf_int[2, 1]
          }
        }
      } else {
        warning(paste("Model for response", var, "did not converge or has insufficient predictors."))
      }
    }, error = function(e) {
      warning(paste("Error fitting model for response", var, ":", conditionMessage(e)))
    })
    overall_trend_result <- rbind(overall_trend_result, table)
  }
  return(overall_trend_result)
}
