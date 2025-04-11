#' Overall Trend Analysis
#'
#' This function fits a linear model to analyze trends over time.
#'
#' @param Data A data frame containing the variables for the model.
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
#' @importFrom stats as.formula confint formula lm summary
#'
#' @examples
#'
#'Data <- data.frame(
#'    species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
#'    year = sample(1900:2024, 500, replace = TRUE),
#'    month = sample(1:12, 500, replace = TRUE),
#'    Lon = runif(500, -10, 20),
#'    Lat = runif(500, 30, 70),
#'    Tmx = rnorm(500, 15, 10),
#'    Tmn = rnorm(500, 10, 8))
#'
#'Data$year_month   = Data$year + Data$month * 0.075
#'
#'predictor <- "year_month" # predictor <- "year" in case month is not available
#'responses <- c("Lat", "Lon", "Tmx", "Tmn")
#'
#'overall_trend_result <- overall_trend(Data, predictor, responses)
#'
#' @export
#'
overall_trend <- function(Data, predictor, responses) {
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
      model_g <- lm(as.formula(formula_str), data = Data)
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
