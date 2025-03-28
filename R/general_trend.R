#' General Trend Analysis
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
#' @examples
#'
#'Data <- data.frame(
#'   species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
#'   year = sample(1900:2024, 500, replace = TRUE),
#'   month = sample(1:12, 500, replace = TRUE),
#'   Lon = runif(500, -10, 20),
#'   Lat = runif(500, 30, 70),
#'   Tmx = rnorm(500, 15, 10),
#'   Tmn = rnorm(500, 10, 8))
#'
#'Data$year_month  = Data$month * 0.075
#'Data$year_month  = Data$year + Data$year_month
#'
#'predictor <- "year_month" # predictor <- "year" in case month is not available
#'responses <- c("Lat", "Lon", "Tmx", "Tmn")
#'
#'general_trend_result = general_trend(Data, predictor, responses)
#'
#' @export
#'
general_trend <- function(Data, predictor, responses) {
  general_trend_result <- data.frame(
    "response" = character(),
    "trend" = numeric(),
    "t" = numeric(),
    "pvalue" = numeric(),
    "ci_95_max" = numeric(),
    "ci_95_min" = numeric())
  for (var in responses) {
    table <- data.frame(
      "response" = var,
      "trend" = NA,
      "t" = NA,
      "pvalue" = NA,
      "ci_95_max" = NA,
      "ci_95_min" = NA)
    formula_str <- paste(var, "~", paste(predictor, collapse = "+"))
    model_g <- lm(as.formula(formula_str), data = Data)
    table$trend <- model_g$coefficients[[2]]
    table$t <- summary(model_g)$coefficients[2, 3]
    table$pvalue <- summary(model_g)$coefficients[2, 4]
    table$ci_95_max <- confint(model_g, level = 0.95)[2, 2]
    table$ci_95_min <- confint(model_g, level = 0.95)[2, 1]
    general_trend_result <- rbind(general_trend_result, table)
  }
  return(general_trend_result)
}
