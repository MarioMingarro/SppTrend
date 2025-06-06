% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overall_trend.R
\name{overall_trend}
\alias{overall_trend}
\title{Overall Trend Analysis}
\usage{
overall_trend(data, predictor, responses)
}
\arguments{
\item{data}{A data frame containing the variables for the model.}

\item{predictor}{A vector of predictor variable names.}

\item{responses}{A vector of response variable names.}
}
\value{
A data frame with trend statistics, including:
\itemize{
\item \code{Response}: The name of the variable analyzed.
\item \code{Trend}: The slope of the linear model.
\item \code{t}: The t-statistic of the model.
\item \code{pvalue}: The p-value of the trend.
\item \code{ci_95_max}: Upper bound of the 95\% confidence interval.
\item \code{ci_95_min}: Lower bound of the 95\% confidence interval.
}
}
\description{
This function fits a linear model to analyze the overall trends over time.
}
\examples{

data <- data.frame(
   species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
   year = sample(1950:2020, 500, replace = TRUE),
   month = sample(1:12, 500, replace = TRUE),
   lon = runif(500, -10, 20),
   lat = runif(500, 30, 70),
   tme = rnorm(500, 15, 10)
)

data$year_month <- data$year + data$month * 0.075

predictor <- "year_month"
responses <- c("lat", "lon", "tme")

overall_trend_result <- overall_trend(data, predictor, responses)

print(overall_trend_result)

}
