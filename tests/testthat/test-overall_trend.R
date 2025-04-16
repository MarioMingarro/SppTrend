library(testthat)

data <- data.frame(
  species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
  year = sample(1900:2024, 500, replace = TRUE),
  month = sample(1:12, 500, replace = TRUE),
  lon = runif(500, -10, 20),
  lat = runif(500, 30, 70),
  tme = rnorm(500, 15, 10)
)

data$year_month  = data$year + data$month * 0.075

predictor <- "year_month"
responses <- c("lat", "tme")

testthat::test_that("overall_trend works correctly", {
  overall_trend_result <- overall_trend(data, predictor, responses)
  expect_s3_class(overall_trend_result, "data.frame")
  expect_false(any(is.na(overall_trend_result$Trend)))
})
