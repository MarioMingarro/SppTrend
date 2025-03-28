library(testthat)

Data <- data.frame(
  species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
  year = sample(1900:2024, 500, replace = TRUE),
  month = sample(1:12, 500, replace = TRUE),
  Long = runif(500, -10, 20),
  Lat = runif(500, 30, 70),
  TMAX = rnorm(500, 15, 10),
  TMIN = rnorm(500, 10, 8))

Data$year_month  <- Data$month * 0.075
Data$year_month  <- Data$year + Data$year_month

predictor <- "year_month"
responses <- c("Lat", "TMAX")

testthat::test_that("general_trend works correctly", {
  general_trend_result <- general_trend(Data, predictor, responses)
  expect_s3_class(general_trend_result, "data.frame")
  expect_false(any(is.na(general_trend_result$Trend)))
})
