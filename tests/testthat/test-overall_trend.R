library(testthat)

Data <- data.frame(
  species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
  year = sample(1900:2024, 500, replace = TRUE),
  month = sample(1:12, 500, replace = TRUE),
  Lon = runif(500, -10, 20),
  Lat = runif(500, 30, 70),
  Tmx = rnorm(500, 15, 10),
  Tmx = rnorm(500, 10, 8)
)

Data$year_month  = Data$year + Data$month * 0.075

predictor <- "year_month"
responses <- c("Lat", "Tmx")

testthat::test_that("overall_trend works correctly", {
  overall_trend_result <- overall_trend(Data, predictor, responses)
  expect_s3_class(overall_trend_result, "data.frame")
  expect_false(any(is.na(overall_trend_result$Trend)))
})
