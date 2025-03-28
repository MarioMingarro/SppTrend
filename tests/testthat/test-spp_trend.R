library(testthat)
library(dplyr)

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
spp <- unique(Data$species)

testthat::test_that("spp_trend works correctly", {
  spp_trend_result <- spp_trend(Data, spp, predictor, responses, n_min = 15)
  expect_s3_class(spp_trend_result, "data.frame") # result = data frame
  expect_false(any(is.na(spp_trend_result$Trend))) # no NA
})
