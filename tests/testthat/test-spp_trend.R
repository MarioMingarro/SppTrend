library(testthat)

data <- data.frame(
  species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
  year = sample(1900:2024, 500, replace = TRUE),
  month = sample(1:12, 500, replace = TRUE),
  Lon = runif(500, -10, 20),
  Lat = runif(500, 30, 70),
  Tme = rnorm(500, 15, 10)
)

data$year_month  = data$year + data$month * 0.075

predictor <- "year_month"
responses <- c("Lat", "Tme")
spp <- unique(data$species)

testthat::test_that("spp_trend works correctly", {
  spp_trend_result <- spp_trend(data, spp, predictor, responses, n_min = 10)
  expect_s3_class(spp_trend_result, "data.frame")
  expect_false(any(is.na(spp_trend_result$Trend)))
})
