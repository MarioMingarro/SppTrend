library(testthat)

data <- data.frame(
  species = sample(paste0("spp_", 1:10), 10, replace = TRUE),
  year = sample(1950:2020, 10, replace = TRUE),
  month = sample(1:12, 10, replace = TRUE),
  lon = runif(10, -10, 20),
  lat = runif(10, 30, 70)
)


testthat::test_that("get_era5_tme works correctly", {
  nc_file <- "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/MARIPOSAS/TEST_Spp_Trends/era5_1940_2023.nc"
  data_with_Tme <- get_era5_tme(data, nc_file, month_col = "month")
  expect_s3_class(data_with_Tme, "data.frame")
})
