library(testthat)

Data <- data.frame(
  species = sample(paste0("spp_", 1:10), 10, replace = TRUE),
  year = sample(1950:2023, 10, replace = TRUE),
  month = sample(1:12, 10, replace = TRUE),
  Lon = runif(10, -10, 20),
  Lat = runif(10, 30, 70)
)


testthat::test_that("get_era5_tme works correctly", {
  nc_file <- "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/MARIPOSAS/TEST_Spp_Trends/era5_1940_2023.nc"
  get_era5_tme_result <- get_era5_tme(Data, nc_file, month_col = "month")
  expect_s3_class(get_era5_tme_result, "data.frame")
})
