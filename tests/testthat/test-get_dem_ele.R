library(testthat)

Data <- data.frame(
  species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
  year = sample(1950:2020, 500, replace = TRUE),
  month = sample(1:12, 500, replace = TRUE),
  Lon = runif(500, -10, 20),
  Lat = runif(500, 30, 70)
)


testthat::test_that("get_era5_tme works correctly", {
  dem_file <- "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/MARIPOSAS/TEST_Spp_Trends/wc2.1_30s_elev.tif"
  Data_with_elevation <- get_dem_ele(Data, dem_file)
  expect_s3_class(Data_with_elevation, "data.frame")
})
