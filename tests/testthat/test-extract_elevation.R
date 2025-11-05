library(testthat)
library(terra)

testthat::test_that("get_era5_tme works correctly", {
data <- data.frame(
  species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
  year = sample(1950:2020, 500, replace = TRUE),
  month = sample(1:12, 500, replace = TRUE),
  lon = runif(500, -10, 20),
  lat = runif(500, 30, 70))

dem_test <- terra::rast(
  ext = terra::ext(-10, 30, 20, 70),
  res = 1,
  crs = "+proj=longlat +datum=WGS84")

raster::values(dem_test) <- sample(100:2000, raster::ncell(dem_test), replace = TRUE)

dem_file_temp <- base::tempfile(fileext = ".tif")
raster::writeRaster(dem_test, dem_file_temp, overwrite = TRUE)

data_with_elevation <- extract_elevation(data, dem_file_temp)

expect_s3_class(data_with_elevation, "data.frame")
expect_true("elevation" %in% names(data_with_elevation))
})
