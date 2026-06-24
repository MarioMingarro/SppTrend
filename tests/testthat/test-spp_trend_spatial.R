library(testthat)

set.seed(42)
n <- 600
sim_data <- data.frame(
  Species = sample(c("Sp_A", "Sp_B", "Sp_C"), n, replace = TRUE),
  Year = sample(2000:2020, n, replace = TRUE),
  Month = sample(1:12, n, replace = TRUE),
  Latitude = runif(n, 30, 65),
  Longitude = runif(n, -10, 30)
)

test_that("spp_trend_spatial_ecef returns a list with expected elements", {
  result <- spp_trend_spatial_ecef(
    data = sim_data,
    min_records = 20,
    min_years = 5,
    spatial_simulation_n = 100,
    spatial_probability_threshold = 0.90,
    direction_angle_threshold_deg = 68,
    random_seed = 1,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_true(all(
    c("spatial", "species_filter", "metadata") %in% names(result)
  ))
})

test_that("spp_trend_spatial_ecef$spatial is a data.frame with required columns",
          {
            result <- spp_trend_spatial_ecef(
              data = sim_data,
              min_records = 20,
              min_years = 5,
              spatial_simulation_n = 100,
              random_seed = 1,
              verbose = FALSE
            )

            df <- result$spatial
            expect_s3_class(df, "data.frame")
            expect_true(nrow(df) >= 1)

            required_cols <- c(
              "Species",
              "analysis",
              "n_records",
              "n_time_steps",
              "speed_surface_km_year",
              "direction_bearing_deg",
              "spatial_class_Latitudelon"
            )
            expect_true(all(required_cols %in% names(df)))
          })

test_that("spp_trend_spatial_ecef$spatial first row is global pool", {
  result <- spp_trend_spatial_ecef(
    data = sim_data,
    min_records = 20,
    min_years = 5,
    spatial_simulation_n = 100,
    random_seed = 1,
    verbose = FALSE
  )

  expect_equal(result$spatial$Species[1], "__GLOBAL_POOL__")
  expect_true(is.na(result$spatial$spatial_class_Latitudelon[1]))
})

test_that("spp_trend_spatial_ecef$spatial_class values are valid", {
  result <- spp_trend_spatial_ecef(
    data = sim_data,
    min_records = 20,
    min_years = 5,
    spatial_simulation_n = 100,
    random_seed = 1,
    verbose = FALSE
  )

  classes <- result$spatial$spatial_class_Latitudelon
  species_classes <- classes[result$spatial$Species != "__GLOBAL_POOL__"]
  expect_true(all(species_classes %in% c("SA", "SD", "SC")))
})

test_that("spp_trend_spatial_ecef$species_filter has correct structure",
          {
            result <- spp_trend_spatial_ecef(
              data = sim_data,
              min_records = 20,
              min_years = 5,
              spatial_simulation_n = 100,
              random_seed = 1,
              verbose = FALSE
            )

            sf <- result$species_filter
            expect_s3_class(sf, "data.frame")
            expect_true(all(
              c("Species", "n_records", "n_years", "retained") %in% names(sf)
            ))
            expect_type(sf$retained, "logical")
          })

test_that("spp_trend_spatial_ecef errors on missing required columns", {
  bad_data <- sim_data[, c("Species", "Year", "Month", "Latitude")]
  expect_error(spp_trend_spatial_ecef(bad_data, verbose = FALSE),
               "Longitude")
})

test_that("spp_trend_spatial_ecef handles spp subsetting", {
  result <- spp_trend_spatial_ecef(
    data = sim_data,
    spp = c("Sp_A", "Sp_B"),
    min_records = 20,
    min_years = 5,
    spatial_simulation_n = 100,
    random_seed = 1,
    verbose = FALSE
  )

  species_present <- result$spatial$Species[result$spatial$Species != "__GLOBAL_POOL__"]
  expect_true(all(species_present %in% c("Sp_A", "Sp_B")))
})
