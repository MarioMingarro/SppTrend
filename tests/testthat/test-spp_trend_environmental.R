library(testthat)

set.seed(99)
n <- 600
sim_data <- data.frame(
Species = sample(paste0("Sp", 1:5), n, replace = TRUE),
Year= sample(2000:2020, n, replace = TRUE),
Month = sample(1:12, n, replace = TRUE),
Temperature = rnorm(n, mean = 15, sd = 3),
Elevation = rnorm(n, mean = 500, sd = 100)
)

test_that("spp_trend_environmental returns a list with expected elements", {
result <- spp_trend_environmental(
data= sim_data,
responses = c("Temperature", "Elevation"),
min_records = 20,
min_years = 5,
verbose = FALSE
)

expect_type(result, "list")
expected_names <- c(
"params", "species_filter",
"environmental_global_lm",
"environmental_individual_lm",
"environmental_comparison"
)
expect_true(all(expected_names %in% names(result)))
})

test_that("spp_trend_environmental$environmental_comparison has classification column", {
result <- spp_trend_environmental(
data= sim_data,
responses = c("Temperature", "Elevation"),
min_records = 20,
min_years = 5,
verbose = FALSE
)

df <- result$environmental_comparison
expect_s3_class(df, "data.frame")
expect_true("individual_response_class" %in% names(df))
})

test_that("environmental classification values are valid", {
result <- spp_trend_environmental(
data= sim_data,
responses = c("Temperature", "Elevation"),
min_records = 20,
min_years = 5,
verbose = FALSE
)

df <- result$environmental_comparison
temp_classes <- df$individual_response_class[df$response == "Temperature"]
elev_classes <- df$individual_response_class[df$response == "Elevation"]

expect_true(all(temp_classes %in% c("TT", "TA", "TC")))
expect_true(all(elev_classes %in% c("SA", "SD", "SC")))
})

test_that("spp_trend_environmental$species_filter has retained column", {
result <- spp_trend_environmental(
data= sim_data,
responses = "Temperature",
min_records = 20,
min_years = 5,
verbose = FALSE
)

sf <- result$species_filter
expect_s3_class(sf, "data.frame")
expect_true(all(c("Species", "response", "n_records", "n_years", "retained") %in% names(sf)))
expect_type(sf$retained, "logical")
})

test_that("spp_trend_environmental$environmental_global_lm has correct columns", {
result <- spp_trend_environmental(
data= sim_data,
responses = "Temperature",
min_records = 20,
min_years = 5,
verbose = FALSE
)

glm_df <- result$environmental_global_lm
expect_s3_class(glm_df, "data.frame")
expect_true(all(c("response", "global_lm_slope", "global_lm_p_value") %in% names(glm_df)))
})

test_that("spp_trend_environmental works with only Temperature", {
result <- spp_trend_environmental(
data= sim_data,
responses = "Temperature",
min_records = 20,
min_years = 5,
verbose = FALSE
)

expect_true(all(result$environmental_comparison$response == "Temperature"))
})

test_that("spp_trend_environmental errors on missing required column", {
bad_data <- sim_data[, c("Species", "Year", "Month", "Temperature")]
expect_error(
spp_trend_environmental(bad_data, responses = "Elevation", verbose = FALSE)
)
})

test_that("use_Ncorrected_alpha FALSE gives same alpha for all species", {
result <- spp_trend_environmental(
data = sim_data,
responses= "Temperature",
min_records= 20,
min_years= 5,
alpha_ref= 0.05,
use_Ncorrected_alpha = FALSE,
verbose= FALSE
)

df <- result$environmental_individual_lm
if (nrow(df) > 0) {
expect_true(all(df$alpha == 0.05))
}
})

test_that("params element stores function arguments correctly", {
result <- spp_trend_environmental(
data= sim_data,
responses = "Temperature",
min_records = 15,
min_years = 4,
verbose = FALSE
)

expect_equal(result$params$min_records, 15)
expect_equal(result$params$min_years, 4)
expect_equal(result$params$responses, "Temperature")
})
