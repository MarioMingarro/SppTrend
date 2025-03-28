library(testthat)

# Crear el data frame
spp_trends_result <- data.frame(
  species = paste0("spp_", 1:10),
  Response = rep("Lat", 10),
  Trend = runif(10, -0.5, 0.5),
  t = runif(10, -2, 2),
  pvalue = runif(10, 0, 1),
  ci_95_max = runif(10, 0.05, 0.2),
  ci_95_min = runif(10, -0.1, 0.05),
  Dif_t = runif(10, -1, 1.5),
  Dif_pvalue = runif(10, 0.001, 0.9),
  n = runif(10, 40, 60)
)

bonferroni = 0.05
spp <- unique(spp_trends_result$species)

testthat::test_that("spp_strategy works correctly", {
  strategies <- spp_strategy(spp_trends_result, bonferroni)
  expect_s3_class(strategies, "data.frame") # result = data frame
  expect_false(any(is.na(strategies$n))) # no NA
})


