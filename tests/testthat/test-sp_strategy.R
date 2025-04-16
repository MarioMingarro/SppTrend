library(testthat)

spp_trends_results <- data.frame(
  species = paste0("spp_", 1:10),
  responses = rep("Lat", 10),
  trend = runif(10, -0.5, 0.5),
  t = runif(10, -2, 2),
  pvalue = runif(10, 0, 1),
  ci_95_max = runif(10, 0.05, 0.2),
  ci_95_min = runif(10, -0.1, 0.05),
  dif_t = runif(10, -1, 1.5),
  dif_pvalue = runif(10, 0.001, 0.9),
  n = round(runif(10, 40, 60)),
  hemisphere = sample(c("North", "South", "Both"), 10, replace = TRUE)
)

spp <- unique(spp_trends_results$species)
sig_level = 0.05 / length(spp) # Bonferroni

testthat::test_that("spp_strategy works correctly", {
  strategies <- spp_strategy(spp_trends_results, sig_level)
  expect_s3_class(strategies, "data.frame")
  expect_false(any(is.na(strategies$n)))
})
