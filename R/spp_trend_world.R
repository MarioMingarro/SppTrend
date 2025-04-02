#' This function fits a linear model to analyze individual trends over time,
#' comparing with general data trend, and includes longitude transformation
#' and hemisphere detection.
#'
#' @param Data A data frame containing the variables for the model, including
#'   'species', 'year', 'month', 'Lon', 'Lat', 'Tmx', 'Tmn'.
#' @param responses A vector of response variable names (e.g., "Lat", "Lon", "Tmx").
#' @param predictor A vector of predictor variable names (e.g., "year_month").
#' @param spp A vector of species names.
#' @param n_min Minimum number of presences required for a species.
#'
#' @return A data frame with trend statistics, including:
#'   - `species`: Species name.
#'   - `response`: The name of the variable analyzed.
#'   - `trend`: The slope of the linear model.
#'   - `t`: The t-statistic of the model.
#'   - `pvalue`: The p-value of the trend.
#'   - `ci_95_max`: Upper bound of the 95% confidence interval.
#'   - `ci_95_min`: Lower bound of the 95% confidence interval.
#'   - `dif_t`: Difference in t-statistic values.
#'   - `dif_pvalue`: Difference in p-values.
#'   - `hemisphere`: Detected hemisphere ("N", "S", or "Both").
#'
#' @importFrom stats as.formula confint formula lm
#' @importFrom dplyr %>% mutate filter
#'
#' @examples
#' Data <- data.frame(
#'   species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
#'   year = sample(1900:2024, 500, replace = TRUE),
#'   month = sample(1:12, 500, replace = TRUE),
#'   Lon = runif(500, -10, 20),
#'   Lat = runif(500, 30, 70),
#'   Tmx = rnorm(500, 15, 10),
#'   Tmn = rnorm(500, 10, 8)
#' )
#' Data$year_month <- Data$year + Data$month * 0.075
#' predictor <- "year_month"
#' responses <- c("Lat", "Lon", "Tmx", "Tmn")
#' spp <- unique(Data$species)
#' general_trend_result <- spp_trend_world(Data, spp, predictor, responses, n_min = 50)
#'
#' @export
spp_trend_world <- function(Data, spp, predictor, responses, n_min = 50) {
  spp_trend_result <- data.frame(
    "species" = NA,
    "response" = NA,
    "trend" = NA,
    "t" = NA,
    "pvalue" = NA,
    "ci_95_max" = NA,
    "ci_95_min" = NA,
    "dif_t" = NA,
    "dif_pvalue" = NA,
    "hemisphere" = NA
  )

  spp_trend_result <- spp_trend_result[-1, ]

  # Function to transform longitude: Antimeridian becomes 0
  transform_lon_antimeridian_zero <- function(lon) {
    new_lon <- lon - 180
    new_lon[new_lon <= -180] <- new_lon[new_lon <= -180] + 360
    return(new_lon)
  }

  for (n in 1:length(spp)) {
    ind <- Data %>%
      dplyr::filter(species == spp[n]) %>%
      dplyr::mutate(group = "i")

    gen <- Data %>%
      dplyr::mutate(group = "g")

    dat <- rbind(gen, ind)

    if (nrow(ind) > n_min) {
      # Detect hemisphere (using min/max latitudes)
      min_lat <- min(ind$Lat, na.rm = TRUE)
      max_lat <- max(ind$Lat, na.rm = TRUE)

      hemisphere <- if (min_lat < 0 && max_lat > 0) {
        "Both"
      } else if (min_lat >= 0 && max_lat >= 0) {
        "N"
      } else if (min_lat < 0 && max_lat <= 0) {
        "S"
      } else {
        "Unknown" # Handle cases where min/max are NA or unexpected
      }

      for (i in 1:length(responses)) {
        tryCatch({
          table <- data.frame(
            "species" = unique(ind$species),
            "response" = responses[i],
            "trend" = NA,
            "t" = NA,
            "pvalue" = NA,
            "ci_95_max" = NA,
            "ci_95_min" = NA,
            "dif_t" = NA,
            "dif_pvalue" = NA,
            "n" = nrow(ind),
            "hemisphere" = hemisphere
          )

          # Transform longitude if response is "Lon"
          if (responses[i] == "Lon") {
            ind$Lon_transformed <- transform_lon_antimeridian_zero(ind$Lon)
            gen$Lon_transformed <- transform_lon_antimeridian_zero(gen$Lon)
            dat$Lon_transformed <- transform_lon_antimeridian_zero(dat$Lon)
            model_g <- stats::lm(stats::formula(paste(
              "Lon_transformed", paste(predictor, collapse = "+"), sep = " ~ "
            )), data = gen)
            model_i <- stats::lm(stats::formula(paste(
              "Lon_transformed", paste(predictor, collapse = "+"), sep = " ~ "
            )), data = ind)
            model_int <- lm(formula(paste(
              "Lon_transformed", paste(predictor, "*group", collapse = "+"), sep = " ~ "
            )), data = dat)
          } else {
            model_g <- stats::lm(stats::formula(paste(
              responses[i], paste(predictor, collapse = "+"), sep = " ~ "
            )), data = gen)
            model_i <- stats::lm(stats::formula(paste(
              responses[i], paste(predictor, collapse = "+"), sep = " ~ "
            )), data = ind)
            model_int <- lm(formula(paste(
              responses[i], paste(predictor, "*group", collapse = "+"), sep = " ~ "
            )), data = dat)
          }

          table$trend <- model_i$coefficients[[2]]
          table$t <- summary(model_i)$coefficients[2, 3]
          table$pvalue <- summary(model_i)$coefficients[2, 4]
          table$ci_95_max <- stats::confint(model_i, predictor, level = 0.95)[, 2]
          table$ci_95_min <- stats::confint(model_i, predictor, level = 0.95)[, 1]
          table$dif_t <- summary(model_int)$coefficients[4, 3]
          table$dif_pvalue <- summary(model_int)$coefficients[4, 4]

          spp_trend_result <- rbind(spp_trend_result, table)
        }, error = function(e) {
          cat(paste0("WARNING: Specie ", ind[1, 1], " response (", responses[i], ") has"), conditionMessage(e), "\n")
        })
      }
    } else {
      print(paste0("Data for ", ind[1, 1], " specie are insufficient"))
    }
  }
  return(spp_trend_result)
}
