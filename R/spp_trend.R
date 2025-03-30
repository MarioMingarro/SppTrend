#' Indivudual Trend Analysis
#'
#' This function fits a linear model to analyze individual trends over time and comparing with general data trend
#'
#' @param Data A data frame containing the variables for the model.
#' @param responses In this case date is response.
#' @param predictor A vector of predictor variable names.
#' @param spp A vector of species names.
#' @param n_min Value correspondign to minumun presences of a specie.
#'
#' @return A data frame with trend statistics, including:
#' - `Response`: The name of the variable analyzed.
#' - `Trend`: The slope of the linear model.
#' - `t`: The t-statistic of the model.
#' - `pvalue`: The p-value of the trend.
#' - `ci_95_max`: Upper bound of the 95% confidence interval.
#' - `ci_95_min`: Lower bound of the 95% confidence interval.
#'
#' @importFrom stats as.formula confint formula lm
#' @importFrom dplyr %>%
#' @examples
#'
#'Data <- data.frame(
#'   species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
#'   year = sample(1900:2024, 500, replace = TRUE),
#'   month = sample(1:12, 500, replace = TRUE),
#'   Lon = runif(500, -10, 20),
#'   Lat = runif(500, 30, 70),
#'   Tmx = rnorm(500, 15, 10),
#'   Tmn = rnorm(500, 10, 8))
#'
#'Data$year_month  = Data$year + Data$month * 0.075
#'
#'predictor <- "year_month" # predictor <- "year" in case month is not available
#'responses <- c("Lat", "Lon", "TMmx", "TMmn")
#'spp <- unique(Data$species)
#'
#'general_trend_result <- spp_trend(Data, spp, predictor, responses, n_min = 50)
#'
#' @export
#'
spp_trend <- function(Data, spp, predictor, responses, n_min = 50) {
  spp_trend_result <- data.frame(
    "species" = NA,
    "response" = NA,
    "trend" = NA,
    "t" = NA,
    "pvalue" = NA,
    "ci_95_max" = NA,
    "ci_95_min" = NA,
    "dif_t" = NA,
    "dif_pvalue" = NA
  )

  spp_trend_result <- spp_trend_result[-1, ]

  # Bucle para calcular las tendencias de cada una de las especies
  for (n in 1:length(spp)) {
    # Bucle para actuar sobre cada una de las especies
    # Filtra la especie
    ind <- Data %>%
      dplyr::filter(species == spp[n]) %>%
      dplyr::mutate(group = "i")

    gen <- Data %>%
      dplyr::mutate(group = "g")

    dat <- rbind(gen, ind)

    if (nrow(ind) > n_min) {
      for (i in 1:length(responses)) {
        tryCatch({
          table <-
            data.frame(
              "species" = NA,
              "response" = NA,
              "trend" = NA,
              "t" = NA,
              "pvalue" = NA,
              "ci_95_max" = NA,
              "ci_95_min" = NA,
              "dif_t" = NA,
              "dif_pvalue" = NA,
              "n" = NA
            )

          model_g <- stats::lm(stats::formula(paste(
            responses[i], paste(predictor, collapse = "+"), sep = " ~ "
          )), data = gen)
          table$species <- unique(ind$species)
          table$response <- responses[i]

          model_i <- stats::lm(stats::formula(paste(
            responses[i], paste(predictor, collapse = "+"), sep = " ~ "
          )), data = ind)

          table$trend <- model_i$coefficients[[2]]
          table$t <- summary(model_i)$coefficients[2, 3]
          table$pvalue <- summary(model_i)$coefficients[2, 4]
          table$ci_95_max <- stats::confint(model_i, predictor, level = 0.95)[, 2]
          table$ci_95_min <- stats::confint(model_i, predictor, level = 0.95)[, 1]
          table$n <- nrow(ind)

          model_int <- lm(formula(paste(
            responses[i],
            paste(predictor, "*group", collapse = "+"),
            sep = " ~ "
          )), data = dat)


          table$dif_t <- summary(model_int)$coefficients[4, 3]
          table$dif_pvalue <- summary(model_int)$coefficients[4, 4]

          spp_trend_result <- rbind(spp_trend_result, table)

        }, error = function(e) {
          cat(
            paste0(
              "WARNING: Specie ",
              ind[1, 1],
              " response (",
              responses[i],
              ") has"
            ),
            conditionMessage(e),
            "\n"
          )
        })
      }
    } else{
      print(paste0("Data for ", ind[1, 1], " specie are insufficient"))
    }
  }
  return(spp_trend_result)
}
