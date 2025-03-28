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
#' @return A data frame with trend statistics.
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
#'Data$year_month  = Data$month * 0.075
#'Data$year_month  = Data$year + Data$year_month
#'
#'predictor <- "year_month"
#'responses <- c("Lat", "Lon", "TMmx", "TMmn")
#'spp <- unique(Data$species)
#'
#'general_trend_result <- spp_trend(Data, spp, predictor, responses, n_min = 50)
#'
#' @export
#'
spp_trend <- function(Data, spp, predictor, responses, n_min = 50) {
  spp_trend_result <- data.frame(
    "Spp" = NA,
    "Response" = NA,
    "Trend" = NA,
    "t" = NA,
    "pvalue" = NA,
    "ci_95_max" = NA,
    "ci_95_min" = NA,
    "Dif_t" = NA,
    "Dif_pvalue" = NA)

  spp_trend_result <- spp_trend_result[-1,]

  # Bucle para calcular las tendencias de cada una de las especies
  for (n in 1:length(spp)){
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
              "Spp" = NA,
              "Response" = NA,
              "Trend" = NA,
              "t" = NA,
              "pvalue" = NA,
              "ci_95_max" = NA,
              "ci_95_min" = NA,
              "Dif_t" = NA,
              "Dif_pvalue" = NA,
              "n" = NA
            )

          model_g <- stats::lm(stats::formula(paste(responses[i], paste(predictor, collapse = "+"), sep = " ~ ")), data = gen)
          table$Spp <- unique(ind$species)
          table$Response <- responses[i]

          model_i <- stats::lm(stats::formula(paste(responses[i], paste(predictor, collapse = "+"), sep = " ~ ")), data = ind)

          table$Trend <- model_i$coefficients[[2]]
          table$t <- summary(model_i)$coefficients[2, 3]
          table$pvalue <- summary(model_i)$coefficients[2, 4]
          table$ci_95_max <- stats::confint(model_i, "year_month", level = 0.95)[, 2]
          table$ci_95_min <- stats::confint(model_i, "year_month", level = 0.95)[, 1]
          table$n <- nrow(ind)

          model_int <- lm(formula(paste(responses[i], paste(
            predictor, "*group", collapse = "+"
          ), sep = " ~ ")), data = dat)


          table$Dif_t <- summary(model_int)$coefficients[4, 3]
          table$Dif_pvalue <- summary(model_int)$coefficients[4, 4]

          spp_trend_result <- rbind(spp_trend_result, table)

        }, error = function(e) {
          cat(
            paste0("WARNING: Specie ", ind[1, 1], " response (", responses[i], ") has"),
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
