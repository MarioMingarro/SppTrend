#' @title Individual trend analysis
#'
#' @description Calculates species-specific temporal trends for geographic and environmental variables.
#' It compares individual species' trajectories against the regional baseline (calculated via an interaction term).
#'
#' @param data A `data frame` containing the variables for the model, including `species`, `year`, `month`, `lon`, `lat`, `tme` and/or `ele`.
#' @param predictor A `character` vector of predictor variable names representing a temporal variable (`year_month`).
#' @param responses A `character` vector of response variable names to analyze.
#' @param spp A `character` vector of unique species names.
#' @param n_min Minimum `numeric` number of presences required for a species in each hemisphere (or globally for species in both hemispheres) to perform the analysis.
#'
#' @return A data frame with trend statistics, including:
#'    - `species`: Name of the analyzed species.
#'    - `responses`: Name of the variable analyzed.
#'    - `trend`: Slope of the linear model (rate of change over time).
#'    - `t`: t-statistic for the species-specific trend.
#'    - `pvalue`: Statistical significance of the species trend.
#'    - `ci_95_max`, `ci_95_min`: 95% confidence interval bounds for the slope.
#'    - `dif_t`: t-statistic of the interaction term (species vs. baseline).
#'    - `dif_pvalue`: p-values of the interaction term. A low value indicates a significant deviation from the general trend.
#'    - `n`: Sample size for the specific species/hemisphere subset
#'    - `hemisphere`: Geographic context (`North`, `South`, or `Both` for global comparison).
#'
#' @details
#' The function fits linear models for each species and compares them to the general trend of the
#' corresponding hemisphere (or global data) using an interaction model (`response ~ predictor * group`).
#' Separate analyses are performed for Northern and Southern Hemispheres to account for divergent
#' ecological responses. For species spanning both hemispheres, a "Both" category provides a global
#' comparison. Longitude values are transformed to a 0-360 range to ensure statistical consistency
#' near the antimeridian.
#'
#' @importFrom stats as.formula confint formula lm coef
#' @importFrom dplyr %>% mutate filter
#'
#' @examples
#'
#' data <- data.frame(
#'    species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
#'    year = sample(1950:2020, 500, replace = TRUE),
#'    month = sample(1:12, 500, replace = TRUE),
#'    lon = runif(500, -10, 20),
#'    lat = runif(500, 30, 70),
#'    tme = rnorm(500, 15, 10)
#' )
#'
#' data$year_month <- data$year + data$month * 0.075
#'
#' predictor <- "year_month"
#' responses <- c("lat", "lon", "tme")
#'
#' spp <- unique(data$species)
#'
#' spp_trend_result <- spp_trend(data, spp, predictor, responses, n_min = 50)
#'
#' print(spp_trend_result)
#'
#' @export
#'
spp_trend <- function(data, spp, predictor, responses, n_min = 50) {
  data <- na.omit(data)
  col_names <- names(data)
  if (!all(responses %in% col_names)) {
    missing_in_call <- responses[!responses %in% col_names]
    stop(paste0("Critical Error: Response(s) '", paste(missing_in_call, collapse = ", "), "' not found in dataset."))
  }
  if (!predictor %in% col_names) stop(paste0("Critical Error: Predictor '", predictor, "' not found."))
  data$hemisphere <- ifelse(data$lat >= 0, "North", "South")
  results_list <- list()
  for (n in 1:length(spp)) {
    ind <- data[data$species == spp[n], ]
    hemispheres_present <- unique(ind$hemisphere)
    ind_list <- split(ind, f = ind$hemisphere)
    for (h in names(ind_list)) {
      ind_hemisphere <- ind_list[[h]]
      data_hemisphere <- data[data$hemisphere == h, ]
      if (nrow(ind_hemisphere) > n_min) {
        for (i in 1:length(responses)) {
          tryCatch({
            current_resp <- responses[i]
            if (current_resp == "lon") {
              val_ind <- (ind_hemisphere$lon + 180) %% 360
              val_gen <- (data_hemisphere$lon + 180) %% 360
            } else {
              val_ind <- ind_hemisphere[[current_resp]]
              val_gen <- data_hemisphere[[current_resp]]
            }
            if (length(unique(ind_hemisphere[[predictor]])) > 1) {
              df_ind <- data.frame(y = val_ind, time = ind_hemisphere[[predictor]], group = "i")
              df_gen <- data.frame(y = val_gen, time = data_hemisphere[[predictor]], group = "g")
              dat_model <- rbind(df_ind, df_gen)
              model_i <- lm(y ~ time, data = df_ind)
              model_int <- lm(y ~ time * group, data = dat_model)
              sum_i <- summary(model_i)$coefficients
              sum_int <- summary(model_int)$coefficients
              ci <- confint(model_i, "time", level = .95)
              int_term <- "time:groupi"
              results_list[[length(results_list) + 1]] <- data.frame(
                species = spp[n],
                responses = current_resp,
                trend      = round(coef(model_i)[2], 3),
                t          = round(sum_i[2, 3], 3),
                pvalue     = round(sum_i[2, 4], 6),
                ci_95_max  = round(ci[1, 2], 3),
                ci_95_min  = round(ci[1, 1], 3),
                dif_t = if (int_term %in% rownames(sum_int)) round(sum_int[int_term, 3], 3) else NA,
                dif_pvalue = if (int_term %in% rownames(sum_int)) round(sum_int[int_term, 4], 6) else NA,
                n = nrow(ind_hemisphere),
                hemisphere = h,
                stringsAsFactors = FALSE
              )
            } else {
              message(paste0("WARNING: Specie ", spp[n], " response (", responses[i], ") in ", h,
                             " hemisphere has insufficient variation in predictor (", predictor, ")."))
            }
          }, error = function(e) {
            message(paste("Error en", spp[n], "-", responses[i], "(", h, "):", e$message))
          })
        }
      } else {
        message(paste0("WARNING: Specie ", spp[n], " has insufficient data (n = ", nrow(ind_hemisphere), " and < n_min = ",  n_min, ") in ", h, " hemisphere."))
      }
    }
    if (all(c("North", "South") %in% hemispheres_present)){
      n_north <- sum(ind$hemisphere == "North")
      n_south <- sum(ind$hemisphere == "South")
      if (n_north > n_min && n_south > n_min) {
        for (i in 1:length(responses)) {
          tryCatch({
            current_resp <- responses[i]
            if (length(unique(ind[[predictor]])) > 1) {
              if (current_resp == "lat") {
                val_ind_g <- abs(ind$lat)
                val_gen_g <- abs(data$lat)
              } else if (current_resp == "lon") {
                val_ind_g <- (ind$lon + 180) %% 360
                val_gen_g <- (data$lon + 180) %% 360
              } else {
                val_ind_g <- ind[[current_resp]]
                val_gen_g <- data[[current_resp]]
              }
              df_ind_g <- data.frame(y = val_ind_g, time = ind[[predictor]], group = "i")
              df_gen_g <- data.frame(y = val_gen_g, time = data[[predictor]], group = "g")
              dat_global <- rbind(df_ind_g, df_gen_g)
              model_i_g <- lm(y ~ time, data = df_ind_g)
              model_int_g <- lm(y ~ time * group, data = dat_global)
              sum_i_g <- summary(model_i_g)$coefficients
              sum_int_g <- summary(model_int_g)$coefficients
              ci_g <- confint(model_i_g, "time", level = .95)
              results_list[[length(results_list) + 1]] <- data.frame(
                species = spp[n],
                responses = current_resp,
                trend      = round(coef(model_i_g)[2], 3),
                t          = round(sum_i_g[2, 3], 3),
                pvalue     = round(sum_i_g[2, 4], 6),
                ci_95_max  = round(ci_g[1, 2], 3),
                ci_95_min  = round(ci_g[1, 1], 3),
                dif_t      = if ("time:groupi" %in% rownames(sum_int_g)) round(sum_int_g["time:groupi", 3], 3) else NA,
                dif_pvalue = if ("time:groupi" %in% rownames(sum_int_g)) round(sum_int_g["time:groupi", 4], 6) else NA,
                n          = nrow(ind),
                hemisphere = "Global",
                stringsAsFactors = FALSE
              )
            } else {
              message(paste0("WARNING: Specie ", spp[n], " response (", responses[i], ") in both hemisphere (Global) has insufficient variation in predictor (", predictor, ")."))
            }
          }, error = function(e) {
            message(paste("Error in", spp[n], "-", responses[i], ":", e$message))
          })
        }
      } else {
        message(paste0("WARNING: Specie ", spp[n], " has insufficient data in some hemisphere"))
      }
    }
  }
  if (length(results_list) > 0) {
    final_res <- do.call(rbind, results_list)
    rownames(final_res) <- NULL
    return(final_res)
  } else {
    return(data.frame())
  }
}
