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
#' @return Data data frame with Mean Temperature in K
#' @importFrom terra rast nlyr
#'
#' @examples
#' \dontrun{
#'
#' Data <- data.frame(
#'    species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
#'    year = sample(1950:2020, 500, replace = TRUE),
#'    month = sample(1:12, 500, replace = TRUE),
#'    Lon = runif(500, -10, 20),
#'    Lat = runif(500, 30, 70)
#' )
#'
#' nc_file <- "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/MARIPOSAS/TEST_Spp_Trends/era5_1940_2023.nc"
#'
#' Data_with_Tme <- get_era5_tme(Data, nc_file, month_col = "month")
#'
#' print(Data_with_Tme)
#'}
#' @export
#'
get_era5_tme <- function(Data, nc_file, month_col = NULL) {
  era5_raster <- terra::rast(nc_file)
  variable_name <- "t2m"
  num_layers <- terra::nlyr(era5_raster)
  start_date <- as.Date("1940-01-01")
  expected_months <- num_layers / 2
  date_sequence <- seq(start_date, by = "month", length.out = expected_months)
  end_date <- as.Date(tail(date_sequence)[6])
  print(paste("First year: ", start_date))
  print(paste("Last dates:", end_date))

  temperatures <- numeric(nrow(Data))

  for (i in 1:nrow(Data)) {
    lon <- Data$Lon[i]
    lon <- ifelse(lon < 0, 360 + lon, lon)
    lat <- Data$Lat[i]
    year <- Data$year[i]

    if (!is.null(month_col) &&
        month_col %in% colnames(Data) && !is.na(Data[[month_col]][i])) {
      month_val <- sprintf("%02d", Data[[month_col]][i])
      target_date_str <- paste0(year, "-", month_val, "-01")
      target_date <- as.Date(target_date_str)

      month_year_str <- format(target_date, "%Y-%m")
      date_sequence_ym <- format(date_sequence, "%Y-%m")
      month_index_in_sequence <- which(date_sequence_ym == month_year_str)

      if (length(month_index_in_sequence) > 0) {
        layer_index <- (month_index_in_sequence - 1) * 2 + 1
        if (layer_index <= num_layers) {
          temperatures[i] <- terra::extract(era5_raster[[layer_index]], cbind(lon, lat))[1, 1]
        } else {
          temperatures[i] <- NA
          warning(paste(
            "No data found for",
            target_date_str,
            "at Lon:",
            lon,
            "Lat:",
            lat
          ))
        }
      } else {
        temperatures[i] <- NA
        warning(paste(
          "No data found for",
          target_date_str,
          "at Lon:",
          lon,
          "Lat:",
          lat
        ))
      }

    } else {
      start_year_date <- as.Date(paste0(year, "-01-01"))
      end_year_date <- as.Date(paste0(year, "-12-31"))

      year_sequence <- as.numeric(format(date_sequence, "%Y"))
      relevant_month_indices <- which(year_sequence == year)

      if (length(relevant_month_indices) > 0) {
        relevant_layer_indices <- (relevant_month_indices - 1) * 2 + 1
        relevant_layer_indices <- relevant_layer_indices[relevant_layer_indices <= num_layers]

        if (length(relevant_layer_indices) > 0) {
          annual_data <- era5_raster[[relevant_layer_indices]]
          if (terra::nlyr(annual_data) > 0) {
            annual_mean_raster <- mean(annual_data)
            extracted_value <- terra::extract(annual_mean_raster, cbind(lon, lat))[1, 1]
            if (!is.null(extracted_value) &&
                !is.na(extracted_value)) {
              temperatures[i] <- extracted_value
            } else {
              temperatures[i] <- NA
              warning(paste(
                "Extracted NA value for year",
                year,
                "at Lon:",
                lon,
                "Lat:",
                lat
              ))
            }
          } else {
            temperatures[i] <- NA
            warning(paste(
              "No layers for year",
              year,
              "in annual_data at Lon:",
              lon,
              "Lat:",
              lat
            ))
          }
        } else {
          temperatures[i] <- NA
          warning(
            paste(
              "No relevant layer indices found for year",
              year,
              "at Lon:",
              lon,
              "Lat:",
              lat
            )
          )
        }
      } else {
        temperatures[i] <- NA
        warning(
          paste(
            "No months found for the year",
            year,
            "in date_sequence at Lon:",
            lon,
            "Lat:",
            lat
          )
        )
      }
    }
  }
  Data$Tme <- temperatures - 273.15
  return(Data)
}
