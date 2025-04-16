#' Get ERA5 Temperature data
#'
#' This function extracts temperature data from an ERA5 netCDF file for given coordinates and times.
#' It can extract monthly temperature if a month column is provided, or annual mean temperature otherwise.
#'
#' @param data A data frame with columns for longitude ('lon'), latitude ('lat'), and year ('year').
#'             Optionally, a column specifying the month can be provided if the `month_col` argument is used.
#' @param nc_file Path to the ERA5 netCDF file. This file should contain the 't2m' (2-meter temperature) variable.
#' @param month_col (Optional) The name of the column in the `data` data frame that contains the month (as a numeric value from 1 to 12). If this argument is not NULL, the function will extract monthly temperature. If NULL, it will extract the annual mean temperature for the given year.
#'
#' @return A data frame identical to the input `data` but with an additional column named 'tme' containing the temperature in degrees Celsius.
#'
#' @importFrom terra rast nlyr extract mean
#' @importFrom utils tail
#' @importFrom lubridate as_date
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
#'   year = sample(1950:2020, 500, replace = TRUE),
#'   month = sample(1:12, 500, replace = TRUE),
#'   lon = runif(500, -10, 20),
#'   lat = runif(500, 30, 70)
#' )
#'
#' nc_file <- "path/to/your/era5_data.nc" # Replace with the actual path to your file
#'
#' data_with_monthly_tme <- get_era5_tme(data, nc_file, month_col = "month")
#' print(head(data_with_monthly_tme))
#'
#' data_annual <- data.frame(
#'   species = sample(paste0("spp_", 1:5), 200, replace = TRUE),
#'   year = sample(1960:2010, 200, replace = TRUE),
#'   lon = runif(200, -5, 15),
#'   lat = runif(200, 35, 65)
#' )
#'
#' data_with_annual_tme <- get_era5_tme(data_annual, nc_file)
#' print(head(data_with_annual_tme))
#' }
#' @export
#'
get_era5_tme <- function(data, nc_file, month_col = NULL) {
  era5_raster <- terra::rast(nc_file, subds = "t2m")
  variable_name <- "t2m"
  num_layers <- terra::nlyr(era5_raster)
  start_date <- as.Date("1940-01-01")
  expected_months <- num_layers / 2
  date_sequence <- seq(start_date, by = "month", length.out = expected_months)
  end_date <- tail(date_sequence, n = 1)
  print(paste("Raster coverage: From ", start_date, " to ", end_date))

  temperatures <- numeric(nrow(data))

  for (i in 1:nrow(data)) {
    lon <- data$lon[i]
    lon <- ifelse(lon < 0, 360 + lon, lon)
    lat <- data$lat[i]
    year <- data$year[i]

    if (!is.null(month_col) &&
        month_col %in% colnames(data) &&
        !is.na(data[[month_col]][i])) {
      month_val <- sprintf("%02d", data[[month_col]][i])
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
            "at lon:",
            lon,
            "lat:",
            lat
          ))
        }
      } else {
        temperatures[i] <- NA
        warning(paste(
          "No data found for",
          target_date_str,
          "at lon:",
          lon,
          "lat:",
          lat
        ))
      }

    } else {
      year_sequence <- as.numeric(format(date_sequence, "%Y"))
      relevant_month_indices <- which(year_sequence == year)

      if (length(relevant_month_indices) > 0) {
        relevant_layer_indices <- (relevant_month_indices - 1) * 2 + 1
        relevant_layer_indices <- relevant_layer_indices[relevant_layer_indices <= num_layers]

        if (length(relevant_layer_indices) > 0) {
          annual_data <- era5_raster[[relevant_layer_indices]]
          if (terra::nlyr(annual_data) > 0) {
            annual_mean_raster <- terra::mean(annual_data)
            extracted_value <- terra::extract(annual_mean_raster, cbind(lon, lat))[1, 1]
            if (!is.null(extracted_value) &&
                !is.na(extracted_value)) {
              temperatures[i] <- extracted_value
            } else {
              temperatures[i] <- NA
              # Removed warning as per user feedback
            }
          } else {
            temperatures[i] <- NA
            warning(paste(
              "No layers for year",
              year,
              "in annual_data at lon:",
              lon,
              "lat:",
              lat
            ))
          }
        } else {
          temperatures[i] <- NA
          # Removed warning as per user feedback
        }
      } else {
        temperatures[i] <- NA
        # Removed warning as per user feedback
      }
    }
  }
  data$tme <- temperatures - 273.15
  return(data)
}
