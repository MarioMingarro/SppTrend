#' Get ERA5 Temperature data
#'
#' This function extracts monthly temperature data from an ERA5 netCDF file for given coordinates and times.
#'
#' @param data A data frame with columns for longitude ('lon'), latitude ('lat'), year ('year') and month ('month').
#' @param nc_file Path to the ERA5 netCDF file. This file should contain the 't2m' (2-meter temperature) variable.
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
#' data_with_monthly_tme <- get_era5_tme(data, nc_file)
#' print(head(data_with_monthly_tme))
#'
#' }
#' @export
#'
get_era5_tme <- function(data, nc_file) {
  era5_raster <- terra::rast(nc_file)
  variable_name <- terra::varnames(era5_raster)
  num_layers <- terra::nlyr(era5_raster)
  date_info <- terra::time(era5_raster)
  start_date <- as.Date(date_info)[1]
  end_date <- as.Date(date_info)[length(date_info)]
  date_sequence <- seq(from = start_date, to = end_date, by = "month")
  print(paste("Raster coverage: From ", start_date, " to ", end_date))
  temperatures <- numeric(nrow(data))
  for (i in 1:nrow(data)) {
    lon <- data$lon[i]
    lon <- ifelse(lon < 0, 360 + lon, lon) #ERA5
    lat <- data$lat[i]
    year <- data$year[i]
    month_val <- sprintf("%02d", data[["month"]][i])
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
          format(target_date, "%Y-%m"),
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
        format(target_date, "%Y-%m"),
        "at lon:",
        lon,
        "lat:",
        lat
      ))
    }
  }
  data$tme <- temperatures - 273.15
  return(data)
}
