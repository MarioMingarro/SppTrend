#' @title Extract ERA5 data from NetCDF file
#'
#' @description This function extracts climate variable values from an ERA5 NetCDF file (or similar structure) based on location (lon/lat) and time (year/month) for a set of data points.
#'
#' @param data A data frame containing points of interest. Must include 'lon', 'lat', 'year', and 'month' columns.
#' @param nc_file Path to the NetCDF raster file containing the climate data.
#' @param variable Character string specifying the variable name to extract (e.g., "t2m" for 2m temperature). Default is "t2m".
#' @param input_crs Character string or object specifying the CRS of the input coordinates (e.g., "EPSG:4326"). Default is "EPSG:4326".
#'
#' @return The input data frame 'data' with a new column named after the extracted 'variable' (e.g., 't2m'), containing the climate values.
#'
#' @importFrom terra rast varnames nlyr time vect project extract
#' @importFrom stringr str_extract
#' @importFrom stats na.omit
#'
#' @export
extract_era5_data <- function(data, nc_file, variable = "t2m", input_crs = "EPSG:4326") {
  required_data_cols <- c("lon", "lat", "year", "month")
  if (!all(required_data_cols %in% names(data))) {
    missing_cols <- setdiff(required_data_cols, names(data))
    stop(paste0("Error: Missing required columns in 'data': ", paste(missing_cols, collapse = ", ")))
  }
  if (is.null(variable) || nchar(variable) == 0) {
    stop("Error: 'variable' cannot be empty or NULL.")
  }

  era5_raster_all <- tryCatch({
    terra::rast(nc_file)
  }, error = function(e) {
    stop(paste("Error loading NetCDF file:", conditionMessage(e)))
  })

  if (!(variable %in% terra::varnames(era5_raster_all))) {
    stop(paste0("Error: variable '", variable, "' not found in file '", nc_file, "'."))
  }

  era5_raster <- terra::rast(nc_file, subds = variable)

  date_info <- terra::time(era5_raster)

  if (!any(is.na(date_info))) {
    layer_dates <- data.frame(dates = as.Date(date_info))
  } else {
    layer_dates <- data.frame(dates = as.numeric(stringr::str_extract(names(era5_raster), "-?\\d+$")))
    layer_dates$dates <- as.POSIXct(layer_dates$dates, origin = "1970-01-01 00:00:00", tz = "UTC")
  }

  layer_dates$years <- as.numeric(format(layer_dates$dates, "%Y"))
  layer_dates$months <- as.numeric(format(layer_dates$dates, "%m"))
  layer_dates$days <- as.numeric(format(layer_dates$dates, "%d"))

  if (any(layer_dates$days != 1)) {
    stop("Error: ERA5 monthly averages expected (day=1), but daily or other values were found.")
  }

  data_years <- unique(data$year)
  not_found_years <- setdiff(data_years, layer_dates$years)

  if (length(not_found_years) == length(data_years)) {
    stop("Error: No data found in the NetCDF file for any year present in the input data.")
  } else if (length(not_found_years) > 0) {
    warning(paste("No data found for years:", paste(sort(not_found_years), collapse = ", ")))
  }

  data$lon[data$lon < 0] <- data$lon[data$lon < 0] + 360
  era5_crs <- terra::crs(era5_raster)
  if (is.null(era5_crs) || era5_crs == "") {
    warning("Raster CRS is undefined. Assuming input_crs is correct.")
    era5_crs <- input_crs
  }

  coords_spatvector <- terra::vect(data, geom = c("lon", "lat"), crs = input_crs)

  if (era5_crs != input_crs) {
    message(paste("Reprojecting points from", input_crs, "to", era5_crs, "to match the ERA5 raster."))
    coords_spatvector <- terra::project(coords_spatvector, era5_crs)
  }
  data[[variable]] <- NA_real_

  unique_year_month_combinations <- unique(data[c("year", "month")])
  unique_year_month_combinations <- na.omit(unique_year_month_combinations)
  unique_year_month_combinations <- unique_year_month_combinations[order(
    unique_year_month_combinations$year,
    unique_year_month_combinations$month
  ), ]

  for (year_month_index in 1:nrow(unique_year_month_combinations)) {
    year_month <- unique_year_month_combinations[year_month_index, ]
    selected_data_rows <- which(data$year == year_month$year & data$month == year_month$month)
    layer_index <- which(year_month$year == layer_dates$years & year_month$month == layer_dates$months)

    if (length(layer_index) == 0) {
      warning(paste("No data layer found for year", year_month$year, "month", year_month$month))
    } else if (length(layer_index) > 1) {
      stop(paste("Multiple layers found for year", year_month$year, "month", year_month$month, ". Data layers may be structured incorrectly."))
    } else {
      temperatures <- terra::extract(era5_raster[[layer_index]],
                                     coords_spatvector[selected_data_rows],
                                     ID = FALSE)
      data[selected_data_rows, variable] <- temperatures[, 1]
      null_temps <- is.na(data[selected_data_rows, variable])
      if (any(null_temps)) {
        warning(paste(
          "Missing data (NA) for", sum(null_temps), "points for", year_month$year, "/", year_month$month, ". Points may be outside raster extent."
        ))
      }
    }
  }

  return(data)
}
