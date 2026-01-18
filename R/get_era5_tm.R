#' @title Extract ERA5 data from NetCDF file
#'
#' @description This function extracts climate variable values from an ERA5 NetCDF file based on location (`lon/lat`) and time (`year/month`).
#'
#' @param data A `data frame` containing points of interest. Must include `lon`, `lat`, `year`, and `month` columns.
#' @param nc_file Full `character` path to the downloaded ERA5-Land raster (`.nc`) file.
#'
#' @return The input data frame `data` with a new column named (`tme`), containing the temperature values.
#'
#' @importFrom terra rast varnames nlyr time vect project extract
#' @importFrom stringr str_extract
#' @importFrom stats na.omit
#'
#' @export

get_era5_tme <- function(data, nc_file) {
  required_cols <- c("lon", "lat", "year", "month")
  if (!all(required_cols %in% names(data))) {
    missing_cols <- setdiff(required_cols, names(data))
    stop(paste0("Error: ", paste(missing_cols, collapse = ", "), " coulumn cannot be found"))
  }
  era5_raster <- terra::rast(nc_file, subds="t2m")
  num_layers <- terra::nlyr(era5_raster)
  date_info <- terra::time(era5_raster)
  if (!any(is.na(date_info)))
  {
    layer <- data.frame(dates=as.Date(date_info))
  }else{
    layer <- data.frame(dates=as.numeric(str_extract(names(era5_raster), "-?\\d+$")))
    layer$dates <- as.POSIXct( layer$dates, origin = "1970-01-01 00:00:00", tz = "UTC")
  }
  layer$years <-as.numeric(format(layer$dates, "%Y"))
  layer$months <- as.numeric(format(layer$dates, "%m"))
  layer$days <- as.numeric(format(layer$dates, "%d"))
  if (any(layer$days!=1))
  {
    stop("Error: Era5 monthly averages expected, but daily values found")
  }
  print(paste("Raster coverage: From ", min(layer$dates), " to ", max(layer$dates)))
  data_years <- unique(data$year)
  not_found_years=setdiff(data_years,layer$years)
  if (!is.null(not_found_years) & length(not_found_years)>0)
  {
    if (length(data_years)== length(not_found_years))
    {
      stop("Error: no data found for any year of the ocurrences")
    }
    else{
      warning(paste(
        "No data found for years:", paste(sort(not_found_years), collapse = ", ")
      ))
    }
  }
  raster_crs <- terra::crs(era5_raster)
  original_lon <- data$lon
  data$lon[data$lon < 0] <- data$lon[data$lon < 0] + 360
  coords_spatvector <- terra::vect(data, geom = c("lon", "lat"), crs = "EPSG:4326")
  terra::crs(coords_spatvector) <- raster_crs
  unique_year_month_combinations <- unique(data[c("year", "month")])
  unique_year_month_combinations_ <- na.omit(unique_year_month_combinations)
  unique_year_month_combinations <- unique_year_month_combinations[order(unique_year_month_combinations$year,
                                                                      unique_year_month_combinations$month),]
  for(year_month_index in 1:nrow(unique_year_month_combinations))
  {
    year_month <- unique_year_month_combinations[year_month_index,]
    selected_row_indexes <- which(data$year==year_month$year & data$month==year_month$month)
    layer_index <- which(year_month$year==layer$years & year_month$month==layer$months)
    if (length(layer_index)==0)
    {
      data[selected_row_indexes,"tme"] <- NA
      warning(paste(
        "No data found for year", year_month$year, "month", year_month$month
      ))
    }
    else{
      layer_index <- floor(mean(layer_index))
      temperatures <- terra::extract(era5_raster[[layer_index]],
                                  coords_spatvector[selected_row_indexes],
                                  ID=F)
      data[selected_row_indexes,"tme"] = temperatures - 273.15
      null_temps <- sapply(data[selected_row_indexes,"tme" ], is.null)
      if (any(null_temps))
      {
        warning(paste(
          "No data found for year", year_month$year, "month", year_month$month,
          "at coords",
          sapply(data[selected_row_indexes,c("lon", "lat")][null_temps, ],
                 FUN=function(lonlat){
                   paste("lon=",lonlat[1],"lat=",lonlat[2])
                 }),
          "\n"
        ))
      }
    }
  }
  data$lon <- original_lon
  data$tme <- round(data$tme, 3)
  data <- na.omit(data)
  return(data)
}
