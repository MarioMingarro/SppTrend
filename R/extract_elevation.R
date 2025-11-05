#' Extract elevation from DEM
#'
#' This function extracts elevation values from a Digital Elevation Model (DEM) raster file for given longitude and latitude coordinates.
#' It ensures that point coordinates are correctly transformed to the DEM's Coordinate Reference System (CRS) before extraction.
#'
#' @param data A data frame containing location coordinates. Must include columns specified by 'lon_col' and 'lat_col'.
#' @param dem_file Full path to the Digital Elevation Model (DEM) raster file.
#' @param lon_col Name of the longitude column in 'data' (default: "lon").
#' @param lat_col Name of the latitude column in 'data' (default: "lat").
#' @param output_col Name for the new elevation column (default: "elevation").
#'
#' @return The input data frame 'data' with a new column containing the extracted
#'         elevation values.
#'
#' @importFrom terra rast extract
#' @importFrom stringr str_extract
#'
#' @export
#'
extract_elevation <- function(data, dem_file, lon_col = "lon", lat_col = "lat", output_col = "elevation") {

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop("Error: 'data' must be a non-empty data frame.")
  }

  required_cols <- c(lon_col, lat_col)
  if (!all(required_cols %in% names(data))) {
    missing_cols <- setdiff(required_cols, names(data))
    stop(paste0("Error: Missing required coordinate columns in 'data': ", paste(missing_cols, collapse = ", ")))
  }

  if (!file.exists(dem_file)) {
    stop(paste0("Error: DEM file not found at specified path: ", dem_file))
  }

  dem_raster <- tryCatch({
    terra::rast(dem_file)
  }, error = function(e) {
    stop(paste("Error loading DEM raster file:", conditionMessage(e)))
  })

  if (terra::nlyr(dem_raster) != 1) {
    warning("DEM file has more than one layer. Only the first layer will be used for extraction.")
    dem_raster <- dem_raster[[1]]
  }

  coords <- data[, c(lon_col, lat_col)]
  names(coords) <- c("lon", "lat")

  elevations <- terra::extract(dem_raster, coords, ID = FALSE)

  data[[output_col]] <- elevations[, 1]

  return(data)
}
