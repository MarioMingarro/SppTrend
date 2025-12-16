#' @title Extract elevation from DEM
#'
#' @description This function extracts elevation values from a Digital Elevation Model (DEM) raster file for given longitude and latitude coordinates.
#' Ensure that the DEM is projected in WGS84 (EPSG:4326) before extraction.
#' The output column will always be "elevation".
#'
#' @param data A data frame containing location coordinates. Must include columns specified by 'lon_col' and 'lat_col'.
#' @param dem_file Full path to the Digital Elevation Model (DEM) raster file.
#'
#' @return The input data frame 'data' with a new column containing the extracted elevation values.
#'
#' @importFrom terra rast extract
#' @importFrom stringr str_extract
#'
#' @export
extract_elevation <- function(data, dem_file) {
  target_crs <- "EPSG:4326"
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop("Error: 'data' must be a non-empty data frame.")
  }
  if (!("lon" %in% names(data))) {
    stop("Error: a longitude column named 'lon' cannot be found")
  }
  if (!("lat" %in% names(data))) {
    stop("Error: a latitude column named 'lat' cannot be found")
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
  is_same_crs <- terra::same.crs(dem_raster, target_crs)
  if (!is_same_crs) {
    message(paste("Reproyectando DEM a", target_crs, "para estandarización."))
    dem_raster <- terra::project(dem_raster, target_crs)
  }
  coords <- data[, c("lon", "lat")]
  coords_spatvector <- terra::vect(coords, geom = c("lon", "lat"), crs = target_crs)
  elevations <- terra::extract(dem_raster, coords_spatvector, ID = FALSE)
  data[["ele"]] <- elevations[, 1]
  if (any(is.na(data[["ele"]]))) {
    warning(paste(
      "Missing elevation data (NA) for", sum(is.na(data[["elevation"]])),
      "points. Points may be outside DEM extent."
    ))
  }
  return(data)
}
