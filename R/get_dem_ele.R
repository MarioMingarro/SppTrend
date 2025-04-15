#' Extract Elevation from DEM
#'
#' This function extracts elevation values from a Digital Elevation Model (DEM)
#' for given longitude and latitude coordinates.
#'
#' @param data A data frame containing longitude and latitude coordinates.
#' @param dem_file Path to the DEM raster file.
#'
#' @return A data frame with an added 'elevation' column containing the extracted elevation values.
#'
#' @importFrom terra rast extract
#'
#' @examples
#' \dontrun{
#'
#' data <- data.frame(
#'    species = sample(paste0("spp_", 1:10), 500, replace = TRUE),
#'    year = sample(1950:2020, 500, replace = TRUE),
#'    month = sample(1:12, 500, replace = TRUE),
#'    lon = runif(500, -10, 20),
#'    lat = runif(500, 30, 70)
#' )
#'
#'  dem_file <- "path/to/your/dem.tif"
#'
#'  data_with_elevation <- get_dem_ele(data, dem_file)
#'
#'  print(data_with_elevation)
#' }
#'
#' @export
#'
get_dem_ele <- function(data, dem_file) {
  dem_raster <- terra::rast(dem_file)
  elevations <- numeric(nrow(data))
  for (i in 1:nrow(data)) {
    lon <- data$lon[i]
    lat <- data$lat[i]
    elevations[i] <- terra::extract(dem_raster, cbind(lon, lat))[1, 1]
  }
  data$Ele <- elevations
  return(data)
}
