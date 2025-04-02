#' Extract Elevation from DEM
#'
#' This function extracts elevation values from a Digital Elevation Model (DEM)
#' for given longitude and latitude coordinates.
#'
#' @param df A data frame containing longitude and latitude coordinates.
#' @param dem_file Path to the DEM raster file.
#'
#' @return A data frame with an added 'elevation' column containing the extracted elevation values.
#'
#' @importFrom terra rast extract
#'
#' @examples
#' # Example data frame
#' Data <- data.frame(
#'   species = sample(paste0("spp_", 1:10), 10, replace = TRUE),
#'   Lon = runif(10, -10, 20),
#'   Lat = runif(10, 30, 70)
#' )
#'
#' # Replace 'path/to/your/dem.tif' with the actual path to your DEM file
#' # dem_file <- "path/to/your/dem.tif"
#' # Data_with_elevation <- get_dem_elevation(Data, dem_file)
#' # print(Data_with_elevation)
#'
#' @export
get_dem_elevation <- function(df, dem_file) {
  # Load the DEM raster
  dem_raster <- terra::rast(dem_file)

  # Initialize a vector to store elevation values
  elevations <- numeric(nrow(df))

  # Loop through each row of the data frame
  for (i in 1:nrow(df)) {
    lon <- df$Lon[i]
    lon <- ifelse(lon < 0, 360 + lon, lon) # Convert negative longitudes

    lat <- df$Lat[i]

    # Extract elevation at the given coordinates
    elevations[i] <- terra::extract(dem_raster, cbind(lon, lat))[1, 1]
  }

  # Add the elevation values as a new column to the data frame
  df$elevation <- elevations

  return(df)
}
