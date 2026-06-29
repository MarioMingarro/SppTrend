#' Analyse Temporal Changes in Species Geographical Position Using ECEF Vector Analysis
#'
#' @description
#' Estimates temporal trends in the spatial distribution of species by modelling
#' the annual displacement of each species' geographical centroid in three-dimensional
#' Earth-Centred Earth-Fixed (ECEF) Cartesian coordinates derived from the WGS84
#' reference ellipsoid.
#'
#' For each species (and for a pooled global reference), the function computes
#' monthly ECEF centroids from occurrence records, fits a multivariate linear
#' regression of centroid position against a continuous time variable
#' (\eqn{\text{time\_cont} = \text{Year} + (\text{Month} - 1) / 12}), and
#' projects the resulting three-dimensional slope vector onto the local tangent
#' plane at the species centroid to obtain ecologically interpretable quantities:
#' surface displacement speed (km yr\eqn{^{-1}}) and absolute geographical bearing.
#'
#' Each species is compared against a single global reference vector estimated
#' from all retained records pooled together. Uncertainty is propagated via
#' covariance-based multivariate simulation, and each species is assigned a
#' spatial class:
#' \describe{
#' \item{\strong{SA} (Spatial Adaptation)}{The species moves faster than the
#' global pool, or moves in a significantly different direction that is
#' poleward, consistent with a climate-driven distributional shift.}
#' \item{\strong{SD} (Spatial Discordance)}{The species moves slower than the
#' global pool, or moves in a significantly different direction that is
#' non-poleward, inconsistent with strict climatic control of distribution.}
#' \item{\strong{SC} (Spatial Conformance)}{Neither speed nor direction differs
#' significantly from the global spatial pattern.}
#' }
#'
#' The ECEF coordinate system used here places the X-axis towards the intersection
#' of the Equator and the Greenwich meridian, the Y-axis towards 90°E longitude,
#' and the Z-axis towards the North Pole.
#'
#' @param data A \code{data.frame} containing occurrence records. Must include
#' the columns \code{Species}, \code{Year}, \code{Month}, \code{Latitude},
#' and \code{Longitude} (additional columns are permitted and ignored).
#' \describe{
#' \item{\code{Species}}{Character or factor. Taxon name identifying each
#' occurrence record.}
#' \item{\code{Year}}{Numeric. Four-digit calendar year of the record.
#' Values that cannot be coerced to a finite number are dropped.}
#' \item{\code{Month}}{Numeric. Calendar month (1–12) of the record.
#' Values outside this range or non-numeric entries are dropped.}
#' \item{\code{Latitude}}{Numeric. Decimal degrees latitude (−90 to +90).
#' Records outside this range are dropped.}
#' \item{\code{Longitude}}{Numeric. Decimal degrees longitude (−180 to +180).
#' Records outside this range are dropped.}
#' }
#'
#' @param spp Character vector of species names to analyse. When \code{NULL}
#' (default) all species present in \code{data} are considered. Species listed
#' here that are absent from \code{data} are silently ignored after a warning.
#'
#' @param min_records Integer (default \code{20}). Minimum total number of valid
#' occurrence records a species must have across the entire dataset to be
#' retained for analysis. Species with fewer records are excluded and flagged
#' in \code{$species_filter}.
#'
#' @param min_years Integer (default \code{5}). Minimum number of distinct
#' calendar years in which a species must have been recorded to be retained
#' for analysis. Species spanning fewer years are excluded and flagged in
#' \code{$species_filter}. The same threshold is applied to the global pool's
#' temporal centroids.
#'
#' @param spatial_simulation_n Integer (default \code{1000}). Number of
#' multivariate normal random vectors drawn from the slope covariance matrix
#' to estimate the probability distributions of speed and directional
#' differences relative to the global reference. Larger values reduce Monte
#' Carlo noise at the cost of computation time.
#'
#' @param spatial_probability_threshold Numeric in (0, 1) (default \code{0.90}).
#' Proportion of simulations that must support a speed or directional
#' difference for that difference to be considered statistically significant
#' when assigning the spatial class. A conservative default of 0.90 is used
#' to reduce overinterpretation of uncertain trends.
#'
#' @param direction_angle_threshold_deg Numeric (default \code{68}). Minimum
#' three-dimensional angle (degrees) between a species ECEF slope vector and
#' the global ECEF slope vector for their directions to be considered
#' different. Angles below this threshold indicate roughly parallel movement;
#' angles at or above it indicate orthogonal or opposite movement.
#'
#' @param random_seed Integer or \code{NULL} (default \code{NULL}). If a
#' non-null integer is supplied it is passed to \code{\link[base]{set.seed}}
#' before the Monte Carlo simulations, ensuring reproducible results.
#'
#' @param verbose Logical (default \code{TRUE}). If \code{TRUE}, a progress
#' message summarising the number of species retained after filtering is
#' printed to the console via \code{\link[base]{message}}.
#'
#' @return A named list with three elements:
#'
#' \describe{
#' \item{\strong{\code{spatial}}}{A \code{data.frame} with one row per
#' entity analysed. The first row always corresponds to the global pooled
#' reference (\code{Species == "__GLOBAL_POOL__"}); subsequent rows
#' correspond to individual species. Columns:
#' \describe{
#' \item{\code{Species}}{Character. Species name, or \code{"__GLOBAL_POOL__"}
#' for the global reference row.}
#' \item{\code{analysis}}{Character. Identifies the type of analysis:
#' \code{"spatial_ecef_centroid_vector_global_pool"} for the global row
#' or \code{"spatial_ecef_centroid_vector_species"} for individual
#' species.}
#' \item{\code{n_records}}{Integer. Total number of valid occurrence
#' records used. For the global pool this is the sum across all
#' temporal centroids.}
#' \item{\code{n_time_steps}}{Integer. Number of year-month combinations
#' (temporal centroids) used to fit the ECEF slope model.}
#' \item{\code{centroid_Longitude}}{Numeric. Decimal degrees longitude of
#' the mean ECEF centroid, converted back to geodetic coordinates.}
#' \item{\code{centroid_Latitude}}{Numeric. Decimal degrees latitude of
#' the mean ECEF centroid, converted back to geodetic coordinates.}
#' \item{\code{hemisphere}}{Character. \code{"Northern"} if
#' \code{centroid_Latitude >= 0}, otherwise \code{"Southern"}.}
#' \item{\code{poleward_status}}{Character. \code{"poleward"} when the
#' northward component of the displacement vector is positive in the
#' Northern Hemisphere or negative in the Southern Hemisphere;
#' \code{"non_poleward"} otherwise. \code{NA} for the global pool row.}
#' \item{\code{slope_ecef_x_km_year}}{Numeric. Annual rate of change
#' (km yr\eqn{^{-1}}) of the ECEF X-coordinate centroid, estimated from
#' the multivariate linear regression.}
#' \item{\code{slope_ecef_y_km_year}}{Numeric. Annual rate of change
#' (km yr\eqn{^{-1}}) of the ECEF Y-coordinate centroid.}
#' \item{\code{slope_ecef_z_km_year}}{Numeric. Annual rate of change
#' (km yr\eqn{^{-1}}) of the ECEF Z-coordinate centroid.}
#' \item{\code{speed_chord_km_year}}{Numeric. Magnitude of the 3-D ECEF
#' slope vector (km yr\eqn{^{-1}}), representing the straight-line chord
#' displacement speed through the Earth's interior. Equal to
#' \eqn{\sqrt{\dot{X}^2 + \dot{Y}^2 + \dot{Z}^2}}.}
#' \item{\code{speed_surface_km_year}}{Numeric. Horizontal surface
#' displacement speed (km yr\eqn{^{-1}}) obtained by projecting the ECEF
#' slope vector onto the local tangent plane at the centroid and taking
#' the magnitude of the east and north components. This is the
#' ecologically preferred speed metric.}
#' \item{\code{direction_bearing_deg}}{Numeric. Absolute geographical
#' bearing (0–360°, clockwise from North) of the displacement vector
#' on the local tangent plane. \code{NA} when the surface speed is zero
#' or not finite.}
#' \item{\code{direction_cardinal}}{Character. Eight-point compass
#' direction corresponding to \code{direction_bearing_deg}: one of
#' \code{"N"}, \code{"NE"}, \code{"E"}, \code{"SE"}, \code{"S"},
#' \code{"SW"}, \code{"W"}, or \code{"NW"}.}
#' \item{\code{east_km_year}}{Numeric. Eastward component (km yr\eqn{^{-1}})
#' of the tangent-plane displacement vector. Positive values indicate
#' eastward movement.}
#' \item{\code{north_km_year}}{Numeric. Northward component (km yr\eqn{^{-1}})
#' of the tangent-plane displacement vector. Positive values indicate
#' northward movement in geographic coordinates.}
#' \item{\code{radial_km_year}}{Numeric. Radial (vertical) component
#' (km yr\eqn{^{-1}}) of the ECEF slope vector projected onto the local
#' up direction. Non-zero values indicate apparent elevation change and
#' should be close to zero for purely horizontal distributional shifts.}
#' \item{\code{global_speed_surface_km_year_full}}{Numeric. Surface
#' displacement speed (km yr\eqn{^{-1}}) of the global reference vector
#' (estimated from all retained records). Provided as a fixed reference
#' for comparison. \code{NA} for the global pool row itself.}
#' \item{\code{speed_diff_surface_km_year}}{Numeric. Difference between
#' the species surface speed and the global surface speed
#' (km yr\eqn{^{-1}}). Positive values indicate the species is moving
#' faster than the global pattern. \code{NA} for the global pool row.}
#' \item{\code{speed_ratio_to_global}}{Numeric. Ratio of the species
#' surface speed to the global surface speed. A value greater than 1
#' indicates faster movement than the global reference. \code{NA} when
#' the global speed is zero or for the global pool row.}
#' \item{\code{angle_to_global_3d_deg}}{Numeric. Three-dimensional angle
#' (degrees, 0–180) between the species ECEF slope vector and the global
#' ECEF slope vector. Values near 0° indicate nearly parallel movement;
#' values near 90° indicate orthogonal movement; values near 180°
#' indicate opposite movement. \code{NA} for the global pool row.}
#' \item{\code{prob_speed_greater_than_global}}{Numeric (0–1). Estimated
#' probability, derived from \code{spatial_simulation_n} Monte Carlo
#' draws, that the species surface speed exceeds the global surface
#' speed. \code{NA} for the global pool row.}
#' \item{\code{prob_speed_lower_than_global}}{Numeric (0–1). Estimated
#' probability that the species surface speed is lower than the global
#' surface speed. \code{NA} for the global pool row.}
#' \item{\code{prob_direction_different_from_global}}{Numeric (0–1).
#' Estimated probability that the three-dimensional angle between the
#' species slope vector and the global slope vector is at least
#' \code{direction_angle_threshold_deg} degrees, indicating directional
#' divergence from the global pattern. \code{NA} for the global pool
#' row.}
#' \item{\code{direction_angle_threshold_deg}}{Numeric. The value of the
#' \code{direction_angle_threshold_deg} argument used in the analysis,
#' stored here for traceability.}
#' \item{\code{spatial_class_Latitudelon}}{Character. Joint spatial
#' classification assigned to each species: \code{"SA"} (Spatial
#' Adaptation), \code{"SD"} (Spatial Discordance), or \code{"SC"}
#' (Spatial Conformance). See Details for the full decision hierarchy.
#' \code{NA} for the global pool row.}
#' }
#' }
#'
#' \item{\strong{\code{species_filter}}}{A \code{data.frame} with one row per
#' species found in \code{data} (after optional \code{spp} subsetting).
#' Records whether each species passed the data-sufficiency filters.
#' Columns:
#' \describe{
#' \item{\code{Species}}{Character. Species name.}
#' \item{\code{n_records}}{Integer. Total number of valid records for
#' this species after cleaning.}
#' \item{\code{n_years}}{Integer. Number of distinct calendar years in
#' which the species was recorded.}
#' \item{\code{retained}}{Logical. \code{TRUE} if the species met both
#' \code{min_records} and \code{min_years} thresholds and was included
#' in the spatial analysis; \code{FALSE} otherwise.}
#' }
#' }
#'
#' \item{\strong{\code{metadata}}}{A named list documenting analysis settings
#' and methodological details. Elements:
#' \describe{
#' \item{\code{analysis}}{Character. Fixed label
#' \code{"spatial_ecef_wgs84_vector_only"}.}
#' \item{\code{time_variable}}{Character. Description of the continuous
#' time variable: \code{"time_cont = Year + (Month - 1) / 12"}.}
#' \item{\code{coordinate_system}}{Character. Coordinate system and units:
#' \code{"WGS84 ECEF, kilometres"}.}
#' \item{\code{model}}{Character. Formula of the multivariate regression:
#' \code{"cbind(ecef_x_km, ecef_y_km, ecef_z_km) ~ time_cont"}.}
#' \item{\code{joint_spatial_classification}}{Character. Human-readable
#' description of the hierarchical decision rule used to assign SA, SD,
#' or SC, including the threshold values applied.}
#' \item{\code{spatial_simulation_n}}{Integer. Value of the
#' \code{spatial_simulation_n} argument used.}
#' \item{\code{spatial_probability_threshold}}{Numeric. Value of the
#' \code{spatial_probability_threshold} argument used.}
#' \item{\code{direction_angle_threshold_deg}}{Numeric. Value of the
#' \code{direction_angle_threshold_deg} argument used.}
#' \item{\code{min_records}}{Integer. Value of the \code{min_records}
#' argument used.}
#' \item{\code{min_years}}{Integer. Value of the \code{min_years}
#' argument used.}
#' }
#' }
#' }
#'
#' @details
#' \subsection{Coordinate transformation}{
#' Latitude and longitude (decimal degrees, WGS84) are converted to ECEF
#' Cartesian coordinates (kilometres) using the standard geodetic
#' transformation with WGS84 parameters: semi-major axis
#' \eqn{a = 6378.137} km and flattening \eqn{f = 1/298.257223563}.
#' The prime vertical radius of curvature
#' \eqn{N(\phi) = a / \sqrt{1 - e^2 \sin^2\phi}} accounts for the
#' ellipsoidal shape of the Earth.
#' }
#'
#' \subsection{Temporal centroids}{
#' Records are grouped by species and year-month combination. Within each
#' group the arithmetic mean of the ECEF coordinates is computed to produce
#' a temporal centroid. Groups with fewer records than
#' \eqn{\lfloor \texttt{min\_records} / \max(1, \texttt{min\_years}) \rfloor}
#' are discarded. Global-pool centroids use all retained species combined.
#' }
#'
#' \subsection{ECEF slope model}{
#' A multivariate ordinary least-squares regression is fitted:
#' \deqn{\mathbf{C}(t) = \boldsymbol{\alpha} + \boldsymbol{\beta}\, t + \boldsymbol{\varepsilon}}
#' where \eqn{\mathbf{C}(t)} is the \eqn{3 \times 1} vector of ECEF
#' coordinates at time \eqn{t}, and \eqn{\boldsymbol{\beta}} is the slope
#' vector (km yr\eqn{^{-1}}) of interest. The residual covariance matrix
#' \eqn{\hat{\boldsymbol{\Sigma}}} is used together with
#' \eqn{(X^\top X)^{-1}} to construct the \eqn{3 \times 3} covariance
#' matrix of \eqn{\hat{\boldsymbol{\beta}}}, which propagates uncertainty
#' into the Monte Carlo simulations.
#' }
#'
#' \subsection{Tangent-plane projection}{
#' The ECEF slope vector is projected onto the local east–north–up frame at
#' the species centroid. The east and north components are used to compute
#' surface speed and bearing; the up component is reported as
#' \code{radial_km_year}.
#' }
#'
#' \subsection{Spatial classification hierarchy}{
#' \enumerate{
#' \item \strong{Direction test.} If
#' \code{prob_direction_different_from_global >= spatial_probability_threshold}
#' \emph{and} \code{angle_to_global_3d_deg >= direction_angle_threshold_deg},
#' the direction is considered significantly different. Under this
#' condition, poleward movement → \strong{SA}; non-poleward movement →
#' \strong{SD}.
#' \item \strong{Speed test (direction not significant).} If
#' \code{prob_speed_greater_than_global >= spatial_probability_threshold},
#' poleward movement → \strong{SA}; non-poleward → \strong{SD}. If
#' \code{prob_speed_lower_than_global >= spatial_probability_threshold}
#' → \strong{SD}.
#' \item \strong{Default.} Neither speed nor direction differs significantly
#' → \strong{SC}.
#' }
#' Poleward movement is defined as a positive northward component in the
#' Northern Hemisphere or a negative northward component in the Southern
#' Hemisphere.
#' }
#'
#' @examples
#' # Minimal example with simulated data
#' set.seed(42)
#' n <- 600
#' sim_data <- data.frame(Species = sample(c("Sp_A", "Sp_B", "Sp_C"), n, replace = TRUE),
#' Year = sample(2000:2020, n, replace = TRUE),
#' Month = sample(1:12, n, replace = TRUE),
#' Latitude = runif(n, 30, 65),
#' Longitude = runif(n, -10, 30)
#' )
#'
#' result <- spp_trend_spatial(
#' data= sim_data,
#' min_records = 20,
#' min_years = 5,
#' spatial_simulation_n = 100,
#' spatial_probability_threshold = 0.90,
#' direction_angle_threshold_deg = 68,
#' random_seed = 1,
#' verbose = FALSE
#' )
#'
#' # Main results table
#' head(result$spatial)
#'
#' # Species filtering summary
#' result$species_filter
#'
#' # Analysis metadata
#' result$metadata
#'
#' # Extract classified species only (excluding global pool row)
#' classified <- result$spatial[result$spatial$Species != "__GLOBAL_POOL__", ]
#' table(classified$spatial_class_Latitudelon)
#'
#' @seealso
#' \code{\link[stats]{lm}} for the underlying regression engine.
#'
#' @importFrom stats lm coef model.matrix residuals df.residual rnorm
#'
#' @export
spp_trend_spatial <- function(data,
                                   spp = NULL,
                                   min_records = 20,
                                   min_years = 5,
                                   spatial_simulation_n = 1000,
                                   spatial_probability_threshold = 0.90,
                                   direction_angle_threshold_deg = 68,
                                   random_seed = NULL,
                                   verbose = TRUE) {
  required_cols <- c("Species", "Year", "Month", "Latitude", "Longitude")
  missing_required <- setdiff(required_cols, names(data))

  if (length(missing_required) > 0) {
    stop(
      "Critical error: required column(s) not found: ",
      paste(missing_required, collapse = ", ")
    )
  }

  .as_numeric_clean <- function(x) {
    if (is.data.frame(x))
      x <- x[[1]]
    if (is.factor(x))
      x <- as.character(x)
    if (is.character(x)) {
      x <- trimws(x)
      x <- gsub("\\s+", "", x)
      x <- gsub(",", ".", x, fixed = TRUE)
    }
    suppressWarnings(as.numeric(x))
  }

  .get_required_col <- function(data, colname) {
    idx <- which(names(data) == colname)
    if (length(idx) == 0)
      stop("Required column not found: ", colname)
    data[[idx[1]]]
  }

  .safe_rbind <- function(x) {
    x <- Filter(function(z)
      is.data.frame(z) && nrow(z) > 0, x)
    if (length(x) == 0)
      return(data.frame())
    do.call(rbind, x)
  }

  .direction_label <- function(bearing_deg) {
    if (!is.finite(bearing_deg) ||
        is.na(bearing_deg))
      return(NA_character_)
    dirs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    dirs[((floor(((bearing_deg %% 360) + 22.5
    ) / 45) %% 8) + 1)]
  }

  .lonlat_to_ecef_wgs84 <- function(longitude_deg,
                                    latitude_deg,
                                    height_km = 0) {
    a <- 6378.137
    f <- 1 / 298.257223563
    e2 <- f * (2 - f)

    longitude_rad <- longitude_deg * pi / 180
    latitude_rad <- latitude_deg * pi / 180

    sin_lat <- sin(latitude_rad)
    cos_lat <- cos(latitude_rad)
    N <- a / sqrt(1 - e2 * sin_lat^2)

    data.frame(
      ecef_x_km = (N + height_km) * cos_lat * cos(longitude_rad),
      ecef_y_km = (N + height_km) * cos_lat * sin(longitude_rad),
      ecef_z_km = (N * (1 - e2) + height_km) * sin_lat
    )
  }

  .ecef_to_geodetic_wgs84 <- function(x, y, z) {
    a <- 6378.137
    f <- 1 / 298.257223563
    b <- a * (1 - f)
    e2 <- f * (2 - f)
    ep2 <- (a^2 - b^2) / b^2

    p <- sqrt(x^2 + y^2)
    if (!all(is.finite(c(x, y, z, p))) || p == 0) {
      return(c(Longitude = NA_real_, Latitude = NA_real_))
    }

    theta <- atan2(z * a, p * b)
    longitude <- atan2(y, x)
    latitude <- atan2(z + ep2 * b * sin(theta)^3, p - e2 * a * cos(theta)^3)

    c(Longitude = longitude * 180 / pi, Latitude = latitude * 180 / pi)
  }
  .local_tangent_components <- function(vx, vy, vz, longitude_deg, latitude_deg) {
    if (!all(is.finite(c(
      vx, vy, vz, longitude_deg, latitude_deg
    )))) {
      return(
        list(
          east = NA_real_,
          north = NA_real_,
          up = NA_real_,
          speed = NA_real_,
          bearing = NA_real_,
          cardinal = NA_character_
        )
      )
    }

    lon <- longitude_deg * pi / 180
    lat <- latitude_deg * pi / 180

    east <- c(-sin(lon), cos(lon), 0)
    north <- c(-sin(lat) * cos(lon), -sin(lat) * sin(lon), cos(lat))
    up <- c(cos(lat) * cos(lon), cos(lat) * sin(lon), sin(lat))

    v <- c(vx, vy, vz)
    ve <- sum(v * east)
    vn <- sum(v * north)
    vu <- sum(v * up)
    speed <- sqrt(ve^2 + vn^2)
    bearing <- if (is.finite(speed) &&
                   speed > 0)
      ((atan2(ve, vn) * 180 / pi) + 360) %% 360
    else
      NA_real_

    list(
      east = ve,
      north = vn,
      up = vu,
      speed = speed,
      bearing = bearing,
      cardinal = .direction_label(bearing)
    )
  }

  .local_tangent_components_matrix <- function(B, longitude_deg, latitude_deg) {
    if (is.null(B) ||
        nrow(B) == 0 || !all(is.finite(c(longitude_deg, latitude_deg)))) {
      return(
        data.frame(
          east = numeric(0),
          north = numeric(0),
          up = numeric(0),
          speed = numeric(0),
          bearing = numeric(0)
        )
      )
    }

    lon <- longitude_deg * pi / 180
    lat <- latitude_deg * pi / 180

    east <- c(-sin(lon), cos(lon), 0)
    north <- c(-sin(lat) * cos(lon), -sin(lat) * sin(lon), cos(lat))
    up <- c(cos(lat) * cos(lon), cos(lat) * sin(lon), sin(lat))

    ve <- as.numeric(B %*% east)
    vn <- as.numeric(B %*% north)
    vu <- as.numeric(B %*% up)
    speed <- sqrt(ve^2 + vn^2)
    bearing <- ifelse(is.finite(speed) &
                        speed > 0, ((atan2(ve, vn) * 180 / pi) + 360) %% 360, NA_real_)

    data.frame(
      east = ve,
      north = vn,
      up = vu,
      speed = speed,
      bearing = bearing
    )
  }

  .angle_between_vectors_3d_deg <- function(v1, v2) {
    if (!all(is.finite(c(v1, v2))))
      return(NA_real_)
    n1 <- sqrt(sum(v1^2))
    n2 <- sqrt(sum(v2^2))
    if (!is.finite(n1) ||
        !is.finite(n2) || n1 <= 0 || n2 <= 0)
      return(NA_real_)
    cosang <- max(min(sum(v1 * v2) / (n1 * n2), 1), -1)
    acos(cosang) * 180 / pi
  }

  .angle_between_vectors_3d_deg_matrix <- function(B1, B2) {
    if (is.null(B1) || is.null(B2) || nrow(B1) != nrow(B2)) {
      return(rep(NA_real_, max(nrow(B1), nrow(B2))))
    }

    dot <- rowSums(B1 * B2)
    n1 <- sqrt(rowSums(B1^2))
    n2 <- sqrt(rowSums(B2^2))
    cosang <- dot / (n1 * n2)
    cosang[!is.finite(cosang)] <- NA_real_
    cosang <- pmin(pmax(cosang, -1), 1)
    acos(cosang) * 180 / pi
  }

  .vector_summary_ecef <- function(slope_x,
                                   slope_y,
                                   slope_z,
                                   centroid_longitude,
                                   centroid_latitude) {
    chord_speed <- if (all(is.finite(c(slope_x, slope_y, slope_z)))) {
      sqrt(slope_x^2 + slope_y^2 + slope_z^2)
    } else {
      NA_real_
    }

    tangent <- .local_tangent_components(slope_x,
                                         slope_y,
                                         slope_z,
                                         centroid_longitude,
                                         centroid_latitude)

    interpretable <- is.finite(tangent$speed) && tangent$speed > 0

    list(
      chord_speed = chord_speed,
      surface_speed = tangent$speed,
      bearing_deg = if (interpretable)
        tangent$bearing
      else
        NA_real_,
      direction_cardinal = if (interpretable)
        tangent$cardinal
      else
        NA_character_,
      direction_interpretable = interpretable,
      east_km_year = tangent$east,
      north_km_year = tangent$north,
      radial_km_year = tangent$up
    )
  }
  .temporal_ecef_centroids <- function(df, min_records_local = 1) {
    df <- df[is.finite(df$Latitude) & is.finite(df$Longitude) &
               df$Latitude >= -90 & df$Latitude <= 90 &
               df$Longitude >= -180 & df$Longitude <= 180 &
               is.finite(df$time_cont), , drop = FALSE]

    if (nrow(df) == 0)
      return(data.frame())

    xyz <- .lonlat_to_ecef_wgs84(df$Longitude, df$Latitude)
    df$ecef_x_km <- xyz$ecef_x_km
    df$ecef_y_km <- xyz$ecef_y_km
    df$ecef_z_km <- xyz$ecef_z_km

    groups <- split(df, interaction(df$Year, df$Month, drop = TRUE))

    out <- lapply(groups, function(g) {
      if (nrow(g) < min_records_local)
        return(NULL)

      cx <- mean(g$ecef_x_km, na.rm = TRUE)
      cy <- mean(g$ecef_y_km, na.rm = TRUE)
      cz <- mean(g$ecef_z_km, na.rm = TRUE)
      ll <- .ecef_to_geodetic_wgs84(cx, cy, cz)

      data.frame(
        time_cont = mean(g$time_cont, na.rm = TRUE),
        Year_integer = unique(g$Year_integer)[1],
        Year = unique(g$Year)[1],
        Month = unique(g$Month)[1],
        n_records = nrow(g),
        centroid_Longitude = ll[["Longitude"]],
        centroid_Latitude = ll[["Latitude"]],
        ecef_x_km = cx,
        ecef_y_km = cy,
        ecef_z_km = cz,
        stringsAsFactors = FALSE
      )
    })

    .safe_rbind(out)
  }

  .fit_ecef_slope_model <- function(df) {
    empty <- list(slope = c(NA_real_, NA_real_, NA_real_),
                  vcov = matrix(NA_real_, 3, 3))

    df <- df[is.finite(df$time_cont) &
               is.finite(df$ecef_x_km) &
               is.finite(df$ecef_y_km) &
               is.finite(df$ecef_z_km), , drop = FALSE]

    if (nrow(df) < 4 || length(unique(df$time_cont)) < 4)
      return(empty)

    fit <- tryCatch(
      stats::lm(cbind(ecef_x_km, ecef_y_km, ecef_z_km) ~ time_cont, data = df),
      error = function(e)
        NULL
    )

    if (is.null(fit))
      return(empty)

    co <- stats::coef(fit)
    if (!"time_cont" %in% rownames(co))
      return(empty)

    slope <- as.numeric(co["time_cont", ])
    X <- stats::model.matrix(fit)
    XtX_inv <- tryCatch(
      solve(crossprod(X)),
      error = function(e)
        NULL
    )

    if (is.null(XtX_inv) ||
        !"time_cont" %in% colnames(XtX_inv))
      return(empty)

    resid_mat <- stats::residuals(fit)
    df_res <- stats::df.residual(fit)
    if (!is.finite(df_res) || df_res <= 0)
      return(empty)

    Sigma_res <- crossprod(resid_mat) / df_res
    V_slope <- as.numeric(XtX_inv["time_cont", "time_cont"]) * Sigma_res

    list(slope = slope, vcov = V_slope)
  }

  .rmvnorm_from_slope <- function(n, mu, Sigma) {
    if (!all(is.finite(mu)) || !all(is.finite(Sigma)) || n < 1) {
      return(matrix(NA_real_, nrow = n, ncol = length(mu)))
    }

    Sigma <- (Sigma + t(Sigma)) / 2
    eg <- eigen(Sigma, symmetric = TRUE)
    vals <- pmax(eg$values, .Machine$double.eps)
    A <- eg$vectors %*% diag(sqrt(vals), nrow = length(vals))
    Z <- matrix(stats::rnorm(n * length(mu)),
                nrow = n,
                ncol = length(mu))
    sweep(Z %*% t(A), 2, mu, `+`)
  }

  .is_poleward_movement <- function(latitude_deg, north_km_year) {
    if (!is.finite(latitude_deg) ||
        !is.finite(north_km_year))
      return(NA)
    if (latitude_deg >= 0 && north_km_year > 0)
      return(TRUE)
    if (latitude_deg < 0 && north_km_year < 0)
      return(TRUE)
    FALSE
  }

  .joint_spatial_class <- function(prob_speed_greater,
                                   prob_speed_lower,
                                   prob_direction_different,
                                   angle_to_global_deg,
                                   latitude_deg,
                                   north_km_year,
                                   probability_threshold = 0.90,
                                   direction_angle_threshold_deg = 68) {
    direction_is_significant <- (
      is.finite(prob_direction_different) &&
        prob_direction_different >= probability_threshold &&
        is.finite(angle_to_global_deg) &&
        angle_to_global_deg >= direction_angle_threshold_deg
    )

    if (isTRUE(direction_is_significant)) {
      poleward <- .is_poleward_movement(latitude_deg, north_km_year)
      if (isTRUE(poleward))
        return("SA")
      if (isFALSE(poleward))
        return("SD")
      return("SD")
    }

    if (is.finite(prob_speed_greater) &&
        prob_speed_greater >= probability_threshold) {
      poleward <- .is_poleward_movement(latitude_deg, north_km_year)
      if (isTRUE(poleward))
        return("SA")
      if (isFALSE(poleward))
        return("SD")
      return("SD")
    }

    if (is.finite(prob_speed_lower) &&
        prob_speed_lower >= probability_threshold)
      return("SD")

    "SC"
  }

  data$Species <- as.character(.get_required_col(data, "Species"))
  data$Year <- .as_numeric_clean(.get_required_col(data, "Year"))
  data$Month <- .as_numeric_clean(.get_required_col(data, "Month"))
  data$Latitude <- .as_numeric_clean(.get_required_col(data, "Latitude"))
  data$Longitude <- .as_numeric_clean(.get_required_col(data, "Longitude"))

  core_ok <- !is.na(data$Species) & data$Species != "" &
    is.finite(data$Year) &
    is.finite(data$Month) & data$Month >= 1 & data$Month <= 12 &
    is.finite(data$Latitude) &
    data$Latitude >= -90 & data$Latitude <= 90 &
    is.finite(data$Longitude) &
    data$Longitude >= -180 & data$Longitude <= 180

  data <- data[core_ok, , drop = FALSE]

  if (!is.null(spp)) {
    spp <- as.character(spp)
    found_spp <- intersect(spp, unique(data$Species))
    if (length(found_spp) == 0) {
      stop("Critical error: none of the Species specified in 'spp' was found in the data.")
    }
    if (length(found_spp) < length(spp)) {
      warning("Some Species specified in 'spp' were not found in the data and will be ignored.")
    }
    data <- data[data$Species %in% found_spp, , drop = FALSE]
  }

  if (nrow(data) == 0) {
    warning(
      "No valid records remain after filtering Species, Year, Month, Latitude and Longitude."
    )
    return(list(
      spatial = data.frame(),
      species_filter = data.frame(),
      metadata = list()
    ))
  }

  data$time_cont <- data$Year + (data$Month - 1) / 12
  data$Year_integer <- floor(data$time_cont)

  species_filter <- .safe_rbind(lapply(split(data, data$Species), function(x) {
    data.frame(
      Species = x$Species[1],
      n_records = nrow(x),
      n_years = length(unique(x$Year_integer)),
      retained = nrow(x) >= min_records &&
        length(unique(x$Year_integer)) >= min_years,
      stringsAsFactors = FALSE
    )
  }))

  valid_species <- species_filter$Species[species_filter$retained]
  spatial_data <- data[data$Species %in% valid_species, , drop = FALSE]

  if (verbose) {
    message(
      "Spatial filtering summary: retained ",
      length(valid_species),
      " of ",
      length(unique(data$Species)),
      " species using min_records = ",
      min_records,
      " and min_years = ",
      min_years,
      "."
    )
  }

  if (nrow(spatial_data) == 0 || length(valid_species) == 0) {
    warning("No species retained for spatial ECEF analysis.")
    return(list(
      spatial = data.frame(),
      species_filter = species_filter,
      metadata = list()
    ))
  }

  if (!is.null(random_seed))
    set.seed(random_seed)
  global_temporal <- .temporal_ecef_centroids(spatial_data, min_records_local = max(1, floor(min_records / max(1, min_years))))

  if (nrow(global_temporal) < min_years) {
    warning("The retained global pool has too few temporal centroids for ECEF vector modelling.")
    return(list(
      spatial = data.frame(),
      species_filter = species_filter,
      metadata = list()
    ))
  }

  global_centroid <- c(
    mean(global_temporal$ecef_x_km, na.rm = TRUE),
    mean(global_temporal$ecef_y_km, na.rm = TRUE),
    mean(global_temporal$ecef_z_km, na.rm = TRUE)
  )

  global_ll <- .ecef_to_geodetic_wgs84(global_centroid[1], global_centroid[2], global_centroid[3])
  global_model <- .fit_ecef_slope_model(global_temporal)
  global_vec <- .vector_summary_ecef(
    global_model$slope[1],
    global_model$slope[2],
    global_model$slope[3],
    global_ll[["Longitude"]],
    global_ll[["Latitude"]]
  )

  spatial_results <- data.frame(
    Species = "__GLOBAL_POOL__",
    analysis = "spatial_ecef_centroid_vector_global_pool",
    n_records = sum(global_temporal$n_records),
    n_time_steps = nrow(global_temporal),
    centroid_Longitude = global_ll[["Longitude"]],
    centroid_Latitude = global_ll[["Latitude"]],
    hemisphere = ifelse(global_ll[["Latitude"]] >= 0, "Northern", "Southern"),
    poleward_status = NA_character_,
    slope_ecef_x_km_year = global_model$slope[1],
    slope_ecef_y_km_year = global_model$slope[2],
    slope_ecef_z_km_year = global_model$slope[3],
    speed_chord_km_year = global_vec$chord_speed,
    speed_surface_km_year = global_vec$surface_speed,
    direction_bearing_deg = global_vec$bearing_deg,
    direction_cardinal = global_vec$direction_cardinal,
    east_km_year = global_vec$east_km_year,
    north_km_year = global_vec$north_km_year,
    radial_km_year = global_vec$radial_km_year,
    global_speed_surface_km_year_full = NA_real_,
    speed_diff_surface_km_year = NA_real_,
    speed_ratio_to_global = NA_real_,
    angle_to_global_3d_deg = NA_real_,
    prob_speed_greater_than_global = NA_real_,
    prob_speed_lower_than_global = NA_real_,
    prob_direction_different_from_global = NA_real_,
    direction_angle_threshold_deg = direction_angle_threshold_deg,
    spatial_class_Latitudelon = NA_character_,
    stringsAsFactors = FALSE
  )

  for (sp in valid_species) {
    ind <- spatial_data[spatial_data$Species == sp, , drop = FALSE]
    spp_temporal <- .temporal_ecef_centroids(ind, min_records_local = 1)

    if (nrow(spp_temporal) < min_years)
      next

    spp_centroid <- c(
      mean(spp_temporal$ecef_x_km, na.rm = TRUE),
      mean(spp_temporal$ecef_y_km, na.rm = TRUE),
      mean(spp_temporal$ecef_z_km, na.rm = TRUE)
    )

    spp_ll <- .ecef_to_geodetic_wgs84(spp_centroid[1], spp_centroid[2], spp_centroid[3])
    spp_hemi <- ifelse(spp_ll[["Latitude"]] >= 0, "Northern", "Southern")
    spp_model <- .fit_ecef_slope_model(spp_temporal)

    species_vec <- .vector_summary_ecef(spp_model$slope[1],
                                        spp_model$slope[2],
                                        spp_model$slope[3],
                                        spp_ll[["Longitude"]],
                                        spp_ll[["Latitude"]])

    poleward_status <- ifelse(isTRUE(
      .is_poleward_movement(spp_ll[["Latitude"]], species_vec$north_km_year)
    ),
    "poleward",
    "non_poleward")

    global_full_speed <- global_vec$surface_speed
    speed_diff_surface <- species_vec$surface_speed - global_full_speed
    speed_ratio <- if (is.finite(global_full_speed) &&
                       global_full_speed > 0) {
      species_vec$surface_speed / global_full_speed
    } else {
      NA_real_
    }

    angle_to_global <- .angle_between_vectors_3d_deg(spp_model$slope, global_model$slope)

    prob_speed_greater <- NA_real_
    prob_speed_lower <- NA_real_
    prob_direction_different <- NA_real_
    spatial_class <- "SC"

    spp_sim <- .rmvnorm_from_slope(spatial_simulation_n, spp_model$slope, spp_model$vcov)
    glb_sim <- .rmvnorm_from_slope(spatial_simulation_n,
                                   global_model$slope,
                                   global_model$vcov)

    if (all(is.finite(c(spp_sim, glb_sim)))) {
      spp_tan <- .local_tangent_components_matrix(spp_sim, spp_ll[["Longitude"]], spp_ll[["Latitude"]])
      glb_tan <- .local_tangent_components_matrix(glb_sim, spp_ll[["Longitude"]], spp_ll[["Latitude"]])

      prob_speed_greater <- mean(spp_tan$speed > glb_tan$speed, na.rm = TRUE)
      prob_speed_lower <- mean(spp_tan$speed < glb_tan$speed, na.rm = TRUE)

      angle_sim <- .angle_between_vectors_3d_deg_matrix(spp_sim, glb_sim)
      prob_direction_different <- mean(angle_sim >= direction_angle_threshold_deg, na.rm = TRUE)

      spatial_class <- .joint_spatial_class(
        prob_speed_greater = prob_speed_greater,
        prob_speed_lower = prob_speed_lower,
        prob_direction_different = prob_direction_different,
        angle_to_global_deg = angle_to_global,
        latitude_deg = spp_ll[["Latitude"]],
        north_km_year = species_vec$north_km_year,
        probability_threshold = spatial_probability_threshold,
        direction_angle_threshold_deg = direction_angle_threshold_deg
      )
    }

    spatial_results <- rbind(
      spatial_results,
      data.frame(
        Species = sp,
        analysis = "spatial_ecef_centroid_vector_species",
        n_records = nrow(ind),
        n_time_steps = nrow(spp_temporal),
        centroid_Longitude = spp_ll[["Longitude"]],
        centroid_Latitude = spp_ll[["Latitude"]],
        hemisphere = spp_hemi,
        poleward_status = poleward_status,
        slope_ecef_x_km_year = spp_model$slope[1],
        slope_ecef_y_km_year = spp_model$slope[2],
        slope_ecef_z_km_year = spp_model$slope[3],
        speed_chord_km_year = species_vec$chord_speed,
        speed_surface_km_year = species_vec$surface_speed,
        direction_bearing_deg = species_vec$bearing_deg,
        direction_cardinal = species_vec$direction_cardinal,
        east_km_year = species_vec$east_km_year,
        north_km_year = species_vec$north_km_year,
        radial_km_year = species_vec$radial_km_year,
        global_speed_surface_km_year_full = global_full_speed,
        speed_diff_surface_km_year = speed_diff_surface,
        speed_ratio_to_global = speed_ratio,
        angle_to_global_3d_deg = angle_to_global,
        prob_speed_greater_than_global = prob_speed_greater,
        prob_speed_lower_than_global = prob_speed_lower,
        prob_direction_different_from_global = prob_direction_different,
        direction_angle_threshold_deg = direction_angle_threshold_deg,
        spatial_class_Latitudelon = spatial_class,
        stringsAsFactors = FALSE
      )
    )
  }

  metadata <- list(
    analysis = "spatial_ecef_wgs84_vector_only",
    time_variable = "time_cont = Year + (Month - 1) / 12",
    coordinate_system = "WGS84 ECEF, kilometres",
    model = "cbind(ecef_x_km, ecef_y_km, ecef_z_km) ~ time_cont",
    joint_spatial_classification = paste0(
      "Hierarchical direction-then-speed rule. Direction is considered significantly different when ",
      "P(angle_to_global_3d_deg >= ",
      direction_angle_threshold_deg,
      " degrees) >= ",
      spatial_probability_threshold,
      ". Species with significantly different poleward movement are classified as SA; ",
      "species with significantly different non-poleward movement are classified as SD. ",
      "If direction is not significantly different, SA is assigned when P(species speed > full global speed) >= ",
      spatial_probability_threshold,
      "; SD is assigned when P(species speed < full global speed) >= ",
      spatial_probability_threshold,
      "; otherwise SC is assigned."
    ),
    spatial_simulation_n = spatial_simulation_n,
    spatial_probability_threshold = spatial_probability_threshold,
    direction_angle_threshold_deg = direction_angle_threshold_deg,
    min_records = min_records,
    min_years = min_years
  )

  list(spatial = spatial_results,
       species_filter = species_filter,
       metadata = metadata)
}
