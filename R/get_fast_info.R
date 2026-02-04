#' @title Quick visual diagnostic of the input data
#'
#' @description This function provides a quick visual diagnostic of the input data.
#' It generates a map showing the spatial distribution of occurrence records together with a time-series plot derived from a NetCDF environmental dataste, including a linear trend analysis.
#' Using the geographic coordinates of the occurrence records, the function extracts the complete climate time-series (from the earliest to the latest year represented in the data) for the corresponding occupied cells.
#' All temperature values from occupied cells are then added annually to estimate and visualise the overall temperature trend (including slope and associated p-value).
#' This diagnostic step allows users to quickly assess the climate trajectory of the regions where the species have been recorded and to evaluate whether sufficient temporal and environmental variation is present for subsequent analyses.
#'
#' @param data A data frame containing species records. Must include `lon`, `lat`, `year`, and `month` columns.
#' @param nc_file Full `character` path to the downloaded ERA5-Land raster (`.nc`) file.
#'
#' @return Invisibly returns a composite plot.
#' Displays a composite plot showing the geographic distribution and the thermal trend with its corresponding global slope and p-value.
#'
#' @import ggplot2
#' @importFrom terra rast time nlyr vect crs extract
#' @importFrom dplyr %>% distinct mutate filter group_by summarise count
#' @importFrom patchwork plot_layout
#' @importFrom stats na.omit
#' @importFrom sf read_sf st_as_sf
#' @export
get_fast_info <- function(data, nc_file) {
  lon <- lat <- year <- lon_adj <- idx_lyr <- val <- ID <- temp_c <- temp_era <- n <- mean_temp <- NULL
  point_map <- data %>%
    dplyr::distinct(lon, lat, .keep_all = TRUE)
  data_coords_ext <- point_map %>%
    dplyr::mutate(lon_adj = ifelse(lon < 0, lon + 360, lon))
  r_stack <- terra::rast(nc_file)
  date_nc <- terra::time(r_stack)

  if (all(is.na(date_nc))) {
    date_nc <- seq(as.Date("1950-01-01"),
                   by = "month",
                   length.out = terra::nlyr(r_stack))
  }
  df_tiempos <- data.frame(lyr_idx = 1:terra::nlyr(r_stack),
                           year = as.numeric(format(as.Date(date_nc), "%Y")))
  year_range <- range(data$year, na.rm = TRUE)
  lyr_f <- df_tiempos %>%
    dplyr::filter(year >= year_range[1] & year <= year_range[2])
  pts_vect <- terra::vect(
    data_coords_ext,
    geom = c("lon_adj", "lat"),
    crs = terra::crs(r_stack)
  )
  vals_cells <- terra::extract(r_stack[[lyr_f$lyr_idx]], pts_vect, ID = TRUE)
  df_long <- stats::reshape(
    vals_cells,
    direction = "long",
    varying = list(2:ncol(vals_cells)),
    v.names = "val",
    timevar = "idx_lyr"
  )

  df_era_anual <- df_long %>%
    dplyr::mutate(year = lyr_f$year[idx_lyr], temp_c = val - 273.15) %>%
    dplyr::group_by(ID, year) %>%
    dplyr::summarise(temp_era = mean(temp_c, na.rm = TRUE),
                     .groups = "drop")

  df_global_mean <- df_era_anual %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(mean_temp = mean(temp_era, na.rm = TRUE),
                     .groups = "drop")

  fit <- lm(mean_temp ~ year, data = df_global_mean)
  s_fit <- summary(fit)

  stats_label <- paste0(
    "Tme trend: ",
    round(coef(fit)[2], 4),
    " C/yr\n",
    "t(",
    s_fit$df[2],
    ") = ",
    round(s_fit$coefficients[2, 3], 2),
    "\n",
    "p = ",
    ifelse(
      round(s_fit$coefficients[2, 4], 4) < 0.001,
      "< 0.001",
      round(s_fit$coefficients[2, 4], 4)
    )
  )

  world <- sf::read_sf(system.file("extdata", "ne_land.shp", package = "SppTrend"))
  world <- sf::st_as_sf(world)
  p_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world,
                     fill = "#f9f9f9",
                     color = "grey80") +
    ggplot2::geom_point(data = point_map,
                        ggplot2::aes(x = lon, y = lat, color = year),
                        alpha = 0.4) +
    ggplot2::scale_color_viridis_c(option = "viridis", name = "Year", ) +
    ggplot2::coord_sf(
      xlim = range(point_map$lon, na.rm = TRUE) + c(-0.5, 0.5),
      ylim = range(point_map$lat, na.rm = TRUE) + c(-0.5, 0.5),
      expand = FALSE
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 0.5
      ),
      legend.title = ggplot2::element_text(vjust = 0.5, hjust = 0.5)
    )

  n_anual <- data %>% dplyr::count(year)
  y_min <- min(df_era_anual$temp_era, na.rm = TRUE)
  y_max <- max(df_era_anual$temp_era, na.rm = TRUE)

  p_trend <- ggplot2::ggplot(df_era_anual, ggplot2::aes(x = year, y = temp_era)) +
    ggplot2::geom_line(ggplot2::aes(group = ID),
                       color = "aquamarine3",
                       alpha = 0.05) +
    ggplot2::geom_smooth(method = "lm",
                         color = "aquamarine4",
                         se = FALSE) +
    ggplot2::geom_point(
      data = n_anual,
      ggplot2::aes(
        x = year,
        y = y_min - 0.5,
        size = n,
        color = n
      ),
      shape = 15,
      alpha = 0.7
    ) +
    ggplot2::annotate(
      "label",
      x = year_range[1],
      y = y_max,
      label = stats_label,
      hjust = 0,
      vjust = 1,
      size = 3,
      fill = "white",
      color = "black",
      alpha = 0.1
    ) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ ., breaks = y_min - 0.5, labels = "n")) +
    ggplot2::scale_x_continuous(breaks = seq(year_range[1], year_range[2], by = 2)) +
    ggplot2::scale_color_viridis_c(option = "mako", name = "n") +
    ggplot2::guides(
      size = "none",
      color = ggplot2::guide_colorbar(
        direction = "horizontal",
        title.position = "right",
        label.position = "top"
      )
    ) +
    ggplot2::labs(y = "Tme (C)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(vjust = 0.5, hjust = 0.5),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        vjust = 0.1,
        hjust = 0.1
      ),
      legend.text = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 0.5
      ),
      legend.title = ggplot2::element_text(vjust = 0.5, hjust = 0.5)
    )

  final_plot <- (p_map | p_trend) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
  suppressMessages(suppressWarnings(print(final_plot)))
  return(invisible(final_plot))
}
