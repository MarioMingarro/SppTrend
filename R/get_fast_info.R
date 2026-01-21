get_fast_info <- function(data_with_spp, nc_path) {
  puntos_mapa <- data_with_spp %>%
    dplyr::distinct(lon, lat, .keep_all = TRUE)

  lon_range <- range(puntos_mapa$lon, na.rm = TRUE)
  lat_range <- range(puntos_mapa$lat, na.rm = TRUE)

  data_coords_ext <- puntos_mapa %>%
    dplyr::mutate(lon_adj = ifelse(lon < 0, lon + 360, lon))

  presencias_anuales <- data_with_spp %>%
    dplyr::count(year, name = "n_presences")
  r_stack <- terra::rast(nc_path)
  fechas_nc <- terra::time(r_stack)
  if(all(is.na(fechas_nc))) {
    fechas_nc <- seq(as.Date("1950-01-01"), by = "month", length.out = terra::nlyr(r_stack))
  }

  df_tiempos <- data.frame(
    capa_idx = 1:terra::nlyr(r_stack),
    year = as.numeric(format(as.Date(fechas_nc), "%Y"))
  )

  range_years <- min(data_with_spp$year):max(data_with_spp$year)
  capas_filtradas <- df_tiempos %>% dplyr::filter(year %in% range_years)
  r_subset <- r_stack[[capas_filtradas$capa_idx]]
  pts_vect <- terra::vect(data_coords_ext, geom = c("lon_adj", "lat"), crs = terra::crs(r_stack))
  vals_cells <- terra::extract(r_subset, pts_vect, ID = TRUE)

  df_long <- reshape(vals_cells, direction = "long", varying = list(2:ncol(vals_cells)),
                     v.names = "val", timevar = "idx_capa", times = 1:(ncol(vals_cells)-1))

  df_era_anual <- df_long %>%
    dplyr::mutate(year = capas_filtradas$year[idx_capa], temp_c = val - 273.15) %>%
    dplyr::group_by(ID, year) %>%
    dplyr::summarise(temp_era = mean(temp_c, na.rm = TRUE), .groups = "drop")
  df_global_mean <- df_era_anual %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(mean_temp = mean(temp_era, na.rm = TRUE), .groups = "drop")

  fit <- lm(mean_temp ~ year, data = df_global_mean)
  s_fit <- summary(fit)

  slope <- s_fit$coefficients[2, 1]
  t_val <- s_fit$coefficients[2, 3]
  p_val <- s_fit$coefficients[2, 4]

  stats_label <- paste0(
    "Tme trend: ", round(slope, 4), " °C/yr\n",
    "t-value: ", round(t_val, 2), "\n",
    "p-value: ", round(p_val, 6)
  )
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  p_mapa <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world, fill = "#f9f9f9", color = "grey80") +
    ggplot2::geom_point(data = puntos_mapa, ggplot2::aes(x = lon, y = lat, color = year), size = 1.5, alpha = 0.4) +
    ggplot2::scale_color_viridis_c(option = "viridis", name = "Year",
                                   guide = ggplot2::guide_colorbar(order = 1, title.position = "left", barwidth = 10)) +
    ggplot2::coord_sf(xlim = c(lon_range[1] - 1, lon_range[2] + 1),
                      ylim = c(lat_range[1] - 1, lat_range[2] + 1), expand = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  y_min_val <- min(df_era_anual$temp_era, na.rm = TRUE)
  y_max_val <- max(df_era_anual$temp_era, na.rm = TRUE)

  p_trend <- ggplot2::ggplot() +
    ggplot2::geom_line(data = df_era_anual,
                       ggplot2::aes(x = year, y = temp_era, group = ID),
                       color = "aquamarine3", alpha = 0.1) +
    ggplot2::geom_smooth(data = df_era_anual,
                         ggplot2::aes(x = year, y = temp_era),
                         method = "lm", formula = y ~ x, color = "aquamarine4", linewidth = 1) +
    ggplot2::annotate("label",
                      x = min(range_years), y = y_max_val,
                      label = stats_label,
                      hjust = 0, vjust = 1,
                      size = 3, fill = "white", alpha = 0.1) +
    ggplot2::geom_point(data = presencias_anuales,
                        ggplot2::aes(x = year, y = y_min_val - 0.5,
                                     size = n_presences, color = n_presences),
                        alpha = 0.8, shape = 15) +
    ggplot2::scale_y_continuous(
      name = "Temperature (°C)",
      sec.axis = ggplot2::sec_axis(
        trans = ~ .,
        breaks = y_min_val - 0.5,
        labels = "n"
      )) +
    ggplot2::scale_color_viridis_c(option = "mako", name = "n",
                                   guide = ggplot2::guide_colorbar(order = 2, title.position = "right",
                                                                   label.position = "top", barwidth = 10)) +
    ggplot2::scale_size_continuous(range = c(4, 15), guide = "none") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
                   axis.title.x = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(5, 5, 5, 20))
  final_plot <- (p_mapa | p_trend) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom",
                   legend.box = "horizontal",
                   legend.title = ggplot2::element_text(vjust = 0.5),
                   legend.spacing.x = ggplot2::unit(0.1, "cm"))

  print(final_plot)
  return(invisible(df_era_anual))
}
