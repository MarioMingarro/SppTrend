library(terra)
library(dplyr)
library(ggplot2)

get_fast_trend <- function(data_with_spp, nc_path, n_min = 10) {

  r_stack <- terra::rast(nc_path)
  fechas_nc <- terra::time(r_stack)
  if(all(is.na(fechas_nc))) {
    warning("No se detectaron fechas en el NetCDF. Generando fechas mensuales desde 1950.")
    fechas_nc <- seq(as.Date("1950-01-01"), by = "month", length.out = terra::nlyr(r_stack))
  }

  fechas_standard <- as.Date(fechas_nc)
  años_nc <- as.numeric(format(fechas_standard, "%Y"))
  conteo_anual <- data_with_spp %>%
    group_by(year) %>%
    summarise(n = n(), .groups = "drop")

  años_validos <- conteo_anual %>%
    filter(n >= n_min) %>%
    pull(year)

  if(length(años_validos) == 0) {
    stop(paste("Ningún año tiene al menos", n_min, "registros."))
  }

  year_min <- min(años_validos)
  year_max <- max(años_validos)

  cat("Rango filtrado (n >=", n_min, "):", year_min, "-", year_max, "\n")
  indices_periodo <- which(años_nc %in% años_validos)

  if(length(indices_periodo) == 0) {
    stop("Los años filtrados de especies no coinciden con el archivo NetCDF.")
  }

  r_subset <- r_stack[[indices_periodo]]
  años_simples <- años_nc[indices_periodo]
  data_coords <- data_with_spp %>%
    mutate(lon_360 = (lon + 360) %% 360)
  grid_pts <- expand.grid(
    lon = seq(min(data_coords$lon_360), max(data_coords$lon_360), length.out = 100),
    lat = seq(min(data_coords$lat), max(data_coords$lat), length.out = 100)
  )
  pts_vect <- terra::vect(grid_pts, geom = c("lon", "lat"), crs = terra::crs(r_stack))

  cat("Extrayendo valores ERA5...\n")
  vals_cells <- terra::extract(r_subset, pts_vect, ID = TRUE, buffer = 10, fun = mean, na.rm = TRUE)
  colnames(vals_cells) <- c("ID", paste0("v", 1:(ncol(vals_cells)-1)))

  df_era_anual <- vals_cells %>%
    tidyr::pivot_longer(cols = starts_with("v"), names_to = "temp_idx", values_to = "temp_k") %>%
    mutate(
      idx = as.numeric(gsub("v", "", temp_idx)),
      year = años_simples[idx],
      temp_c = temp_k - 273.15
    ) %>%
    filter(!is.na(temp_c)) %>%
    group_by(ID, year) %>%
    summarise(temp_era = mean(temp_c, na.rm = TRUE), .groups = "drop")
  df_spp_anual <- data_with_spp %>%
    filter(year %in% años_validos) %>%
    group_by(year) %>%
    summarise(temp_data = mean(tme, na.rm = TRUE), .groups = "drop")
  p <- ggplot() +
    geom_line(data = df_era_anual, aes(x = year, y = temp_era, group = ID),
              color = "red", alpha = 0.05) +
    geom_smooth(data = df_era_anual, aes(x = year, y = temp_era),
                method = "lm", color = "darkred", se = FALSE, linewidth = 1.2) +
    geom_line(data = df_spp_anual, aes(x = year, y = temp_data),
              color = "green", linewidth = 1) +
    geom_smooth(data = df_spp_anual, aes(x = year, y = temp_data),
                method = "lm", color = "darkgreen", linetype = "dashed", se = FALSE) +
    labs(title = "Tendencia Filtrada: ERA5 vs Especies",
         subtitle = paste("Años con n >=", n_min, "registros. Periodo:", year_min, "-", year_max),
         x = "Año", y = "Temperatura (°C)") +
    theme_minimal()

  print(p)
  return(list(era = df_era_anual, spp = df_spp_anual, conteo = conteo_anual))
}

mi_grafico2 <- get_fast_trend(puntos_muestra, "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/ERA5_land/era5_land.nc",  n_min = 50)
data_with_spp=puntos_muestra
nc_path <- "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/ERA5_land/era5_land.nc"


library(ggplot2)
library(sf)
library(rnaturalearth)


margin <- 0.1
lon_range <- range(puntos_muestra$lon, na.rm = TRUE)
lat_range <- range(puntos_muestra$lat, na.rm = TRUE)

lon_buffer <- diff(lon_range) * margin
lat_buffer <- diff(lat_range) * margin

world <- ne_countries(scale = "medium", returnclass = "sf")

grid_lon <- seq(lon_range[1], lon_range[2], length.out = 100)
grid_lat <- seq(lat_range[1], lat_range[2], length.out = 100)
pts_malla <- expand.grid(lon = grid_lon, lat = grid_lat)

ggplot() +
  geom_sf(data = world, fill = "#f9f9f9", color = "grey80") +
  geom_point(data = pts_malla, aes(x = lon, y = lat),
             color = "red", shape = 3, size = 2, stroke = 1, alpha = .5) +
  geom_point(data = puntos_muestra, aes(x = lon, y = lat),
             color = "darkgreen", size = 1.5, alpha = 0.5) +
  coord_sf(xlim = c(lon_range[1] - lon_buffer, lon_range[2] + lon_buffer),
           ylim = c(lat_range[1] - lat_buffer, lat_range[2] + lat_buffer),
           expand = FALSE) +
  labs(title = "Zoom al Área de Estudio",
       subtitle = "Malla de extracción (rojo) sobre registros de especies (verde)",
       x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "aliceblue"))
