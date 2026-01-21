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
              color = "coral3", alpha = 0.9) +
    geom_smooth(data = df_era_anual, aes(x = year, y = temp_era),
                method = "lm", color = "darkred", se = FALSE, linewidth = 1.2) +
    geom_point(data = df_spp_anual, aes(x = year, y = temp_data),
              color = "darkgreen") +
    geom_smooth(data = df_spp_anual, aes(x = year, y = temp_data),
                method = "lm", color = "darkgreen", linetype = "dashed", se = FALSE) +
    labs(title = "Tendencia Filtrada: ERA5 vs Especies",
         subtitle = paste("Años con n >=", n_min, "registros. Periodo:", year_min, "-", year_max),
         x = "Año", y = "Temperatura (°C)") +
    theme_minimal()

  print(p)
  return(list(era = df_era_anual, spp = df_spp_anual, conteo = conteo_anual))
}

mi_grafico2 <- get_fast_trend(puntos_muestra, "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/ERA5_land/era5_land.nc",  n_min = 100)
data_with_spp=puntos_muestra
nc_path <- "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/ERA5_land/era5_land.nc"
#####


get_fast_trend <- function(data_with_spp, nc_path) {
  require(terra)
  require(dplyr)
  require(ggplot2)

  # 1. Coordenadas únicas de presencia (sin inventar grid)
  data_coords <- data_with_spp %>%
    distinct(lon, lat) %>%
    mutate(lon_adj = ifelse(lon < 0, lon + 360, lon))

  # 2. Cargar Raster y Fechas
  r_stack <- terra::rast(nc_path)
  fechas_nc <- terra::time(r_stack)

  if(all(is.na(fechas_nc))) {
    fechas_nc <- seq(as.Date("1950-01-01"), by = "month", length.out = terra::nlyr(r_stack))
  }

  df_tiempos <- data.frame(
    capa_idx = 1:terra::nlyr(r_stack),
    year = as.numeric(format(as.Date(fechas_nc), "%Y"))
  )

  # Rango completo: desde el primer año de especies hasta el último
  rango_años <- min(data_with_spp$year):max(data_with_spp$year)
  capas_filtradas <- df_tiempos[df_tiempos$year %in% rango_años, ]

  if(nrow(capas_filtradas) == 0) stop("El rango de años no coincide con el NetCDF.")

  r_subset <- r_stack[[capas_filtradas$capa_idx]]
  años_subset <- capas_filtradas$year

  # 3. Extracción en los puntos "sonda"
  pts_vect <- terra::vect(data_coords, geom = c("lon_adj", "lat"), crs = terra::crs(r_stack))
  cat("Extrayendo serie temporal completa para cada punto de presencia...\n")
  vals_cells <- terra::extract(r_subset, pts_vect, ID = TRUE)

  # 4. Procesamiento a formato largo (R base)
  df_long <- reshape(vals_cells,
                     direction = "long",
                     varying = list(2:ncol(vals_cells)),
                     v.names = "val",
                     timevar = "idx_capa",
                     times = 1:(ncol(vals_cells)-1))

  df_era_anual <- df_long %>%
    mutate(
      year = años_subset[idx_capa],
      temp_c = val - 273.15
    ) %>%
    filter(!is.na(temp_c)) %>%
    group_by(ID, year) %>%
    summarise(temp_era = mean(temp_c, na.rm = TRUE), .groups = "drop")

  # 5. Estadísticas en consola
  modelo_global <- lm(temp_era ~ year, data = df_era_anual)
  cat("\n================================================\n")
  cat("   ANÁLISIS DE TENDENCIA TEMPORAL (ERA5-LAND)   \n")
  cat("   Puntos analizados:", nrow(data_coords), "\n")
  cat("   Rango años:", min(rango_años), "-", max(rango_años), "\n")
  cat("================================================\n")
  print(summary(modelo_global))
  cat("================================================\n")

  # 6. Gráfico
  p <- ggplot(df_era_anual, aes(x = year, y = temp_era)) +
    geom_line(aes(group = ID), color = "red", alpha = 0.1) +
    geom_smooth(method = "lm", color = "darkred", linewidth = 1.2, se = TRUE) +
    labs(
      title = "Evolución Térmica en Puntos de Presencia",
      subtitle = "Líneas rojas: serie anual completa por celda | Línea oscura: tendencia global",
      x = "Año", y = "Temperatura (°C)"
    ) +
    theme_minimal()

  print(p)

  return(df_era_anual)
}
mi_grafico2 <- get_fast_trend(puntos_muestra, "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/ERA5_land/era5_land.nc")

library(ggplot2)
library(sf)
library(rnaturalearth)





#######################################


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
             color = "red", shape = 3, size = 2, stroke = 1, alpha = .2) +
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
