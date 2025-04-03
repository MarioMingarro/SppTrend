library(dplyr)
library(sf)

set.seed(123) # Para reproducibilidad

# Parámetros
num_spp <- 100
num_years <- 100
years <- 1900:(1900 + num_years - 1)

# Generar especies SC (sin tendencia)
num_spp_sc <- 33 # 80% de las especies serán SC
spp_sc <- paste0("spp_", 1:num_spp_sc, "_SC")
data_sc <- lapply(spp_sc, function(sp) {
  data.frame(
    species = sp,
    lat = runif(num_years, min = -70, max = 70),
    lon = runif(num_years, min = -180, max = 180),
    year = years
  )
}) %>% bind_rows()

# Generar especies con tendencia (SA)
num_spp_sa <- num_spp - num_spp_sc # 20 especies con tendencia
spp_sa <- paste0("spp_", (num_spp_sc + 1):num_spp, "_SA")
data_sa <- lapply(spp_sa, function(sp) {
  hemisphere <- sample(c("North", "South", "Both", "Opposite"), 1) # Asignar hemisferio aleatorio
  if (hemisphere == "North") {
    data.frame(
      species = sp,
      lat = runif(num_years, min = 30, max = 70) + (years - 1900) * 0.1, # Tendencia positiva en el norte
      lon = runif(num_years, min = -180, max = 180),
      year = years
    ) %>% filter(lat > 0)
  } else if (hemisphere == "South") {
    data.frame(
      species = sp,
      lat = runif(num_years, min = -70, max = -30) - (years - 1900) * 0.1, # Tendencia negativa en el sur
      lon = runif(num_years, min = -180, max = 180),
      year = years
    ) %>% filter(lat < 0)
  } else if(hemisphere == "Both"){
    rbind(
      data.frame(
        species = sp,
        lat = runif(num_years / 2, min = -70, max = -30) - (years[1:(num_years / 2)] - 1900) * 0.1, # Tendencia negativa en el sur
        lon = runif(num_years / 2, min = -180, max = 180),
        year = years[1:(num_years / 2)]
      ) %>% filter(lat < 0),
      data.frame(
        species = sp,
        lat = runif(num_years / 2, min = 30, max = 70) + (years[(num_years / 2 + 1):num_years] - 1900) * 0.1, # Tendencia positiva en el norte
        lon = runif(num_years / 2, min = -180, max = 180),
        year = years[(num_years / 2 + 1):num_years]
      ) %>% filter(lat >0)
    )
  } else {
    rbind(
      data.frame(
        species = sp,
        lat = runif(num_years / 2, min = -30, max = -70) + (years[1:(num_years / 2)] - 1900) * 0.1, # Tendencia negativa en el sur
        lon = runif(num_years / 2, min = -180, max = 180),
        year = years[1:(num_years / 2)]
      ) %>% filter(lat < 0),
      data.frame(
        species = sp,
        lat = runif(num_years / 2, min = 30, max = 70) + (years[(num_years / 2 + 1):num_years] - 1900) * 0.1, # Tendencia positiva en el norte
        lon = runif(num_years / 2, min = -180, max = 180),
        year = years[(num_years / 2 + 1):num_years]
      ) %>% filter(lat >0)
    )
  }
}) %>% bind_rows()

# Combinar todos los datos
Data <- bind_rows(data_sc, data_sa)
colnames(Data) <- c("species","Lat"  ,   "Lon"   ,  "year" )

predictor <- "year"
responses <- c("Lat")
spp <- unique(Data$species)
spp_trends_world_results <- spp_trend_world(Data, spp, predictor, responses, n_min = 10)

spp_strategy_poleward_results <- spp_strategy_poleward(spp_trend_world_result, bonferroni = 0.05, responses = responses)

# Convertir a objeto sf
example_sf <- st_as_sf(example_data, coords = c("lon", "lat"), crs = 4326)

# Exportar a shapefile
st_write(example_sf, "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/example_data_100spp.shp")

# Imprimir las primeras filas del objeto sf
head(example_sf)
