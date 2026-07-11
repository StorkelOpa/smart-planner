# Schritt 3: Mittlere Windgeschwindigkeit je Kreis (Global Wind Atlas, 150 m)
# Zonale Statistik: Mittelwert aller Rasterzellen innerhalb jedes Kreises.
# Ergebnis: data/windgeschwindigkeit_je_kreis.csv

library(sf)
library(terra)
library(tidyverse)

# Wind-Raster und Kreisgrenzen laden
wind_raster <- rast("data_raw/wind_speed_150m.tif")

kreise <- st_read("data/kreise_geodaten.gpkg", quiet = TRUE) %>%
  select(AGS) %>%
  st_transform(crs = crs(wind_raster))   # Kreise ins Koordinatensystem des Rasters bringen

# Mittelwert der Rasterzellen je Kreis berechnen.
# extract() liefert eine Tabelle mit Spalte 1 = ID, Spalte 2 = Mittelwert.
mittelwerte <- terra::extract(wind_raster, vect(kreise),
                              fun = mean, na.rm = TRUE, touches = TRUE)

wind_speed <- tibble(
  AGS = kreise$AGS,
  Windgeschwindigkeit_ms = round(mittelwerte[[2]], 2)
)

# Kurze Kontrolle
wind_speed

write_csv(wind_speed, "data/windgeschwindigkeit_je_kreis.csv")
