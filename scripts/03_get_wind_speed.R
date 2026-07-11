# ==============================================================================
# SCHRITT 3: WINDGESCHWINDIGKEIT JE KREIS (Global Wind Atlas)
# ==============================================================================
# Datenquelle: siehe docs/00_datenquellen.md, Abschnitt "Global Wind Atlas".
#
# Berechnet die mittlere Windgeschwindigkeit (150 m Nabenhoehe) je Kreis per
# zonaler Statistik (Mittelwert der Rasterzellen innerhalb jedes Polygons).
#
# Eingang : data_raw/wind_speed_150m.tif      (Raster, bereits heruntergeladen)
#           data/kreise_geodaten.gpkg          (Output von 01_get_geodata.R)
# Ausgang : data/windgeschwindigkeit_je_kreis.csv  (AGS, Windgeschwindigkeit_ms)
# ==============================================================================

library(sf)
library(terra)
library(tidyverse)

raster_path <- "data_raw/wind_speed_150m.tif"
geodata_path <- "data/kreise_geodaten.gpkg"
output_path <- "data/windgeschwindigkeit_je_kreis.csv"

if (!file.exists(raster_path)) {
  cat("Rasterdatei nicht gefunden, lade von Global Wind Atlas herunter...\n")
  dir.create("data_raw", showWarnings = FALSE)
  download.file(
    "https://globalwindatlas.info/api/gis/country/DEU/wind-speed/150",
    destfile = raster_path, mode = "wb"
  )
}
if (!file.exists(geodata_path)) {
  stop("Kreisgeodaten nicht gefunden. Bitte zuerst scripts/01_get_geodata.R ausfuehren.")
}

cat("Lade Windgeschwindigkeits-Raster...\n")
wind_raster <- rast(raster_path)

cat("Lade Kreisgrenzen und projiziere ins Raster-CRS...\n")
districts <- st_read(geodata_path, quiet = TRUE) %>%
  select(AGS) %>%
  st_transform(crs = crs(wind_raster))
districts_vect <- vect(districts)

cat("Berechne mittlere Windgeschwindigkeit je Kreis (zonale Statistik)...\n")
zonal <- terra::extract(
  wind_raster, districts_vect,
  fun = mean, na.rm = TRUE, touches = TRUE, ID = TRUE
)
names(zonal)[2] <- "Windgeschwindigkeit_ms"

wind_speed <- tibble(
  AGS = districts$AGS,
  Windgeschwindigkeit_ms = round(zonal$Windgeschwindigkeit_ms, 2)
)

cat("\n--- Zusammenfassung ---\n")
cat("Kreise             :", nrow(wind_speed), "\n")
cat("Fehlende Werte      :", sum(is.na(wind_speed$Windgeschwindigkeit_ms)), "\n")
cat(sprintf("Spannweite          : %.2f - %.2f m/s (Median %.2f)\n",
            min(wind_speed$Windgeschwindigkeit_ms, na.rm = TRUE),
            max(wind_speed$Windgeschwindigkeit_ms, na.rm = TRUE),
            median(wind_speed$Windgeschwindigkeit_ms, na.rm = TRUE)))

write_csv(wind_speed, output_path)
cat("\nGespeichert nach:", output_path, "\n")
