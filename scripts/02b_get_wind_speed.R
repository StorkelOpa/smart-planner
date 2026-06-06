# ==============================================================================
# SMART PLANNER: WINDGESCHWINDIGKEIT JE LANDKREIS (WS3)
# ==============================================================================
# Dieses Skript erschliesst das WINDPOTENZIAL je Landkreis als mittlere
# Windgeschwindigkeit (m/s) aus dem GLOBAL WIND ATLAS (v4).
#
# Vorgehen:
#   1. Laender-Rasterkachel (GeoTIFF) der mittleren Windgeschwindigkeit fuer
#      Deutschland herunterladen (einmalig, danach lokal gecached).
#   2. Je Kreis-Polygon (BKG vg250) per ZONALER STATISTIK den Mittelwert der
#      Rasterzellen berechnen (terra::extract, fun = mean).
#   3. Tabelle AGS -> Windgeschwindigkeit_ms speichern.
#
# Nutzen: Liefert sowohl die Dashboard-Kennzahl "Ø Windgeschwindigkeit" als auch
# eine bisher fehlende KONTROLLVARIABLE fuer das Regressionsmodell (Windpotenzial
# laut Projektkontext).
#
# Hoehe: 150 m als moderne Nabenhoehe von Onshore-Anlagen (GWA bietet 10/50/100/
#        150/200 m). Ueber HUB_HEIGHT_M anpassbar.
#
# Eingang : data/vg250_2023/...krs...shp        (BKG-Kreisgrenzen)
# Ausgang : data/wind_atlas/DEU_wind-speed_150m.tif  (gecachtes Raster)
#           data/wind_speed_by_county.csv            (AGS, Windgeschwindigkeit_ms)
# ==============================================================================

library(sf)
library(terra)
library(tidyverse)

dir.create("data/wind_atlas", showWarnings = FALSE, recursive = TRUE)

# --- Konfiguration -----------------------------------------------------------
HUB_HEIGHT_M <- 150L  # Bezugshoehe in Metern (GWA: 10/50/100/150/200)
raster_path  <- sprintf("data/wind_atlas/DEU_wind-speed_%dm.tif", HUB_HEIGHT_M)
gwa_url      <- sprintf(
  "https://globalwindatlas.info/api/gis/country/DEU/wind-speed/%d", HUB_HEIGHT_M
)
output_path  <- "data/wind_speed_by_county.csv"

# ------------------------------------------------------------------------------
# 1. Rasterkachel laden (bei Bedarf herunterladen, sonst lokalen Cache nutzen)
# ------------------------------------------------------------------------------
if (!file.exists(raster_path)) {
  cat("Lade Global-Wind-Atlas-Raster (", HUB_HEIGHT_M, "m) von:\n  ", gwa_url, "\n", sep = "")
  # Der API-Endpunkt leitet auf das CDN-GeoTIFF um -> Redirects folgen (libcurl).
  download.file(gwa_url, destfile = raster_path, mode = "wb", quiet = FALSE)
} else {
  cat("Nutze gecachtes Raster:", raster_path, "\n")
}

wind_raster <- rast(raster_path)
cat("Raster geladen. Aufloesung:", round(res(wind_raster)[1], 4),
    "Grad, CRS:", crs(wind_raster, describe = TRUE)$code, "\n")

# ------------------------------------------------------------------------------
# 2. Kreisgrenzen laden (gleiche Quelle/Filter wie 04_merge_data.R)
# ------------------------------------------------------------------------------
cat("Lade BKG-Kreisgrenzen...\n")
shapefile_path <- list.files(
  path = "data/vg250_2023",
  pattern = ".*krs.*\\.shp$",
  recursive = TRUE, full.names = TRUE, ignore.case = TRUE
)
if (length(shapefile_path) == 0) {
  stop("Kreis-Shapefile nicht gefunden! Bitte zuerst scripts/02_get_geodata.R ausfuehren.")
}

districts <- st_read(shapefile_path[1], quiet = TRUE) %>%
  filter(GF == 4) %>%        # nur Landflaeche (ohne Kuestengewaesser)
  select(AGS) %>%
  st_make_valid()

# Polygone in das Raster-CRS (WGS84) projizieren, damit die zonale Statistik passt
districts <- st_transform(districts, crs = crs(wind_raster))
districts_vect <- vect(districts)  # sf -> terra SpatVector

# ------------------------------------------------------------------------------
# 3. Zonale Statistik: mittlere Windgeschwindigkeit je Kreis
#    (terra::extract mit fun = mean; touches=TRUE faengt sehr kleine Stadtkreise
#     ab, deren Polygon sonst zwischen die Rasterzellen fallen koennte)
# ------------------------------------------------------------------------------
cat("Berechne mittlere Windgeschwindigkeit je Kreis (zonale Statistik)...\n")
zonal <- terra::extract(
  wind_raster, districts_vect,
  fun = mean, na.rm = TRUE, touches = TRUE, ID = TRUE
)
# zonal$ID ist der Zeilenindex der Polygone -> zurueck auf AGS mappen
names(zonal)[2] <- "Windgeschwindigkeit_ms"

wind_speed <- tibble(
  AGS = districts$AGS,
  Windgeschwindigkeit_ms = round(zonal$Windgeschwindigkeit_ms, 2)
)

# ------------------------------------------------------------------------------
# 4. Speichern + Plausibilitaetskontrolle
# ------------------------------------------------------------------------------
write_csv(wind_speed, output_path)

cat("\n--- Zusammenfassung Windgeschwindigkeit (", HUB_HEIGHT_M, "m) ---\n", sep = "")
cat("Kreise              :", nrow(wind_speed), "\n")
cat("Fehlende Werte      :", sum(is.na(wind_speed$Windgeschwindigkeit_ms)), "\n")
cat(sprintf("Spannweite          : %.2f - %.2f m/s (Median %.2f)\n",
            min(wind_speed$Windgeschwindigkeit_ms, na.rm = TRUE),
            max(wind_speed$Windgeschwindigkeit_ms, na.rm = TRUE),
            median(wind_speed$Windgeschwindigkeit_ms, na.rm = TRUE)))
cat("Gespeichert nach    :", output_path, "\n")
cat("\nWindgeschwindigkeit je Kreis erfolgreich erstellt!\n")
