# ==============================================================================
# SCHRITT 1: KREISGRENZEN LADEN (BKG VG250-EW)
# ==============================================================================
# Datenquelle: siehe docs/00_datenquellen.md, Abschnitt "BKG VG250-EW".
#
# Laedt die amtlichen Verwaltungsgebiete (Landkreise & kreisfreie Staedte,
# Stand 31.12.2023) und behaelt nur die Spalten, die wir spaeter brauchen:
# AGS (Schluessel zum Verknuepfen), GEN/BEZ (Name), EWZ (Einwohnerzahl),
# KFL (Flaeche in km²).
#
# Eingang : data_raw/vg250_2023.zip (bereits heruntergeladen)
# Ausgang : data/kreise_geodaten.gpkg
# ==============================================================================

library(sf)
library(tidyverse)

dir.create("data", showWarnings = FALSE)

raw_zip     <- "data_raw/vg250_2023.zip"
unzip_dir   <- "data/vg250_2023"
output_path <- "data/kreise_geodaten.gpkg"

# Falls die Rohdatei fehlt (z.B. frischer Klon ohne data_raw/), einmalig
# nachladen. Normalerweise ist sie bereits vorhanden.
url_bkg <- "https://daten.gdz.bkg.bund.de/produkte/vg/vg250-ew_ebenen_1231/2023/vg250-ew_12-31.utm32s.shape.ebenen.zip"
if (!file.exists(raw_zip)) {
  cat("Rohdatei nicht gefunden, lade von BKG herunter...\n")
  dir.create("data_raw", showWarnings = FALSE)
  download.file(url_bkg, destfile = raw_zip, mode = "wb")
}

if (!dir.exists(unzip_dir)) {
  cat("Entpacke Kreisgrenzen...\n")
  unzip(raw_zip, exdir = unzip_dir)
}

shapefile_path <- list.files(
  path = unzip_dir, pattern = ".*krs.*\\.shp$",
  recursive = TRUE, full.names = TRUE, ignore.case = TRUE
)
if (length(shapefile_path) == 0) {
  stop("Kreis-Shapefile (*krs*.shp) nicht im entpackten Ordner gefunden!")
}

cat("Lade Kreisgrenzen aus:", shapefile_path[1], "\n")
districts <- st_read(shapefile_path[1], quiet = TRUE) %>%
  # GF (Geofaktor) == 4 heisst "nur Landflaeche" -> entfernt reine
  # Wasserflaechen-Polygone (Nordsee, Ostsee, Bodensee), uebrig bleiben
  # exakt die 400 Landkreise und kreisfreien Staedte.
  filter(GF == 4) %>%
  select(AGS, GEN, BEZ, EWZ, KFL_km2 = KFL) %>%
  st_make_valid()

cat("Kreise nach GF==4 Filter:", nrow(districts), "(erwartet: 400)\n")

st_write(districts, output_path, delete_dsn = TRUE, quiet = TRUE)
cat("Gespeichert nach:", output_path, "\n")
