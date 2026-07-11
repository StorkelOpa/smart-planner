# Schritt 1: Kreisgrenzen laden (BKG VG250-EW, Stand 31.12.2023)
# Quelle: siehe docs/00_datenquellen.md.
# Ergebnis: data/kreise_geodaten.gpkg mit AGS, Name, Einwohnerzahl, Flaeche.

library(sf)
library(tidyverse)

# Rohdaten entpacken (liegen als ZIP in data_raw/)
unzip("data_raw/vg250_2023.zip", exdir = "data/vg250_2023")

# Das Kreis-Shapefile im entpackten Ordner suchen (Dateiname enthaelt "krs")
shapefile <- list.files("data/vg250_2023", pattern = "krs.*\\.shp$",
                        recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

# Kreise einlesen und auf die Spalten reduzieren, die wir brauchen
kreise <- st_read(shapefile[1], quiet = TRUE) %>%
  filter(GF == 4) %>%                        # GF == 4 = nur Landflaeche (ohne Meer/Seen)
  select(AGS, GEN, BEZ, EWZ, KFL_km2 = KFL) %>%
  st_make_valid()

# Kontrolle: es sollten 400 Kreise sein
nrow(kreise)

st_write(kreise, "data/kreise_geodaten.gpkg", delete_dsn = TRUE, quiet = TRUE)
