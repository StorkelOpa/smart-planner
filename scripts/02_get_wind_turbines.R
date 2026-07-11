# ==============================================================================
# SCHRITT 2b: WINDKRAFTANLAGEN JE KREIS AGGREGIEREN
# ==============================================================================
# Setzt auf scripts/02_get_wind_turbines.py auf. Filtert die geparste
# MaStR-CSV auf aktive Onshore-Anlagen und aggregiert Anzahl + installierte
# Leistung je Kreis (5-stelliger AGS).
#
# Filterkriterien (siehe docs/00_datenquellen.md):
# - EinheitBetriebsstatus == 35  ("In Betrieb")
# - Lage == 888                 ("Windkraft an Land" / onshore)
#
# Eingang : data/wind_turbines_roh.csv
# Ausgang : data/wind_turbinen_je_kreis.csv
#           (AGS, Turbine_Count, Total_Bruttoleistung_kW, Total_Nettoleistung_kW)
# ==============================================================================

library(tidyverse)

input_path  <- "data/wind_turbines_roh.csv"
output_path <- "data/wind_turbinen_je_kreis.csv"

if (!file.exists(input_path)) {
  stop("Rohdaten nicht gefunden: ", input_path,
       "\nBitte zuerst 'python3 scripts/02_get_wind_turbines.py' ausfuehren.")
}

cat("Lade geparste Windkraftanlagen...\n")
turbines_raw <- read_csv(input_path, col_types = cols(
  EinheitMastrNummer    = col_character(),
  Gemeindeschluessel    = col_character(),
  Landkreis             = col_character(),
  Gemeinde              = col_character(),
  Laengengrad           = col_double(),
  Breitengrad           = col_double(),
  Inbetriebnahmedatum   = col_character(),
  EinheitBetriebsstatus = col_integer(),
  Bruttoleistung        = col_double(),
  Nettonennleistung     = col_double(),
  Lage                  = col_integer()
))

cat("Filtere auf aktive Onshore-Anlagen...\n")
turbines_clean <- turbines_raw %>%
  filter(
    EinheitBetriebsstatus == 35,  # in Betrieb
    Lage == 888,                 # onshore
    !is.na(Gemeindeschluessel)
  )

cat("Aggregiere je Kreis (5-stelliger AGS)...\n")
wind_je_kreis <- turbines_clean %>%
  mutate(AGS = str_sub(Gemeindeschluessel, 1, 5)) %>%
  filter(nchar(AGS) == 5) %>%
  group_by(AGS) %>%
  summarise(
    Turbine_Count           = n(),
    Total_Bruttoleistung_kW = sum(Bruttoleistung, na.rm = TRUE),
    Total_Nettoleistung_kW  = sum(Nettonennleistung, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- Zusammenfassung ---\n")
cat("Aktive Onshore-Anlagen gesamt :", nrow(turbines_clean), "\n")
cat("Kreise mit Windkraft          :", nrow(wind_je_kreis), "von 400\n")
cat("Installierte Nettoleistung    :", round(sum(wind_je_kreis$Total_Nettoleistung_kW) / 1e6, 2), "GW\n")

write_csv(wind_je_kreis, output_path)
cat("\nGespeichert nach:", output_path, "\n")
