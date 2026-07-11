# ==============================================================================
# SCHRITT 5: ALLE DATENQUELLEN ZUSAMMENFUEHREN
# ==============================================================================
# Liest die vier Zwischendateien aus den Schritten 1-4 und fuehrt sie ueber
# den AGS (Amtlicher Gemeindeschluessel) zusammen. Berechnet daraus die
# Zielvariable (Windkraftdichte) und leitet die Wirtschaftsstruktur-
# Klassifikation ab.
#
# Eingang : data/kreise_geodaten.gpkg               (Schritt 1)
#           data/wind_turbinen_je_kreis.csv          (Schritt 2)
#           data/windgeschwindigkeit_je_kreis.csv    (Schritt 3)
#           data/inkar_kennzahlen_je_kreis.csv       (Schritt 4)
# Ausgang : data/smart_planner_daten.gpkg  (mit Geometrie, fuers Modell + Karten)
#           data/smart_planner_daten.csv   (ohne Geometrie, fuer schnelle Analysen)
# ==============================================================================

library(sf)
library(tidyverse)

cat("Lade die vier Zwischendateien...\n")
geodaten     <- st_read("data/kreise_geodaten.gpkg", quiet = TRUE)
windkraft    <- read_csv("data/wind_turbinen_je_kreis.csv", col_types = cols(AGS = col_character()))
windgeschw   <- read_csv("data/windgeschwindigkeit_je_kreis.csv", col_types = cols(AGS = col_character()))
inkar        <- read_csv("data/inkar_kennzahlen_je_kreis.csv", col_types = cols(AGS = col_character()))

cat("Fuehre ueber AGS zusammen...\n")
daten <- geodaten %>%
  left_join(windkraft, by = "AGS") %>%
  left_join(windgeschw, by = "AGS") %>%
  left_join(inkar, by = "AGS") %>%
  mutate(
    # Kreise ohne Eintrag in der Windkraft-Tabelle haben schlicht keine
    # Anlagen (0), nicht "fehlend".
    Turbine_Count           = coalesce(Turbine_Count, 0L),
    Total_Bruttoleistung_kW = coalesce(Total_Bruttoleistung_kW, 0),
    Total_Nettoleistung_kW  = coalesce(Total_Nettoleistung_kW, 0),

    # Zielvariable: installierte Leistung je km² Kreisflaeche.
    Wind_Density_kW_km2 = Total_Nettoleistung_kW / KFL_km2,

    # Flaechennutzungs-Restkategorie (die vier INKAR-Anteile summieren
    # nicht exakt auf 100%; der Rest sind z.B. Tagebau-/Truppenuebungsflaechen).
    Sonstige_Prozent = pmax(0, 100 - (Landwirtschaft_Prozent + Waldflaeche_Prozent +
                                        Siedlung_Verkehr_Prozent + Wasser_Prozent)),

    # Bundesland aus den ersten 2 Ziffern des AGS (fuer Regions-Auswertungen,
    # z.B. den Robustheitscheck "nur Norddeutschland" in 06_model_regression.R).
    Bundesland = case_when(
      str_sub(AGS, 1, 2) == "01" ~ "Schleswig-Holstein",
      str_sub(AGS, 1, 2) == "02" ~ "Hamburg",
      str_sub(AGS, 1, 2) == "03" ~ "Niedersachsen",
      str_sub(AGS, 1, 2) == "04" ~ "Bremen",
      str_sub(AGS, 1, 2) == "05" ~ "Nordrhein-Westfalen",
      str_sub(AGS, 1, 2) == "06" ~ "Hessen",
      str_sub(AGS, 1, 2) == "07" ~ "Rheinland-Pfalz",
      str_sub(AGS, 1, 2) == "08" ~ "Baden-Württemberg",
      str_sub(AGS, 1, 2) == "09" ~ "Bayern",
      str_sub(AGS, 1, 2) == "10" ~ "Saarland",
      str_sub(AGS, 1, 2) == "11" ~ "Berlin",
      str_sub(AGS, 1, 2) == "12" ~ "Brandenburg",
      str_sub(AGS, 1, 2) == "13" ~ "Mecklenburg-Vorpommern",
      str_sub(AGS, 1, 2) == "14" ~ "Sachsen",
      str_sub(AGS, 1, 2) == "15" ~ "Sachsen-Anhalt",
      str_sub(AGS, 1, 2) == "16" ~ "Thüringen"
    ),

    # Dominante Wirtschaftsstruktur (Schwellenwerte s. docs/01_datenaufbereitung.md)
    Economic_Structure = case_when(
      Beschaeftigte_Sekundar >= 35 ~ "Industriell",
      Beschaeftigte_Primar >= 3.0  ~ "Ländlich/Agrarisch",
      Beschaeftigte_Tertiar >= 65  ~ "Dienstleistungsorientiert",
      TRUE ~ "Ausgeglichen"
    )
  )

cat("Transformiere ins WGS84-Koordinatensystem (fuer Karten)...\n")
daten <- st_transform(daten, crs = 4326)

cat("\n--- Zusammenfassung finaler Datensatz ---\n")
cat("Kreise gesamt          :", nrow(daten), "\n")
cat("Gesamt installierte Leistung:", round(sum(daten$Total_Nettoleistung_kW) / 1e6, 2), "GW\n")
cat("Wirtschaftsstruktur:\n")
print(table(daten$Economic_Structure))

st_write(daten, "data/smart_planner_daten.gpkg", delete_dsn = TRUE, quiet = TRUE)
write_csv(st_drop_geometry(daten), "data/smart_planner_daten.csv")
cat("\nGespeichert nach: data/smart_planner_daten.gpkg + .csv\n")
