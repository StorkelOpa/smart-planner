# Schritt 5: Alle vier Datenquellen zusammenfuehren
# Verknuepft Geodaten + Windkraft + Windgeschwindigkeit + INKAR ueber den AGS,
# berechnet die Zielvariable (Windkraftdichte) und leitet Bundesland und
# Wirtschaftsstruktur ab.
# Ergebnis: data/smart_planner_daten.gpkg (mit Geometrie) und .csv (ohne)

library(sf)
library(tidyverse)

# Die vier Zwischendateien einlesen (AGS ueberall als Text, wegen fuehrender Nullen)
geodaten   <- st_read("data/kreise_geodaten.gpkg", quiet = TRUE)
windkraft  <- read_csv("data/wind_turbinen_je_kreis.csv", col_types = cols(AGS = col_character()))
windgeschw <- read_csv("data/windgeschwindigkeit_je_kreis.csv", col_types = cols(AGS = col_character()))
inkar      <- read_csv("data/inkar_kennzahlen_je_kreis.csv", col_types = cols(AGS = col_character()))

# Ueber den AGS zusammenfuehren (die Geodaten sind die Basis, alles andere links dran)
daten <- geodaten %>%
  left_join(windkraft, by = "AGS") %>%
  left_join(windgeschw, by = "AGS") %>%
  left_join(inkar, by = "AGS")

# Kreise ohne Windkraft-Eintrag haben schlicht keine Anlagen -> 0 statt NA
daten <- daten %>%
  mutate(
    Turbine_Count           = ifelse(is.na(Turbine_Count), 0L, Turbine_Count),
    Total_Bruttoleistung_kW = ifelse(is.na(Total_Bruttoleistung_kW), 0, Total_Bruttoleistung_kW),
    Total_Nettoleistung_kW  = ifelse(is.na(Total_Nettoleistung_kW), 0, Total_Nettoleistung_kW)
  )

# Zielvariable: installierte Leistung je km2 Kreisflaeche
daten <- daten %>%
  mutate(Wind_Density_kW_km2 = Total_Nettoleistung_kW / KFL_km2)

# Restkategorie der Flaechennutzung (die vier Anteile ergeben nicht exakt 100%,
# der Rest sind z.B. Tagebau- oder Truppenuebungsflaechen)
daten <- daten %>%
  mutate(Sonstige_Prozent = pmax(0, 100 - (Landwirtschaft_Prozent + Waldflaeche_Prozent +
                                             Siedlung_Verkehr_Prozent + Wasser_Prozent)))

# Bundesland aus den ersten 2 Ziffern des AGS ableiten
daten <- daten %>%
  mutate(
    bl_code = str_sub(AGS, 1, 2),
    Bundesland = case_when(
      bl_code == "01" ~ "Schleswig-Holstein",
      bl_code == "02" ~ "Hamburg",
      bl_code == "03" ~ "Niedersachsen",
      bl_code == "04" ~ "Bremen",
      bl_code == "05" ~ "Nordrhein-Westfalen",
      bl_code == "06" ~ "Hessen",
      bl_code == "07" ~ "Rheinland-Pfalz",
      bl_code == "08" ~ "Baden-Württemberg",
      bl_code == "09" ~ "Bayern",
      bl_code == "10" ~ "Saarland",
      bl_code == "11" ~ "Berlin",
      bl_code == "12" ~ "Brandenburg",
      bl_code == "13" ~ "Mecklenburg-Vorpommern",
      bl_code == "14" ~ "Sachsen",
      bl_code == "15" ~ "Sachsen-Anhalt",
      bl_code == "16" ~ "Thüringen"
    )
  ) %>%
  select(-bl_code)

# Dominante Wirtschaftsstruktur (Schwellenwerte siehe docs/01_datenaufbereitung.md)
daten <- daten %>%
  mutate(
    Economic_Structure = case_when(
      Beschaeftigte_Sekundar >= 35 ~ "Industriell",
      Beschaeftigte_Primar >= 3.0  ~ "Ländlich/Agrarisch",
      Beschaeftigte_Tertiar >= 65  ~ "Dienstleistungsorientiert",
      TRUE ~ "Ausgeglichen"
    )
  )

# Ins WGS84-Koordinatensystem bringen (fuer die Karten)
daten <- st_transform(daten, crs = 4326)

# Kurze Kontrolle
nrow(daten)
table(daten$Economic_Structure)

st_write(daten, "data/smart_planner_daten.gpkg", delete_dsn = TRUE, quiet = TRUE)
write_csv(st_drop_geometry(daten), "data/smart_planner_daten.csv")
