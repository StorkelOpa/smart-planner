# Schritt 2: Windkraftanlagen je Kreis aggregieren
# Liest die aus dem MaStR geparste CSV (erzeugt von 02_get_wind_turbines.py),
# filtert auf aktive Onshore-Anlagen und zaehlt/summiert je Kreis.
# Ergebnis: data/wind_turbinen_je_kreis.csv

library(tidyverse)

# AGS als Text einlesen, damit fuehrende Nullen (z.B. "01001") erhalten bleiben
turbinen <- read_csv("data/wind_turbines_roh.csv",
                     col_types = cols(Gemeindeschluessel = col_character()))

# Nur Anlagen "In Betrieb" (Status 35) und "an Land" (Lage 888) behalten
turbinen_aktiv <- turbinen %>%
  filter(EinheitBetriebsstatus == 35,
         Lage == 888,
         !is.na(Gemeindeschluessel))

# Die ersten 5 Ziffern des Gemeindeschluessels sind der Kreis-AGS
wind_je_kreis <- turbinen_aktiv %>%
  mutate(AGS = str_sub(Gemeindeschluessel, 1, 5)) %>%
  filter(nchar(AGS) == 5) %>%
  group_by(AGS) %>%
  summarise(
    Turbine_Count           = n(),
    Total_Bruttoleistung_kW = sum(Bruttoleistung, na.rm = TRUE),
    Total_Nettoleistung_kW  = sum(Nettonennleistung, na.rm = TRUE)
  )

# Kurze Kontrolle
wind_je_kreis

write_csv(wind_je_kreis, "data/wind_turbinen_je_kreis.csv")
