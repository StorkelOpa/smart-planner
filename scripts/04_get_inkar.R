# ==============================================================================
# SCHRITT 4: INKAR-KENNZAHLEN JE KREIS ABRUFEN
# ==============================================================================
# Datenquelle: siehe docs/00_datenquellen.md, Abschnitt "INKAR".
#
# Ruft genau die 9 bekannten INKAR-Kennziffern ab (keine Katalogsuche - die
# Kennziffern stehen bereits in docs/00_datenquellen.md):
#   294 Steuerkraft            320 Einwohnerdichte
#   264 Waldflaeche            261 Landwirtschaftsflaeche
#   255 Siedlungs-/Verkehrsflaeche   265 Wasserflaeche
#   103 Beschaeftigte Primaersektor
#   104 Beschaeftigte Sekundaersektor
#   105 Beschaeftigte Tertiaersektor
#
# Eingang : INKAR JSON-API (live, kein lokaler Rohdaten-Cache noetig)
# Ausgang : data/inkar_kennzahlen_je_kreis.csv
# ==============================================================================

library(bonn)
library(tidyverse)
library(httr)

# SSL-Verifikation deaktivieren (umgeht lokale CA-Zertifikatsfehler bei www.inkar.de)
set_config(config(ssl_verifypeer = FALSE))

output_path <- "data/inkar_kennzahlen_je_kreis.csv"

fetch_inkar_var <- function(kennziffer, spaltenname) {
  cat("  Kennziffer", kennziffer, "->", spaltenname, "\n")
  get_data(variable = kennziffer, geography = "KRE", time = 2023) %>%
    select(AGS = Schlüssel, !!spaltenname := Wert)
}

cat("Rufe INKAR-Kennziffern ab (Kreisebene, Berichtsjahr 2023)...\n")
inkar_raw <- fetch_inkar_var("294", "Steuerkraft") %>%
  full_join(fetch_inkar_var("320", "Einwohnerdichte"), by = "AGS") %>%
  full_join(fetch_inkar_var("264", "Waldflaeche_Prozent"), by = "AGS") %>%
  full_join(fetch_inkar_var("261", "Landwirtschaft_Prozent"), by = "AGS") %>%
  full_join(fetch_inkar_var("255", "Siedlung_Verkehr_Prozent"), by = "AGS") %>%
  full_join(fetch_inkar_var("265", "Wasser_Prozent"), by = "AGS") %>%
  full_join(fetch_inkar_var("103", "Beschaeftigte_Primar"), by = "AGS") %>%
  full_join(fetch_inkar_var("104", "Beschaeftigte_Sekundar"), by = "AGS") %>%
  full_join(fetch_inkar_var("105", "Beschaeftigte_Tertiar"), by = "AGS")

# Geheimhaltung: in kleinen (meist staedtischen) Kreisen werden Beschaeftigten-
# zahlen im primaeren Sektor aus Datenschutzgruenden geschwaerzt (NA). Da die
# drei Sektoranteile in Summe 100% ergeben muessen, koennen wir das ueber die
# beiden anderen Sektoren rekonstruieren statt die Kreise zu verlieren.
cat("Impute fehlende Beschaeftigtenanteile (Geheimhaltung in kleinen Kreisen)...\n")
inkar_clean <- inkar_raw %>%
  mutate(
    Beschaeftigte_Primar = coalesce(Beschaeftigte_Primar, 0),
    Beschaeftigte_Sekundar = ifelse(
      is.na(Beschaeftigte_Sekundar),
      100 - Beschaeftigte_Tertiar - Beschaeftigte_Primar,
      Beschaeftigte_Sekundar
    ),
    Beschaeftigte_Sekundar = coalesce(Beschaeftigte_Sekundar, 0)
  )

cat("Kreise mit INKAR-Daten:", nrow(inkar_clean), "\n")

write_csv(inkar_clean, output_path)
cat("Gespeichert nach:", output_path, "\n")
