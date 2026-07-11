# Schritt 4: INKAR-Kennzahlen je Kreis abrufen (Kreisebene, Jahr 2023)
# Die Kennziffern stehen in docs/00_datenquellen.md. Jede Variable wird
# einzeln abgerufen und dann ueber den AGS zusammengefuehrt.
# Ergebnis: data/inkar_kennzahlen_je_kreis.csv

library(bonn)
library(tidyverse)
library(httr)

# Umgeht ein lokales Zertifikatsproblem beim Zugriff auf www.inkar.de
set_config(config(ssl_verifypeer = FALSE))

# Jede Kennziffer einzeln laden und die Wertspalte passend benennen.
# (Schluessel = AGS des Kreises, Wert = der abgefragte Kennzahlenwert.)
steuerkraft     <- get_data(variable = "294", geography = "KRE", time = 2023) %>%
  select(AGS = Schlüssel, Steuerkraft = Wert)
einwohnerdichte <- get_data(variable = "320", geography = "KRE", time = 2023) %>%
  select(AGS = Schlüssel, Einwohnerdichte = Wert)
waldflaeche     <- get_data(variable = "264", geography = "KRE", time = 2023) %>%
  select(AGS = Schlüssel, Waldflaeche_Prozent = Wert)
landwirtschaft  <- get_data(variable = "261", geography = "KRE", time = 2023) %>%
  select(AGS = Schlüssel, Landwirtschaft_Prozent = Wert)
siedlung        <- get_data(variable = "255", geography = "KRE", time = 2023) %>%
  select(AGS = Schlüssel, Siedlung_Verkehr_Prozent = Wert)
wasser          <- get_data(variable = "265", geography = "KRE", time = 2023) %>%
  select(AGS = Schlüssel, Wasser_Prozent = Wert)
besch_primar    <- get_data(variable = "103", geography = "KRE", time = 2023) %>%
  select(AGS = Schlüssel, Beschaeftigte_Primar = Wert)
besch_sekundar  <- get_data(variable = "104", geography = "KRE", time = 2023) %>%
  select(AGS = Schlüssel, Beschaeftigte_Sekundar = Wert)
besch_tertiar   <- get_data(variable = "105", geography = "KRE", time = 2023) %>%
  select(AGS = Schlüssel, Beschaeftigte_Tertiar = Wert)

# Alle Kennzahlen ueber den AGS zusammenfuehren
inkar <- steuerkraft %>%
  full_join(einwohnerdichte, by = "AGS") %>%
  full_join(waldflaeche, by = "AGS") %>%
  full_join(landwirtschaft, by = "AGS") %>%
  full_join(siedlung, by = "AGS") %>%
  full_join(wasser, by = "AGS") %>%
  full_join(besch_primar, by = "AGS") %>%
  full_join(besch_sekundar, by = "AGS") %>%
  full_join(besch_tertiar, by = "AGS")

# In kleinen (staedtischen) Kreisen ist der primaere Sektor aus Datenschutz-
# gruenden geschwaerzt (NA). Da die drei Sektoranteile zusammen 100% ergeben,
# koennen wir den fehlenden Wert rekonstruieren, statt den Kreis zu verlieren.
inkar <- inkar %>%
  mutate(
    Beschaeftigte_Primar = ifelse(is.na(Beschaeftigte_Primar), 0, Beschaeftigte_Primar),
    Beschaeftigte_Sekundar = ifelse(is.na(Beschaeftigte_Sekundar),
                                    100 - Beschaeftigte_Tertiar - Beschaeftigte_Primar,
                                    Beschaeftigte_Sekundar),
    Beschaeftigte_Sekundar = ifelse(is.na(Beschaeftigte_Sekundar), 0, Beschaeftigte_Sekundar)
  )

# Kurze Kontrolle
nrow(inkar)

write_csv(inkar, "data/inkar_kennzahlen_je_kreis.csv")
