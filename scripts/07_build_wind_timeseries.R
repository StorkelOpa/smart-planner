# ==============================================================================
# SMART PLANNER: WINDAUSBAU-ZEITREIHE JE LANDKREIS (WS1)
# ==============================================================================
# Dieses Skript baut aus den einzelnen Windkraftanlagen (MaStR, turbine-level)
# eine ZEITREIHE der kumulierten installierten Leistung je Landkreis und Jahr.
# Es speist das Liniendiagramm "Windleistung im Zeitverlauf" im Dashboard.
#
# Grundgedanke: Jede Anlage hat ein 'Inbetriebnahmedatum'. Summiert man je Kreis
# die bis zu einem Jahr in Betrieb genommene Nettoleistung auf, ergibt sich der
# kumulierte Ausbaustand zum Jahresende.
#
# WICHTIGER CAVEAT (ehrlich dokumentiert):
# Die MaStR-Auszugsdatei enthaelt ueberwiegend AKTUELL BETRIEBENE Anlagen
# (EinheitBetriebsstatus == 35). Bereits stillgelegte Altanlagen fehlen, daher
# sind sehr fruehe Jahre leicht untererfasst. Fuer einen Ausbau-TREND unkritisch.
#
# Eingang : data/wind_turbines.csv               (eine Zeile je Anlage)
#           data/smart_planner_final_data.csv     (fuer Kreisflaeche KFL_km2)
# Ausgang : data/wind_timeseries_by_county.csv    (AGS x Jahr, kumuliert)
# ==============================================================================

library(tidyverse)
library(lubridate)

dir.create("data", showWarnings = FALSE)

turbine_path   <- "data/wind_turbines.csv"
final_data_csv <- "data/smart_planner_final_data.csv"
output_path    <- "data/wind_timeseries_by_county.csv"

if (!file.exists(turbine_path)) {
  stop(paste("Anlagen-CSV nicht gefunden:", turbine_path,
             "\nBitte zuerst scripts/03_parse_mastr.py ausfuehren."))
}
if (!file.exists(final_data_csv)) {
  stop(paste("Finaler Datensatz nicht gefunden:", final_data_csv,
             "\nBitte zuerst scripts/04_merge_data.R ausfuehren (liefert KFL_km2)."))
}

# ------------------------------------------------------------------------------
# 1. Anlagen laden und auf aktive Onshore-Anlagen filtern
#    (gleiche Filterlogik wie 03_process_wind_data.R, damit die kumulierte
#     Endsumme zum Querschnittswert Total_Nettoleistung_kW passt)
# ------------------------------------------------------------------------------
cat("Lade Anlagen-Daten...\n")
# Gemeindeschluessel als character lesen, damit fuehrende Nullen (z. B. "05...") bleiben
turbines <- read_csv(turbine_path, col_types = cols(
  Gemeindeschluessel    = col_character(),
  Inbetriebnahmedatum   = col_character(),
  EinheitBetriebsstatus = col_integer(),
  Nettonennleistung     = col_double(),
  Lage                  = col_integer(),
  .default              = col_guess()
))

cat("Filtere aktive Onshore-Anlagen (Status 35, Lage 888)...\n")
turbines_clean <- turbines %>%
  filter(
    EinheitBetriebsstatus == 35,          # In Betrieb
    Lage == 888,                          # Windkraft an Land (onshore)
    !is.na(Gemeindeschluessel),
    !is.na(Inbetriebnahmedatum)
  ) %>%
  mutate(
    AGS  = str_sub(Gemeindeschluessel, 1, 5),   # erste 5 Stellen = Kreis-AGS
    Jahr = year(ymd(Inbetriebnahmedatum))       # Inbetriebnahmejahr
  ) %>%
  filter(nchar(AGS) == 5, !is.na(Jahr))

# ------------------------------------------------------------------------------
# 2. NEU-Zubau je Kreis und Jahr (noch nicht kumuliert)
# ------------------------------------------------------------------------------
cat("Aggregiere Neu-Zubau je Kreis und Jahr...\n")
yearly_new <- turbines_clean %>%
  group_by(AGS, Jahr) %>%
  summarise(
    Neu_Anlagen        = n(),
    Neu_Nettoleistung  = sum(Nettonennleistung, na.rm = TRUE),
    .groups = "drop"
  )

# Vollstaendiges Gitter AGS x Jahr aufspannen, damit die Kumulation auch in
# Jahren OHNE Zubau korrekt fortgeschrieben wird (Wert bleibt konstant).
jahr_min <- min(yearly_new$Jahr)
jahr_max <- max(yearly_new$Jahr)
cat("  Zeitraum:", jahr_min, "bis", jahr_max, "\n")

grid <- expand_grid(
  AGS  = sort(unique(yearly_new$AGS)),
  Jahr = jahr_min:jahr_max
)

# ------------------------------------------------------------------------------
# 3. Kumulierte Leistung je Kreis ueber die Jahre
# ------------------------------------------------------------------------------
cat("Berechne kumulierten Ausbaustand je Kreis...\n")
county_ts <- grid %>%
  left_join(yearly_new, by = c("AGS", "Jahr")) %>%
  mutate(
    Neu_Anlagen       = replace_na(Neu_Anlagen, 0L),
    Neu_Nettoleistung = replace_na(Neu_Nettoleistung, 0)
  ) %>%
  arrange(AGS, Jahr) %>%
  group_by(AGS) %>%
  mutate(
    Anlagen_kumuliert          = cumsum(Neu_Anlagen),
    Nettoleistung_kW_kumuliert = cumsum(Neu_Nettoleistung)
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 4. Kreisflaeche anjoinen -> kumulierte Dichte (kW/km2)
# ------------------------------------------------------------------------------
cat("Verknuepfe Kreisflaeche fuer die Dichte-Zeitreihe...\n")
flaechen <- read_csv(final_data_csv, col_types = cols(
  AGS    = col_character(),
  KFL_km2 = col_double(),
  .default = col_guess()
)) %>%
  select(AGS, KFL_km2)

county_ts <- county_ts %>%
  left_join(flaechen, by = "AGS") %>%
  mutate(
    Wind_Density_kW_km2_kumuliert = Nettoleistung_kW_kumuliert / KFL_km2
  )

# ------------------------------------------------------------------------------
# 5. Bundesweite Vergleichslinie (AGS = "DE")
#    -> ermoeglicht im Dashboard "dieser Kreis vs. Deutschland gesamt"
# ------------------------------------------------------------------------------
cat("Berechne bundesweite Vergleichs-Zeitreihe (AGS = 'DE')...\n")
flaeche_de <- sum(flaechen$KFL_km2, na.rm = TRUE)

national_ts <- county_ts %>%
  group_by(Jahr) %>%
  summarise(
    AGS                        = "DE",
    Neu_Anlagen                = sum(Neu_Anlagen),
    Neu_Nettoleistung          = sum(Neu_Nettoleistung),
    Anlagen_kumuliert          = sum(Anlagen_kumuliert),
    Nettoleistung_kW_kumuliert = sum(Nettoleistung_kW_kumuliert),
    KFL_km2                    = flaeche_de,
    .groups = "drop"
  ) %>%
  mutate(
    Wind_Density_kW_km2_kumuliert = Nettoleistung_kW_kumuliert / KFL_km2
  )

# ------------------------------------------------------------------------------
# 6. Speichern (Kreise + Bundeszeile), Spalten in sinnvoller Reihenfolge
# ------------------------------------------------------------------------------
timeseries <- bind_rows(county_ts, national_ts) %>%
  select(
    AGS, Jahr,
    Anlagen_kumuliert,
    Nettoleistung_kW_kumuliert,
    Wind_Density_kW_km2_kumuliert
  ) %>%
  arrange(AGS, Jahr)

write_csv(timeseries, output_path)

# ------------------------------------------------------------------------------
# 7. Konsolen-Zusammenfassung als Plausibilitaetskontrolle
# ------------------------------------------------------------------------------
cat("\n--- Zusammenfassung Zeitreihe ---\n")
cat("Kreise mit Zeitreihe :", n_distinct(county_ts$AGS), "\n")
cat("Jahre                :", jahr_min, "-", jahr_max, "\n")
de_last <- national_ts %>% filter(Jahr == jahr_max)
cat(sprintf("Bundesweit %d: %.2f GW kumuliert (%d Anlagen)\n",
            jahr_max,
            de_last$Nettoleistung_kW_kumuliert / 1e6,
            de_last$Anlagen_kumuliert))
cat("Gespeichert nach     :", output_path, "\n")
cat("\nZeitreihe erfolgreich erstellt!\n")
