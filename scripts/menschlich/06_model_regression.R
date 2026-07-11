# Schritt 6: Regression, Diagnostik & Robustheitschecks
# Schaetzt das OLS-Hauptmodell (Rohwerte + standardisierte Version), prueft die
# Modellannahmen (VIF, Breusch-Pagan, Moran's I) und rechnet zwei Robustheits-
# checks auf eingeschraenkten Teilstichproben.
# Ergebnis: data/smart_planner_daten_mit_residuen.gpkg / .csv
#           data/modell_ergebnisse.RData

library(sf)
library(tidyverse)
library(car)
library(lmtest)
library(spdep)

# Finalen Datensatz laden. Fuer die Regression brauchen wir keine Geometrie,
# darum ziehen wir die Attribute in ein normales Tibble.
daten_sf <- st_read("data/smart_planner_daten.gpkg", quiet = TRUE)
df <- daten_sf %>% st_drop_geometry() %>% as_tibble()


# --- 1. Hauptmodell (alle 400 Kreise) ----------------------------------------
# Beschaeftigte_Tertiar wird bewusst weggelassen: die drei Sektoranteile ergeben
# zusammen 100%, sonst haetten wir perfekte Multikollinearitaet.
modell_formel <- Wind_Density_kW_km2 ~ Steuerkraft + Einwohnerdichte +
  Windgeschwindigkeit_ms + Waldflaeche_Prozent + Landwirtschaft_Prozent +
  Beschaeftigte_Sekundar + Beschaeftigte_Primar

modell_roh <- lm(modell_formel, data = df)
summary(modell_roh)

# Standardisierte Version (z-Werte), damit die Effektstaerken vergleichbar sind.
# scale() rechnet jede Variable in Standardabweichungen um.
df_z <- df %>%
  mutate(
    Wind_Density_kW_km2    = as.numeric(scale(Wind_Density_kW_km2)),
    Steuerkraft            = as.numeric(scale(Steuerkraft)),
    Einwohnerdichte        = as.numeric(scale(Einwohnerdichte)),
    Windgeschwindigkeit_ms = as.numeric(scale(Windgeschwindigkeit_ms)),
    Waldflaeche_Prozent    = as.numeric(scale(Waldflaeche_Prozent)),
    Landwirtschaft_Prozent = as.numeric(scale(Landwirtschaft_Prozent)),
    Beschaeftigte_Sekundar = as.numeric(scale(Beschaeftigte_Sekundar)),
    Beschaeftigte_Primar   = as.numeric(scale(Beschaeftigte_Primar))
  )

modell_std <- lm(modell_formel, data = df_z)
summary(modell_std)


# --- 2. Diagnostik -----------------------------------------------------------
# Multikollinearitaet (Werte deutlich ueber 5 waeren problematisch)
vif_werte <- vif(modell_roh)
vif_werte

# Heteroskedastizitaet (ungleiche Streuung der Residuen)
bp_test <- bptest(modell_roh)
bp_test

# Raeumliche Autokorrelation der Residuen (aehneln sich benachbarte Kreise?)
nb <- poly2nb(daten_sf)
listw <- nb2listw(nb, style = "W", zero.policy = TRUE)
moran_test <- lm.morantest(modell_roh, listw, zero.policy = TRUE)
moran_test


# --- 3. Robustheitschecks auf Teilstichproben --------------------------------
# Dasselbe Modell nochmal auf a) nur windreichen Kreisen und b) nur Nord-
# deutschland rechnen. Frage: bleibt der (nicht signifikante) Steuerkraft-
# Effekt bestehen, wenn wir die Kreise vergleichbarer machen?
nord_bundeslaender <- c("Schleswig-Holstein", "Mecklenburg-Vorpommern",
                        "Niedersachsen", "Bremen", "Hamburg")

df_nur_wind <- df %>% filter(Windgeschwindigkeit_ms >= median(Windgeschwindigkeit_ms))
modell_nur_wind <- lm(modell_formel, data = df_nur_wind)

df_nur_nord <- df %>% filter(Bundesland %in% nord_bundeslaender)
modell_nur_nord <- lm(modell_formel, data = df_nur_nord)

# Steuerkraft-Koeffizient und Konfidenzintervall aus den drei Modellen sammeln
robustheit_tabelle <- tibble(
  Stichprobe = c("Hauptmodell (alle Kreise)",
                 "Nur überdurchschnittliche Windgeschwindigkeit",
                 "Nur Norddeutschland"),
  N = c(nrow(df), nrow(df_nur_wind), nrow(df_nur_nord)),
  Steuerkraft_Koeffizient = c(coef(modell_roh)["Steuerkraft"],
                              coef(modell_nur_wind)["Steuerkraft"],
                              coef(modell_nur_nord)["Steuerkraft"]),
  Steuerkraft_Lower = c(confint(modell_roh)["Steuerkraft", 1],
                        confint(modell_nur_wind)["Steuerkraft", 1],
                        confint(modell_nur_nord)["Steuerkraft", 1]),
  Steuerkraft_Upper = c(confint(modell_roh)["Steuerkraft", 2],
                        confint(modell_nur_wind)["Steuerkraft", 2],
                        confint(modell_nur_nord)["Steuerkraft", 2]),
  Steuerkraft_p_Wert = c(summary(modell_roh)$coefficients["Steuerkraft", "Pr(>|t|)"],
                         summary(modell_nur_wind)$coefficients["Steuerkraft", "Pr(>|t|)"],
                         summary(modell_nur_nord)$coefficients["Steuerkraft", "Pr(>|t|)"])
)
robustheit_tabelle


# --- 4. Residuen & Performance-Klassifikation --------------------------------
# Residuum = tatsaechlicher minus vorhergesagter Wert. Kreise, die stark nach
# oben/unten abweichen, markieren wir als Out-/Underperformer.
daten_ergebnis <- daten_sf %>%
  mutate(
    Predicted_Wind_Density = predict(modell_roh, newdata = df),
    Residuals = residuals(modell_roh),
    Residuals_Std = as.numeric(scale(Residuals)),
    Performance_Class = case_when(
      Residuals_Std >= 1.5  ~ "Outperformer (Hoch)",
      Residuals_Std <= -1.5 ~ "Underperformer (Tief)",
      TRUE ~ "Normal"
    )
  )

table(daten_ergebnis$Performance_Class)

st_write(daten_ergebnis, "data/smart_planner_daten_mit_residuen.gpkg", delete_dsn = TRUE, quiet = TRUE)
write_csv(st_drop_geometry(daten_ergebnis), "data/smart_planner_daten_mit_residuen.csv")

save(modell_roh, modell_std, vif_werte, bp_test, moran_test,
     modell_nur_wind, modell_nur_nord, robustheit_tabelle,
     file = "data/modell_ergebnisse.RData")
