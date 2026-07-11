# ==============================================================================
# SCHRITT 6: REGRESSION, DIAGNOSTIK & ROBUSTHEITSCHECKS
# ==============================================================================
# Schaetzt das OLS-Hauptmodell (Rohwerte + standardisierte Version), prueft
# die Modellannahmen (VIF, Breusch-Pagan, Moran's I) und rechnet zusaetzlich
# zwei Robustheitschecks auf eingeschraenkten Teilstichproben (siehe
# docs/02_statistische_modellierung.md, Abschnitt "Robustheitschecks").
#
# Eingang : data/smart_planner_daten.gpkg
# Ausgang : data/smart_planner_daten_mit_residuen.gpkg / .csv
#           data/modell_ergebnisse.RData
# ==============================================================================

library(sf)
library(tidyverse)
library(car)
library(lmtest)
library(spdep)

cat("Lade finalen Datensatz...\n")
daten_sf <- st_read("data/smart_planner_daten.gpkg", quiet = TRUE)
df <- daten_sf %>% st_drop_geometry() %>% as_tibble()

# ==============================================================================
# 1. HAUPTMODELL (alle 400 Kreise)
# ==============================================================================
cat("\nSchaetze Hauptmodell (N = 400)...\n")

modell_formel <- Wind_Density_kW_km2 ~ Steuerkraft + Einwohnerdichte +
  Windgeschwindigkeit_ms + Waldflaeche_Prozent + Landwirtschaft_Prozent +
  Beschaeftigte_Sekundar + Beschaeftigte_Primar
# (Beschaeftigte_Tertiar wird ausgelassen, da die drei Sektoranteile in
#  Summe 100% ergeben - sonst perfekte Multikollinearitaet.)

modell_roh <- lm(modell_formel, data = df)
print(summary(modell_roh))

cat("\nSchaetze standardisierte Version (Z-Scores, fuer vergleichbare Effektstaerken)...\n")
df_z <- df %>%
  select(Wind_Density_kW_km2, Steuerkraft, Einwohnerdichte, Windgeschwindigkeit_ms,
         Waldflaeche_Prozent, Landwirtschaft_Prozent,
         Beschaeftigte_Sekundar, Beschaeftigte_Primar) %>%
  mutate(across(everything(), ~ as.vector(scale(.))))

modell_std <- lm(modell_formel, data = df_z)
print(summary(modell_std))

# ==============================================================================
# 2. DIAGNOSTIK
# ==============================================================================
cat("\n--- DIAGNOSTIK ---\n")

vif_werte <- vif(modell_roh)
cat("\nVariance Inflation Factors (Multikollinearitaet):\n")
print(vif_werte)

bp_test <- bptest(modell_roh)
cat("\nBreusch-Pagan-Test (Heteroskedastizitaet):\n")
print(bp_test)

cat("\nMoran's I (raeumliche Autokorrelation der Residuen)...\n")
nb <- poly2nb(daten_sf)
listw <- nb2listw(nb, style = "W", zero.policy = TRUE)
moran_test <- lm.morantest(modell_roh, listw, zero.policy = TRUE)
print(moran_test)

# ==============================================================================
# 3. ROBUSTHEITSCHECKS: dasselbe Modell auf zwei eingeschraenkten
#    Teilstichproben. Ziel: bleibt der (nicht signifikante) Steuerkraft-
#    Effekt bestehen, wenn wir Kreise vergleichbarer machen? Ersetzt NICHT
#    das Hauptmodell, sondern ergaenzt es (siehe docs/02, "Robustheitschecks":
#    eine geografische Einschraenkung als Hauptmodell wuerde den Kernbefund
#    verzerren, da der Nord-Sued-Kontrast selbst Teil des Befunds ist).
# ==============================================================================
cat("\n--- ROBUSTHEITSCHECKS AUF TEILSTICHPROBEN ---\n")

nord_bundeslaender <- c("Schleswig-Holstein", "Mecklenburg-Vorpommern",
                        "Niedersachsen", "Bremen", "Hamburg")

df_nur_wind <- df %>% filter(Windgeschwindigkeit_ms >= median(Windgeschwindigkeit_ms))
modell_nur_wind <- lm(modell_formel, data = df_nur_wind)

df_nur_nord <- df %>% filter(Bundesland %in% nord_bundeslaender)
modell_nur_nord <- lm(modell_formel, data = df_nur_nord)

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
cat("\nSteuerkraft-Effekt: Hauptmodell vs. Robustheitschecks\n")
print(robustheit_tabelle)

# ==============================================================================
# 4. RESIDUEN & PERFORMANCE-KLASSIFIKATION
# ==============================================================================
cat("\nBerechne Residuen und klassifiziere Outperformer/Underperformer...\n")
daten_ergebnis <- daten_sf %>%
  mutate(
    Predicted_Wind_Density = predict(modell_roh, newdata = df),
    Residuals = residuals(modell_roh),
    Residuals_Std = as.vector(scale(Residuals)),
    Performance_Class = case_when(
      Residuals_Std >= 1.5  ~ "Outperformer (Hoch)",
      Residuals_Std <= -1.5 ~ "Underperformer (Tief)",
      TRUE ~ "Normal"
    )
  )

cat("Kreise nach Performance-Klasse:\n")
print(table(daten_ergebnis$Performance_Class))

st_write(daten_ergebnis, "data/smart_planner_daten_mit_residuen.gpkg", delete_dsn = TRUE, quiet = TRUE)
write_csv(st_drop_geometry(daten_ergebnis), "data/smart_planner_daten_mit_residuen.csv")

save(modell_roh, modell_std, vif_werte, bp_test, moran_test,
     modell_nur_wind, modell_nur_nord, robustheit_tabelle,
     file = "data/modell_ergebnisse.RData")

cat("\nRegression, Diagnostik und Robustheitschecks abgeschlossen.\n")
