# 3. ggplot2 Visualisierungs-Code & Interpretation (ggplot2 Plots)

Dieses Dokument dokumentiert den R-Code für die 6 erzeugten ggplot2-Grafiken im Ordner `plots/` und bietet Formulierungshilfen sowie Interpretationen für das empirische Kapitel Ihres Abschlussberichts.

---

## Plot 1: Bivariate Analyse (Einnahmen vs. Ausbau)
* **Dateiname:** `plots/01_bivariate_scatter.png`
* **Zweck:** Zeigt die Rohdatenpunkte der 400 Landkreise mit zwei Ausgleichslinien (Linear OLS vs. Nicht-lineares LOESS).

### R-Code und Erklärung:
```R
p1 <- ggplot(df_plot, aes(x = Steuerkraft, y = Wind_Density_kW_km2)) +
  # 1. Datenpunkte zeichnen, eingefärbt nach ihrer relativen Modell-Performance
  geom_point(aes(color = Performance_Class), alpha = 0.7, size = 2) +
  
  # 2. Lineare Regressionsgerade (OLS) inklusive grauem Konfidenzintervall
  geom_smooth(method = "lm", aes(fill = "Linear (OLS)"), color = "#0ea5e9", size = 1.2, se = TRUE) +
  
  # 3. Nicht-lineare LOESS-Trendlinie (zur Aufdeckung lokaler Abweichungen)
  geom_smooth(method = "loess", aes(fill = "Nicht-linear (LOESS)"), color = "#10b981", linetype = "dashed", size = 1, se = FALSE) +
  
  # 4. Manuelle Farbzuweisungen für Punkte und Flächen
  scale_color_manual(
    values = c("Normal" = "#64748b", "Outperformer (Hoch)" = "#059669", "Underperformer (Tief)" = "#dc2626"),
    name = "Performance-Klasse"
  ) +
  scale_fill_manual(
    values = c("Linear (OLS)" = "#bae6fd", "Nicht-linear (LOESS)" = NA),
    name = "Modellanpassung"
  ) +
  theme_report()
```
* **Interpretation für den Bericht:** 
  *"Der Scatterplot verdeutlicht die negative Neigung der OLS-Regressionsgeraden. Dies stützt die These, dass eine höhere steuerliche Einnahmenkraft auf kommunaler Ebene mit einer geringeren installierten Windkapazität pro Quadratkilometer einhergeht. Die nicht-lineare LOESS-Kurve offenbart zudem, dass der negative Zusammenhang insbesondere im mittleren Einnahmenbereich stark ausgeprägt ist, während er bei extrem einkommensstarken Kreisen abflacht."*

---

## Plot 2: Boxplots nach Steuerkraft-Quartilen
* **Dateiname:** `plots/02_quartile_boxplots.png`
* **Zweck:** Aggregiert die Steuerkraft in vier gleich große Klassen (Quartile), um Verteilungsunterschiede robuster aufzuzeigen.

### R-Code und Erklärung:
```R
# 1. Kontinuierliche Steuerkraft in vier Quantile (Klassen) einteilen
df_quartiles <- df_plot %>%
  mutate(
    Steuerkraft_Quartil = cut(
      Steuerkraft,
      breaks = quantile(Steuerkraft, probs = 0:4/4),
      include.lowest = TRUE,
      labels = c("Q1 (Finanzschwach)", "Q2 (Mittel-Unter)", "Q3 (Mittel-Ober)", "Q4 (Finanzstark)")
    )
  )

# 2. Boxplot mit unterlegtem Violin-Plot zur Darstellung der Dichteverteilung
p2 <- ggplot(df_quartiles, aes(x = Steuerkraft_Quartil, y = Wind_Density_kW_km2)) +
  geom_violin(fill = "#bae6fd", color = "#0ea5e9", alpha = 0.4) +
  geom_boxplot(width = 0.2, fill = "white", color = "#0f172a", outlier.size = 1.5, outlier.color = "#dc2626")
```
* **Interpretation für den Bericht:**
  *"Die Quartilsanalyse unterstreicht das Muster der bivariaten Regression. Die Median-Windkraftdichte sinkt von der finanzschwächsten Gruppe (Q1) hin zur finanzstärksten Gruppe (Q4) kontinuierlich ab. Die Breite der Violinen zeigt zudem, dass die Verteilung in allen Quartilen rechtsschief ist, wobei sich die extremsten Ausreißer (rote Punkte) vor allem in den ersten drei Quartilen befinden."*

---

## Plot 3: Ausbau nach Wirtschaftsstruktur
* **Dateiname:** `plots/03_economic_structure.png`
* **Zweck:** Prüft den Unterschied des Ausbaus zwischen industriellen, agrarischen und dienstleistungsorientierten Landkreisen.

### R-Code und Erklärung:
```R
p3 <- ggplot(df_plot, aes(x = Economic_Structure, y = Wind_Density_kW_km2)) +
  geom_violin(aes(fill = Economic_Structure), alpha = 0.5, color = "#475569") +
  # Boxplot im Violin-Plot zeichnen, Ausreißer ausblenden (da die Violine diese zeigt)
  geom_boxplot(width = 0.1, fill = "white", color = "#0f172a", outlier.shape = NA) +
  scale_fill_brewer(palette = "Set2")
```
* **Interpretation für den Bericht:**
  *"Der Vergleich der Wirtschaftsstrukturtypen zeigt, dass 'Ländlich/Agrarisch' geprägte Kreise im Median die höchste Windkraftdichte aufweisen. Industriell dominierte Kreise und Dienstleistungsregionen zeigen signifikant geringere Werte. Dies liegt nahe, da landwirtschaftlich geprägte Kreise über die notwendigen großen Freiflächen verfügen und oft geringere Siedlungsdichten aufweisen."*

---

## Plot 4: Einfluss der Kontrollvariablen
* **Dateiname:** `plots/04_control_variables.png`
* **Zweck:** Ein mehrteiliges Raster zur Isolierung der Einzeleffekte von Dichte und Geografie.

### R-Code und Erklärung:
```R
# 1. Daten ins Langformat transformieren, um das Rastern (faceting) zu ermöglichen
df_long <- df_plot %>%
  mutate(Log10_Einwohnerdichte = log10(Einwohnerdichte)) %>%
  select(Wind_Density_kW_km2, Log10_Einwohnerdichte, Waldflaeche_Prozent, Landwirtschaft_Prozent) %>%
  pivot_longer(
    cols = c(Log10_Einwohnerdichte, Waldflaeche_Prozent, Landwirtschaft_Prozent),
    names_to = "Variable",
    values_to = "Wert"
  )

# 2. Faceted Plot erstellen mit flexibler x-Achsenskalierung
p4 <- ggplot(df_long, aes(x = Wert, y = Wind_Density_kW_km2)) +
  geom_point(alpha = 0.4, color = "#64748b", size = 1.2) +
  geom_smooth(method = "lm", color = "#0ea5e9", fill = "#bae6fd", alpha = 0.2, size = 1) +
  facet_wrap(~ Variable_Clean, scales = "free_x")
```
* **Interpretation für den Bericht:**
  *"Die Facetten-Analyse bestätigt den starken, erwarteten negativen Einfluss der Einwohnerdichte (logarithmisch skaliert) und des Waldflächenanteils. Beide Faktoren stellen physische und planerische Ausschlusskriterien für Windkraftanlagen dar. Der landwirtschaftliche Flächenanteil zeigt hingegen eine leicht positive, aber flachere Tendenz, was die Rolle offener Kulturlandschaften als primäre Ausbauflächen untermauert."*

---

## Plot 5: Residuenanalyse (Outperformer)
* **Dateiname:** `plots/05_model_outliers.png`
* **Zweck:** Identifiziert diejenigen Kreise, die am weitesten nach oben oder unten vom statistischen Erwartungswert abweichen.

### R-Code und Erklärung:
```R
# 1. Top 10 positive und negative Residuen extrahieren
top_outperformers <- head(outliers, 10)
top_underperformers <- tail(outliers, 10)
top_both <- bind_rows(top_outperformers, top_underperformers)

# 2. Plot erstellen
p5 <- ggplot(top_both, aes(x = reorder(Landkreis, Residuals), y = Residuals, fill = Performance_Class)) +
  geom_col(color = "black", size = 0.2) +
  coord_flip() # Horizontale Ausrichtung für bessere Lesbarkeit der Kreisnamen
```
* **Interpretation für den Bericht:**
  *"Die Residuenanalyse identifiziert die stärksten Outperformer-Landkreise. An der Spitze stehen Kreise wie Rotenburg (Wümme), Dithmarschen und der Heidekreis. Diese Landkreise weisen eine signifikant höhere installierte Windkraftleistung auf, als durch ihre sozioökonomischen und geografischen Daten prognostiziert. Dies legt nahe, dass lokale politische Steuerungen und Planungsakzeptanz dort den Ausbau stark begünstigen."*

---

## Plots 6a & 6b: Modelldiagnostik
* **Dateiname:** `plots/06a_residuals_fit.png` und `plots/06b_qq_plot.png`
* **Zweck:** Überprüfung der OLS-Modellannahmen (Linearität, Heteroskedastizität, Normalverteilung der Fehler).

### R-Code und Erklärung:
```R
# Residuals vs. Fitted
p6_fit <- ggplot(diag_df, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "#64748b", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "#dc2626", linetype = "dashed") +
  geom_smooth(method = "loess", color = "#0ea5e9", se = FALSE)

# QQ-Plot
p6_qq <- ggplot(diag_df, aes(sample = Std_Residuals)) +
  stat_qq(color = "#64748b", alpha = 0.5) +
  stat_qq_line(color = "#0ea5e9")
```
* **Interpretation für den Bericht:**
  *"Der Residuals-vs-Fitted-Plot zeigt eine trichterförmige Streuung der Residuen bei höheren vorhergesagten Werten, was auf eine Verletzung der Homoskedastizitätsannahme hindeutet (bestätigt durch den Breusch-Pagan-Test). Der Q-Q-Plot zeigt eine relativ gute Anpassung in der Mitte, aber erhebliche Abweichungen an den Rändern (Fat Tails). Dies resultiert aus einigen wenigen Landkreisen mit extrem hohem Windkraft-Ausbau, die das Modell unterschätzt."*
