# 3. Visualisierungs-Code & Interpretation (ggplot2 Plots)

Dokumentiert den R-Code für die 8 Grafiken im Ordner `plots/`
(`scripts/07_generate_plots.R`) und liefert Formulierungshilfen für das
empirische Kapitel des Berichts.

## Warum eine log-Skala?

Die Windkraftdichte ist stark rechtsschief: der Median liegt bei nur **4 %**
des Maximalwerts (1.723 kW/km²). Auf einer normalen (linearen) Achse kleben
dadurch fast alle 400 Kreise in einem dünnen Streifen nahe 0, während ein
Dutzend Ausreißer die ganze Skala aufspannen — Unterschiede zwischen z. B.
20 und 200 kW/km² wären nicht mehr zu erkennen. Plot 1–4 nutzen deshalb eine
**pseudo-logarithmische Skala** (`scales::pseudo_log_trans()`) auf der
Windkraftdichte-Achse: gleicher Achsenabstand bedeutet "verzehnfacht" statt
"plus X", wodurch sich die Punkte über die ganze Fläche verteilen — inkl.
der Kreise mit exakt 0 Windkraft, die ein normaler Logarithmus nicht
darstellen könnte. **Das regressionsmodell selbst (Kapitel 2) rechnet
weiterhin mit den unveränderten Rohwerten** — nur die Darstellung ändert
sich, nicht die Daten oder das Modell.

---

## Plot 1: Bivariate Analyse (Einnahmen vs. Ausbau)
* **Datei:** `plots/01_bivariate_scatter.png`
* **Zweck:** Rohdatenpunkte der 400 Landkreise mit zwei Ausgleichslinien
  (Linear OLS vs. nicht-lineares LOESS), y-Achse log-skaliert.

```R
p1 <- ggplot(df, aes(x = Steuerkraft, y = Wind_Density_kW_km2)) +
  geom_point(aes(color = Performance_Class), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", aes(fill = "Linear (OLS)"), color = "#0ea5e9", linewidth = 1.2, se = TRUE) +
  geom_smooth(method = "loess", aes(fill = "Nicht-linear (LOESS)"), color = "#10b981", linetype = "dashed", linewidth = 1, se = FALSE) +
  scale_color_manual(values = c("Normal" = "#64748b", "Outperformer (Hoch)" = "#059669", "Underperformer (Tief)" = "#dc2626")) +
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10), breaks = c(0, 10, 100, 1000)) +
  theme_report()
```

* **Interpretation:** *"Der Scatterplot zeigt eine bivariate negative
  Neigung der OLS-Regressionsgeraden: rein optisch geht eine höhere
  Steuerkraft mit weniger installierter Windkapazität einher. Dieser
  Eindruck täuscht über die Kausalstruktur hinweg: Im multivariaten Modell
  (Kapitel 2) ist der Steuerkraft-Effekt nicht mehr signifikant ($p=0{,}73$).
  Der bivariate Trend entsteht, weil die windreichen Kreise des
  norddeutschen Tieflands zugleich finanzschwächer sind
  (Omitted-Variable-Bias)."*

---

## Plot 2: Boxplots nach Steuerkraft-Quartilen
* **Datei:** `plots/02_quartile_boxplots.png`
* **Zweck:** Robustere Prüfung desselben Zusammenhangs, gruppiert in vier
  gleich große Steuerkraft-Klassen.

```R
df_quartile <- df %>%
  mutate(Steuerkraft_Quartil = cut(Steuerkraft, breaks = quantile(Steuerkraft, probs = 0:4/4),
                                    include.lowest = TRUE,
                                    labels = c("Q1 (Finanzschwach)", "Q2", "Q3", "Q4 (Finanzstark)")))
p2 <- ggplot(df_quartile, aes(x = Steuerkraft_Quartil, y = Wind_Density_kW_km2)) +
  geom_violin(fill = "#bae6fd", color = "#0ea5e9", alpha = 0.4) +
  geom_boxplot(width = 0.2, fill = "white", outlier.color = "#dc2626") +
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10), breaks = c(0, 10, 100, 1000)) +
  theme_report()
```

* **Interpretation:** *"Die Median-Windkraftdichte sinkt von Q1
  (finanzschwach) zu Q4 (finanzstark) kontinuierlich ab — dasselbe Muster
  wie in Plot 1. Die log-Skala macht zusätzlich sichtbar, dass die
  Verteilung in allen vier Quartilen ähnlich rechtsschief ist."*

---

## Plot 3: Ausbau nach Wirtschaftsstruktur
* **Datei:** `plots/03_wirtschaftsstruktur.png`
* **Zweck:** Unterschied zwischen industriellen, agrarischen und
  dienstleistungsorientierten Kreisen.

* **Interpretation:** *"'Ländlich/Agrarisch' geprägte Kreise weisen im
  Median die höchste Windkraftdichte auf. Industriell dominierte Kreise und
  Dienstleistungsregionen liegen deutlich niedriger — plausibel, da
  landwirtschaftlich geprägte Kreise über die nötigen Freiflächen verfügen
  und geringere Siedlungsdichten aufweisen."*

---

## Plot 4: Einfluss der Kontrollvariablen
* **Datei:** `plots/04_kontrollvariablen.png`
* **Zweck:** Facettiertes Raster — isoliert die Einzeleffekte von
  Windgeschwindigkeit, Einwohnerdichte und Flächennutzung.

* **Interpretation:** *"Windgeschwindigkeit zeigt den stärksten, klar
  positiven Zusammenhang — der physische Rohstoff selbst ist der wichtigste
  Treiber. Einwohnerdichte (log-skaliert) und Waldflächenanteil wirken
  negativ: beides physische bzw. planerische Ausschlusskriterien für
  Windkraftanlagen. Der Landwirtschaftsanteil zeigt eine leicht positive
  Tendenz, was offene Kulturlandschaften als bevorzugte Ausbauflächen
  bestätigt."*

---

## Plot 5: Residuenanalyse (Outperformer)
* **Datei:** `plots/05_residuen_ausreisser.png`
* **Zweck:** Die zehn Kreise mit der größten positiven bzw. negativen
  Modellabweichung. **Keine log-Skala** — Residuen streuen um 0 und sind
  kein rechtsschiefes Rohmaß.

* **Interpretation:** *"An der Spitze der Outperformer stehen Kreise wie
  Rotenburg (Wümme), Dithmarschen und der Heidekreis — sie bauen
  signifikant mehr Windkraft aus, als ihre sozioökonomischen und
  geografischen Kennzahlen erwarten lassen. Das deutet auf lokale
  politische Steuerung und Planungsakzeptanz als zusätzlichen, hier nicht
  gemessenen Faktor hin."*

---

## Plots 6a & 6b: Modelldiagnostik
* **Dateien:** `plots/06a_residuen_fitted.png`, `plots/06b_qq_plot.png`
* **Zweck:** Prüfung der OLS-Annahmen (Linearität, Heteroskedastizität,
  Normalverteilung).

* **Interpretation:** *"Der Residuals-vs-Fitted-Plot zeigt eine
  trichterförmige Streuung bei höheren vorhergesagten Werten
  (Heteroskedastizität, bestätigt durch den Breusch-Pagan-Test, Kapitel 2).
  Der Q-Q-Plot passt in der Mitte gut, weicht aber an den Rändern ab (Fat
  Tails) — verursacht durch wenige Kreise mit extrem hohem Ausbau, die das
  Modell unterschätzt."*

---

## Plot 7 (neu): Koeffizientenplot — das Kernergebnis
* **Datei:** `plots/07_koeffizientenplot.png`
* **Zweck:** Zeigt alle sieben Einflussfaktoren gleichzeitig mit
  standardisierten Effektstärken und 95 %-Konfidenzintervallen — beantwortet
  direkt die Forschungsfrage: welche Faktoren haben nach gegenseitiger
  Kontrolle noch Einfluss?

```R
koeff <- as.data.frame(summary(modell_std)$coefficients)
koeff$Lower <- confint(modell_std)[, 1]
koeff$Upper <- confint(modell_std)[, 2]

p7 <- ggplot(koeff, aes(x = reorder(Klarname, Estimate), y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#dc2626") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "#94a3b8") +
  geom_point(color = "#0284c7", size = 4) +
  coord_flip() +
  theme_report()
```

* **Interpretation:** *"Jede Zeile ist ein Faktor: der Punkt zeigt Stärke
  und Richtung des Effekts, der Balken das 95 %-Konfidenzintervall. Die
  Windgeschwindigkeit hat mit Abstand den stärksten (positiven) Effekt und
  kreuzt die Null-Linie nicht — ein belastbarer Befund. Der Balken der
  Steuerkraft kreuzt die Null-Linie deutlich: der Effekt ist statistisch
  nicht von 0 zu unterscheiden. Diese eine Grafik fasst damit den zentralen
  Befund des gesamten Berichts zusammen: physisches Windpotenzial erklärt
  den Ausbau, kommunale Finanzkraft nicht."*

---

## Plot 8 (neu): Robustheitscheck — Steuerkraft über drei Stichproben
* **Datei:** `plots/08_robustheitscheck.png`
* **Zweck:** Visualisiert die Robustheitschecks aus
  [02_statistische_modellierung.md](02_statistische_modellierung.md#5-robustheitschecks):
  derselbe Steuerkraft-Koeffizient (Rohwerte, kW/km² je €/Einwohner — direkt
  vergleichbar, da dieselbe Variable/Einheit in allen drei Fällen), einmal
  auf allen 400 Kreisen geschätzt und zusätzlich auf zwei eingeschränkten
  Teilstichproben (nur überdurchschnittliche Windgeschwindigkeit, nur
  Norddeutschland).

```R
p8 <- ggplot(robustheit_tabelle, aes(x = Stichprobe, y = Steuerkraft_Koeffizient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#dc2626") +
  geom_errorbar(aes(ymin = Steuerkraft_Lower, ymax = Steuerkraft_Upper), width = 0.15, color = "#94a3b8") +
  geom_point(color = "#0284c7", size = 4) +
  coord_flip() +
  theme_report()
```

* **Interpretation:** *"Egal ob man alle 400 Kreise betrachtet oder die
  Stichprobe auf besonders windreiche bzw. norddeutsche Kreise beschränkt —
  der Steuerkraft-Koeffizient bleibt klein und sein Konfidenzintervall
  kreuzt in allen drei Fällen deutlich die Null-Linie. Das schließt aus,
  dass der fehlende Zusammenhang nur ein Artefakt der gewählten Stichprobe
  ist: die Frage 'wirkt Finanzkraft nur bei vergleichbaren Kreisen?' wurde
  konkret getestet und mit Nein beantwortet."*
