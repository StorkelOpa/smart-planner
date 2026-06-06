# 2. Statistische Modellierung & Diagnostik (Statistical Modeling)

Dieses Dokument erläutert das Regressionsmodell (OLS) zur Beantwortung der Forschungsfrage, die Standardisierung der Koeffizienten sowie die Durchführung und Interpretation der statistischen Diagnosetests.

---

## 1. Das Regressionsmodell (OLS)

Zur Untersuchung der Forschungsfrage wird eine multiple lineare Regression (Ordinary Least Squares, OLS) geschätzt. Die mathematische Gleichung lautet:

$$Y_i = \beta_0 + \beta_1 \cdot \text{Steuerkraft}_i + \beta_2 \cdot \text{Einwohnerdichte}_i + \beta_3 \cdot \text{Windgeschwindigkeit}_i + \beta_4 \cdot \text{Wald}_i + \beta_5 \cdot \text{Landwirtschaft}_i + \beta_6 \cdot \text{Industriebeschäftigte}_i + \beta_7 \cdot \text{Landwirtschaftsbeschäftigte}_i + \epsilon_i$$

Wobei:
* $Y_i$: **Windkapazitätsdichte** ($\text{kW/km}^2$) des Landkreises $i$.
* $\text{Steuerkraft}_i$: Steuereinnahmekraft (€/Ew.) – **primäre unabhängige Variable** der Forschungsfrage.
* $\text{Windgeschwindigkeit}_i$: mittlere Windgeschwindigkeit (m/s, 150 m Nabenhöhe, Global Wind Atlas) als Maß des physischen **Windpotenzials**. Diese Kontrollvariable ist zentral, da das Windangebot eine wesentliche Voraussetzung des Ausbaus ist (siehe Befund in Abschnitt 2).
* Die weiteren Variablen sind Kontrollvariablen, die den Einfluss von Siedlungseinschränkungen, Flächenverfügbarkeit und der lokalen Wirtschaftsstruktur herausrechnen. (Der Beschäftigtenanteil im Dienstleistungssektor wird ausgelassen, um perfekte Multikollinearität zu vermeiden).

---

## 2. Standardisierung der Variablen (Z-Transformation)

Da die Variablen in unterschiedlichen Einheiten gemessen werden (€/Ew., Ew./km², %-Anteile), können die rohen Regressionskoeffizienten ($\beta$-Schätzer) nicht direkt verglichen werden. Ein Anstieg der Steuerkraft um 1 € ist nicht vergleichbar mit einem Anstieg des Waldanteils um 1 %.

Daher schätzen wir ein zweites Modell mit **Z-standardisierten Variablen**:

$$Z(x) = \frac{x - \mu}{\sigma}$$

* **Interpretation der standardisierten Koeffizienten:** Der standardisierte Koeffizient gibt an, um wie viele Standardabweichungen sich die Zielvariable (Windkraftdichte) ändert, wenn die entsprechende unabhängige Variable um eine Standardabweichung steigt, während alle anderen Variablen konstant gehalten werden.
* **Ergebnis:** Den mit Abstand stärksten Einfluss hat die **Windgeschwindigkeit** mit einem standardisierten Koeffizienten von **+0.464** ($p < 0.001$) – das physische Windpotenzial erklärt den Ausbau am besten. Es folgen **Einwohnerdichte** (**-0.254**, $p = 0.019$) und **Waldflächenanteil** (**-0.204**, $p = 0.045$) als signifikante räumliche Restriktionen.
* **Zentraler Befund zur Forschungsfrage:** Die **Steuerkraft** liegt bei lediglich **+0.016** ($p = 0.73$) und ist damit **nicht signifikant**. Der in einer bivariaten Betrachtung sichtbare negative Zusammenhang (vgl. [03_ggplot_visualisations.md](file:///home/carl/Code_Projekte/Smart%20Planner/docs/03_ggplot_visualisations.md)) verschwindet, sobald das Windpotenzial kontrolliert wird. Es handelt sich also weitgehend um einen **Scheinzusammenhang (Omitted-Variable-Bias)**: Finanzschwache, ländliche Kreise im norddeutschen Tiefland sind zugleich besonders windreich. Nach Kontrolle dieses Faktors lässt sich kein eigenständiger Einfluss der kommunalen Finanzkraft auf den Windkraftausbau nachweisen.

---

## 3. Regressionsdiagnostik und mathematische Hintergründe

Die OLS-Schätzung beruht auf restriktiven Annahmen. Wir überprüfen diese systematisch in [05_model_regression.R](file:///home/carl/Code_Projekte/Smart%20Planner/scripts/05_model_regression.R):

### A. Multikollinearität (VIF)
Multikollinearität tritt auf, wenn unabhängige Variablen stark miteinander korrelieren. Dies bläht die Standardfehler der Koeffizienten auf und macht die Schätzer instabil. Wir nutzen den **Variance Inflation Factor (VIF)**:

$$\text{VIF}_j = \frac{1}{1 - R_j^2}$$

Wobei $R_j^2$ das Bestimmtheitsmaß einer Hilfsregression der Variable $j$ auf alle anderen unabhängigen Variablen ist.
* **Interpretation:** Ein VIF > 10 (manchmal auch > 5) deutet auf problematische Multikollinearität hin.
* **Ergebnisse:** Unsere VIF-Werte liegen für Wald- und Landwirtschaftsflächen bei **6,5** bzw. **8,2** und für Einwohnerdichte bei **7,3**. Dies ist leicht erhöht (typisch für Raumindikatoren), ist aber rechnerisch noch im Rahmen. Die Steuerkraft (**1,44**) und die neu aufgenommene Windgeschwindigkeit (**1,90**) weisen keinerlei Multikollinearitätsprobleme auf.

### B. Heteroskedastizität (Breusch-Pagan-Test)
OLS nimmt Homoskedastizität (konstante Varianz der Fehlerterme $\epsilon$) an. Wenn die Fehlerstreuung nicht konstant ist (Heteroskedastizität), sind die p-Werte unzuverlässig. Der **Breusch-Pagan-Test** prüft, ob die quadrierten Residuen von den unabhängigen Variablen abhängen:

$$\epsilon_i^2 = \gamma_0 + \gamma_1 x_{i1} + \dots + v_i$$

* **Nullhypothese ($H_0$):** Homoskedastizität liegt vor.
* **Ergebnis:** $\text{BP} = 43.85$ ($df = 7$, $p < 0.001$). Die Nullhypothese wird verworfen. Es liegt signifikante Heteroskedastizität vor. In der Shiny-App und Interpretation sollte dies beachtet werden (Hinweis auf robuste Standardfehler oder räumliche Strukturen).

### C. Räumliche Autokorrelation (Moran's I)
In geografischen Regressionsmodellen ist die Annahme unabhängiger Beobachtungen oft verletzt, da benachbarte Landkreise sich gegenseitig beeinflussen (räumliche Autokorrelation). Wir testen die Residuen mittels **Global Moran's I**:

$$I = \frac{N}{S_0} \frac{\sum_i \sum_j w_{ij}(e_i)(e_j)}{\sum_i e_i^2}$$

Wobei $w_{ij}$ die räumliche Gewichtungsmatrix (Queen-Nachbarschaft) darstellt, $e_i$ die OLS-Residuen sind und $S_0$ die Summe aller Gewichte ist.
* **Insel-Problem im BKG-Datensatz:**
  Einige Landkreise (wie z. B. der Landkreis Vorpommern-Rügen oder die kreisfreie Stadt Wilhelmshaven) haben aufgrund der Polygon-Topologie oder Küstenlage keine direkt angrenzenden Landnachbarn in der Matrix (sie sind "Inseln"). Dies würde in R ohne Gegensteuerung zum Absturz der Gewichtungsberechnung führen.
  In R lösen wir dies über das Argument `zero.policy = TRUE` in den Funktionen `nb2listw()` und `lm.morantest()` des `spdep`-Pakets:
  ```R
  # Nachbarschaftsliste (Queen-Kriterium)
  nb <- poly2nb(districts_sf)
  # Gewichtungsmatrix mit Null-Gewichtung für Inseln
  listw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  # Moran's I für Regressionsresiduen
  moran_test <- lm.morantest(model_raw, listw, zero.policy = TRUE)
  ```
* **Ergebnis:** $\text{Moran's I} = 0,260$ ($z = 8.13$, $p < 0.001$). Es liegt eine hochgradig signifikante, positive räumliche Autokorrelation vor. Die Residuen des Modells clustern räumlich stark (z. B. der hohe Windkraftausbau im norddeutschen Tiefland).

---

## 4. Berechnung der Residuen & Klassifikation der Performer

Residuen stellen die Abweichung der tatsächlichen Windkraftdichte vom statistisch prognostizierten Wert dar ($e_i = y_i - \hat{y}_i$).
* Ein **positives Residuum** zeigt, dass ein Landkreis *mehr* Windenergie installiert hat, als sein Windpotenzial, seine Finanzkraft und seine geografischen Bedingungen vermuten lassen.
* Ein **negatives Residuum** zeigt einen *Rückstand* an.

Wir standardisieren diese Residuen (Z-Score) und klassifizieren die Landkreise:
* **Outperformer:** Standardisiertes Residuum $\ge 1,5$ (17 Landkreise).
* **Underperformer:** Standardisiertes Residuum $\le -1.5$ (6 Landkreise).
* **Normal:** Dazwischen (377 Landkreise).

Diese Abweichungen sind ein starker Indikator für weiche Faktoren (z. B. lokale Akzeptanz, politische Priorisierung im jeweiligen Bundesland oder schnelle Planungsverfahren).
