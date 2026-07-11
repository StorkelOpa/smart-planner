# 2. Statistische Modellierung & Diagnostik

Dieses Dokument erläutert das Regressionsmodell (OLS), die Standardisierung
der Koeffizienten, die Diagnosetests und die Robustheitschecks
(`scripts/06_model_regression.R`).

---

## 1. Das Regressionsmodell (OLS)

$$Y_i = \beta_0 + \beta_1 \cdot \text{Steuerkraft}_i + \beta_2 \cdot \text{Einwohnerdichte}_i + \beta_3 \cdot \text{Windgeschwindigkeit}_i + \beta_4 \cdot \text{Wald}_i + \beta_5 \cdot \text{Landwirtschaft}_i + \beta_6 \cdot \text{Industriebeschäftigte}_i + \beta_7 \cdot \text{Landwirtschaftsbeschäftigte}_i + \epsilon_i$$

* $Y_i$: **Windkapazitätsdichte** (kW/km²) des Landkreises $i$.
* $\text{Steuerkraft}_i$: Steuereinnahmekraft (€/Ew.) — **primäre unabhängige Variable**.
* $\text{Windgeschwindigkeit}_i$: mittlere Windgeschwindigkeit (m/s, 150 m
  Nabenhöhe) als Maß des physischen **Windpotenzials** — zentral, da das
  Windangebot eine wesentliche Voraussetzung des Ausbaus ist.
* Die übrigen Variablen sind Kontrollvariablen für Siedlungseinschränkungen,
  Flächenverfügbarkeit und lokale Wirtschaftsstruktur. Der
  Dienstleistungssektor wird als Regressor ausgelassen, um perfekte
  Multikollinearität zu vermeiden (die drei Sektoranteile summieren zu 100 %).

---

## 2. Standardisierung der Variablen (Z-Transformation)

Da die Variablen unterschiedliche Einheiten haben (€/Ew., Ew./km², %-Anteile),
schätzen wir zusätzlich ein Modell mit **Z-standardisierten Variablen**
($Z(x) = \frac{x-\mu}{\sigma}$), dessen Koeffizienten direkt vergleichbar sind.

* **Ergebnis:** Den mit Abstand stärksten Einfluss hat die
  **Windgeschwindigkeit** (**+0,464**, $p < 0{,}001$). Es folgen
  **Einwohnerdichte** (**-0,254**, $p = 0{,}019$) und **Waldflächenanteil**
  (**-0,204**, $p = 0{,}045$).
* **Zentraler Befund:** Die **Steuerkraft** liegt bei lediglich **+0,016**
  ($p = 0{,}73$) — **nicht signifikant**. Der bivariate negative
  Zusammenhang (vgl. [03_visualisierungen.md](03_visualisierungen.md))
  verschwindet, sobald das Windpotenzial kontrolliert wird
  (Omitted-Variable-Bias): Finanzschwache, ländliche Kreise im
  norddeutschen Tiefland sind zugleich besonders windreich.

---

## 3. Regressionsdiagnostik

### A. Multikollinearität (VIF)

$$\text{VIF}_j = \frac{1}{1 - R_j^2}$$

Wald- und Landwirtschaftsflächen liegen bei **6,5** bzw. **8,2**,
Einwohnerdichte bei **7,3** — leicht erhöht (typisch für Raumindikatoren),
rechnerisch aber im Rahmen. Steuerkraft (**1,44**) und Windgeschwindigkeit
(**1,90**) sind unproblematisch.

### B. Heteroskedastizität (Breusch-Pagan-Test)

$$\epsilon_i^2 = \gamma_0 + \gamma_1 x_{i1} + \dots + v_i$$

**Ergebnis:** $\text{BP} = 43{,}85$ ($df=7$, $p < 0{,}001$) — signifikante
Heteroskedastizität liegt vor.

### C. Räumliche Autokorrelation (Moran's I)

$$I = \frac{N}{S_0} \frac{\sum_i \sum_j w_{ij}(e_i)(e_j)}{\sum_i e_i^2}$$

Einige Kreise (Küstenlage, Inselproblem in der BKG-Topologie) haben keine
direkten Landnachbarn — gelöst über `zero.policy = TRUE` in `poly2nb()` /
`lm.morantest()` (`spdep`-Paket).

**Ergebnis:** $\text{Moran's I} = 0{,}260$ ($z = 8{,}13$, $p < 0{,}001$) —
hochsignifikante positive räumliche Autokorrelation. Die Residuen clustern
räumlich stark (hoher Windkraftausbau im norddeutschen Tiefland).

---

## 4. Residuen & Klassifikation der Performer

$e_i = y_i - \hat{y}_i$, standardisiert (Z-Score) und klassifiziert:

* **Outperformer:** Residuum $\ge 1{,}5$ (17 Landkreise).
* **Underperformer:** Residuum $\le -1{,}5$ (6 Landkreise).
* **Normal:** dazwischen (377 Landkreise).

---

## 5. Robustheitschecks

Bei der Konsultation kam die Frage auf, ob man Kreise ohne (oder mit sehr
wenig) Windkraft aus der Analyse herausfiltern oder die Betrachtung auf eine
windreiche Region beschränken sollte, um "vergleichbarere" Kreise zu haben.
Wir haben vier konkrete Alternativen an den Daten geprüft:

| # | Alternative | Ergebnis | Konsequenz |
|---|---|---|---|
| 1 | Nur Norddeutschland als **Hauptmodell**-Stichprobe | Der bivariate Zusammenhang Steuerkraft↔Winddichte verschwindet komplett (R² 0,062 → 0,002) | **Verworfen als Hauptmodell** — der Nord/Süd-Kontrast trägt den Kernbefund selbst; ihn wegzufiltern würde die Frage verzerren, nicht beantworten |
| 2 | Interaktionseffekt Steuerkraft × Windgeschwindigkeit (wirkt Finanzkraft unterschiedlich stark je nach Windzone?) | Im Vollmodell nicht signifikant ($p=0{,}357$; ANOVA-Modellvergleich $F=0{,}85$) | Kein Hinweis auf versteckte Effektheterogenität — additives Modell ist gerechtfertigt |
| 3 | Windkraft **pro Einwohner** statt pro km² als Zielvariable | Verschlechtert die Rechtsschiefe (Median sinkt von 4 % auf 1,5 % des Maximums); Korrelation mit Steuerkraft ändert sich kaum (-0,25 → -0,26) | Beibehalten: kW/km² ist die physikalisch sinnvollere Normierung (Windkraft braucht Fläche, keine Einwohner) |
| 4 | Dasselbe Vollmodell **zusätzlich** auf zwei eingeschränkten Teilstichproben geschätzt (nicht als Ersatz, sondern als Robustheitscheck) | Steuerkraft bleibt in beiden nicht signifikant (siehe Tabelle unten) | Bestätigt: der Nullbefund ist robust gegenüber der Stichprobenwahl |

**Robustheitscheck-Tabelle (Steuerkraft-Effekt):**

| Stichprobe | N | Koeffizient | p-Wert |
|---|---|---|---|
| Hauptmodell (alle Kreise) | 400 | 0,010 | 0,732 |
| Nur überdurchschnittliche Windgeschwindigkeit | 201 | 0,027 | 0,692 |
| Nur Norddeutschland | 71 | 0,067 | 0,708 |

Fazit: unabhängig davon, wie man die Stichprobe eingrenzt, bleibt der
Steuerkraft-Effekt klein und statistisch nicht von 0 zu unterscheiden. Das
Hauptmodell nutzt weiterhin alle 400 Kreise — Filtern verbessert hier nicht
die Aussagekraft, es verkleinert nur die Stichprobe.
