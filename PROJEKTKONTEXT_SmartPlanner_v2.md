# PROJEKTKONTEXT: Smart Planner – BTU Cottbus-Senftenberg

## METADATEN
- Kurs: Smart Planner – Data Science und KI für die Stadt- und Regionalplanung
- Dozent: Dr. Andreas Kuebart (FG Raumbezogene Transformationsforschung)
- Modul: 13881/13882 (Stadtplanung I/II) | Projekt 4 | 12 LP
- Gruppe: Carl Vogt, Thiago [Nachname], Marlon [Nachname]
- Sprache/Stack: R (DataCamp-Kurs), ggplot2, sf, tidyverse
- Endprodukt: Interaktives Dashboard (Vorbild: klimadashboard.de)

---

## FORSCHUNGSFRAGE
"Inwieweit lassen sich Zusammenhänge zwischen kommunaler Finanzkraft
und dem Ausbau von Onshore-Windkraft auf Kreisebene in Deutschland
identifizieren?"

---

## RÄUMLICHE EINHEIT & ZEITRAUM
- Einheit: Landkreise und kreisfreie Städte (~400), Gebietsstand 31.12.2023
- Zeitraum: Querschnitt Stand 2024

---

## VARIABLEN

### Abhängige Variable (y)
| Variable                          | Einheit  | Quelle                        |
|-----------------------------------|----------|-------------------------------|
| Installierte Windleistung onshore | kW/km²   | Marktstammdatenregister (BNetzA) |

### Hauptunabhängige Variable (x)
| Variable                        | Einheit | Quelle      |
|---------------------------------|---------|-------------|
| Steuereinnahmekraft je Einwohner| €/Ew.   | INKAR (BBSR)|

Begründung: Steuereinnahmekraft misst *aktuelle* fiskalische Kapazität.
Schulden je Einwohner als Alternative/Ergänzung möglich, aber schwächer
(Schulden können aus historischen Investitionen stammen, auch aus EE selbst
→ Zirkelschlussproblem).

### Kontrollvariablen
| Variable                                   | Einheit  | Quelle                  |
|--------------------------------------------|----------|-------------------------|
| Windpotenzial (mittlere Windgeschw.)        | m/s      | DWD Climate Data Center |
| Flächenanteil Offenland/Landwirtschaft      | %        | INKAR                   |
| Flächenanteil Wald                          | %        | INKAR                   |
| Bevölkerungsdichte                          | Ew./km²  | INKAR                   |
| Dominante Wirtschaftsstruktur des Kreises   | kategorial | INKAR / Destatis      |

**Hinweis zur Wirtschaftsstruktur (Feedback Dr. Kuebart):**
Die dominante Industrie eines Kreises (z. B. Tourismus, Maschinenbau,
Landwirtschaft, Bergbau/Energie, Dienstleistungen) kann den Windausbau
erheblich beeinflussen – sowohl durch politische Akzeptanz als auch durch
konkurrierende Flächennutzung und lokale Interessenkonstellationen.
Operationalisierung: Anteil der Beschäftigten je Wirtschaftsabschnitt (WZ 2008)
→ Bildung von Kategorien (z. B. „Tourismuskreis", „Industriekreis") oder
kontinuierliche Anteile als separate Regressoren.

---

## METHODIK (vom Dozenten bestätigt, Stand nach Feedback)

### Schritt 1: Lineare Regression
Modell:
  Windleistung_kW_km2 ~ Steuereinnahmekraft_je_Ew
                      + Windpotenzial_m_s
                      + Flaechenanteil_Offenland
                      + Flaechenanteil_Wald
                      + Bevoelkerungsdichte
                      + Wirtschaftsstruktur_Kategorie  ← neu

Ziel:
- Quantifizierung des Zusammenhangs Finanzkraft → Windausbau
- Residualanalyse: Kreise mit hohem positivem Residuum
  = "Outperformer trotz Finanzschwäche" → inhaltlich besonders interessant
- Prüfungen: VIF (Multikollinearität), Breusch-Pagan (Heteroskedastizität)
- Z-Standardisierung für vergleichbare Koeffizienten

### Schritt 2 (optional/Bonus): k-NN
- Klassifikation für Dashboard-Ähnlichkeitssuche
- "Welche Kreise ähneln Kreis X am meisten?"

> **Hinweis:** k-Means Clustering wurde auf Empfehlung von Dr. Kuebart
> zunächst aus dem Projektscope herausgenommen (Aufwand vs. Mehrwert im
> aktuellen Projektstadium nicht verhältnismäßig). Kann bei verbleibendem
> Zeitbudget wieder aufgegriffen werden.

---

## DATENQUELLEN

### Marktstammdatenregister (BNetzA)
- URL: https://www.marktstammdatenregister.de
- Direktdownload: https://marktstammdatenregister.dev
- Lizenz: CC-BY-4.0
- Inhalt: Alle netzgekoppelten Windenergieanlagen mit Koordinaten
- Aggregation auf Kreisebene via AGS-Schlüssel
- Empfehlung: GOAL100-bereinigten Datensatz nutzen (sauberere Koordinaten)
- Zeitreihe ab ca. 2000, vollständig ab 2019

### INKAR (BBSR)
- URL: https://www.inkar.de
- ~600 Indikatoren auf Kreisebene, Stand 31.12.2023
- R-Paket: **bonn** (empfohlen von Dr. Kuebart)
  Installation: `install.packages("bonn")`
  Beispiel:
  ```r
  library(bonn)
  # Verfügbare Indikatoren anzeigen
  get_INKAR_meta()
  # Daten abrufen (Kreisebene)
  get_INKAR_data(Kennziffer = "...", Raumbezug = "KRS")
  ```
- Enthält: Steuereinnahmekraft, Schulden, Bevölkerungsdichte, Flächennutzung,
  Beschäftigtenstruktur nach Wirtschaftsabschnitten
- Vorteil gegenüber manuellem Download: direkte API-Anbindung in R,
  reproduzierbarer Workflow, kein händisches CSV-Handling

### Destatis (Statistisches Bundesamt)
- Beschäftigte nach Wirtschaftszweigen auf Kreisebene (WZ 2008)
- Alternativ: Bundesagentur für Arbeit, Regionalstatistik GENESIS
- Für Operationalisierung der dominanten Wirtschaftsstruktur

### DWD Climate Data Center
- URL: https://opendata.dwd.de
- Windgeschwindigkeit auf Stationsebene → Interpolation auf Kreise nötig
- Alternativ: falls INKAR Windpotenzial-Indikator verfügbar, diesen nutzen

### Geodaten (Kreispolygone)
- Quelle: GeoBasis-DE / BKG (basemap.de)
- Format: Shapefile / GeoPackage
- Gebietsstand: einheitlich 31.12.2023 verwenden

---

## HYPOTHESEN (zu prüfen)

1. Positive Korrelation Steuereinnahmekraft → Windausbau ERWARTET
   aber NICHT SELBSTVERSTÄNDLICH, weil:
   - Windanlagen = keine kommunale Investition nötig
   - Pacht + Gewerbesteuer = Einnahmen für finanzschwache Kreise
   - NIMBY-Effekte in wohlhabenden Kreisen möglich

2. Wirtschaftsstruktur als Moderator:
   - Tourismuskreise: niedrigerer Windausbau zu erwarten (Landschaftsschutz,
     Akzeptanzprobleme, Abhängigkeit vom Landschaftsbild)
   - Industriekreise (Maschinenbau, Energie): potenziell höhere Akzeptanz
     oder bereits belastete Landschaft → höherer Ausbau möglich
   - Landwirtschaftlich geprägte Kreise: Pachteinnahmen als Anreiz
     → eher positiver Effekt auf Windausbau

---

## BEKANNTE METHODISCHE HERAUSFORDERUNGEN

- Landesplanungsrecht variiert stark je Bundesland (Abstandsregeln, 2%-Ziel)
  → ggf. Bundesland als Dummy-Variable in Regression aufnehmen
- Räumliche Autokorrelation: Nachbarkreise strukturell ähnlich → Moran's I
  dokumentieren
- Multikollinearität: Steuereinnahmekraft & Schulden stark korreliert
  → nur eine als Hauptvariable verwenden
- Wirtschaftsstruktur & Bevölkerungsdichte ggf. korreliert → VIF prüfen
- Stadtkreise als Sonderfall: kaum Fläche → ggf. separat behandeln
- MaStR: Altanlagen teils mit ungenauen Koordinaten → GOAL100 nutzen
- Gebietsstandsprobleme: verschiedene Quellen, verschiedene Stichtage

---

## ENDPRODUKT: DASHBOARD

Vorbild: klimadashboard.de
Geplante Elemente:
- Interaktive Karte mit Kreis-Profilen und Residualdarstellung
- Ausreißer-Ranking: "Wer baut trotz Finanzschwäche besonders viel?"
- Region-vs.-Region-Vergleich
- Wirtschaftsstruktur-Filter
- Optional: Zeitreihen-Slider Windzubau nach Jahr
- Optional: k-NN-Ähnlichkeitssuche ("Welche Kreise ähneln Kreis X?")

Tech-Stack (präferiert): R Shiny + Leaflet

---

## PRÜFUNGSLEISTUNG (MCA)
1. Zwischenpräsentation
2. Endpräsentation
3. Projektbericht (Datenanalyse, Code, Interpretation)

---

*Zuletzt aktualisiert nach Feedback-Gespräch mit Dr. Kuebart:*
*k-Means vorerst gestrichen; Wirtschaftsstruktur als Kontrollvariable ergänzt;*
*R-Paket für INKAR auf `bonn` (Dozenten-Empfehlung) aktualisiert.*
