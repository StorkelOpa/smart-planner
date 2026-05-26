# 1. Datenpipeline & Datenaufbereitung (Data Pipeline)

Dieses Dokument beschreibt die Schritte zur Akquisition, Vorbereitung und Zusammenführung der verschiedenen Datenquellen des Smart Planner Projekts. Diese Details dienen als Grundlage für das methodische Kapitel Ihres Berichts.

---

## 1. Datenquellen und Struktur

### A. Windenergiedaten (Marktstammdatenregister - MaStR)
Die Rohdaten stammen aus dem Gesamtdatenexport der Bundesnetzagentur (BNetzA) im XML-Format. 
* **Filterung der Rohdaten (Python):**
  * `EinheitBetriebsstatus == 35`: Nur Anlagen, die sich "In Betrieb" befinden.
  * `Lage == 888`: Nur Windkraftanlagen "an Land" (Onshore-Windkraft).
* **Aggregationslogik:** 
  * Der 8-stellige Gemeindeschlüssel der Anlage wird auf die ersten 5 Ziffern gekürzt. Diese stellen den **Amtlichen Gemeindeschlüssel (AGS)** des Landkreises bzw. der kreisfreien Stadt dar (z. B. `03405` für den Landkreis Rotenburg (Wümme)).
  * Für jeden AGS wird die Anzahl der Anlagen (`Turbine_Count`) sowie die Summe der Nettonennleistung in Kilowatt (`Total_Nettoleistung_kW`) berechnet.

### B. Geodaten (Bundesamt für Kartographie und Geodäsie - BKG)
Verwendet wird das Produkt **Verwaltungsgebiete 1:250 000 (VG250-EW)** mit Einwohnerzahlen und Katasterflächen (Stand: 31.12.2023).
* **Geometrische Bereinigung:**
  * Das Shapefile enthält Polygone für Festlandgebiete und Wasserflächen. Über das Attribut `GF` (Geofaktor) filtern wir ausschließlich `GF == 4` (Kreisgebiete an Land). Dies entfernt 33 reine Wasserpolygone (z. B. Nordsee, Ostsee, Bodensee), die keine Einwohner oder Verwaltungseinheiten besitzen, und belässt exakt die **400 Landkreise und kreisfreien Städte** Deutschlands.
  * Geometrien werden mit `sf::st_make_valid()` repariert, um topologische Fehler (z. B. Selbstüberschneidungen) zu korrigieren.

### C. Sozioökonomische und strukturelle Indikatoren (BBSR INKAR)
Über die JSON-API des Bundesinstituts für Bau-, Stadt- und Raumforschung (BBSR) werden mittels des R-Pakets `bonn` folgende Indikatoren für die geografische Ebene `KRE` (Kreise) und das Berichtsjahr `2023` abgerufen:
1. **Steuerkraft (Kennziffer 294):** Steuereinnahmekraft je Einwohner in €/Ew. (Gemeindesteuern + Gemeindeanteil an Gemeinschaftssteuern). Dies ist unsere **primäre unabhängige Variable ($x_1$)**.
2. **Einwohnerdichte (Kennziffer 320):** Einwohner je km² Landfläche (demografische Kontrollvariable $x_2$).
3. **Waldfläche (Kennziffer 264):** Flächenanteil des Waldes an der Gesamtfläche in % (geografische Kontrollvariable $x_3$).
4. **Landwirtschaftsfläche (Kennziffer 261):** Flächenanteil der landwirtschaftlichen Nutzung in % (geografische Kontrollvariable $x_4$).
5. **Wirtschaftsstruktur (Kennziffern 103, 104, 105):** Anteile der sozialversicherungspflichtig Beschäftigten am Arbeitsort in den Wirtschaftssektoren (Primär/Landwirtschaft, Sekundär/Industrie, Tertiär/Dienstleistungen) in % (Wirtschaftliche Kontrollvariablen $x_5, x_6$).

---

## 2. Datenbereinigung und Imputation (Code-Erklärung)

Ein wichtiges praktisches Problem bei amtlichen Statistiken ist die **Geheimhaltung (Confidentiality)**. In sehr kleinen kreisfreien Städten werden die Beschäftigtenzahlen im primären (landwirtschaftlichen) Sektor aus Datenschutzgründen geschwärzt (als `NA` ausgegeben), wenn Rückschlüsse auf einzelne Betriebe möglich wären.

Im Skript [04_merge_data.R](file:///home/carl/Code_Projekte/Smart%20Planner/scripts/04_merge_data.R) lösen wir dies über logische Summenbeschränkungen (da die Sektoranteile in der Summe 100 % ergeben müssen):

```R
# Bereinigung und logische Imputation der Beschäftigtenanteile
inkar_clean <- inkar_combined %>%
  mutate(
    # 1. Wenn der primäre Sektor fehlt, setzen wir ihn auf 0 (da in Städten vernachlässigbar)
    Beschaeftigte_Primar = coalesce(Beschaeftigte_Primar, 0),
    
    # 2. Wenn der sekundäre Sektor fehlt, berechnen wir ihn als Residuum zu 100 %
    Beschaeftigte_Sekundar = ifelse(
      is.na(Beschaeftigte_Sekundar), 
      100 - Beschaeftigte_Tertiar - Beschaeftigte_Primar, 
      Beschaeftigte_Sekundar
    ),
    
    # 3. Absicherung gegen verbleibende NAs
    Beschaeftigte_Sekundar = coalesce(Beschaeftigte_Sekundar, 0)
  )
```

---

## 3. Berechnung abgeleiteter Variablen

### A. Zielvariable (Windkraft-Kapazitätsdichte)
Um den Ausbaustand der Windkraft unabhängig von der reinen Größe eines Landkreises vergleichen zu können, normieren wir die installierte Nettoleistung auf die Landfläche des Kreises ($KFL$ aus dem BKG-Datensatz in $\text{km}^2$):

$$\text{Wind\_Density\_kW\_km2} = \frac{\text{Total\_Nettoleistung\_kW}}{\text{KFL\_km2}}$$

### B. Klassifikation der Wirtschaftsstruktur
Um den Einfluss des wirtschaftlichen Charakters kategorial analysieren zu können, gruppieren wir die Kreise basierend auf ihren Beschäftigtenanteilen:
* **Industriell:** Beschäftigtenanteil im sekundären Sektor $\ge 35\%$.
* **Ländlich/Agrarisch:** Beschäftigtenanteil im primären Sektor $\ge 3\%$ (sehr hoch im deutschen Vergleich).
* **Dienstleistungsorientiert:** Tertiärer Sektor $\ge 65\%$.
* **Ausgeglichen:** Alle anderen Kreise.

```R
merged_data <- merged_data %>%
  mutate(
    Economic_Structure = case_when(
      Beschaeftigte_Sekundar >= 35 ~ "Industriell",
      Beschaeftigte_Primar >= 3.0 ~ "Ländlich/Agrarisch",
      Beschaeftigte_Tertiar >= 65 ~ "Dienstleistungsorientiert",
      TRUE ~ "Ausgeglichen"
    )
  )
```

Die fertigen Tabellendaten werden schließlich als `data/smart_planner_final_data.csv` und die Geometriedaten für das Dashboard als Geopackage `data/smart_planner_final_data.gpkg` exportiert.
