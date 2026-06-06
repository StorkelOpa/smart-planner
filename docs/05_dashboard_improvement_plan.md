# Dashboard-Verbesserungsplan: Kreis-Profil im klimadashboard-Stil

Dieses Dokument beschreibt den Plan, das Smart-Planner-Dashboard in Richtung des
visuellen Vorbilds (heller, erzählender Ein-Screen-„Kreis-Steckbrief" wie auf
klimadashboard.de) weiterzuentwickeln. Schwerpunkt dieser Ausbaustufe:
**zuerst die nötigen Daten erschließen**, danach die Visualisierungen bauen.

Stand: 2026-06-06 · Verantwortlich: Gruppe (Vogt, Thiago, Marlon)

---

## 1. Ausgangslage

Das aktuelle Dashboard ([app.R](../app.R)) ist analytisch stark, aber als
Sammlung von vier weitgehend getrennten Tabs organisiert:

1. **Daten-Visualisierung** – 7 statische ggplot2-Analysegrafiken
2. **Modell-Analyse** – R², Moran's I, Breusch-Pagan, Koeffizienten, Forest-Plot
3. **Kreis-Profil & Peer-Suche** – k-NN-Ähnlichkeitssuche
4. **Deutschlandkarte** – Leaflet-Choropleth

Das visuelle Vorbild macht etwas anderes: Es erzählt **eine Geschichte zu *einem*
Kreis auf einem Screen** – Kennzahlen mit Einordnung, Zeitverlauf, Flächennutzung,
Soll-/Ist-Performance, Bundesvergleich und Peer-Liste. Das ist im Kern eine stark
aufgewertete Version von Tab 3.

Die größten Verständnis-Hebel liegen daher nicht in *neuen Analysen*, sondern in
**Einordnung, Lesbarkeit und Dramaturgie** der vorhandenen Zahlen – plus drei
neuen Datenbausteinen (Zeitreihe, vollständige Flächennutzung, Windgeschwindigkeit).

---

## 2. Designentscheidungen (festgelegt)

| Entscheidung | Wahl |
|---|---|
| Schwerpunkt dieser Stufe | **Erst neue Daten erschließen**, dann Visualisierung |
| Visueller Stil | **Hell**, wie das Mockup (klimadashboard-Look) |
| Windgeschwindigkeit | **Global Wind Atlas** einbinden (liefert Karte *und* Modell-Kontrollvariable) |
| Sprache/Stack | R (tidyverse, sf, terra), gut kommentiert und nachvollziehbar |

---

## 3. Datenerschließung (diese Ausbaustufe)

Drei neue Datenbausteine, die jeweils Mockup-Komponenten freischalten. Die
Datenlage ist sehr unterschiedlich – das bestimmt Aufwand und Reihenfolge.

### WS1 — Zeitreihe des Windausbaus  ·  *schnellster Gewinn*

- **Quelle:** bereits vorhanden in [data/wind_turbines.csv](../data/wind_turbines.csv)
  (Spalte `Inbetriebnahmedatum` je Anlage).
- **Skript:** [scripts/07_build_wind_timeseries.R](../scripts/07_build_wind_timeseries.R)
- **Logik:** Kreis-AGS = erste 5 Stellen von `Gemeindeschluessel`; Jahr aus
  `Inbetriebnahmedatum`; je Kreis × Jahr die **kumulierte** `Nettonennleistung`
  (und ÷ Kreisfläche = Dichte-Zeitreihe). Zusätzlich eine bundesweite Aggregation
  (`AGS = "DE"`) als Vergleichslinie „Kreis vs. Deutschland".
- **Output:** `data/wind_timeseries_by_county.csv`
  (Spalten: `AGS`, `Jahr`, `Anlagen_kumuliert`, `Nettoleistung_kW_kumuliert`,
  `Wind_Density_kW_km2_kumuliert`).
- **Caveat (zu dokumentieren):** Die MaStR-Datei enthält v. a. *aktuell
  betriebene* Anlagen (`EinheitBetriebsstatus == 35`). Stillgelegte Altanlagen
  fehlen, frühe Jahre sind daher leicht untererfasst. Für einen Ausbau-*Trend*
  unkritisch, aber ehrlich benennen.
- **Schaltet frei:** Liniendiagramm „Windleistung im Zeitverlauf" (Kreis + DE).

### WS2 — Flächennutzung vervollständigen  ·  *kleiner INKAR-Pull*

- **Quelle:** INKAR (bereits angebunden). Verfügbar laut Katalog:
  *Siedlungs- u. Verkehrsfläche* (Gruppe 255), *Wasserfläche* (265),
  zusätzlich zu den bereits genutzten Landwirtschafts- (261) und Waldflächen (264).
- **Skript:** [scripts/04_merge_data.R](../scripts/04_merge_data.R) erweitern.
- **Logik:** Siedlungs-/Verkehrs- und Wasserfläche dazu-fetchen, dann
  `Sonstige_Prozent = 100 − (Landwirtschaft + Wald + Siedlung + Wasser)`
  (auf ≥ 0 begrenzt).
- **Output:** zusätzliche Spalten `Siedlung_Verkehr_Prozent`, `Wasser_Prozent`,
  `Sonstige_Prozent` im finalen Datensatz.
- **Schaltet frei:** Flächennutzungs-Donut.

### WS3 — Windgeschwindigkeit (Windpotenzial)  ·  *eigenständiges Stück, doppelter Nutzen*

- **Quelle:** **Global Wind Atlas v4** (globalwindatlas.info). Länder-GeoTIFF der
  mittleren Windgeschwindigkeit; hier **150 m** Höhe als moderne Nabenhöhe.
  Download-Endpunkt: `https://globalwindatlas.info/api/gis/country/DEU/wind-speed/150`
  (leitet auf das CDN-TIFF um).
- **Skript:** [scripts/02b_get_wind_speed.R](../scripts/02b_get_wind_speed.R)
- **Logik:** Raster mit `terra` laden, je Kreis-Polygon die **mittlere**
  Windgeschwindigkeit per zonaler Statistik (`terra::extract(fun = mean)`)
  berechnen. (Optional genauer mit `exactextractr`, falls installiert.)
- **Output:** `data/wind_speed_by_county.csv` (`AGS`, `Windgeschwindigkeit_ms`),
  wird in [04_merge_data.R](../scripts/04_merge_data.R) angejoint.
- **Doppelter Nutzen:** Windpotenzial steht im
  [Projektkontext](../PROJEKTKONTEXT_SmartPlanner_v2.md) als geplante
  Kontrollvariable, fehlt aber bisher im Regressionsmodell. WS3 liefert damit
  **nicht nur die Mockup-Karte, sondern vervollständigt auch das Modell**
  (Aufnahme in [05_model_regression.R](../scripts/05_model_regression.R)).

### Reihenfolge

**WS1 → WS2 → WS3.** WS1/WS2 nutzen vorhandene Daten und liefern sofort zwei
Mockup-Bausteine; WS3 ist das größere Stück mit zusätzlichem Modell-Mehrwert.

---

## 4. Visualisierungs-Roadmap (Folge-Ausbaustufe)

Nach der Datenerschließung wird **Tab 3 zur erzählenden „Kreis-Profil"-Hauptseite**
im hellen Mockup-Stil ausgebaut. Geplante Komponenten:

| # | Komponente | Datenbasis | Status |
|---|---|---|---|
| 1 | KPI-Karten mit Einordnungs-Slider (Position im Bundesvergleich, Ø-Marker) | vorhanden | Perzentil-Logik in [app.R](../app.R#L718) ausbauen |
| 2 | Zeitverlauf Windleistung (Kreis + DE) | **WS1** | neu |
| 3 | Flächennutzungs-Donut | **WS2** | neu |
| 4 | Windgeschwindigkeits-Karte (m/s) | **WS3** | neu |
| 5 | Soll-vs-Ist-Performance (Bullet/Tacho statt nacktem Residuum) | vorhanden | aus `Residuals`/`Predicted` |
| 6 | Bundesweites Rang-Band („Rang X von 400") | vorhanden | ableitbar |
| 7 | Peer-Tabelle mit Ähnlichkeits-Balken | vorhanden (k-NN) | visuell aufwerten |

**Weiter gedacht (über das Mockup hinaus):**

- **Bivariate Choroplethen-Karte** (Steuerkraft × Windausbau) – macht den
  Kernzusammenhang räumlich sichtbar.
- **Karte ↔ Profil verlinken** – Klick auf Kreis öffnet sein Profil.
- **Heller Theme-Umbau** des gesamten Dashboards (bslib-Theme von `darkly` auf
  hellen, klimadashboard-nahen Look).

---

## 5. Pipeline-Einordnung

Die neuen Schritte fügen sich so in [run_data_pipeline.sh](../scripts/run_data_pipeline.sh) ein:

```
... BKG-Geodaten (02) ...
→ 02b_get_wind_speed.R        (WS3: Wind-Atlas → wind_speed_by_county.csv)
... INKAR-Katalog (01) ...
→ 04_merge_data.R             (WS2: + Flächennutzung, + Windgeschwindigkeit-Join)
→ 05_model_regression.R       (WS3: Windgeschwindigkeit als Kontrollvariable)
→ 07_build_wind_timeseries.R  (WS1: Zeitreihe nach dem Merge, nutzt Kreisflächen)
... Plots (06) ...
```

---

## 6. Offene Punkte / spätere Entscheidungen

- Heller Theme-Umbau: betrifft das gesamte `bslib`-Theme und alle ggplot-Themes
  (aktuell `theme_shiny_dark()`) – eigener Arbeitsblock.
- Donut „Sonstige": INKAR-Restkategorie ggf. inhaltlich erläutern (Bergbau-,
  Tagebau-, Truppenübungsflächen etc.).
- Windgeschwindigkeit als Regressor: VIF erneut prüfen (Korrelation mit Wald-/
  Bevölkerungsdichte möglich).
</content>
</invoke>
