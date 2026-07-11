# 1. Datenaufbereitung (Data Pipeline)

Dieses Dokument beschreibt, wie aus den vier Rohdatenquellen (siehe
[00_datenquellen.md](00_datenquellen.md)) der finale Analysedatensatz
entsteht. Es dient als Grundlage für das methodische Kapitel des Berichts.

---

## 1. Windenergiedaten (MaStR) — `scripts/02_get_wind_turbines.py` + `.R`

Die Rohdaten stammen aus dem Gesamtdatenexport der Bundesnetzagentur (BNetzA)
im XML-Format (`data_raw/EinheitenWind.xml.gz`).

* **Parsen (Python, `02_get_wind_turbines.py`):** liest die komprimierte XML
  speicherschonend per `iterparse` und schreibt je Windkraftanlage eine Zeile
  in `data/wind_turbines_roh.csv`.
* **Filtern & Aggregieren (R, `02_get_wind_turbines.R`):**
  * `EinheitBetriebsstatus == 35`: nur Anlagen "In Betrieb".
  * `Lage == 888`: nur Windkraft "an Land" (Onshore).
  * Der 8-stellige `Gemeindeschluessel` wird auf die ersten 5 Ziffern
    gekürzt — das ist der **Amtliche Gemeindeschlüssel (AGS)** des Kreises
    (z. B. `03405` für Rotenburg (Wümme)).
  * Je AGS werden Anzahl (`Turbine_Count`) und installierte Nettoleistung
    (`Total_Nettoleistung_kW`) summiert.

## 2. Geodaten (BKG) — `scripts/01_get_geodata.R`

Produkt **Verwaltungsgebiete 1:250 000 (VG250-EW)**, Stand 31.12.2023, aus
`data_raw/vg250_2023.zip`.

* Über das Attribut `GF` (Geofaktor) wird auf `GF == 4` gefiltert
  (Kreisgebiete an Land) — entfernt reine Wasserflächen-Polygone (Nordsee,
  Ostsee, Bodensee), übrig bleiben exakt die **400 Landkreise und
  kreisfreien Städte**.
* Geometrien werden mit `sf::st_make_valid()` bereinigt.
* Output: `data/kreise_geodaten.gpkg` (AGS, GEN, BEZ, EWZ, KFL_km2).

## 3. Windpotenzial (Global Wind Atlas) — `scripts/03_get_wind_speed.R`

Mittlere Windgeschwindigkeit (150 m Nabenhöhe) aus `data_raw/wind_speed_150m.tif`.
Per **zonaler Statistik** (`terra::extract(fun = mean)`) auf die
Kreis-Polygone aus Schritt 1 heruntergebrochen → `Windgeschwindigkeit_ms`.
Output: `data/windgeschwindigkeit_je_kreis.csv`.

## 4. Sozioökonomische Indikatoren (INKAR) — `scripts/04_get_inkar.R`

Direkter Abruf der neun bekannten Kennziffern (siehe
[00_datenquellen.md](00_datenquellen.md)) über die JSON-API (`bonn`-Paket),
Berichtsjahr 2023, Ebene `KRE`. Output: `data/inkar_kennzahlen_je_kreis.csv`.

**Geheimhaltung:** In sehr kleinen (meist städtischen) Kreisen werden die
Beschäftigtenzahlen im primären Sektor aus Datenschutzgründen geschwärzt
(`NA`). Da die drei Sektoranteile in Summe 100 % ergeben müssen, wird das
über logische Summenbeschränkungen imputiert:

```R
inkar_clean <- inkar_raw %>%
  mutate(
    Beschaeftigte_Primar = coalesce(Beschaeftigte_Primar, 0),
    Beschaeftigte_Sekundar = ifelse(
      is.na(Beschaeftigte_Sekundar),
      100 - Beschaeftigte_Tertiar - Beschaeftigte_Primar,
      Beschaeftigte_Sekundar
    ),
    Beschaeftigte_Sekundar = coalesce(Beschaeftigte_Sekundar, 0)
  )
```

---

## 5. Zusammenführen — `scripts/05_merge_data.R`

Alle vier Zwischendateien werden über den AGS zusammengeführt. Daraus
abgeleitet:

### Zielvariable (Windkraft-Kapazitätsdichte)

$$\text{Wind\_Density\_kW\_km2} = \frac{\text{Total\_Nettoleistung\_kW}}{\text{KFL\_km2}}$$

Kreise ohne Eintrag in der Windkraft-Tabelle haben schlicht keine Anlagen
(0), nicht "fehlend" — werden entsprechend mit 0 aufgefüllt statt gelöscht.

### Bundesland

Aus den ersten zwei Ziffern des AGS abgeleitet (z. B. `01` → Schleswig-
Holstein). Wird für die Robustheitschecks in
[02_statistische_modellierung.md](02_statistische_modellierung.md) gebraucht
(Teilstichprobe "nur Norddeutschland").

### Klassifikation der Wirtschaftsstruktur

* **Industriell:** Beschäftigtenanteil im sekundären Sektor ≥ 35 %.
* **Ländlich/Agrarisch:** Beschäftigtenanteil im primären Sektor ≥ 3 %.
* **Dienstleistungsorientiert:** Tertiärer Sektor ≥ 65 %.
* **Ausgeglichen:** alle anderen Kreise.

Output: `data/smart_planner_daten.gpkg` (mit Geometrie) und `.csv` (ohne).

---

## Pipeline auf einen Blick

```
scripts/01_get_geodata.R          -> data/kreise_geodaten.gpkg
scripts/02_get_wind_turbines.py   -> data/wind_turbines_roh.csv
scripts/02_get_wind_turbines.R    -> data/wind_turbinen_je_kreis.csv
scripts/03_get_wind_speed.R       -> data/windgeschwindigkeit_je_kreis.csv
scripts/04_get_inkar.R            -> data/inkar_kennzahlen_je_kreis.csv
scripts/05_merge_data.R           -> data/smart_planner_daten.gpkg / .csv
scripts/06_model_regression.R     -> data/smart_planner_daten_mit_residuen.gpkg / .csv
                                     data/modell_ergebnisse.RData
scripts/07_generate_plots.R       -> plots/*.png
```

Alles zusammen ausführbar über `bash run_pipeline.sh`.
