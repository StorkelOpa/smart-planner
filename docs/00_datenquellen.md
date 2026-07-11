# 0. Datenquellen im Überblick

Dieses Dokument beantwortet eine Frage: **welche Daten brauchen wir, wofür,
und woher genau kommen sie?** Keine Recherche-Protokolle, keine
Schlagwortsuchen über Kataloge — nur die fertigen Antworten, damit die
Skripte in `scripts/` direkt loslegen können.

## Forschungsfrage

*"Inwieweit lassen sich Zusammenhänge zwischen kommunaler Finanzkraft und
dem Ausbau von Onshore-Windkraft auf Kreisebene in Deutschland
identifizieren?"*

Räumliche Einheit: alle 400 Landkreise und kreisfreien Städte Deutschlands,
Gebietsstand 31.12.2023. Zeitraum: Querschnitt (ein Zeitpunkt, nicht über
Jahre hinweg).

## Die Variablen und wo sie herkommen

| Variable | Bedeutung | Rolle im Modell | Quelle | Konkrete Kennziffer / Endpunkt |
|---|---|---|---|---|
| `Wind_Density_kW_km2` | installierte Netto-Windleistung je km² Kreisfläche | **Zielvariable (Y)** | Marktstammdatenregister (MaStR, BNetzA) | Gesamtdatenexport, Filter `EinheitBetriebsstatus==35` (in Betrieb) + `Lage==888` (onshore) |
| `Steuerkraft` | Steuereinnahmekraft je Einwohner (€/Ew.) | **Hauptvariable (X)** | INKAR (BBSR) | Kennziffer **294** |
| `Windgeschwindigkeit_ms` | mittlere Windgeschwindigkeit, 150 m Nabenhöhe | Kontrollvariable (Windpotenzial) | Global Wind Atlas v4 | API `wind-speed/150`, zonales Mittel je Kreis |
| `Einwohnerdichte` | Einwohner je km² | Kontrollvariable | INKAR | Kennziffer **320** |
| `Waldflaeche_Prozent` | Waldflächenanteil (%) | Kontrollvariable | INKAR | Kennziffer **264** |
| `Landwirtschaft_Prozent` | Landwirtschaftsflächenanteil (%) | Kontrollvariable | INKAR | Kennziffer **261** |
| `Beschaeftigte_Primar/Sekundar/Tertiar` | Beschäftigtenanteile nach Wirtschaftssektor (%) | Kontrollvariable (Wirtschaftsstruktur) | INKAR | Kennziffern **103, 104, 105** |
| `Siedlung_Verkehr_Prozent`, `Wasser_Prozent` | Flächenanteile (%) | nur für die Flächennutzungs-Übersicht, kein Regressor | INKAR | Kennziffern **255, 265** |
| Kreisgrenzen, `KFL_km2` (Fläche), `EWZ` (Einwohnerzahl) | Geometrie + Attribute je Kreis | Grundgerüst (Join-Schlüssel `AGS`) | BKG VG250-EW | Shapefile-Produkt "Verwaltungsgebiete 1:250 000", Stand 31.12.2023 |

## Die vier Datenquellen im Detail

### 1. MaStR — Marktstammdatenregister (Bundesnetzagentur)
Register **aller** Stromerzeugungsanlagen in Deutschland. Wir nutzen den
Gesamtdatenexport (XML, komprimiert als `data_raw/EinheitenWind.xml.gz`),
gefiltert auf Windkraftanlagen. Der 8-stellige `Gemeindeschluessel` einer
Anlage wird auf die ersten 5 Ziffern gekürzt → das ist der **AGS**
(Amtlicher Gemeindeschlüssel) des Kreises, über den alles andere verknüpft
wird.

### 2. INKAR (BBSR) — sozioökonomische Indikatoren
~600 Indikatoren je Kreis, abrufbar über die JSON-API (R-Paket `bonn`).
Wir brauchen davon genau 9 Kennziffern (Tabelle oben) — die sind bereits
bekannt und werden direkt abgerufen, ohne den Katalog zu durchsuchen.

### 3. BKG VG250-EW — Verwaltungsgebiete
Amtliche Kreisgrenzen als Shapefile, inkl. Fläche (`KFL`) und Einwohnerzahl
(`EWZ`) als Attribute. Enthält auch reine Wasserflächen-Polygone (Nordsee,
Ostsee, Bodensee) — die werden über das Attribut `GF==4` (nur Landflächen)
ausgeschlossen, übrig bleiben exakt die 400 Kreise.

### 4. Global Wind Atlas v4 — Windpotenzial
Raster (GeoTIFF) der mittleren Windgeschwindigkeit für Deutschland, 150 m
Nabenhöhe (moderne Anlagenhöhe). Wird per zonaler Statistik (Mittelwert je
Kreis-Polygon) auf die AGS-Ebene heruntergebrochen.

## Pipeline-Reihenfolge

```
01_get_geodata.R        -> Kreisgrenzen, KFL, EWZ
02_get_wind_turbines.*  -> Windkraftanlagen je Kreis (Anzahl, installierte Leistung)
03_get_wind_speed.R     -> Windgeschwindigkeit je Kreis
04_get_inkar.R          -> die 9 INKAR-Kennziffern je Kreis
05_merge_data.R         -> alles über AGS zusammenführen, Y berechnen
06_model_regression.R   -> OLS-Regression + Diagnostik
07_generate_plots.R     -> Report-Grafiken
```

Jedes Skript 01–04 erzeugt eine Zwischendatei in `data/`; `05_merge_data.R`
liest nur diese vier Dateien plus die Kreisgrenzen und führt sie zusammen.
Kein Skript sucht in den Daten nach der "richtigen" Variable — das ist hier,
in dieser Übersicht, bereits geklärt.
