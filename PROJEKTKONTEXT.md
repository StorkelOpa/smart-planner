# PROJEKTKONTEXT: Smart Planner – BTU Cottbus-Senftenberg

## METADATEN
- Kurs: Smart Planner – Data Science und KI für die Stadt- und Regionalplanung
- Dozent: Dr. Andreas Kuebart (FG Raumbezogene Transformationsforschung)
- Modul: 13881/13882 (Stadtplanung I/II) | Projekt 4 | 12 LP
- Gruppe: Carl Vogt, Thiago, Marlon
- Sprache/Stack: R (tidyverse, sf, terra), Python nur für den MaStR-XML-Parser
- Stand: 2026-07-04, nach Konsultations-Nachbereitung — neu aufgesetzter,
  schlanker Projektordner (siehe "Warum ein neuer Ordner?" unten)

---

## FORSCHUNGSFRAGE
"Inwieweit lassen sich Zusammenhänge zwischen kommunaler Finanzkraft und
dem Ausbau von Onshore-Windkraft auf Kreisebene in Deutschland
identifizieren?"

---

## WARUM EIN NEUER ORDNER?

Der vorherige Projektordner (`Smart Planner/`) war um ein interaktives
Dashboard herum gewachsen und enthielt Skripte, die widerspiegelten, *wie*
nach den richtigen Datenquellen gesucht wurde (z. B. ein Skript, das alle
~600 INKAR-Variablen durchsuchte, um die Kennziffer für "Steuerkraft" zu
finden), statt direkt die bereits bekannte Antwort zu verwenden. Dieser
Ordner ist bewusst schlank aufgesetzt:

1. **Bericht zuerst.** Die Prüfungsleistung ist der Projektbericht
   (Datenanalyse, Code, Interpretation) — das Dashboard ist eine mögliche,
   spätere Ergänzung, kein gleichrangiges Ziel (s. Abschnitt "Endprodukt").
2. **Datenquellen sind bekannt, nicht gesucht.** Siehe
   [docs/00_datenquellen.md](docs/00_datenquellen.md) — eine Tabelle mit
   Antworten (z. B. "Steuerkraft = INKAR Kennziffer 294"), keine
   Recherche-Skripte.
3. **Eine klare Pipeline-Reihenfolge:** je Datenquelle ein Skript
   (`01`–`04`), dann Zusammenführen (`05`), Regression (`06`), Report-Plots
   (`07`). Siehe [docs/01_datenaufbereitung.md](docs/01_datenaufbereitung.md).

Die Rohdaten (`data_raw/`) sind identisch zum alten Ordner; alle
Zwischenergebnisse wurden gegengeprüft und sind bit-identisch zum alten
Modell (siehe Commit-Historie).

---

## RÄUMLICHE EINHEIT & ZEITRAUM
- Einheit: Landkreise und kreisfreie Städte (400), Gebietsstand 31.12.2023
- Zeitraum: Querschnitt Stand 2023/2024

---

## VARIABLEN

Vollständige Übersicht mit Kennziffern/Quellen:
[docs/00_datenquellen.md](docs/00_datenquellen.md).

| Rolle | Variable | Einheit |
|---|---|---|
| Y (Ziel) | Windkraft-Kapazitätsdichte | kW/km² |
| X (Haupt) | Steuereinnahmekraft je Einwohner | €/Ew. |
| Kontrolle | Windgeschwindigkeit (150 m) | m/s |
| Kontrolle | Einwohnerdichte | Ew./km² |
| Kontrolle | Waldflächenanteil, Landwirtschaftsanteil | % |
| Kontrolle | Beschäftigtenanteile nach Sektor | % |

---

## METHODIK

### Hauptmodell: multiple lineare Regression (OLS)

```
Windleistung_kW_km2 ~ Steuerkraft + Einwohnerdichte + Windgeschwindigkeit
                     + Waldflaeche + Landwirtschaft
                     + Beschaeftigte_Sekundar + Beschaeftigte_Primar
```

Auf **allen 400 Kreisen** geschätzt (Details, Diagnostik und
Standardisierung: [docs/02_statistische_modellierung.md](docs/02_statistische_modellierung.md)).

**Zentraler Befund:** Steuerkraft ist **nicht signifikant**
(standardisiert +0,016, $p=0{,}73$) — der bivariat sichtbare negative
Zusammenhang ist ein Omitted-Variable-Bias durch die Windgeschwindigkeit
(+0,464, stärkster Faktor).

### Darstellung schiefer Verteilungen (log-Skala)

Die Windkraftdichte ist rechtsschief (Median ≈ 4 % des Maximums). Die
**deskriptiven Report-Plots** (nicht das Modell!) nutzen deshalb eine
pseudo-logarithmische y-Achse, damit die Unterschiede zwischen den vielen
"nahe Null"-Kreisen überhaupt sichtbar werden. Details:
[docs/03_visualisierungen.md](docs/03_visualisierungen.md).

### Geprüfte Alternativen (Robustheitschecks)

Vier Alternativen wurden konkret an den Daten geprüft, um die Frage "sollen
wir Kreise filtern oder die Region eingrenzen?" nicht aus dem Bauch heraus,
sondern mit Zahlen zu beantworten (voller Vergleich:
[docs/02_statistische_modellierung.md](docs/02_statistische_modellierung.md),
Abschnitt 5):

1. Nur Norddeutschland als Haupt-Stichprobe → **verworfen**, zerstört den
   Kernbefund (bivariates R² 0,062 → 0,002).
2. Interaktion Steuerkraft × Windgeschwindigkeit → nicht signifikant
   ($p=0{,}357$), additives Modell bestätigt.
3. Windkraft pro Einwohner statt pro Fläche → verschlechtert die Schiefe,
   keine inhaltliche Verbesserung.
4. Vollmodell **zusätzlich** auf zwei Teilstichproben (überdurchschnittliche
   Windgeschwindigkeit, Norddeutschland) gerechnet: Steuerkraft bleibt in
   beiden nicht signifikant — der Nullbefund ist robust.

### Schritt 2 (optional/Bonus): k-NN

Klassifikation für eine mögliche Dashboard-Ähnlichkeitssuche ("Welche Kreise
ähneln Kreis X am meisten?"). Nicht Teil des Kernberichts.

---

## HYPOTHESEN

1. Positive Korrelation Steuerkraft → Windausbau war *erwartet*, aber nicht
   selbstverständlich (Pacht/Gewerbesteuer als Anreiz für finanzschwache
   Kreise; NIMBY-Effekte in wohlhabenden Kreisen möglich) — **nicht
   bestätigt**, sobald Windpotenzial kontrolliert wird.
2. Wirtschaftsstruktur als Moderator (Tourismus → weniger Ausbau,
   Industrie/Landwirtschaft → mehr) — deskriptiv sichtbar in Plot 3.

---

## BEKANNTE METHODISCHE HERAUSFORDERUNGEN

- Landesplanungsrecht variiert je Bundesland (Abstandsregeln, 2 %-Ziel) —
  als Robustheitscheck über die Norddeutschland-Teilstichprobe mit
  abgedeckt (s. o.), nicht als eigene Dummy-Variable im Hauptmodell.
- Räumliche Autokorrelation dokumentiert (Moran's I, Abschnitt "Methodik").
- Heteroskedastizität dokumentiert (Breusch-Pagan-Test).
- MaStR: Altanlagen teils mit ungenauen Koordinaten (aus dem alten Projekt
  bekannt, hier nicht erneut geprüft, da Datenbasis identisch).

---

## ENDPRODUKT

### 1. Projektbericht (Kern, verpflichtend)

- Datenpipeline & Methodik ([docs/00](docs/00_datenquellen.md)–[01](docs/01_datenaufbereitung.md))
- Deskriptive Analyse: bivariater Zusammenhang, Quartile,
  Wirtschaftsstruktur, Kontrollvariablen (Plot 1–4, log-skaliert)
- Multivariates Modell + Koeffizientenplot als Kernergebnis (Plot 7)
- Diagnostik (VIF, Breusch-Pagan, Moran's I) und Robustheitschecks
- Residuen-/Ausreißeranalyse (Plot 5)

### 2. Dashboard (optional, spätere Ausbaustufe)

Ein interaktives Shiny-Dashboard existiert im ursprünglichen Projektordner
(`Smart Planner/app.R`) mit Kreis-Explorer-Karte, k-NN-Peer-Suche und
Zeitreihen-Ansicht. Es ist eine mögliche spätere Ergänzung ("soweit die Zeit
reicht"), aber nicht Teil dieses Kernberichts-Ordners.

---

## PRÜFUNGSLEISTUNG (MCA)
1. Zwischenpräsentation
2. Endpräsentation
3. Projektbericht (Datenanalyse, Code, Interpretation) ← **Kern dieses Ordners**

---

*Neu aufgesetzt am 2026-07-04 nach Konsultations-Nachbereitung: Bericht statt
Dashboard als Fokus, Datenquellen-Recherche durch direkte Übersicht ersetzt,
Robustheitschecks zu Filter-/Regions-Fragen ergänzt, log-Skala für schiefe
Verteilungen dokumentiert.*
