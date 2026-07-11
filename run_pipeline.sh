#!/bin/bash
# ==============================================================================
# SMART PLANNER BERICHT: GESAMTE PIPELINE AUSFUEHREN
# ==============================================================================
# Fuehrt alle Schritte in der Reihenfolge aus, in der sie gedacht sind:
# Datenquellen holen (01-04) -> zusammenfuehren (05) -> Regression (06) ->
# Report-Grafiken (07). Siehe docs/00_datenquellen.md fuer den Kontext.
#
# Aufruf (vom Projektordner aus):
#   bash run_pipeline.sh
# ==============================================================================
set -e

echo "========================================================"
echo "SMART PLANNER BERICHT: Pipeline startet..."
echo "========================================================"

echo ""
echo "--- Schritt 1: Kreisgrenzen (BKG VG250-EW) ---"
Rscript scripts/01_get_geodata.R

echo ""
echo "--- Schritt 2: Windkraftanlagen je Kreis (MaStR) ---"
python3 scripts/02_get_wind_turbines.py
Rscript scripts/02_get_wind_turbines.R

echo ""
echo "--- Schritt 3: Windgeschwindigkeit je Kreis (Global Wind Atlas) ---"
Rscript scripts/03_get_wind_speed.R

echo ""
echo "--- Schritt 4: INKAR-Kennzahlen je Kreis ---"
Rscript scripts/04_get_inkar.R

echo ""
echo "--- Schritt 5: Datenquellen zusammenfuehren ---"
Rscript scripts/05_merge_data.R

echo ""
echo "--- Schritt 6: Regression, Diagnostik & Robustheitschecks ---"
Rscript scripts/06_model_regression.R

echo ""
echo "--- Schritt 7: Report-Grafiken erzeugen ---"
Rscript scripts/07_generate_plots.R

echo ""
echo "========================================================"
echo "FERTIG: Pipeline erfolgreich durchgelaufen."
echo "Ergebnisse: data/smart_planner_daten_mit_residuen.gpkg,"
echo "Report-Grafiken unter plots/."
echo "========================================================"
