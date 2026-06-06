#!/bin/bash
# ==============================================================================
# SMART PLANNER: DATA PIPELINE RUNNER
# ==============================================================================
# This script executes all data ingestion and preparation steps in sequence.
# Run this from the root directory of the project:
#   bash scripts/run_data_pipeline.sh
# ==============================================================================

# Exit immediately if a command exits with a non-zero status
set -e

echo "========================================================"
echo "SMART PLANNER: Starting data pipeline..."
echo "========================================================"
echo ""

echo "--- STEP 1: Environment Setup (Installing R Packages) ---"
Rscript scripts/00_setup.R
echo ""

echo "--- STEP 2: Parsing MaStR Wind XML to CSV ---"
python3 scripts/03_parse_mastr.py
echo ""

echo "--- STEP 3: Aggregating Wind Capacity by County ---"
Rscript scripts/03_process_wind_data.R
echo ""

echo "--- STEP 4: Downloading & Preparing BKG Geodata ---"
Rscript scripts/02_get_geodata.R
echo ""

echo "--- STEP 4b: Wind speed per county from Global Wind Atlas (WS3) ---"
Rscript scripts/02b_get_wind_speed.R
echo ""

echo "--- STEP 5: Fetching INKAR variables & creating catalog ---"
Rscript scripts/01_fetch_inkar_variables.R
echo ""

echo "--- STEP 6: Merging Data Sources (MaStR, INKAR, BKG, Wind Atlas) ---"
Rscript scripts/04_merge_data.R
echo ""

echo "--- STEP 7: Fitting Regression Models & Diagnostics ---"
Rscript scripts/05_model_regression.R
echo ""

echo "--- STEP 7b: Building wind expansion time series per county (WS1) ---"
Rscript scripts/07_build_wind_timeseries.R
echo ""

echo "--- STEP 8: Generating Scientific Plots for Report ---"
Rscript scripts/06_generate_plots.R
echo ""

echo "========================================================"
echo "SUCCESS: Full data pipeline completed!"
echo "Parsed wind data, aggregated capacity, downloaded county shapes,"
echo "saved INKAR metadata, merged all data, executed regression"
echo "diagnostics, and generated report plots in 'plots/'."
echo "You can now launch the dashboard using 'shiny::runApp()'."
echo "========================================================"

