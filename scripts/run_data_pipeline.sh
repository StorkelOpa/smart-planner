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

echo "--- STEP 5: Fetching INKAR variables & creating catalog ---"
Rscript scripts/01_fetch_inkar_variables.R
echo ""

echo "========================================================"
echo "SUCCESS: Data pipeline completed!"
echo "Parsed wind data, aggregated capacity, downloaded county shapes,"
echo "and saved INKAR metadata catalog in the 'data/' directory."
echo "========================================================"
