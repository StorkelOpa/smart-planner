# ==============================================================================
# SMART PLANNER: PROCESS & AGGREGATE MASTR WIND DATA
# ==============================================================================
# This script loads the parsed wind turbine CSV file, filters for onshore and 
# active (in operation) wind turbines, extracts the 5-digit AGS (county key), 
# and aggregates the installed capacity by county.
# ==============================================================================

library(tidyverse)

# Create data directory if it doesn't exist
dir.create("data", showWarnings = FALSE)

csv_path <- "data/wind_turbines.csv"
output_path <- "data/wind_capacity_by_county.csv"

if (!file.exists(csv_path)) {
  stop(paste("Error: Parsed CSV not found at", csv_path, "\nPlease run the Python parser script first (python scripts/03_parse_mastr.py)."))
}

cat("Loading parsed wind turbine data...\n")
# We explicitly read Gemeindeschluessel as character to preserve leading zeros (e.g. "05...")
wind_data <- read_csv(csv_path, col_types = cols(
  EinheitMastrNummer = col_character(),
  Gemeindeschluessel = col_character(),
  Landkreis = col_character(),
  Gemeinde = col_character(),
  Laengengrad = col_double(),
  Breitengrad = col_double(),
  Inbetriebnahmedatum = col_character(),
  EinheitBetriebsstatus = col_integer(),
  Bruttoleistung = col_double(),
  Nettonennleistung = col_double(),
  Lage = col_integer()
))

cat("Filtering for onshore and active turbines...\n")
# Filter criteria:
# - EinheitBetriebsstatus == 35 (In Betrieb / in operation)
# - Lage == 888 (Windkraft an Land / onshore)
wind_clean <- wind_data %>%
  filter(
    EinheitBetriebsstatus == 35,
    Lage == 888,
    !is.na(Gemeindeschluessel)
  )

cat("Aggregating installed capacity by county level (5-digit AGS)...\n")
# Extract first 5 digits of the Gemeindeschluessel to get the AGS for the county
wind_county <- wind_clean %>%
  mutate(
    AGS = str_sub(Gemeindeschluessel, 1, 5)
  ) %>%
  filter(nchar(AGS) == 5) %>%
  group_by(AGS) %>%
  summarise(
    Turbine_Count = n(),
    Total_Bruttoleistung_kW = sum(Bruttoleistung, na.rm = TRUE),
    Total_Nettoleistung_kW = sum(Nettonennleistung, na.rm = TRUE),
    # Keep list of unique county names for validation check
    Landkreis_Names = paste(unique(na.omit(Landkreis)), collapse = "; "),
    .groups = "drop"
  )

# Add some stats to the console
cat("\nSummary Statistics:\n")
cat("Total active onshore turbines parsed:", nrow(wind_clean), "\n")
cat("Number of unique counties with wind capacity:", nrow(wind_county), "\n")
cat("Total national capacity (Gross):", round(sum(wind_county$Total_Bruttoleistung_kW) / 1e6, 2), "GW\n")

# Save the aggregated dataset
write_csv(wind_county, output_path)
cat("\nSuccessfully saved aggregated wind capacity by county to:", output_path, "\n")
