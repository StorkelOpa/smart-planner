# ==============================================================================
# SMART PLANNER: DATA MERGING & PREPARATION
# ==============================================================================
# This script loads the aggregated wind capacity data, fetches the independent
# and control variables from BBSR INKAR, loads the BKG county shapes, joins
# everything by AGS (county key), calculates the target variable (Wind Density),
# classifies the economic structures, and saves the final datasets (GPKG and CSV).
# ==============================================================================

library(sf)
library(tidyverse)
library(bonn)
library(httr)

# Disable SSL verification to bypass local CA certificate errors for www.inkar.de
set_config(config(ssl_verifypeer = FALSE))

cat("Creating output directories...\n")
dir.create("data", showWarnings = FALSE)

# 1. Load wind capacity data
cat("Loading aggregated wind capacity data...\n")
wind_data <- read_csv("data/wind_capacity_by_county.csv", col_types = cols(
  AGS = col_character(),
  Turbine_Count = col_integer(),
  Total_Bruttoleistung_kW = col_double(),
  Total_Nettoleistung_kW = col_double(),
  Landkreis_Names = col_character()
))

# 2. Fetch variables from INKAR for 2023
cat("Fetching variables from INKAR for year 2023...\n")
# Target variables mapping:
# 294: Steuerkraft
# 320: Einwohnerdichte
# 264: Waldfläche
# 261: Landwirtschaftsfläche
# 103: Beschäftigte Primärer Sektor
# 104: Beschäftigte Sekundärer Sektor
# 105: Beschäftigte Tertiärer Sektor

fetch_inkar_var <- function(var_id, col_name) {
  cat("  Fetching variable", var_id, "(", col_name, ")...\n")
  d <- get_data(variable = var_id, geography = "KRE", time = 2023)
  d %>%
    select(AGS = Schlüssel, !!col_name := Wert)
}

steuerkraft <- fetch_inkar_var("294", "Steuerkraft")
einwohnerdichte <- fetch_inkar_var("320", "Einwohnerdichte")
waldflaeche <- fetch_inkar_var("264", "Waldflaeche_Prozent")
landwirtschaft <- fetch_inkar_var("261", "Landwirtschaft_Prozent")
primar_sektor <- fetch_inkar_var("103", "Beschaeftigte_Primar")
sekundar_sektor <- fetch_inkar_var("104", "Beschaeftigte_Sekundar")
tertiar_sektor <- fetch_inkar_var("105", "Beschaeftigte_Tertiar")

# Combine all INKAR data
cat("Combining INKAR data...\n")
inkar_combined <- steuerkraft %>%
  full_join(einwohnerdichte, by = "AGS") %>%
  full_join(waldflaeche, by = "AGS") %>%
  full_join(landwirtschaft, by = "AGS") %>%
  full_join(primar_sektor, by = "AGS") %>%
  full_join(sekundar_sektor, by = "AGS") %>%
  full_join(tertiar_sektor, by = "AGS")

# Clean and impute missing values for employment sectors
# Since primary/secondary employment can be missing in some small city-counties due to confidentiality,
# we impute these values based on logical sum constraints (shares sum to 100%).
inkar_clean <- inkar_combined %>%
  mutate(
    Beschaeftigte_Primar = coalesce(Beschaeftigte_Primar, 0),
    Beschaeftigte_Sekundar = ifelse(is.na(Beschaeftigte_Sekundar), 100 - Beschaeftigte_Tertiar - Beschaeftigte_Primar, Beschaeftigte_Sekundar),
    Beschaeftigte_Sekundar = coalesce(Beschaeftigte_Sekundar, 0)
  )

# 3. Load administrative boundaries from BKG
cat("Loading BKG geodata...\n")
shapefile_path <- list.files(
  path = "data/vg250_2023", 
  pattern = ".*krs.*\\.shp$", 
  recursive = TRUE, 
  full.names = TRUE, 
  ignore.case = TRUE
)

if (length(shapefile_path) == 0) {
  stop("District shapefile not found! Please run scripts/02_get_geodata.R first.")
}

districts <- st_read(shapefile_path[1]) %>%
  filter(GF == 4) %>% # Filter out coastal water areas
  select(AGS, GEN, BEZ, EWZ, KFL_km2 = KFL)

# Make geometries valid
districts <- st_make_valid(districts)

# 4. Merge all data sources
cat("Merging all datasets...\n")
merged_data <- districts %>%
  left_join(wind_data, by = "AGS") %>%
  left_join(inkar_clean, by = "AGS")

# Fill missing wind capacity values with 0 (since counties without wind turbines won't be in MaStR aggregated file)
merged_data <- merged_data %>%
  mutate(
    Turbine_Count = coalesce(Turbine_Count, 0L),
    Total_Bruttoleistung_kW = coalesce(Total_Bruttoleistung_kW, 0.0),
    Total_Nettoleistung_kW = coalesce(Total_Nettoleistung_kW, 0.0)
  )

# Calculate Wind Capacity Density (kW per km2 of land area)
merged_data <- merged_data %>%
  mutate(
    Wind_Density_kW_km2 = Total_Nettoleistung_kW / KFL_km2
  )

# 5. Classify the dominant economic structure
# Classification scheme:
# - "Industriell": Sekundärer Sektor >= 35%
# - "Ländlich/Agrarisch": Primärer Sektor >= 3.0% (very high agricultural share compared to average)
# - "Dienstleistungsorientiert": Tertiärer Sektor >= 65%
# - "Ausgeglichen": otherwise
merged_data <- merged_data %>%
  mutate(
    Economic_Structure = case_when(
      Beschaeftigte_Sekundar >= 35 ~ "Industriell",
      Beschaeftigte_Primar >= 3.0 ~ "Ländlich/Agrarisch",
      Beschaeftigte_Tertiar >= 65 ~ "Dienstleistungsorientiert",
      TRUE ~ "Ausgeglichen"
    )
  )

# Convert to WGS84 for Leaflet map compatibility
cat("Transforming CRS to WGS84 (EPSG:4326) for Leaflet...\n")
merged_data_wgs84 <- st_transform(merged_data, crs = 4326)

# Save final dataset
output_gpkg <- "data/smart_planner_final_data.gpkg"
output_csv <- "data/smart_planner_final_data.csv"

cat("Saving merged dataset...\n")
st_write(merged_data_wgs84, output_gpkg, delete_dsn = TRUE)

# For the CSV version, drop the geometry column
merged_csv_data <- merged_data_wgs84 %>%
  st_drop_geometry()

write_csv(merged_csv_data, output_csv)

cat("Summary of final dataset:\n")
cat("Counties in final dataset:", nrow(merged_csv_data), "\n")
cat("Total Wind Capacity (Net):", round(sum(merged_csv_data$Total_Nettoleistung_kW) / 1e6, 2), "GW\n")
cat("Counties by economic structure:\n")
print(table(merged_csv_data$Economic_Structure))

cat("\nMerging successfully completed and saved!\n")
