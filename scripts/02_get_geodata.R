# ==============================================================================
# SMART PLANNER: RETRIEVE BKG DISTRICT GEODATA
# ==============================================================================
# This script downloads the official administrative boundaries (VG250-EW) of
# German districts (Landkreise / kreisfreie Städte) as of 31.12.2023,
# extracts it, and creates a basic preview plot.
# ==============================================================================

library(sf)
library(ggplot2)
library(tidyverse)

# Create data and plots directories if they don't exist
dir.create("data", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)

# BKG Open Data URL for 31.12.2023 district boundaries (Ebenen-version, UTM32 projection)
# VG250-EW contains population numbers and area sizes as attributes
url_bkg <- "https://daten.gdz.bkg.bund.de/produkte/vg/vg250-ew_ebenen_1231/2023/vg250-ew_12-31.utm32s.shape.ebenen.zip"
dest_zip <- "data/vg250_2023.zip"
unzip_dir <- "data/vg250_2023"

# Download the file
if (!file.exists(dest_zip)) {
  cat("Downloading BKG district geodata (VG250-EW 2023). This may take a moment...\n")
  download.file(url_bkg, destfile = dest_zip, mode = "wb")
  cat("Download complete!\n")
} else {
  cat("ZIP file already exists locally. Skipping download.\n")
}

# Unzip the file
if (!dir.exists(unzip_dir)) {
  cat("Unzipping geodata...\n")
  unzip(dest_zip, exdir = unzip_dir)
  cat("Unzip complete!\n")
} else {
  cat("Geodata folder already exists. Skipping unzip.\n")
}

# Locating the shapefiles
# Typically, the folder contains multiple shapefiles for different levels:
# - VG250_GEM: Gemeinden (municipalities)
# - VG250_KRS: Kreise / Landkreise (districts) - this is our target!
# - VG250_LAN: Länder (states)
shapefile_path <- list.files(
  path = unzip_dir, 
  pattern = ".*krs.*\\.shp$", 
  recursive = TRUE, 
  full.names = TRUE, 
  ignore.case = TRUE
)

if (length(shapefile_path) == 0) {
  stop("Could not find county/district level shapefile (*krs*.shp) in the unzipped folder!")
}

cat("Found district shapefile:", shapefile_path[1], "\n")

# Load the shapefile
cat("Loading geodata into R...\n")
districts <- st_read(shapefile_path[1])

# Inspect coordinate system and structure
cat("\nCoordinate Reference System (CRS):\n")
print(st_crs(districts))

cat("\nSummary of attributes:\n")
print(glimpse(districts))

# Create a basic map plot to verify
cat("\nGenerating preview map...\n")
preview_plot <- ggplot(data = districts) +
  geom_sf(fill = "#2c3e50", color = "#ecf0f1", size = 0.1) +
  labs(
    title = "Verwaltungsgebiete Deutschland 1:250 000 (VG250)",
    subtitle = "Stand: 31.12.2023 (Kreisebene)",
    caption = "Datenquelle: © GeoBasis-DE / BKG (2023)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30")
  )

# Save the plot
ggsave("plots/02_geodata_preview.png", preview_plot, width = 8, height = 10, dpi = 150)
cat("Saved preview map to: plots/02_geodata_preview.png\n")
