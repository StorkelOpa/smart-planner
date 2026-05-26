# ==============================================================================
# SMART PLANNER: SETUP & SYSTEM DEPENDENCIES
# ==============================================================================
# This script prepares the R environment and explains the required system 
# dependencies for installing spatial and API packages on Fedora.
#
# STEP 1: SYSTEM DEPENDENCIES (Execute in your Linux terminal)
# Before running this R script, make sure to install the underlying geospatial
# C/C++ libraries on Fedora. Run the following command in your terminal:
#
#   sudo dnf install -y gdal-devel proj-devel geos-devel udunits2-devel sqlite-devel
#
# ==============================================================================

# STEP 2: INSTALL R PACKAGES
cat("Starting packages installation...\n")

# Configure user-level library path to avoid write permission errors on /usr/lib64/R/library
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib, .libPaths()))
cat("Using library path:", .libPaths()[1], "\n")

# List of required CRAN packages
required_cran_packages <- c(
  "tidyverse",  # Data manipulation, plotting (ggplot2)
  "sf",         # Spatial data handling (requires GDAL, GEOS, PROJ)
  "shiny",      # Interactive dashboard framework
  "leaflet",    # Interactive mapping library
  "car",        # Regression diagnostics (VIF test)
  "lmtest",     # Heteroskedasticity tests (Breusch-Pagan)
  "spdep",      # Spatial autocorrelation (Moran's I)
  "remotes"     # For installing packages from GitHub
)

# Install missing packages from CRAN
new_packages <- required_cran_packages[!(required_cran_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  cat("Installing from CRAN:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages, repos = "https://cloud.r-project.org")
} else {
  cat("All CRAN packages are already installed.\n")
}

# STEP 3: INSTALL THE 'bonn' PACKAGE FOR INKAR DATA
# The 'bonn' package is the recommended client for the BBSR INKAR JSON API.
if (!("bonn" %in% installed.packages()[,"Package"])) {
  cat("Installing 'bonn' package from GitHub (sumtxt/bonn)...\n")
  remotes::install_github("sumtxt/bonn", force = TRUE)
} else {
  cat("Package 'bonn' is already installed.\n")
}

cat("\nEnvironment setup complete!\n")
