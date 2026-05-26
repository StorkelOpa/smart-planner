# ==============================================================================
# SMART PLANNER: EXPLORE BBSR INKAR VARIABLES
# ==============================================================================
# This script uses the 'bonn' package to connect to the INKAR JSON API,
# retrieve available themes and variables, and search for the indicator codes
# (Kennziffern) needed for the regression model.
# ==============================================================================

library(bonn)
library(tidyverse)
library(httr)

# Disable SSL verification to bypass local CA certificate errors for www.inkar.de
set_config(config(ssl_verifypeer = FALSE))

cat("Fetching geographic levels from INKAR...\n")
geographies <- get_geographies()
print(head(geographies))

# Target geography is "KRE" (Kreise)
krs_geo <- geographies %>% filter(grepl("KRE|Kreis", Kurzname, ignore.case = TRUE) | ID == "KRE")
cat("\nTarget Geography for Districts:\n")
print(krs_geo)

cat("\nFetching themes for KRE (Kreise)...\n")
# We fetch all themes for KRE geography
themes <- get_themes(geography = "KRE")
cat("Found", nrow(themes), "themes. Examples:\n")
print(head(themes, 10))

cat("\nFetching all variables for KRE to build a searchable metadata catalog...\n")
# Fetch variables for each theme
all_variables <- list()

for (i in 1:nrow(themes)) {
  theme_id <- themes$ID[i]
  theme_title <- paste(themes$Bereich[i], themes$Unterbereich[i], sep = " - ")
  cat("Fetching variables for theme:", theme_title, "(", theme_id, ")...\n")
  
  # Fetch variables for this theme
  tryCatch({
    vars <- get_variables(geography = "KRE", theme = theme_id)
    if (!is.null(vars) && nrow(vars) > 0) {
      vars_mapped <- vars %>%
        mutate(
          name = Gruppe,
          title = KurznamePlus,
          description = paste(theme_title, KurznamePlus),
          theme_id = theme_id,
          theme_title = theme_title
        )
      all_variables[[theme_id]] <- vars_mapped
    }
  }, error = function(e) {
    cat("  Warning: could not fetch variables for theme", theme_id, "\n")
  })
}

# Combine into a single tibble
metadata <- bind_rows(all_variables)
cat("\nTotal variables found:", nrow(metadata), "\n")

# Save a local CSV copy of the metadata catalog for easy browsing
metadata_file <- "data/data_metadata_inkar.csv"
write_csv(metadata, metadata_file)
cat("Saved searchable metadata catalog to:", metadata_file, "\n")

# ==============================================================================
# SEARCH FOR OUR SPECIFIC TARGET VARIABLES
# ==============================================================================
cat("\n--- SEARCHING FOR TARGET VARIABLES ---\n")

# 1. Steuereinnahmekraft (Tax Revenue Capacity)
tax_vars <- metadata %>% 
  filter(grepl("steuer|einnahmekraft|finanz", title, ignore.case = TRUE) | 
         grepl("steuer|einnahmekraft|finanz", description, ignore.case = TRUE))
cat("\n[Tax Revenue / Finanzkraft Variables]:\n")
print(tax_vars %>% select(name, title, theme_title))

# 2. Flächennutzung (Land Use: Wald, Offenland, Landwirtschaft)
land_vars <- metadata %>% 
  filter(grepl("wald|forst|landwirtschaft|offenland|fläche", title, ignore.case = TRUE))
cat("\n[Land Use / Flächennutzung Variables]:\n")
print(land_vars %>% select(name, title, theme_title) %>% head(15))

# 3. Bevölkerungsdichte (Population Density)
pop_vars <- metadata %>% 
  filter(grepl("dichte|bevölkerung", title, ignore.case = TRUE))
cat("\n[Population Density / Bevölkerungsdichte Variables]:\n")
print(pop_vars %>% select(name, title, theme_title) %>% head(10))

# 4. Wirtschaftsstruktur / Beschäftigte (Economic Structure / Employment Shares)
emp_vars <- metadata %>% 
  filter(grepl("beschäftigte|wirtschafts|wz|dienstleistung|tourismus|industrie|produzier", title, ignore.case = TRUE))
cat("\n[Employment / Wirtschaftsstruktur Variables]:\n")
print(emp_vars %>% select(name, title, theme_title) %>% head(15))

cat("\nDone! Review the printed output or open 'data_metadata_inkar.csv' to choose the exact variable codes ('name' column) for the next step.\n")
