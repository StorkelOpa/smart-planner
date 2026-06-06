# ==============================================================================
# SMART PLANNER: REGRESSION MODELING & DIAGNOSTICS
# ==============================================================================
# This script fits the OLS regression models (both raw and standardized),
# performs regression diagnostics (VIF for multicollinearity, Breusch-Pagan
# for heteroskedasticity, and Moran's I for spatial autocorrelation),
# extracts the residuals to identify outperformers/underperformers,
# and saves the results for the Shiny dashboard.
# ==============================================================================

library(sf)
library(tidyverse)
library(car)
library(lmtest)
library(spdep)

cat("Loading final merged dataset...\n")
districts_sf <- st_read("data/smart_planner_final_data.gpkg")

# Convert to standard data frame for regression (remove geometry for speed)
df_regression <- districts_sf %>%
  st_drop_geometry() %>%
  as_tibble()

# ==============================================================================
# 1. FIT OLS REGRESSION MODEL (RAW DATA)
# ==============================================================================
cat("\nFitting OLS regression model (raw data)...\n")

# Model formula: Target is Wind_Density_kW_km2
# Predictors: Steuerkraft (finance), Einwohnerdichte (density),
# Windgeschwindigkeit_ms (Windpotenzial, NEU/WS3 - Kontrollvariable),
# Waldflaeche_Prozent, Landwirtschaft_Prozent (geographic/land use),
# Beschaeftigte_Sekundar, Beschaeftigte_Primar (employment structure).
# (Beschaeftigte_Tertiar is omitted to avoid perfect multicollinearity)
model_formula <- Wind_Density_kW_km2 ~ Steuerkraft + Einwohnerdichte +
  Windgeschwindigkeit_ms +
  Waldflaeche_Prozent + Landwirtschaft_Prozent +
  Beschaeftigte_Sekundar + Beschaeftigte_Primar

model_raw <- lm(model_formula, data = df_regression)
print(summary(model_raw))

# ==============================================================================
# 2. FIT STANDARDIZED OLS MODEL (Z-SCORES)
# ==============================================================================
cat("\nFitting standardized OLS regression model (Z-scores)...\n")

df_scaled <- df_regression %>%
  select(Wind_Density_kW_km2, Steuerkraft, Einwohnerdichte,
         Windgeschwindigkeit_ms,
         Waldflaeche_Prozent, Landwirtschaft_Prozent,
         Beschaeftigte_Sekundar, Beschaeftigte_Primar) %>%
  mutate(across(everything(), ~ as.vector(scale(.))))

model_std <- lm(model_formula, data = df_scaled)
print(summary(model_std))

# ==============================================================================
# 3. REGRESSION DIAGNOSTICS
# ==============================================================================
cat("\n--- REGRESSION DIAGNOSTICS ---\n")

# A. Multicollinearity (VIF)
vif_values <- vif(model_raw)
cat("\nVariance Inflation Factors (VIF):\n")
print(vif_values)

# B. Heteroskedasticity (Breusch-Pagan Test)
bp_test <- bptest(model_raw)
cat("\nBreusch-Pagan Test for Heteroskedasticity:\n")
print(bp_test)

# C. Spatial Autocorrelation (Moran's I of Residuals)
cat("\nCalculating spatial autocorrelation (Moran's I) for residuals...\n")

# Find neighbors using queen contiguity
nb <- poly2nb(districts_sf)
# Convert to spatial weights matrix (using zero.policy = TRUE to handle islands)
listw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Run Moran's I test on OLS residuals
moran_test <- lm.morantest(model_raw, listw, zero.policy = TRUE)
print(moran_test)

# ==============================================================================
# 4. EXTRACT RESIDUALS & IDENTIFY OUTLIERS
# ==============================================================================
cat("\nExtracting residuals and classifying outperformers/underperformers...\n")

# Add residuals to the dataset
districts_results <- districts_sf %>%
  mutate(
    Predicted_Wind_Density = predict(model_raw, newdata = df_regression),
    Residuals = residuals(model_raw),
    # Standardized residuals
    Residuals_Std = as.vector(scale(Residuals)),
    # Classification based on standardized residuals
    Performance_Class = case_when(
      Residuals_Std >= 1.5 ~ "Outperformer (Hoch)",
      Residuals_Std <= -1.5 ~ "Underperformer (Tief)",
      TRUE ~ "Normal"
    )
  )

cat("Counties by performance classification:\n")
print(table(districts_results$Performance_Class))

# Save results
output_gpkg <- "data/smart_planner_final_data_with_residuals.gpkg"
output_csv <- "data/smart_planner_final_data_with_residuals.csv"

cat("\nSaving final dataset with residuals...\n")
st_write(districts_results, output_gpkg, delete_dsn = TRUE)
write_csv(st_drop_geometry(districts_results), output_csv)

# Save RData object for the Shiny app
model_results_file <- "data/model_results.RData"
save(model_raw, model_std, vif_values, bp_test, moran_test, file = model_results_file)
cat("Saved R model objects and diagnostics to:", model_results_file, "\n")

cat("\nRegression modeling and diagnostics completed successfully!\n")
