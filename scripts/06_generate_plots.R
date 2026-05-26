# ==============================================================================
# SMART PLANNER: GGPLOT2 PLOT GENERATOR FOR REPORT
# ==============================================================================
# This script generates 6 high-resolution plots for the final report.
# The plots are designed in a clean, professional scientific layout (light theme)
# suitable for insertion into documents and presentations.
# Saved to: plots/
# ==============================================================================

library(sf)
library(tidyverse)
library(car)

cat("Creating plots directory...\n")
dir.create("plots", showWarnings = FALSE)

cat("Loading final dataset with residuals...\n")
districts_sf <- st_read("data/smart_planner_final_data_with_residuals.gpkg")

# Convert to standard tibble for plotting
df_plot <- districts_sf %>%
  st_drop_geometry() %>%
  as_tibble()

# Load R model objects
load("data/model_results.RData")

# ----------------------------------------------------------------------------
# REPORT GGPLOT THEME (Clean, high-contrast, professional)
# ----------------------------------------------------------------------------
theme_report <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#f8fafc", color = NA),
      panel.grid.major = element_line(color = "#e2e8f0", size = 0.5),
      panel.grid.minor = element_line(color = "#f1f5f9", size = 0.3),
      text = element_text(color = "#0f172a", family = "sans"),
      axis.text = element_text(color = "#475569"),
      axis.title = element_text(color = "#0f172a", face = "bold"),
      legend.background = element_rect(fill = "white", color = "#cbd5e1"),
      legend.text = element_text(color = "#475569"),
      legend.title = element_text(color = "#0f172a", face = "bold"),
      plot.title = element_text(face = "bold", size = 14, color = "#0f172a", margin = margin(b=8)),
      plot.subtitle = element_text(color = "#475569", size = 10, margin = margin(b=12)),
      plot.caption = element_text(color = "#64748b", size = 8, margin = margin(t=10)),
      strip.background = element_rect(fill = "#cbd5e1", color = NA),
      strip.text = element_text(face = "bold", color = "#0f172a")
    )
}

# ==============================================================================
# PLOT 1: Bivariate Analyse (Steuerkraft vs. Windkraftdichte)
# ==============================================================================
cat("Generating Plot 1: Bivariate Scatterplot...\n")
p1 <- ggplot(df_plot, aes(x = Steuerkraft, y = Wind_Density_kW_km2)) +
  geom_point(aes(color = Performance_Class), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", aes(fill = "Linear (OLS)"), color = "#0ea5e9", size = 1.2, se = TRUE) +
  geom_smooth(method = "loess", aes(fill = "Nicht-linear (LOESS)"), color = "#10b981", linetype = "dashed", size = 1, se = FALSE) +
  scale_color_manual(
    values = c("Normal" = "#64748b", "Outperformer (Hoch)" = "#059669", "Underperformer (Tief)" = "#dc2626"),
    name = "Performance-Klasse"
  ) +
  scale_fill_manual(
    values = c("Linear (OLS)" = "#bae6fd", "Nicht-linear (LOESS)" = NA),
    name = "Modellanpassung"
  ) +
  labs(
    title = "Zusammenhang zwischen Steuerkraft und Windkraftdichte",
    subtitle = "400 Landkreise in Deutschland (Stand: 31.12.2023)",
    x = "Steuereinnahmekraft (€ / Einwohner)",
    y = "Windkraft-Kapazitätsdichte (kW / km² Landfläche)",
    caption = "Datenquellen: Marktstammdatenregister (BNetzA), BBSR INKAR, BKG. Eigene Berechnungen."
  ) +
  theme_report()

ggsave("plots/01_bivariate_scatter.png", p1, width = 7, height = 5, dpi = 300)

# ==============================================================================
# PLOT 2: Ausbau nach Steuerkraft-Quartilen (Boxplots)
# ==============================================================================
cat("Generating Plot 2: Quartile Boxplots...\n")
# Create quartiles
df_quartiles <- df_plot %>%
  mutate(
    Steuerkraft_Quartil = cut(
      Steuerkraft,
      breaks = quantile(Steuerkraft, probs = 0:4/4),
      include.lowest = TRUE,
      labels = c("Q1 (Finanzschwach)", "Q2 (Mittel-Unter)", "Q3 (Mittel-Ober)", "Q4 (Finanzstark)")
    )
  )

p2 <- ggplot(df_quartiles, aes(x = Steuerkraft_Quartil, y = Wind_Density_kW_km2)) +
  geom_violin(fill = "#bae6fd", color = "#0ea5e9", alpha = 0.4) +
  geom_boxplot(width = 0.2, fill = "white", color = "#0f172a", outlier.size = 1.5, outlier.color = "#dc2626") +
  labs(
    title = "Windkraftdichte nach Steuerkraft-Quartilen",
    subtitle = "Einteilung der 400 Landkreise in vier Finanzstärkeklassen",
    x = "Steuereinnahmekraft-Klasse",
    y = "Windkraft-Kapazitätsdichte (kW / km²)",
    caption = "Q1 bis Q4 stellen die vier Viertel der Landkreise dar. Rote Punkte zeigen Ausreißer."
  ) +
  theme_report()

ggsave("plots/02_quartile_boxplots.png", p2, width = 7, height = 5, dpi = 300)

# ==============================================================================
# PLOT 3: Ausbau nach Wirtschaftsstruktur (Violin-Plots)
# ==============================================================================
cat("Generating Plot 3: Economic Structure Violin plots...\n")
p3 <- ggplot(df_plot, aes(x = Economic_Structure, y = Wind_Density_kW_km2)) +
  geom_violin(aes(fill = Economic_Structure), alpha = 0.5, color = "#475569") +
  geom_boxplot(width = 0.1, fill = "white", color = "#0f172a", outlier.shape = NA) +
  scale_fill_brewer(palette = "Set2", name = "Wirtschaftsstruktur") +
  labs(
    title = "Windkraftdichte nach dominanter Wirtschaftsstruktur",
    subtitle = "Klassifikation basierend auf den Beschäftigtenanteilen im Kreis",
    x = "Wirtschaftsstruktur-Typus",
    y = "Windkraft-Kapazitätsdichte (kW / km²)",
    caption = "Klassifizierung: Industriell (Sek. Sektor >= 35%), Ländlich (Prim. Sektor >= 3%), Dienstleistungen (Tert. Sektor >= 65%)."
  ) +
  theme_report() +
  theme(legend.position = "none")

ggsave("plots/03_economic_structure.png", p3, width = 7, height = 5, dpi = 300)

# ==============================================================================
# PLOT 4: Einfluss der Kontrollvariablen (Faceted Scatterplots)
# ==============================================================================
cat("Generating Plot 4: Faceted Control Variables...\n")
df_long <- df_plot %>%
  mutate(Log10_Einwohnerdichte = log10(Einwohnerdichte)) %>%
  select(Wind_Density_kW_km2, Log10_Einwohnerdichte, Waldflaeche_Prozent, Landwirtschaft_Prozent) %>%
  pivot_longer(
    cols = c(Log10_Einwohnerdichte, Waldflaeche_Prozent, Landwirtschaft_Prozent),
    names_to = "Variable",
    values_to = "Wert"
  ) %>%
  mutate(
    Variable_Clean = case_when(
      Variable == "Log10_Einwohnerdichte" ~ "Einwohnerdichte (log10, Ew./km²)",
      Variable == "Waldflaeche_Prozent" ~ "Waldflächenanteil (%)",
      Variable == "Landwirtschaft_Prozent" ~ "Landwirtschaftsanteil (%)",
      TRUE ~ Variable
    )
  )

p4 <- ggplot(df_long, aes(x = Wert, y = Wind_Density_kW_km2)) +
  geom_point(alpha = 0.4, color = "#64748b", size = 1.2) +
  geom_smooth(method = "lm", color = "#0ea5e9", fill = "#bae6fd", alpha = 0.2, size = 1) +
  facet_wrap(~ Variable_Clean, scales = "free_x") +
  labs(
    title = "Einfluss geografischer und demografischer Kontrollvariablen",
    subtitle = "Zusammenhang mit der Windkraftdichte und OLS-Regressionslinie",
    x = "Wert der Kontrollvariable",
    y = "Windkraft-Kapazitätsdichte (kW / km²)",
    caption = "Einwohnerdichte wurde logarithmisch transformiert, um die städtischen Ballungsräume adäquat abzubilden."
  ) +
  theme_report()

ggsave("plots/04_control_variables.png", p4, width = 8.5, height = 4.5, dpi = 300)

# ==============================================================================
# PLOT 5: Die größten Abweichungen (Residuen-Bar-Chart)
# ==============================================================================
cat("Generating Plot 5: Top Outliers (Residuals)...\n")
outliers <- df_plot %>%
  arrange(desc(Residuals))

top_outperformers <- head(outliers, 10)
top_underperformers <- tail(outliers, 10)

top_both <- bind_rows(top_outperformers, top_underperformers) %>%
  mutate(
    Landkreis = reorder(paste0(GEN, " (", BEZ, ")"), Residuals)
  )

p5 <- ggplot(top_both, aes(x = Landkreis, y = Residuals, fill = Performance_Class)) +
  geom_col(color = "black", size = 0.2) +
  coord_flip() +
  scale_fill_manual(
    values = c("Outperformer (Hoch)" = "#059669", "Underperformer (Tief)" = "#dc2626"),
    name = "Performance-Klasse"
  ) +
  labs(
    title = "Die 10 extremsten Outperformer und Underperformer",
    subtitle = "Größte positive und negative Abweichungen (Residuen) vom OLS-Modell",
    x = "Landkreis",
    y = "Modellabweichung / Residuum (kW / km²)",
    caption = "Positive Abweichung = mehr Windenergie als modellseitig erwartet; Negative Abweichung = weniger Ausbau als erwartet."
  ) +
  theme_report() +
  theme(legend.position = "bottom")

ggsave("plots/05_model_outliers.png", p5, width = 7, height = 5.5, dpi = 300)

# ==============================================================================
# PLOT 6: Modelldiagnose-Plots (Residuals vs. Fitted & Q-Q)
# ==============================================================================
cat("Generating Plot 6: Regression Diagnostics...\n")
# Prepare data frames for diagnostic plots
diag_df <- data.frame(
  Fitted = fitted(model_raw),
  Residuals = residuals(model_raw),
  Std_Residuals = scale(residuals(model_raw))
)

p6_fit <- ggplot(diag_df, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "#64748b", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "#dc2626", linetype = "dashed", size = 0.8) +
  geom_smooth(method = "loess", color = "#0ea5e9", se = FALSE, size = 1) +
  labs(
    title = "Residuen vs. Vorhergesagte Werte",
    subtitle = "Überprüfung der Linearität und Homoskedastizität",
    x = "Vorhergesagter Wert (Fitted Values)",
    y = "Residuen",
    caption = "Die blaue Linie zeigt den LOESS-Trend der Abweichungen."
  ) +
  theme_report()

p6_qq <- ggplot(diag_df, aes(sample = Std_Residuals)) +
  stat_qq(color = "#64748b", alpha = 0.5) +
  stat_qq_line(color = "#0ea5e9", size = 1) +
  labs(
    title = "Normal-Q-Q-Plot der Residuen",
    subtitle = "Überprüfung der Normalverteilungsannahme der Fehlerterme",
    x = "Theoretische Quantile",
    y = "Standardisierte Residuen",
    caption = "Punkte sollten idealerweise auf der diagonalen Referenzlinie liegen."
  ) +
  theme_report()

ggsave("plots/06a_residuals_fit.png", p6_fit, width = 6, height = 4.5, dpi = 300)
ggsave("plots/06b_qq_plot.png", p6_qq, width = 6, height = 4.5, dpi = 300)

cat("Successfully generated and saved all ggplot2 plots under plots/\n")
