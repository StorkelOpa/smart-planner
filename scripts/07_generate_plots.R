# ==============================================================================
# SCHRITT 7: REPORT-GRAFIKEN ERZEUGEN
# ==============================================================================
# Erstellt die ggplot2-Grafiken fuer den Projektbericht (siehe
# docs/03_visualisierungen.md fuer Interpretationstexte). Alle Grafiken, die
# die Windkraftdichte auf einer kontinuierlichen Achse zeigen (Plot 1-4),
# nutzen eine pseudo-logarithmische Skala: die Variable ist stark rechts-
# schief (Median liegt bei nur ~4% des Maximalwerts), auf linearer Skala
# kleben fast alle Punkte am unteren Rand. Das Modell selbst (Schritt 6)
# bleibt davon unberuehrt - hier aendert sich nur die Darstellung.
#
# Eingang : data/smart_planner_daten_mit_residuen.gpkg
#           data/modell_ergebnisse.RData
# Ausgang : plots/01-08 (.png)
# ==============================================================================

library(sf)
library(tidyverse)
library(scales)

dir.create("plots", showWarnings = FALSE)

cat("Lade finalen Datensatz und Modellergebnisse...\n")
daten_sf <- st_read("data/smart_planner_daten_mit_residuen.gpkg", quiet = TRUE)
df <- daten_sf %>% st_drop_geometry() %>% as_tibble()
load("data/modell_ergebnisse.RData")

# ------------------------------------------------------------------------------
# REPORT-THEME (helles, professionelles Layout)
# ------------------------------------------------------------------------------
theme_report <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#f8fafc", color = NA),
      panel.grid.major = element_line(color = "#e2e8f0", linewidth = 0.5),
      panel.grid.minor = element_line(color = "#f1f5f9", linewidth = 0.3),
      text = element_text(color = "#0f172a", family = "sans"),
      axis.text = element_text(color = "#475569"),
      axis.title = element_text(color = "#0f172a", face = "bold"),
      legend.background = element_rect(fill = "white", color = "#cbd5e1"),
      legend.text = element_text(color = "#475569"),
      legend.title = element_text(color = "#0f172a", face = "bold"),
      plot.title = element_text(face = "bold", size = 14, color = "#0f172a", margin = margin(b = 8)),
      plot.subtitle = element_text(color = "#475569", size = 10, margin = margin(b = 12)),
      plot.caption = element_text(color = "#64748b", size = 8, margin = margin(t = 10)),
      strip.background = element_rect(fill = "#cbd5e1", color = NA),
      strip.text = element_text(face = "bold", color = "#0f172a")
    )
}

# Pseudo-log-Skala fuer die Windkraftdichte-Achse: verhaelt sich wie log10,
# behandelt aber exakte Nullen (Kreise ganz ohne Windkraft) korrekt - ein
# normaler Logarithmus wuerde dort -Inf ergeben.
skala_windkraftdichte <- scale_y_continuous(
  trans = pseudo_log_trans(base = 10),
  breaks = c(0, 10, 100, 1000)
)

# ==============================================================================
# PLOT 1: Bivariate Analyse (Steuerkraft vs. Windkraftdichte)
# ==============================================================================
cat("Erzeuge Plot 1: Bivariater Scatterplot...\n")
p1 <- ggplot(df, aes(x = Steuerkraft, y = Wind_Density_kW_km2)) +
  geom_point(aes(color = Performance_Class), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", aes(fill = "Linear (OLS)"), color = "#0ea5e9", linewidth = 1.2, se = TRUE) +
  geom_smooth(method = "loess", aes(fill = "Nicht-linear (LOESS)"), color = "#10b981", linetype = "dashed", linewidth = 1, se = FALSE) +
  scale_color_manual(
    values = c("Normal" = "#64748b", "Outperformer (Hoch)" = "#059669", "Underperformer (Tief)" = "#dc2626"),
    name = "Performance-Klasse"
  ) +
  scale_fill_manual(
    values = c("Linear (OLS)" = "#bae6fd", "Nicht-linear (LOESS)" = NA),
    name = "Modellanpassung"
  ) +
  skala_windkraftdichte +
  labs(
    title = "Zusammenhang zwischen Steuerkraft und Windkraftdichte",
    subtitle = "400 Landkreise in Deutschland (Stand: 31.12.2023) – y-Achse log-skaliert",
    x = "Steuereinnahmekraft (€ / Einwohner)",
    y = "Windkraft-Kapazitätsdichte (kW / km², log-Skala)",
    caption = "Datenquellen: Marktstammdatenregister (BNetzA), BBSR INKAR, BKG. Eigene Berechnungen."
  ) +
  theme_report()

ggsave("plots/01_bivariate_scatter.png", p1, width = 7, height = 5, dpi = 300)

# ==============================================================================
# PLOT 2: Ausbau nach Steuerkraft-Quartilen (Boxplots)
# ==============================================================================
cat("Erzeuge Plot 2: Quartils-Boxplots...\n")
df_quartile <- df %>%
  mutate(
    Steuerkraft_Quartil = cut(
      Steuerkraft,
      breaks = quantile(Steuerkraft, probs = 0:4 / 4),
      include.lowest = TRUE,
      labels = c("Q1 (Finanzschwach)", "Q2 (Mittel-Unter)", "Q3 (Mittel-Ober)", "Q4 (Finanzstark)")
    )
  )

p2 <- ggplot(df_quartile, aes(x = Steuerkraft_Quartil, y = Wind_Density_kW_km2)) +
  geom_violin(fill = "#bae6fd", color = "#0ea5e9", alpha = 0.4) +
  geom_boxplot(width = 0.2, fill = "white", color = "#0f172a", outlier.size = 1.5, outlier.color = "#dc2626") +
  skala_windkraftdichte +
  labs(
    title = "Windkraftdichte nach Steuerkraft-Quartilen",
    subtitle = "Einteilung der 400 Landkreise in vier Finanzstärkeklassen – y-Achse log-skaliert",
    x = "Steuereinnahmekraft-Klasse",
    y = "Windkraft-Kapazitätsdichte (kW / km², log-Skala)",
    caption = "Q1 bis Q4 stellen die vier Viertel der Landkreise dar. Rote Punkte zeigen Ausreißer."
  ) +
  theme_report()

ggsave("plots/02_quartile_boxplots.png", p2, width = 7, height = 5, dpi = 300)

# ==============================================================================
# PLOT 3: Ausbau nach Wirtschaftsstruktur (Violin-Plots)
# ==============================================================================
cat("Erzeuge Plot 3: Wirtschaftsstruktur-Violinplots...\n")
p3 <- ggplot(df, aes(x = Economic_Structure, y = Wind_Density_kW_km2)) +
  geom_violin(aes(fill = Economic_Structure), alpha = 0.5, color = "#475569") +
  geom_boxplot(width = 0.1, fill = "white", color = "#0f172a", outlier.shape = NA) +
  scale_fill_brewer(palette = "Set2", name = "Wirtschaftsstruktur") +
  skala_windkraftdichte +
  labs(
    title = "Windkraftdichte nach dominanter Wirtschaftsstruktur",
    subtitle = "Klassifikation basierend auf den Beschäftigtenanteilen im Kreis – y-Achse log-skaliert",
    x = "Wirtschaftsstruktur-Typus",
    y = "Windkraft-Kapazitätsdichte (kW / km², log-Skala)",
    caption = "Klassifizierung: Industriell (Sek. Sektor >= 35%), Ländlich (Prim. Sektor >= 3%), Dienstleistungen (Tert. Sektor >= 65%)."
  ) +
  theme_report() +
  theme(legend.position = "none")

ggsave("plots/03_wirtschaftsstruktur.png", p3, width = 7, height = 5, dpi = 300)

# ==============================================================================
# PLOT 4: Einfluss der Kontrollvariablen (Faceted Scatterplots)
# ==============================================================================
cat("Erzeuge Plot 4: Facettierte Kontrollvariablen...\n")
df_lang <- df %>%
  mutate(Log10_Einwohnerdichte = log10(Einwohnerdichte)) %>%
  select(Wind_Density_kW_km2, Windgeschwindigkeit_ms, Log10_Einwohnerdichte,
         Waldflaeche_Prozent, Landwirtschaft_Prozent) %>%
  pivot_longer(
    cols = c(Windgeschwindigkeit_ms, Log10_Einwohnerdichte,
             Waldflaeche_Prozent, Landwirtschaft_Prozent),
    names_to = "Variable", values_to = "Wert"
  ) %>%
  mutate(
    Variable_Clean = case_when(
      Variable == "Windgeschwindigkeit_ms" ~ "Ø Windgeschwindigkeit (m/s, 150 m)",
      Variable == "Log10_Einwohnerdichte" ~ "Einwohnerdichte (log10, Ew./km²)",
      Variable == "Waldflaeche_Prozent" ~ "Waldflächenanteil (%)",
      Variable == "Landwirtschaft_Prozent" ~ "Landwirtschaftsanteil (%)",
      TRUE ~ Variable
    ),
    Variable_Clean = factor(Variable_Clean, levels = c(
      "Ø Windgeschwindigkeit (m/s, 150 m)",
      "Einwohnerdichte (log10, Ew./km²)",
      "Waldflächenanteil (%)",
      "Landwirtschaftsanteil (%)"
    ))
  )

p4 <- ggplot(df_lang, aes(x = Wert, y = Wind_Density_kW_km2)) +
  geom_point(alpha = 0.4, color = "#64748b", size = 1.2) +
  geom_smooth(method = "lm", color = "#0ea5e9", fill = "#bae6fd", alpha = 0.2, linewidth = 1) +
  facet_wrap(~ Variable_Clean, scales = "free_x") +
  skala_windkraftdichte +
  labs(
    title = "Einfluss geografischer und demografischer Kontrollvariablen",
    subtitle = "Zusammenhang mit der Windkraftdichte und OLS-Regressionslinie – y-Achse log-skaliert",
    x = "Wert der Kontrollvariable",
    y = "Windkraft-Kapazitätsdichte (kW / km², log-Skala)",
    caption = "Einwohnerdichte wurde logarithmisch transformiert, um die städtischen Ballungsräume adäquat abzubilden."
  ) +
  theme_report()

ggsave("plots/04_kontrollvariablen.png", p4, width = 8.5, height = 7, dpi = 300)

# ==============================================================================
# PLOT 5: Die groessten Abweichungen (Residuen-Bar-Chart)
# ==============================================================================
# Hinweis: Residuen sind kein rechtsschiefes Rohmass (sie streuen um 0) -
# hier bleibt die lineare Skala, keine log-Transformation.
cat("Erzeuge Plot 5: Top-Ausreißer (Residuen)...\n")
ausreisser <- df %>% arrange(desc(Residuals))
top_beide <- bind_rows(head(ausreisser, 10), tail(ausreisser, 10)) %>%
  mutate(Landkreis = reorder(paste0(GEN, " (", BEZ, ")"), Residuals))

p5 <- ggplot(top_beide, aes(x = Landkreis, y = Residuals, fill = Performance_Class)) +
  geom_col(color = "black", linewidth = 0.2) +
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
    caption = "Positive Abweichung = mehr Windenergie als modellseitig erwartet; negative = weniger als erwartet."
  ) +
  theme_report() +
  theme(legend.position = "bottom")

ggsave("plots/05_residuen_ausreisser.png", p5, width = 7, height = 5.5, dpi = 300)

# ==============================================================================
# PLOT 6: Modelldiagnose (Residuals vs. Fitted & Q-Q)
# ==============================================================================
cat("Erzeuge Plot 6: Modelldiagnostik...\n")
diag_df <- data.frame(
  Fitted = fitted(modell_roh),
  Residuals = residuals(modell_roh),
  Std_Residuals = scale(residuals(modell_roh))
)

p6_fit <- ggplot(diag_df, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "#64748b", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "#dc2626", linetype = "dashed", linewidth = 0.8) +
  geom_smooth(method = "loess", color = "#0ea5e9", se = FALSE, linewidth = 1) +
  labs(
    title = "Residuen vs. Vorhergesagte Werte",
    subtitle = "Überprüfung der Linearität und Homoskedastizität",
    x = "Vorhergesagter Wert (Fitted Values)", y = "Residuen",
    caption = "Die blaue Linie zeigt den LOESS-Trend der Abweichungen."
  ) +
  theme_report()

p6_qq <- ggplot(diag_df, aes(sample = Std_Residuals)) +
  stat_qq(color = "#64748b", alpha = 0.5) +
  stat_qq_line(color = "#0ea5e9", linewidth = 1) +
  labs(
    title = "Normal-Q-Q-Plot der Residuen",
    subtitle = "Überprüfung der Normalverteilungsannahme der Fehlerterme",
    x = "Theoretische Quantile", y = "Standardisierte Residuen",
    caption = "Punkte sollten idealerweise auf der diagonalen Referenzlinie liegen."
  ) +
  theme_report()

ggsave("plots/06a_residuen_fitted.png", p6_fit, width = 6, height = 4.5, dpi = 300)
ggsave("plots/06b_qq_plot.png", p6_qq, width = 6, height = 4.5, dpi = 300)

# ==============================================================================
# PLOT 7 (NEU): Koeffizientenplot - das "Herzstueck" des multivariaten Modells
# ==============================================================================
# Zeigt alle sieben Einflussfaktoren gleichzeitig, mit standardisierten
# Effektstaerken (vergleichbare Einheiten) und 95%-Konfidenzintervallen.
# Beantwortet direkt: welche Faktoren haben nach gegenseitiger Kontrolle noch
# Einfluss? (Bislang nur im Dashboard vorhanden, jetzt auch im Bericht.)
cat("Erzeuge Plot 7: Koeffizientenplot (Kernergebnis des multivariaten Modells)...\n")
koeff <- as.data.frame(summary(modell_std)$coefficients)
koeff$Variable <- rownames(koeff)
konf <- confint(modell_std)
koeff$Lower <- konf[, 1]
koeff$Upper <- konf[, 2]
koeff <- koeff %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    Klarname = case_when(
      Variable == "Steuerkraft" ~ "Steuereinnahmekraft",
      Variable == "Einwohnerdichte" ~ "Einwohnerdichte",
      Variable == "Windgeschwindigkeit_ms" ~ "Ø Windgeschwindigkeit",
      Variable == "Waldflaeche_Prozent" ~ "Flächenanteil Wald",
      Variable == "Landwirtschaft_Prozent" ~ "Flächenanteil Landwirtschaft",
      Variable == "Beschaeftigte_Sekundar" ~ "Anteil Beschäftigte Industrie",
      Variable == "Beschaeftigte_Primar" ~ "Anteil Beschäftigte Landwirtschaft",
      TRUE ~ Variable
    )
  )

p7 <- ggplot(koeff, aes(x = reorder(Klarname, Estimate), y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#dc2626", linewidth = 0.8) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "#94a3b8", linewidth = 1) +
  geom_point(color = "#0284c7", size = 4) +
  coord_flip() +
  labs(
    title = str_wrap("Standardisierte Effektstärken im multivariaten Modell", width = 50),
    subtitle = str_wrap("Punkt = geschätzter Koeffizient, Balken = 95%-Konfidenzintervall. Kreuzt der Balken die 0, ist der Effekt nicht belastbar.", width = 75),
    x = NULL, y = "Effektstärke (in Standardabweichungen)",
    caption = str_wrap("Windgeschwindigkeit dominiert den Ausbau; Steuerkraft ist nicht signifikant (kreuzt die Null-Linie deutlich).", width = 90)
  ) +
  theme_report()

ggsave("plots/07_koeffizientenplot.png", p7, width = 7.5, height = 5.2, dpi = 300)

# ==============================================================================
# PLOT 8 (NEU): Robustheitschecks - Steuerkraft-Effekt ueber drei Stichproben
# ==============================================================================
# Dieselbe Fragestellung ("wirkt Steuerkraft?"), dreimal auf unterschiedlich
# eingeschraenkten Teilstichproben beantwortet (siehe docs/02, Abschnitt 5).
# Rohkoeffizienten sind hier direkt vergleichbar, da es in allen drei Faellen
# dieselbe Variable in denselben Einheiten ist (kW/km² je €/Einwohner) - nur
# die Stichprobe aendert sich, nicht die Einheiten.
cat("Erzeuge Plot 8: Robustheitschecks (Steuerkraft ueber drei Stichproben)...\n")
robustheit_plot_df <- robustheit_tabelle %>%
  mutate(
    Stichprobe_Label = paste0(Stichprobe, "\n(N = ", N, ")"),
    Stichprobe_Label = factor(Stichprobe_Label, levels = rev(unique(Stichprobe_Label)))
  )

p8 <- ggplot(robustheit_plot_df, aes(x = Stichprobe_Label, y = Steuerkraft_Koeffizient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#dc2626", linewidth = 0.8) +
  geom_errorbar(aes(ymin = Steuerkraft_Lower, ymax = Steuerkraft_Upper),
                width = 0.15, color = "#94a3b8", linewidth = 1) +
  geom_point(color = "#0284c7", size = 4) +
  coord_flip() +
  labs(
    title = str_wrap("Robustheitscheck: Steuerkraft-Effekt über drei Stichproben", width = 55),
    subtitle = str_wrap("Derselbe Modell-Koeffizient (kW/km² je €/Einwohner), einmal auf allen Kreisen und zusätzlich auf zwei eingeschränkten Teilstichproben geschätzt.", width = 65),
    x = NULL, y = "Steuerkraft-Koeffizient (kW/km² je €/Einwohner)",
    caption = str_wrap("Der Balken kreuzt in allen drei Fällen deutlich die Null-Linie: der fehlende Steuerkraft-Effekt ist robust gegenüber der Stichprobenwahl.", width = 90)
  ) +
  theme_report()

ggsave("plots/08_robustheitscheck.png", p8, width = 8, height = 4.8, dpi = 300)

cat("\nAlle 9 ggplot2-Grafiken erfolgreich erzeugt und unter plots/ gespeichert.\n")
