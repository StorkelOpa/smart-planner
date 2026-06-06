# ==============================================================================
# SMART PLANNER: INTERACTIVE SHINY DASHBOARD
# ==============================================================================
# A premium Bootstrap 5 dashboard built with bslib and leaflet.
# It displays municipal finance power and wind energy expansion on county level,
# shows regression model diagnostics, and provides a k-NN peer search.
# Redesigned to put the main focus on professional ggplot2 analytical plots.
# ==============================================================================

library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(spdep)  # Load spdep first so that bslib::card() masks spdep::card()
library(bslib)
library(car)
library(lmtest)

# ==============================================================================
# DATA LOAD & PREPARATION
# ==============================================================================

# Federal states mapping
state_names <- c(
  "01" = "Schleswig-Holstein",
  "02" = "Hamburg",
  "03" = "Niedersachsen",
  "04" = "Bremen",
  "05" = "Nordrhein-Westfalen",
  "06" = "Hessen",
  "07" = "Rheinland-Pfalz",
  "08" = "Baden-Württemberg",
  "09" = "Bayern",
  "10" = "Saarland",
  "11" = "Berlin",
  "12" = "Brandenburg",
  "13" = "Mecklenburg-Vorpommern",
  "14" = "Sachsen",
  "15" = "Sachsen-Anhalt",
  "16" = "Thüringen"
)

# Load geopackage (includes geometry for leaflet)
cat("Loading geodata with residuals...\n")
districts_sf <- st_read("data/smart_planner_final_data_with_residuals.gpkg") %>%
  mutate(
    Bundesland = state_names[str_sub(AGS, 1, 2)],
    Landkreis_Label = paste0(GEN, " (", BEZ, ")")
  )

# Load model data
cat("Loading regression model statistics...\n")
load("data/model_results.RData")

# Load wind expansion time series (WS1) for the Kreis profile line chart.
# Contains cumulative installed capacity per county and year, plus a national
# comparison row (AGS == "DE").
cat("Loading wind expansion time series...\n")
wind_ts <- readr::read_csv(
  "data/wind_timeseries_by_county.csv",
  col_types = readr::cols(
    AGS = readr::col_character(),
    Jahr = readr::col_integer(),
    Anlagen_kumuliert = readr::col_integer(),
    Nettoleistung_kW_kumuliert = readr::col_double(),
    Wind_Density_kW_km2_kumuliert = readr::col_double()
  )
)

# Extract coefficients and confidence intervals manually to avoid broom dependency
coef_raw <- as.data.frame(summary(model_raw)$coefficients)
conf_raw <- confint(model_raw)
coef_raw$Variable <- rownames(coef_raw)
coef_raw$Lower <- conf_raw[, 1]
coef_raw$Upper <- conf_raw[, 2]
coef_raw <- coef_raw %>% filter(Variable != "(Intercept)")

coef_std <- as.data.frame(summary(model_std)$coefficients)
conf_std <- confint(model_std)
coef_std$Variable <- rownames(coef_std)
coef_std$Lower <- conf_std[, 1]
coef_std$Upper <- conf_std[, 2]
coef_std <- coef_std %>% filter(Variable != "(Intercept)")

# Custom ggplot light theme matching the dashboard (klimadashboard-style)
theme_shiny_light <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#e2e8f0", linewidth = 0.5),
      panel.grid.minor = element_line(color = "#f1f5f9", linewidth = 0.3),
      text = element_text(color = "#0f172a"),
      axis.text = element_text(color = "#475569"),
      axis.text.x = element_text(color = "#475569"),
      axis.text.y = element_text(color = "#475569"),
      axis.title = element_text(color = "#0f172a", face = "bold"),
      legend.background = element_rect(fill = "white", color = NA),
      legend.text = element_text(color = "#475569"),
      legend.title = element_text(color = "#0f172a", face = "bold"),
      plot.title = element_text(face = "bold", size = 15, color = "#0f172a", margin = margin(b=10)),
      plot.subtitle = element_text(color = "#475569", size = 11, margin = margin(b=15)),
      strip.background = element_rect(fill = "#e2e8f0", color = NA),
      strip.text = element_text(color = "#0f172a", face = "bold")
    )
}

# ==============================================================================
# UI DESIGN (BOOTSTRAP 5 VIA BSLIB)
# ==============================================================================

app_theme <- bs_theme(
  version = 5,
  bg = "#f1f5f9",       # Light slate background (klimadashboard style)
  fg = "#0f172a",       # Dark slate text
  primary = "#0284c7",  # Sky blue accent
  secondary = "#64748b",
  success = "#059669",  # Emerald green
  warning = "#d97706",
  danger = "#dc2626",
  base_font = font_google("Outfit"),
  heading_font = font_google("Outfit")
)

ui <- page_navbar(
  theme = app_theme,
  title = "Smart Planner: Windenergie & Kommunale Finanzen",
  
  # Custom CSS for modern premium styling (glassmorphism borders, card shadows, leaflet popup adjustments)
  header = tags$head(
    tags$style(HTML("
      body { background-color: #f1f5f9; }
      .navbar {
        background: #ffffff !important;
        border-bottom: 1px solid #e2e8f0;
        box-shadow: 0 1px 3px rgb(0 0 0 / 0.06);
      }
      .navbar .nav-link {
        color: #64748b !important;
        font-weight: 500;
        transition: color 0.2s ease-in-out;
        padding-bottom: 6px;
      }
      .navbar .nav-link:hover {
        color: #0284c7 !important;
      }
      .navbar .nav-link.active {
        color: #0f172a !important;
        border-bottom: 2px solid #0284c7;
      }
      .navbar-brand {
        color: #0f172a !important;
        font-weight: bold;
      }
      .card {
        border: 1px solid #e2e8f0;
        border-radius: 12px;
        box-shadow: 0 1px 3px rgb(0 0 0 / 0.06), 0 1px 2px rgb(0 0 0 / 0.04);
        background-color: #ffffff !important;
      }
      .card-header {
        border-bottom: 1px solid #e2e8f0 !important;
        font-weight: bold;
        color: #0f172a;
        background-color: #ffffff !important;
      }
      .leaflet-popup-content-wrapper {
        background: #ffffff;
        color: #0f172a;
        border: 1px solid #e2e8f0;
        border-radius: 8px;
      }
      .leaflet-popup-tip {
        background: #ffffff;
      }
      .value-box {
        border: 1px solid #e2e8f0;
      }
      /* KPI cards with comparison sliders (Kreis profile) */
      .kpi-card {
        background: #ffffff;
        border: 1px solid #e2e8f0;
        border-radius: 12px;
        padding: 16px 18px;
        height: 100%;
        box-shadow: 0 1px 3px rgb(0 0 0 / 0.06);
      }
      .kpi-label { font-size: 12px; color: #64748b; font-weight: 600; text-transform: uppercase; letter-spacing: 0.03em; }
      .kpi-value { font-size: 26px; font-weight: 700; color: #0f172a; line-height: 1.1; margin: 4px 0 2px 0; }
      .kpi-unit { font-size: 12px; color: #94a3b8; }
      .control-panel {
        background: #ffffff;
        padding: 15px;
        border-radius: 8px;
        border: 1px solid #e2e8f0;
      }
    "))
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 1: GGPLOT2 VISUALISIERUNG (NEUE HAUPTANSICHT)
  # ----------------------------------------------------------------------------
  nav_panel(
    "Daten-Visualisierung",
    icon = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Diagrammsteuerung",
        width = 300,
        selectInput(
          "selected_plot",
          "Analyse-Grafik wählen:",
          choices = c(
            "1. Steuerkraft vs. Windkraftdichte" = "1",
            "2. Windkraftdichte nach Steuerkraft-Quartilen" = "2",
            "3. Windkraftdichte nach Wirtschaftsstruktur" = "3",
            "4. Einfluss der Kontrollvariablen (Geografie)" = "4",
            "5. Die extremsten Out- & Underperformer" = "5",
            "6a. Modelldiagnostik: Residuen vs. Fitted" = "6a",
            "6b. Modelldiagnostik: Normal-Q-Q-Plot" = "6b"
          ),
          selected = "1"
        ),
        hr(),
        selectInput(
          "filter_state",
          "Bundesland filtern (für Plots 1-4):",
          choices = c("Alle Bundesländer" = "all", sort(unique(districts_sf$Bundesland)))
        ),
        selectInput(
          "filter_econ",
          "Wirtschaftsstruktur filtern (für Plots 1-4):",
          choices = c("Alle Strukturen" = "all", sort(unique(districts_sf$Economic_Structure)))
        ),
        hr(),
        markdown("
        *Hinweis:* Plots 5 und 6 stellen bundesweite statistische Modell-Diagnosen dar und sind nicht nach Bundesländern filterbar.
        ")
      ),
      
      layout_column_wrap(
        width = 1,
        card(
          card_header("Interaktives ggplot2-Diagramm"),
          plotOutput("ggplot_display", height = "550px")
        ),
        card(
          card_header("Erklärung & Interpretation für den Bericht"),
          uiOutput("ggplot_explanation_ui")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 2: MODELL-ERGEBNISSE & STATISTIK
  # ----------------------------------------------------------------------------
  nav_panel(
    "Modell-Analyse",
    icon = icon("table"),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Modell-Güte & Diagnostik"),
        layout_column_wrap(
          width = 1/3,
          value_box(
            title = "Bestimmtheitsmaß (R²)",
            value = textOutput("r_squared"),
            showcase = icon("chart-pie"),
            theme = "primary"
          ),
          value_box(
            title = "Moran's I (Räumliche Autokorrelation)",
            value = textOutput("morans_i"),
            showcase = icon("project-diagram"),
            theme = "danger"
          ),
          value_box(
            title = "Breusch-Pagan Test (Heteroskedastizität)",
            value = textOutput("bp_test_p"),
            showcase = icon("balance-scale"),
            theme = "warning"
          )
        )
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Regressionskoeffizienten (OLS) & Multikollinearität (VIF)"),
        tableOutput("coeff_table"),
        markdown("
        **Interpretation der Koeffizienten:**
        * **Ø Windgeschwindigkeit:** Mit Abstand stärkster Faktor und hochsignifikant (p < 0.001; standardisierter Effekt +0.46). Das physische Windpotenzial erklärt den Ausbau am besten.
        * **Steuereinnahmekraft:** Nach Kontrolle des Windpotenzials **nicht mehr signifikant** (p = 0.73; Effekt ≈ 0). Der zuvor sichtbare negative Zusammenhang war weitgehend ein Scheinzusammenhang (Omitted-Variable-Bias: windreiche Nordkreise sind zugleich finanzschwächer).
        * **Einwohnerdichte & Waldfläche:** Weiterhin signifikant negativ (p < 0.05) – die primären räumlichen Restriktionen.
        * **VIF:** Keine kritische Multikollinearität für Steuerkraft (VIF = 1.44) oder Windgeschwindigkeit (VIF = 1.90).
        ")
      ),
      card(
        card_header("Standardisierte Effekte (Forest-Plot)"),
        plotOutput("forestplot", height = "350px")
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 3: KREIS-PROFIL & PEER-SUCHE
  # ----------------------------------------------------------------------------
  nav_panel(
    "Kreis-Profil & Peer-Suche",
    icon = icon("search"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Kreis auswählen",
        width = 300,
        selectizeInput(
          "selected_district",
          "Landkreis / Kreisfreie Stadt:",
          choices = NULL,
          options = list(placeholder = 'Name eingeben...')
        ),
        hr(),
        markdown("
        **k-NN Peer-Suche:**
        Findet die 5 strukturell ähnlichsten Landkreise basierend auf Einwohnerdichte, Wald- & Landwirtschaftsfläche sowie Wirtschaftsstruktur. Ermöglicht einen fairen Vergleich der Windkraftraten unter gleichen Rahmenbedingungen.
        ")
      ),
      
      # --- Kreis-Profil im klimadashboard-Stil ---
      # (1) Steckbrief-Kopf mit Identität des Kreises
      card(
        card_header("Kreis-Profil"),
        uiOutput("district_header_ui")
      ),
      # (2) KPI-Karten mit Einordnungs-Slidern (Position im Bundesvergleich)
      uiOutput("kpi_cards_ui"),
      # (3) Zeitverlauf + Flächennutzung nebeneinander
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Windleistung im Zeitverlauf"),
          plotOutput("ts_plot", height = "300px")
        ),
        card(
          card_header("Flächennutzung"),
          plotOutput("landuse_donut", height = "300px")
        )
      ),
      # (4) Performance (Soll/Ist) + Einordnung im Bundesvergleich
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Performance: mehr oder weniger Ausbau als erwartet"),
          uiOutput("performance_ui")
        ),
        card(
          card_header("Einordnung im Bundesvergleich"),
          uiOutput("rank_ui")
        )
      ),
      # (5) Strukturell ähnlichste Peer-Landkreise (k-NN) mit Ähnlichkeits-Balken
      card(
        card_header("Ähnlichste strukturelle Peer-Landkreise (k-NN)"),
        uiOutput("peer_ui")
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 4: DEUTSCHLANDKARTE (LEAFLET)
  # ----------------------------------------------------------------------------
  nav_panel(
    "Deutschlandkarte",
    icon = icon("map"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Kartensteuerung",
        width = 300,
        selectInput(
          "map_layer",
          "Karten-Layer wählen:",
          choices = c(
            "Windenergie-Dichte (kW/km²)" = "Wind_Density_kW_km2",
            "Steuereinnahmekraft (€/Einwohner)" = "Steuerkraft",
            "Modell-Residuen (Z-Score)" = "Residuals_Std"
          ),
          selected = "Wind_Density_kW_km2"
        ),
        hr(),
        markdown("
        **Residuen-Farbskala:**
        * **Blau/Cyan (Positiv):** Outperformer (mehr Ausbau als statistisch erwartet).
        * **Rot/Orange (Negativ):** Underperformer (weniger Ausbau als erwartet).
        ")
      ),
      
      layout_column_wrap(
        width = 1,
        card(
          full_screen = TRUE,
          card_header("Geografische Verteilung der Variablen"),
          leafletOutput("map", height = "650px")
        )
      )
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # Populate selectize choices dynamically
  updateSelectizeInput(
    session, 
    "selected_district", 
    choices = setNames(districts_sf$AGS, districts_sf$Landkreis_Label),
    selected = "03405", # Default to Rotenburg (Wümme)
    server = TRUE
  )
  
  # Reactive data filter for Plots 1-4 & Map
  filtered_data <- reactive({
    data <- districts_sf
    
    if (input$filter_state != "all") {
      data <- data %>% filter(Bundesland == input$filter_state)
    }
    
    if (input$filter_econ != "all") {
      data <- data %>% filter(Economic_Structure == input$filter_econ)
    }
    
    data
  })
  
  # ----------------------------------------------------------------------------
  # RENDER DYNAMIC GGPLOT2 PLOTS
  # ----------------------------------------------------------------------------
  output$ggplot_display <- renderPlot({
    req(input$selected_plot)
    data_sf <- filtered_data()
    data_df <- data_sf %>% st_drop_geometry() %>% as_tibble()
    
    if (nrow(data_df) == 0) {
      return(ggplot() + 
               annotate("text", x = 1, y = 1, label = "Keine Daten für diese Filterkombination verfügbar.", size = 6, color = "#475569") +
               theme_void() +
               theme(plot.background = element_rect(fill = "white", color = NA)))
    }
    
    if (input$selected_plot == "1") {
      # Plot 1: Bivariate Scatterplot
      ggplot(data_df, aes(x = Steuerkraft, y = Wind_Density_kW_km2)) +
        geom_point(aes(color = Performance_Class), alpha = 0.8, size = 3) +
        geom_smooth(method = "lm", aes(fill = "Linear (OLS)"), color = "#0284c7", size = 1.2, se = TRUE) +
        geom_smooth(method = "loess", aes(fill = "Nicht-linear (LOESS)"), color = "#10b981", linetype = "dashed", size = 1.2, se = FALSE) +
        scale_color_manual(
          values = c("Normal" = "#64748b", "Outperformer (Hoch)" = "#10b981", "Underperformer (Tief)" = "#ef4444"),
          name = "Performance-Klasse"
        ) +
        scale_fill_manual(
          values = c("Linear (OLS)" = "#0284c7", "Nicht-linear (LOESS)" = NA),
          name = "Modellanpassung"
        ) +
        labs(
          title = "Zusammenhang zwischen Steuerkraft und Windkraftdichte",
          subtitle = paste0("Gefilterte Landkreise (N = ", nrow(data_df), ")"),
          x = "Steuereinnahmekraft (€ / Einwohner)",
          y = "Windkraft-Kapazitätsdichte (kW / km² Landfläche)"
        ) +
        theme_shiny_light()
        
    } else if (input$selected_plot == "2") {
      # Plot 2: Boxplots by Quartile
      # Calculate stable quantiles based on the FULL dataset
      full_quantiles <- quantile(districts_sf$Steuerkraft, probs = 0:4/4)
      
      data_quartiles <- data_df %>%
        mutate(
          Steuerkraft_Quartil = cut(
            Steuerkraft,
            breaks = full_quantiles,
            include.lowest = TRUE,
            labels = c("Q1 (Finanzschwach)", "Q2 (Mittel-Unter)", "Q3 (Mittel-Ober)", "Q4 (Finanzstark)")
          )
        ) %>%
        filter(!is.na(Steuerkraft_Quartil))
        
      ggplot(data_quartiles, aes(x = Steuerkraft_Quartil, y = Wind_Density_kW_km2)) +
        geom_violin(fill = "#0284c7", color = "#0284c7", alpha = 0.4) +
        geom_boxplot(width = 0.2, fill = "white", color = "#0f172a", outlier.size = 2.5, outlier.color = "#ef4444") +
        labs(
          title = "Windkraftdichte nach Steuerkraft-Quartilen",
          subtitle = paste0("Gefilterte Landkreise (N = ", nrow(data_quartiles), ")"),
          x = "Steuereinnahmekraft-Klasse",
          y = "Windkraft-Kapazitätsdichte (kW / km²)"
        ) +
        theme_shiny_light()
        
    } else if (input$selected_plot == "3") {
      # Plot 3: Economic Structure Violin Plots
      ggplot(data_df, aes(x = Economic_Structure, y = Wind_Density_kW_km2)) +
        geom_violin(aes(fill = Economic_Structure), alpha = 0.5, color = "#475569") +
        geom_boxplot(width = 0.1, fill = "white", color = "#0f172a", outlier.shape = NA) +
        scale_fill_brewer(palette = "Set2", name = "Wirtschaftsstruktur") +
        labs(
          title = "Windkraftdichte nach dominanter Wirtschaftsstruktur",
          subtitle = paste0("Gefilterte Landkreise (N = ", nrow(data_df), ")"),
          x = "Wirtschaftsstruktur-Typus",
          y = "Windkraft-Kapazitätsdichte (kW / km²)"
        ) +
        theme_shiny_light() +
        theme(legend.position = "none")
        
    } else if (input$selected_plot == "4") {
      # Plot 4: Faceted Control Variables
      df_long <- data_df %>%
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
        
      ggplot(df_long, aes(x = Wert, y = Wind_Density_kW_km2)) +
        geom_point(alpha = 0.5, color = "#94a3b8", size = 1.5) +
        geom_smooth(method = "lm", color = "#0284c7", fill = "#0284c7", alpha = 0.15, size = 1) +
        facet_wrap(~ Variable_Clean, scales = "free_x") +
        labs(
          title = "Einfluss geografischer und demografischer Kontrollvariablen",
          subtitle = paste0("Gefilterte Landkreise (N = ", nrow(data_df), ")"),
          x = "Wert der Kontrollvariable",
          y = "Windkraft-Kapazitätsdichte (kW / km²)"
        ) +
        theme_shiny_light()
        
    } else if (input$selected_plot == "5") {
      # Plot 5: Top Outliers (Residuals, National)
      outliers <- districts_sf %>%
        st_drop_geometry() %>%
        arrange(desc(Residuals))
      
      top_outperformers <- head(outliers, 10)
      top_underperformers <- tail(outliers, 10)
      
      top_both <- bind_rows(top_outperformers, top_underperformers) %>%
        mutate(
          Landkreis = reorder(paste0(GEN, " (", BEZ, ")"), Residuals)
        )
        
      ggplot(top_both, aes(x = Landkreis, y = Residuals, fill = Performance_Class)) +
        geom_col(color = "black", size = 0.2) +
        coord_flip() +
        scale_fill_manual(
          values = c("Outperformer (Hoch)" = "#10b981", "Underperformer (Tief)" = "#ef4444"),
          name = "Performance-Klasse"
        ) +
        labs(
          title = "Die 10 extremsten Outperformer und Underperformer (Bundesweit)",
          subtitle = "Größte positive und negative Abweichungen (Residuen) vom OLS-Modell",
          x = "Landkreis",
          y = "Modellabweichung / Residuum (kW / km²)"
        ) +
        theme_shiny_light() +
        theme(legend.position = "bottom")
        
    } else if (input$selected_plot == "6a") {
      # Plot 6a: Residuals vs Fitted
      diag_df <- data.frame(
        Fitted = fitted(model_raw),
        Residuals = residuals(model_raw)
      )
      ggplot(diag_df, aes(x = Fitted, y = Residuals)) +
        geom_point(color = "#94a3b8", alpha = 0.6) +
        geom_hline(yintercept = 0, color = "#ef4444", linetype = "dashed", size = 1) +
        geom_smooth(method = "loess", color = "#0284c7", se = FALSE, size = 1.2) +
        labs(
          title = "Modellüberprüfung: Residuen vs. Vorhergesagte Werte",
          subtitle = "Die rote Line zeigt die Nulllinie, die blaue Linie den LOESS-Trend der Abweichungen.",
          x = "Vorhergesagter Wert (Fitted Values)",
          y = "Residuum (Abweichung)"
        ) +
        theme_shiny_light()
        
    } else if (input$selected_plot == "6b") {
      # Plot 6b: QQ plot
      diag_df <- data.frame(
        Std_Residuals = scale(residuals(model_raw))
      )
      ggplot(diag_df, aes(sample = Std_Residuals)) +
        stat_qq(color = "#94a3b8", alpha = 0.6) +
        stat_qq_line(color = "#0284c7", size = 1.2) +
        labs(
          title = "Modellüberprüfung: Normal-Q-Q-Plot der Residuen",
          subtitle = "Punkte sollten auf der blauen diagonalen Referenzlinie liegen.",
          x = "Theoretische Quantile",
          y = "Standardisierte Residuen"
        ) +
        theme_shiny_light()
    }
  })
  
  # ----------------------------------------------------------------------------
  # EXPLANATORY TEXT GENERATION FOR REPORT
  # ----------------------------------------------------------------------------
  output$ggplot_explanation_ui <- renderUI({
    req(input$selected_plot)
    
    text_content <- switch(
      input$selected_plot,
      "1" = markdown("
      **Wissenschaftliche Interpretation:**
      * Die blaue **durchgezogene OLS-Gerade** zeigt einen **bivariaten** (unkontrollierten) negativen Zusammenhang: rein optisch bauen finanzstärkere Kommunen weniger aus.
      * **Achtung – Scheinzusammenhang:** Im multivariaten Modell (mit Windpotenzial als Kontrollvariable) ist dieser Effekt **nicht signifikant** (p = 0.73). Der bivariate Trend entsteht vor allem dadurch, dass windreiche Nordkreise zugleich finanzschwächer sind.
      * Die grüne **gestrichelte LOESS-Kurve** zeigt lokale Abweichungen vom linearen Trend.
      * *Nutzen für Ihren Bericht:* Eignet sich, um den Unterschied zwischen bivariater Korrelation und kontrolliertem Effekt herauszuarbeiten.
      "),
      "2" = markdown("
      **Wissenschaftliche Interpretation:**
      * Die Quartilsdarstellung gruppiert die Landkreise nach ihrer Finanzkraft. Die Medianwerte sinken von Q1 (finanzschwach) zu Q4 (finanzstark) – ein **bivariates** Muster.
      * Es ist robust gegenüber einzelnen Ausreißern, **erklärt sich aber großteils über das Windpotenzial**: finanzschwächere Q1-Kreise liegen häufiger in windreichen Regionen.
      * *Nutzen für Ihren Bericht:* Zeigt das Rohmuster, das im kontrollierten Modell (Modell-Tab) relativiert wird.
      "),
      "3" = markdown("
      **Wissenschaftliche Interpretation:**
      * Violin-Plots vergleichen den Ausbau über die vier klassifizierten Wirtschaftsstrukturtypen hinweg.
      * **Ländlich/Agrarische** Kreise zeigen im Median mit Abstand die höchste Windkraftdichte, während dienstleistungsorientierte Regionen den geringsten Ausbau aufweisen.
      * *Nutzen für Ihren Bericht:* Erklärt den Einfluss sektoraler Wirtschaftsstrukturen auf die Flächenverfügbarkeit und Planungseignung.
      "),
      "4" = markdown("
      **Wissenschaftliche Interpretation:**
      * Die drei Facetten stellen den isolierten Einfluss der drei wichtigsten geografischen Kontrollvariablen dar.
      * **Einwohnerdichte (log10):** Zeigt einen starken, hochsignifikanten negativen Einfluss. Ballungsräume schließen Windkraft strukturell aus.
      * **Waldanteil:** Zeigt ebenfalls einen starken negativen Zusammenhang (Restriktionen im Forst).
      * **Landwirtschaftsanteil:** Zeigt einen leicht positiven, wenn auch flacheren Trend (Offenlandschaften als Ausbauflächen).
      "),
      "5" = markdown("
      **Wissenschaftliche Interpretation:**
      * Diese Grafik zeigt Landkreise, deren Ausbau stark von der OLS-Prognose abweicht.
      * **Outperformer (Grün):** Z. B. Rotenburg (Wümme) oder Dithmarschen bauen extrem viel mehr aus, als durch ihre Finanzen/Geografie vorhergesagt wird. Hier wirken weiche Faktoren wie hohe politische Akzeptanz oder schnelle Planungsverfahren.
      * **Underperformer (Rot):** Weisen einen starken Rückstand auf.
      "),
      "6a" = markdown("
      **Wissenschaftliche Interpretation:**
      * Dieser Diagnoseplot prüft die Homoskedastizität. Die Punktewolke sollte idealerweise gleichmäßig um die rote Nulllinie streuen.
      * Der leichte Trichtereffekt bei höheren fitted values und der BP-Test ($p < 0.001$) bestätigen jedoch Heteroskedastizität, was auf ungleiche Varianzen in Starkwindregionen hinweist.
      "),
      "6b" = markdown("
      **Wissenschaftliche Interpretation:**
      * Prüft die Normalverteilungsannahme der Fehler. Die Punkte sollten perfekt auf der blauen Geraden liegen.
      * Die Abweichungen an den Enden (S-Form) zeigen 'Fat Tails' (Ausreißer), d.h. extreme Outperformer, die vom Modell unterschätzt werden.
      ")
    )
    
    div(style = "padding: 10px; font-size: 14px; line-height: 1.6;", text_content)
  })
  
  # ----------------------------------------------------------------------------
  # RENDER TAB 2: MODEL STATISTICS
  # ----------------------------------------------------------------------------
  output$r_squared <- renderText({
    paste0(round(summary(model_raw)$r.squared * 100, 1), "%")
  })
  
  output$morans_i <- renderText({
    paste0(round(moran_test$estimate[1], 3), " (p < 0.001)")
  })
  
  output$bp_test_p <- renderText({
    p_val <- bp_test$p.value
    if (p_val < 0.001) "p < 0.001" else paste0("p = ", round(p_val, 4))
  })
  
  output$forestplot <- renderPlot({
    coef_plot_data <- coef_std %>%
      mutate(
        Clean_Var = case_when(
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
    
    ggplot(coef_plot_data, aes(x = reorder(Clean_Var, Estimate), y = Estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#ef4444", size = 0.8) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "#94a3b8", size = 1) +
      geom_point(color = "#0284c7", size = 4) +
      coord_flip() +
      labs(
        x = "",
        y = "Effektstärke (in Standardabweichungen)"
      ) +
      theme_shiny_light()
  })
  
  output$coeff_table <- renderTable({
    # Reihenfolge muss der Modellformel in 05_model_regression.R entsprechen,
    # da die Koeffizienten positionsbasiert aus coef_raw/coef_std uebernommen werden.
    tibble(
      Variable = c("Steuereinnahmekraft (€/Ew.)", "Einwohnerdichte (Ew./km²)", "Ø Windgeschwindigkeit (m/s)", "Waldflächenanteil (%)", "Landwirtschaftsanteil (%)", "Industriebeschäftigte (%)", "Landwirtschaftsbeschäftigte (%)"),
      VIF = c(vif_values["Steuerkraft"], vif_values["Einwohnerdichte"], vif_values["Windgeschwindigkeit_ms"], vif_values["Waldflaeche_Prozent"], vif_values["Landwirtschaft_Prozent"], vif_values["Beschaeftigte_Sekundar"], vif_values["Beschaeftigte_Primar"]),
      `Koeffizient (Roh)` = coef_raw$Estimate,
      `p-Wert (Roh)` = coef_raw$`Pr(>|t|)`,
      `Koeffizient (Standardisiert)` = coef_std$Estimate,
      `p-Wert (Standardisiert)` = coef_std$`Pr(>|t|)`
    ) %>%
      mutate(
        across(c(VIF, `Koeffizient (Roh)`, `Koeffizient (Standardisiert)`), ~ round(., 3)),
        `p-Wert (Roh)` = ifelse(`p-Wert (Roh)` < 0.001, "< 0.001", as.character(round(`p-Wert (Roh)`, 4))),
        `p-Wert (Standardisiert)` = ifelse(`p-Wert (Standardisiert)` < 0.001, "< 0.001", as.character(round(`p-Wert (Standardisiert)`, 4)))
      )
  }, striped = TRUE, spacing = "m", align = "l")
  
  # ----------------------------------------------------------------------------
  # RENDER TAB 3: KREIS PROFILE & k-NN PEERS
  # ----------------------------------------------------------------------------
  selected_profile <- reactive({
    req(input$selected_district)
    districts_sf %>%
      filter(AGS == input$selected_district) %>%
      st_drop_geometry()
  })
  
  # German number formatter (thousands ".", decimal ",")
  fmt_de <- function(x, digits = 0) {
    formatC(round(x, digits), format = "f", digits = digits,
            big.mark = ".", decimal.mark = ",")
  }

  # --- (1) Kreis-Steckbrief-Kopf: Identität des Kreises -----------------------
  output$district_header_ui <- renderUI({
    req(selected_profile())
    prof <- selected_profile()
    div(
      style = "display:flex; flex-wrap:wrap; align-items:baseline; gap:8px 28px;",
      h3(prof$Landkreis_Label, style = "color:#0f172a; font-weight:700; margin:0;"),
      span(prof$Bundesland, style = "color:#0284c7; font-weight:600;"),
      span(paste0("Kreistyp: ", prof$BEZ), style = "color:#64748b;"),
      span(paste0("Einwohner: ", fmt_de(prof$EWZ)), style = "color:#64748b;"),
      span(paste0("Fläche: ", fmt_de(prof$KFL_km2, 0), " km²"), style = "color:#64748b;"),
      span(paste0("Wirtschaftsstruktur: ", prof$Economic_Structure), style = "color:#64748b;")
    )
  })

  # --- (2) KPI-Karten mit Einordnungs-Slider ----------------------------------
  # Jede Karte zeigt den Kreiswert UND seine Position in der bundesweiten
  # Verteilung (Min / Ø / Max + Marker), damit man die Zahl sofort einordnen kann.
  output$kpi_cards_ui <- renderUI({
    req(selected_profile())
    prof <- selected_profile()

    # Baustein: eine KPI-Karte mit Vergleichs-Slider
    kpi_card <- function(label, value_txt, unit, val, dist, digits = 0,
                         accent = "#0284c7") {
      dmin <- min(dist, na.rm = TRUE); dmax <- max(dist, na.rm = TRUE)
      dmean <- mean(dist, na.rm = TRUE)
      rng <- if (dmax > dmin) dmax - dmin else 1
      pos  <- max(0, min(100, (val - dmin) / rng * 100))
      mpos <- max(0, min(100, (dmean - dmin) / rng * 100))
      div(
        class = "kpi-card",
        div(class = "kpi-label", label),
        div(class = "kpi-value", value_txt),
        div(class = "kpi-unit", unit),
        # Slider-Spur mit Ø-Tick und Kreis-Marker
        div(
          style = "position:relative; height:26px; margin-top:14px;",
          div(style = "position:absolute; top:12px; left:0; right:0; height:6px; background:#e2e8f0; border-radius:3px;"),
          div(style = sprintf("position:absolute; top:6px; left:%.1f%%; width:2px; height:18px; background:#94a3b8;", mpos)),
          div(style = sprintf("position:absolute; top:0px; left:%.1f%%; transform:translateX(-50%%); width:0;height:0;border-left:6px solid transparent;border-right:6px solid transparent;border-top:9px solid %s;", pos, accent)),
          div(style = sprintf("position:absolute; top:9px; left:%.1f%%; transform:translateX(-50%%); width:8px;height:8px;border-radius:50%%;background:%s;", pos, accent))
        ),
        # Min / Ø / Max Beschriftung
        div(
          style = "position:relative; height:14px; font-size:10px; color:#94a3b8;",
          span(style = "position:absolute; left:0;", fmt_de(dmin, digits)),
          span(style = sprintf("position:absolute; left:%.1f%%; transform:translateX(-50%%); color:#64748b;", mpos), paste0("Ø ", fmt_de(dmean, digits))),
          span(style = "position:absolute; right:0;", fmt_de(dmax, digits))
        )
      )
    }

    cards <- list(
      kpi_card("Installierte Windleistung",
               paste0(fmt_de(prof$Total_Nettoleistung_kW / 1000), " MW"),
               paste0(fmt_de(prof$Wind_Density_kW_km2), " kW/km²"),
               prof$Total_Nettoleistung_kW / 1000,
               districts_sf$Total_Nettoleistung_kW / 1000),
      kpi_card("Steuereinnahmekraft",
               paste0(fmt_de(prof$Steuerkraft), " €"),
               "je Einwohner",
               prof$Steuerkraft, districts_sf$Steuerkraft),
      kpi_card("Ø Windgeschwindigkeit",
               paste0(fmt_de(prof$Windgeschwindigkeit_ms, 1), " m/s"),
               "auf 150 m Höhe",
               prof$Windgeschwindigkeit_ms, districts_sf$Windgeschwindigkeit_ms,
               digits = 1),
      kpi_card("Bevölkerungsdichte",
               fmt_de(prof$Einwohnerdichte),
               "Einwohner/km²",
               prof$Einwohnerdichte, districts_sf$Einwohnerdichte),
      kpi_card("Flächenanteil Landwirtschaft",
               paste0(fmt_de(prof$Landwirtschaft_Prozent), " %"),
               "der Kreisfläche",
               prof$Landwirtschaft_Prozent, districts_sf$Landwirtschaft_Prozent)
    )

    div(
      style = "display:grid; grid-template-columns:repeat(auto-fit, minmax(190px, 1fr)); gap:12px; margin:4px 0 6px 0;",
      cards
    )
  })

  # --- (3a) Zeitverlauf der installierten Windleistung ------------------------
  output$ts_plot <- renderPlot({
    req(input$selected_district)
    d <- wind_ts %>%
      filter(AGS == input$selected_district, Jahr >= 2000) %>%
      mutate(MW = Nettoleistung_kW_kumuliert / 1000)

    ggplot(d, aes(x = Jahr, y = MW)) +
      geom_area(fill = "#bae6fd", alpha = 0.5) +
      geom_line(color = "#0284c7", linewidth = 1.2) +
      geom_point(color = "#0284c7", size = 2) +
      labs(x = NULL, y = "Installierte Leistung (MW)",
           subtitle = "Kumulierter Ausbaustand zum Jahresende") +
      theme_shiny_light()
  }, bg = "white")

  # --- (3b) Flächennutzung als Donut ------------------------------------------
  output$landuse_donut <- renderPlot({
    req(selected_profile())
    prof <- selected_profile()
    kat <- c("Landwirtschaft", "Wald", "Siedlung/Verkehr", "Wasser", "Sonstige")
    val <- c(prof$Landwirtschaft_Prozent, prof$Waldflaeche_Prozent,
             prof$Siedlung_Verkehr_Prozent, prof$Wasser_Prozent, prof$Sonstige_Prozent)
    dd <- tibble(Kategorie = factor(kat, levels = kat), Anteil = val)
    cols <- c("Landwirtschaft" = "#84cc16", "Wald" = "#15803d",
              "Siedlung/Verkehr" = "#f59e0b", "Wasser" = "#0ea5e9",
              "Sonstige" = "#94a3b8")

    ggplot(dd, aes(x = 2, y = Anteil, fill = Kategorie)) +
      geom_col(width = 1, color = "white", linewidth = 0.6) +
      coord_polar(theta = "y") +
      xlim(0.4, 2.5) +
      scale_fill_manual(
        values = cols, name = NULL,
        labels = paste0(kat, "  ", fmt_de(val), " %")
      ) +
      theme_void(base_size = 13) +
      theme(
        legend.position = "right",
        legend.text = element_text(color = "#475569"),
        plot.background = element_rect(fill = "white", color = NA)
      )
  }, bg = "white")

  # --- (4a) Performance: Soll (Modell) vs. Ist (tatsächlich) ------------------
  output$performance_ui <- renderUI({
    req(selected_profile())
    prof <- selected_profile()
    soll <- prof$Predicted_Wind_Density
    ist  <- prof$Wind_Density_kW_km2
    resid <- prof$Residuals
    pos_col <- "#059669"; neg_col <- "#dc2626"
    main_col <- if (resid >= 0) pos_col else neg_col

    # Prozentuale Abweichung nur sinnvoll, wenn Soll positiv ist
    pct_txt <- if (!is.na(soll) && soll > 0) {
      sprintf("%+d %%", round((ist - soll) / soll * 100))
    } else {
      sprintf("%+s kW/km²", fmt_de(resid, 1))
    }

    # Gemeinsame Skala für die zwei Mini-Balken (negative Werte auf 0 begrenzt)
    scale_max <- max(soll, ist, 1, na.rm = TRUE)
    bar <- function(label, value, color) {
      w <- max(0, min(100, value / scale_max * 100))
      div(
        style = "margin-bottom:10px;",
        div(style = "font-size:12px; color:#64748b;", label,
            span(paste0(fmt_de(value, 1), " kW/km²"),
                 style = "float:right; color:#0f172a; font-weight:600;")),
        div(style = "height:10px; background:#e2e8f0; border-radius:5px; margin-top:4px;",
            div(style = sprintf("height:100%%; width:%.1f%%; background:%s; border-radius:5px;", w, color)))
      )
    }

    div(
      div(style = sprintf("font-size:34px; font-weight:700; color:%s; line-height:1;", main_col), pct_txt),
      div(style = "font-size:12px; color:#94a3b8; margin-bottom:14px;",
          "Abweichung vom statistisch erwarteten Ausbau"),
      bar("Erwartet (Soll, Modell)", max(soll, 0), "#94a3b8"),
      bar("Tatsächlich (Ist)", max(ist, 0), "#0284c7"),
      div(
        style = "margin-top:8px;",
        strong("Performance-Klasse: "),
        span(prof$Performance_Class,
             style = sprintf("font-weight:700; color:%s;",
               if (prof$Performance_Class == "Outperformer (Hoch)") pos_col
               else if (prof$Performance_Class == "Underperformer (Tief)") neg_col
               else "#64748b"))
      )
    )
  })

  # --- (4b) Einordnung im Bundesvergleich (Rang nach Windkraft-Dichte) --------
  output$rank_ui <- renderUI({
    req(selected_profile())
    prof <- selected_profile()
    N <- nrow(districts_sf)
    ranks <- rank(-districts_sf$Wind_Density_kW_km2, ties.method = "min")
    r <- ranks[match(prof$AGS, districts_sf$AGS)]
    # Rang 1 (höchste Dichte) -> Marker rechts
    pos <- (1 - (r - 1) / (N - 1)) * 100

    div(
      div(style = "font-size:13px; color:#64748b; margin-bottom:2px;",
          "Position nach Windkraft-Dichte (kW/km²)"),
      div(style = "font-size:30px; font-weight:700; color:#0f172a;",
          paste0("Rang ", r), span(paste0(" von ", N), style = "font-size:16px; color:#94a3b8; font-weight:500;")),
      # Band mit Marker
      div(
        style = "position:relative; height:30px; margin-top:18px;",
        div(style = "position:absolute; top:11px; left:0; right:0; height:8px; background:linear-gradient(90deg,#e2e8f0,#bae6fd); border-radius:4px;"),
        div(style = sprintf("position:absolute; top:4px; left:%.1f%%; transform:translateX(-50%%); width:0;height:0;border-left:7px solid transparent;border-right:7px solid transparent;border-top:11px solid #0284c7;", pos))
      ),
      div(style = "position:relative; height:14px; font-size:10px; color:#94a3b8;",
          span(style = "position:absolute; left:0;", paste0("Rang ", N, " (wenig)")),
          span(style = "position:absolute; right:0;", "Rang 1 (viel)"))
    )
  })

  # --- (5) k-NN Peer-Landkreise mit Ähnlichkeits-Balken -----------------------
  output$peer_ui <- renderUI({
    req(input$selected_district)

    knn_data <- districts_sf %>%
      st_drop_geometry() %>%
      select(AGS, Einwohnerdichte, Waldflaeche_Prozent, Landwirtschaft_Prozent,
             Beschaeftigte_Sekundar, Beschaeftigte_Primar)

    scaled_df <- as.data.frame(scale(knn_data %>% select(-AGS)))
    scaled_df$AGS <- knn_data$AGS

    target <- scaled_df %>% filter(AGS == input$selected_district) %>%
      select(-AGS) %>% as.numeric()
    scaled_df$Distance <- apply(scaled_df %>% select(-AGS), 1, function(row) {
      sqrt(sum((row - target)^2, na.rm = TRUE))
    })

    peers <- scaled_df %>%
      filter(AGS != input$selected_district) %>%
      arrange(Distance) %>% slice_head(n = 5) %>%
      left_join(districts_sf %>% st_drop_geometry(), by = "AGS")

    # Ähnlichkeits-Balken: kleinerer Abstand = ähnlicher = längerer Balken
    dmax <- max(peers$Distance)
    peers <- peers %>% mutate(sim_pct = (1 - Distance / (dmax * 1.15)) * 100)

    header <- tags$tr(
      lapply(c("Rang", "Landkreis", "Bundesland", "Ähnlichkeit (kleiner = ähnlicher)",
               "Dichte (kW/km²)", "Steuerkraft (€/Ew.)", "Klasse"),
             function(h) tags$th(h, style = "text-align:left; padding:8px 10px; font-size:12px; color:#64748b; border-bottom:1px solid #e2e8f0;"))
    )

    rows <- lapply(seq_len(nrow(peers)), function(i) {
      p <- peers[i, ]
      tags$tr(
        tags$td(i, style = "padding:8px 10px; color:#94a3b8; font-weight:600;"),
        tags$td(p$Landkreis_Label, style = "padding:8px 10px; color:#0f172a; font-weight:600;"),
        tags$td(p$Bundesland, style = "padding:8px 10px; color:#64748b;"),
        tags$td(
          style = "padding:8px 10px; min-width:160px;",
          div(style = "display:flex; align-items:center; gap:8px;",
              span(fmt_de(p$Distance, 2), style = "color:#475569; width:34px;"),
              div(style = "flex:1; height:8px; background:#e2e8f0; border-radius:4px;",
                  div(style = sprintf("height:100%%; width:%.1f%%; background:#0284c7; border-radius:4px;", p$sim_pct))))
        ),
        tags$td(fmt_de(p$Wind_Density_kW_km2), style = "padding:8px 10px; color:#0f172a;"),
        tags$td(fmt_de(p$Steuerkraft), style = "padding:8px 10px; color:#0f172a;"),
        tags$td(p$Performance_Class, style = "padding:8px 10px; color:#64748b; font-size:12px;")
      )
    })

    tagList(
      tags$table(style = "width:100%; border-collapse:collapse;", header, rows),
      div(style = "font-size:11px; color:#94a3b8; margin-top:10px;",
          "Ähnlichkeit basiert auf strukturellen Merkmalen (Einwohnerdichte, Wald- & Landwirtschaftsfläche, Beschäftigtenstruktur). Keine Aussage über Performance.")
    )
  })
  
  # ----------------------------------------------------------------------------
  # RENDER TAB 4: LEAFLET MAP
  # ----------------------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 10.4515, lat = 51.1657, zoom = 6)
  })
  
  observe({
    req(input$map_layer)
    data_sf <- filtered_data()
    active_col <- data_sf[[input$map_layer]]
    
    if (input$map_layer == "Residuals_Std") {
      max_abs <- max(abs(active_col), na.rm = TRUE)
      pal <- colorNumeric(
        palette = "RdYlBu", 
        domain = c(-max_abs, max_abs),
        reverse = TRUE
      )
      legend_title <- "Modell-Abweichung (Z-Score)"
    } else if (input$map_layer == "Steuerkraft") {
      pal <- colorNumeric(
        palette = "Viridis",
        domain = active_col
      )
      legend_title <- "Steuerkraft (€/Einwohner)"
    } else {
      pal <- colorNumeric(
        palette = "YlGnBu",
        domain = active_col
      )
      legend_title = "Windkraft-Dichte (kW/km²)"
    }
    
    leafletProxy("map", data = data_sf) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(active_col),
        fillOpacity = 0.75,
        color = "#334155",
        weight = 0.8,
        smoothFactor = 0.5,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#cbd5e1",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        popup = ~paste0(
          "<div style='font-size:13px; font-family: Outfit, sans-serif;'>",
          "<strong>", Landkreis_Label, "</strong> (", Bundesland, ")<br/>",
          "<hr style='margin: 5px 0; border-color: #475569;'/>",
          "Einwohner: ", format(EWZ, big.mark = "."), "<br/>",
          "Fläche: ", round(KFL_km2, 1), " km²<br/>",
          "Wirtschaftsstruktur: ", Economic_Structure, "<br/>",
          "<hr style='margin: 5px 0; border-color: #475569;'/>",
          "<strong>Windkraft-Statistik:</strong><br/>",
          "Anzahl Anlagen: ", Turbine_Count, "<br/>",
          "Netto-Windleistung: ", format(round(Total_Nettoleistung_kW), big.mark = "."), " kW<br/>",
          "Wind-Dichte: <strong>", round(Wind_Density_kW_km2, 2), " kW/km²</strong><br/>",
          "<hr style='margin: 5px 0; border-color: #475569;'/>",
          "<strong>Finanzkraft:</strong><br/>",
          "Steuerkraft: <strong>", format(round(Steuerkraft), big.mark = "."), " €/Ew.</strong><br/>",
          "<hr style='margin: 5px 0; border-color: #475569;'/>",
          "<strong>Modell-Vergleich:</strong><br/>",
          "Erwarteter Ausbau: ", round(Predicted_Wind_Density, 2), " kW/km²<br/>",
          "Abweichung: <strong>", round(Residuals, 2), " kW/km²</strong><br/>",
          "Kategorie: <span style='font-weight:bold; color:", 
            ifelse(Performance_Class == "Outperformer (Hoch)", "#10b981", 
            ifelse(Performance_Class == "Underperformer (Tief)", "#ef4444", "#cbd5e1")), 
            ";'>", Performance_Class, "</span>",
          "</div>"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = active_col,
        title = legend_title,
        opacity = 0.8
      )
  })
}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================
shinyApp(ui = ui, server = server)
