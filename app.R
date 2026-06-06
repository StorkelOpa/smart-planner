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

# Custom ggplot dark theme matching the dashboard
theme_shiny_dark <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#1e293b", color = NA),
      panel.background = element_rect(fill = "#1e293b", color = NA),
      panel.grid.major = element_line(color = "#334155", linewidth = 0.5),
      panel.grid.minor = element_line(color = "#1e293b", linewidth = 0.2),
      text = element_text(color = "#f8fafc"),
      axis.text = element_text(color = "#cbd5e1"),
      axis.text.x = element_text(color = "#cbd5e1"),
      axis.text.y = element_text(color = "#cbd5e1"),
      axis.title = element_text(color = "#f8fafc", face = "bold"),
      legend.background = element_rect(fill = "#1e293b", color = NA),
      legend.text = element_text(color = "#cbd5e1"),
      legend.title = element_text(color = "#f8fafc", face = "bold"),
      plot.title = element_text(face = "bold", size = 15, color = "#f8fafc", margin = margin(b=10)),
      plot.subtitle = element_text(color = "#cbd5e1", size = 11, margin = margin(b=15)),
      strip.background = element_rect(fill = "#334155", color = NA),
      strip.text = element_text(color = "#f8fafc", face = "bold")
    )
}

# ==============================================================================
# UI DESIGN (BOOTSTRAP 5 VIA BSLIB)
# ==============================================================================

app_theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  bg = "#0f172a",
  fg = "#f8fafc",
  primary = "#0ea5e9", # Sky blue accent
  secondary = "#475569",
  success = "#10b981", # Emerald green
  warning = "#f59e0b",
  danger = "#ef4444",
  base_font = font_google("Outfit"),
  heading_font = font_google("Outfit")
)

ui <- page_navbar(
  theme = app_theme,
  title = "Smart Planner: Windenergie & Kommunale Finanzen",
  
  # Custom CSS for modern premium styling (glassmorphism borders, card shadows, leaflet popup adjustments)
  header = tags$head(
    tags$style(HTML("
      .navbar {
        background: linear-gradient(135deg, #1e293b 0%, #0f172a 100%) !important;
        border-bottom: 1px solid #334155;
      }
      .navbar .nav-link {
        color: #94a3b8 !important;
        font-weight: 500;
        transition: color 0.2s ease-in-out;
        padding-bottom: 6px;
      }
      .navbar .nav-link:hover {
        color: #0ea5e9 !important;
      }
      .navbar .nav-link.active {
        color: #f8fafc !important;
        border-bottom: 2px solid #0ea5e9;
      }
      .navbar-brand {
        color: #f8fafc !important;
        font-weight: bold;
      }
      .card {
        border: 1px solid #334155;
        border-radius: 12px;
        box-shadow: 0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1);
        background-color: #1e293b !important;
      }
      .card-header {
        border-bottom: 1px solid #334155 !important;
        font-weight: bold;
        color: #f8fafc;
        background-color: #1e293b !important;
      }
      .leaflet-popup-content-wrapper {
        background: #1e293b;
        color: #f8fafc;
        border: 1px solid #334155;
        border-radius: 8px;
      }
      .leaflet-popup-tip {
        background: #1e293b;
      }
      .value-box {
        border: 1px solid #334155;
      }
      .control-panel {
        background: #1e293b;
        padding: 15px;
        border-radius: 8px;
        border: 1px solid #334155;
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
      
      layout_column_wrap(
        width = 1,
        card(
          card_header("Steckbrief des ausgewählten Landkreises"),
          uiOutput("district_profile_ui")
        ),
        card(
          card_header("Top 5 strukturell ähnlichste Peer-Landkreise (k-NN)"),
          tableOutput("peer_table")
        )
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
               annotate("text", x = 1, y = 1, label = "Keine Daten für diese Filterkombination verfügbar.", size = 6, color = "white") + 
               theme_void() + 
               theme(plot.background = element_rect(fill = "#1e293b", color = NA)))
    }
    
    if (input$selected_plot == "1") {
      # Plot 1: Bivariate Scatterplot
      ggplot(data_df, aes(x = Steuerkraft, y = Wind_Density_kW_km2)) +
        geom_point(aes(color = Performance_Class), alpha = 0.8, size = 3) +
        geom_smooth(method = "lm", aes(fill = "Linear (OLS)"), color = "#38bdf8", size = 1.2, se = TRUE) +
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
        theme_shiny_dark()
        
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
        geom_violin(fill = "#0284c7", color = "#38bdf8", alpha = 0.4) +
        geom_boxplot(width = 0.2, fill = "white", color = "#0f172a", outlier.size = 2.5, outlier.color = "#ef4444") +
        labs(
          title = "Windkraftdichte nach Steuerkraft-Quartilen",
          subtitle = paste0("Gefilterte Landkreise (N = ", nrow(data_quartiles), ")"),
          x = "Steuereinnahmekraft-Klasse",
          y = "Windkraft-Kapazitätsdichte (kW / km²)"
        ) +
        theme_shiny_dark()
        
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
        theme_shiny_dark() +
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
        geom_smooth(method = "lm", color = "#38bdf8", fill = "#0284c7", alpha = 0.15, size = 1) +
        facet_wrap(~ Variable_Clean, scales = "free_x") +
        labs(
          title = "Einfluss geografischer und demografischer Kontrollvariablen",
          subtitle = paste0("Gefilterte Landkreise (N = ", nrow(data_df), ")"),
          x = "Wert der Kontrollvariable",
          y = "Windkraft-Kapazitätsdichte (kW / km²)"
        ) +
        theme_shiny_dark()
        
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
        theme_shiny_dark() +
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
        geom_smooth(method = "loess", color = "#38bdf8", se = FALSE, size = 1.2) +
        labs(
          title = "Modellüberprüfung: Residuen vs. Vorhergesagte Werte",
          subtitle = "Die rote Line zeigt die Nulllinie, die blaue Linie den LOESS-Trend der Abweichungen.",
          x = "Vorhergesagter Wert (Fitted Values)",
          y = "Residuum (Abweichung)"
        ) +
        theme_shiny_dark()
        
    } else if (input$selected_plot == "6b") {
      # Plot 6b: QQ plot
      diag_df <- data.frame(
        Std_Residuals = scale(residuals(model_raw))
      )
      ggplot(diag_df, aes(sample = Std_Residuals)) +
        stat_qq(color = "#94a3b8", alpha = 0.6) +
        stat_qq_line(color = "#38bdf8", size = 1.2) +
        labs(
          title = "Modellüberprüfung: Normal-Q-Q-Plot der Residuen",
          subtitle = "Punkte sollten auf der blauen diagonalen Referenzlinie liegen.",
          x = "Theoretische Quantile",
          y = "Standardisierte Residuen"
        ) +
        theme_shiny_dark()
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
      geom_point(color = "#38bdf8", size = 4) +
      coord_flip() +
      labs(
        x = "",
        y = "Effektstärke (in Standardabweichungen)"
      ) +
      theme_shiny_dark()
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
  
  output$district_profile_ui <- renderUI({
    req(selected_profile())
    prof <- selected_profile()
    
    # Compute percentile rank for Steuerkraft
    tax_percentile <- round(mean(districts_sf$Steuerkraft <= prof$Steuerkraft) * 100)
    # Compute percentile rank for Wind Density
    wind_percentile <- round(mean(districts_sf$Wind_Density_kW_km2 <= prof$Wind_Density_kW_km2) * 100)
    
    # Define Quartiles based on current dataset values
    cuts <- quantile(districts_sf$Steuerkraft, probs = c(0.25, 0.50, 0.75))
    
    q_info <- if (prof$Steuerkraft <= cuts[1]) {
      list(label = "Q1: Finanzschwach (untere 25%)", color = "#ef4444")
    } else if (prof$Steuerkraft <= cuts[2]) {
      list(label = "Q2: Mittel-Unter (25% - 50%)", color = "#f59e0b")
    } else if (prof$Steuerkraft <= cuts[3]) {
      list(label = "Q3: Mittel-Ober (50% - 75%)", color = "#10b981")
    } else {
      list(label = "Q4: Finanzstark (obere 25%)", color = "#0ea5e9")
    }
    
    # Color for wind percentile fill
    wind_color <- if (wind_percentile <= 25) {
      "#ef4444"
    } else if (wind_percentile <= 50) {
      "#f59e0b"
    } else if (wind_percentile <= 75) {
      "#10b981"
    } else {
      "#0ea5e9"
    }
    
    # Helper to generate visual comparison bar
    create_percentile_bar <- function(pct, fill_color) {
      div(
        style = "position: relative; height: 10px; background: #334155; border-radius: 5px; margin-top: 18px; margin-bottom: 8px; width: 100%;",
        div(
          style = sprintf("height: 100%%; width: %d%%; background: %s; border-radius: 5px; transition: width 0.5s ease-in-out;", pct, fill_color)
        ),
        # Median line marker (at 50%)
        div(
          style = "position: absolute; left: 50%; top: -3px; height: 16px; width: 2px; background: #cbd5e1; z-index: 10;"
        ),
        # Median label
        div(
          style = "position: absolute; left: 50%; top: -16px; transform: translateX(-50%); font-size: 9px; color: #cbd5e1; font-weight: bold;",
          "Median (50%)"
        )
      )
    }
    
    fluidRow(
      column(
        width = 6,
        h4(prof$Landkreis_Label, style = "color: #38bdf8; font-weight: bold; margin-bottom: 15px;"),
        p(strong("Bundesland: "), prof$Bundesland),
        p(strong("Einwohnerzahl: "), format(prof$EWZ, big.mark = ".")),
        p(strong("Fläche: "), round(prof$KFL_km2, 1), " km²"),
        p(strong("Wirtschaftsstruktur: "), prof$Economic_Structure),
        hr(style = "border-color: #334155;"),
        
        # Didactic Steuerkraft block
        p(
          strong("Steuereinnahmekraft: "), 
          format(round(prof$Steuerkraft), big.mark = "."), " € / Einwohner",
          br(),
          span(q_info$label, style = sprintf("background-color: %s; color: #0f172a; padding: 2px 8px; border-radius: 12px; font-size: 11px; font-weight: bold; margin-top: 4px; display: inline-block;", q_info$color)),
          span(sprintf(" (Reicher als %d%% aller Landkreise)", tax_percentile), style = "font-size: 12px; color: #94a3b8; margin-left: 5px;")
        ),
        create_percentile_bar(tax_percentile, q_info$color)
      ),
      column(
        width = 6,
        h5("Windenergie-Kennzahlen", style = "font-weight: bold; margin-bottom: 15px;"),
        p(strong("Installierte Windleistung: "), format(round(prof$Total_Nettoleistung_kW), big.mark = "."), " kW"),
        p(strong("Windkraftanlagen: "), prof$Turbine_Count, " Turbinen"),
        
        # Didactic Windkraft-Dichte block
        p(
          strong("Windkraft-Dichte (Ist): "), round(prof$Wind_Density_kW_km2, 2), " kW/km²",
          br(),
          span(sprintf("Mehr Ausbau als %d%% aller Landkreise", wind_percentile), style = "font-size: 12px; color: #94a3b8;")
        ),
        create_percentile_bar(wind_percentile, wind_color),
        
        hr(style = "border-color: #334155;"),
        p(strong("Windkraft-Dichte (Soll/Modell): "), round(prof$Predicted_Wind_Density, 2), " kW/km²"),
        p(strong("Statistische Abweichung: "), 
          span(paste0(ifelse(prof$Residuals > 0, "+", ""), round(prof$Residuals, 2), " kW/km²"),
               style = paste0("font-weight: bold; color: ", ifelse(prof$Residuals > 0, "#10b981", "#ef4444"), ";")),
          br(),
          strong("Performance-Klasse: "),
          span(prof$Performance_Class, 
               style = paste0("font-weight: bold; color: ", 
                              ifelse(prof$Performance_Class == "Outperformer (Hoch)", "#10b981", 
                              ifelse(prof$Performance_Class == "Underperformer (Tief)", "#ef4444", "#94a3b8")), ";"))
        )
      )
    )
  })
  
  output$peer_table <- renderTable({
    req(input$selected_district)
    
    knn_data <- districts_sf %>%
      st_drop_geometry() %>%
      select(AGS, Einwohnerdichte, Waldflaeche_Prozent, Landwirtschaft_Prozent, Beschaeftigte_Sekundar, Beschaeftigte_Primar)
    
    scaled_matrix <- scale(knn_data %>% select(-AGS))
    scaled_df <- as.data.frame(scaled_matrix)
    scaled_df$AGS <- knn_data$AGS
    
    target_values <- scaled_df %>%
      filter(AGS == input$selected_district) %>%
      select(-AGS) %>%
      as.numeric()
    
    scaled_df$Distance <- apply(scaled_df %>% select(-AGS), 1, function(row) {
      sqrt(sum((row - target_values)^2, na.rm = TRUE))
    })
    
    top_peers <- scaled_df %>%
      filter(AGS != input$selected_district) %>%
      arrange(Distance) %>%
      slice_head(n = 5) %>%
      select(AGS, Distance)
    
    peer_stats <- top_peers %>%
      left_join(districts_sf %>% st_drop_geometry(), by = "AGS") %>%
      select(
        Landkreis = Landkreis_Label,
        Bundesland,
        `Steuerkraft (€/Ew)` = Steuerkraft,
        `Nettoleistung (kW)` = Total_Nettoleistung_kW,
        `Dichte (kW/km²)` = Wind_Density_kW_km2,
        `Modell-Klasse` = Performance_Class,
        `Abstand` = Distance
      ) %>%
      mutate(
        `Steuerkraft (€/Ew)` = round(`Steuerkraft (€/Ew)`),
        `Nettoleistung (kW)` = format(`Nettoleistung (kW)`, big.mark = "."),
        `Dichte (kW/km²)` = round(`Dichte (kW/km²)`, 2),
        `Abstand` = round(`Abstand`, 3)
      )
    
    peer_stats
  }, striped = TRUE, spacing = "m", align = "l")
  
  # ----------------------------------------------------------------------------
  # RENDER TAB 4: LEAFLET MAP
  # ----------------------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
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
