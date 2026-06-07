# ==============================================================================
# SMART PLANNER: INTERACTIVE SHINY DASHBOARD
# ==============================================================================
# A premium Bootstrap 5 dashboard built with bslib and leaflet.
# It displays municipal finance power and wind energy expansion on county level,
# shows regression model diagnostics, and provides a k-NN peer search.
# Redesigned to put the main focus on professional ggplot2 analytical plots.
# ==============================================================================

library(shiny)
library(mapgl)   # MapLibre GL vector maps (token-free) for the Kreis-Explorer
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
    Landkreis_Label = paste0(GEN, " (", BEZ, ")"),
    # Prozentuale Abweichung vom statistisch erwarteten Ausbau (für Tooltip-Satz)
    Pct_Deviation = ifelse(Predicted_Wind_Density > 0,
                           round((Wind_Density_kW_km2 - Predicted_Wind_Density) /
                                   Predicted_Wind_Density * 100),
                           NA_real_),
    # Kurzklasse für lesbare Sätze
    Klasse_kurz = case_when(
      Performance_Class == "Outperformer (Hoch)"  ~ "Outperformer",
      Performance_Class == "Underperformer (Tief)" ~ "Underperformer",
      TRUE ~ "im Soll"
    ),
    # Satz-Tooltip statt Schlüssel-Wert-Liste (Kommunikation in Klartext)
    map_tip = paste0(
      "<div style='font-family:Outfit,sans-serif; line-height:1.45;'>",
      "<div style='font-weight:700; font-size:14px;'>", Landkreis_Label, "</div>",
      "<div style='color:#64748b; font-size:12px;'>", Bundesland, "</div>",
      "<div style='margin-top:4px;'>", formatC(round(Wind_Density_kW_km2), format = "f",
        digits = 0, big.mark = ".", decimal.mark = ","), " kW/km² Windkraft-Dichte</div>",
      ifelse(!is.na(Pct_Deviation),
        paste0("<div style='font-size:12px; color:",
               ifelse(Pct_Deviation >= 0, "#059669", "#dc2626"), ";'>baut ",
               ifelse(Pct_Deviation >= 0, "+", ""), Pct_Deviation,
               " % ", ifelse(Pct_Deviation >= 0, "mehr", "weniger"),
               " aus als erwartet (", Klasse_kurz, ")</div>"),
        ""),
      "</div>"
    )
  )

# Schlanke, vereinfachte Geometrie NUR für die Karte (reduziert die an den
# Browser übertragene GeoJSON-Menge von ~20 MB auf ~3 MB). Die volle Geometrie
# in districts_sf bleibt für Analyse & Berechnungen unangetastet.
cat("Building simplified map geometry...\n")
map_sf <- districts_sf %>%
  select(AGS, Landkreis_Label, Bundesland,
         Wind_Density_kW_km2, Steuerkraft, Residuals_Std, map_tip) %>%
  st_simplify(dTolerance = 200, preserveTopology = TRUE)

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

# ------------------------------------------------------------------------------
# "VORHER / NACHHER"-KENNZAHLEN FUER DIE LEITGRAFIK (Tab 1, Plot "0")
# ------------------------------------------------------------------------------
# Kernbotschaft des Projekts in einer einzigen Grafik: Wie stark "schrumpft" der
# scheinbare Finanzkraft-Effekt, sobald das Windpotenzial ins Modell aufgenommen
# wird? Wir vergleichen STANDARDISIERTE Koeffizienten (Beta), damit die Balken
# trotz unterschiedlicher Einheiten direkt vergleichbar sind.
#
#   - "scheinbar":   bivariater Beta der Steuerkraft (entspricht der einfachen
#                    Korrelation) -- ganz ohne Kontrollvariablen.
#   - "tatsaechlich": Beta der Steuerkraft im VOLLEN Modell (model_std), in dem
#                    Windpotenzial & Geografie herausgerechnet sind.
#   - Zum Vergleich zeigen wir den Beta der Windgeschwindigkeit -- den
#     eigentlichen Treiber.
model_df_std <- districts_sf %>% st_drop_geometry() %>% as_tibble()

# Bivariater (unkontrollierter) Effekt der Steuerkraft, standardisiert.
biv_steuerkraft_std <- as.numeric(coef(
  lm(scale(Wind_Density_kW_km2) ~ scale(Steuerkraft), data = model_df_std)
)[2])

# Daten fuer die Leitgrafik. Reihenfolge = Anzeigereihenfolge von oben nach unten.
story_effects <- tibble(
  Label = c(
    "Finanzkraft\n– allein betrachtet",
    "Finanzkraft\n– Wind berücksichtigt",
    "Windgeschwindigkeit\n– der eigentliche Treiber"
  ),
  Beta = c(
    biv_steuerkraft_std,
    coef_std$Estimate[coef_std$Variable == "Steuerkraft"],
    coef_std$Estimate[coef_std$Variable == "Windgeschwindigkeit_ms"]
  ),
  Gruppe = c("scheinbar", "tatsaechlich", "treiber")
)

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
# UI HELPER-KOMPONENTEN (KOMMUNIKATIONSEBENE)
# ==============================================================================

# Hero-Banner: stellt ganz oben die Leitfrage und den Kernbefund in einem Satz
# Laiensprache dar, BEVOR irgendeine Statistik kommt (umgekehrte Pyramide).
hero_banner <- function() {
  div(
    class = "hero",
    div(class = "hero-eyebrow", "Worum geht's?"),
    div(class = "hero-question",
        "Bauen finanzschwächere Landkreise mehr Windkraft aus, um Einnahmen zu erzielen – oder entscheidet vor allem, wie viel Wind weht?"),
    div(class = "hero-finding",
        HTML("<b>Nicht das Geld entscheidet, sondern der Wind.</b> Der scheinbare Zusammenhang zwischen schwacher Finanzkraft und mehr Ausbau ist größtenteils eine Scheinkorrelation.")),
    # Nuance bewusst weggeklappt, damit der Kernbefund knapp bleibt.
    tags$details(
      class = "hero-more",
      tags$summary("Warum? Kurz erklärt"),
      HTML("Windreiche Nordkreise sind zugleich finanzschwächer. Sobald man das physische Windpotenzial im Modell berücksichtigt, verschwindet der Finanzkraft-Effekt fast vollständig – von einem standardisierten Beta von <b>−0,25</b> auf praktisch null (<b>+0,02</b>, statistisch nicht mehr signifikant). Der Wind selbst bleibt mit Abstand der stärkste Treiber.")
    )
  )
}

# Tab-Intro: ein Satz, der erklärt, was man hier sieht und worauf zu achten ist.
tab_intro <- function(text) {
  div(
    class = "tab-intro",
    span(class = "ti-icon", HTML("&#9432;")),  # ℹ-Symbol
    span(HTML(text))
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
  # Alle Tabs fließen als scrollendes Dokument, damit Cards mit ihrem Inhalt
  # wachsen und nichts intern abgeschnitten / weggescrollt wird. Die Karte im
  # Explorer bekommt stattdessen eine feste, großzügige Höhe.
  fillable = FALSE,
  
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
      /* Hero-Banner: Leitfrage + Kernbefund (oberste Kommunikationsebene) */
      .hero {
        background: #ffffff;
        border: 1px solid #e2e8f0;
        border-left: 5px solid #0284c7;
        border-radius: 12px;
        padding: 22px 26px;
        margin-bottom: 16px;
        box-shadow: 0 1px 3px rgb(0 0 0 / 0.06);
      }
      .hero-eyebrow { font-size: 12px; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; color: #0284c7; }
      .hero-question { font-size: 24px; font-weight: 700; color: #0f172a; line-height: 1.25; margin: 6px 0 14px 0; }
      .hero-finding { font-size: 15px; color: #334155; line-height: 1.55; }
      .hero-finding b { color: #059669; font-weight: 700; }
      /* Tab-Intro: ein Satz Laien-Orientierung pro Tab */
      .tab-intro {
        font-size: 14px; color: #475569; line-height: 1.5;
        background: #ffffff; border: 1px solid #e2e8f0; border-radius: 10px;
        padding: 12px 16px; margin-bottom: 14px;
        display: flex; gap: 10px; align-items: flex-start;
      }
      .tab-intro .ti-icon { color: #0284c7; font-weight: 700; font-size: 16px; line-height: 1.4; flex-shrink: 0; }
      .tab-intro b { color: #0f172a; }
      /* Modellgleichung: ruhiger, gut lesbarer Formelblock auf Tab 2 */
      .model-eq {
        background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px;
        padding: 14px 16px; margin: 4px 0 6px 0;
        font-size: 15px; line-height: 1.7; color: #0f172a;
      }
      .model-eq .eq-y { color: #0284c7; font-weight: 700; }
      .model-eq .eq-key { color: #059669; font-weight: 700; }
      .model-eq .eq-terms { color: #475569; }
      /* kurze Lese-Hilfe unter Diagramm/Tabelle */
      .read-help {
        font-size: 13px; color: #475569; line-height: 1.55;
        background: #f8fafc; border-left: 3px solid #cbd5e1; border-radius: 6px;
        padding: 10px 14px; margin-top: 12px;
      }
      .read-help b { color: #0f172a; }
      /* erklärende Mini-Zeile innerhalb der value_box-Diagnostik */
      .vb-note { font-size: 12px; opacity: 0.85; line-height: 1.4; margin-top: 4px; }
      /* Hero: ausklappbare Nuance ('Mehr dazu') */
      .hero-more { margin-top: 12px; }
      .hero-more summary { cursor: pointer; font-size: 13px; font-weight: 600; color: #0284c7; list-style: none; width: fit-content; }
      .hero-more summary::-webkit-details-marker { display: none; }
      .hero-more summary::before { content: '▸ '; }
      .hero-more[open] summary::before { content: '▾ '; }
      .hero-more > :not(summary) { font-size: 14px; color: #475569; line-height: 1.55; margin-top: 8px; }
      .hero-more b { color: #0f172a; font-weight: 700; }
    "))
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 1: GGPLOT2 VISUALISIERUNG (NEUE HAUPTANSICHT)
  # ----------------------------------------------------------------------------
  nav_panel(
    "Daten-Visualisierung",
    icon = icon("chart-bar"),
    hero_banner(),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filter",
        width = 300,
        markdown("Die Filter wirken auf die **erste Grafik** (Rohdaten-Streudiagramm) – z. B. um einzelne Bundesländer zu vergleichen."),
        selectInput(
          "filter_state",
          "Bundesland:",
          choices = c("Alle Bundesländer" = "all", sort(unique(districts_sf$Bundesland)))
        ),
        selectInput(
          "filter_econ",
          "Wirtschaftsstruktur:",
          choices = c("Alle Strukturen" = "all", sort(unique(districts_sf$Economic_Structure)))
        ),
        hr(),
        markdown("
        *Hinweis:* Die zweite Grafik (Auflösung) ist eine bundesweite Modell-Auswertung und daher nicht filterbar.
        ")
      ),

      # Tab 1 erzählt nur die Kerngeschichte in zwei Schritten und beantwortet
      # damit direkt die Forschungsfrage (Finanzkraft -> Windausbau?):
      #   (1) der scheinbare Zusammenhang in den Rohdaten,
      #   (2) seine Auflösung, sobald das Windpotenzial kontrolliert wird.
      # Technische Plots (Diagnostik, Ausreißer) leben nur im Projektbericht.
      layout_column_wrap(
        width = 1,
        card(
          card_header("① Der scheinbare Zusammenhang – was die Rohdaten nahelegen"),
          plotOutput("plot_scatter", height = "480px"),
          div(
            class = "read-help",
            HTML("<b>So lesen Sie diese Grafik:</b> Jeder Punkt ist ein Landkreis. Die blaue Gerade zeigt den <i>unkontrollierten</i> Trend: rein optisch bauen finanzstärkere Kreise <b>weniger</b> Windkraft aus. Das ist der scheinbare Zusammenhang – aber ist es wirklich das Geld? Die nächste Grafik prüft das.")
          )
        ),
        card(
          card_header("② Die Auflösung – Wind statt Geld"),
          plotOutput("plot_story", height = "440px"),
          div(
            class = "read-help",
            HTML("<b>So lesen Sie diese Grafik:</b> Sobald das physische Windpotenzial im Modell berücksichtigt wird, schrumpft der Finanzkraft-Effekt von <b>−0,25</b> auf praktisch <b>null</b> (+0,02, statistisch nicht signifikant). Der scheinbare Zusammenhang war also größtenteils eine <b>Scheinkorrelation</b>: windreiche Nordkreise sind zugleich finanzschwächer. Der eigentliche Treiber ist die <b>Windgeschwindigkeit</b> (+0,46) – Details im Tab „Modell-Analyse“.")
          )
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
    tab_intro("<b>Das statistische Modell trennt echte von scheinbaren Einflüssen.</b> Sie sehen zuerst die Ergebnisse – die ausführliche Erklärung der Methode steht direkt darunter."),

    # ---- VISUELLES ZUERST: Kennzahlen, Tabelle, Forest-Plot ------------------
    layout_column_wrap(
      width = 1,
      card(
        card_header("Modell-Güte & Diagnostik"),
        layout_column_wrap(
          width = 1/3,
          value_box(
            title = "Bestimmtheitsmaß (R²)",
            value = textOutput("r_squared"),
            showcase = icon("chart-pie", style = "color:#0284c7;"),
            theme = value_box_theme(bg = "#ffffff", fg = "#0f172a"),
            p(class = "vb-note", "Anteil der Streuung im Windausbau, den das Modell erklärt. Höher = besser.")
          ),
          value_box(
            title = "Moran's I (Räumliche Autokorrelation)",
            value = textOutput("morans_i"),
            showcase = icon("project-diagram", style = "color:#dc2626;"),
            theme = value_box_theme(bg = "#ffffff", fg = "#0f172a"),
            p(class = "vb-note", "Klumpen sich die Modellfehler räumlich? Nahe 0 = unauffällig; hier leicht erhöht (Nachbarkreise ähneln sich).")
          ),
          value_box(
            title = "Breusch-Pagan Test (Heteroskedastizität)",
            value = textOutput("bp_test_p"),
            showcase = icon("balance-scale", style = "color:#d97706;"),
            theme = value_box_theme(bg = "#ffffff", fg = "#0f172a"),
            p(class = "vb-note", "Streuen die Fehler ungleichmäßig? p < 0,05 = ja – Effekte stimmen, Standardfehler mit Vorsicht lesen.")
          )
        )
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Regressionskoeffizienten (OLS) & Multikollinearität (VIF)"),
        tableOutput("coeff_table"),
        div(
          class = "read-help",
          HTML("<b>Spalten:</b> <b>VIF</b> prüft Multikollinearität (Variablen, die sich gegenseitig „doppeln“); < 5 ist unkritisch. <b>Koeffizient (Roh)</b> = Effekt in echten Einheiten, <b>(Standardisiert)</b> = vergleichbar skaliert. Der <b>p-Wert</b> zeigt die statistische Belastbarkeit (< 0,05 = signifikant).")
        ),
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
        plotOutput("forestplot", height = "350px"),
        div(
          class = "read-help",
          HTML("<b>So lesen Sie den Forest-Plot:</b> Jeder Punkt ist der <i>standardisierte</i> Effekt einer Variable, die waagerechte Linie ihr 95%-Konfidenzintervall. Je weiter ein Punkt von der gestrichelten <b>Null-Linie</b> entfernt liegt, desto stärker der Effekt. <b>Kreuzt</b> das Intervall die Null-Linie, ist der Effekt statistisch <b>nicht</b> von Null zu unterscheiden – so liegt die Finanzkraft praktisch auf Null, während die Windgeschwindigkeit klar rechts heraussticht.")
        )
      )
    ),

    # ---- ERKLÄRUNG DANACH: Was macht dieses Modell überhaupt? ----------------
    card(
      card_header("Was macht dieses Modell? – Multiple lineare Regression"),
      card_body(
        markdown("
**Das Problem.** Auf der ersten Seite sah es so aus, als bauten finanzschwächere Kreise mehr Windkraft aus. Aber viele Dinge wirken *gleichzeitig*: Wind, Fläche, Besiedlung, Wirtschaftsstruktur. Eine einfache Korrelation vermischt all das.

**Die Methode.** Eine **multiple lineare Regression** schätzt den Effekt *jeder* Einflussgröße, **während alle anderen rechnerisch konstant gehalten werden**. So lässt sich der eigenständige Beitrag der Finanzkraft vom Effekt des Windpotenzials sauber trennen – genau das, was die Forschungsfrage verlangt.
        "),
        div(
          class = "model-eq",
          HTML(
            "<span class='eq-y'>Windleistung (kW/km²)</span> = β₀ <span class='eq-key'>+ β₁·Finanzkraft</span> <span class='eq-key'>+ β₂·Windgeschwindigkeit</span> <span class='eq-terms'>+ β₃·Einwohnerdichte + β₄·Waldanteil + β₅·Landwirtschaftsanteil + β₆·Industriebeschäftigte + β₇·Agrarbeschäftigte</span> + ε"
          )
        ),
        markdown("
Jeder Koeffizient **β** beantwortet: *Wie stark ändert sich der Ausbau, wenn diese eine Größe steigt – bei allen anderen gleich?* Die beiden Hauptgrößen sind farblich hervorgehoben: <span style='color:#0284c7'>**Finanzkraft**</span> (die Forschungsfrage) und die wichtigste Kontrolle, das <span style='color:#059669'>**Windpotenzial**</span>.

**So lesen Sie die Ergebnisse oben:**
* **Koeffizient (Roh)** = Effekt in echten Einheiten. **Standardisiert (β)** = alle Größen auf eine gemeinsame Skala gebracht, damit die Effekte *direkt vergleichbar* sind (das zeigt der Forest-Plot).
* **p-Wert** = Wie wahrscheinlich wäre dieser Effekt reiner Zufall? **p < 0,05** gilt als statistisch belastbar.
* **R²**, **Moran's I**, **Breusch-Pagan** und **VIF** prüfen die Güte und Annahmen des Modells.
        ")
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 3: KREIS-EXPLORER (KARTE IM ZENTRUM, PROFIL & PEERS VERKNÜPFT)
  # ----------------------------------------------------------------------------
  # Map-centric Single-Screen nach Vorbild des "Commute Explorer": Die Karte ist
  # das Interface. Ein Klick auf einen Kreis wählt ihn aus -> Profil (links) und
  # Peers (rechts) aktualisieren sich, und die Karte hebt Auswahl + Struktur-
  # Zwillinge hervor (Brushing & Linking).
  nav_panel(
    "Kreis-Explorer",
    icon = icon("map-location-dot"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Kreis auswählen",
        width = 300,
        selectizeInput(
          "selected_district",
          "Suche per Name:",
          choices = NULL,
          options = list(placeholder = 'Name eingeben...')
        ),
        markdown("*… oder klicken Sie einen Kreis direkt auf der Karte an.*"),
        hr(),
        markdown("
        **Kartenfarbe (Choroplethen):**
        * **Wind-Dichte / Steuerkraft:** dunkler = höher.
        * **Modell-Abweichung:** <span style='color:#0284c7'>**blau**</span> = mehr Ausbau als erwartet (Outperformer), <span style='color:#dc2626'>**rot**</span> = weniger (Underperformer).

        **Hervorhebung:**
        * **Schwarze Umrandung:** ausgewählter Kreis.
        * **Blaue Umrandung:** die 5 strukturellen Zwillinge.
        "),
        hr(),
        markdown("
        **k-NN Peer-Suche:** Findet die 5 strukturell ähnlichsten Kreise (Einwohnerdichte, Wald- & Landwirtschaftsfläche, Beschäftigtenstruktur) – für einen fairen Vergleich der Windkraftraten unter gleichen Rahmenbedingungen.
        ")
      ),

      tab_intro("<b>Erkunden Sie die Landkarte.</b> Klicken Sie einen Kreis an (oder suchen Sie ihn links). Die Karte färbt sich nach der gewählten Kennzahl, hebt den Kreis und seine 5 Struktur-Zwillinge hervor – Profil und Peer-Vergleich daneben aktualisieren sich sofort."),

      # (1) Steckbrief-Kopf (volle Breite) – Identität des ausgewählten Kreises
      card(
        uiOutput("district_header_ui")
      ),

      # (2) Drei-Spalten-Layout: Profil | KARTE (Zentrum) | Peers
      layout_columns(
        col_widths = c(3, 6, 3),
        # --- Spalte links: KPIs + Performance + Rang ---
        div(
          uiOutput("kpi_cards_ui"),
          card(
            card_header("Mehr oder weniger Ausbau als erwartet?"),
            uiOutput("performance_ui")
          ),
          card(
            card_header("Rang im Bundesvergleich"),
            uiOutput("rank_ui")
          )
        ),
        # --- Spalte Mitte: die Karte als Herzstück + Zeitverlauf ---
        div(
          card(
            card_header(
              div(
                style = "display:flex; flex-wrap:wrap; align-items:center; justify-content:space-between; gap:8px;",
                span("Landkarte – Kreis anklicken"),
                div(
                  style = "font-weight:400;",
                  radioButtons(
                    "map_metric", NULL,
                    choices = c(
                      "Wind-Dichte" = "Wind_Density_kW_km2",
                      "Steuerkraft" = "Steuerkraft",
                      "Modell-Abweichung" = "Residuals_Std"
                    ),
                    selected = "Wind_Density_kW_km2",
                    inline = TRUE
                  )
                )
              )
            ),
            maplibreOutput("expmap", height = "600px")
          ),
          card(
            card_header("Windleistung im Zeitverlauf"),
            plotOutput("ts_plot", height = "260px")
          )
        ),
        # --- Spalte rechts: Peers + Flächennutzung ---
        div(
          card(
            card_header("Strukturelle Zwillinge (k-NN)"),
            uiOutput("peer_ui")
          ),
          card(
            card_header("Flächennutzung"),
            plotOutput("landuse_donut", height = "260px")
          )
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

  # --- Zentraler Auswahl-State (geteilt zwischen Such-Feld und Kartenklick) ---
  # reactiveVal benachrichtigt nur bei tatsächlicher Wertänderung (identical-
  # Check), daher entsteht beim Rück-Synchronisieren keine Endlosschleife.
  sel <- reactiveVal("03405")

  # Suche -> State
  observeEvent(input$selected_district, {
    req(input$selected_district)
    sel(input$selected_district)
  })

  # Kartenklick -> State (+ Suchfeld nachziehen)
  observeEvent(input$expmap_feature_click, {
    ags <- input$expmap_feature_click$properties$AGS
    req(!is.null(ags))
    sel(ags)
    updateSelectizeInput(session, "selected_district", selected = ags)
  })


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
  # RENDER TAB 1: ZWEI-SCHRITT-GESCHICHTE (Scatter -> Vorher/Nachher)
  # ----------------------------------------------------------------------------

  # (1) Scheinbarer Zusammenhang: bivariater Scatter Finanzkraft <-> Winddichte.
  # Reagiert auf die Bundesland-/Struktur-Filter (filtered_data()).
  output$plot_scatter <- renderPlot({
    data_df <- filtered_data() %>% st_drop_geometry() %>% as_tibble()

    if (nrow(data_df) == 0) {
      return(ggplot() +
               annotate("text", x = 1, y = 1, label = "Keine Daten für diese Filterkombination verfügbar.", size = 6, color = "#475569") +
               theme_void() +
               theme(plot.background = element_rect(fill = "white", color = NA)))
    }

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
  })

  # (2) Auflösung: "Vorher/Nachher"-Effektstärken (bundesweit, nicht filterbar).
  # Effektstärke (|Beta|) als Balkenlänge, vorzeichenbehafteter Beta als Label.
  # Reihenfolge oben->unten erhalten: Faktorlevel umkehren, da coord_flip spiegelt.
  output$plot_story <- renderPlot({
    story_plot_df <- story_effects %>%
      mutate(
        Strength = abs(Beta),
        Label = factor(Label, levels = rev(Label))
      )

    ggplot(story_plot_df, aes(x = Label, y = Strength, fill = Gruppe)) +
      geom_col(width = 0.62) +
      geom_text(
        aes(label = sprintf("%+.2f", Beta)),
        hjust = -0.2, size = 5, fontface = "bold", color = "#0f172a"
      ) +
      coord_flip() +
      scale_fill_manual(
        values = c("scheinbar" = "#94a3b8", "tatsaechlich" = "#cbd5e1", "treiber" = "#10b981"),
        guide = "none"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
      labs(
        title = "Die zentrale Erkenntnis: Der Geld-Effekt verschwindet",
        subtitle = "Standardisierte Effektstärke (Beta) auf die Windkraftdichte – je länger der Balken, desto stärker der Einfluss",
        x = NULL,
        y = "Effektstärke (standardisierter Beta-Koeffizient)"
      ) +
      theme_shiny_light() +
      theme(panel.grid.major.y = element_blank())
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
    req(sel())
    districts_sf %>%
      filter(AGS == sel()) %>%
      st_drop_geometry()
  })

  # k-NN Peers als gemeinsame Reactive (genutzt von Tabelle UND Kartenmarkierung)
  peers_data <- reactive({
    req(sel())
    knn_data <- districts_sf %>%
      st_drop_geometry() %>%
      select(AGS, Einwohnerdichte, Waldflaeche_Prozent, Landwirtschaft_Prozent,
             Beschaeftigte_Sekundar, Beschaeftigte_Primar)

    scaled_df <- as.data.frame(scale(knn_data %>% select(-AGS)))
    scaled_df$AGS <- knn_data$AGS

    target <- scaled_df %>% filter(AGS == sel()) %>%
      select(-AGS) %>% as.numeric()
    scaled_df$Distance <- apply(scaled_df %>% select(-AGS), 1, function(row) {
      sqrt(sum((row - target)^2, na.rm = TRUE))
    })

    scaled_df %>%
      filter(AGS != sel()) %>%
      arrange(Distance) %>% slice_head(n = 5) %>%
      left_join(districts_sf %>% st_drop_geometry(), by = "AGS")
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

    # Ein Metadaten-Chip: kleines Label oben, Wert darunter (skaliert sauber,
    # bricht bei schmalen Breiten um statt zu überlappen).
    meta_chip <- function(label, value) {
      div(
        style = "min-width:0;",
        div(label, style = "font-size:11px; color:#94a3b8; text-transform:uppercase; letter-spacing:0.03em; font-weight:600; white-space:nowrap;"),
        div(value, style = "font-size:15px; color:#0f172a; font-weight:600;")
      )
    }

    div(
      # Zeile 1: Titel + Bundesland-Badge (eigene Zeile, kein Baseline-Mix mehr)
      div(
        style = "display:flex; align-items:center; flex-wrap:wrap; gap:12px;",
        tags$h2(prof$Landkreis_Label,
                style = "color:#0f172a; font-weight:700; margin:0; font-size:28px; line-height:1.2;"),
        span(prof$Bundesland,
             style = "background:#e0f2fe; color:#0284c7; font-weight:600; font-size:13px; padding:4px 12px; border-radius:999px;")
      ),
      # Zeile 2: Metadaten als Chips, durch Trennlinie abgesetzt
      div(
        style = "display:flex; flex-wrap:wrap; gap:14px 40px; margin-top:16px; padding-top:14px; border-top:1px solid #e2e8f0;",
        meta_chip("Kreistyp", prof$BEZ),
        meta_chip("Einwohner", fmt_de(prof$EWZ)),
        meta_chip("Fläche", paste0(fmt_de(prof$KFL_km2, 0), " km²")),
        meta_chip("Wirtschaftsstruktur", prof$Economic_Structure)
      )
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
    req(sel())
    d <- wind_ts %>%
      filter(AGS == sel(), Jahr >= 2000) %>%
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
  # Schmale Spalte rechts: kompakte Kartenliste statt breiter Tabelle, damit in
  # der engen Spalte nichts abgeschnitten wird. Klick auf einen Peer wählt ihn.
  output$peer_ui <- renderUI({
    peers <- peers_data()
    req(nrow(peers) > 0)

    # Ähnlichkeits-Balken: kleinerer Abstand = ähnlicher = längerer Balken
    dmax <- max(peers$Distance)
    peers <- peers %>% mutate(sim_pct = (1 - Distance / (dmax * 1.15)) * 100)

    perf_col <- function(cls) {
      if (cls == "Outperformer (Hoch)") "#059669"
      else if (cls == "Underperformer (Tief)") "#dc2626"
      else "#64748b"
    }

    items <- lapply(seq_len(nrow(peers)), function(i) {
      p <- peers[i, ]
      div(
        style = "padding:10px 0; border-bottom:1px solid #f1f5f9;",
        div(
          style = "display:flex; justify-content:space-between; gap:8px;",
          span(paste0(i, ". ", p$Landkreis_Label),
               style = "font-weight:600; color:#0f172a; font-size:13px;"),
          span(p$Klasse_kurz, style = sprintf("font-size:11px; font-weight:600; color:%s; white-space:nowrap;", perf_col(p$Performance_Class)))
        ),
        div(p$Bundesland, style = "font-size:11px; color:#94a3b8;"),
        # Ähnlichkeits-Balken
        div(style = "height:6px; background:#e2e8f0; border-radius:3px; margin:6px 0 4px 0;",
            div(style = sprintf("height:100%%; width:%.1f%%; background:#0284c7; border-radius:3px;", p$sim_pct))),
        div(
          style = "display:flex; justify-content:space-between; font-size:11px; color:#64748b;",
          span(paste0(fmt_de(p$Wind_Density_kW_km2), " kW/km²")),
          span(paste0(fmt_de(p$Steuerkraft), " €/Ew."))
        )
      )
    })

    tagList(
      items,
      div(style = "font-size:11px; color:#94a3b8; margin-top:10px;",
          "Ähnlichkeit nach Struktur (Dichte, Wald-/Landwirtschaftsfläche, Beschäftigte) – sie sind blau auf der Karte umrandet. Keine Aussage über Performance.")
    )
  })
  
  # ----------------------------------------------------------------------------
  # RENDER TAB 3: MAPLIBRE EXPLORER-KARTE (Choroplethe + Auswahl/Peer-Highlights)
  # ----------------------------------------------------------------------------

  # Farbskala + Legenden-Angaben je nach gewählter Kennzahl
  metric_spec <- function(metric) {
    vals <- districts_sf[[metric]]
    if (metric == "Residuals_Std") {
      m <- max(abs(vals), na.rm = TRUE)
      list(
        fill = interpolate(column = metric, values = c(-m, 0, m),
                           stops = c("#dc2626", "#f1f5f9", "#0284c7"), na_color = "#e2e8f0"),
        legend_title = "Modell-Abweichung (weniger ← 0 → mehr)",
        legend_vals = c(fmt_de(-m, 1), "0", fmt_de(m, 1)),
        legend_cols = c("#dc2626", "#f1f5f9", "#0284c7")
      )
    } else if (metric == "Steuerkraft") {
      rng <- range(vals, na.rm = TRUE)
      list(
        fill = interpolate(column = metric, values = rng,
                           stops = c("#fde725", "#440154"), na_color = "#e2e8f0"),
        legend_title = "Steuerkraft (€/Ew.)",
        legend_vals = c(fmt_de(rng[1]), fmt_de(mean(rng)), fmt_de(rng[2])),
        legend_cols = c("#fde725", "#90548b", "#440154")
      )
    } else {
      rng <- range(vals, na.rm = TRUE)
      list(
        fill = interpolate(column = metric, values = rng,
                           stops = c("#edf8fb", "#0868ac"), na_color = "#e2e8f0"),
        legend_title = "Windkraft-Dichte (kW/km²)",
        legend_vals = c(fmt_de(rng[1]), fmt_de(mean(rng)), fmt_de(rng[2])),
        legend_cols = c("#edf8fb", "#4292c6", "#0868ac")
      )
    }
  }

  # Karte EINMAL rendern (Start: Wind-Dichte). Metrik-Wechsel ändert danach nur
  # die Füllfarbe per Proxy, damit der Zoom/die Auswahl erhalten bleiben.
  output$expmap <- renderMaplibre({
    spec <- metric_spec("Wind_Density_kW_km2")
    cur_sel   <- isolate(sel())
    cur_peers <- isolate(peers_data()$AGS)

    maplibre(style = carto_style("positron"),
             center = c(10.45, 51.16), zoom = 4.6) |>
      add_fill_layer(
        id = "kreise",
        source = map_sf,
        fill_color = spec$fill,
        fill_opacity = 0.8,
        fill_outline_color = "#ffffff",
        tooltip = "map_tip",
        hover_options = list(fill_opacity = 1)
      ) |>
      add_line_layer(
        id = "peers_hl", source = map_sf,
        line_color = "#0284c7", line_width = 2.5,
        filter = list("in", list("get", "AGS"), list("literal", as.list(cur_peers)))
      ) |>
      add_line_layer(
        id = "selected_hl", source = map_sf,
        line_color = "#0f172a", line_width = 3.5,
        filter = list("==", list("get", "AGS"), cur_sel)
      ) |>
      add_continuous_legend(
        legend_title = spec$legend_title,
        values = spec$legend_vals,
        colors = spec$legend_cols,
        position = "bottom-right", unique_id = "map_legend"
      )
  })

  # Metrik-Wechsel: nur Füllfarbe + Legende per Proxy aktualisieren (kein Re-Render)
  observeEvent(input$map_metric, {
    spec <- metric_spec(input$map_metric)
    maplibre_proxy("expmap") |>
      set_paint_property("kreise", "fill-color", spec$fill) |>
      clear_legend(legend_ids = "map_legend") |>
      add_continuous_legend(
        legend_title = spec$legend_title,
        values = spec$legend_vals,
        colors = spec$legend_cols,
        position = "bottom-right", unique_id = "map_legend"
      )
  }, ignoreInit = TRUE)

  # Auswahl + Peers auf der Karte aktualisieren (Umrandungen via Filter)
  observe({
    cur_sel <- sel()
    req(cur_sel)
    cur_peers <- peers_data()$AGS
    maplibre_proxy("expmap") |>
      set_filter("selected_hl", list("==", list("get", "AGS"), cur_sel)) |>
      set_filter("peers_hl",
                 list("in", list("get", "AGS"), list("literal", as.list(cur_peers))))
  })

  # Karte sanft auf den ausgewählten Kreis + seine Peers zoomen
  observeEvent(list(sel(), peers_data()), {
    req(sel())
    focus_sf <- map_sf %>% filter(AGS %in% c(sel(), peers_data()$AGS))
    req(nrow(focus_sf) > 0)
    maplibre_proxy("expmap") |>
      fit_bounds(focus_sf, animate = TRUE)
  })
}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================
shinyApp(ui = ui, server = server)
