# 4. Shiny Dashboard & k-NN Peer-Suche (Shiny Dashboard)

Dieses Dokument erläutert den Code und die mathematische Logik der interaktiven Shiny-App (`app.R`), insbesondere die Funktionsweise des k-NN-Ähnlichkeitsalgorithmus für den Landkreis-Vergleich.

---

## 1. Architektur der Shiny-Anwendung (`app.R`)

Die Anwendung ist als Single-File Shiny-App aufgebaut und kombiniert eine moderne Bootstrap 5 UI mit einer reaktiven Server-Logik.

### A. UI-Komponenten (User Interface)
Wir nutzen das moderne `bslib`-Paket, um ein ästhetisch anspruchsvolles Design zu erzeugen:
* **Themierung:** `bs_theme(bootswatch = "darkly", bg = "#0f172a", primary = "#0ea5e9")` sorgt für ein dunkles Premium-Layout mit blauen Akzentfarben und der modernen Schriftart "Outfit".
* **Seitenleisten (Sidebar):** Filtersteuerungen für Bundesländer und Wirtschaftsstrukturen werden kompakt links dargestellt.
* **Layouts:** `layout_column_wrap()` und `page_navbar()` sorgen für ein responsives Kachel- und Registerkarten-Layout.

### B. Server-Komponenten (Server Logic)
Der Server liest die vorbereiteten Modelldaten und Geodaten einmalig beim Starten ein und reagiert auf Nutzereingaben:
* **Reaktive Filterung:** Kartenpolygone und Diagrammpunkte werden bei Änderungen der Filter im Sidebar neu gerendert (`filtered_data <- reactive(...)`).
* **Leaflet-Proxy:** Anstatt die gesamte Karte bei jedem Layerwechsel neu zu laden (was langsam wäre), aktualisiert `leafletProxy()` flüssig nur die Füllfarben und Popups der Polygone.

---

## 2. Der k-NN Peer-Suche-Algorithmus

Die Peer-Suche ermöglicht es kommunalen Planern, ihren eigenen Landkreis mit strukturell ähnlichen Landkreisen in Deutschland zu vergleichen. Dies ist wichtig, da ein reiner Vergleich zwischen z. B. dem städtischen München und dem flachen, ländlichen Dithmarschen fachlich nicht aussagekräftig ist.

### A. Mathematische Logik der Ähnlichkeit
Um die Ähnlichkeit zwischen Landkreis $A$ und $B$ zu bestimmen, berechnen wir die **euklidische Distanz** im mehrdimensionalen Raum der Kontrollvariablen:
1. **Auswahl der Kontrollvariablen (Matching-Kriterien):**
   * Einwohnerdichte
   * Waldflächenanteil (%)
   * Landwirtschaftsflächenanteil (%)
   * Anteil Beschäftigte im sekundären Sektor (%)
   * Anteil Beschäftigte im primären Sektor (%)
2. **Standardisierung (Skalierung):**
   Die Variablen haben unterschiedliche Wertebereiche (z. B. Dichte bis zu 4000 Ew./km², primärer Beschäftigtensektor nur 0–10 %). Wenn wir die Distanz auf den Rohwerten berechnen würden, würde die Einwohnerdichte die gesamte Distanzberechnung dominieren. 
   Deshalb transformieren wir alle Matching-Variablen vorab in Z-Scores (Mittelwert 0, Standardabweichung 1).
3. **Euklidischer Abstand:**
   Der Abstand $d(A, B)$ zwischen Landkreis $A$ und $B$ berechnet sich aus den skalierten Werten $Z$:

$$d(A, B) = \sqrt{\sum_{v=1}^{5} \left( Z(A_v) - Z(B_v) \right)^2}$$

Ein Abstand von $d = 0$ bedeutet Identität; je kleiner der Abstand, desto ähnlicher sind sich die Kreise in ihren strukturellen Rahmenbedingungen.

---

## 3. R-Code-Implementierung (Server-Logic)

In der Shiny-App wird die Distanzberechnung reaktiv ausgelöst, sobald der Nutzer einen Kreis auswählt (`input$selected_district`):

```R
# Reaktiv berechnete Tabelle der 5 ähnlichsten Peers
output$peer_table <- renderTable({
  req(input$selected_district)
  
  # 1. Matching-Variablen selektieren
  knn_data <- districts_sf %>%
    st_drop_geometry() %>%
    select(AGS, Einwohnerdichte, Waldflaeche_Prozent, Landwirtschaft_Prozent, 
           Beschaeftigte_Sekundar, Beschaeftigte_Primar)
  
  # 2. Alle Daten in Z-Scores transformieren (Mittelwert = 0, SD = 1)
  scaled_matrix <- scale(knn_data %>% select(-AGS))
  scaled_df <- as.data.frame(scaled_matrix)
  scaled_df$AGS <- knn_data$AGS
  
  # 3. Z-Werte des vom Nutzer ausgewählten Landkreises extrahieren
  target_values <- scaled_df %>%
    filter(AGS == input$selected_district) %>%
    select(-AGS) %>%
    as.numeric()
  
  # 4. Euklidische Distanz für alle 399 anderen Landkreise berechnen
  # Wir nutzen apply(), um zeilenweise die Wurzel der quadrierten Differenzen zu summen
  scaled_df$Distance <- apply(scaled_df %>% select(-AGS), 1, function(row) {
    sqrt(sum((row - target_values)^2, na.rm = TRUE))
  })
  
  # 5. Top 5 ähnlichste Peers herausfiltern (den ausgewählten Kreis selbst ausschließen)
  top_peers <- scaled_df %>%
    filter(AGS != input$selected_district) %>%
    arrange(Distance) %>%
    slice_head(n = 5) %>%
    select(AGS, Distance)
  
  # 6. Statistiken der Peers dazuladen und für die Anzeige formatieren
  peer_stats <- top_peers %>%
    left_join(districts_sf %>% st_drop_geometry(), by = "AGS") %>%
    select(
      Landkreis = Landkreis_Label,
      Bundesland,
      `Steuerkraft (€/Ew)` = Steuerkraft,
      `Nettoleistung (kW)` = Total_Nettoleistung_kW,
      `Dichte (kW/km²)` = Wind_Density_kW_km2,
      `Modell-Klasse` = Performance_Class,
      `Abstand (d)` = Distance
    )
  
  peer_stats
}, striped = TRUE, spacing = "m", align = "l")
```

Dieser reaktive Code stellt sicher, dass die Distanzmatrix von $400 \times 400$ Landkreisen in Bruchteilen einer Millisekunde im RAM berechnet und die Peer-Tabelle sofort aktualisiert wird.
