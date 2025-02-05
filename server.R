
# Funktionen für Server-Teil
source("funktionen.R", local = TRUE)

server <- function(input, output, session) {
  
  
  ########## HOME ###########
  
  # Deutsche Monatsnamen 
  deutsche_monate <- c(
    "Januar", "Februar", "März", "April", "Mai", "Juni",
    "Juli", "August", "September", "Oktober", "November", "Dezember"
  )
  
  
  # Infoboxen auf Landingpage
  
  output$landingpage_boxes <- renderUI({
    
    # Datums-Range für den letzten Monat
    erster_tag_diesen_monat <- as.Date(format(Sys.Date(), "%Y-%m-01"))
    letzter_tag_vormonat <- erster_tag_diesen_monat - days(1)
    erster_tag_vormonat <- as.Date(format(letzter_tag_vormonat, "%Y-%m-01"))
    
    # Download-Statistiken für den letzten Monat
    total_downloads_letzter_monat <- ogd_analytics |> 
      dplyr::filter(datensatz_titel != "Alle Datensätze aggregiert",
                    datum >= erster_tag_vormonat & datum <= letzter_tag_vormonat) |> 
      summarise(total_downloads = sum(anzahl_downloads)) |> 
      pull(total_downloads)
    
    # letzten Tag (vorübergehend ersetzt durch Tagesschnitt letze 7 Tage)
    # total_downloads_gestern <- ogd_analytics |> 
    #   dplyr::filter(datensatz_titel != "Alle Datensätze aggregiert",
    #                 datum == Sys.Date() - days(1)) |> 
    #   summarise(total_downloads = sum(anzahl_downloads)) |> 
    #   pull(total_downloads)
    
    
    # letzten Woche Tages-Durchschnitt
    avg_7tage_downloads <- ogd_analytics |> 
      dplyr::filter(datensatz_titel != "Alle Datensätze aggregiert",
                    datum >= Sys.Date() - days(7)) |> 
      group_by(datum) |> 
      summarise(total_downloads_tag = sum(anzahl_downloads)) |> 
      summarise(avg_downloads_7tage = round(mean(total_downloads_tag, na.rm = TRUE))) |> 
      pull(avg_downloads_7tage)
    
    
    # letzte 12 Monate
    total_downloads_letzte_12_monate <- ogd_analytics |> 
      dplyr::filter(datensatz_titel != "Alle Datensätze aggregiert",
                    datum >= (Sys.Date() - months(12))) |> 
      summarise(total_downloads = sum(anzahl_downloads)) |> 
      pull(total_downloads)
    
    # Label "Letzter Monat"
    letzter_monat_label <- paste0("im Monat: ", 
                                  deutsche_monate[month(erster_tag_vormonat)], " ", year(erster_tag_vormonat))
    
    # Label "Letzter Tag"
    letzter_tag_label <- paste0("am: ", format(Sys.Date() - days(1), "%d. "), 
                                deutsche_monate[month(Sys.Date() - days(1))], " ", year(Sys.Date() - days(1)))
    
    # Label "Letzte Woche"
    letzte_woche_label <- paste0("seit: ", format(Sys.Date() - days(7), "%d. "), 
                                deutsche_monate[month(Sys.Date() - days(7))], " ", year(Sys.Date() - days(7)))
    
    # Label für die letzten 12 Monate
    letzte_12_monate_label <- paste0("seit: ", format(Sys.Date() - months(12), "%d. "), 
                                     deutsche_monate[month(Sys.Date() - months(12))], " ", year(Sys.Date() - months(12)))
    
    # Download-Statistiken Boxen für alle Datensätze
    layout_columns(
      bslib::value_box(
        title = paste0("Downloads letzte 12 Monate"),
        value = format(total_downloads_letzte_12_monate, big.mark = "'"),
        theme = bslib::value_box_theme(bg = "#EDF5FA", fg = "#0070B4"),
        showcase = icon("calendar-alt", class = "fa-2x"),
        p(letzte_12_monate_label),
        showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE, height = NULL,
        class = "box-12-monate"
      ),
      
      bslib::value_box(
        title = paste0("Downloads letzter Monat"),
        value = format(total_downloads_letzter_monat, big.mark = "'"),
        theme = bslib::value_box_theme(bg = "#E8F3F2", fg = "#00797B"),
        showcase = icon("calendar-day", class = "fa-2x"),
        p(letzter_monat_label),
        showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE, height = NULL,
        class = "box-letzter-monat"
      ),
      
      bslib::value_box(
        title = paste0("Durchschnittliche tägliche Downloads"),
        value = format(avg_7tage_downloads, big.mark = "'"),
        theme = bslib::value_box_theme(bg = "#EBF6EB", fg = "#1A7F1F"),
        showcase = icon("calendar-check", class = "fa-2x"),
        p("der letzten Woche"),
        showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE, height = NULL,
        class = "box-gestern"
      )
    )
  })
  
  
  
  ########## TOP-10  ########## 
  
  
  # Abkürzungen auf Deutsch für Darstellung
  ger_month <- c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
  
  # UI für den Monatsrange-Slider
  output$monat_range_ui <- renderUI({
    req(input$jahr_monatlich)  

    jahr_data <- ogd_analytics_monatlich |>
      dplyr::filter(jahr_num == input$jahr_monatlich)
    
    first_month <- min(jahr_data$monat_num, na.rm = TRUE)
    last_month <- max(jahr_data$monat_num, na.rm = TRUE)
    
    shinyWidgets::sliderTextInput(
      inputId = "monat_range",
      label = "Wähle Monat(e):",
      choices = ger_month,
      selected = c(ger_month[first_month], ger_month[last_month]),  
      from_min = ger_month[first_month],
      from_max = ger_month[last_month],
      to_min = ger_month[first_month],
      to_max = ger_month[last_month],
      grid = TRUE, hide_min_max = TRUE
    )
  })
  
  
  
  # Funktion für gefilterte und zusammengefasste Daten
  summarized_data <- reactive({
    req(input$jahr_monatlich)

    # Konvertiere gewählten Monatsabkürzungen in numerische Werte
    selected_months <- match(input$monat_range, ger_month)
    
    
    # Filtere Daten basierend auf dem ausgewählten Zeitraum
    gefilterte_daten <- ogd_analytics_monatlich |>
      dplyr::filter(
        datensatz_titel != "Alle Datensätze aggregiert",
        jahr_num == input$jahr_monatlich,
        monat_num >= selected_months[1] & monat_num <= selected_months[2]
      )
    
    # Downloads und Besuchende aggregieren
    gefilterte_daten_downloads <- gefilterte_daten |>
      group_by(datensatz_titel) |>
      summarise(downloads = sum(downloads), .groups = "drop") |>
      arrange(desc(downloads)) |>
      slice_head(n = 10) |>
      mutate(top_10 = row_number())
    
    gefilterte_daten_besuchende <- gefilterte_daten |>
      group_by(datensatz_titel) |>
      summarise(besuchende = sum(besuchende), .groups = "drop") |>
      arrange(desc(besuchende)) |>
      slice_head(n = 10) |>
      mutate(top_10 = row_number())
    
    list(downloads = gefilterte_daten_downloads, besuchende = gefilterte_daten_besuchende)
  })
  
  
  # Outputs der Balkendiagramme 
  output$diagramm_downloads <- renderPlot({
    data <- summarized_data()
    if (is.null(data) || is.null(data$downloads) || nrow(data$downloads) == 0) {
      return(NULL)
    }
    
    ressource_monatlich_diagramm(data$downloads, "downloads")
  })
  
  output$diagramm_besuchende <- renderPlot({
    data <- summarized_data()
    if (is.null(data) || is.null(data$besuchende) || nrow(data$besuchende) == 0) {
      return(NULL)
    }
    
    ressource_monatlich_diagramm(data$besuchende, "besuchende")
  })
  
  # Outputs der Tabellen
  output$tabelle_downloads <- render_gt({
    data <- summarized_data()
    if (is.null(data) || is.null(data$downloads) || nrow(data$downloads) == 0) {
      return(NULL)
    }
    
    top_ten_tabelle(data$downloads, y_var = "downloads") |> 
      tab_options(table.width = pct(100))
  })
  
  output$tabelle_besuchende <- render_gt({
    data <- summarized_data()
    if (is.null(data) || is.null(data$besuchende) || nrow(data$besuchende) == 0) {
      return(NULL)
    }
    
    top_ten_tabelle(data$besuchende, y_var = "besuchende") |> 
      tab_options(table.width = pct(100))
  })
  
  
  
  ########## Publisher  ########## 
  
  # auf Basis des Zeitraums gewählte Daten filtern
  filtered_publisher_data <- reactive({
    req(input$selected_publisher)
    req(input$date_range)
    
    # Wandelt das Datum aus der Eingabe in das richtige Format um
    start_date <- as.Date(paste0(input$date_range[1], "-01"), format = "%Y-%m-%d")
    end_date <- as.Date(paste0(input$date_range[2], "-01"), format = "%Y-%m-%d")
    
    ogd_analytics_monatlich |> 
      dplyr::filter(
        datensatz_titel != "Alle Datensätze",
        publisher == input$selected_publisher,
        as.Date(paste(jahr_num, monat_num, "01", sep = "-")) >= start_date,
        as.Date(paste(jahr_num, monat_num, "01", sep = "-")) <= end_date
      ) |> 
      group_by(datensatz_titel) |> 
      summarise(downloads = sum(downloads),
                besuchende = sum(besuchende))
  })
  
  # DT Tabelle zur Darstellung
  output$publisher_table <- renderDT({
    dat_pub <- filtered_publisher_data() |> 
      arrange(desc(downloads))
    
    # Tausender-Trennzeichen vor der Anzeige formatieren
    dat_pub$besuchende <- format(dat_pub$besuchende, big.mark = "'", scientific = FALSE)
    dat_pub$downloads <- format(dat_pub$downloads, big.mark = "'", scientific = FALSE)
    
    # Spaltenreihenfolge anpassen
    dat_pub <- dat_pub %>% select(datensatz_titel, besuchende, downloads)
    
    datatable(dat_pub, 
              selection = "single", 
              colnames = c("Titel" = "datensatz_titel", "Besuchende" = "besuchende", "Downloads" = "downloads"),
              options = list(
                pageLength = 25,
                autoWidth = TRUE,
                dom = 'lfrtip',
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json')
              ))
  })
  
  
  # Interaktivität: bei Klick auf Datenstaztitel auf andere Seite wechseln
  observeEvent(input$publisher_table_rows_selected, {
    row <- input$publisher_table_rows_selected 
    if (length(row) > 0) {  
      
      selected_title <- filtered_publisher_data() %>%
        arrange(desc(downloads)) %>%  
        slice(row) %>%
        pull(datensatz_titel)  
      
      updateSelectizeInput(session, "dataset_search", 
                           choices = unique(ogd_analytics$datensatz_titel), 
                           selected = selected_title,  # Set the selected title
                           server = TRUE)
      
      updateTabItems(session, "tabs", selected = "einzelne_daten")
    }
  })
  
  
  
  
  ########## Einzelne Datensätze Analyse  ########## 
  
  
  # Auswahl für die Datensatz-Suche
  vormonat <- Sys.Date() %m-% months(1)
  
  # Filter und Aggregation für den letzten Monat
  sorted_datasets <- ogd_analytics |>
    dplyr::filter(month(datum) == month(vormonat) & year(datum) == year(vormonat)) |>
    dplyr::group_by(datensatz_titel) |>
    dplyr::summarise(anzahl_downloads = sum(anzahl_downloads), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(anzahl_downloads))
  
  # Alle Datensatz-Titel
  all_titles <- unique(ogd_analytics$datensatz_titel)
  
  # Sortiere alle Titel: Zuerst nach `sorted_datasets`, dann füge die restlichen hinzu
  sorted_choices <- c(
    sorted_datasets$datensatz_titel,        
    setdiff(all_titles, sorted_datasets$datensatz_titel) 
  )
  
  # SelectizeInput mit sortierten Titeln
  updateSelectizeInput(session, "dataset_search", 
                       choices = sorted_choices, 
                       selected = sorted_choices[1], 
                       server = F)
  
  
  # Reaktiver Ausdruck zur Berechnung der Anzahl der verfügbaren Monate
  anzahl_monate <- reactive({
    req(input$dataset_search)  # Sicherstellen, dass ein Datensatz gewählt wurde
    
    ogd_analytics_monatlich |> 
      dplyr::filter(datensatz_titel == input$dataset_search) |> 
      dplyr::filter(monatsbeginn >= (Sys.Date() - months(12))) |> 
      summarise(anzahl = n()) |> 
      pull(anzahl)
  })
  
  # Titel oberhalb der Infoboxen und Diagramme
  output$einzelne_analyse_titel_besuchende <- renderUI({
    req(input$dataset_search)
    
    # Titel für besuchende
    if (anzahl_monate() < 12) {
      HTML(paste0("<b style='font-family: \"InterBlack\"; color: #00407C; font-size: 18px; '>Besuchende der letzten ", anzahl_monate(), " Monate: '", input$dataset_search, "'</b>"))
    } else {
      HTML(paste0("<b style='font-family: \"InterBlack\"; color: #00407C; font-size: 18px; '>Besuchende der letzten 12 Monate: '", input$dataset_search, "'</b>"))
    }
  })
  
  output$einzelne_analyse_titel_downloads <- renderUI({
    req(input$dataset_search)
    
    # Titel für Downloads
    if (anzahl_monate() < 12) {
      HTML(paste0("<b style='font-family: \"InterBlack\"; color: #0070B4; font-size: 18px; '>Downloads der letzten ", anzahl_monate(), " Monate: '", input$dataset_search, "'</b>"))
    } else {
      HTML(paste0("<b style='font-family: \"InterBlack\"; color: #0070B4; font-size: 18px; '>Downloads der letzten 12 Monate: '", input$dataset_search, "'</b>"))
    }
  })
  
  
  
  # Downloads-Plot
  output$datensatz_jahr_trend_downloads <- renderPlotly({
    req(input$dataset_search)
    p <- datensatz_jahr_plotly(input$dataset_search)
    p[[1]]  
  })
  
  # Besuchende-Plot 
  output$datensatz_jahr_trend_besuchende <- renderPlotly({
    req(input$dataset_search)
    p <- datensatz_jahr_plotly(input$dataset_search)
    p[[2]]  
  })
  
  
  # Downloads-Statistiken Boxen
  output$download_boxes <- renderUI({
    req(input$dataset_search)  # Sicherstellen, dass ein Datensatz gewählt wurde
    
    stats <- datensatz_statistiken(input$dataset_search)
    
    # Startdatum (1. des ersten verfügbaren Monats) extrahieren
    startdatum <- stats$startdatum
    monat <- month(startdatum)
    jahr <- year(startdatum)
    
    # Label für den Zeitraum dynamisch generieren
    monate_label <- paste0("(seit 1. ", deutsche_monate[monat], " ", jahr, ")")
    
    # Download-Statistiken Boxen
    layout_columns(
      bslib::value_box(
        title = "Durchschnittliche Downloads Monat",
        value = format(stats$avg_monatliche_downloads, digits = 2, big.mark = "'", decimal.mark = "."),
        theme = bslib::value_box_theme(bg = "#EDF5FA", fg = "#0070B4"),
        showcase = icon("calendar", class = "fa-2x"),
        p(monate_label),  # Dynamisches Label
        showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE, height = NULL
      ),
      bslib::value_box(
        title = "Total Downloads",
        value = format(stats$total_downloads_letzte_12_monate, big.mark = "'"),
        theme = bslib::value_box_theme(bg = "#EDF5FA", fg = "#0070B4"),
        showcase = icon("calendar-alt", class = "fa-2x"),
        p(monate_label),  # Dynamisches Label
        showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE, height = NULL
      ),
      bslib::value_box(
        title = "Gesamtdownloads",
        value = format(stats$all_time_downloads, big.mark = "'"),
        theme = bslib::value_box_theme(bg = "#EDF5FA", fg = "#0070B4"),
        showcase = icon("download", class = "fa-2x"),
        p(paste0("(seit September 2023)")),  
        showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE, height = NULL
      )
    )
  })
  
  
  # Besuchende-Statistiken Boxen
  output$besuchende_boxes <- renderUI({
    req(input$dataset_search)  # Sicherstellen, dass ein Datensatz gewählt wurde
    
    stats <- datensatz_statistiken(input$dataset_search)
    
    # Startdatum (1. des ersten verfügbaren Monats) extrahieren
    startdatum <- stats$startdatum
    monat <- month(startdatum)
    jahr <- year(startdatum)
    
    # Label für den Zeitraum dynamisch generieren
    monate_label <- paste0("(seit 1. ", deutsche_monate[monat], " ", jahr, ")")
    
    # besuchende-Statistiken Boxen
    layout_columns(
      bslib::value_box(
        title = "Durchschnittliche Besuchende Monat",
        value = format(stats$avg_monatliche_besuchende, digits = 2, big.mark = "'", decimal.mark = "."),
        theme = bslib::value_box_theme(bg = "#E0E8EE", fg = "#00407C"),
        showcase = icon("user", class = "fa-2x"),
        p(monate_label),  
        showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE, height = NULL
      ),
      bslib::value_box(
        title = "Total Besuchende",
        value = format(stats$total_besuchende_letzte_12_monate, big.mark = "'"),
        theme = bslib::value_box_theme(bg = "#E0E8EE", fg = "#00407C"),
        showcase = icon("user-alt", class = "fa-2x"),
        p(monate_label), 
        showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE, height = NULL
      ),
      bslib::value_box(
        title = "Gesamtsbesuchende",
        value = format(stats$all_time_besuchende, big.mark = "'"),
        theme = bslib::value_box_theme(bg = "#E0E8EE", fg = "#00407C"),
        showcase = icon("users", class = "fa-2x"),
        p(paste0("(seit September 2023)")),  
        showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE, height = NULL
      )
    )
  })
  
  
  
  
}

