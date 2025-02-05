
ui <- dashboardPage(
  dashboardHeader(
    title = "OGD Analytics",
    titleWidth = 250,
    tags$li(
      tags$img(src = "Logo_KTZH.png", height = "50px"),
      title = "KTZH",
      class = "dropdown"
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Top-10", tabName = "top_10", icon = icon("chart-bar")),
      menuItem(
        "Publisher",
        tabName = "publisher",
        icon = icon("chart-pie")
      ),
      menuItem(
        "Datensätze",
        tabName = "einzelne_daten",
        icon = icon("magnifying-glass")
      )
    )
  ),
  dashboardBody(
    use_theme(OGD_theme),
    tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$link(rel = "stylesheet", href = "fonts.css")
    ),
    tabItems(
      # Landing-Page
      tabItem(tabName = "home", fluidRow(
        box(
          title = span(
            "Nutzung des Datenangebots des Kantons Zürich",
            class = "custom-box-title",
            style = "padding-top: 10px; padding-left: 10px; display: block;"
          ),
          solidHeader = FALSE,
          width = 12,
          status = "primary",
          collapsible = FALSE,
          class = "custom-padding-box",
          p(
            HTML(
              "<strong class='custom-strong'>Offene Daten sichtbar gemacht:</strong> die OGD Analytics App bietet wertvollen Einblick in die Nutzung des Datenkatalogs des Kantons Zürich. Sie zeigt, wie oft die Daten gefunden und heruntergeladen wurden, macht Trends sichtbar und hilft, die Nutzung offener Daten besser zu verstehen.
          <a href='https://www.zh.ch/de/politik-staat/statistik-daten/datenkatalog.html#/' target='_blank' class='custom-strong' style='color: #545454; text-decoration: none;'>Zum Datenkatalog</a>"
            )
          )
        ),
        box(
          width = 12,
          class = "custom-box",
          solidHeader = TRUE,
          layout_columns(uiOutput("landingpage_boxes"))
        ),
        box(
          width = 12,
          class = "custom-padding-box",
          h4("Was wir messen"),
          tags$ul(list(
            tags$li(
              HTML(
                "<strong class='custom-strong'>Downloads: </strong> Anzahl der Downloads eines Datensatzes, entweder über einen Klick auf den Download-Button order mit direktem Aufruf des Download-Links."
              )
            ), tags$li(
              HTML(
                "<strong class='custom-strong'>Besuchende: </strong> Anzahl Besuchende eines Datensatzes im Datenkatalog des Kantons Zürich."
              )
            ), tags$li(
              HTML(
                'weitere Messwerte finden Sie im <a href="https://www.zh.ch/de/politik-staat/statistik-daten/datenkatalog.html#/datasets/2522@statistisches-amt-kanton-zuerich" target="_blank"; class="custom-strong"; style="color: #545454"; ;>offenen Datensatz</a>, der dieser App zugrunde liegt.'
              )
            )
           )
          ),
          br(),
          h4("Funktionen der App"),
          tags$ul(list(
            tags$li(tagList(
              icon("chart-bar"),
              tags$strong(class = "custom-strong", "Top-10"),
              tags$p(
                "Durchsuche die Top-10 Datensätze nach Downloads und Besuchenden über einen bestimmten Zeitraum innerhalb eines Kalenderjahres."
              )
             )
            ), 
            tags$li(tagList(
              icon("chart-pie"),
              tags$strong(class = "custom-strong", "Publisher"),
              tags$p(
                "Liste Datensätze nach Publishern und sehe deren Download- und besuchendeanzahlen über einen bestimmten Zeitraum."
              )
             )
            ), 
            tags$li(tagList(
              icon("magnifying-glass"),
              tags$strong(class = "custom-strong", "Datensätze"),
              tags$p(
                "Analysiere einzelne Datensätze, sehe Statistiken und Trends der letzten 12 Monate."
              )
             )
            )
           )
          ),
          br(),
          h4("Hinweise"),
          tags$ul(
            tags$li(
              HTML(
                "<strong class='custom-strong'>Quelle der Daten: </strong>Die Zahlen stammen aus mehreren Matomo-APIs."
              )
            ),
            tags$ul(list(tags$li(
              tags$a(
                class = "custom-strong",
                style = "color: #545454",
                href = "https://www.zh.ch/de/politik-staat/statistik-daten/datenkatalog.html#/datasets/2522@statistisches-amt-kanton-zuerich",
                target = "_blank",
                "Rohdaten: Web Analytics des Datenkatalogs des Kantons Zürich"
               )
              )
             )
            ),
            tags$li(
              HTML(
                "<strong class='custom-strong'>Einschränkungen: </strong>Nicht alle Zugriffe werden erfasst, z. B. wenn Werbeblocker (Ad-Blocker) verwendet werden."
              )
            ),
            tags$ul(list(
              tags$li("Nicht alle Interaktionen werden vollständig aufgezeichnet."),
              tags$li("Es gibt Verzögerungen bei der Datenerfassung.")
             )
            ),
            tags$li(
              HTML(
                "<strong class='custom-strong'>Nicht berücksichtigt: </strong>"
              )
            ),
            tags$ul(list(
              tags$li("Aufrufe von Datensätzen mit 0 Besuchen."),
              tags$li("Zugriffe über externe Kataloge wie opendata.swiss."),
              tags$li(
                "Downloads von extern gespeicherten Ressourcen werden unter Umständen nicht erfasst."
              ),
              tags$li("Datenpunkte mit über 10'000 Downloads pro Tag.")
            )
           )
          )
        )
      ), 
      # Footer
      fluidRow(
        tags$footer(
          style = "
          position: fixed;
          bottom: 0;
          right: 0;
          width: calc(100% - 250px);
          background-color: #0070B4;
          color: white;
          text-align: center;
          padding: 10px;
          font-family: 'InterRegular', sans-serif;
          z-index: 9999;",
          HTML("<a href='https://www.zh.ch/de/direktion-der-justiz-und-des-innern/statistisches-amt/open-government-data.html'
          target='_blank'
          class='custom-strong';  style='color: white; text-decoration: none;'>Fach- und Koordinationsstelle OGD</a>
          | Kontakt: info@open.zh.ch"
          )
         )
       )
      ),
      # Top-10 Page
      tabItem(tabName = "top_10", fluidRow(
        box(
          title = "Zeitraum Einstellung",
          solidHeader = FALSE,
          width = 12,
          # Zeitraum Einstellungen
          fluidRow(column(
            width = 6,
            selectizeInput(
              inputId = "jahr_monatlich",
              label = "Wähle Jahr:",
              choices = unique(ogd_analytics_monatlich$jahr_num),
              selected = max(ogd_analytics_monatlich$jahr_num),
              options = list(dropdownParent = 'body')
            )
          ), column(width = 6, uiOutput("monat_range_ui")))
        ),
        # Downloads Top-10 Plot und Tabelle
        box(
          title = "Downloads",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          fluidRow(column(
            width = 6, gt_output(outputId = "tabelle_downloads")
          ), column(
            width = 6, div(style = "padding-top: 5px;", plotOutput(outputId = "diagramm_downloads"))
           )
         )
        ),
        # Besuchende Top- 10 Plot und Tabelle
        box(
          title = "Besuchende",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          fluidRow(column(
            width = 6, gt_output(outputId = "tabelle_besuchende")
          ), column(
            width = 6, div(style = "padding-top: 5px;", plotOutput(outputId = "diagramm_besuchende"))
           )
          )
        )
       ) 
      ),
      # Publisher Page
      tabItem(tabName = "publisher", fluidRow(
        box(
          title = "Publisher Auswahl",
          solidHeader = TRUE,
          width = 12,
          selectInput("selected_publisher", "Wähle Publisher:", choices = sorted_publishers),
          
          # Kalender Datumsauswahl
          airDatepickerInput(
            inputId = "date_range",
            label = "Zeitraum auswählen (Gesamtzeitraum vorgewählt):",
            range = TRUE,
            dateFormat = "MM/yyyy",
            minDate = format(min(available_months), "%Y-%m-%d"),
            maxDate = format(max(available_months), "%Y-%m-%d"),
            value = c(format(min(available_months), "%Y-%m"), format(max(available_months), "%Y-%m")),
            view = "months",
            minView = "months",
            monthsField = "monthsShort",
            language = "de",
            toggleSelected = FALSE
          )
        )
      ), # Datensätze Page
      fluidRow(
        box(
          title = "Datensätze",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          DT::dataTableOutput("publisher_table")
        )
       )
      ),
      # Suche/Datensatzauswahl
      tabItem(
        tabName = "einzelne_daten",
        fluidRow(
          box(
            title = "Datensatz Auswahl",
            solidHeader = FALSE,
            width = 12,
            selectizeInput(
              "dataset_search",
              label = NULL,
              choices = ogd_analytics_monatlich$datensatz_titel |> unique() |> sort(),
              options = list(placeholder = "Suche nach Datensatz")
            ),
            helpText(
              "Um einen neuen Datensatz zu suchen, löschen Sie den aktuell gewählten, indem Sie im Suchfeld die Rücktaste (Backspace) drücken. Sortiert nach den meisten Downloads im letzten Monat.",
              class = "custom-help-text"
            )
          )
        ),
        #Titel Downloads
        fluidRow(
          conditionalPanel(
            condition = "input.dataset_search != ''",
            box(
              style = "margin-bottom: -20px;",
              title = NULL,
              width = 12,
              status = "primary",
              solidHeader = FALSE,
              collapsible = FALSE,
              uiOutput("einzelne_analyse_titel_downloads")
            )
          )
        ),
        # Downloads Info Boxen
        fluidRow(
          tags$div(
            style = "margin-bottom: 10px; margin-top: 10px; padding-left: 20px; padding-right: 20px;",
            conditionalPanel(condition = "input.dataset_search != ''", layout_columns(uiOutput("download_boxes")))
          )
        ),
        # Plot Downloads
        fluidRow(
          conditionalPanel(
            condition = "input.dataset_search != ''",
            box(
              style = "margin-bottom: -15px;" ,
              solidHeader = FALSE,
              width = 12,
              plotlyOutput("datensatz_jahr_trend_downloads")
            )
          )
        ),
        # Titel Besuchende
        fluidRow(
          conditionalPanel(
            condition = "input.dataset_search != ''",
            box(
              style = "margin-bottom: -20px; margin-top: 25px;",
              title = NULL,
              width = 12,
              status = "primary",
              solidHeader = FALSE,
              collapsible = FALSE,
              uiOutput("einzelne_analyse_titel_besuchende")
            )
          )
        ),
        # Besuchende Info Boxen
        fluidRow(
          tags$div(
            style = "margin-bottom: 10px; margin-top: 10px; padding-left: 20px; padding-right: 20px;",
            conditionalPanel(condition = "input.dataset_search != ''", layout_columns(uiOutput("besuchende_boxes")))
          )
        ),
        # Plot Besuchende
        fluidRow(
          conditionalPanel(condition = "input.dataset_search != ''", 
          box(
            solidHeader = FALSE,
            width = 12,
            plotlyOutput("datensatz_jahr_trend_besuchende")
          )
         )
        )
      )
    )
  )
)