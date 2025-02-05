# Die folgenden Funktionen werden im Server-Teil verwendet, strukturiert nach den 3 Pages

################## Top-10 ###################

#' Erstellt ein Balkendiagramm der Top-10-Datensätze für monatliche Downloads oder Besuchende.
#'
#' @param gefilterte_daten Ein Dataframe mit den relevanten Daten.
#' @param y_var Eine Zeichenkette für die zu visualisierende Variable ("downloads" oder "besuchende").
#'
#' @return Ein ggplot2-Balkendiagramm der Top-10-Datensätze.
ressource_monatlich_diagramm <- function(gefilterte_daten, y_var) {
  
  max_value <- max(gefilterte_daten[[y_var]])
  
  p <- ggplot(data = gefilterte_daten, aes(y = !!sym(y_var), x = reorder(datensatz_titel, !!sym(y_var)), fill = top_10)) +
    geom_col(width = 0.90) +
    scale_fill_gradientn(colors = c("#00407C", "#7AB6E2"), limits = c(1, 10)) +
    geom_text(aes(label = sprintf("%s", format(!!sym(y_var), big.mark = "'"))), 
              hjust = -0.08, family = "InterRegular", size = 3.5) +
    coord_flip() + 
    scale_x_discrete(labels = paste0(c(10:1), ".")) +     
    scale_y_continuous(
      labels = label_number(big.mark = "'"),
      expand = c(0.00, 0), 
      limits = c(0, max_value * 1.1),
      breaks = breaks_pretty(n = 5) 
    ) +
    labs(x = NULL, y = NULL) + 
    theme(
      title = element_text(size = 17, family = "InterBlack"),
      axis.text.y = element_text(size = 11, color ="#00407C", family = "InterBlack"),
      axis.ticks = element_blank(),  
      axis.ticks.length = unit(0.2, "cm"),  
      legend.position = "none", 
      panel.background = element_blank(), 
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_line(color = "grey75", linewidth = 0.1),
      panel.grid.major.x = element_line(color = "grey45", linewidth = 0.1),
      axis.text.x = element_text(size = 10, color ="#00407C"),  
      panel.border = element_rect(color = "grey45", fill = NA, linewidth = 0.5) 
    )
  p
}


#' Erstellt eine Tabelle der Top-10-Datensätze für monatliche Downloads oder Besuchende.
#'
#' @param gefilterte_daten Ein Dataframe mit den relevanten Daten.
#' @param y_var Eine Zeichenkette für die darzustellende Variable ("downloads" oder "besuchende").
#'
#' @return Eine gt-Tabelle für die Top-10-Datensätze.
top_ten_tabelle <- function(gefilterte_daten, y_var) {
  
  tabelle <- gefilterte_daten |>
    select(top_10, datensatz_titel, .data[[y_var]]) |>
    arrange(desc(.data[[y_var]])) |>
    gt(id = "top10tabelle") |>
    cols_label(
      top_10 = "",
      datensatz_titel = "Datensatz",
      !!y_var := ifelse(y_var == "downloads", "Downloads", "Besuchende")
    ) |>
    tab_style(
      style = list(
        cell_text(font = "InterRegular")
      ),
      locations = cells_body()
    ) |>
    tab_style(
      style = list(
        cell_text(font = "InterBlack")
      ),
      locations = cells_title("title")
    ) |>
    tab_options(
      table.font.size = px(12),
      heading.title.font.size = px(16),
      heading.subtitle.font.size = px(14),
      column_labels.font.size = px(14)
    ) |>
    cols_align(
      columns = c(datensatz_titel),
      align = "left"
    ) |>
    fmt_number(
      columns = .data[[y_var]],
      sep_mark = "'",
      decimals = 0
    ) |>
    data_color(
      columns = c(top_10),  
      target_columns = c(y_var),
      fn = scales::col_numeric(
        palette = c("#00407C", "#7AB6E2"),
        domain = c(1, 10) 
      ),
      autocolor_text = FALSE
    ) |>
    tab_style(
      style = list(
        cell_text(color = "white")
      ),
      locations = cells_body(columns = y_var)
    ) |> 
    opt_css(
      '
      #top10tabelle .gt_col_heading {
        font-family: "InterBlack";
        color: #00407C;
      }
      #top10tabelle .gt_table {
        font-family: "InterRegular";
      }
      '
    )
  
  return(tabelle)
}


################### Einzelne Datensätze analysieren ###################

#' Erstellt ein interaktives Liniendiagramm für monatliche Downloads und Besuchende.
#'
#' Standardmäßig werden die letzten 12 Monate oder die maximal verfügbaren Monate angezeigt.
#'
#' @param dataset Ein Dataframe mit den relevanten Daten.
#'
#' @return Ein plotly-Linendiagramm.
datensatz_jahr_plotly <- function(dataset) {
  
  monat_abkuerzungen_deutsch <- c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun", 
                                  "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
  
  # Daten für den gewählten Datensatz der letzten 12 Monate filtern
  gefilterte_daten_jahr <- ogd_analytics_monatlich |> 
    dplyr::filter(datensatz_titel == {{ dataset }}) |> 
    dplyr::filter(monatsbeginn >= (Sys.Date() - months(12))) |> 
    arrange(monatsbeginn) |>    
    mutate(monat_text = factor(monat_num, levels = unique(monat_num), labels = monat_abkuerzungen_deutsch[unique(monat_num)])) 
  
  # Maximalen Downloads und Besuchende für y-Achse bestimmen
  max_downloads_jahr <- max(gefilterte_daten_jahr$downloads, na.rm = TRUE)
  max_besuchende_jahr <- max(gefilterte_daten_jahr$besuchende, na.rm = TRUE)
  
  yaxis_besuchende_range <- if (max_besuchende_jahr > 0) c(0, max_besuchende_jahr * 1.3) else c(0, 1)
  yaxis_downloads_range <- if (max_downloads_jahr > 0) c(0, max_downloads_jahr * 1.3) else c(0, 1)
  
  # Ticks generieren basierend auf den Daten
  generate_ticks <- function(range, n = 4) {
    if (all(range == 0)) {
      return(c(0, 1))
    }
    
    ticks <- pretty_breaks()(range, n = n)
    ticks <- ticks[ticks %% 1 == 0]
    ticks <- unique(ticks)
    
    if (length(ticks) == 0) {
      ticks <- c(0, 1)
    }
    
    return(ticks)
  }
  
  tick_values_downloads <- generate_ticks(c(0, max_downloads_jahr*0.8))
  tick_values_besuchende <- generate_ticks(c(0, max_besuchende_jahr*0.8))
  
  if (length(tick_values_downloads) < 2) {
    tick_values_downloads <- c(0, max_downloads_jahr)
  }
  
  if (length(tick_values_besuchende) < 2) {
    tick_values_besuchende <- c(0, max_besuchende_jahr)
  }
  
  
  # Plot besuchende
  p_bs <- plot_ly(data = gefilterte_daten_jahr, x = ~monat_text) %>%
    add_lines(y = ~besuchende, name = "Besuchende", line = list(color = '#00407C', width = 2)) %>%
    add_markers(y = ~besuchende, name = "Besuchende", marker = list(color = '#00407C', size = 6)) %>%
    layout(
      yaxis = list(
        title = "",
        range = yaxis_besuchende_range,
        tickvals = tick_values_besuchende,
        tickformat = ",d", 
        ticklen = 5,
        tickcolor = 'white',
        showticklabels = TRUE,
        showline = FALSE,
        zeroline = TRUE,  
        zerolinecolor = '#BBBBBB', 
        zerolinewidth = 1,  
        automargin = T
      ),
      xaxis = list(
        title = "",
        showticklabels = TRUE,
        showgrid = FALSE,
        tickangle = 0, 
        tickvals = gefilterte_daten_jahr$monat_text,
        ticktext = gefilterte_daten_jahr$monat_text,
        tickmode = "array",
        ticklen = 8,  
        tickcolor = 'white',
        tickfont = list(
          color = '#00407C',  
          family = "InterBlack"
        )
      ),
      hoverlabel = list(
        font = list(family = "InterRegular") 
      ),
      showlegend = FALSE,
      font = list(family = "InterRegular"),
      annotations = list(
        list(
          x = -0.2,
          y = ifelse(max_besuchende_jahr == 0, (max_besuchende_jahr + 1), max_besuchende_jahr*1.25),
          text = "Besuchende",
          xanchor = "left",
          yanchor = "top",
          font = list(family = "InterBlack", size = 12, color = '#00407C'),
          showarrow = FALSE,
          align = "left"
        )
      )
    ) |> 
    config(
      displayModeBar = FALSE  #
    )
  
  # Plot Downloads
  p_dl <- plot_ly(data = gefilterte_daten_jahr, x = ~monat_text) %>%
    add_lines(y = ~downloads, name = "Downloads", line = list(color = '#0070B4', width = 2)) %>%
    add_markers(y = ~downloads, name = "Downloads", marker = list(color = '#0070B4', size = 6)) %>%
    layout(
      yaxis = list(
        title = "",
        range = yaxis_downloads_range,
        tickvals = tick_values_downloads,
        tickformatstops = list(
          list(dtickrange = c(1000, 1000000), value = "'d")
        ),        
        ticklen = 5,
        tickcolor = 'white',
        showticklabels = TRUE,
        showline = FALSE,
        zeroline = TRUE, 
        zerolinecolor = '#BBBBBB', 
        zerolinewidth = 1,
        automargin = TRUE
      ),
      xaxis = list(
        title = "",
        showticklabels = TRUE,
        showgrid = FALSE,
        tickangle = 0,  
        tickvals = gefilterte_daten_jahr$monat_text,
        ticktext = gefilterte_daten_jahr$monat_text,
        tickmode = "array",
        ticklen = 8,  
        tickcolor = 'white',
        tickfont = list(
          color = '#0070B4',  
          family = "InterBlack"
        )
      ),
      hoverlabel = list(
        font = list(family = "InterRegular")
      ),
      showlegend = FALSE,
      font = list(family = "InterRegular"),
      annotations = list(
        list(
          x = -0.2,
          y = ifelse(max_downloads_jahr == 0, (max_downloads_jahr + 1), max_downloads_jahr*1.25),
          text = "Downloads",
          xanchor = "left",
          yanchor = "top",
          font = list(family = "InterBlack", size = 12, color = '#0070B4'),
          showarrow = FALSE,
          align = "left"
        )
      )
    ) |> 
    config(
      displayModeBar = FALSE 
    )
  
  return(list(p_dl, p_bs))
}

#' Berechnet Statistiken für die Infoboxen zu Downloads und Besuchenden.
#'
#' @param datensatz_titel Der Titel des Datensatzes, für den die Statistiken berechnet werden.
#'
#' @return Deskriptive Statistiken.
datensatz_statistiken <- function(datensatz_titel) {
  
  # Daten für den gewählten Datensatz der letzten 12 Monate filtern
  gefilterte_daten_jahr <- ogd_analytics_monatlich |> 
    dplyr::filter(datensatz_titel == {{ datensatz_titel }}) |> 
    dplyr::filter(monatsbeginn >= (Sys.Date() - months(12))) 
  
  # Startdatum ermitteln (1. des Monats, des ersten verfügbaren Monats)
  erster_verf_monat <- gefilterte_daten_jahr |> 
    dplyr::summarise(erstes_datum = min(monatsbeginn, na.rm = TRUE)) |> 
    pull(erstes_datum)
  
  # Downloads-Statistiken berechnen
  total_downloads_letzte_12_monate <- sum(gefilterte_daten_jahr$downloads)
  avg_monatliche_downloads <- gefilterte_daten_jahr |> 
    group_by(monat_num) |> 
    summarize(monatliche_downloads = sum(downloads, na.rm = TRUE), .groups = "drop") |>  
    summarize(avg_downloads = mean(monatliche_downloads, na.rm = TRUE)) |>  
    pull(avg_downloads)
  
  
  all_time_downloads <- ogd_analytics_monatlich |> 
    dplyr::filter(datensatz_titel == {{ datensatz_titel }}) |> 
    summarise(total_downloads = sum(downloads)) |> 
    pull(total_downloads)
  
  # Besuchende-Statistiken berechnen
  total_besuchende_letzte_12_monate <- sum(gefilterte_daten_jahr$besuchende)
  avg_monatliche_besuchende <- gefilterte_daten_jahr |> 
    group_by(monat_num) |> 
    summarize(monatliche_besuchende = sum(besuchende, na.rm = TRUE), .groups = "drop") |> 
    summarize(avg_besuchende = mean(monatliche_besuchende, na.rm = TRUE)) |>  
    pull(avg_besuchende)
  
  
  all_time_besuchende <- ogd_analytics_monatlich |> 
    dplyr::filter(datensatz_titel == {{ datensatz_titel }}) |> 
    summarise(total_besuchende = sum(besuchende)) |> 
    pull(total_besuchende)
  
  list(
    # Downloads-Statistiken
    total_downloads_letzte_12_monate = total_downloads_letzte_12_monate,
    avg_monatliche_downloads = avg_monatliche_downloads,
    all_time_downloads = all_time_downloads,
    
    # Besuchende-Statistiken
    total_besuchende_letzte_12_monate = total_besuchende_letzte_12_monate,
    avg_monatliche_besuchende = avg_monatliche_besuchende,
    all_time_besuchende = all_time_besuchende,
    
    # Startdatum (für Anzeige in den Value Boxes)
    startdatum = erster_verf_monat
  )
}

