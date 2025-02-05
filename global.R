# Pakete laden
pacman::p_load(tidyverse, gt, DT, bslib, shiny, shinydashboard, fresh, shinyWidgets, ggsci, showtext, scales, plotly, htmltools)

# Daten laden
ogd_analytics_ressource <- readr::read_delim("https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002522_00005043.csv")
ogd_analytics_ds <- readr::read_delim("https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002522_00005024.csv")

ogd_analytics <- ogd_analytics_ressource |> select(-c("anzahl_besuchende", "anzahl_klicks")) |> 
  left_join(ogd_analytics_ds, by = c("datensatz_id", "datensatz_titel", "publisher", "datum"))

# Daten manipulieren: Datumsformat und Ausreisser (mehr als 10k downloads pro Tag) entfernen, sowie den Datensatz welchen die App anzieht
ogd_analytics <- ogd_analytics |>
  mutate(
    anzahl_besuchende = replace_na(anzahl_besuchende, 0),
    anzahl_downloads = replace_na(anzahl_downloads, 0),
    datum = as.Date(datum, format = "%Y-%m-%d"),                       
  ) |>
  dplyr::filter(anzahl_downloads <= 10000,
                datensatz_titel != "Web Analytics des Datenkatalogs des Kantons Zürich")  


### Daten manipulieren ###

# Publisher Abkürzungen (kann allenfalls noch erweitert werden)
abk_publisher <- c("Awi", "Afm", "Aln", "Are", "Awel", "Ekz", "Ima", "Ogd")

# Bindestriche entfernen, erster Buchstabe kapital und Abkürzungen komplett kapital formatieren
ogd_analytics <- ogd_analytics |> 
  mutate(publisher = str_to_title(str_replace_all(publisher, "-", " "))) |>
  mutate(publisher = reduce(abk_publisher, 
                            .init = publisher, 
                            ~ str_replace_all(.x, fixed(.y), toupper(.y)))) |> 
  dplyr::arrange(desc(anzahl_downloads))


# Aggregation für "Alle Datensätze" - für ID und Publisher 000 eingesetzt
alle_dist <- ogd_analytics |> 
  group_by(datum) |> 
  summarise(
    anzahl_downloads = sum(anzahl_downloads),
    anzahl_besuchende = sum(anzahl_besuchende),
    datensatz_id = 000,  
    datensatz_titel = "Alle Datensätze aggregiert",
    publisher = "000"
  )

# in original Datensatz einbinden
ogd_analytics <- bind_rows(
  ogd_analytics,
  alle_dist
)

# Monatliche Downloads
ogd_analytics_monatlich <- ogd_analytics |> 
  mutate(
    monatsbeginn = floor_date(datum, "month"),
    monat_num = month(monatsbeginn),
    jahr_num = year(monatsbeginn)
  ) |> 
  group_by(jahr_num, monat_num, monatsbeginn, datensatz_id, datensatz_titel, publisher) |> 
  summarise(downloads = sum(anzahl_downloads), 
            besuchende = sum(anzahl_besuchende),
            .groups = "drop") |> 
  arrange(desc(downloads))


# Publisher nach Anzahl titel
sorted_publishers <- ogd_analytics_monatlich |> 
  dplyr::filter(publisher != "000") |> 
  group_by(publisher) |> 
  summarise(n = n(), .groups = "drop") |> 
  arrange(desc(n)) |> 
  pull(publisher)


# Verfügbare Monate
available_months <- unique(ogd_analytics_monatlich$monatsbeginn)

ogd_analytics_jahre <- ogd_analytics_monatlich |>  
  group_by(datensatz_id, datensatz_titel, jahr_num) |> 
  summarise(downloads_jahr = sum(downloads), 
            besuchende_jahr = sum(besuchende),
            .groups = "drop") |> 
  arrange(desc(downloads_jahr)) 

# eigenes ggplot Theme 
OGD_theme <- create_theme(
  adminlte_color(
    light_blue = "#0070B4"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#FFFFFE",
    dark_hover_bg = "#EDF5FA",
    dark_color = "#00407C"
  ),
  adminlte_global(
    content_bg = "#F0F0F0",
    box_bg = "#FFFFFE", 
    info_box_bg = "#FFFFFE"
  )
)


# Schriften laden aus CSS stylesheet
font_add("InterRegular", "www/Inter-Regular.ttf")
font_add("InterBlack", "www/Inter-Black.ttf")
showtext_auto()

