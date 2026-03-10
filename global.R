# Pakete laden
pacman::p_load(
  tidyverse, gt, DT, bslib, shiny, shinydashboard, fresh,
  shinyWidgets, ggsci, showtext, scales, plotly, htmltools, lubridate
)

# Daten laden
ogd_analytics_ressource <- readr::read_delim(
  "https://daten.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002522_00005043.csv",
  show_col_types = FALSE
)

ogd_analytics_ds <- readr::read_delim(
  "https://daten.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002522_00005024.csv",
  show_col_types = FALSE
)

# Publisher-Abkürzungen
abk_publisher <- c("Awi", "Afm", "Aln", "Are", "Awel", "Ekz", "Ima", "Ogd")

# Hilfsfunktion für Publisher-Namen
clean_publisher <- function(x, abbreviations = abk_publisher) {
  out <- x |>
    stringr::str_replace_all("-", " ") |>
    stringr::str_to_title()
  
  for (abk in abbreviations) {
    out <- stringr::str_replace_all(
      string = out,
      pattern = stringr::fixed(abk),
      replacement = toupper(abk)
    )
  }
  
  out
}

# -----------------------------
# Downloads pro Datensatz und Tag
# Quelle: Ressourcentabelle
# -----------------------------
downloads_tag <- ogd_analytics_ressource |>
  dplyr::mutate(
    datum = as.Date(datum, format = "%Y-%m-%d"),
    anzahl_downloads = tidyr::replace_na(anzahl_downloads, 0)
  ) |>
  dplyr::filter(
    anzahl_downloads <= 10000,
    datensatz_titel != "Web Analytics des Datenkatalogs des Kantons Zürich"
  ) |>
  dplyr::group_by(datum, datensatz_id, datensatz_titel, publisher) |>
  dplyr::summarise(
    anzahl_downloads = sum(anzahl_downloads, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# Tatsächliche Datensatz-Besuche pro Datensatz und Tag
# Quelle: Datensatztabelle
# -----------------------------
besuchende_tag <- ogd_analytics_ds |>
  dplyr::mutate(
    datum = as.Date(datum, format = "%Y-%m-%d"),
    anzahl_besuchende = tidyr::replace_na(anzahl_besuchende, 0)
  ) |>
  dplyr::filter(
    datensatz_titel != "Web Analytics des Datenkatalogs des Kantons Zürich"
  ) |>
  dplyr::group_by(datum, datensatz_id, datensatz_titel, publisher) |>
  dplyr::summarise(
    anzahl_besuchende = sum(anzahl_besuchende, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# Gemeinsame Tagesbasis auf Datensatz-Ebene
# -----------------------------
ogd_analytics <- downloads_tag |>
  dplyr::full_join(
    besuchende_tag,
    by = c("datum", "datensatz_id", "datensatz_titel", "publisher")
  ) |>
  dplyr::mutate(
    anzahl_downloads = tidyr::replace_na(anzahl_downloads, 0),
    anzahl_besuchende = tidyr::replace_na(anzahl_besuchende, 0),
    publisher = clean_publisher(publisher)
  ) |>
  dplyr::arrange(dplyr::desc(anzahl_downloads))

# -----------------------------
# Aggregation "Alle Datensätze"
# -----------------------------
alle_dist <- ogd_analytics |>
  dplyr::group_by(datum) |>
  dplyr::summarise(
    anzahl_downloads = sum(anzahl_downloads, na.rm = TRUE),
    anzahl_besuchende = sum(anzahl_besuchende, na.rm = TRUE),
    datensatz_id = 0,
    datensatz_titel = "Alle Datensätze aggregiert",
    publisher = "000",
    .groups = "drop"
  )

ogd_analytics <- dplyr::bind_rows(
  ogd_analytics,
  alle_dist
)

# -----------------------------
# Monatliche Aggregation
# -----------------------------
ogd_analytics_monatlich <- ogd_analytics |>
  dplyr::mutate(
    monatsbeginn = lubridate::floor_date(datum, unit = "month"),
    monat_num = lubridate::month(monatsbeginn),
    jahr_num = lubridate::year(monatsbeginn)
  ) |>
  dplyr::group_by(
    jahr_num, monat_num, monatsbeginn,
    datensatz_id, datensatz_titel, publisher
  ) |>
  dplyr::summarise(
    downloads = sum(anzahl_downloads, na.rm = TRUE),
    besuchende = sum(anzahl_besuchende, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(downloads))

# Publisher nach Anzahl Titel
sorted_publishers <- ogd_analytics_monatlich |>
  dplyr::filter(publisher != "000") |>
  dplyr::group_by(publisher) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(n)) |>
  dplyr::pull(publisher)

# Verfügbare Monate
available_months <- unique(ogd_analytics_monatlich$monatsbeginn)

# Jahresaggregation
ogd_analytics_jahre <- ogd_analytics_monatlich |>
  dplyr::group_by(datensatz_id, datensatz_titel, jahr_num) |>
  dplyr::summarise(
    downloads_jahr = sum(downloads, na.rm = TRUE),
    besuchende_jahr = sum(besuchende, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(downloads_jahr))

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

