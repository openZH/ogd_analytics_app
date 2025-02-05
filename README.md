# OGD Analytics App

Die OGD Analytics Shiny-App bietet wertvolle Einblicke in die Nutzung des Datenkatalogs des Kantons Zürich. Sie zeigt, wie oft einzelne Datensätze heruntergeladen wurden, macht Trends sichtbar und hilft, die Nutzung offener Daten besser zu verstehen.

Die Shiny App wird unter folgendem Link gehostet:  
👉 [OGD Analytics](https://openzh.shinyapps.io/OGD_Analytics/)  

---

## Funktionen

- **Top-10 Datensätze:** Durchsuche die Top-10 Datensätze nach Downloads und Besuchenden über einen bestimmten Zeitraum innerhalb eines Kalenderjahres.
- **Publisher-Statistiken:** Liste Datensätze nach Publishern und sehe deren Download- und Besucheranzahlen über einen bestimmten Zeitraum.
- **Datensätze:** Analysiere einzelne Datensätze, sehe Statistiken und Trends der letzten 12 Monate.


## Datenquelle

Die App nutzt einen Datensatz aus dem Open Government Data (OGD) Katalog des Kantons Zürich:

- **Rohdaten:** [Web Analytics des Datenkatalogs des Kantons Zürich](https://www.zh.ch/de/politik-staat/statistik-daten/datenkatalog.html#/datasets/2522@statistisches-amt-kanton-zuerich/distributions/5043)
- Die Daten werden beim Start der App direkt aus der Quelle geladen.

---

## Einrichtung und Installation

### Voraussetzungen
Stelle sicher, dass R (Version 4.3.3 oder höher) sowie die folgenden Bibliotheken installiert sind (werden beim Ausführen der Skripte automatisch installiert und geladen):

- `shiny`
- `shinydashboard`
- `fresh`
- `bslib`
- `tidyverse`
- `plotly`
- `DT`
- `gt`
- `shinyWidgets`
- `ggsci`
- `scales`
- `showtext`
- `htmltools`

Die App verwendet das Paket `pacman` für das Paketmanagement und das Laden der Bibliotheken.

### Schritte zum Ausführen der App

1. Klone oder lade das Repository herunter.
2. Öffne das Projekt in deiner R-IDE (z. B. RStudio).
3. Starte die App, indem du das Skript `app.R` ausführst.

---

## Kontakt

Für Fragen oder Vorschläge kannst du uns gerne kontaktieren:

Fach- und Koordinationsstelle OGD  
- **E-Mail:** info@open.zh.ch  
- **GitHub:** [https://github.com/openZH](https://github.com/openZH)

---

## Autor

Diese App wurde von Loris Kaufmann (Praktikant Data Science & Analysen) entwickelt, mit Unterstützung des Team Data im Statistischen Amt des Kantons Zürich. 

- **Autor:** Loris Kaufmann
- **Zugehörigkeit:** Statistisches Amt Kanton Zürich
- **GitHub:** [https://github.com/LoKauf](https://github.com/LoKauf)

