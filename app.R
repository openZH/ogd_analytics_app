
source("ui.R")
source("server.R")


ui <- create_ui()
server <- create_server()

shinyApp(ui = ui, server = server)