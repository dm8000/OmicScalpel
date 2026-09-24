#!/usr/bin/env Rscript
# Run ONE module on its own, in a browser, before the unified app exists.
#
#   Rscript tools/run_module.R compare_genes [port]
#
# The dataset selector here stands in for the one app.R puts in the sidebar, so
# a converted module can be looked at -- and its dataset switched -- without
# waiting for the other eight.

source("global.R")

pick <- commandArgs(TRUE)
if (!length(pick) || !(pick[1] %in% names(MODULES))) {
  cat("usage: Rscript tools/run_module.R <module> [port]\nmodules: ",
      paste(names(MODULES), collapse = ", "), "\n", sep = "")
  quit(status = 2)
}
m <- MODULES[[pick[1]]]
if (!file.exists(module_file(m))) { cat(m$id, " is not built yet\n", sep = ""); quit(status = 2) }
port <- if (length(pick) > 1) as.integer(pick[2]) else 8787L

datasets <- unique(load_metadata()$dataset)

# shinyApp() built in code does not serve www/ the way runApp(appDir) does, so
# the stylesheet has to be published explicitly or the module renders unstyled.
addResourcePath("os", "www")

ui <- fluidPage(
  title = paste("OmicScalpel /", m$title),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "os/omicscalpel.css")),
  tags$div(
    class = "os-app",
    div(
      class = "os-topbar",
      div(class = "os-brand", m$title),
      selectInput("ds", NULL, choices = datasets, width = "240px"),
      div(class = "os-context", "standing in for the app's top bar")
    ),
    div(class = "os-tabs", div(class = "tab-content", get(m$ui)(m$id)))
  )
)

server <- function(input, output, session) {
  ds <- reactiveVal(datasets[1])
  observeEvent(input$ds, ds(input$ds))
  get(m$server)(m$id,
                ds    = ds,
                meta  = reactive(load_metadata()),
                go_to = function(tab, dataset) {
                  showNotification(sprintf("go_to(%s, %s) -- no other tab here", tab, dataset))
                })
}

cat("http://127.0.0.1:", port, "\n", sep = "")
shinyApp(ui, server, options = list(port = port, host = "127.0.0.1", launch.browser = FALSE))
