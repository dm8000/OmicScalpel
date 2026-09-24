# app.R -- OmicScalpel, one app, one URL, nine tabs.
#
# Replaces nine separately deployed apps. Two things are shared here and nowhere
# else, which is the whole point of merging them:
#
#   active_dataset -- picked once in the top bar, read by every module that
#                     works on a single dataset. Before, the researcher chose
#                     the same dataset in six different apps.
#   shared_meta    -- Metadata.xlsx read once per change instead of once per
#                     app. 1.7 MB, and it used to be read nine times.
#
# Load the environment explicitly. Shiny does NOT source global.R for an app
# that has an R/ directory: it turns on shiny.autoload.r instead and calls
# loadSupport(globalrenv = NULL), which auto-sources R/*.R and skips global.R
# entirely. The app then starts with no packages attached and dies on the first
# dashboardPage(). Sourcing it here works either way, and under shiny-server too.
source("global.R")

# --- ui --------------------------------------------------------------------

# No shinydashboard here. Its header and sidebar were what put a logo block and
# a collapse button on top of the left panel in the first place, and its
# stylesheet then fought every attempt to darken a panel body. The markup is
# ours now: see R/ui_helpers.R and www/omicscalpel.css.

module_tabs <- function() {
  panels <- list()
  last_group <- NULL
  for (m in MODULES) {
    if (!exists(m$ui, mode = "function")) next
    label <- m$title
    if (!is.null(last_group) && !identical(m$group, last_group)) {
      # marks where Explore ends and Export begins, for the CSS separator
      label <- tagList(span(class = "os-group-mark"), m$title)
    }
    last_group <- m$group
    panels[[length(panels) + 1L]] <- tabPanel(title = label, value = m$id,
                                              get(m$ui)(m$id))
  }
  do.call(tabsetPanel, c(list(id = "tabs", type = "tabs"), unname(panels)))
}

ui <- fluidPage(
  title = "OmicScalpel",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "omicscalpel.css")
  ),
  tags$div(
    class = "os-app",
    div(
      class = "os-topbar",
      div(class = "os-brand", "OmicScalpel"),
      selectInput("active_dataset", NULL, choices = NULL, width = "240px"),
      div(class = "os-context", textOutput("dataset_context", inline = TRUE)),
      div(
        class = "os-topbar-right",
        actionButton("reload_metadata", "Re-read metadata",
                     icon = icon("rotate"), class = "btn-xs")
      )
    ),
    div(class = "os-tabs", module_tabs())
  )
)

# --- server ----------------------------------------------------------------

server <- function(input, output, session) {

  # Read once, and again only when a module says it changed the file.
  shared_meta <- reactive({
    metadata_version()
    load_metadata()
  })

  active_dataset <- reactiveVal(NULL)

  # Keep the selector's choices in step with the file: a dataset added through
  # the upload tab, or renamed in the editor, shows up here without a restart.
  observe({
    choices <- sort(unique(shared_meta()$dataset))
    current <- isolate(active_dataset())
    keep <- if (!is.null(current) && current %in% choices) current else choices[1]
    updateSelectInput(session, "active_dataset", choices = choices, selected = keep)
    if (!identical(isolate(active_dataset()), keep)) active_dataset(keep)
  })

  observeEvent(input$active_dataset, {
    if (!identical(active_dataset(), input$active_dataset)) {
      active_dataset(input$active_dataset)
    }
  })

  observeEvent(input$reload_metadata, {
    invalidate_metadata()
    showNotification("Metadata re-read from disk.", duration = 3)
  })

  # What the top bar says about the active dataset: enough to know whether the
  # tab you are about to open can do anything with it.
  output$dataset_context <- renderText({
    ds <- active_dataset()
    if (is.null(ds)) return("no dataset")
    md <- shared_meta()
    n  <- sum(md$dataset == ds, na.rm = TRUE)
    ty <- unique(md$`Data.type`[md$dataset == ds])
    ty <- ty[!is.na(ty)]
    units <- list_units(ds)
    paste0(n, " samples",
           if (length(ty)) paste0(" · ", paste(ty, collapse = "/")) else "",
           " · ", if (length(units)) paste(units, collapse = ", ") else "no matrix")
  })

  # Handed to the modules so a tab can send the user elsewhere without knowing
  # there is a dashboard around it. tab = NULL means "change the dataset and
  # stay where you are", which is what the summary tab's Load button does.
  go_to <- function(tab = NULL, dataset = NULL) {
    if (!is.null(dataset)) {
      active_dataset(dataset)
      updateSelectInput(session, "active_dataset", selected = dataset)
    }
    if (!is.null(tab)) updateTabsetPanel(session, "tabs", selected = tab)
  }

  for (m in MODULES) {
    if (!exists(m$server, mode = "function")) next
    get(m$server)(m$id, ds = active_dataset, meta = shared_meta, go_to = go_to)
  }
}

shinyApp(ui, server)
