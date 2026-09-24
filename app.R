# app.R -- OmicScalpel, one app, one URL, nine tabs.
#
# Replaces nine separately deployed apps. Two things are shared here and nowhere
# else, which is the whole point of merging them:
#
#   active_dataset -- picked once in the sidebar, read by every module that
#                     works on a single dataset. Before, the researcher chose
#                     the same dataset in six different apps.
#   shared_meta    -- Metadata.xlsx read once per change instead of once per
#                     app. 1.7 MB, and it used to be read nine times.
#
# global.R has already run: config, library path, packages, R/ and the modules.

# --- ui --------------------------------------------------------------------

sidebar_menu <- function() {
  items <- list()
  for (g in module_groups()) {
    items <- c(items, list(tags$li(class = "header", toupper(g))))
    for (m in MODULES) {
      if (!identical(m$group, g)) next
      items <- c(items, list(menuItem(m$title, tabName = m$id, icon = icon(m$icon))))
    }
  }
  do.call(sidebarMenu, c(list(id = "tabs"), items))
}

body_tabs <- function() {
  built <- Filter(function(m) exists(m$ui, mode = "function"), MODULES)
  tabs <- lapply(built, function(m) tabItem(tabName = m$id, get(m$ui)(m$id)))
  missing <- setdiff(names(MODULES), names(built))
  tabs <- c(tabs, lapply(missing, function(id) tabItem(
    tabName = id,
    box(width = 12, status = "warning", title = MODULES[[id]]$title,
        "Not converted yet.")
  )))
  do.call(tabItems, tabs)
}

ui <- dashboardPage(
  dashboardHeader(
    title = "OmicScalpel",
    tags$li(class = "dropdown",
            tags$span(class = "navbar-text",
                      style = "line-height:50px; padding-right:12px;",
                      textOutput("dataset_badge", inline = TRUE)))
  ),
  dashboardSidebar(
    width = 260,
    selectInput("active_dataset", "Active dataset", choices = NULL, width = "95%"),
    tags$div(style = "padding: 0 15px 10px 15px; font-size: 85%; opacity: .7;",
             "Used by every single-dataset tab."),
    sidebar_menu()
  ),
  dashboardBody(body_tabs())
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

  output$dataset_badge <- renderText({
    ds <- active_dataset()
    if (is.null(ds)) "no dataset" else paste("Dataset:", ds)
  })

  # Handed to the modules so a tab can send the user elsewhere without knowing
  # there is a dashboard around it. data-summary is the one that uses it.
  go_to <- function(tab, dataset = NULL) {
    if (!is.null(dataset)) {
      active_dataset(dataset)
      updateSelectInput(session, "active_dataset", selected = dataset)
    }
    updateTabItems(session, "tabs", tab)
  }

  for (m in MODULES) {
    if (!exists(m$server, mode = "function")) next
    get(m$server)(m$id, ds = active_dataset, meta = shared_meta, go_to = go_to)
  }
}

shinyApp(ui, server)
