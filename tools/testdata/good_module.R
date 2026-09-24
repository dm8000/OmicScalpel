# Correct module. lint_ns.R must report nothing here.

goodUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("dataset"), "Dataset", choices = NULL),
    selectInput(inputId = ns("unit"), label = "Unit", choices = NULL),
    actionButton(ns("go"), "Run", icon = icon("play")),
    downloadButton(ns("dl"), "Download"),
    radioButtons(ns("mode"), "Mode", c("a", "b")),
    checkboxInput(ns("log2"), "log2", FALSE),
    plotOutput(ns("plot"), height = "300px"),
    uiOutput(ns("extra")),
    DTOutput(outputId = ns("table")),
    conditionalPanel("input.mode == 'a'", ns = ns, helpText("only for a")),
    box(title = "Not an id", status = "primary", h4("plain text"))
  )
}

goodServer <- function(id, ds, meta, go_to = NULL) {
  moduleServer(id, function(input, output, session) {
    # update* takes a bare id on purpose: the module session namespaces it.
    observeEvent(ds(), {
      updateSelectInput(session, "unit", choices = c("TPM", "CPM"))
      updateSelectizeInput(session, "dataset", selected = ds())
    })

    output$extra <- renderUI({
      tagList(
        numericInput(session$ns("cutoff"), "Cutoff", 0.05),
        add_rank_list("Visible Groups", labels = NULL, input_id = session$ns("visible"))
      )
    })

    observeEvent(input$go, {
      showModal(modalDialog(
        textInput(session$ns("new_name"), "New name"),
        footer = modalButton("Cancel")
      ))
    })

    output$plot <- renderPlot(plot(1))
  })
}
