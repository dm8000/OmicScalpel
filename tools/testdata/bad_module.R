# Broken module. Every numbered comment marks one problem lint_ns.R must find.

badUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput("dataset", "Dataset", choices = NULL),          # 1 bare id in UI
    actionButton("go", "Run", icon = icon("play")),             # 2 bare id in UI
    plotOutput(outputId = "plot"),                              # 3 bare named outputId
    uiOutput(ns("extra")),                                      #   correct, no report
    conditionalPanel("input.mode == 'a'", helpText("x")),       # 4 conditionalPanel w/o ns
    tabItem(tabName = "leftover", h1("never shown"))            # 8 page shell in a module
  )
}

badServer <- function(id, ds, meta, go_to = NULL) {
  moduleServer(id, function(input, output, session) {
    updateSelectInput(session, "unit", choices = "TPM")          #   correct, no report

    output$extra <- renderUI({
      numericInput("cutoff", "Cutoff", 0.05)                     # 5 bare id in renderUI
    })

    observeEvent(input$go, {
      showModal(modalDialog(textInput("new_name", "New name")))  # 6 bare id in modal
    })

    observe({
      req(input$dataset)                                         # 7 input$dataset (rule D)
    })
  })
}
