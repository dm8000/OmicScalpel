# cutoff_editor_app.R
# Paths come from config/config.txt -- see R/config.R
source("../../R/config.R")
Sys.setenv(R_LIBS = os_path("lib"))
Sys.setenv(R_LIBS_USER = os_path("lib"))
.libPaths(os_path("lib"))
library(shiny)
library(shinydashboard)
library(rhandsontable)
library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)

# Caminho do arquivo de dados
file_path <- os_path("hubdata", "Metadata.xlsx")

# Função para carregar os dados
load_data <- function() {
  data <- read_excel(file_path)
  data[data == "NA"] <- NA
  data
}

# Função para salvar dados com backup
save_data <- function(new_data) {
  current_data <- tryCatch(read_excel(file_path), error = function(e) NULL)
  
  backup_folder <- os_path("backups")
  if (!dir.exists(backup_folder)) dir.create(backup_folder, recursive = TRUE)
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_file <- file.path(backup_folder, paste0("Metadata_backup_", timestamp, ".xlsx"))
  
  if (!is.null(current_data)) write_xlsx(current_data, backup_file)
  
  new_data[is.na(new_data)] <- "NA"
  write_xlsx(new_data, file_path)
}

# Interface do usuário
ui <- dashboardPage(
  dashboardHeader(
    title = "Cutoff maker"
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
  /* Container principal do slider */
  .shiny-input-container {
    width: 100% !important;
    padding: 0 15px;
  }

  /* Linha do slider */
  .irs-line {
    width: 100% !important;
    max-width: 100% !important;
  }

  /* Grades e labels */
  .irs-grid {
    width: 100% !important;
  }

  /* Handle do slider */
  .irs-handle {
    top: 25px;
  }
")),
      tags$style(HTML("
        .box { border-radius: 5px; }
        .content-wrapper { background-color: #f9f9f9; }
        .main-header .logo { font-weight: bold; }
        #table { overflow-y: auto; max-height: 60vh; }
        .rhandsontable { overflow: visible; }
        .btn-xs {
          padding: 2px 6px;
          font-size: 12px;
          line-height: 1.5;
          border-radius: 3px;
          margin: 0 2px;
        }
      "))
    ),
    
    fluidRow(
      box(
        title = "Data Visualization & Controls",
        status = "primary",
        solidHeader = TRUE,
        width = 8,
        height = "90vh",
        plotlyOutput("dist_plot", height = "60%"),
        fluidRow(
          column(6,
                 pickerInput(
                   "dataset",
                   "Select Dataset:",
                   choices = NULL
                 )),
          column(6,
                 pickerInput(
                   "numeric_col",
                   "Select Numeric Column:",
                   choices = NULL
                 ))
        ),
        fluidRow(
          column(12,
                 div(style = "text-align: right; margin-bottom: 5px;",
                     actionButton("add_cutoff", "+", class = "btn-xs"),
                     actionButton("remove_cutoff", "-", class = "btn-xs")
                 ),
                 uiOutput("cutoff_sliders")
          )
        ),
        fluidRow(
          column(12,
                 div(style = "text-align: right;",
                     actionBttn(
                       "make_cutoff",
                       "Apply Cutoff",
                       icon = icon("cut"),
                       style = "material-flat",
                       color = "primary",
                       size = "sm"
                     )
                 )
          )
        )
      ),
      
      box(
        title = "Data Table",
        status = "info",
        solidHeader = TRUE,
        width = 4,
        height = "90vh",
        div(style = "height: 70vh; overflow-y: auto;",
            rHandsontableOutput("table")
        ),
        br(),
        actionBttn(
          "save",
          "Save Changes",
          icon = icon("floppy-disk"),
          style = "material-flat",
          color = "primary",
          block = TRUE
        )
      )
    )
  )
)

# Lógica do servidor
server <- function(input, output, session) {
  
  # Valores reativos
  rv <- reactiveValues(
    data = NULL,
    filtered_data = NULL,
    numeric_cols = NULL,
    original_indices = NULL,
    num_cutoffs = 1,
    cutoff_values = list()
  )
  
  # Função para atualizar as colunas numéricas
  update_numeric_cols <- function(data) {
    numeric_cols <- sapply(data, function(x) {
      if (all(is.na(x))) return(FALSE)
      conv <- suppressWarnings(as.numeric(x))
      is_numeric <- !any(is.na(conv) & !is.na(x))
      is_numeric && (length(unique(na.omit(conv))) >= 2)
    })
    rv$numeric_cols <- names(data)[numeric_cols]
  }
  
  # Carrega os dados e atualiza o dropdown de dataset
  observe({
    data <- load_data()
    rv$data <- data
    updatePickerInput(session, "dataset", choices = unique(data$dataset))
    update_numeric_cols(data)
  })
  
  # Atualiza o dropdown de coluna numérica conforme o dataset selecionado
  observeEvent(input$dataset, {
    req(rv$data)
    idx <- which(rv$data$dataset == input$dataset)
    rv$original_indices <- idx
    
    filtered <- rv$data[idx, , drop = FALSE] %>%
      dplyr::select(any_of(rv$numeric_cols)) %>%
      dplyr::select(where(~ !all(is.na(.))))
    
    rv$filtered_data <- filtered
    updatePickerInput(session, "numeric_col", choices = names(filtered))
  })
  
  # Controle dos sliders
  observeEvent(input$add_cutoff, {
    rv$num_cutoffs <- min(rv$num_cutoffs + 1, 5)  # Máximo de 5 cutoffs
  })
  
  observeEvent(input$remove_cutoff, {
    rv$num_cutoffs <- max(rv$num_cutoffs - 1, 1)  # Mínimo de 1 cutoff
  })
  
  # Geração dinâmica dos sliders
  output$cutoff_sliders <- renderUI({
    req(input$numeric_col, rv$filtered_data)
    col_vals <- na.omit(as.numeric(rv$filtered_data[[input$numeric_col]]))
    
    slider_list <- lapply(1:rv$num_cutoffs, function(i) {
      # Create a sequence for slider choices
      min_val <- floor(min(col_vals))
      max_val <- ceiling(max(col_vals))
      step_size <- 0.1
      choices <- seq(min_val, max_val, by = step_size)
      
      # Find the closest value to the median in the choices
      median_val <- median(col_vals)
      closest_val <- choices[which.min(abs(choices - median_val))]
      
      sliderTextInput(
        inputId = paste0("cutoff_", i),
        label = if(i == 1) "Cutoff Values:" else NULL,
        choices = choices,
        selected = closest_val,
        grid = TRUE,
        hide_min_max = TRUE,
        width = "100%"
      )
    })
    
    do.call(tagList, slider_list)
  })
  
  # Renderiza o gráfico de distribuição interativo
  output$dist_plot <- renderPlotly({
    req(input$numeric_col, rv$filtered_data)
    dados <- na.omit(as.numeric(rv$filtered_data[[input$numeric_col]]))
    if (length(dados) == 0) return(NULL)
    
    p <- ggplot(data.frame(value = dados), aes(x = value)) +
      geom_histogram(fill = "#3c8dbc", color = "white", bins = 30, alpha = 0.8) +
      labs(title = paste("Distribution of", input$numeric_col),
           x = input$numeric_col,
           y = "Count") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold"))
    
    # Adiciona linhas para cada cutoff
    for(i in 1:rv$num_cutoffs) {
      if(!is.null(input[[paste0("cutoff_", i)]])) {
        p <- p + geom_vline(xintercept = input[[paste0("cutoff_", i)]], 
                            color = "#dd4b39", linetype = "dashed", size = 1)
      }
    }
    
    ggplotly(p) %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
  })
  
  # Cria a nova coluna de cutoff com múltiplos pontos
  observeEvent(input$make_cutoff, {
    req(input$numeric_col, input$dataset)
    idx <- which(rv$data$dataset == input$dataset)
    numeric_vals <- as.numeric(rv$data[[input$numeric_col]][idx])
    
    # Coleta todos os valores de cutoff
    cutoffs <- sort(sapply(1:rv$num_cutoffs, function(i) {
      as.numeric(input[[paste0("cutoff_", i)]])
    }))
    
    # Cria a classificação
    classification <- cut(numeric_vals,
                          breaks = c(-Inf, cutoffs, Inf),
                          labels = c(paste0("< ", cutoffs[1]),
                                     if(length(cutoffs) > 1) {
                                       paste(cutoffs[-length(cutoffs)], 
                                             "-", cutoffs[-1])
                                     },
                                     paste0("≥ ", cutoffs[length(cutoffs)])))
    
    new_col <- paste0(input$numeric_col, ".", 
                      paste(gsub("\\.", "_", format(round(cutoffs, 2), nsmall = 2)), 
                            collapse = "_"))
    
    rv$data[[new_col]] <- NA_character_
    rv$data[[new_col]][idx] <- as.character(classification)
    rv$filtered_data[[new_col]] <- rv$data[[new_col]][idx]
    update_numeric_cols(rv$data)
  })
  
  # Renderiza a tabela estilizada
  output$table <- renderRHandsontable({
    req(rv$filtered_data)
    rhandsontable(rv$filtered_data, 
                  readOnly = TRUE, 
                  height = 600,
                  rowHeaderWidth = 50) %>%
      hot_table(stretchH = "all") %>%
      hot_cols(columnSorting = TRUE, manualColumnMove = TRUE) %>%
      hot_context_menu(allowRowEdit = FALSE, 
                       allowColEdit = FALSE)
  })
  
  # Ação de salvar com feedback visual
  observeEvent(input$save, {
    showModal(modalDialog(
      title = "Saving Data",
      footer = NULL,
      tags$div(class = "text-center",
               tags$p("Saving changes and creating backup..."),
               tags$div(class = "spinner-border text-primary"))
    ))
    
    save_data(rv$data)
    
    removeModal()
    showNotification("Data successfully saved! Backup created.", 
                     type = "message",
                     duration = 5)
  })
}

# Configuração do aplicativo
shinyApp(ui = ui, server = server, options = list(
  port = 49159,
  host = "127.0.0.1",
  launch.browser = TRUE
))