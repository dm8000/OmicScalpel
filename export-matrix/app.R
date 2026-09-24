# Paths come from config/config.txt -- see R/config.R
source("../R/config.R")
Sys.setenv(R_LIBS = os_path("lib"))
Sys.setenv(R_LIBS_USER = os_path("lib"))
.libPaths(os_path("lib"))
library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(tidyr)

# Função para carregar os metadados
load_metadata <- function() {
  read_excel(os_path("hubdata", "Metadata.xlsx"))
}

# Função para carregar dados de expressão a partir do dataset e da unidade escolhida
load_expression <- function(dataset, file_unit) {
  dataset_dir <- file.path(os_path("hubdata"), dataset)
  file_path <- file.path(dataset_dir, paste0(dataset, "_", file_unit, ".txt"))
  if (!file.exists(file_path)) {
    files <- list.files(dataset_dir, pattern = paste0("^", dataset, "_.*\\.txt$"), full.names = TRUE)
    if (length(files) > 0) {
      warning("Arquivo ", file_path, " not found. Using file ", files[1], " instead.")
      file_path <- files[1]
    } else {
      stop("No expression file found in the folder: ", dataset_dir)
    }
  }
  read.delim(file_path, check.names = FALSE)
}

# Verificações e sumários
eval_checks <- function(mat) {
  vals <- as.numeric(mat)
  has_frac <- any(vals != floor(vals), na.rm = TRUE)
  has_neg <- any(vals < 0, na.rm = TRUE)
  total <- length(vals)
  zeros <- sum(vals == 0, na.rm = TRUE)
  paste0("Matrix checks:\n",
         "- Fractions: ", ifelse(has_frac, "Y", "N"), "\n",
         "- Negative values: ", ifelse(has_neg, "Y", "N"), "\n",
         "- Features x Samples: ", total, "\n",
         "- Zero values: ", zeros)
}

sample_sums_text <- function(mat) {
  sums <- colSums(mat, na.rm = TRUE)
  paste(names(sums), "=", round(sums, 2), collapse = "\n")
}

ui <- fluidPage(
  titlePanel("Generate Expression Matrix"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose the dataset", choices = unique(load_metadata()$dataset)),
      uiOutput("unit_ui"),
      selectizeInput("genes_list", "Select genes:", choices = NULL, multiple = TRUE),
      helpText("Leave empty to include all genes"),
      hr(),
      h4("Metadata options"),
      selectizeInput("metadata_fields", "Select metadata fields to include:", choices = NULL, multiple = TRUE),
      hr(),
      checkboxInput("do_log", "+0.001 and log transform", FALSE),
      checkboxInput("do_zscore", "Z-score normalization", FALSE),
      actionButton("generate_matrix", "Generate matrix"),
      downloadButton("download_matrix", "Download matrix"),
      hr(),
      verbatimTextOutput("matrix_checks"),
      plotOutput("boxplot", height = "250px"),
      hr(),
      h4("Expression sum"),
      verbatimTextOutput("sample_sums")
    ),
    mainPanel(
      DTOutput("matrix_table")
    )
  )
)

server <- function(input, output, session) {
  # Atualiza unidades e escolhas de genes/metadados ao escolher dataset
  observeEvent(input$dataset, {
    meta <- load_metadata()
    expr <- load_expression(input$dataset, list.files(file.path(os_path("hubdata"), input$dataset), pattern = paste0("^", input$dataset, ".*\\.txt$"))[1])
    
    # units
    dir <- file.path(os_path("hubdata"), input$dataset)
    files <- list.files(dir, pattern = paste0("^", input$dataset, "_.*\\.txt$"))
    units <- sub(paste0("^", input$dataset, "_(.*)\\.txt$"), "\\1", files)
    updateSelectInput(session, "file_unit", choices = units)
    
    # genes
    updateSelectizeInput(session, "genes_list", choices = expr$Symbol)
    
    # metadata fields - only show fields with at least one non-"NA" value for current dataset samples
    current_samples <- colnames(expr)[-1]  # exclude Symbol column
    meta_subset <- filter(meta, SampleID %in% current_samples)
    meta_opts <- setdiff(names(meta), "SampleID")
    # Filter to fields that have at least one non-"NA" value
    valid_fields <- meta_opts[sapply(meta_opts, function(field) {
      any(meta_subset[[field]] != "NA")
    })]
    updateSelectizeInput(session, "metadata_fields", choices = valid_fields)
  })
  
  output$unit_ui <- renderUI({
    selectInput("file_unit", "Pick normalization unit:", choices = NULL)
  })
  
  data_reactive <- reactive({
    req(input$dataset, input$file_unit)
    list(
      Metadata = load_metadata(),
      Expression = load_expression(input$dataset, input$file_unit)
    )
  })
  
  expr_matrix <- reactive({
    expr <- data_reactive()$Expression
    df <- as.data.frame(expr)
    rownames(df) <- make.unique(df$Symbol)
    df$Symbol <- NULL
    mat <- as.matrix(df)
    mode(mat) <- "numeric"
    mat
  })
  
  selected_matrix <- reactive({
    mat <- expr_matrix()
    genes <- input$genes_list
    if (length(genes) > 0) {
      mat <- mat[rownames(mat) %in% genes, , drop = FALSE]
    }
    if (input$do_log) mat <- log2(mat + 0.001)
    if (input$do_zscore) mat <- t(scale(t(mat)))
    mat
  })
  
  metadata_for <- reactive({
    meta <- data_reactive()$Metadata
    expr <- data_reactive()$Expression
    samples <- colnames(expr)[-1]
    subset <- filter(meta, SampleID %in% samples)
    ordered <- subset[match(samples, subset$SampleID), ]
    ordered
  })
  
  create_combined <- function() {
    mat <- selected_matrix()
    df_combined <- data.frame()
    sample_row <- as.data.frame(t(colnames(mat)))
    names(sample_row) <- colnames(mat)
    rownames(sample_row) <- "SampleID"
    df_combined <- rbind(df_combined, sample_row)
    
    if (length(input$metadata_fields) > 0) {
      meta <- metadata_for()
      for (fld in input$metadata_fields) {
        vals <- meta[[fld]]
        row <- as.data.frame(t(vals))
        names(row) <- colnames(mat)
        rownames(row) <- fld
        df_combined <- rbind(df_combined, row)
      }
    }
    gene_df <- as.data.frame(mat)
    df_combined <- rbind(df_combined, gene_df)
    df_combined
  }
  
  # Outputs
  observe({
    mat <- selected_matrix()
    output$matrix_checks <- renderPrint({ cat(eval_checks(mat)) })
    output$boxplot <- renderPlot({
      par(mar = c(8,4,2,1))
      to_plot <- mat
      if (ncol(to_plot) > 30) {
        set.seed(123)
        cols <- sample(ncol(to_plot), 30)
        to_plot <- to_plot[, cols]
      }
      boxplot(as.data.frame(to_plot), main = "Gene expression distribution",
              xlab = "", ylab = "Expression", las = 2, outline = FALSE)
    })
    output$sample_sums <- renderPrint({ cat(sample_sums_text(mat)) })
  })
  
  observeEvent(list(input$generate_matrix, input$do_log, input$do_zscore), {
    output$matrix_table <- renderDT({
      combined <- create_combined()
      datatable(combined, options = list(scrollX = TRUE))
    })
  }, ignoreInit = TRUE)
  
  output$download_matrix <- downloadHandler(
    filename = function() paste0("matriz_", input$dataset, "_", input$file_unit, ".txt"),
    content = function(file) {
      # Registrar o download em log
      log_dir <- os_path("logs")
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE)
      }
      
      log_entry <- paste0(
        Sys.time(), " | ",
        "Dataset: ", input$dataset, " | ",
        "Unit: ", input$file_unit, " | ",
        "Genes selected: ", ifelse(length(input$genes_list) > 0, length(input$genes_list), "all"), " | ",
        "Log transform: ", input$do_log, " | ",
        "Z-score: ", input$do_zscore, " | ",
        "Metadata fields: ", ifelse(length(input$metadata_fields) > 0, 
                                    paste(input$metadata_fields, collapse = ", "), 
                                    "none"),
        "\n"
      )
      
      cat(log_entry, file = file.path(log_dir, "downloads.log"), append = TRUE)
      
      # Criar o arquivo para download
      write.table(create_combined(), file, sep = "\t", row.names = TRUE, col.names = NA, quote = FALSE)
    }
  )
}

shinyApp(ui, server)