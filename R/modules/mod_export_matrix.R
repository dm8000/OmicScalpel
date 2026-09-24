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

exportMatrixUI <- function(id) {
  ns <- NS(id)
  os_layout(
    left = tagList(
      os_panel(title = "Selection",
        uiOutput(ns("unit_ui")),
        selectizeInput(ns("genes_list"), "Select genes:", choices = NULL, multiple = TRUE),
        helpText("Leave empty to include all genes"),
        hr(),
        h4("Metadata options"),
        selectizeInput(ns("metadata_fields"), "Select metadata fields to include:", choices = NULL, multiple = TRUE),
        hr(),
        checkboxInput(ns("do_log"), "+0.001 and log transform", FALSE),
        checkboxInput(ns("do_zscore"), "Z-score normalization", FALSE)
      )
    ),
    center = tagList(
      os_panel(title = "Matrix",
        DTOutput(ns("matrix_table"))
      )
    ),
    right = tagList(
      os_panel(title = "Export",
        actionButton(ns("generate_matrix"), "Generate matrix"),
        downloadButton(ns("download_matrix"), "Download matrix")
      ),
      os_panel(title = "Checks",
        verbatimTextOutput(ns("matrix_checks")),
        plotOutput(ns("boxplot"), height = "250px"),
        hr(),
        h4("Expression sum"),
        verbatimTextOutput(ns("sample_sums"))
      )
    )
  )
}

exportMatrixServer <- function(id, ds, meta, go_to = NULL) {
  moduleServer(id, function(input, output, session) {
    observeEvent(ds(), {
      dataset <- ds()
      units <- list_units(dataset)
      updateSelectInput(session, "file_unit", choices = units)
      
      if (length(units) > 0) {
        expr <- load_expression(dataset, units[1])
        updateSelectizeInput(session, "genes_list", choices = expr$Symbol)
        
        current_samples <- colnames(expr)[-1]
        meta_df <- meta()
        meta_subset <- filter(meta_df, SampleID %in% current_samples)
        meta_opts <- setdiff(names(meta_df), "SampleID")
        valid_fields <- meta_opts[sapply(meta_opts, function(field) {
          any(meta_subset[[field]] != "NA")
        })]
        updateSelectizeInput(session, "metadata_fields", choices = valid_fields)
      }
    })
    
    output$unit_ui <- renderUI({
      selectInput(session$ns("file_unit"), "Pick normalization unit:", choices = NULL)
    })
    
    data_reactive <- reactive({
      req(ds(), input$file_unit)
      list(
        Metadata = meta(),
        Expression = load_expression(ds(), input$file_unit)
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
      meta_df <- meta()
      expr <- data_reactive()$Expression
      samples <- colnames(expr)[-1]
      subset <- filter(meta_df, SampleID %in% samples)
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
        meta_df <- metadata_for()
        for (fld in input$metadata_fields) {
          vals <- meta_df[[fld]]
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
      filename = function() paste0("matriz_", ds(), "_", input$file_unit, ".txt"),
      content = function(file) {
        log_download(
          dataset = ds(),
          unit = input$file_unit,
          genes_selected = ifelse(length(input$genes_list) > 0, length(input$genes_list), "all"),
          log_transform = input$do_log,
          z_score = input$do_zscore,
          metadata_fields = ifelse(length(input$metadata_fields) > 0, 
                                   paste(input$metadata_fields, collapse = ", "), 
                                   "none")
        )
        write.table(create_combined(), file, sep = "\t", row.names = TRUE, col.names = NA, quote = FALSE)
      }
    )
  })
}