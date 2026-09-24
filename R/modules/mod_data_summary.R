dataSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML("
      .dataTable { width: 100% !important; }
      .dataTable td { padding: 6px !important; }
      .box { padding-bottom: 20px; }
    "))),
    fluidRow(
      column(
        width = 3,
        box(
          width = NULL, status = "primary",
          selectInput(ns("dataset_selector"), "Select Datasets:",
            choices = NULL, multiple = TRUE, selectize = TRUE
          )
        ),
        box(
          width = NULL, status = "info",
          title = "Sex Distribution",
          plotlyOutput(ns("sex_pie"), height = "300px")
        ),
        box(
          width = NULL, status = "info",
          title = "Age Distribution",
          plotlyOutput(ns("age_hist"), height = "300px")
        )
      ),
      column(
        width = 9,
        box(
          width = NULL, status = "primary",
          title = "Dataset Summary",
          tags$div(
            style = "margin-bottom: 8px;",
            tags$span(
              style = "display:inline-block;width:12px;height:12px;background:#AEC6CF;margin-right:5px;border:1px solid #ccc;"
            ),
            "Locked (non-editable) columns"
          ),
          DTOutput(ns("summary_table")),
          helpText("Select a row, then open that dataset in:"),
          tagList(
            actionButton(ns("goto_compare_genes"), "Compare genes"),
            actionButton(ns("goto_compare_samples"), "Compare samples"),
            actionButton(ns("goto_correlation_analysis"), "Correlation"),
            actionButton(ns("goto_export_matrix"), "Export matrix"),
            actionButton(ns("goto_metadata_editor"), "Edit metadata"),
            actionButton(ns("goto_cutoff_maker"), "Cutoffs")
          ),
          br(),
          actionButton(ns("save_button"), "Save Changes", icon = icon("save"), class = "btn-success")
        )
      )
    )
  )
}

dataSummaryServer <- function(id, ds, meta, go_to = NULL) {
  moduleServer(id, function(input, output, session) {
    data_folder <- os_path("hubdata")
    backup_dir  <- os_path("backups")
    if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
    
    summary_data     <- reactiveVal()
    original_summary <- reactiveVal()
    
    check_file_existence <- function(dataset) {
      check_files <- c(
        TMM    = paste0(dataset, "_TMM.txt"),
        TPM    = paste0(dataset, "_TPM.txt"),
        counts = paste0(dataset, "_counts.txt"),
        combat = paste0(dataset, "_combat.txt")
      )
      exists <- sapply(check_files, function(f) {
        any(file.exists(file.path(data_folder, dataset, f)))
      })
      c(
        "Has.TMM.normalized.data"         = if (exists["TMM"])    "Y" else "N",
        "Has.TPM.normalized.data"         = if (exists["TPM"])    "Y" else "N",
        "Has.count.data"                  = if (exists["counts"]) "Y" else "N",
        "Has.Combat.batch.corrected.data" = if (exists["combat"]) "Y" else "N"
      )
    }
    
    aggregate_metadata_values <- function(metadata, dataset, column) {
      vals <- unique(metadata[metadata$dataset == dataset & !is.na(metadata[[column]]), column])
      if (length(vals) == 0) return(NA_character_)
      paste(vals, collapse = ", ")
    }
    
    observe({
      summary_df <- load_datasets_summary()
      original_summary(summary_df)
      
      md <- meta()
      if (!is.null(md)) {
        md <- md %>%
          mutate(dataset = as.character(dataset),
                 Sex     = factor(Sex),
                 Age     = suppressWarnings(as.numeric(Age)))
      }
      
      unique_ds <- unique(md$dataset)
      rows_lst  <- lapply(unique_ds, function(ds) {
        existing   <- summary_df[summary_df$dataset == ds, , drop = FALSE]
        new_row    <- summary_df[0, , drop = FALSE]
        new_row[1, ] <- NA
        new_row$dataset     <- ds
        new_row$Sample_size <- sum(md$dataset == ds)
        
        cols_map <- c(
          "Fellow who generated/uploaded dataset" = "Author",
          "Species"            = "Species",
          "Cell.type"          = "Cell.type",
          "Tissue"             = "Tissue",
          "Strain"             = "Strain",
          "Anatomical_region"  = "Anatomical_region",
          "Data_avaiability"   = "Data_avaiability",
          "Data.type"          = "Data.type"
        )
        for (out_col in names(cols_map)) {
          in_col <- cols_map[[out_col]]
          new_row[[out_col]] <- aggregate_metadata_values(md, ds, in_col)
        }
        
        chk <- check_file_existence(ds)
        for (nm in names(chk)) new_row[[nm]] <- chk[[nm]]
        
        if (nrow(existing) > 0) {
          fixed <- c("dataset", "Sample_size", names(cols_map), names(chk))
          editable <- setdiff(names(summary_df), fixed)
          new_row[editable] <- existing[editable]
        }
        new_row
      })
      
      final_df <- bind_rows(rows_lst) %>%
        mutate(across(
          c(Species, Cell.type, Tissue, Strain,
            Anatomical_region, Data_avaiability, Data.type),
          as.character
        ))
      summary_data(final_df)
      
      showNotification("Data loaded.", type = "message", duration = 5)
    }, priority = 1000)
    
    observe({
      md <- meta(); req(md)
      updateSelectInput(session, "dataset_selector",
                        choices  = unique(md$dataset),
                        selected = unique(md$dataset))
    })
    
    filtered_meta <- reactive({
      md <- meta(); req(md, input$dataset_selector)
      md %>%
        filter(dataset %in% input$dataset_selector) %>%
        mutate(Sex = fct_explicit_na(Sex, na_level = "(Missing)"))
    })
    
    output$sex_pie <- renderPlotly({
      df <- filtered_meta() %>% count(Sex, name = "Count") %>% mutate(Percentage = Count/sum(Count)*100)
      plot_ly(df, labels = ~Sex, values = ~Count, type = "pie",
              textinfo = "label+percent+value", hoverinfo = "label+percent+value",
              marker = list(colors = brewer.pal(8, "Set2"), line = list(color = "#FFF", width = 1))) %>%
        layout(showlegend = FALSE, title = list(text = "Sex Distribution", y = 0.98), margin = list(t = 40))
    })
    
    output$age_hist <- renderPlotly({
      p <- ggplot(filtered_meta(), aes(x = Age)) +
        geom_histogram(fill = "#66C2A5", color = "white", bins = 30) +
        labs(x = "Age", y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"))
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$summary_table <- renderDT({
      df <- summary_data(); req(df)
      non_edit <- c("dataset", "Sample_size",
                    "Fellow who generated/uploaded dataset",
                    "Species", "Cell.type", "Tissue", "Strain",
                    "Anatomical_region", "Data_avaiability", "Data.type",
                    "Has.TMM.normalized.data", "Has.TPM.normalized.data",
                    "Has.count.data", "Has.Combat.batch.corrected.data")
      non_idx <- which(names(df) %in% non_edit) - 1
      
      datatable(
        df,
        editable = list(target = "cell", disable = list(columns = non_idx)),
        selection = "single",
        options = list(pageLength = -1, dom = "t", ordering = TRUE,
                       scrollY = "600px", scrollX = TRUE),
        rownames = FALSE
      ) %>%
        formatStyle(
          columns = names(df)[non_idx + 1],
          backgroundColor = "#AEC6CF"
        )
    })
    
    observeEvent(input$summary_table_cell_edit, {
      info <- input$summary_table_cell_edit
      df   <- summary_data()
      df[info$row, info$col + 1] <- info$value
      summary_data(df)
    })
    
    observeEvent(input$save_button, {
      df_edited <- summary_data()
      df_orig   <- original_summary()
      save_datasets_summary(df_orig, df_edited)
      log_download(action = "save_summary")
      showNotification("Manual save done.", type = "message", duration = 5)
    })
    
    observeEvent(input$goto_compare_genes, {
      sel <- input$summary_table_rows_selected
      if (length(sel) != 1) { showNotification("Select a dataset row first.", type = "warning"); return(invisible(NULL)) }
      d <- summary_data()[sel, "dataset", drop = TRUE]
      if (length(list_units(d)) == 0) { showNotification(paste(d, "has no expression matrix."), type = "warning"); return(invisible(NULL)) }
      if (!is.null(go_to)) go_to("compare_genes", d)
    })
    observeEvent(input$goto_compare_samples, {
      sel <- input$summary_table_rows_selected
      if (length(sel) != 1) { showNotification("Select a dataset row first.", type = "warning"); return(invisible(NULL)) }
      d <- summary_data()[sel, "dataset", drop = TRUE]
      if (length(list_units(d)) == 0) { showNotification(paste(d, "has no expression matrix."), type = "warning"); return(invisible(NULL)) }
      if (!is.null(go_to)) go_to("compare_samples", d)
    })
    observeEvent(input$goto_correlation_analysis, {
      sel <- input$summary_table_rows_selected
      if (length(sel) != 1) { showNotification("Select a dataset row first.", type = "warning"); return(invisible(NULL)) }
      d <- summary_data()[sel, "dataset", drop = TRUE]
      if (length(list_units(d)) == 0) { showNotification(paste(d, "has no expression matrix."), type = "warning"); return(invisible(NULL)) }
      if (!is.null(go_to)) go_to("correlation_analysis", d)
    })
    observeEvent(input$goto_export_matrix, {
      sel <- input$summary_table_rows_selected
      if (length(sel) != 1) { showNotification("Select a dataset row first.", type = "warning"); return(invisible(NULL)) }
      d <- summary_data()[sel, "dataset", drop = TRUE]
      if (length(list_units(d)) == 0) { showNotification(paste(d, "has no expression matrix."), type = "warning"); return(invisible(NULL)) }
      if (!is.null(go_to)) go_to("export_matrix", d)
    })
    observeEvent(input$goto_metadata_editor, {
      sel <- input$summary_table_rows_selected
      if (length(sel) != 1) { showNotification("Select a dataset row first.", type = "warning"); return(invisible(NULL)) }
      d <- summary_data()[sel, "dataset", drop = TRUE]
      if (!is.null(go_to)) go_to("metadata_editor", d)
    })
    observeEvent(input$goto_cutoff_maker, {
      sel <- input$summary_table_rows_selected
      if (length(sel) != 1) { showNotification("Select a dataset row first.", type = "warning"); return(invisible(NULL)) }
      d <- summary_data()[sel, "dataset", drop = TRUE]
      if (!is.null(go_to)) go_to("cutoff_maker", d)
    })
  })
}