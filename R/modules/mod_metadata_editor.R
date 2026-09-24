metadataEditorUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(
        ".tooltip-inner span[style*='color: red'] { color: #d9534f !important; }
.tooltip-inner .red-field { color: #d9534f !important; font-weight: bold; }
      .tooltip-inner { max-width: 600px; text-align: left; }
      .required-field { color: #d9534f; font-weight: bold; }
      .optional-field { color: #888; font-style: italic; }
      pre.data-example { background-color: #f5f5f5; padding: 10px; white-space: pre; overflow-x: auto; font-size: 13px; }
      .instructions-list { padding-left: 20px; }
      .instructions-list li { margin-bottom: 5px; }
      .inline-help-icon { margin-left: 5px; cursor: pointer; color: #5bc0de; }
      .file-input-label { display: flex; align-items: center; }"
      )),
      tags$script(HTML(
        "$(document).ready(function(){
        $('[data-toggle=\"tooltip\"]').tooltip({ html: true, container: 'body' });
      });"
      ))
    ),
    titlePanel("Edit, create or upload metadata"),
    fluidRow(
      column(width = 3,
        actionButton(ns("save"), "Save", icon = icon("save")),
        uiOutput(ns("existing_columns_ui")),
        checkboxInput(ns("toggle_na"), "Hide Columns with only 'NA'", value = TRUE),
        tags$label(
          class = "control-label file-input-label",
          "Upload tab separated .txt with your metadata",
          tags$span(
            icon("question-circle"), class = "inline-help-icon",
            `data-toggle` = "tooltip", `data-html` = "true", `data-placement` = "right",
            title = HTML("
<div style='text-align: left; max-width: 650px;'>
  <strong>Data Structure Example:</strong><br>
  <pre style='font-family: monospace; font-size: 13px; background: #f8f9fa; padding: 10px; border-radius: 4px; margin: 8px 0;'>
SampleID      <span class='red-field'>dataset</span> <span class='red-field'>Data.type</span> <span class='red-field'>Author</span> Cell.type        Lineage
A38.WAT.D0.1  A38       RNAseq      Diogo    white.adipocytes Primary
  </pre>
  <strong>Instructions:</strong> 
  <ol style='padding-left: 20px; margin-top: 10px;'> 
    <li>These fields are <span class='red-field'>required</span> (<span class='red-field'>SampleID</span>, <span class='red-field'>dataset</span>, <span class='red-field'>Data.type</span>, <span class='red-field'>Author</span>)</li>
    <li>SampleIDs must match your data matrix and be unique</li>
    <li>You can add your own classifiers (treatment, etc.)</li>
    <li>Use existing nomenclature where possible</li>
    <li>If external data, fill LABEID column with GSE ID—otherwise omit the column</li>
    <li>Save after uploading</li>
  </ol> 
</div>
"
          )
        )
      ),
      fileInput(ns("upload_file"), NULL, accept = ".txt"),
      verbatimTextOutput(ns("debug_output"))
      ),
      column(width = 9,
        DTOutput(ns("table"))
      )
    )
  )
}

metadataEditorServer <- function(id, ds, meta, go_to = NULL) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      data                = NULL,
      filtered_data       = NULL,
      dataset_list        = NULL,
      current_dataset     = NULL,
      new_metadata        = NULL,
      custom_columns      = list(),
      custom_column_count = 0,
      existing_columns    = NULL,
      data_modified       = FALSE
    )
    
    log_message <- function(message) {
      isolate({ cat(paste0(message, "\n")) })
    }
    
    observe({
      log_message("Loading initial data")
      rv$data         <- meta()
      rv$dataset_list <- unique(as.character(rv$data$dataset))
      rv$current_dataset <- ds()
      rv$existing_columns <- colnames(rv$data)
      rv$data_modified <- FALSE
      isolate({ log_message(paste("After initial load: data_modified =", rv$data_modified)) })
      if (!is.null(rv$current_dataset)) update_filtered_data()
    })
    
    update_filtered_data <- function() {
      req(rv$data, rv$current_dataset)
      if (!rv$current_dataset %in% rv$data$dataset) {
        showNotification(paste("Dataset", rv$current_dataset, "not found"), type = "warning")
        return()
      }
      subset_data <- rv$data[rv$data$dataset == rv$current_dataset, , drop = FALSE]
      if (isTRUE(input$toggle_na)) {
        non_na_cols   <- colnames(subset_data)[colSums(subset_data == "NA" | is.na(subset_data)) < nrow(subset_data)]
        essential     <- c("SampleID", "dataset", "Data.type", "Author", "LABEID")
        required_cols <- union(non_na_cols, intersect(essential, colnames(subset_data)))
        if (!is.null(input$existing_column) && input$existing_column != "") {
          required_cols <- union(required_cols, input$existing_column)
        }
        rv$filtered_data <- subset_data[, required_cols, drop = FALSE]
      } else {
        rv$filtered_data <- subset_data
      }
    }
    
    output$debug_output <- renderPrint({
      cat("Datasets:", paste(rv$dataset_list, collapse = ", "), "\n")
      cat("Current dataset:", rv$current_dataset, "\n")
      cat("Rows in database:", ifelse(is.null(rv$data), 0, nrow(rv$data)), "\n")
      cat("Data modified:", rv$data_modified, "\n")
      if (!is.null(input$upload_file)) {
        cat("Uploaded file:", input$upload_file$name, "\n")
      }
    })
    
    observeEvent(input$addCol, {
      new_col <- paste0("New_Col_", ncol(rv$data) + 1)
      rv$data[[new_col]] <- "NA"
      rv$existing_columns <- colnames(rv$data)
      rv$data_modified <- TRUE
      log_message(paste("Added column and set data_modified to TRUE"))
      update_filtered_data()
      showNotification(paste("Added new column:", new_col), type = "message")
    })
    observeEvent(input$existing_column, { update_filtered_data() })
    observeEvent(input$toggle_na,       { update_filtered_data() })
    
    output$table <- renderDT({ req(rv$filtered_data); datatable(rv$filtered_data, editable = FALSE) })
    
    observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      rv$data_modified <- TRUE
      rv$filtered_data[info$row, info$col] <- info$value
      col <- colnames(rv$filtered_data)[info$col]
      sid <- rv$filtered_data$SampleID[info$row]
      idx <- which(rv$data$SampleID == sid & rv$data$dataset == rv$current_dataset)
      if (length(idx)) rv$data[idx, col] <- info$value
    })
    
    generate_tseng_ids <- function(new_metadata, existing_data) {
      max_numbers <- list()
      if (!is.null(existing_data) && nrow(existing_data) > 0) {
        tse_entries <- existing_data[grepl("^TSE\\d+\\.", existing_data$LABEID, perl = TRUE), ]
        for (dtype in unique(tse_entries$Data.type)) {
          entries <- tse_entries[tse_entries$Data.type == dtype, ]
          numbers <- sapply(entries$LABEID, function(tid) {
            m <- regmatches(tid, regexec("^TSE(\\d+)\\.", tid))[[1]]
            if (length(m)>1) as.numeric(m[2]) else NA
          })
          valid <- numbers[!is.na(numbers)]
          if (length(valid)>0) max_numbers[[dtype]] <- max(valid)
        }
      }
      for (i in seq_len(nrow(new_metadata))) {
        dtype <- new_metadata$Data.type[i]
        cur   <- max_numbers[[dtype]] %||% 0
        nxt   <- cur + 1
        new_metadata$LABEID[i] <- sprintf("TSE%05d.%s", nxt, dtype)
        max_numbers[[dtype]]    <- nxt
      }
      new_metadata
    }
    
    ## — Modal and “Build metadata” code unchanged — ##
    ## (omitted here for brevity; identical to your original)   ##
    ## — End “Build metadata” region — ##
    
    observeEvent(input$upload_file, {
      req(input$upload_file)
      id <- showNotification("Processing uploaded file...", type = "message", duration = NULL)
      
      uploaded_df <- tryCatch({
        df <- read.table(
          input$upload_file$datapath, header = TRUE, sep = "\t",
          stringsAsFactors = FALSE, fill = TRUE, quote = "",
          comment.char = "", check.names = FALSE,
          na.strings = c("NA","", "N/A", "#N/A")
        )
        df[is.na(df)] <- "NA"
        for (col in names(df)) {
          df[[col]] <- as.character(df[[col]])
        }
        for (col in names(df)) {
          if (is.character(df[[col]]) && all(grepl("^$|^NA$|^[0-9,.-]+$", df[[col]]))) {
            df[[col]] <- gsub(",", ".", df[[col]])
          }
        }
        df
      }, error = function(e) {
        removeNotification(id)
        showNotification(paste("Error reading file:", e$message), type = "error", duration = 10)
        NULL
      })
      
      if (is.null(uploaded_df) || nrow(uploaded_df)==0) {
        removeNotification(id)
        showNotification("Could not read data or file is empty", type="error", duration=10)
        return()
      }
      
      req_cols <- c("SampleID","dataset","Data.type","Author")
      missing  <- setdiff(req_cols, names(uploaded_df))
      if (length(missing)) {
        removeNotification(id)
        showNotification(paste("Missing required columns:", paste(missing, collapse=", ")), type="error")
        return()
      }
      if (!"LABEID" %in% names(uploaded_df)) uploaded_df$LABEID <- "NA"
      
      if (!is.null(rv$data) && nrow(rv$data)) {
        for (i in seq_len(nrow(uploaded_df))) {
          if (uploaded_df$LABEID[i] == "NA") {
            old_idx <- which(
              rv$data$dataset  == uploaded_df$dataset[i] &
                rv$data$SampleID == uploaded_df$SampleID[i]
            )
            if (length(old_idx) == 1) {
              uploaded_df$LABEID[i] <- rv$data$LABEID[old_idx]
            }
          }
        }
      }
      
      na_idx <- uploaded_df$LABEID=="NA"
      if (any(na_idx)) {
        na_df    <- uploaded_df[na_idx, , drop=FALSE]
        non_na   <- uploaded_df[!na_idx, , drop=FALSE]
        combined <- rv$data
        if (nrow(non_na)) {
          common <- intersect(names(combined), names(non_na))
          combined <- rbind(combined[,common,drop=FALSE], non_na[,common,drop=FALSE])
        }
        na_df <- generate_tseng_ids(na_df, combined)
        uploaded_df <- if (nrow(non_na)) rbind(non_na, na_df) else na_df
      }
      
      if (!is.null(rv$data) && nrow(rv$data)) {
        for (col in names(rv$data)) {
          rv$data[[col]] <- as.character(rv$data[[col]])
        }
        for (col in setdiff(names(uploaded_df), names(rv$data))) rv$data[[col]] <- "NA"
        for (col in setdiff(names(rv$data), names(uploaded_df))) uploaded_df[[col]] <- "NA"
        uploaded_df <- uploaded_df[, names(rv$data), drop=FALSE]
      }
      
      new_ds <- unique(uploaded_df$dataset)[1]
      rv$data_modified <- TRUE
      
      merged <- rv$data
      for (i in seq_len(nrow(uploaded_df))) {
        row_i <- uploaded_df[i, , drop=FALSE]
        idx <- which(
          merged$SampleID == row_i$SampleID &
            merged$dataset  == row_i$dataset
        )
        if (length(idx) > 0) {
          if (row_i$LABEID == "NA") {
            row_i$LABEID <- merged$LABEID[idx[1]]
          }
          merged[idx[1], ] <- row_i
          if (length(idx) > 1) {
            merged <- merged[-idx[-1], ]
          }
        } else {
          merged <- rbind(merged, row_i)
        }
      }
      rv$data <- merged
      
      rv$dataset_list     <- unique(as.character(rv$data$dataset))
      rv$current_dataset  <- new_ds
      rv$existing_columns <- names(rv$data)
      removeNotification(id)
      
      save_id <- showNotification("Saving uploaded metadata to file...", type = "message", duration = NULL)
      tryCatch({
        orig <- meta()
        save_metadata(orig, rv$data)
        rv$data_modified <- FALSE
        removeNotification(save_id)
        showNotification(
          paste("✔️ Uploaded", nrow(uploaded_df), "rows for dataset:", new_ds, "and saved"),
          type="message", duration=5
        )
      }, error = function(e) {
        removeNotification(save_id)
        showNotification(paste0("❌ Save failed after upload:\n", e$message), type="error", duration=10)
      })
      
      update_filtered_data()
    })
    
    observeEvent(input$save, {
      req(rv$data)
      showNotification("Starting save…", type="message")
      isolate({ log_message(paste("Save button pressed, data_modified =", rv$data_modified)) })
      orig <- meta()
      tryCatch({
        isolate({
          log_message(paste("Current data rows:", nrow(rv$data)))
          log_message(paste("Original data rows:", nrow(orig)))
        })
        save_metadata(orig, rv$data)
        rv$data_modified <- FALSE
        log_message("After save: Setting data_modified to FALSE")
        showNotification(paste0("✔️ Wrote ", nrow(rv$data), " rows to metadata file"),
                         type="message", duration=5)
      }, error = function(e) {
        log_message(paste("Save error:", e$message))
        showNotification(paste0("❌ Save failed:\n", e$message), type="error", duration=10)
      })
    })
  })
}