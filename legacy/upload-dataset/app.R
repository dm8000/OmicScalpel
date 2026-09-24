options(shiny.maxRequestSize = 1000*1024^2)

# Paths come from config/config.txt -- see R/config.R
source("../../R/config.R")
Sys.setenv(R_LIBS = os_path("lib"))
Sys.setenv(R_LIBS_USER = os_path("lib"))
.libPaths(os_path("lib"))
options(shiny.maxRequestSize = 1000*1024^2)
library(shiny)
library(datamods)
library(readxl)
library(writexl)
library(DT)
library(magrittr)

file_path <- os_path("hubdata", "Metadata.xlsx")
datasets_summary_path <- os_path("hubdata", "Datasets_summary.xlsx")

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

load_data <- function() {
  tryCatch({
    read_excel(file_path)
  }, error = function(e) {
    data.frame(
      SampleID   = character(),
      dataset    = character(),
      Data.type  = character(),
      Author     = character(),
      TsengID    = character(),
      stringsAsFactors = FALSE
    )
  })
}

load_datasets_summary <- function() {
  tryCatch({
    read_excel(datasets_summary_path)
  }, error = function(e) {
    data.frame(
      dataset = character(),
      Sample_size = numeric(),
      Tissue = character(),
      Cell.type = character(),
      Strain = character(),
      Anatomical_region = character(),
      Species = character(),
      Data_avaiability = character(),
      Data.type = character(),
      `Fellow.who.generated/uploaded.dataset` = character(),
      `Description/observation` = character(),
      Treatment = character(),
      `Folder.Name.in.TsengLab/Datasets` = character(),
      Project = character(),
      `in.vivo.or.in.vitro` = character(),
      `Type.of.samples` = character(),
      `Date.of.collection` = character(),
      Samples = character(),
      Conditions = character(),
      replicates = character(),
      `Where.is.the.Omics.performed` = character(),
      `Data.location.&.ELN` = character(),
      publication = character(),
      `Has.TMM.normalized.data` = character(),
      `Has.TPM.normalized.data` = character(),
      `Has.count.data` = character(),
      `Has.Combat.batch.corrected.data` = character(),
      `Fellow who generated/uploaded dataset` = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
}

save_data <- function(original_data, edited_data) {
  backup_folder <- os_path("backups")
  if (!dir.exists(backup_folder)) {
    dir.create(backup_folder, recursive = TRUE)
  }
  timestamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_file <- file.path(backup_folder, paste0("Metadata_backup_", timestamp, ".xlsx"))
  write_xlsx(original_data, backup_file)
  write_xlsx(edited_data,   file_path)
}

save_datasets_summary <- function(original_data, edited_data) {
  backup_folder <- os_path("backups")
  if (!dir.exists(backup_folder)) {
    dir.create(backup_folder, recursive = TRUE)
  }
  timestamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_file <- file.path(backup_folder, paste0("Datasets_summary_backup_", timestamp, ".xlsx"))
  write_xlsx(original_data, backup_file)
  write_xlsx(edited_data, datasets_summary_path)
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$style(HTML(
      ".tooltip-inner { max-width: 600px; text-align: left; }
      .required-field { color: #d9534f; font-weight: bold; }
      .inline-help-icon { margin-left: 5px; cursor: pointer; color: #5bc0de; }
      .readonly-input { background-color: #f9f9f9; }
      .readonly-input input { background-color: #f9f9f9 !important; }
      .hidden-field { display: none !important; }
      .legend-required { margin-top: 10px; color: #d9534f; font-style: italic; }"
    )),
    tags$script(HTML(
      "$(document).ready(function(){
         $('[data-toggle=\"tooltip\"]').tooltip({ html: true, container: 'body' });
       });"
    ))
  ),
  
  titlePanel("Upload a new dataset"),
  
  sidebarLayout(
    sidebarPanel(
      # Data upload section
      tags$div(style = "margin-top:15px; margin-bottom:5px;",
               tags$strong("Upload Data Files")
      ),
      fileInput("upload_data_files", "Select data files (.txt)", 
                accept = ".txt", multiple = TRUE),
      
      tags$hr(),
      
      # Build metadata table section was already removed
      
      verbatimTextOutput("debug_output")
    ),
    
    mainPanel(
      tags$div(
        style = "padding: 20px; text-align: center; color: #666;",
        tags$h4("Upload one or multiple data files"),
        tags$p()
      )
    )
  )
) 

server <- function(input, output, session) {
  
  runjs <- function(code) {
    session$sendCustomMessage(type = 'jsCode', message = code)
  }
  
  rv <- reactiveValues(
    data                = NULL,
    dataset_list        = NULL,
    new_metadata        = NULL,
    custom_columns      = list(),
    custom_column_count = 0,
    existing_columns    = NULL,
    data_modified       = FALSE,
    uploaded_data_files = NULL,
    extracted_samples   = NULL,
    filename_base       = NULL,
    data_upload_mode    = FALSE,
    dataset_info        = NULL
  )
  
  log_message <- function(message) {
    isolate({
      message_text <- paste0(message, "\n")
      cat(message_text)
    })
  }
  
  observe({
    log_message("Loading initial data")
    rv$data         <- load_data()
    rv$dataset_list <- unique(as.character(rv$data$dataset))
    rv$existing_columns <- colnames(rv$data)
    rv$data_modified <- FALSE
    isolate({
      log_message(paste("After initial load: data_modified =", rv$data_modified))
    })
  })
  
  output$debug_output <- renderPrint({
    if (!is.null(input$upload_file)) {
      cat("Uploaded file:", input$upload_file$name, "\n")
    }
    if (!is.null(rv$uploaded_data_files)) {
      cat("Data files uploaded:", nrow(rv$uploaded_data_files), "\n")
      cat("Filename base:", rv$filename_base, "\n")
    }
  })
  
  observeEvent(input$upload_data_files, {
    req(input$upload_data_files)
    
    files_info <- input$upload_data_files
    base_names <- sapply(files_info$name, function(fname) {
      name_no_ext <- sub("\\.txt$", "", fname)
      parts <- strsplit(name_no_ext, "_")[[1]]
      if (length(parts) > 1) {
        paste(parts[-length(parts)], collapse = "_")
      } else {
        name_no_ext
      }
    })
    
    unique_bases <- unique(base_names)
    
    if (length(unique_bases) > 1) {
      showNotification("Error: All files must have the same base filename (before the last underscore)", 
                       type = "error", duration = 10)
      return()
    }
    
    rv$filename_base <- unique_bases[1]
    all_samples <- list()
    valid_files <- list()
    
    for (i in seq_len(nrow(files_info))) {
      file_path_local <- files_info$datapath[i]
      file_name <- files_info$name[i]
      
      tryCatch({
        data_file <- read.table(file_path_local, header = TRUE, sep = "\t", 
                                stringsAsFactors = FALSE, check.names = FALSE)
        
        if (names(data_file)[1] != "Symbol") {
          showNotification(paste("Error in", file_name, ": First column must be 'Symbol'"), 
                           type = "error", duration = 10)
          return()
        }
        
        sample_names <- names(data_file)[-1]
        all_samples[[file_name]] <- sample_names
        valid_files[[file_name]] <- list(path = file_path_local, data = data_file)
        
      }, error = function(e) {
        showNotification(paste("Error reading", file_name, ":", e$message), 
                         type = "error", duration = 10)
        return()
      })
    }
    
    if (length(all_samples) > 1) {
      first_samples <- all_samples[[1]]
      for (i in 2:length(all_samples)) {
        if (!identical(sort(first_samples), sort(all_samples[[i]]))) {
          showNotification("Error: Sample names are not consistent across all uploaded files", 
                           type = "error", duration = 10)
          return()
        }
      }
    }
    
    rv$uploaded_data_files <- valid_files
    rv$extracted_samples <- all_samples[[1]]
    
    showNotification(paste("Successfully loaded", length(valid_files), "data files with", 
                           length(rv$extracted_samples), "samples"), 
                     type = "message", duration = 5)
    
    showDatasetInfoModal()
  })
  
  showDatasetInfoModal <- function() {
    # Predefine the long vector of options for Data Type:
    data_type_choices <- c(
      "RNAseq",
      "Single.Cell.RNA.seq",
      "Small.RNA.seq",
      "miRNA.seq",
      "Ribo.seq",
      "Microarray",
      "ChIP.seq",
      "ATAC.seq",
      "DNase.seq",
      "MNase.seq",
      "Mixture.Nuclease.seq",
      "Hi.C",
      "Capture.Hi.C",
      "3C",
      "4C",
      "5C",
      "Bisulfite.seq",
      "RRBS",
      "WGBS",
      "Mass.Spectrometry.Proteomics",
      "LC.MS.Proteomics",
      "SWATH.MS",
      "TMT.Quantitative.Proteomics",
      "SILAC.Quantitative.Proteomics",
      "Imaging.Mass.Spectrometry",
      "LC.MS.Metabolomics",
      "GC.MS.Metabolomics",
      "NMR.Metabolomics",
      "LC.MS.Lipidomics",
      "GC.MS.Lipidomics",
      "LC.MS.Glycomics",
      "Flow.Cytometry.Cytomics",
      "Single.Cell.Proteomics.MS",
      "Spatial.Transcriptomics",
      "Spatial.Proteomics",
      "Signal.Lipidomics"
    )
    
    showModal(modalDialog(
      title = "Dataset Information",
      size = "l",
      
      # Begin modal body
      fluidPage(
        tags$div(
          style = "background-color:#e7f3ff; padding:15px; border-radius:4px; margin-bottom:20px;",
          tags$h4("Dataset:", rv$filename_base),
          tags$p("Number of samples:", length(rv$extracted_samples)),
          tags$p("Sample names:", paste(rv$extracted_samples[1:min(5, length(rv$extracted_samples))], 
                                        collapse = ", "),
                 if(length(rv$extracted_samples) > 5) "..." else "")
        ),
        
        # Required fields legend
        tags$div(class = "legend-required", "* Required fields"),
        
        # Row 1: Data Type (dropdown) and Author (text)
        fluidRow(
          column(
            6,
            # Data Type label + asterisk + tooltip
            tags$label(
              "Data Type", tags$span("*", class = "required-field"),
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Select the omics data type from the list.")
            ),
            selectInput(
              "data_type_input", NULL,
              choices = data_type_choices,
              selected = "",
              width = "100%"
            )
          ),
          column(
            6,
            # Author label + asterisk + tooltip
            tags$label(
              "Author", tags$span("*", class = "required-field"),
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Who's responsible for this dataset?")
            ),
            textInput("author_input", NULL, value = "")
          )
        ),
        
        # Row 2: Fellow + Description
        fluidRow(
          column(
            6,
            tags$label(
              "Fellow who generated/uploaded the dataset",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Who are you?")
            ),
            textInput("fellow_input", NULL, value = "")
          ),
          column(
            6,
            tags$label(
              "Description/observation",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Any additional notes or observations about the dataset or samples.")
            ),
            textInput("description_input", NULL, value = "")
          )
        ),
        
        # Row 3: Treatment + (Folder name field removed entirely)
        fluidRow(
          column(
            6,
            tags$label(
              "Treatment",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Describe what was done to the cells or animals (e.g., drug, dose, time).")
            ),
            textInput("treatment_input", NULL, value = "")
          )
        ),
        
        # Row 4: Project + in.vivo.or.in.vitro
        fluidRow(
          column(
            6,
            tags$label(
              "Project",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Which grant or project funded this dataset?")
            ),
            textInput("project_input", NULL, value = "")
          ),
          column(
            6,
            tags$label(
              "in.vivo.or.in.vitro",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Self explanatory")
            ),
            textInput("invivo_input", NULL, value = "")
          )
        ),
        
        # Row 5: Type of samples + Date of collection
        fluidRow(
          column(
            6,
            tags$label(
              "Type of samples",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Provide a brief description of the sample type (e.g., tissue, cell line).")
            ),
            textInput("sample_type_input", NULL, value = "")
          ),
          column(
            6,
            tags$label(
              "Date of collection",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Enter the date when samples were collected (YYYY-MM-DD).")
            ),
            textInput("date_input", NULL, value = "")
          )
        ),
        
        # Row 6: Samples + Conditions
        fluidRow(
          column(
            6,
            tags$label(
              "Samples",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "List any other observations about the samples.")
            ),
            textInput("samples_input", NULL, value = "")
          ),
          column(
            6,
            tags$label(
              "Conditions",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Describe experimental conditions (e.g., Cold, Fat, Exercised).")
            ),
            textInput("conditions_input", NULL, value = "")
          )
        ),
        
        # Row 7: Replicates + Where is the Omics performed
        fluidRow(
          column(
            6,
            tags$label(
              "Replicates",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "How many biological replicates per group?")
            ),
            textInput("replicates_input", NULL, value = "")
          ),
          column(
            6,
            tags$label(
              "Where is the Omics performed",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Institution or core facility where data generation took place.")
            ),
            textInput("omics_location_input", NULL, value = "")
          )
        ),
        
        # Row 8: Data location & ELN + Publication
        fluidRow(
          column(
            6,
            tags$label(
              "Data location & ELN",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Where are the raw data stored, and what is the Biovia electronic notebook ID of this experiment?")
            ),
            textInput("data_location_input", NULL, value = "")
          ),
          column(
            6,
            tags$label(
              "Publication",
              icon("question-circle", class = "inline-help-icon", style = "color: black;", `data-toggle` = "tooltip",
                   title = "Enter DOI, PMID, or other reference to the associated publication.")
            ),
            textInput("publication_input", NULL, value = "")
          )
        )
        
      ), # End modal body
      
      footer = tagList(
        tags$div(class = "legend-required", "* Required fields"),
        modalButton("Cancel"),
        actionButton("confirm_dataset_info", "Continue to Metadata", 
                     style = "background-color:#5cb85c; color:#fff;")
      )
    ))
  }
  
  observeEvent(input$confirm_dataset_info, {
    # Check required fields
    if (!nzchar(input$data_type_input) || !nzchar(input$author_input)) {
      showNotification("Data Type and Author are required fields", type = "error")
      return()
    }
    
    rv$dataset_info <- list(
      data_type  = input$data_type_input,
      author     = input$author_input,
      fellow     = input$fellow_input,
      description= input$description_input,
      treatment  = input$treatment_input,
      # folder is already locked, so we still capture it
      folder     = rv$filename_base,
      project    = input$project_input,
      invivo     = input$invivo_input,
      sample_type= input$sample_type_input,
      date       = input$date_input,
      samples    = input$samples_input,
      conditions = input$conditions_input,
      replicates = input$replicates_input,
      omics_location = input$omics_location_input,
      data_location   = input$data_location_input,
      publication     = input$publication_input
    )
    
    rv$data_upload_mode <- TRUE
    removeModal()
    showPrefilledMetadataModal()
  })
  
  showPrefilledMetadataModal <- function() {
    rv$custom_columns      <- list()
    rv$custom_column_count <- 0
    
    showModal(modalDialog(
      title = "Add Metadata (Data Upload Mode)",
      size  = "l",
      
      fluidPage(
        tags$div(
          style = "background-color:#fff3cd; padding:10px; border-radius:4px; margin-bottom:15px;",
          tags$p(tags$strong("Note:"), "The following fields are pre-filled from your data upload and cannot be modified.")
        ),
        
        # Hide the following four fields completely (Number of Samples, Dataset, Data Type, Author):
        fluidRow(
          column(
            6, class = "hidden-field",
            numericInput("meta_num_samples", "Number of Samples", 
                         value = length(rv$extracted_samples), min = 1, max = 100)
          ),
          column(
            6, class = "hidden-field",
            textInput("meta_dataset", "Dataset", value = rv$filename_base)
          )
        ),
        fluidRow(
          column(
            6, class = "hidden-field",
            textInput("meta_data_type", "Data Type", value = rv$dataset_info$data_type)
          ),
          column(
            6, class = "hidden-field",
            textInput("meta_author", "Author", value = rv$dataset_info$author)
          )
        ),
        
        # Allow user to add existing or new classifier columns:
        fluidRow(
          column(
            4,
            selectInput("meta_show_column", "Current Classifiers", 
                        choices = c("Select column" = "", rv$existing_columns), selectize = FALSE)
          ),
          column(
            4,
            textInput("meta_default_value", "Default Value", value = "NA")
          ),
          column(
            4,
            actionButton("meta_add_existing_col", "Add classifier", style = "margin-top:25px;")
          )
        ),
        fluidRow(
          column(
            4,
            textInput("meta_new_col_name", "New Classifier")
          ),
          column(
            4,
            textInput("meta_new_col_default", "Default Value", value = "NA")
          ),
          column(
            4,
            actionButton("meta_add_new_col", "Add new classifier", style = "margin-top:25px;")
          )
        ),
        DTOutput("meta_preview_table"),
        tags$hr(),
        tags$div(style = "background-color:#dff0d8; padding:15px; border-radius:4px;",
                 tags$p(tags$strong("Instructions:")),
                 tags$ol(
                   tags$li("Pre-filled fields cannot be changed"),
                   tags$li("Sample IDs are automatically set from your data files"),
                   tags$li("Add classifiers and edit values if needed"),
                   tags$li("Try to use Current Classifiers first before adding a new"),
                   tags$li("Avoid special characters and spaces if possible, giving preference to dot (.)"),
                   tags$li("Classifiers need to be clear and CLARIFY THE UNIT. USE: Body.weight(kg). DON'T USE: BW."),
                   tags$li("Click Confirm to process and save all data")
                 )
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmDataUpload", "Confirm", style = "background-color:#5cb85c; color:#fff;")
      )
    ))
    
    # Disable editing on hidden fields if ever shown, just in case
    runjs("
      $('#meta_num_samples').prop('disabled', true);
      $('#meta_dataset').prop('disabled', true);
      $('#meta_data_type').prop('disabled', true);
      $('#meta_author').prop('disabled', true);
    ")
    
    observe({
      if (!is.null(input$meta_preview_table_cell_edit)) {
        info <- input$meta_preview_table_cell_edit
        coln <- colnames(rv$new_metadata)[info$col]
        
        if (rv$data_upload_mode) {
          # Lock columns 0: SampleID, 1: dataset, 2: Author, 3: Data.type, 4: TsengID
          if (info$col %in% c(0, 1, 2, 3, 4)) {
            showNotification("This field cannot be modified in data upload mode", type = "warning", duration = 3)
            return()
          }
        } else {
          # In non-upload mode, protect TsengID column
          if (coln == "TsengID") {
            showNotification("TsengID cannot be modified", type = "warning", duration = 3)
            return()
          }
        }
        
        rv$new_metadata[info$row, coln] <- info$value
      }
    })
  }
  
  # TsengID generation helper
  generate_tseng_ids <- function(new_metadata, existing_data) {
    max_numbers <- list()
    
    if (!is.null(existing_data) &&
        nrow(existing_data) > 0 &&
        "TsengID" %in% names(existing_data)) {

      existing_data$TsengID <- as.character(existing_data$TsengID)

      tse_entries <- existing_data[grepl("^TSE\\d+\\.", existing_data$TsengID, perl = TRUE), ]
      
      if (nrow(tse_entries) > 0) {
        # Group by data type and find max number for each
        for (dtype in unique(tse_entries$Data.type)) {
          dtype_entries <- tse_entries[tse_entries$Data.type == dtype, ]
          
          numbers <- sapply(dtype_entries$TsengID, function(tid) {
            if (grepl("^TSE\\d+\\.", tid)) {
              num_match <- regmatches(tid, regexec("^TSE(\\d+)\\.", tid))
              if (length(num_match[[1]]) > 1) {
                return(as.numeric(num_match[[1]][2]))
              }
            }
            return(NA)
          })
          
          valid_numbers <- numbers[!is.na(numbers)]
          if (length(valid_numbers) > 0) {
            max_numbers[[dtype]] <- max(valid_numbers)
          }
        }
      }
    }

    # Ensure TsengID column exists in new_metadata before writing to it
    if (!"TsengID" %in% names(new_metadata)) new_metadata$TsengID <- NA_character_
    
    # Generate new TsengIDs
    for (i in seq_len(nrow(new_metadata))) {
      dtype <- new_metadata$Data.type[i]
      current_max <- max_numbers[[dtype]] %||% 0
      next_num <- current_max + 1
      new_metadata$TsengID[i] <- sprintf("TSE%05d.%s", next_num, dtype)
      max_numbers[[dtype]] <- next_num
    }
    
    return(new_metadata)
  }
  
  observeEvent(input$meta_add_existing_col, {
    if (nzchar(input$meta_show_column)) {
      rv$custom_column_count <- rv$custom_column_count + 1
      rv$custom_columns[[rv$custom_column_count]] <- list(
        name    = input$meta_show_column,
        default = input$meta_default_value
      )
    }
  })
  
  observeEvent(input$meta_add_new_col, {
    if (nzchar(input$meta_new_col_name)) {
      rv$custom_column_count <- rv$custom_column_count + 1
      rv$custom_columns[[rv$custom_column_count]] <- list(
        name    = input$meta_new_col_name,
        default = input$meta_new_col_default
      )
    }
  })
  
  observe({
    req(input$meta_dataset, input$meta_author, input$meta_data_type)
    if (rv$data_upload_mode && !is.null(rv$extracted_samples)) {
      sample_ids <- rv$extracted_samples
    } else {
      sample_ids <- paste0("Sample", seq_len(input$meta_num_samples))
    }
    
    df <- data.frame(
      SampleID  = sample_ids,
      dataset   = rv$filename_base,
      Author    = rv$dataset_info$author,
      Data.type = rv$dataset_info$data_type,
      TsengID   = "NA",
      stringsAsFactors = FALSE
    )
    for (i in seq_len(rv$custom_column_count)) {
      coln <- rv$custom_columns[[i]]$name
      colv <- rv$custom_columns[[i]]$default
      if (!coln %in% names(df)) df[[coln]] <- colv
    }
    if (!is.null(rv$data)) {
      for (col in setdiff(names(rv$data), names(df))) df[[col]] <- "NA"
    }
    rv$new_metadata <- generate_tseng_ids(df, rv$data)
  })
  
  output$meta_preview_table <- renderDT({
    req(rv$new_metadata)
    if (rv$data_upload_mode) {
      datatable(
        rv$new_metadata,
        editable = list(target = "cell", disable = list(columns = c(0,1,2,3,4))),
        options = list(pageLength = 5, scrollX = TRUE)
      )
    } else {
      # Find TsengID column index
      tseng_col_idx <- which(names(rv$new_metadata) == "TsengID") - 1  # 0-indexed for JS
      disabled_cols <- c(tseng_col_idx)
      
      datatable(
        rv$new_metadata,
        editable = list(target = "cell", disable = list(columns = disabled_cols)),
        options = list(pageLength = 5, scrollX = TRUE)
      )
    }
  })
  
  observeEvent(input$meta_preview_table_cell_edit, {
    info <- input$meta_preview_table_cell_edit
    coln <- colnames(rv$new_metadata)[info$col]
    rv$new_metadata[info$row, coln] <- info$value
  })
  
  observeEvent(input$confirmDataUpload, {
    req(rv$data_upload_mode, rv$filename_base, rv$uploaded_data_files, rv$dataset_info)
    
    data_folder <- file.path(os_path("hubdata"), rv$filename_base)
    if (dir.exists(data_folder)) {
      showNotification(paste("Error: Folder", rv$filename_base, "already exists in Dataset_curated/data"), 
                       type = "error", duration = 10)
      return()
    }
    dir.create(data_folder, recursive = TRUE)
    
    for (file_name in names(rv$uploaded_data_files)) {
      source_path <- rv$uploaded_data_files[[file_name]]$path
      dest_path <- file.path(data_folder, file_name)
      file.copy(source_path, dest_path, overwrite = TRUE)
    }
    
    log_message("Adding new metadata from data upload")
    rv$data_modified <- TRUE
    log_message("Setting data_modified to TRUE from data upload")
    
    all_cols <- names(rv$data)
    for (col in setdiff(all_cols, names(rv$new_metadata))) rv$new_metadata[[col]] <- "NA"
    rv$new_metadata <- rv$new_metadata[, all_cols, drop = FALSE]
    rv$data <- rbind(rv$data, rv$new_metadata)
    rv$dataset_list    <- unique(as.character(rv$data$dataset))
    rv$existing_columns <- colnames(rv$data)
    
    tryCatch({
      orig <- load_data()
      save_data(orig, rv$data)
      rv$data_modified <- FALSE
      log_message("After data upload save: Setting data_modified to FALSE")
    }, error = function(e) {
      log_message(paste("Metadata save error:", e$message))
      showNotification(paste0("❌ Metadata save failed:\n", e$message), type = "error", duration = 10)
      return()
    })
    
    tryCatch({
      datasets_summary <- load_datasets_summary()
      
      has_tmm   <- any(grepl("_TMM\\.txt$", names(rv$uploaded_data_files)))
      has_tpm   <- any(grepl("_TPM\\.txt$", names(rv$uploaded_data_files)))
      has_count <- any(grepl("_count\\.txt$|_counts\\.txt$", names(rv$uploaded_data_files), ignore.case = TRUE))
      has_combat<- any(grepl("_Combat\\.txt$|_combat\\.txt$", names(rv$uploaded_data_files), ignore.case = TRUE))
      
      new_row <-   data.frame(
        dataset = character(),
        Sample_size = numeric(),
        Tissue = character(),
        Cell.type = character(),
        Strain = character(),
        Anatomical_region = character(),
        Species = character(),
        Data_avaiability = character(),
        Data.type = character(),
        `Fellow.who.generated/uploaded.dataset` = character(),
        `Description/observation` = character(),
        Treatment = character(),
        `Folder.Name.in.TsengLab/Datasets` = character(),
        Project = character(),
        `in.vivo.or.in.vitro` = character(),
        `Type.of.samples` = character(),
        `Date.of.collection` = character(),
        Samples = character(),
        Conditions = character(),
        replicates = character(),
        `Where.is.the.Omics.performed` = character(),
        `Data.location.&.ELN` = character(),
        publication = character(),
        `Has.TMM.normalized.data` = character(),
        `Has.TPM.normalized.data` = character(),
        `Has.count.data` = character(),
        `Has.Combat.batch.corrected.data` = character(),
        `Fellow who generated/uploaded dataset` = character(),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      datasets_summary_updated <- rbind(datasets_summary, new_row)
      orig_summary <- load_datasets_summary()
      save_datasets_summary(orig_summary, datasets_summary_updated)
      
    }, error = function(e) {
      log_message(paste("Datasets summary save error:", e$message))
      showNotification(paste0("❌ Datasets summary save failed:\n", e$message), type = "error", duration = 10)
    })
    
    rv$data_upload_mode <- FALSE
    rv$uploaded_data_files <- NULL
    rv$extracted_samples <- NULL
    rv$filename_base <- NULL
    rv$dataset_info <- NULL
    
    removeModal()
    showNotification(paste("✔️ Successfully processed dataset:", rv$filename_base, 
                           "- Data files copied, metadata saved, and summary updated"), 
                     type = "message", duration = 8)
  })
  
  observeEvent(input$upload_file, {
    req(input$upload_file)
    id <- showNotification("Processing uploaded file...", type = "message", duration = NULL)
    
    isolate({
      log_message(paste("Upload started, current modification status:", rv$data_modified))
    })
    
    uploaded_df <- tryCatch({
      df <- read.table(
        input$upload_file$datapath,
        header      = TRUE,
        sep         = "\t",
        stringsAsFactors = FALSE,
        fill        = TRUE,
        quote       = "",
        comment.char= "",
        check.names = FALSE,
        na.strings  = c("NA", "", "N/A", "#N/A")
      )
      df[is.na(df)] <- "NA"
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
    
    if (is.null(uploaded_df) || nrow(uploaded_df) == 0) {
      removeNotification(id)
      showNotification("Could not read data or file is empty", type = "error", duration = 10)
      return()
    }
    
    req_cols <- c("SampleID", "dataset", "Data.type", "Author")
    missing <- setdiff(req_cols, names(uploaded_df))
    if (length(missing)) {
      removeNotification(id)
      showNotification(paste("Missing required columns:", paste(missing, collapse = ", ")), type = "error", duration = 10)
      return()
    }
    
    if (!"TsengID" %in% names(uploaded_df)) uploaded_df$TsengID <- "NA"
    
    na_idx <- uploaded_df$TsengID == "NA"
    if (any(na_idx)) {
      na_df    <- uploaded_df[na_idx, ]
      non_na_df<- uploaded_df[!na_idx, ]
      if (!is.null(rv$data) && "Data.type" %in% names(na_df)) {
        combined <- rv$data
        if (nrow(non_na_df)) {
          common <- intersect(names(combined), names(non_na_df))
          combined <- rbind(combined[, common, drop = FALSE], non_na_df[, common, drop = FALSE])
        }
        na_df <- generate_tseng_ids(na_df, combined)
      }
      uploaded_df <- if (nrow(non_na_df)) rbind(non_na_df, na_df) else na_df
    }
    
    if (!is.null(rv$data) && nrow(rv$data)) {
      for (col in setdiff(names(uploaded_df), names(rv$data))) rv$data[[col]] <- "NA"
      for (col in setdiff(names(rv$data), names(uploaded_df))) uploaded_df[[col]] <- "NA"
      uploaded_df <- uploaded_df[, names(rv$data), drop = FALSE]
    }
    
    new_ds <- unique(uploaded_df$dataset)[1]
    
    rv$data_modified <- TRUE
    log_message("Setting data_modified to TRUE from file upload")
    
    rv$data <- if (is.null(rv$data) || !nrow(rv$data)) uploaded_df else rbind(rv$data, uploaded_df)
    rv$dataset_list     <- unique(as.character(rv$data$dataset))
    rv$existing_columns <- names(rv$data)
    
    removeNotification(id)
    
    save_id <- showNotification("Saving uploaded metadata to file...", type = "message", duration = NULL)
    tryCatch({
      orig <- load_data()
      save_data(orig, rv$data)
      rv$data_modified <- FALSE
      log_message("After upload save: Setting data_modified to FALSE")
      removeNotification(save_id)
      showNotification(paste("✔️ Uploaded", nrow(uploaded_df), "rows for dataset:", new_ds, 
                             " and saved to file"), type = "message", duration = 5)
    }, error = function(e) {
      log_message(paste("Save error after upload:", e$message))
      removeNotification(save_id)
      showNotification(paste0("❌ Save failed after upload:\n", e$message), type = "error", duration = 10)
    })
  })
  
  observeEvent(input$save, {
    req(rv$data)
    showNotification("Starting save…", type="message")
    
    isolate({
      log_message(paste("Save button pressed, data_modified =", rv$data_modified))
    })
    
    orig <- load_data()
    tryCatch({
      isolate({
        log_message(paste("Current data rows:", nrow(rv$data)))
        log_message(paste("Original data rows:", nrow(orig)))
        log_message(paste("Save to path:", file_path))
      })
      
      save_data(orig, rv$data)
      rv$data_modified <- FALSE
      log_message("After save: Setting data_modified to FALSE")
      
      showNotification(
        paste0("✔️ Wrote ", nrow(rv$data), " rows to:\n", file_path),
        type="message", duration=5
      )
    }, error = function(e) {
      log_message(paste("Save error:", e$message))
      showNotification(
        paste0("❌ Save failed:\n", e$message),
        type="error", duration=10
      )
    })
  })
}

shinyApp(ui = ui, server = server)