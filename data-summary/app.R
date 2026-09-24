# Paths come from config/config.txt -- see R/config.R
source("../R/config.R")
Sys.setenv(R_LIBS = os_path("lib"))
Sys.setenv(R_LIBS_USER = os_path("lib"))
.libPaths(os_path("lib"))
library(shinydashboard)
library(readxl)
library(datamods)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(plotly)
library(forcats)
library(DT)
library(RColorBrewer)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Dataset Summary Editor"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      # Left Panel - Visualizations
      column(
        width = 6,
        box(
          width = NULL, status = "primary",
          selectInput(
            "dataset_selector", "Select Datasets:",
            choices = NULL, multiple = TRUE,
            selectize = TRUE
          )
        ),
        box(
          width = NULL, status = "info",
          title = "Sex Distribution",
          plotlyOutput("sex_pie", height = "300px")
        ),
        box(
          width = NULL, status = "info",
          title = "Age Distribution",
          plotlyOutput("age_hist", height = "300px")
        )
      ),
      
      # Right Panel - Data Editor
      column(
        width = 6,
        box(
          width = NULL, status = "primary",
          title = "Dataset Summary",
          # Legend for locked columns
          tags$div(
            style = "margin-bottom: 8px;",
            tags$span(
              style = "display:inline-block;width:12px;height:12px;
                       background:#AEC6CF;margin-right:5px;border:1px solid #ccc;"
            ),
            "Locked (non-editable) columns"
          ),
          DTOutput("summary_table"),
          br(),
          actionButton("save_button", "Save Changes", icon = icon("save"), 
                       class = "btn-success")
        )
      )
    ),
    tags$head(tags$style(HTML("
      .dataTable { width: 100% !important; }
      .dataTable td { padding: 6px !important; }
      .box { padding-bottom: 20px; }
    ")))
  )
)

server <- function(input, output, session) {
  # File paths
  data_file     <- os_path("hubdata", "Datasets_summary.xlsx")
  metadata_file <- os_path("hubdata", "Metadata.xlsx")
  data_folder   <- os_path("hubdata")
  backup_dir    <- os_path("backups")
  
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  summary_data      <- reactiveVal()
  metadata_reactive <- reactiveVal()
  
  # Check existence of normalized files
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
  
  # Aggregate metadata values
  aggregate_metadata_values <- function(metadata, dataset, column) {
    vals <- unique(metadata[metadata$dataset == dataset & !is.na(metadata[[column]]), column])
    if (length(vals) == 0) return(NA_character_)
    paste(vals, collapse = ", ")
  }
  
  # Load, process, backup & save summary
  observe({
    summary_df <- read_excel(data_file)
    metadata   <- read_excel(metadata_file) %>%
      mutate(dataset = as.character(dataset),
             Sex     = factor(Sex),
             Age     = suppressWarnings(as.numeric(Age)))
    metadata_reactive(metadata)
    
    unique_ds <- unique(metadata$dataset)
    rows_lst  <- lapply(unique_ds, function(ds) {
      existing   <- summary_df[summary_df$dataset == ds, , drop = FALSE]
      new_row    <- summary_df[0, , drop = FALSE]
      new_row[1, ] <- NA
      new_row$dataset     <- ds
      new_row$Sample_size <- sum(metadata$dataset == ds)
      
      # Fill metadata fields
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
        new_row[[out_col]] <- aggregate_metadata_values(metadata, ds, in_col)
      }
      
      # File existence
      chk <- check_file_existence(ds)
      for (nm in names(chk)) new_row[[nm]] <- chk[[nm]]
      
      # Preserve previous editable columns
      if (nrow(existing) > 0) {
        fixed <- c("dataset","Sample_size", names(cols_map), names(chk))
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
    
    # Backup & overwrite
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    bk <- file.path(backup_dir, paste0("Datasets_summary_backup_", ts, ".xlsx"))
    file.copy(data_file, bk)
    write.xlsx(final_df, data_file)
    showNotification(paste("Backup created:", bk), type = "message", duration = 5)
  }, priority = 1000)
  
  # Update dataset selector
  observe({
    md <- metadata_reactive(); req(md)
    updateSelectInput(session, "dataset_selector",
                      choices  = unique(md$dataset),
                      selected = unique(md$dataset))
  })
  
  # Filter metadata for plots
  filtered_meta <- reactive({
    md <- metadata_reactive(); req(md, input$dataset_selector)
    md %>%
      filter(dataset %in% input$dataset_selector) %>%
      mutate(Sex = fct_explicit_na(Sex, na_level = "(Missing)"))
  })
  
  # Sex pie
  output$sex_pie <- renderPlotly({
    df <- filtered_meta() %>% count(Sex, name = "Count") %>% mutate(Percentage = Count/sum(Count)*100)
    plot_ly(df, labels=~Sex, values=~Count, type="pie",
            textinfo="label+percent+value", hoverinfo="label+percent+value",
            marker=list(colors=brewer.pal(8,"Set2"), line=list(color="#FFF", width=1))) %>%
      layout(showlegend=FALSE, title=list(text="Sex Distribution", y=0.98), margin=list(t=40))
  })
  
  # Age histogram
  output$age_hist <- renderPlotly({
    p <- ggplot(filtered_meta(), aes(x=Age)) +
      geom_histogram(fill="#66C2A5", color="white", bins=30) +
      labs(x="Age", y="Count") +
      theme_minimal() +
      theme(plot.title=element_text(face="bold"))
    ggplotly(p) %>% config(displayModeBar=FALSE)
  })
  
  # Render editable DT with styled locked columns
  output$summary_table <- renderDT({
    df <- summary_data(); req(df)
    # define non-editable names and indices
    non_edit <- c("dataset", "Sample_size",
                  "Fellow who generated/uploaded dataset",
                  "Species", "Cell.type", "Tissue", "Strain",
                  "Anatomical_region", "Data_avaiability", "Data.type",
                  "Has.TMM.normalized.data", "Has.TPM.normalized.data",
                  "Has.count.data", "Has.Combat.batch.corrected.data")
    non_idx <- which(names(df) %in% non_edit) - 1
    
    datatable(
      df,
      editable = list(target="cell", disable=list(columns=non_idx)),
      options = list(pageLength=-1, dom="t", ordering=TRUE,
                     scrollY="600px", scrollX=TRUE),
      rownames=FALSE
    ) %>%
      formatStyle(
        columns = names(df)[non_idx + 1],
        backgroundColor = "#AEC6CF"
      )
  })
  
  # Handle in-table edits
  observeEvent(input$summary_table_cell_edit, {
    info <- input$summary_table_cell_edit
    df   <- summary_data()
    df[info$row, info$col + 1] <- info$value
    summary_data(df)
  })
  
  # Manual save with backup
  observeEvent(input$save_button, {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    bk <- file.path(backup_dir, paste0("Datasets_summary_backup_", ts, ".xlsx"))
    file.copy(data_file, bk)
    write.xlsx(summary_data(), data_file)
    showNotification(paste("Manual save done. Backup at", bk), type="message", duration=5)
  })
}

shinyApp(ui = ui, server = server)
