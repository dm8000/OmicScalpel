dataSummaryUI <- function(id) {
  ns <- NS(id)
  os_layout(
    widths = c(3, 5, 4),

    # Left: the whole database, for the datasets you have selected.
    left = tagList(
      os_panel(title = "Selection", collapse = TRUE,
        selectInput(ns("dataset_selector"), "Select Datasets:",
          choices = NULL, multiple = TRUE, selectize = TRUE
        )
      ),
      os_panel(title = "Sex Distribution", collapse = TRUE,
        plotlyOutput(ns("sex_pie"), height = "260px")
      ),
      os_panel(title = "Age Distribution", collapse = TRUE,
        plotlyOutput(ns("age_hist"), height = "260px")
      )
    ),

    center = tagList(
      os_panel(title = "Dataset Summary",
        DTOutput(ns("summary_table")),
        helpText("Select a row to inspect and edit that dataset on the right."),
        actionButton(ns("load_dataset"), "Load dataset",
                     icon = icon("circle-check"), class = "btn-primary")
      )
    ),

    # Right: the one dataset selected in the table.
    right = tagList(
      os_panel(title = "Dataset distributions", collapse = TRUE,
        selectInput(ns("cat_var"), "Categorical", choices = NULL),
        plotlyOutput(ns("ds_pie"), height = "210px"),
        tags$hr(),
        selectInput(ns("num_var"), "Numeric", choices = NULL),
        plotlyOutput(ns("ds_hist"), height = "210px")
      ),
      os_panel(title = "Selected dataset", collapse = TRUE, class = "os-scroll os-editor",
        uiOutput(ns("row_editor")),
        actionButton(ns("save_button"), "Save Changes",
                     icon = icon("save"), class = "btn-success")
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
        mutate(Sex = fct_explicit_na(factor(Sex), na_level = "(Missing)"),
               # Age arrives as text; without this the histogram fails with
               # "stat_bin() requires a continuous x aesthetic", which plotly
               # reports to the browser as "Error: [object Object]". The
               # conversion existed, but only where the summary table is built.
               Age = suppressWarnings(as.numeric(as.character(Age))))
    })
    
    output$sex_pie <- renderPlotly({
      df <- filtered_meta() %>% count(Sex, name = "Count") %>% mutate(Percentage = Count/sum(Count)*100)
      os_plotly(
        plot_ly(df, labels = ~Sex, values = ~Count, type = "pie",
                textinfo = "label+percent+value", hoverinfo = "label+percent+value",
                marker = list(colors = os_palette(nrow(df)),
                              line = list(color = "#FFF", width = 1))) %>%
          layout(showlegend = FALSE, margin = list(t = 40)),
        title = "Sex Distribution"
      )
    })
    
    output$age_hist <- renderPlotly({
      p <- ggplot(filtered_meta(), aes(x = Age)) +
        geom_histogram(bins = 30) +
        labs(x = "Age", y = "Count") +
        os_theme() +
        theme(plot.title = element_text(face = "bold"))
      os_plotly(ggplotly(p))
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
      
      # Read-only: editing happens in the panel on the right, where a field has
      # its own label and room to breathe. It also removes the pale blue
      # formatStyle that marked locked columns -- DT writes that inline, so it
      # overrode the dark theme and made half the table unreadable.
      datatable(
        df,
        editable  = FALSE,
        selection = "single",
        options = list(pageLength = -1, dom = "t", ordering = TRUE,
                       scrollY = "600px", scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    # ---- the selected dataset, on the right -----------------------------

    # Which row of the table is selected, as a dataset name. Everything on the
    # right hangs off this; nothing on the right shows until a row is picked.
    selected_dataset <- reactive({
      sel <- input$summary_table_rows_selected
      df  <- summary_data()
      if (length(sel) != 1 || is.null(df)) return(NULL)
      as.character(df[sel, "dataset", drop = TRUE])
    })

    # That dataset's samples, from the metadata rather than the summary sheet.
    selected_meta <- reactive({
      d <- selected_dataset(); md <- meta()
      if (is.null(d) || is.null(md)) return(NULL)
      md[md$dataset == d, , drop = FALSE]
    })

    # Columns worth offering. A column is categorical when it has between two
    # and twenty distinct values and is not mostly numbers; numeric when enough
    # of its values parse as numbers. Both ignore the literal "NA" the
    # spreadsheet uses, or every column would look populated.
    real_values <- function(x) {
      v <- as.character(x)
      v[!is.na(v) & v != "NA" & nzchar(trimws(v))]
    }

    categorical_cols <- function(df) {
      keep <- vapply(names(df), function(cn) {
        v <- real_values(df[[cn]])
        if (length(v) < 2) return(FALSE)
        n <- length(unique(v))
        if (n < 2 || n > 20) return(FALSE)
        mostly_numeric <- mean(!is.na(suppressWarnings(as.numeric(v)))) > 0.8
        !mostly_numeric
      }, logical(1))
      sort(names(df)[keep])
    }

    numeric_cols <- function(df) {
      keep <- vapply(names(df), function(cn) {
        v <- real_values(df[[cn]])
        length(v) >= 3 && mean(!is.na(suppressWarnings(as.numeric(v)))) > 0.8 &&
          length(unique(v)) > 2
      }, logical(1))
      sort(names(df)[keep])
    }

    # The menus follow the dataset: a column that is empty for this dataset is
    # not offered, which is the point of building them per dataset.
    observeEvent(selected_meta(), {
      md <- selected_meta(); req(md)
      cats <- setdiff(categorical_cols(md), c("SampleID", "dataset"))
      nums <- setdiff(numeric_cols(md), c("SampleID", "dataset"))
      updateSelectInput(session, "cat_var", choices = cats,
                        selected = if ("Sex" %in% cats) "Sex" else cats[1])
      updateSelectInput(session, "num_var", choices = nums,
                        selected = if ("Age" %in% nums) "Age" else nums[1])
    })

    output$ds_pie <- renderPlotly({
      md <- selected_meta(); req(md, input$cat_var)
      v <- real_values(md[[input$cat_var]])
      req(length(v) > 0)
      df <- as.data.frame(table(value = v), stringsAsFactors = FALSE)
      os_plotly(
        plot_ly(df, labels = ~value, values = ~Freq, type = "pie",
                textinfo = "label+percent", hoverinfo = "label+percent+value",
                marker = list(colors = os_palette(nrow(df)),
                              line = list(color = "#FFF", width = 1))) %>%
          layout(showlegend = FALSE, margin = list(t = 34, b = 10, l = 10, r = 10)),
        title = input$cat_var
      )
    })

    output$ds_hist <- renderPlotly({
      md <- selected_meta(); req(md, input$num_var)
      v <- suppressWarnings(as.numeric(real_values(md[[input$num_var]])))
      v <- v[!is.na(v)]
      req(length(v) > 0)
      os_plotly(
        plot_ly(x = v, type = "histogram", nbinsx = 20,
                marker = list(color = OS_PLOT$accent,
                              line = list(color = "white", width = .5))) %>%
          layout(xaxis = list(title = input$num_var), yaxis = list(title = "Count"),
                 margin = list(t = 34, b = 40, l = 40, r = 10)),
        title = input$num_var
      )
    })

    # The selected row's fields, as labelled inputs. Derived columns are shown
    # but not editable: sample size and the has-this-file flags are computed
    # from the hub, so typing over them would be overwritten on the next read.
    # Columns the table recomputes on every load, so typing over them would be
    # thrown away at the next read. Two groups: what is counted from the hub
    # (sample size, which files exist) and what is summarised from the sample
    # metadata (species, tissue, cell type and the rest of cols_map). Editing
    # those means editing the metadata, in the Edit metadata tab.
    DERIVED <- c("dataset", "Sample_size",
                 "Has.TMM.normalized.data", "Has.TPM.normalized.data",
                 "Has.count.data", "Has.Combat.batch.corrected.data",
                 "Fellow who generated/uploaded dataset",
                 "Species", "Cell.type", "Tissue", "Strain",
                 "Anatomical_region", "Data_avaiability", "Data.type")

    output$row_editor <- renderUI({
      d <- selected_dataset()
      if (is.null(d)) {
        return(helpText("Select a row in the table to see and edit it here."))
      }
      df  <- summary_data()
      row <- df[df$dataset == d, , drop = FALSE][1, ]
      shown <- function(cn) {
        val <- as.character(row[[cn]])
        if (is.na(val)) "" else val
      }

      # The derived ones first, as one tight block: they are read-only, so
      # scattering them between the editable fields only made the fields
      # harder to scan.
      derived <- intersect(DERIVED, names(row))
      editable <- setdiff(names(row), derived)

      # Editable first: they are what this panel is for. The computed ones go
      # underneath, grouped and labelled, so it is clear why they cannot be
      # typed into rather than looking like fields that ignore you.
      tagList(
        tags$div(class = "os-section", d),
        lapply(editable, function(cn) {
          textInput(session$ns(paste0("f_", cn)), cn, value = shown(cn), width = "100%")
        }),
        tags$div(class = "os-section os-computed-head", "Computed \u2014 not editable"),
        tags$div(
          class = "os-readonly-block",
          lapply(derived, function(cn) {
            v <- shown(cn)
            tags$div(class = "os-readonly",
                     tags$span(class = "os-readonly-label", cn),
                     tags$span(class = "os-readonly-value",
                               if (nzchar(v)) v else "\u2014"))
          })
        )
      )
    })

    # Save collects the fields from the panel on the right back into the row
    # they came from. Only the selected dataset's row can change, so a save
    # cannot touch a dataset nobody was looking at.
    observeEvent(input$save_button, {
      d <- selected_dataset()
      if (is.null(d)) {
        showNotification("Select a dataset row first.", type = "warning")
        return(invisible(NULL))
      }
      df  <- summary_data()
      i   <- which(df$dataset == d)[1]
      if (is.na(i)) {
        showNotification(paste(d, "is no longer in the table."), type = "warning")
        return(invisible(NULL))
      }

      changed <- character(0)
      for (cn in setdiff(names(df), DERIVED)) {
        v <- input[[paste0("f_", cn)]]
        if (is.null(v)) next
        before <- as.character(df[i, cn, drop = TRUE])
        if (is.na(before)) before <- ""
        if (!identical(as.character(v), before)) {
          df[i, cn] <- v
          changed <- c(changed, cn)
        }
      }

      if (!length(changed)) {
        showNotification("Nothing changed.", duration = 3)
        return(invisible(NULL))
      }

      summary_data(df)
      save_datasets_summary(original_summary(), df)
      original_summary(df)
      log_download(action = "save_summary", dataset = d,
                   fields = paste(changed, collapse = ", "))
      showNotification(paste0("Saved ", d, ": ", paste(changed, collapse = ", ")),
                       type = "message", duration = 5)
    })
    
    # One button instead of six. Loading a dataset that has no expression
    # matrix is legitimate -- the tab that needs one is the tab that should
    # refuse -- so the guard the six buttons carried is gone with them.
    observeEvent(input$load_dataset, {
      sel <- input$summary_table_rows_selected
      if (length(sel) != 1) {
        showNotification("Select a dataset row first.", type = "warning")
        return(invisible(NULL))
      }
      d <- summary_data()[sel, "dataset", drop = TRUE]
      if (!is.null(go_to)) go_to(dataset = d)
      showNotification(paste0("Active dataset: ", d), duration = 3)
    })
  })
}