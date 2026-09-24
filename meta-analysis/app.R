# Paths come from config/config.txt -- see R/config.R
source("../R/config.R")
Sys.setenv(R_LIBS = os_path("lib"))
Sys.setenv(R_LIBS_USER = os_path("lib"))
.libPaths(os_path("lib"))
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(readxl)
library(meta)
library(grid)
library(cowplot)
library(gridExtra)

ui <- dashboardPage(
  dashboardHeader(title = "Omic Data Forest Plot Generator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Forest Plot Analysis", tabName = "analysis", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "analysis",
              
              # First row: inputs on left, forest plot + downloads on right
              fluidRow(
                box(
                  title = "Input Parameters", status = "primary", solidHeader = TRUE, width = 4,
                  
                  textInput("biomolecule", "Biomolecule Name:", placeholder = "e.g., LEP"),
                  
                  selectInput("condition", "Select Condition:", choices = NULL),
                  
                  radioButtons("stat_test", "Statistical Test:",
                               choices = list("T-test" = "ttest", "Wilcoxon test" = "wilcoxon"),
                               selected = "ttest"),
                  
                  conditionalPanel(
                    condition = "output.show_categorical_groups",
                    h4("Group Assignment"),
                    selectInput("group1_categories", "Group 1 (Reference):", choices = NULL, multiple = TRUE),
                    selectInput("group2_categories", "Group 2 (Comparison):", choices = NULL, multiple = TRUE)
                  ),
                  
                  conditionalPanel(
                    condition = "output.show_numeric_groups",
                    h4("Numeric Variable Analysis"),
                    selectInput("numeric_split", "Split method:",
                                choices = list(
                                  "Median split (50th percentile)" = "median",
                                  "Tertile split (top 33% vs bottom 33%)" = "tertile",
                                  "Quartile split (top 25% vs bottom 25%)" = "quartile",
                                  "Custom percentile" = "percentile",
                                  "Custom threshold" = "threshold"
                                )
                    ),
                    conditionalPanel(
                      condition = "input.numeric_split == 'percentile'",
                      numericInput("custom_percentile", "Percentile cutoff (%):", value = 20, min = 5, max = 45, step = 5),
                      p("Top X% vs Bottom X%", style = "font-size: 12px; color: gray;")
                    ),
                    conditionalPanel(
                      condition = "input.numeric_split == 'threshold'",
                      numericInput("custom_threshold", "Threshold value:", value = 0),
                      radioButtons("threshold_direction", "Groups:",
                                   choices = list(
                                     "Below threshold vs Above threshold" = "below_above",
                                     "Above threshold vs Below threshold" = "above_below"
                                   )
                      )
                    )
                  ),
                  
                  h4("Filter Conditions (Optional)"),
                  selectInput("filter_conditions", "Select filter conditions:", choices = NULL, multiple = TRUE),
                  uiOutput("filter_ui"),
                  
                  h4("Data Preferences"),
                  selectInput("data_preference", "Data type preference:",
                              choices = list(
                                "TMM" = "TMM", "CPM" = "CPM", "TPM" = "TPM",
                                "FPKM" = "FPKM", "count" = "count", "unknown_unit" = "unknown_unit"
                              ),
                              selected = "TMM"
                  ),
                  
                  br(),
                  actionButton("generate_plot", "Generate Forest Plot", class = "btn-primary btn-lg")
                ),
                
                box(
                  title = "Forest Plot", status = "primary", solidHeader = TRUE, width = 8,
                  div(style = "position: relative;",
                      plotOutput("forest_plot", height = "700px"),
                      absolutePanel(top = 10, right = 10, draggable = FALSE,
                                    downloadButton("dl_forest_png", label = "PNG"),
                                    br(),
                                    downloadButton("dl_forest_svg", label = "SVG"),
                                    br(),
                                    downloadButton("dl_forest_pdf", label = "PDF")
                      )
                  )
                )
              ),  # end fluidRow
              
              # Second row: Analysis Summary
              fluidRow(
                box(
                  title = "Analysis Summary", status = "primary", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("analysis_summary")
                )
              ),
              
              # Third row: Selected Metadata
              fluidRow(
                box(
                  title = "Selected Metadata", status = "primary", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("metadata_table")
                )
              )
              
      )  # end tabItem
    )    # end tabItems
  )      # end dashboardBody
)        # end dashboardPage

# Define server logic
server <- function(input, output, session) {
  
  # ═══════════════════════════════════════════════════════════════════════════════
  # ADD THIS: Cache for negative detection results (PERFORMANCE OPTIMIZATION)
  # ═══════════════════════════════════════════════════════════════════════════════
  negative_cache <- new.env()
  
  # ═══════════════════════════════════════════════════════════════════════════════
  # ADD THIS: Ultra-fast negative detection function (OPTION 3)
  # ═══════════════════════════════════════════════════════════════════════════════
  quick_negative_check <- function(data_info, dataset_name) {
    # Check cache first
    cache_key <- paste0(dataset_name, "_", data_info$type)
    if (exists(cache_key, envir = negative_cache)) {
      return(get(cache_key, envir = negative_cache))
    }
    
    # Quick heuristic based on data type
    data_type <- data_info$type
    if (data_type %in% c("TPM", "FPKM", "CPM")) {
      # These are typically positive-only metrics
      has_negative <- FALSE
    } else if (data_type %in% c("TMM", "count")) {
      # These could have negatives, do a quick sample check
      sample_rows <- min(100, nrow(data_info$data))
      sample_cols <- min(10, ncol(data_info$data))
      sample_data <- data_info$data[1:sample_rows, 1:sample_cols, drop = FALSE]
      sample_values <- sample(as.numeric(as.matrix(sample_data)),
                              min(1000, sample_rows * sample_cols))
      has_negative <- any(sample_values < 0, na.rm = TRUE)
    } else {
      # Unknown type, do a moderate sample check
      total_elements <- nrow(data_info$data) * ncol(data_info$data)
      sample_size <- min(2000, total_elements)
      sampled_values <- sample(as.numeric(as.matrix(data_info$data)), sample_size)
      has_negative <- any(sampled_values < 0, na.rm = TRUE)
    }
    
    # Cache result
    assign(cache_key, has_negative, envir = negative_cache)
    return(has_negative)
  }
  
  # Load metadata with error handling
  metadata <- reactive({
    tryCatch({
      cat("Loading metadata...\n")
      df <- read_excel(os_path("hubdata", "Metadata.xlsx"))
      # Convert "NA" strings to actual NA values
      df[df == "NA"] <- NA
      cat("Metadata loaded successfully. Rows:", nrow(df), "Cols:", ncol(df), "\n")
      return(df)
    }, error = function(e) {
      cat("Error loading metadata:", e$message, "\n")
      showNotification("Error loading metadata file", type = "error")
      return(NULL)
    })
  })
  
  # Update condition choices based on metadata
  observe({
    if (!is.null(metadata())) {
      # Get all potential condition columns (excluding ID columns)
      exclude_cols <- c("TsengID", "SampleID", "dataset", "Data.type")
      condition_choices <- setdiff(names(metadata()), exclude_cols)
      
      updateSelectInput(session, "condition", choices = condition_choices)
      updateSelectInput(session, "filter_conditions", choices = condition_choices)
    }
  })
  
  # Reactive values for condition analysis
  condition_info <- reactive({
    req(input$condition, metadata())
    
    col_data <- metadata()[[input$condition]]
    col_data_clean <- col_data[!is.na(col_data)]
    
    if (length(col_data_clean) == 0) {
      return(list(type = "empty"))
    }
    
    # Check if numeric
    if (is.numeric(col_data_clean)) {
      return(list(type = "numeric", data = col_data_clean))
    } else if (is.character(col_data_clean) && all(grepl("^-?\\d*\\.?\\d+$", col_data_clean))) {
      numeric_data <- as.numeric(col_data_clean)
      return(list(type = "numeric", data = numeric_data))
    } else {
      unique_vals <- unique(col_data_clean)
      return(list(type = "categorical", categories = unique_vals))
    }
  })
  
  # Show/hide categorical group selection
  output$show_categorical_groups <- reactive({
    if (!is.null(condition_info()) && length(condition_info()) > 0) {
      return(condition_info()$type == "categorical")
    }
    return(FALSE)
  })
  outputOptions(output, "show_categorical_groups", suspendWhenHidden = FALSE)
  
  # Show/hide numeric group selection
  output$show_numeric_groups <- reactive({
    if (!is.null(condition_info()) && length(condition_info()) > 0) {
      return(condition_info()$type == "numeric")
    }
    return(FALSE)
  })
  outputOptions(output, "show_numeric_groups", suspendWhenHidden = FALSE)
  
  # Update categorical group choices
  observe({
    if (!is.null(condition_info()) && condition_info()$type == "categorical") {
      updateSelectInput(session, "group1_categories",
                        choices = condition_info()$categories)
      updateSelectInput(session, "group2_categories",
                        choices = condition_info()$categories)
    }
  })
  
  # Dynamic filter UI
  output$filter_ui <- renderUI({
    if (is.null(input$filter_conditions) || length(input$filter_conditions) == 0) {
      return(NULL)
    }
    
    filter_elements <- lapply(input$filter_conditions, function(cond) {
      if (is.null(metadata()) || !cond %in% names(metadata())) {
        return(NULL)
      }
      
      col_data <- metadata()[[cond]]
      col_data_clean <- col_data[!is.na(col_data)]
      
      if (is.numeric(col_data_clean)) {
        return(NULL)
      } else if (is.character(col_data_clean) && all(grepl("^-?\\d*\\.?\\d+$", col_data_clean))) {
        return(NULL)
      } else {
        unique_vals <- unique(col_data_clean)
        if (length(unique_vals) > 0) {
          return(checkboxGroupInput(paste0("filter_", cond),
                                    paste("Include", cond, ":"),
                                    choices = unique_vals,
                                    selected = unique_vals))
        }
      }
      return(NULL)
    })
    
    filter_elements <- filter_elements[!sapply(filter_elements, is.null)]
    if (length(filter_elements) > 0) {
      return(do.call(tagList, filter_elements))
    } else {
      return(NULL)
    }
  })
  
  # Function to find and load data
  load_data <- function(dataset, data_preference) {
    base_path <- os_path("hubdata")
    dataset_path <- file.path(base_path, dataset)
    
    if (!dir.exists(dataset_path)) {
      return(NULL)
    }
    
    file_patterns <- c("_TMM.txt", "_CPM.txt", "_TPM.txt", "_FPKM.txt", "_count.txt", "_unknown_unit.txt")
    for (pattern in file_patterns) {
      file_path <- file.path(dataset_path, paste0(dataset, pattern))
      if (file.exists(file_path)) {
        tryCatch({
          data <- read.table(file_path, header = TRUE, sep = "\t", row.names = 1, check.names = FALSE)
          return(list(data = data, type = gsub("_|\\.txt", "", pattern)))
        }, error = function(e) {
          cat("Error loading file", file_path, ":", e$message, "\n")
          next
        })
      }
    }
    
    return(NULL)
  }
  
  # Function to create groups from numeric data
  create_numeric_groups <- function(meta_data, condition_col, split_method, custom_percentile = NULL, custom_threshold = NULL, threshold_direction = NULL) {
    numeric_values <- as.numeric(meta_data[[condition_col]])
    valid_indices <- !is.na(numeric_values)
    meta_clean <- meta_data[valid_indices, ]
    numeric_clean <- numeric_values[valid_indices]
    
    if (length(numeric_clean) < 4) {
      return(list(group1 = data.frame(), group2 = data.frame()))
    }
    
    if (split_method == "median") {
      median_val <- median(numeric_clean)
      group1_indices <- numeric_clean <= median_val
      group2_indices <- numeric_clean > median_val
      
    } else if (split_method == "tertile") {
      tertiles <- quantile(numeric_clean, c(1/3, 2/3))
      group1_indices <- numeric_clean <= tertiles[1]
      group2_indices <- numeric_clean >= tertiles[2]
      
    } else if (split_method == "quartile") {
      quartiles <- quantile(numeric_clean, c(0.25, 0.75))
      group1_indices <- numeric_clean <= quartiles[1]
      group2_indices <- numeric_clean >= quartiles[2]
      
    } else if (split_method == "percentile") {
      if (is.null(custom_percentile)) custom_percentile <- 20
      percentiles <- quantile(numeric_clean, c(custom_percentile/100, 1 - custom_percentile/100))
      group1_indices <- numeric_clean <= percentiles[1]
      group2_indices <- numeric_clean >= percentiles[2]
      
    } else if (split_method == "threshold") {
      if (is.null(custom_threshold)) custom_threshold <- median(numeric_clean)
      if (is.null(threshold_direction)) threshold_direction <- "below_above"
      
      if (threshold_direction == "below_above") {
        group1_indices <- numeric_clean <= custom_threshold
        group2_indices <- numeric_clean > custom_threshold
      } else {
        group1_indices <- numeric_clean > custom_threshold
        group2_indices <- numeric_clean <= custom_threshold
      }
    }
    
    return(list(
      group1 = meta_clean[group1_indices, ],
      group2 = meta_clean[group2_indices, ]
    ))
  }
  
  # Function to calculate Cohen's d and its standard error
  calculate_cohens_d <- function(group1_values, group2_values) {
    n1 <- length(group1_values)
    n2 <- length(group2_values)
    
    if (n1 < 2 || n2 < 2) {
      return(list(d = NA, se = NA))
    }
    
    mean1 <- mean(group1_values, na.rm = TRUE)
    mean2 <- mean(group2_values, na.rm = TRUE)
    sd1 <- sd(group1_values, na.rm = TRUE)
    sd2 <- sd(group2_values, na.rm = TRUE)
    
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    if (pooled_sd == 0) {
      return(list(d = 0, se = 0))
    }
    
    cohens_d <- (mean2 - mean1) / pooled_sd
    se_d <- sqrt((n1 + n2) / (n1 * n2) + cohens_d^2 / (2 * (n1 + n2 - 2)))
    
    return(list(d = cohens_d, se = se_d))
  }
  
  # Main analysis function
  perform_analysis <- eventReactive(input$generate_plot, {
    req(input$biomolecule, input$condition, metadata())
    
    cat("Starting analysis for biomolecule:", input$biomolecule, "\n")
    meta_all <- metadata()
    
    # Create groups based on condition
    if (condition_info()$type == "categorical") {
      req(input$group1_categories, input$group2_categories)
      group1_samples <- meta_all[meta_all[[input$condition]] %in% input$group1_categories & !is.na(meta_all[[input$condition]]), ]
      group2_samples <- meta_all[meta_all[[input$condition]] %in% input$group2_categories & !is.na(meta_all[[input$condition]]), ]
    } else if (condition_info()$type == "numeric") {
      groups <- create_numeric_groups(meta_all, input$condition, input$numeric_split,
                                      input$custom_percentile, input$custom_threshold,
                                      input$threshold_direction)
      group1_samples <- groups$group1
      group2_samples <- groups$group2
    } else {
      return(list(results = list(), group1_meta = data.frame(), group2_meta = data.frame(), filter_summary = list()))
    }
    
    cat("Group 1 samples:", nrow(group1_samples), "Group 2 samples:", nrow(group2_samples), "\n")
    
    # Apply filters if selected
    filter_info <- list()
    if (!is.null(input$filter_conditions) && length(input$filter_conditions) > 0) {
      for (cond in input$filter_conditions) {
        filter_input_id <- paste0("filter_", cond)
        if (!is.null(input[[filter_input_id]]) && length(input[[filter_input_id]]) > 0) {
          filter_info[[cond]] <- list(
            selected = input[[filter_input_id]],
            na_count_group1 = sum(is.na(group1_samples[[cond]])),
            na_count_group2 = sum(is.na(group2_samples[[cond]]))
          )
          group1_samples <- group1_samples[group1_samples[[cond]] %in% input[[filter_input_id]] | is.na(group1_samples[[cond]]), ]
          group2_samples <- group2_samples[group2_samples[[cond]] %in% input[[filter_input_id]] | is.na(group2_samples[[cond]]), ]
        }
      }
    }
    
    datasets <- unique(c(group1_samples$dataset, group2_samples$dataset))
    cat("Datasets to analyze:", paste(datasets, collapse = ", "), "\n")
    
    results <- list()
    for (dataset in datasets) {
      cat("Processing dataset:", dataset, "\n")
      
      data_info <- load_data(dataset, input$data_preference)
      if (is.null(data_info)) {
        cat("Could not load data for dataset:", dataset, "\n")
        next
      }
      
      if (!input$biomolecule %in% rownames(data_info$data)) {
        cat("Biomolecule", input$biomolecule, "not found in dataset:", dataset, "\n")
        next
      }
      
      dataset_group1 <- group1_samples[group1_samples$dataset == dataset, ]
      dataset_group2 <- group2_samples[group2_samples$dataset == dataset, ]
      if (nrow(dataset_group1) == 0 && nrow(dataset_group2) == 0) {
        cat("No samples found for dataset:", dataset, "\n")
        next
      }
      
      available_samples1 <- intersect(dataset_group1$SampleID, colnames(data_info$data))
      available_samples2 <- intersect(dataset_group2$SampleID, colnames(data_info$data))
      if (length(available_samples1) < 2 || length(available_samples2) < 2) {
        cat("Insufficient samples for dataset:", dataset, "G1:", length(available_samples1), "G2:", length(available_samples2), "\n")
        next
      }
      
      group1_expr <- as.numeric(data_info$data[input$biomolecule, available_samples1])
      group2_expr <- as.numeric(data_info$data[input$biomolecule, available_samples2])
      
      # ═══════════════════════════════════════════════════════════════════════════════
      # REPLACE THIS SECTION: Fast negative detection using Option 3
      # ═══════════════════════════════════════════════════════════════════════════════
      # OLD SLOW CODE (REMOVE):
      # all_dataset_vals <- as.numeric(unlist(data_info$data))
      # has_negative    <- any(all_dataset_vals < 0, na.rm = TRUE)
      
      # NEW FAST CODE (USE THIS):
      has_negative <- quick_negative_check(data_info, dataset)
      
      if (has_negative) {
        group1_expr_trans <- group1_expr
        group2_expr_trans <- group2_expr
        transform_used    <- "none"
        cat("  Dataset", dataset, ": negative values detected ⇒ using raw data (no log2)\n")
      } else {
        group1_expr_trans <- log2(pmax(group1_expr, 0.001))
        group2_expr_trans <- log2(pmax(group2_expr, 0.001))
        transform_used    <- "log2(x + 0.001)"
        cat("  Dataset", dataset, ": no negatives detected ⇒ applying log2(x + 0.001)\n")
      }
      
      group1_expr_trans <- group1_expr_trans[!is.na(group1_expr_trans)]
      group2_expr_trans <- group2_expr_trans[!is.na(group2_expr_trans)]
      if (length(group1_expr_trans) < 2 || length(group2_expr_trans) < 2) {
        cat("Insufficient valid expression values for dataset after transform:", dataset, "\n")
        next
      }
      
      # Perform statistical test
      test_result <- tryCatch({
        if (input$stat_test == "ttest") {
          t.test(group2_expr_trans, group1_expr_trans)
        } else {
          wilcox.test(group2_expr_trans, group1_expr_trans)
        }
      }, error = function(e) {
        cat("Error in statistical test for dataset:", dataset, ":", e$message, "\n")
        return(NULL)
      })
      if (is.null(test_result)) next
      pvalue <- test_result$p.value
      
      effect_result <- calculate_cohens_d(group1_expr_trans, group2_expr_trans)
      if (is.na(effect_result$d)) {
        cat("Could not calculate effect size for dataset:", dataset, "\n")
        next
      }
      
      results[[dataset]] <- list(
        dataset     = dataset,
        transform   = transform_used,
        effect_size = effect_result$d,
        se          = effect_result$se,
        pvalue      = pvalue,
        n_group1    = length(group1_expr_trans),
        n_group2    = length(group2_expr_trans),
        data_type   = data_info$type,
        filter_info = filter_info
      )
      cat("  ✅ Processed:", dataset,
          "| transform:", transform_used,
          "| effect size:", round(effect_result$d, 3), "\n")
    }
    
    cat("Analysis completed. Results for", length(results), "datasets\n")
    return(list(results = results,
                group1_meta = group1_samples,
                group2_meta = group2_samples,
                filter_summary = filter_info))
  })
  
  # --- Start of the corrected plotting output block ---
  output$forest_plot <- renderPlot({
    library(meta)
    library(grid)          # para a unit()
    
    # 1) montar o data.frame de resultados
    analysis_data    <- perform_analysis()
    analysis_results <- analysis_data$results
    if (length(analysis_results) < 1) return(NULL)
    
    forest_data_df <- do.call(rbind, lapply(analysis_results, function(x) {
      data.frame(
        study = x$dataset,
        yi    = x$effect_size,
        sei   = x$se,
        n1    = x$n_group1,
        n2    = x$n_group2,
        stringsAsFactors = FALSE
      )
    }))
    
    # 2) cria o objeto meta‐analysis (REML + HK CI)
    m <- metagen(
      TE            = yi,
      seTE          = sei,
      studlab       = study,
      data          = forest_data_df,
      sm            = "SMD",
      comb.fixed    = FALSE,
      comb.random   = TRUE,
      method.tau    = "REML",
      method.random.ci = "HK"     # Hartung–Knapp CIs
    )
    
    # 3) plota com layout “textbook”
    forest(
      m,
      leftcols           = c("studlab", "n1", "n2"),
      leftlabs           = c("Author",  "n1", "n2"),
      rightcols          = c("TE", "lower", "upper", "w.random"),
      rightlabs          = c("SMD", "95%-CI", "Weight"),
      colgap.forest.left  = unit(2, "cm"),
      colgap.forest.right = unit(1, "cm"),
      digits             = 2,      # casas decimais para SMD & CI
      print.I2           = TRUE,
      print.pval.Q       = TRUE,
      smlab              = "Effect Size (Cohen's d)"
    )
  })
  
  
  
  
  # Helper function for p-value formatting (keep this as it's useful)
  format.pvalue <- function(p) {
    if (is.na(p)) return("NA")
    if (p < 0.001) {
      return(sprintf("%.1e", p))
    } else if (p < 0.01) {
      return(sprintf("%.3f", p))
    } else {
      return(sprintf("%.2f", p))
    }
  }
  
  # Analysis summary
  output$analysis_summary <- renderText({
    analysis_data <- perform_analysis()
    analysis_results <- analysis_data$results
    group1_meta <- analysis_data$group1_meta
    group2_meta <- analysis_data$group2_meta
    filter_summary <- analysis_data$filter_summary
    
    if (length(analysis_results) == 0) {
      return("No data available for the specified biomolecule and conditions.\nPlease check:\n- Biomolecule name spelling\n- Group assignments\n- Data file availability")
    }
    
    filter_text <- ""
    if (length(filter_summary) > 0) {
      filter_text <- paste0("\nApplied Filters:\n",
                            paste(sapply(names(filter_summary), function(f) {
                              paste0("- ", f, ": ", paste(filter_summary[[f]]$selected, collapse = ", "))
                            }), collapse = "\n"))
    }
    
    summary_text <- paste(
      "Analysis Summary:",
      paste("- Biomolecule:", input$biomolecule),
      paste("- Condition:", input$condition),
      paste("- Statistical Test:", ifelse(input$stat_test == "ttest", "T-test", "Wilcoxon test")),
      paste("- Effect Size Measure: Cohen's d"),
      paste("- Data Transformation: log2(x + 0.001) unless negatives detected"),
      paste("- Number of datasets analyzed:", length(analysis_results)),
      paste("- Total samples in Group 1:", nrow(group1_meta)),
      paste("- Total samples in Group 2:", nrow(group2_meta)),
      filter_text,
      "",
      "Dataset Results:",
      paste(sapply(names(analysis_results), function(dataset) {
        res <- analysis_results[[dataset]]
        paste0(
          "- ", dataset, ": transform = ", res$transform,
          ", Effect Size = ", round(res$effect_size, 3),
          ", p-value = ", format(res$pvalue, scientific = TRUE, digits = 3),
          ", n1 = ", res$n_group1,
          ", n2 = ", res$n_group2,
          ", Unit type: ", res$data_type
        )
      }), collapse = "\n"),
      sep = "\n"
    )
    
    if (length(analysis_results) > 1) {
      forest_data_list <- lapply(analysis_results, function(x) {
        data.frame(dataset = x$dataset, estimate = x$effect_size, se = x$se, stringsAsFactors = FALSE)
      })
      forest_data_for_summary <- do.call(rbind, forest_data_list)
      if (nrow(forest_data_for_summary) > 1) {
        tryCatch({
          # Ensure `meta` package is loaded for metagen function
          library(meta)
          meta_res <- metagen(
            TE      = estimate,
            seTE    = se,
            studlab = dataset,
            data    = forest_data_for_summary,
            sm      = "SMD",
            common  = FALSE,
            random = TRUE,
            method.random.ci        = "HK"
          )
          summary_text <- paste0(summary_text, "\n\n",
                                 "Pooled Effect (Random-Effects Model):\n",
                                 "- Effect Size (Cohen's d) = ", round(meta_res$TE.random, 3), "\n",
                                 "- 95% CI: [", round(meta_res$lower.random, 3), ", ", round(meta_res$upper.random, 3), "]\n",
                                 "- p-value = ", format(meta_res$pval.random, scientific = TRUE, digits = 3), "\n",
                                 "- Heterogeneity (I-squared): ", round(meta_res$I2 * 100, 1), "%"
          )
        }, error = function(e) {
          cat("Error calculating pooled effect size for summary:", e$message, "\n")
        })
      }
    }
    
    return(summary_text)
  })
  
  # Helper function for p-value formatting
  format.pvalue <- function(p) {
    if (is.na(p)) return("NA")
    if (p < 0.001) {
      return(sprintf("%.1e", p))
    } else if (p < 0.01) {
      return(sprintf("%.3f", p))
    } else {
      return(sprintf("%.2f", p))
    }
  }
  
  #download plot
  
  # --------------------
  # Download PNG
  # --------------------
  output$dl_forest_png <- downloadHandler(
    filename = function() paste0("forest_plot_", Sys.Date(), ".png"),
    content = function(file) {
      # re‑run the exact same analysis & forest code, but inside a PNG device
      analysis_data    <- perform_analysis()
      forest_df <- do.call(rbind, lapply(analysis_data$results, function(x) {
        data.frame(study = x$dataset, yi = x$effect_size, sei = x$se,
                   n1 = x$n_group1, n2 = x$n_group2, stringsAsFactors = FALSE)
      }))
      m <- metagen(
        TE            = yi,
        seTE          = sei,
        studlab       = study,
        data          = forest_df,
        sm            = "SMD",
        comb.fixed    = FALSE,
        comb.random   = TRUE,
        method.tau    = "REML",
        method.random.ci = "HK"
      )
      
      png(filename = file, width = 1600, height = 1000, res = 150)
      forest(
        m,
        leftcols           = c("studlab", "n1", "n2"),
        leftlabs           = c("Author",  "n1", "n2"),
        rightcols          = c("TE", "lower", "upper", "w.random"),
        rightlabs          = c("SMD", "95%-CI", "Weight"),
        colgap.forest.left  = unit(2, "cm"),
        colgap.forest.right = unit(1, "cm"),
        digits             = 2,
        print.I2           = TRUE,
        print.pval.Q       = TRUE,
        smlab              = "Effect Size (Cohen's d)"
      )
      dev.off()
    }
  )
  
  # --------------------
  # Download SVG
  # --------------------
  output$dl_forest_svg <- downloadHandler(
    filename = function() paste0("forest_plot_", Sys.Date(), ".svg"),
    content = function(file) {
      analysis_data <- perform_analysis()
      forest_df <- do.call(rbind, lapply(analysis_data$results, function(x) {
        data.frame(study = x$dataset, yi = x$effect_size, sei = x$se,
                   n1 = x$n_group1, n2 = x$n_group2, stringsAsFactors = FALSE)
      }))
      m <- metagen(TE = yi, seTE = sei, studlab = study, data = forest_df,
                   sm = "SMD", comb.fixed = FALSE, comb.random = TRUE,
                   method.tau = "REML", method.random.ci = "HK")
      
      svg(filename = file, width = 14, height = 7)
      forest(m,
             leftcols           = c("studlab", "n1", "n2"),
             leftlabs           = c("Author",  "n1", "n2"),
             rightcols          = c("TE", "lower", "upper", "w.random"),
             rightlabs          = c("SMD", "95%-CI", "Weight"),
             colgap.forest.left  = unit(2, "cm"),
             colgap.forest.right = unit(1, "cm"),
             digits             = 2,
             print.I2           = TRUE,
             print.pval.Q       = TRUE,
             smlab              = "Effect Size (Cohen's d)")
      dev.off()
    }
  )
  
  # --------------------
  # Download PDF
  # --------------------
  output$dl_forest_pdf <- downloadHandler(
    filename = function() paste0("forest_plot_", Sys.Date(), ".pdf"),
    content = function(file) {
      analysis_data <- perform_analysis()
      forest_df <- do.call(rbind, lapply(analysis_data$results, function(x) {
        data.frame(study = x$dataset, yi = x$effect_size, sei = x$se,
                   n1 = x$n_group1, n2 = x$n_group2, stringsAsFactors = FALSE)
      }))
      m <- metagen(TE = yi, seTE = sei, studlab = study, data = forest_df,
                   sm = "SMD", comb.fixed = FALSE, comb.random = TRUE,
                   method.tau = "REML", method.random.ci = "HK")
      
      pdf(file = file, width = 14, height = 7)
      forest(m,
             leftcols           = c("studlab", "n1", "n2"),
             leftlabs           = c("Author",  "n1", "n2"),
             rightcols          = c("TE", "lower", "upper", "w.random"),
             rightlabs          = c("SMD", "95%-CI", "Weight"),
             colgap.forest.left  = unit(2, "cm"),
             colgap.forest.right = unit(1, "cm"),
             digits             = 2,
             print.I2           = TRUE,
             print.pval.Q       = TRUE,
             smlab              = "Effect Size (Cohen's d)")
      dev.off()
    }
  )
  
  
  # Metadata table
  output$metadata_table <- DT::renderDataTable({
    analysis_data <- perform_analysis()
    group1_meta <- analysis_data$group1_meta
    group2_meta <- analysis_data$group2_meta
    
    if (nrow(group1_meta) == 0 && nrow(group2_meta) == 0) {
      return(DT::datatable(data.frame(Message = "No data available"), options = list(dom = 't')))
    }
    
    combined_meta <- data.frame()
    if (nrow(group1_meta) > 0) {
      combined_meta <- rbind(combined_meta, data.frame(Group = "Group 1 (Reference)", group1_meta, stringsAsFactors = FALSE))
    }
    if (nrow(group2_meta) > 0) {
      combined_meta <- rbind(combined_meta, data.frame(Group = "Group 2 (Comparison)", group2_meta, stringsAsFactors = FALSE))
    }
    
    DT::datatable(combined_meta,
                  options = list(scrollX = TRUE, pageLength = 15,
                                 columnDefs = list(list(className = 'dt-center', targets = 0))),
                  rownames = FALSE) %>%
      DT::formatStyle('Group',
                      backgroundColor = DT::styleEqual(c("Group 1 (Reference)", "Group 2 (Comparison)"),
                                                       c("#e6f3ff", "#fff2e6")))
  })
} 

# Run the application
shinyApp(ui = ui, server = server)
