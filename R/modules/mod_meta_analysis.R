metaAnalysisUI <- function(id) {
  ns <- NS(id)
  os_layout(
    left = tagList(
      os_panel(title = "Input Parameters",
        textInput(ns("biomolecule"), "Biomolecule Name:", placeholder = "e.g., LEP"),
        selectInput(ns("condition"), "Select Condition:", choices = NULL),
        radioButtons(ns("stat_test"), "Statistical Test:",
                     choices = list("T-test" = "ttest", "Wilcoxon test" = "wilcoxon"),
                     selected = "ttest"),
        conditionalPanel(condition = "output.show_categorical_groups", ns = ns,
          h4("Group Assignment"),
          selectInput(ns("group1_categories"), "Group 1 (Reference):", choices = NULL, multiple = TRUE),
          selectInput(ns("group2_categories"), "Group 2 (Comparison):", choices = NULL, multiple = TRUE)
        ),
        conditionalPanel(condition = "output.show_numeric_groups", ns = ns,
          h4("Numeric Variable Analysis"),
          selectInput(ns("numeric_split"), "Split method:",
                      choices = list(
                        "Median split (50th percentile)" = "median",
                        "Tertile split (top 33% vs bottom 33%)" = "tertile",
                        "Quartile split (top 25% vs bottom 25%)" = "quartile",
                        "Custom percentile" = "percentile",
                        "Custom threshold" = "threshold"
                      )
          ),
          conditionalPanel(condition = "input.numeric_split == 'percentile'", ns = ns,
            numericInput(ns("custom_percentile"), "Percentile cutoff (%):", value = 20, min = 5, max = 45, step = 5),
            p("Top X% vs Bottom X%", style = "font-size: 12px; color: gray;")
          ),
          conditionalPanel(condition = "input.numeric_split == 'threshold'", ns = ns,
            numericInput(ns("custom_threshold"), "Threshold value:", value = 0),
            radioButtons(ns("threshold_direction"), "Groups:",
                         choices = list(
                           "Below threshold vs Above threshold" = "below_above",
                           "Above threshold vs Below threshold" = "above_below"
                         )
                    )
          )
        ),
        h4("Filter Conditions (Optional)"),
        selectInput(ns("filter_conditions"), "Select filter conditions:", choices = NULL, multiple = TRUE),
        uiOutput(ns("filter_ui")),
        h4("Data Preferences"),
        selectInput(ns("data_preference"), "Data type preference:",
                    choices = list(
                      "TMM" = "TMM", "CPM" = "CPM", "TPM" = "TPM",
                      "FPKM" = "FPKM", "count" = "count", "unknown_unit" = "unknown_unit"
                    ),
                    selected = "TMM"
        ),
        br(),
        actionButton(ns("generate_plot"), "Generate Forest Plot", class = "btn-primary btn-lg")
      )
    ),
    center = tagList(
      os_panel(title = "Forest Plot",
        div(style = "position: relative;",
            plotOutput(ns("forest_plot"), height = "700px"),
            absolutePanel(top = 10, right = 10, draggable = FALSE,
                          downloadButton(ns("dl_forest_png"), label = "PNG"),
                          br(),
                          downloadButton(ns("dl_forest_svg"), label = "SVG"),
                          br(),
                          downloadButton(ns("dl_forest_pdf"), label = "PDF")
            )
        )
      )
    ),
    right = tagList(
      os_panel(title = "Analysis Summary",
        verbatimTextOutput(ns("analysis_summary"))
      ),
      os_panel(title = "Selected Metadata",
        DT::dataTableOutput(ns("metadata_table"))
      )
    )
  )
}

metaAnalysisServer <- function(id, ds, meta, go_to = NULL) {
  moduleServer(id, function(input, output, session) {
    negative_cache <- new.env()
    
    quick_negative_check <- function(data_info, dataset_name) {
      cache_key <- paste0(dataset_name, "_", input$data_preference)
      if (exists(cache_key, envir = negative_cache)) {
        return(get(cache_key, envir = negative_cache))
      }
      
      data_type <- input$data_preference
      if (data_type %in% c("TPM", "FPKM", "CPM")) {
        has_negative <- FALSE
      } else if (data_type %in% c("TMM", "count")) {
        sample_rows <- min(100, nrow(data_info))
        sample_cols <- min(10, ncol(data_info))
        sample_data <- data_info[1:sample_rows, 1:sample_cols, drop = FALSE]
        sample_values <- sample(as.numeric(as.matrix(sample_data)),
                                min(1000, sample_rows * sample_cols))
        has_negative <- any(sample_values < 0, na.rm = TRUE)
      } else {
        total_elements <- nrow(data_info) * ncol(data_info)
        sample_size <- min(2000, total_elements)
        sampled_values <- sample(as.numeric(as.matrix(data_info)), sample_size)
        has_negative <- any(sampled_values < 0, na.rm = TRUE)
      }
      
      assign(cache_key, has_negative, envir = negative_cache)
      return(has_negative)
    }
    
    metadata <- reactive({
      tryCatch({
        df <- meta()
        if (is.null(df)) return(NULL)
        df[df == "NA"] <- NA
        return(df)
      }, error = function(e) {
        showNotification("Error loading metadata file", type = "error")
        return(NULL)
      })
    })
    
    observe({
      if (!is.null(metadata())) {
        exclude_cols <- c("TsengID", "SampleID", "dataset", "Data.type")
        condition_choices <- setdiff(names(metadata()), exclude_cols)
        updateSelectInput(session, "condition", choices = condition_choices)
        updateSelectInput(session, "filter_conditions", choices = condition_choices)
      }
    })
    
    condition_info <- reactive({
      req(input$condition, metadata())
      col_data <- metadata()[[input$condition]]
      col_data_clean <- col_data[!is.na(col_data)]
      
      if (length(col_data_clean) == 0) {
        return(list(type = "empty"))
      }
      
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
    
    output$show_categorical_groups <- reactive({
      if (!is.null(condition_info()) && length(condition_info()) > 0) {
        return(condition_info()$type == "categorical")
      }
      return(FALSE)
    })
    outputOptions(output, "show_categorical_groups", suspendWhenHidden = FALSE)
    
    output$show_numeric_groups <- reactive({
      if (!is.null(condition_info()) && length(condition_info()) > 0) {
        return(condition_info()$type == "numeric")
      }
      return(FALSE)
    })
    outputOptions(output, "show_numeric_groups", suspendWhenHidden = FALSE)
    
    observe({
      if (!is.null(condition_info()) && condition_info()$type == "categorical") {
        updateSelectInput(session, "group1_categories",
                          choices = condition_info()$categories)
        updateSelectInput(session, "group2_categories",
                          choices = condition_info()$categories)
      }
    })
    
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
            return(checkboxGroupInput(session$ns(paste0("filter_", cond)),
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
    
    perform_analysis <- eventReactive(input$generate_plot, {
      req(input$biomolecule, input$condition, metadata())
      
      meta_all <- metadata()
      
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
      
      results <- list()
      for (dataset in datasets) {
        data_info <- tryCatch(load_expression(dataset, input$data_preference), error = function(e) NULL)
        if (is.null(data_info)) next
        
        if (!input$biomolecule %in% rownames(data_info)) next
        
        dataset_group1 <- group1_samples[group1_samples$dataset == dataset, ]
        dataset_group2 <- group2_samples[group2_samples$dataset == dataset, ]
        if (nrow(dataset_group1) == 0 && nrow(dataset_group2) == 0) next
        
        available_samples1 <- intersect(dataset_group1$SampleID, colnames(data_info))
        available_samples2 <- intersect(dataset_group2$SampleID, colnames(data_info))
        if (length(available_samples1) < 2 || length(available_samples2) < 2) next
        
        group1_expr <- as.numeric(data_info[input$biomolecule, available_samples1])
        group2_expr <- as.numeric(data_info[input$biomolecule, available_samples2])
        
        has_negative <- quick_negative_check(data_info, dataset)
        
        if (has_negative) {
          group1_expr_trans <- group1_expr
          group2_expr_trans <- group2_expr
          transform_used <- "none"
        } else {
          group1_expr_trans <- log2(pmax(group1_expr, 0.001))
          group2_expr_trans <- log2(pmax(group2_expr, 0.001))
          transform_used <- "log2(x + 0.001)"
        }
        
        group1_expr_trans <- group1_expr_trans[!is.na(group1_expr_trans)]
        group2_expr_trans <- group2_expr_trans[!is.na(group2_expr_trans)]
        if (length(group1_expr_trans) < 2 || length(group2_expr_trans) < 2) next
        
        test_result <- tryCatch({
          if (input$stat_test == "ttest") {
            t.test(group2_expr_trans, group1_expr_trans)
          } else {
            wilcox.test(group2_expr_trans, group1_expr_trans)
          }
        }, error = function(e) NULL)
        if (is.null(test_result)) next
        pvalue <- test_result$p.value
        
        effect_result <- calculate_cohens_d(group1_expr_trans, group2_expr_trans)
        if (is.na(effect_result$d)) next
        
        results[[dataset]] <- list(
          dataset     = dataset,
          transform   = transform_used,
          effect_size = effect_result$d,
          se          = effect_result$se,
          pvalue      = pvalue,
          n_group1    = length(group1_expr_trans),
          n_group2    = length(group2_expr_trans),
          data_type   = input$data_preference,
          filter_info = filter_info
        )
      }
      
      return(list(results = results,
                  group1_meta = group1_samples,
                  group2_meta = group2_samples,
                  filter_summary = filter_info))
    })
    
    output$forest_plot <- renderPlot({
      analysis_data <- perform_analysis()
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
      
      m <- metagen(
        TE            = yi,
        seTE          = sei,
        studlab       = study,
        data          = forest_data_df,
        sm            = "SMD",
        comb.fixed    = FALSE,
        comb.random   = TRUE,
        method.tau    = "REML",
        method.random.ci = "HK"
      )
      
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
    })
    
    format.pvalue <- function(p) {
      if (is.na(p)) return("NA")
      if (p < 0.001) return(sprintf("%.1e", p))
      else if (p < 0.01) return(sprintf("%.3f", p))
      else return(sprintf("%.2f", p))
    }
    
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
            meta_res <- metagen(
              TE      = estimate,
              seTE    = se,
              studlab = dataset,
              data    = forest_data_for_summary,
              sm      = "SMD",
              common  = FALSE,
              random = TRUE,
              method.random.ci = "HK"
            )
            summary_text <- paste0(summary_text, "\n\n",
                                   "Pooled Effect (Random-Effects Model):\n",
                                   "- Effect Size (Cohen's d) = ", round(meta_res$TE.random, 3), "\n",
                                   "- 95% CI: [", round(meta_res$lower.random, 3), ", ", round(meta_res$upper.random, 3), "]\n",
                                   "- p-value = ", format(meta_res$pval.random, scientific = TRUE, digits = 3), "\n",
                                   "- Heterogeneity (I-squared): ", round(meta_res$I2 * 100, 1), "%")
          }, error = function(e) {})
        }
      }
      
      return(summary_text)
    })
    
    output$dl_forest_png <- downloadHandler(
      filename = function() paste0("forest_plot_", Sys.Date(), ".png"),
      content = function(file) {
        analysis_data <- perform_analysis()
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
  })
}