# The data picks the cutpoint. The Manual cutoffs tab is the other direction:
# there the researcher picks it and the app records the split.
#
# Distilled from Cutoff Finder (Budczies et al., PLoS ONE 2012;7(12):e51862).
# The statistics live in R/cutoff_finder.R and the plots in R/cutoff_plots.R;
# this file is only wiring.

# Below this many usable samples the search is theatre: every candidate split
# is small enough that the interval swallows the estimate. The two Example
# datasets (13 and 6 samples) exist to be refused here.
CF_MIN_N <- 20L

cutoffFinderUI <- function(id) {
  ns <- NS(id)
  os_layout(
    left = tagList(
      os_panel(title = "Variable", collapse = TRUE,
        radioButtons(ns("source"), "Cut on:",
                     choices  = c("Metadata column" = "meta",
                                  "Gene expression" = "gene"),
                     selected = "meta"),
        conditionalPanel("input.source == 'meta'", ns = ns,
          selectInput(ns("meta_col"), "Numeric column:", choices = NULL)
        ),
        conditionalPanel("input.source == 'gene'", ns = ns,
          selectInput(ns("unit"), "Normalization unit:", choices = NULL),
          selectizeInput(ns("gene"), "Gene:", choices = NULL,
                         options = list(server = TRUE, maxOptions = 1000))
        )
      ),
      os_panel(title = "Method", collapse = TRUE,
        radioButtons(ns("method"), "Choose the cutpoint by:",
                     choices = c("Survival (log-rank)"     = "survival",
                                 "Binary outcome"          = "binary",
                                 "Distribution (mixture)"  = "distribution")),
        conditionalPanel("input.method == 'survival'", ns = ns,
          selectInput(ns("time_col"),  "Time column:", choices = NULL),
          selectInput(ns("event_col"), "Event column (1 = event, 0 = censored):",
                      choices = NULL)
        ),
        conditionalPanel("input.method == 'binary'", ns = ns,
          selectInput(ns("outcome_col"), "Outcome column:", choices = NULL),
          selectInput(ns("positive"),    "Which level is the event:", choices = NULL),
          radioButtons(ns("rule"), "Among the candidates, keep:",
                       choices = c("the smallest p"            = "significance",
                                   "the ROC corner, euclidean" = "euclidean",
                                   "the ROC corner, manhattan" = "manhattan"))
        ),
        conditionalPanel("input.method != 'distribution'", ns = ns,
          sliderInput(ns("min_frac"), "Smallest group allowed:",
                      min = 0, max = 0.4, value = 0.1, step = 0.05),
          numericInput(ns("perm"), "Permutations for the corrected p:",
                       value = 200, min = 0, max = 2000, step = 100)
        ),
        actionButton(ns("find"), "Find cutoff", class = "btn-primary")
      )
    ),
    center = tagList(
      os_panel(title = "Cutoff",
        plotOutput(ns("scan_plot"), height = "300px")
      ),
      os_panel(title = "Outcome",
        plotOutput(ns("outcome_plot"), height = "320px")
      )
    ),
    right = tagList(
      os_panel(title = "Result",
        uiOutput(ns("result"))
      ),
      os_panel(title = "Save", collapse = TRUE,
        textInput(ns("col_name"), "New metadata column:", value = ""),
        actionButton(ns("save"), "Save split to metadata",
                     icon = icon("floppy-disk")),
        helpText("Writes \"low\"/\"high\" for the samples of this dataset only.")
      )
    )
  )
}

cutoffFinderServer <- function(id, ds, meta, go_to = NULL) {
  moduleServer(id, function(input, output, session) {

    # --- which columns can play which part -----------------------------------
    # ds_name, not dataset: a parameter called dataset shadows the column and
    # filter(dataset == dataset) quietly keeps every row of every dataset.
    sel_meta <- reactive({
      req(ds())
      md <- meta()
      md[!is.na(md$dataset) & md$dataset == ds(), , drop = FALSE]
    })

    as_num <- function(v) {
      ch <- as.character(v)
      ch[ch == "NA"] <- NA
      ok <- grepl("^-?\\d*\\.?\\d+([eE][-+]?\\d+)?$", ch, perl = TRUE)
      out <- suppressWarnings(as.numeric(ch))
      out[!ok] <- NA_real_
      out
    }

    numeric_cols <- reactive({
      md <- sel_meta()
      keep <- vapply(names(md), function(cn) {
        v <- as_num(md[[cn]])
        length(unique(v[!is.na(v)])) >= 2
      }, logical(1))
      names(md)[keep]
    })

    event_cols <- reactive({
      md <- sel_meta()
      keep <- vapply(names(md), function(cn) {
        v <- unique(as_num(md[[cn]]))
        v <- v[!is.na(v)]
        length(v) == 2 && all(v %in% c(0, 1))
      }, logical(1))
      names(md)[keep]
    })

    binary_cols <- reactive({
      md <- sel_meta()
      keep <- vapply(names(md), function(cn) {
        v <- as.character(md[[cn]])
        v <- unique(v[!is.na(v) & v != "NA"])
        length(v) == 2
      }, logical(1))
      names(md)[keep]
    })

    observeEvent(ds(), {
      nums <- numeric_cols()
      updateSelectInput(session, "meta_col",  choices = nums)
      # grep()[1] on no match is NA, and updateSelectInput(selected = NA)
      # selects nothing visible while reporting a selection.
      guess <- grep("time", nums, ignore.case = TRUE, value = TRUE)
      updateSelectInput(session, "time_col", choices = nums,
                        selected = if (length(guess)) guess[1] else NULL)
      updateSelectInput(session, "event_col", choices = event_cols())
      updateSelectInput(session, "outcome_col", choices = binary_cols())

      units <- list_units(ds())
      updateSelectInput(session, "unit", choices = units)
      if (length(units)) {
        expr <- load_expression(ds(), units[1])
        # server = TRUE: 54,000 symbols shipped to the browser is what makes
        # selectize warn and the tab crawl
        updateSelectizeInput(session, "gene", choices = unique(expr$Symbol),
                             server = TRUE)
      }
    })

    observeEvent(input$unit, {
      req(ds(), input$unit)
      expr <- load_expression(ds(), input$unit)
      updateSelectizeInput(session, "gene", choices = unique(expr$Symbol),
                           server = TRUE)
    })

    observeEvent(input$outcome_col, {
      req(input$outcome_col)
      v <- as.character(sel_meta()[[input$outcome_col]])
      lv <- sort(unique(v[!is.na(v) & v != "NA"]))
      updateSelectInput(session, "positive", choices = lv,
                        selected = if (length(lv)) lv[2] else NULL)
    })

    # --- the variable to cut, aligned with the metadata rows -----------------
    variable <- reactive({
      md <- sel_meta()
      if (identical(input$source, "gene")) {
        req(input$unit, input$gene)
        expr <- load_expression(ds(), input$unit)
        row  <- expr[expr$Symbol == input$gene, , drop = FALSE]
        if (!nrow(row)) return(NULL)
        vals <- as_num(unlist(row[1, -1, drop = TRUE]))
        names(vals) <- names(row)[-1]
        list(name   = paste0(input$gene, ".", input$unit),
             values = unname(vals[match(md$SampleID, names(vals))]))
      } else {
        req(input$meta_col)
        list(name = input$meta_col, values = as_num(md[[input$meta_col]]))
      }
    })

    # --- the search ----------------------------------------------------------
    # eventReactive, so nothing scans while the researcher is still choosing:
    # a permutation run is seconds of CPU and must be asked for.
    found <- eventReactive(input$find, {
      md <- sel_meta()
      v  <- variable()
      if (is.null(v) || all(is.na(v$values))) {
        return(list(error = "That variable has no numeric values in this dataset."))
      }

      if (identical(input$method, "distribution")) {
        keep <- !is.na(v$values)
        if (sum(keep) < CF_MIN_N) return(list(error = cf_too_small(sum(keep))))
        dist <- cf_distribution(v$values[keep])
        if (is.null(dist)) {
          return(list(error = paste("No two-component mixture could be fitted.",
                                    "The distribution may be unimodal.")))
        }
        grp <- cf_split(v$values, dist$cutoff)
        return(list(method = "distribution", var = v, dist = dist,
                    cutoff = dist$cutoff,
                    n_low = sum(grp == "low", na.rm = TRUE),
                    n_high = sum(grp == "high", na.rm = TRUE)))
      }

      if (identical(input$method, "survival")) {
        req(input$time_col, input$event_col)
        time  <- as_num(md[[input$time_col]])
        event <- as_num(md[[input$event_col]])
        keep  <- !is.na(v$values) & !is.na(time) & time > 0 & event %in% c(0, 1)
        if (sum(keep) < CF_MIN_N) return(list(error = cf_too_small(sum(keep))))
        scan <- cf_scan_survival(v$values[keep], time[keep], event[keep],
                                 input$min_frac)
        best <- cf_best(scan, "significance")
        if (is.null(best)) return(list(error = cf_no_candidate(input$min_frac)))
        perm <- cf_perm(v$values[keep],
                        list(time = time[keep], event = event[keep]),
                        "survival", input$min_frac, input$perm)
        list(method = "survival", var = v, scan = scan, best = best, perm = perm,
             cutoff = best$cutoff, n_low = best$n_low, n_high = best$n_high,
             time = time[keep], event = event[keep], x = v$values[keep])
      } else {
        req(input$outcome_col, input$positive)
        y    <- as.character(md[[input$outcome_col]])
        y[y == "NA"] <- NA
        keep <- !is.na(v$values) & !is.na(y)
        if (sum(keep) < CF_MIN_N) return(list(error = cf_too_small(sum(keep))))
        scan <- cf_scan_binary(v$values[keep], y[keep], input$positive,
                               input$min_frac)
        best <- cf_best(scan, input$rule)
        if (is.null(best)) return(list(error = cf_no_candidate(input$min_frac)))
        perm <- cf_perm(v$values[keep], list(y = y[keep]), "binary",
                        input$min_frac, input$perm)
        list(method = "binary", var = v, scan = scan, best = best, perm = perm,
             cutoff = best$cutoff, n_low = best$n_low, n_high = best$n_high,
             auc = cf_auc(scan))
      }
    })

    # A reactive rather than a value computed inside the observer: a test can
    # read this, and updateTextInput() cannot be read back without a browser.
    default_name <- reactive({
      f <- found()
      if (is.null(f$error)) cf_column_name(f) else ""
    })

    observeEvent(found(), {
      nm <- default_name()
      if (nzchar(nm)) updateTextInput(session, "col_name", value = nm)
    })

    # --- plots ---------------------------------------------------------------
    output$scan_plot <- renderPlot({
      f <- found()
      # need(TRUE, NULL) is not a no-op: need() insists on a character message
      # whatever the condition, so the happy path threw before reaching a plot.
      if (!is.null(f$error)) validate(need(FALSE, f$error))
      if (identical(f$method, "distribution")) {
        cf_plot_mixture(f$var$values[!is.na(f$var$values)], f$dist, f$var$name)
      } else {
        cf_plot_scan(f$scan, f$cutoff)
      }
    })

    output$outcome_plot <- renderPlot({
      f <- found()
      if (!is.null(f$error)) validate(need(FALSE, " "))
      if (identical(f$method, "survival")) {
        cf_plot_km(f$x, f$time, f$event, f$cutoff, f$var$name)
      } else if (identical(f$method, "binary")) {
        cf_plot_roc(f$scan, f$cutoff)
      } else {
        validate(need(FALSE, paste("The mixture method uses no outcome.",
                                   "The split is above.")))
      }
    })

    # --- the numbers, with the warning the paper asks for -------------------
    output$result <- renderUI({
      f <- found()
      if (!is.null(f$error)) return(div(class = "text-warning", f$error))

      rows <- list(cf_row("Variable", f$var$name),
                   cf_row("Cutoff", format(round(f$cutoff, 3))),
                   cf_row("Samples", paste0("low ", f$n_low, " / high ", f$n_high)))

      if (!identical(f$method, "distribution")) {
        eff <- attr(f$scan, "effect_name")
        rows <- c(rows, list(
          cf_row(paste0(eff, " (95% CI)"),
                 sprintf("%.2f (%.2f - %.2f)", f$best$effect, f$best$lower,
                         f$best$upper)),
          cf_row("p at the best cutoff", format.pval(f$best$pvalue, digits = 3)),
          cf_row("p, permutation-corrected",
                 if (is.na(f$perm$p)) "not run"
                 else sprintf("%.3f  (%d permutations)", f$perm$p, f$perm$B))
        ))
        if (identical(f$method, "binary")) {
          rows <- c(rows, list(
            cf_row("Sensitivity / specificity",
                   sprintf("%.2f / %.2f", f$best$sensitivity, f$best$specificity)),
            cf_row("AUC", sprintf("%.3f", f$auc))))
        }
      }

      tagList(
        tags$table(class = "table table-condensed", tags$tbody(rows)),
        if (!identical(f$method, "distribution"))
          tags$p(class = "text-muted", style = "font-size: 12px;",
            tags$strong("Read the corrected p, not the other one. "),
            "The cutoff was chosen by trying every split and keeping the best, ",
            "so the p beside it is optimistic by construction. The corrected p ",
            "asks how often chance alone does as well when the outcome is ",
            "shuffled. Either way, a cutoff found in one cohort should be ",
            "validated in another.")
      )
    })

    # --- save ----------------------------------------------------------------
    observeEvent(input$save, {
      f <- found()
      if (!is.null(f$error)) {
        showNotification("Nothing to save: no cutoff was found.", type = "error")
        return()
      }
      col <- trimws(input$col_name)
      if (!nchar(col)) {
        showNotification("Give the new column a name.", type = "error")
        return()
      }
      before <- meta()
      idx <- which(!is.na(before$dataset) & before$dataset == ds())
      grp <- cf_split(variable()$values, f$cutoff)

      after <- before
      if (!col %in% names(after)) after[[col]] <- "NA"
      after[[col]] <- as.character(after[[col]])
      vals <- as.character(grp)
      vals[is.na(vals)] <- "NA"
      after[[col]][idx] <- vals

      save_metadata(before, after)
      showNotification(paste0("Saved ", col, " for ", length(idx), " samples."),
                       type = "message", duration = 5)
    })
  })
}

# --- small shared pieces, outside the server ---------------------------------

cf_row <- function(label, value) {
  htmltools::tags$tr(htmltools::tags$td(htmltools::tags$strong(label)),
                     htmltools::tags$td(value))
}

cf_too_small <- function(n) {
  paste0("Only ", n, " samples have both the variable and the outcome. ",
         "This tab needs at least ", CF_MIN_N, ": below that every candidate ",
         "split is too small for the estimate to mean anything.")
}

cf_no_candidate <- function(min_frac) {
  paste0("No cutpoint leaves at least ", round(min_frac * 100),
         "% of the samples on each side. Lower the smallest-group slider, or ",
         "accept that this variable does not split this dataset.")
}

# The name the split gets in the metadata, in the shape the Manual cutoffs tab
# already writes: variable, then the cutoff with the dot replaced.
cf_column_name <- function(f) {
  paste0(gsub("[^A-Za-z0-9._]", "_", f$var$name), ".",
         gsub("\\.", "_", format(round(f$cutoff, 2), nsmall = 2)))
}

# Permutations are the slow part, so they report progress and can be skipped.
cf_perm <- function(x, outcome, kind, min_frac, B) {
  B <- suppressWarnings(as.integer(B))
  if (is.na(B) || B < 1) return(list(p = NA_real_, B = 0L))
  withProgress(message = "Permuting the outcome", value = 0.5, {
    cf_permutation_p(x, outcome, kind, min_frac, B)
  })
}
