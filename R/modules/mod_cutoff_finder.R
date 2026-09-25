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
        os_field(
          radioButtons(ns("source"), "Cut on:",
                       choices  = c("Metadata column" = "meta",
                                    "Gene expression" = "gene"),
                       selected = "meta"),
          "Where the numbers to cut come from. A metadata column is taken as it
           stands; a gene gives one value per sample, read from the expression
           matrix of the unit you choose."),
        conditionalPanel("input.source == 'meta'", ns = ns,
          os_field(
            selectInput(ns("meta_col"), "Numeric column:", choices = NULL),
            "Only columns of this dataset holding at least two different
             numbers are listed.")
        ),
        conditionalPanel("input.source == 'gene'", ns = ns,
          os_field(
            selectInput(ns("unit"), "Normalization unit:", choices = NULL),
            "Which normalized matrix to read the gene from. TPM and TMM are not
             comparable, so the unit becomes part of the saved column's name."),
          os_field(
            selectizeInput(ns("gene"), "Gene:", choices = NULL,
                           options = list(server = TRUE, maxOptions = 1000)),
            "Type to search. The list holds every symbol in this dataset's
             matrix for the chosen unit.")
        )
      ),
      os_panel(title = "Method", collapse = TRUE,
        os_field(
          radioButtons(ns("method"), "Choose the cutpoint by:",
                       choices = c("Survival (log-rank)"     = "survival",
                                   "Binary outcome"          = "binary",
                                   "Distribution (mixture)"  = "distribution")),
          "Survival keeps the split whose log-rank test separates the curves
           best. Binary outcome keeps the split that best predicts a two-valued
           column. Distribution uses no outcome at all: it fits two gaussians to
           the variable and cuts where they cross."),
        conditionalPanel("input.method == 'survival'", ns = ns,
          os_field(
            selectInput(ns("time_col"),  "Time column:", choices = NULL),
            "Follow-up time, in whatever unit the dataset uses. Samples with no
             time, or a time of zero, are left out."),
          os_field(
            selectInput(ns("event_col"), "Event column (1 = event, 0 = censored):",
                        choices = NULL),
            "Only columns holding just 0 and 1 are listed. 1 means the event
             happened; 0 means the sample was censored.")
        ),
        conditionalPanel("input.method == 'binary'", ns = ns,
          os_field(
            selectInput(ns("outcome_col"), "Outcome column:", choices = NULL),
            "A column of this dataset with exactly two distinct values."),
          os_field(
            selectInput(ns("positive"), "Which level is the event:", choices = NULL),
            "The value the cutoff should predict. Sensitivity and specificity
             are reported with respect to it."),
          os_field(
            radioButtons(ns("rule"), "Among the candidates, keep:",
                         choices = c("the smallest p"            = "significance",
                                     "the ROC corner, euclidean" = "euclidean",
                                     "the ROC corner, manhattan" = "manhattan")),
            "The smallest p keeps the most significant 2x2 table. The two ROC
             rules keep the cutpoint closest to the top-left corner of the ROC
             curve, measured straight or along the axes.")
        ),
        conditionalPanel("input.method != 'distribution'", ns = ns,
          os_field(
            sliderInput(ns("min_frac"), "Smallest group allowed:",
                        min = 0, max = 0.4, value = 0.1, step = 0.05),
            "No candidate cutpoint may leave less than this fraction of the
             samples on either side. A split with a handful of samples on one
             side gives a huge ratio with a useless interval, which is the main
             way this method misleads."),
          os_field(
            numericInput(ns("perm"), "Permutations for the corrected p:",
                         value = 200, min = 0, max = 2000, step = 100),
            "How many times to shuffle the outcome and repeat the entire search.
             The corrected p is how often chance alone did as well as your data.
             0 skips it. With B permutations the smallest p obtainable is
             1/(B+1), so 200 cannot report below 0.005.")
        ),
        actionButton(ns("find"), "Find cutoff", class = "btn-primary")
      ),
      os_panel(title = "Split", collapse = TRUE,
        os_field(
          selectInput(ns("split_col"), "Split the analysis by:", choices = NULL),
          "Run the whole search separately inside each group of a categorical
           column -- sex, treatment, batch. Each group gets its own cutoff, its
           own curves and its own p. Numeric columns are not listed: a cutpoint
           is what this tab makes out of those."),
        uiOutput(ns("split_levels"))
      )
    ),
    center = tagList(
      os_panel(title = "Cutoff",
        plotOutput(ns("scan_plot"), height = "320px")
      ),
      os_panel(title = "Outcome",
        plotOutput(ns("outcome_plot"), height = "340px")
      )
    ),
    right = tagList(
      os_panel(title = "Result",
        uiOutput(ns("result"))
      ),
      os_panel(title = "Save", collapse = TRUE,
        os_field(
          textInput(ns("col_name"), "New metadata column:", value = ""),
          "The name the split gets in the metadata. Two cutoffs on one variable
           are two different columns, so the cutoff is part of the name."),
        actionButton(ns("save"), "Save split to metadata",
                     icon = icon("floppy-disk")),
        helpText("Writes \"low\"/\"high\" for the samples of this dataset only.
                  When the analysis is split, each sample is classified by the
                  cutoff of its own group.")
      )
    )
  )
}

cutoffFinderServer <- function(id, ds, meta, go_to = NULL, ai = NULL) {
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

    # Columns worth splitting on. Numeric ones are left out on purpose: turning
    # a number into groups is what this tab does, so offering one here would be
    # asking the researcher to do it twice, by hand and worse.
    categorical_cols <- reactive({
      md <- sel_meta()
      keep <- vapply(names(md), function(cn) {
        v <- as.character(md[[cn]])
        v <- unique(v[!is.na(v) & v != "NA"])
        length(v) >= 2 && length(v) <= 12 &&
          any(is.na(suppressWarnings(as.numeric(v))))
      }, logical(1))
      setdiff(names(md)[keep], c("SampleID", "dataset"))
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
      updateSelectInput(session, "split_col",
                        choices = c("No split" = "", categorical_cols()))

      units <- list_units(ds())
      updateSelectInput(session, "unit", choices = units)
      if (length(units)) {
        # server = TRUE: 54,000 symbols shipped to the browser is what makes
        # selectize warn and the tab crawl. The names come from the gene index,
        # so filling the menu no longer reads the matrix.
        updateSelectizeInput(session, "gene",
                             choices = os_gene_choices(ds(), units[1]), server = TRUE)
      }
    })

    observeEvent(input$unit, {
      req(ds(), input$unit)
      updateSelectizeInput(session, "gene",
                           choices = os_gene_choices(ds(), input$unit), server = TRUE)
    })

    observeEvent(input$outcome_col, {
      req(input$outcome_col)
      v <- as.character(sel_meta()[[input$outcome_col]])
      lv <- sort(unique(v[!is.na(v) & v != "NA"]))
      updateSelectInput(session, "positive", choices = lv,
                        selected = if (length(lv)) lv[2] else NULL)
    })

    # The output id cannot be the input id: both become the same DOM id and the
    # checkboxes end up inside the element that renders them.
    output$split_levels_ui <- renderUI({
      col <- input$split_col
      if (is.null(col) || !nzchar(col)) return(NULL)
      v  <- as.character(sel_meta()[[col]])
      lv <- sort(unique(v[!is.na(v) & v != "NA"]))
      n  <- vapply(lv, function(l) sum(v == l, na.rm = TRUE), integer(1))
      os_field(
        checkboxGroupInput(session$ns("split_levels"), "Groups to include:",
                           choices  = setNames(lv, paste0(lv, "  (n=", n, ")")),
                           selected = lv),
        "Which groups to include. A group too small for the search is reported
         rather than fitted, and the others still run.")
    })

    # --- the variable to cut, aligned with the metadata rows -----------------
    variable <- reactive({
      md <- sel_meta()
      if (identical(input$source, "gene")) {
        req(input$unit, input$gene)
        # One row, not the matrix: on GTEX that is 0.02 s instead of 14 s.
        row <- load_expression_row(ds(), input$gene, input$unit)
        if (is.null(row) || !nrow(row)) return(NULL)
        vals <- as_num(unlist(row[1, -1, drop = TRUE]))
        names(vals) <- names(row)[-1]
        list(name   = paste0(input$gene, ".", input$unit),
             values = unname(vals[match(md$SampleID, names(vals))]))
      } else {
        req(input$meta_col)
        list(name = input$meta_col, values = as_num(md[[input$meta_col]]))
      }
    })

    # --- the groups the search runs inside -----------------------------------
    strata <- reactive({
      md  <- sel_meta()
      col <- input$split_col
      if (is.null(col) || !nzchar(col)) {
        return(list(list(label = "All samples", keep = rep(TRUE, nrow(md)))))
      }
      v <- as.character(md[[col]])
      v[v == "NA"] <- NA
      lv <- input$split_levels
      if (!length(lv)) lv <- sort(unique(v[!is.na(v)]))
      lapply(lv, function(l) list(label = l, keep = !is.na(v) & v == l))
    })

    # --- one search, inside one group ----------------------------------------
    search_one <- function(values, md, keep0, label) {
      fail <- function(msg) list(label = label, error = msg)

      if (identical(input$method, "distribution")) {
        keep <- keep0 & !is.na(values)
        if (sum(keep) < CF_MIN_N) return(fail(cf_too_small(sum(keep))))
        dist <- cf_distribution(values[keep])
        if (is.null(dist)) {
          return(fail(paste("No two-component mixture could be fitted.",
                            "The distribution may be unimodal.")))
        }
        grp <- cf_split(values[keep], dist$cutoff)
        return(list(label = label, method = "distribution", dist = dist,
                    x = values[keep], cutoff = dist$cutoff,
                    n_low = sum(grp == "low"), n_high = sum(grp == "high")))
      }

      if (identical(input$method, "survival")) {
        req(input$time_col, input$event_col)
        time  <- as_num(md[[input$time_col]])
        event <- as_num(md[[input$event_col]])
        keep  <- keep0 & !is.na(values) & !is.na(time) & time > 0 &
                 !is.na(event) & event %in% c(0, 1)
        if (sum(keep) < CF_MIN_N) return(fail(cf_too_small(sum(keep))))
        scan <- cf_scan_survival(values[keep], time[keep], event[keep],
                                 input$min_frac)
        best <- cf_best(scan, "significance")
        if (is.null(best)) return(fail(cf_no_candidate(input$min_frac)))
        perm <- cf_perm(values[keep],
                        list(time = time[keep], event = event[keep]),
                        "survival", input$min_frac, input$perm, label)
        list(label = label, method = "survival", scan = scan, best = best,
             perm = perm, cutoff = best$cutoff,
             n_low = best$n_low, n_high = best$n_high,
             x = values[keep], time = time[keep], event = event[keep])
      } else {
        req(input$outcome_col, input$positive)
        y <- as.character(md[[input$outcome_col]])
        y[y == "NA"] <- NA
        keep <- keep0 & !is.na(values) & !is.na(y)
        if (sum(keep) < CF_MIN_N) return(fail(cf_too_small(sum(keep))))
        scan <- cf_scan_binary(values[keep], y[keep], input$positive,
                               input$min_frac)
        best <- cf_best(scan, input$rule)
        if (is.null(best)) return(fail(cf_no_candidate(input$min_frac)))
        perm <- cf_perm(values[keep], list(y = y[keep]), "binary",
                        input$min_frac, input$perm, label)
        list(label = label, method = "binary", scan = scan, best = best,
             perm = perm, cutoff = best$cutoff,
             n_low = best$n_low, n_high = best$n_high, auc = cf_auc(scan))
      }
    }

    # eventReactive, so nothing scans while the researcher is still choosing:
    # a permutation run is seconds of CPU per group and must be asked for.
    # The human button and the question tab reach the plot by the same path:
    # os_ai_gate() counts both, so nothing here has to know which one asked.
    draw <- os_ai_gate(id, input, ai, session, button = "find")

    found <- eventReactive(draw(), {
      req(draw() > 0)
      md <- sel_meta()
      vv <- variable()
      st <- strata()
      if (is.null(vv) || all(is.na(vv$values))) {
        return(list(var = NULL, split = NULL, strata = st, groups = list(
          list(label = "All samples",
               error = "That variable has no numeric values in this dataset."))))
      }
      list(var    = vv,
           method = input$method,
           split  = if (isTRUE(nzchar(input$split_col))) input$split_col else NULL,
           strata = st,
           groups = lapply(st, function(s) search_one(vv$values, md, s$keep, s$label)))
    })

    # A reactive rather than a value computed inside the observer: a test can
    # read this, and updateTextInput() cannot be read back without a browser.
    default_name <- reactive({
      f  <- found()
      gs <- cf_ok_groups(f)
      if (!length(gs)) return("")
      cf_column_name(f$var$name,
                     cutoff = if (is.null(f$split)) gs[[1]]$cutoff else NULL,
                     split  = f$split)
    })

    observeEvent(found(), {
      nm <- default_name()
      if (nzchar(nm)) updateTextInput(session, "col_name", value = nm)
    })

    # --- plots ---------------------------------------------------------------
    # One panel per group, combined with patchwork instead of faceting: each
    # group has its own cutoff and its own scale, and a shared facet scale
    # would make two unrelated searches look like one.
    panels <- function(f, which) {
      ps <- lapply(cf_ok_groups(f), function(g) {
        p <- if (identical(which, "scan")) {
          if (identical(g$method, "distribution"))
            cf_plot_mixture(g$x, g$dist, f$var$name)
          else cf_plot_scan(g$scan, g$cutoff)
        } else {
          if (identical(g$method, "survival"))
            cf_plot_km(g$x, g$time, g$event, g$cutoff, f$var$name)
          else if (identical(g$method, "binary"))
            cf_plot_roc(g$scan, g$cutoff)
          else NULL
        }
        if (!is.null(p) && !is.null(f$split)) p <- p + ggtitle(g$label)
        p
      })
      Filter(Negate(is.null), ps)
    }

    output$scan_plot <- renderPlot({
      f <- found()
      ps <- panels(f, "scan")
      # shiny::validate, spelled out. global.R attaches jsonlite after shiny, so
      # a bare validate() is jsonlite's -- it asks whether a string is valid
      # JSON. need(FALSE, msg) returns the message, jsonlite happily says it is
      # not JSON, and the render carries on past the guard it was meant to
      # stop; need(TRUE, msg) returns NULL and it errors with
      # "is.character(txt) is not TRUE". Both paths wrong, neither obvious.
      if (!length(ps)) shiny::validate(need(FALSE, cf_nothing_drawn(f)))
      patchwork::wrap_plots(ps, ncol = os_facet_cols(length(ps)))
    })

    output$outcome_plot <- renderPlot({
      f <- found()
      if (identical(f$method, "distribution")) {
        shiny::validate(need(FALSE, paste("The mixture method uses no outcome.",
                                   "The split is above.")))
      }
      ps <- panels(f, "outcome")
      if (!length(ps)) shiny::validate(need(FALSE, " "))
      patchwork::wrap_plots(ps, ncol = os_facet_cols(length(ps)))
    })

    # --- the numbers, with the warning the paper asks for -------------------
    output$result <- renderUI({
      f <- found()
      split <- !is.null(f$split)

      blocks <- lapply(f$groups, function(g) {
        head <- if (split) tags$div(class = "os-section", g$label) else NULL
        if (!is.null(g$error)) {
          return(tagList(head, tags$div(class = "text-warning", g$error)))
        }
        rows <- list(os_kv_row("Cutoff", format(round(g$cutoff, 3))),
                     os_kv_row("Samples", paste0("low ", g$n_low, " / high ", g$n_high)))
        if (!identical(g$method, "distribution")) {
          eff <- attr(g$scan, "effect_name")
          rows <- c(rows, list(
            os_kv_row(paste0(eff, " (95% CI)"),
                   sprintf("%.2f (%.2f - %.2f)", g$best$effect, g$best$lower,
                           g$best$upper)),
            os_kv_row("p at the best cutoff", format.pval(g$best$pvalue, digits = 3)),
            os_kv_row("p, permutation-corrected",
                   if (is.na(g$perm$p)) "not run"
                   else sprintf("%.3f  (%d permutations)", g$perm$p, g$perm$B))))
          if (identical(g$method, "binary")) {
            rows <- c(rows, list(
              os_kv_row("Sensitivity / specificity",
                     sprintf("%.2f / %.2f", g$best$sensitivity, g$best$specificity)),
              os_kv_row("AUC", sprintf("%.3f", g$auc))))
          }
        }
        tagList(head, tags$table(class = "table table-condensed", tags$tbody(rows)))
      })

      tagList(
        if (!is.null(f$var))
          tags$table(class = "table table-condensed", tags$tbody(
            os_kv_row("Variable", f$var$name),
            if (split) os_kv_row("Split by", f$split))),
        blocks,
        if (any(vapply(f$groups, function(g) !identical(g$method, "distribution") &&
                                             is.null(g$error), logical(1))))
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
      f  <- found()
      gs <- cf_ok_groups(f)
      if (!length(gs)) {
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

      # Each group is classified by its own cutoff. A sample in no included
      # group keeps "NA": it was not part of any search, so it has no side.
      vals <- rep("NA", length(idx))
      for (i in seq_along(f$groups)) {
        g <- f$groups[[i]]
        if (!is.null(g$error)) next
        keep <- f$strata[[i]]$keep
        side <- as.character(cf_split(f$var$values, g$cutoff))
        side[is.na(side)] <- "NA"
        vals[keep] <- side[keep]
      }

      after <- before
      if (!col %in% names(after)) after[[col]] <- "NA"
      after[[col]] <- as.character(after[[col]])
      after[[col]][idx] <- vals

      save_metadata(before, after)
      showNotification(
        paste0("Saved ", col, " for ", sum(vals != "NA"), " of ", length(idx),
               " samples."),
        type = "message", duration = 5)
    })
  })
}

# --- small shared pieces, outside the server ---------------------------------

# The groups that produced a cutoff. A group that refused is still reported,
# but it has nothing to plot or save.
cf_ok_groups <- function(f) Filter(function(g) is.null(g$error), f$groups)

# What to put in an empty plot panel: the groups' own refusals, not a generic
# "no data", so the researcher reads why without opening anything.
cf_nothing_drawn <- function(f) {
  msgs <- unique(vapply(f$groups, function(g) g$error %||% "", character(1)))
  msgs <- msgs[nzchar(msgs)]
  if (!length(msgs)) return("Nothing to draw.")
  if (length(msgs) == 1 || is.null(f$split)) return(msgs[1])
  paste(vapply(f$groups, function(g)
    if (is.null(g$error)) "" else paste0(g$label, ": ", g$error),
    character(1))[vapply(f$groups, function(g) !is.null(g$error), logical(1))],
    collapse = "\n\n")
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
# already writes: variable, then the cutoff with the dot replaced. A split
# analysis has one cutoff per group, so it is named after the grouping instead.
cf_column_name <- function(var_name, cutoff = NULL, split = NULL) {
  safe <- function(x) gsub("[^A-Za-z0-9._]", "_", x)
  if (!is.null(split)) return(paste0(safe(var_name), ".", safe(split), "_split"))
  paste0(safe(var_name), ".",
         gsub("\\.", "_", format(round(cutoff, 2), nsmall = 2)))
}

# Permutations are the slow part, so they report progress and can be skipped.
cf_perm <- function(x, outcome, kind, min_frac, B, label = NULL) {
  B <- suppressWarnings(as.integer(B))
  if (is.na(B) || B < 1) return(list(p = NA_real_, B = 0L))
  msg <- if (is.null(label) || identical(label, "All samples")) {
    "Permuting the outcome"
  } else {
    paste0("Permuting the outcome: ", label)
  }
  withProgress(message = msg, value = 0.5, {
    cf_permutation_p(x, outcome, kind, min_frac, B)
  })
}
