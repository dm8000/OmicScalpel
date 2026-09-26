# One gene, several datasets, side by side -- and deliberately not compared.
#
# There is a batch effect between studies, so the number in one dataset does
# not mean the same as the number in another. What it does say is whether the
# gene is expressed in that tissue at all, and how abundantly, which is the
# question "is leptin expressed outside adipose?" actually asks. So: one panel
# per dataset, each labelled with its own unit, no statistic crossing between
# them, and housekeeping genes beside the query as a ruler.
#
# It reads only the rows asked for -- load_expression_row(), not
# load_expression() -- which is what makes "several datasets" affordable at
# all: the GTEX matrix alone is 14 seconds and 530 MB to read in full.

acrossDatasetsUI <- function(id) {
  ns <- NS(id)
  os_layout(
    left = tagList(
      os_panel(title = "Genes", collapse = TRUE,
        os_field(
          selectizeInput(ns("genes"), "Genes:", choices = NULL, multiple = TRUE,
                         options = list(server = TRUE, maxOptions = 1000,
                                        placeholder = "LEP, UCP1, ...")),
          "A few genes at a time. Every dataset is read for these rows only,
           so the cost is in how many genes you ask for, not how big the
           matrices are."),
        os_field(
          checkboxInput(ns("housekeeping"), "Add housekeeping genes as a ruler", TRUE),
          "Plots GAPDH, ACTB and the other reference genes beside yours, in the
           same panel. They are abundant in every tissue, so they tell you
           whether your gene is well expressed or barely there -- which the
           number alone does not."),
        os_field(
          checkboxInput(ns("log_y"), "Log10 scale", TRUE),
          "Expression spans orders of magnitude, and a housekeeping gene is a
           thousand times a transcription factor. On a linear axis everything
           but the most abundant gene is a flat line."),
        actionButton(ns("plot"), "Plot", class = "btn-primary")
      ),
      os_panel(title = "Datasets", collapse = TRUE,
        uiOutput(ns("dataset_picker"))
      ),
      os_panel(title = "Split", collapse = TRUE,
        os_field(
          selectInput(ns("split_col"), "Within each dataset, split by:", choices = NULL),
          "Applied inside each dataset, never across them. Only columns that
           are filled in and have two or more groups in at least one of the
           chosen datasets are listed.")
      )
    ),
    center = tagList(
      os_panel(title = "Panels",
        plotOutput(ns("panels"), height = "640px")
      )
    ),
    right = tagList(
      os_panel(title = "Read this",
        uiOutput(ns("caveat"))
      )
    )
  )
}

acrossDatasetsServer <- function(id, ds, meta, go_to = NULL, ai = NULL) {
  moduleServer(id, function(input, output, session) {

    draw <- os_ai_gate(id, input, ai, session, button = "plot")

    housekeeping <- function() {
      path <- file.path(os_root(), "config", "ai-housekeeping.txt")
      if (!file.exists(path)) return(character(0))
      v <- trimws(readLines(path, warn = FALSE))
      v[nzchar(v) & !startsWith(v, "#")]
    }

    # Every dataset that has an expression file at all.
    datasets <- reactive({
      md <- meta()
      all <- sort(unique(md$dataset))
      all[vapply(all, function(d) length(tryCatch(list_units(d), error = function(e) character(0))) > 0,
                 logical(1))]
    })

    observe({
      # The symbols come from the gene index, not from reading a matrix. Five
      # tabs used to load a whole matrix to fill a menu.
      vocab <- ai_gene_vocabulary()
      updateSelectizeInput(session, "genes", choices = vocab, server = TRUE)
    })

    output$dataset_picker <- renderUI({
      md <- meta()
      cat_ <- ai_catalog(md)
      ds_all <- datasets()
      labels <- vapply(ds_all, function(d) {
        e <- cat_[[d]]
        t <- if (!is.null(e) && length(e$tissue)) paste(e$tissue, collapse = "/") else "tissue not recorded"
        sprintf("%s  (%s, n=%d)", d, t, if (is.null(e)) 0L else e$n)
      }, character(1))
      checkboxGroupInput(session$ns("datasets"), NULL,
                         choices = stats::setNames(ds_all, labels),
                         selected = ds_all)
    })

    observe({
      md <- meta()
      chosen <- input$datasets
      if (!length(chosen)) chosen <- datasets()
      cols <- Reduce(union, lapply(chosen, function(d) {
        rows <- md[!is.na(md$dataset) & md$dataset == d, , drop = FALSE]
        names(rows)[vapply(names(rows), function(cn) {
          v <- ai_real(rows[[cn]])
          u <- unique(v)
          length(u) >= 2 && length(u) <= 12 && any(is.na(suppressWarnings(as.numeric(u))))
        }, logical(1))]
      }), character(0))
      cols <- setdiff(cols, ai_id_cols())
      updateSelectInput(session, "split_col", choices = c("No split" = "", sort(cols)),
                        selected = isolate(input$split_col))
    })

    # --- the data ------------------------------------------------------------

    gathered <- eventReactive(draw(), {
      req(draw() > 0, input$genes)
      md <- meta()
      chosen <- input$datasets
      if (!length(chosen)) chosen <- datasets()
      want <- unique(c(input$genes, if (isTRUE(input$housekeeping)) housekeeping()))

      # Sequential on purpose, and measured: one dataset is one grep, eighteen
      # of them take 0.38 s, and spreading that over eight cores saved 0.13 s.
      # The idle cores were a symptom of reading whole matrices, not of doing
      # the reads one at a time; with load_expression_row() there is nothing
      # left to parallelise here.
      split_col <- input$split_col %||% ""

      read_one <- function(d) {
        unit <- tryCatch(list_units(d)[1], error = function(e) NA_character_)
        if (is.na(unit)) return(NULL)
        m <- tryCatch(load_expression_row(d, want, unit), error = function(e) NULL)
        if (is.null(m) || !nrow(m)) {
          return(list(long = NULL,
                      note = list(dataset = d, unit = unit, n = 0L, found = character(0))))
        }
        meta_d <- md[!is.na(md$dataset) & md$dataset == d, , drop = FALSE]
        samples <- intersect(names(m)[-1], meta_d$SampleID)
        if (!length(samples)) samples <- names(m)[-1]

        split_vals <- rep("all samples", length(samples))
        if (nzchar(split_col) && split_col %in% names(meta_d)) {
          v <- as.character(meta_d[[split_col]][match(samples, meta_d$SampleID)])
          v[is.na(v) | v == "NA"] <- NA
          if (sum(!is.na(v)) > 0) split_vals <- v
        }

        # One allocation, not one data.frame per gene glued together: building
        # 13,000 rows by rbind was three of the four seconds this tab took,
        # while the reads themselves were four tenths.
        ns <- length(samples)
        vals <- as.numeric(t(as.matrix(m[, samples, drop = FALSE])))
        long <- data.frame(
          dataset = d, unit = unit,
          gene   = rep(m$Symbol, each = ns),
          sample = rep(samples, times = nrow(m)),
          value  = vals,
          group  = rep(split_vals, times = nrow(m)),
          stringsAsFactors = FALSE)
        list(long = long[!is.na(long$group), , drop = FALSE],
             note = list(dataset = d, unit = unit, n = length(samples),
                         found = m$Symbol))
      }

      got <- withProgress(message = sprintf("Reading %d datasets", length(chosen)),
                          value = 0, {
        lapply(seq_along(chosen), function(i) {
          incProgress(1 / length(chosen), detail = chosen[i])
          tryCatch(read_one(chosen[i]), error = function(e) NULL)
        })
      })
      got <- Filter(is.list, got)
      rows  <- Filter(Negate(is.null), lapply(got, `[[`, "long"))
      notes <- lapply(got, `[[`, "note")
      names(notes) <- vapply(notes, function(nt) nt$dataset, character(1))

      list(long = if (length(rows)) do.call(rbind, rows) else NULL,
           notes = notes, asked = want, query = input$genes)
    })

    # --- the panels ----------------------------------------------------------

    output$panels <- renderPlot({
      g <- gathered()
      shiny::validate(need(!is.null(g$long) && nrow(g$long) > 0,
                    paste("None of the datasets chosen carry these genes.",
                          "The Datasets panel says what each one holds.")))

      keep_hk <- housekeeping()
      panels <- lapply(split(g$long, g$long$dataset), function(df) {
        df$gene <- factor(df$gene, levels = c(intersect(g$query, unique(df$gene)),
                                              intersect(keep_hk, unique(df$gene))))
        df$kind <- ifelse(df$gene %in% keep_hk, "reference", "asked for")
        p <- ggplot(df, aes(x = gene, y = value, fill = group)) +
          geom_boxplot(outlier.size = .6, linewidth = .35,
                       position = position_dodge(preserve = "single")) +
          scale_fill_manual(values = os_palette(length(unique(df$group)))) +
          labs(title = df$dataset[1],
               subtitle = paste0(df$unit[1], ", n = ", length(unique(df$sample))),
               x = NULL, y = df$unit[1], fill = NULL) +
          os_theme(base_size = 11) +
          theme(legend.position = if (length(unique(df$group)) > 1) "bottom" else "none",
                axis.text.x = element_text(angle = 45, hjust = 1))
        if (isTRUE(input$log_y)) {
          p <- p + scale_y_continuous(trans = "log10",
                                      labels = function(x) format(x, scientific = FALSE, drop0trailing = TRUE))
        }
        p
      })
      patchwork::wrap_plots(panels, ncol = os_facet_cols(length(panels)))
    })

    output$caveat <- renderUI({
      g <- tryCatch(gathered(), error = function(e) NULL)
      tagList(
        tags$p(tags$strong("These panels are not comparable with each other."), " ",
               "Each dataset was prepared and normalised on its own, so the batch ",
               "effect between studies is larger than most biological differences. ",
               "Read each panel against its own housekeeping genes, not against ",
               "the panel beside it."),
        if (!is.null(g) && length(g$notes)) tagList(
          tags$div(class = "os-section", "What was read"),
          tags$table(class = "table table-condensed", tags$tbody(
            lapply(g$notes, function(nt) {
              tags$tr(tags$td(nt$dataset),
                      tags$td(nt$unit),
                      tags$td(paste0("n=", nt$n)),
                      tags$td(if (length(nt$found)) paste(nt$found, collapse = ", ")
                              else tags$span(class = "text-warning", "none of these genes")))
            })))),
        tags$p(class = "text-muted", style = "font-size: 12px;",
               "Only the rows for these genes are read from each matrix, which is ",
               "why several datasets at once is affordable.")
      )
    })
  })
}
