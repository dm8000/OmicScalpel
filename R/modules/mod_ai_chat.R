# The tab that takes a question instead of controls.
#
# It asks Jev a fixed set of typed questions about what was asked, runs the
# answers through R/ai_plan.R, and hands back a button that opens the plot in
# the tab that can draw it. No model writes code, and none chooses a tool: the
# path from question to plot is in ai_plan.R and can be read.
#
# The answer always shows how the search was done, whether or not it found
# anything -- a refusal that does not say what it looked for is not an answer.

# Injectable so tools/behaviour/ai_chat.R runs without a network. The app never
# sets it; the option exists for tests and for tools/jev-cost.R.
ai_transport <- function() getOption("omicscalpel.jev_transport", jev_post)

aiChatUI <- function(id) {
  ns <- NS(id)
  os_layout(
    left = tagList(
      os_panel(title = "Scope", collapse = TRUE,
        uiOutput(ns("scope")),
        hr(),
        os_field(
          sliderInput(ns("min_confidence"), "Act on a choice only above:",
                      min = 0.3, max = 0.9, value = AI_MIN_CONFIDENCE, step = 0.05),
          "How sure the model has to be before the search acts on one of its
           answers. Below this the tab shows the alternatives and asks you,
           instead of guessing and drawing something plausible.")
      )
    ),
    center = tagList(
      os_panel(title = "Conversation",
        div(class = "os-chat", uiOutput(ns("thread"))),
        div(class = "os-chat-entry",
          textAreaInput(ns("question"), NULL, width = "100%", rows = 2,
                        placeholder = "Does leptin expression increase with obesity in adipose tissue?"),
          div(style = "text-align: right;",
              actionButton(ns("ask"), "Ask", class = "btn-primary"))
        )
      )
    ),
    right = tagList(
      os_panel(title = "Cost",
        uiOutput(ns("cost"))
      )
    )
  )
}

aiChatServer <- function(id, ds, meta, go_to = NULL, ai = NULL) {
  moduleServer(id, function(input, output, session) {

    exchanges <- reactiveVal(list())

    output$scope <- renderUI({
      cat_ <- ai_catalog(meta())
      idx <- ai_gene_index_status()
      tagList(
        tags$div(class = "os-section", "What can be searched"),
        tags$p(sprintf("%d datasets, %s, %s.", length(cat_),
                       paste(ai_facet_values(cat_, "species"), collapse = " and "),
                       paste(utils::head(ai_facet_values(cat_, "data_type"), 4),
                             collapse = ", "))),
        tags$p(class = if (idx$ok) "text-muted" else "text-warning", idx$message),
        tags$p(class = "text-muted", style = "font-size: 12px;",
               "The search reads the catalog and the metadata. It never sends ",
               "expression values or sample identifiers anywhere.")
      )
    })

    # --- asking ---------------------------------------------------------------

    observeEvent(input$ask, {
      q <- trimws(input$question)
      if (!nzchar(q)) return()

      hist <- lapply(exchanges(), function(e) e$question)
      # The last plan that produced a plot, so "does that differ between the
      # sexes?" knows what "that" was.
      done <- Filter(function(e) isTRUE(e$plan$ok), exchanges())
      prev <- if (length(done)) done[[length(done)]]$plan else NULL
      res <- withProgress(message = "Reading the catalog", value = 0.5, {
        tryCatch(
          ai_answer(q, catalog = ai_catalog(meta()), history = hist,
                    transport = ai_transport(), previous = prev,
                    min_confidence = input$min_confidence %||% AI_MIN_CONFIDENCE),
          error = function(e) list(ok = FALSE, kind = "error",
                                   headline = conditionMessage(e),
                                   trace = list(), question = q))
      })

      ex <- exchanges()
      ex[[length(ex) + 1L]] <- list(n = length(ex) + 1L, question = q, plan = res)
      exchanges(ex)
      updateTextAreaInput(session, "question", value = "")
    })

    # --- the thread -----------------------------------------------------------

    output$thread <- renderUI({
      ex <- exchanges()
      if (!length(ex)) {
        return(tags$p(class = "text-muted",
                      "Ask a question about the data. The answer is a plot, in the tab that can draw it."))
      }
      ns <- session$ns
      tagList(lapply(rev(ex), function(e) {
        p <- e$plan
        tags$div(class = "os-chat-turn",
          tags$div(class = "os-chat-q", e$question),
          tags$div(class = "os-chat-a",
            tags$p(class = if (isTRUE(p$ok)) "os-chat-head" else "os-chat-head text-warning",
                   p$headline),
            if (isTRUE(p$ok) && !is.na(AI_TOOLS[[p$tab]]$title %||% NA))
              actionButton(ns(paste0("open_", e$n)),
                           paste0("Open in ", AI_TOOLS[[p$tab]]$title, "  →"),
                           class = "btn-primary btn-sm"),
            if (identical(p$kind, "ambiguous") && length(p$options))
              tagList(
                tags$p(class = "text-muted", style = "font-size:12px;",
                       "Which did you mean?"),
                lapply(seq_along(p$options), function(i) {
                  actionButton(ns(paste0("pick_", e$n, "_", i)), p$options[i],
                               class = "btn-sm")
                })),
            ai_trace_ui(p$trace)
          ))
      }))
    })

    # The buttons are created as the thread grows, so their observers are too.
    # Registering them once per exchange keeps each one bound to its own plan.
    observe({
      ex <- exchanges()
      lapply(ex, function(e) {
        oid <- paste0("open_", e$n)
        if (is.null(session$userData[[oid]])) {
          session$userData[[oid]] <- TRUE
          observeEvent(input[[oid]], {
            p <- exchanges()[[e$n]]$plan
            req(isTRUE(p$ok))
            if (!is.null(go_to)) go_to(tab = p$tab, dataset = p$dataset)
            if (!is.null(ai)) ai(p)
          }, ignoreInit = TRUE)
        }
        if (identical(e$plan$kind, "ambiguous")) {
          lapply(seq_along(e$plan$options), function(i) {
            pid <- paste0("pick_", e$n, "_", i)
            if (is.null(session$userData[[pid]])) {
              session$userData[[pid]] <- TRUE
              observeEvent(input[[pid]], {
                # Disambiguation costs nothing: the model already answered, and
                # only the answer being argued about is replaced.
                old <- exchanges()
                e2 <- old[[e$n]]
                fixed <- e2$plan$answers
                fixed$variable <- list(type = "choice", choice = e2$plan$options[i],
                                       probabilities = stats::setNames(list(1),
                                                                       e2$plan$options[i]),
                                       confidence = 1)
                replan <- ai_decide(fixed, ai_catalog(meta()), e2$plan$genes,
                                    min_confidence = input$min_confidence)
                replan$question <- e2$question
                replan$answers <- fixed
                replan$genes <- e2$plan$genes
                old[[e$n]]$plan <- replan
                exchanges(old)
              }, ignoreInit = TRUE)
            }
          })
        }
        NULL
      })
    })

    # --- cost -----------------------------------------------------------------

    output$cost <- renderUI({
      ex <- exchanges()
      spent <- vapply(ex, function(e) e$plan$cost %||% NA_real_, numeric(1))
      toks  <- vapply(ex, function(e) as.numeric(e$plan$usage$input_tokens %||% NA), numeric(1))
      all_time <- jev_log_read()
      tagList(
        tags$table(class = "table table-condensed", tags$tbody(
          os_kv_row("Questions asked", length(ex)),
          os_kv_row("Last question",
                 if (!length(toks) || is.na(utils::tail(toks, 1))) "-"
                 else sprintf("%s tokens, $%.6f", format(utils::tail(toks, 1), big.mark = ","),
                              utils::tail(spent, 1))),
          os_kv_row("This session",
                 sprintf("$%.6f", sum(spent, na.rm = TRUE))),
          os_kv_row("All questions logged",
                 sprintf("%d, $%.4f", nrow(all_time), sum(all_time$cost_usd, na.rm = TRUE)))
        )),
        tags$p(class = "text-muted", style = "font-size: 12px;",
               "Input tokens are billed at $0.042 per million; output is free. ",
               "One question sends the catalog once and asks every question ",
               "against it in a single request.")
      )
    })
  })
}

# The path the search took, as a list anyone can read. Collapsed by default:
# it is the evidence, not the answer.
ai_trace_ui <- function(trace) {
  if (!length(trace)) return(NULL)
  os_panel(title = "How this was searched", collapse = TRUE, collapsed = TRUE,
    tags$table(class = "table table-condensed",
      tags$tbody(lapply(trace, function(s) {
        tags$tr(
          tags$td(style = "width: 38%;", tags$strong(s$step)),
          tags$td(s$detail),
          tags$td(style = "width: 12%; text-align: right;",
                  if (!is.na(s$kept)) sprintf("%d left", s$kept) else "")
        )
      }))))
}
