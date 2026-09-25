# R/ai_tools.R -- the interface the planner drives, mirroring the human one.
#
# For each tab the question tab can operate: which controls may be set, what
# kind of control each is (which decides the update*() call), which ones must
# be set before the tab can draw anything, and the id of the button a person
# would press.
#
# The ids here are the real input ids. tools/lint_ai_tools.R fails if one of
# them is not in that module's UI, so this cannot drift when a control is
# renamed -- which is the only way a machine interface mirroring a human one
# stays honest.

AI_TOOLS <- list(

  correlation_analysis = list(
    title = "Correlation",
    what  = "gene expression against a continuous variable, one dataset",
    scope = "one",
    needs = c("genes", "numeric_columns"),
    controls = list(
      genes           = list(type = "selectize", multiple = TRUE),
      numeric_columns = list(type = "select"),
      conditions      = list(type = "select", multiple = TRUE)
    ),
    draw = "plot"
  ),

  across_datasets = list(
    title = "Across datasets",
    what  = "a gene's expression shown in each dataset separately, never compared between them",
    scope = "many",
    needs = c("genes"),
    controls = list(
      genes        = list(type = "selectize", multiple = TRUE),
      datasets     = list(type = "checkboxgroup", multiple = TRUE),
      split_col    = list(type = "select"),
      housekeeping = list(type = "checkbox"),
      log_y        = list(type = "checkbox")
    ),
    draw = "plot"
  ),

  meta_analysis = list(
    title = "Meta-analysis",
    what  = "one gene across several datasets at once, as a forest plot",
    scope = "many",
    needs = c("biomolecule", "condition"),
    controls = list(
      biomolecule       = list(type = "text"),
      condition         = list(type = "select"),
      group1_categories = list(type = "select", multiple = TRUE),
      group2_categories = list(type = "select", multiple = TRUE),
      numeric_split     = list(type = "select"),
      adjust_for        = list(type = "select", multiple = TRUE),
      data_preference   = list(type = "select"),
      stat_test         = list(type = "radio")
    ),
    draw = "generate_plot"
  ),

  compare_samples = list(
    title = "Compare samples",
    what  = "one gene between groups of samples, one dataset",
    scope = "one",
    needs = c("genes", "conditions"),
    controls = list(
      genes          = list(type = "selectize", multiple = TRUE),
      conditions     = list(type = "select", multiple = TRUE),
      log2_transform = list(type = "checkbox")
    ),
    draw = "plot"
  ),

  compare_genes = list(
    title = "Compare genes",
    what  = "several genes side by side within each group, one dataset",
    scope = "one",
    needs = c("genes"),
    controls = list(
      genes          = list(type = "selectize", multiple = TRUE),
      conditions     = list(type = "select", multiple = TRUE),
      log2_transform = list(type = "checkbox")
    ),
    draw = "plot"
  ),

  cutoff_finder = list(
    title = "Cutoff finder",
    what  = "the cutpoint in a variable that best separates survival or an outcome",
    scope = "one",
    needs = c("source", "method"),
    controls = list(
      source       = list(type = "radio"),
      meta_col     = list(type = "select"),
      unit         = list(type = "select"),
      gene         = list(type = "selectize"),
      method       = list(type = "radio"),
      time_col     = list(type = "select"),
      event_col    = list(type = "select"),
      outcome_col  = list(type = "select"),
      rule         = list(type = "radio"),
      split_col    = list(type = "select"),
      split_levels = list(type = "checkboxgroup", multiple = TRUE)
    ),
    draw = "find"
  ),

  # The fallback: there is data worth seeing but no tool that answers the
  # question. Nothing to set -- switching to it with the dataset selected is
  # the whole action.
  data_summary = list(
    title = "Dataset summary",
    what  = "what a dataset contains, when no plot can answer the question",
    scope = "many",
    needs = character(0),
    controls = list(),
    draw = NA_character_
  )
)

# --- applying a plan --------------------------------------------------------

# One update*() per control, chosen by the declared type. A module calls this
# on itself, in its own session, so the namespace is already right.
os_ai_apply <- function(session, tab, controls) {
  spec <- AI_TOOLS[[tab]]
  if (is.null(spec) || !length(controls)) return(invisible(FALSE))
  for (id in names(controls)) {
    ctl <- spec$controls[[id]]
    if (is.null(ctl)) next                      # not ours to set
    value <- controls[[id]]
    switch(ctl$type,
      select    = updateSelectInput(session, id, selected = value),
      # server = TRUE selectize holds no choices client-side, so the value has
      # to arrive with the choices or it is dropped on the floor.
      selectize = updateSelectizeInput(session, id, choices = value,
                                       selected = value, server = TRUE),
      text      = updateTextInput(session, id, value = value),
      radio     = updateRadioButtons(session, id, selected = value),
      checkbox  = updateCheckboxInput(session, id, value = as.logical(value)),
      checkboxgroup = updateCheckboxGroupInput(session, id, selected = value),
      numeric   = updateNumericInput(session, id, value = value),
      slider    = updateSliderInput(session, id, value = value),
      warning("os_ai_apply: no rule for control type ", ctl$type)
    )
  }
  invisible(TRUE)
}

# Have the values actually arrived? update*() is a message to the browser; the
# input does not change until the browser answers. Drawing before that renders
# the previous question's plot, which is worse than drawing late.
# Returns TRUE, or the id of the first control that has not arrived -- which
# is what the message says when the wait is given up on. "It did not work" is
# not a diagnosis; "condition never became BMI" is.
os_ai_ready <- function(input, tab, controls) {
  spec <- AI_TOOLS[[tab]]
  if (is.null(spec)) return(TRUE)
  for (id in names(controls)) {
    if (is.null(spec$controls[[id]])) next
    want <- controls[[id]]
    got  <- input[[id]]
    if (is.null(got)) return(id)
    ok <- if (isTRUE(spec$controls[[id]]$multiple)) {
      setequal(as.character(got), as.character(want))
    } else {
      identical(as.character(got)[1], as.character(want)[1])
    }
    if (!ok) return(id)
  }
  TRUE
}

# What a drivable module adds, in one call. It returns a reactive the module
# folds into its own draw gate: the human button and the planner then reach the
# plot by the same path, and the button keeps working exactly as before.
#
#   draw <- os_ai_gate(id, input, ai, session, button = "plot")
#   ... req(draw() > 0)
#
# `button` is named by the module, not looked up here: the human path must keep
# working whatever this file says, and testServer instantiates a module under
# an id that is in no table.
os_ai_gate <- function(id, input, ai, session, button) {
  draw    <- reactiveVal(0L)
  pending <- reactiveVal(NULL)

  # No ignoreInit: in a browser the button arrives as 0 with the page and the
  # first run is harmless, but under testServer no initial 0 ever comes, so
  # ignoreInit swallowed the real click. The > 0 guard covers both.
  observeEvent(input[[button]], {
    if (isTRUE(input[[button]] > 0)) draw(isolate(draw()) + 1L)
  })

  # A module the question tab cannot drive still gets its button counted.
  if (!is.null(ai) && !is.null(AI_TOOLS[[id]])) {
    observeEvent(ai(), {
      p <- ai()
      if (is.null(p) || !identical(p$tab, id)) return()
      os_ai_apply(session, id, p$controls)
      pending(p)
    }, ignoreNULL = TRUE)

    # Applying once is not enough. A module fills its own selectInput choices
    # from the metadata, and if that observer runs after the plan arrived it
    # resets the selection to the first choice -- the meta-analysis came up
    # showing Cell.type instead of BMI. The gate then correctly refused to
    # draw, which is the right failure and still the wrong screen. So it
    # reapplies until the inputs report what was asked for -- for up to 30
    # seconds, because this app re-reads a 1.5 MB spreadsheet on the way -- and
    # gives up out loud rather than looping.
    tries <- reactiveVal(0L)
    observe({
      p <- pending()
      req(p)
      state <- os_ai_ready(input, id, p$controls)
      if (isTRUE(state)) {
        pending(NULL)
        tries(0L)
        draw(isolate(draw()) + 1L)
        return()
      }
      n <- isolate(tries())
      if (n >= 60L) {
        pending(NULL)
        tries(0L)
        message("os_ai_gate(", id, "): gave up waiting for '", state, "'")
        showNotification(
          paste0("The Ask tab's setting for \"", state, "\" did not take here. ",
                 "The other controls are filled in; press the button to draw."),
          type = "warning", duration = 10)
        return()
      }
      if (n %% 10L == 0L) message("os_ai_gate(", id, "): waiting for '", state,
                                  "' (attempt ", n + 1L, ")")
      tries(n + 1L)
      os_ai_apply(session, id, p$controls)
      invalidateLater(500)
    })
  }

  draw
}
