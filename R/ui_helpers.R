# R/ui_helpers.R -- the app's own markup.
#
# Replaces shinydashboard's box(). AdminLTE kept painting the body of a
# status="primary" box white through a literal colour at high specificity with
# !important, and since the three-column refactor rewrites every module's UI
# anyway, owning the markup costs nothing and ends the fight.
#
# os_panel() is deliberately plain: a header and a body, styled entirely by
# www/omicscalpel.css. No status colours, no collapse, no tools -- none of the
# nine modules used them for anything but decoration.

os_panel <- function(..., title = NULL, class = NULL) {
  tags$div(
    class = paste("os-panel", class),
    if (!is.null(title)) tags$div(class = "os-panel-head", title),
    tags$div(class = "os-panel-body", ...)
  )
}

# The three columns every analysis tab is laid out in. The middle one is the
# analysis; lint_layout.R checks that what the registry declares ends up here.
os_layout <- function(left = NULL, center = NULL, right = NULL,
                      widths = c(3, 6, 3)) {
  stopifnot(length(widths) == 3, sum(widths) == 12)
  fluidRow(
    column(widths[1], class = "os-left",   left),
    column(widths[2], class = "os-center", center),
    column(widths[3], class = "os-right",  right)
  )
}

# A section label inside a column, for grouping controls without a full panel.
os_label <- function(text) tags$div(class = "os-section", text)
