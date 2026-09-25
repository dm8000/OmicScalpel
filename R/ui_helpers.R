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

# collapse = TRUE gives the panel a header you can click to fold it away.
# It is plain CSS -- a hidden checkbox driving a sibling selector -- so a tool
# panel can be got out of the way without a round trip to the server, and
# without every module having to own an input id for it.
os_panel <- function(..., title = NULL, class = NULL,
                     collapse = FALSE, collapsed = FALSE) {
  body <- tags$div(class = "os-panel-body", ...)

  if (!collapse || is.null(title)) {
    return(tags$div(
      class = paste("os-panel", class),
      if (!is.null(title)) tags$div(class = "os-panel-head", title),
      body
    ))
  }

  # the id only has to be unique within the page, not addressable from R
  cid <- paste0("os-c-", substr(digest_id(title), 1, 10))
  tags$div(
    class = paste("os-panel os-collapsible", class),
    tags$input(type = "checkbox", class = "os-collapse-toggle", id = cid,
               checked = if (collapsed) "checked"),
    tags$label(class = "os-panel-head", `for` = cid,
               tags$span(class = "os-caret"), title),
    body
  )
}

# A stable id from the title. Not a hash for security -- just something legal
# in an HTML id that does not collide between panels on one page.
digest_id <- function(x) {
  v <- utf8ToInt(paste(x, collapse = ""))
  paste0(gsub("[^A-Za-z0-9]", "", paste(x, collapse = "")), sum(v))
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
