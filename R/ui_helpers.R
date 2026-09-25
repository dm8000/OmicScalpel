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
  cid <- os_next_id(title)
  tags$div(
    class = paste("os-panel os-collapsible", class),
    tags$input(type = "checkbox", class = "os-collapse-toggle", id = cid,
               checked = if (collapsed) "checked"),
    tags$label(class = "os-panel-head", `for` = cid,
               tags$span(class = "os-caret"), title),
    body
  )
}

# A unique id per panel on the page.
#
# Deriving it from the title alone gave 76 panels 22 ids: "Labels" exists in
# three modules and "Color selection" in three more, and a <label for="x">
# drives the first element with that id -- so folding Labels in one tab folded
# the hidden one in another and the panel you clicked did nothing. A counter
# is enough: the UI is built once per page, and nothing in R addresses these.
.os_id_counter <- local({
  e <- getOption("omicscalpel.panel_counter")
  if (is.null(e)) { e <- new.env(parent = emptyenv()); e$n <- 0L
                    options(omicscalpel.panel_counter = e) }
  e
})

os_next_id <- function(title) {
  .os_id_counter$n <- .os_id_counter$n + 1L
  slug <- gsub("[^A-Za-z0-9]", "", substr(paste(title, collapse = ""), 1, 12))
  paste0("os-c-", slug, "-", .os_id_counter$n)
}

# The three columns every analysis tab is laid out in. The middle one is the
# analysis; lint_layout.R checks that what the registry declares ends up here.
# A tab with nothing to put on the right gets two columns instead of an empty
# third: metadata-editor keeps its tools together on the left and gives the
# table the rest of the width.
os_layout <- function(left = NULL, center = NULL, right = NULL,
                      widths = c(3, 6, 3)) {
  stopifnot(length(widths) == 3, sum(widths) == 12)

  empty <- function(x) is.null(x) || (is.list(x) && !length(x))
  if (empty(right)) {
    return(fluidRow(
      column(widths[1],              class = "os-left",   left),
      column(widths[2] + widths[3],  class = "os-center", center)
    ))
  }

  fluidRow(
    column(widths[1], class = "os-left",   left),
    column(widths[2], class = "os-center", center),
    column(widths[3], class = "os-right",  right)
  )
}

# A section label inside a column, for grouping controls without a full panel.
os_label <- function(text) tags$div(class = "os-section", text)

# --- plot theme -------------------------------------------------------------
#
# The figures keep a white background on purpose: they are the exported
# artefact and go into papers. What is shared here is everything else -- type
# scale, grid weight, how the panel is framed -- so a plot from any tab looks
# like it came from the same app, and the app's amber reads as the accent it is
# rather than a colour someone picked once.

OS_PLOT <- list(
  ink     = "#2b2b2b",   # text and axes
  muted   = "#6b6b6b",   # secondary text
  grid    = "#e6e6e6",
  frame   = "#cfcfcf",
  accent  = "#d98e3a",   # the app's amber, on white
  accent2 = "#3f7d8c"    # its complement, for a second series
)

# A categorical palette that sits beside the amber instead of fighting it.
os_palette <- function(n) {
  base <- c("#d98e3a", "#3f7d8c", "#8c6e97", "#6f9457", "#c2695b",
            "#4f7ca8", "#b08b4f", "#7a7a7a", "#9c5f7c", "#5f8f7a",
            "#a87f5f", "#557a8c")
  if (n <= length(base)) base[seq_len(n)] else grDevices::colorRampPalette(base)(n)
}

os_theme <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background   = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border       = ggplot2::element_rect(fill = NA, colour = OS_PLOT$frame,
                                                 linewidth = .4),
      panel.grid.major   = ggplot2::element_line(colour = OS_PLOT$grid, linewidth = .35),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(colour = OS_PLOT$muted, size = base_size - 2),
      axis.title         = ggplot2::element_text(colour = OS_PLOT$ink, size = base_size - 1),
      axis.ticks         = ggplot2::element_line(colour = OS_PLOT$frame, linewidth = .3),
      plot.title         = ggplot2::element_text(colour = OS_PLOT$ink, face = "bold",
                                                 size = base_size + 2,
                                                 margin = ggplot2::margin(b = 8)),
      plot.subtitle      = ggplot2::element_text(colour = OS_PLOT$muted),
      legend.title       = ggplot2::element_text(colour = OS_PLOT$muted,
                                                 size = base_size - 2),
      legend.text        = ggplot2::element_text(colour = OS_PLOT$ink,
                                                 size = base_size - 2),
      legend.key         = ggplot2::element_blank(),
      strip.background   = ggplot2::element_rect(fill = "#f4f4f4", colour = OS_PLOT$frame,
                                                 linewidth = .4),
      strip.text         = ggplot2::element_text(colour = OS_PLOT$ink, face = "bold",
                                                 size = base_size - 2,
                                                 margin = ggplot2::margin(4, 4, 4, 4)),
      plot.margin        = ggplot2::margin(10, 12, 8, 8)
    )
}

# Geom defaults, set once in global.R. ggplot's own defaults are tuned for a
# grey theme: black outlines, 0.5pt lines, size-1.5 points. On white with a
# thin frame they read as heavy and the fills fight the palette.
os_set_plot_defaults <- function() {
  ggplot2::theme_set(os_theme())

  g <- ggplot2::update_geom_defaults
  g("boxplot",   list(colour = OS_PLOT$ink, linewidth = .4, outlier.size = 1,
                      outlier.colour = OS_PLOT$muted, outlier.alpha = .6))
  # geom_jitter draws with GeomPoint, so these are one setting, not two: the
  # first draft set point to the accent and jitter to grey, and jitter simply
  # overwrote it. Neutral and small serves both uses here -- a scatter, and
  # points strewn over a boxplot -- and a module that wants the accent asks
  # for it.
  g("point",     list(colour = "#4a4a4a", size = 1.5, alpha = .75))
  g("line",      list(colour = OS_PLOT$accent, linewidth = .6))
  g("smooth",    list(colour = OS_PLOT$accent2, linewidth = .7, fill = OS_PLOT$accent2,
                      alpha = .15))
  g("bar",       list(fill = OS_PLOT$accent, colour = NA))
  g("col",       list(fill = OS_PLOT$accent, colour = NA))
  g("histogram", list(fill = OS_PLOT$accent, colour = "white", linewidth = .25))
  g("vline",     list(colour = OS_PLOT$muted, linewidth = .4, linetype = "dashed"))
  g("hline",     list(colour = OS_PLOT$muted, linewidth = .4, linetype = "dashed"))
  g("text",      list(colour = OS_PLOT$ink, size = 3.2))
  invisible(TRUE)
}

# The same look for the plotly figures, which do not read the ggplot theme.
os_plotly <- function(p, title = NULL) {
  p |>
    plotly::layout(
      font       = list(family = "Helvetica, Arial, sans-serif",
                        size = 12, color = OS_PLOT$ink),
      title      = if (is.null(title)) NULL else
                     list(text = title, font = list(size = 14, color = OS_PLOT$ink)),
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      xaxis = list(gridcolor = OS_PLOT$grid, zerolinecolor = OS_PLOT$frame,
                   linecolor = OS_PLOT$frame, tickfont = list(color = OS_PLOT$muted)),
      yaxis = list(gridcolor = OS_PLOT$grid, zerolinecolor = OS_PLOT$frame,
                   linecolor = OS_PLOT$frame, tickfont = list(color = OS_PLOT$muted)),
      legend = list(font = list(color = OS_PLOT$ink, size = 11))
    ) |>
    plotly::config(displayModeBar = FALSE)
}
