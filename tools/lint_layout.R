#!/usr/bin/env Rscript
# lint_layout.R -- is each module laid out as the registry says?
#
#   Rscript tools/lint_layout.R [module_id ...]
#
# The redesign puts the analysis in the middle with tools either side. That is
# a property of the rendered page, not of the source, so it is checked by
# rendering <name>UI("t") and looking at where each box actually landed.
#
# The map lives in R/registry.R, so the spec handed to the code model and the
# check applied to its output read the same source. A module declaring
# layout$exception is only checked for rendering at all.

source("global.R")

ok_n <- 0L; bad <- 0L
say  <- function(...) cat(..., "\n", sep = "")

# Walk the tag tree, not the HTML string: a box can contain its own columns,
# and counting col-sm-* in the rendered text picks those up too. The first
# attempt did exactly that and reported 3+12+12 for a module whose real
# top-level split is 3+6+3.

has_class <- function(tag, cls) {
  a <- tag$attribs$class
  !is.null(a) && any(grepl(paste0("(^| )", cls), a))
}

is_tag <- function(x) inherits(x, "shiny.tag")

children_of <- function(x) {
  if (is_tag(x)) x$children
  else if (is.list(x)) x
  else list()
}

# every box title anywhere below this node
# os_panel() puts its title in .os-panel-head; a module not yet converted still
# uses shinydashboard's box(), whose title is an h3.box-title. Both count.
titles_below <- function(x, acc = character(0)) {
  if (is_tag(x) && (has_class(x, "box-title") || has_class(x, "os-panel-head"))) {
    acc <- c(acc, trimws(paste(unlist(x$children), collapse = "")))
  }
  for (ch in children_of(x)) acc <- titles_below(ch, acc)
  acc
}

# the first div.row in the tree, and its direct col-sm-* children
top_columns <- function(x) {
  if (is_tag(x) && has_class(x, "row")) {
    cols <- list()
    for (ch in children_of(x)) {
      if (is_tag(ch) && has_class(ch, "col-sm-")) {
        w <- as.integer(sub(".*col-sm-([0-9]+).*", "\\1", ch$attribs$class))
        cols[[length(cols) + 1L]] <- list(width = w, titles = titles_below(ch))
      }
    }
    if (length(cols)) return(cols)
  }
  for (ch in children_of(x)) {
    got <- top_columns(ch)
    if (length(got)) return(got)
  }
  list()
}

pick <- commandArgs(TRUE)
mods <- if (length(pick)) MODULES[pick] else MODULES

for (m in mods) {
  if (!file.exists(module_file(m)) || !exists(m$ui, mode = "function")) next
  ui <- tryCatch(get(m$ui)("t"), error = function(e) e)
  if (inherits(ui, "error")) {
    bad <- bad + 1L; say(m$id, ": UI failed to build: ", conditionMessage(ui)); next
  }

  if (!is.null(m$layout$exception)) {
    ok_n <- ok_n + 1L; say(m$id, ": exception (", m$layout$exception, "), rendering only"); next
  }

  cols <- top_columns(ui)
  if (length(cols) != 3L) {
    bad <- bad + 1L; say(m$id, ": expected three top-level columns, found ", length(cols)); next
  }

  # the three widest top-level columns, in document order, are left/center/right
  three <- cols
  total <- sum(vapply(three, function(c) c$width, integer(1)))
  if (total != 12L) {
    bad <- bad + 1L
    say(m$id, ": the first three columns are ",
        paste(vapply(three, function(c) c$width, integer(1)), collapse = "+"),
        " = ", total, ", not 12")
    next
  }

  problems <- character(0)
  sides <- c("left", "center", "right")
  seen  <- character(0)
  for (i in seq_len(3)) {
    want <- m$layout[[sides[i]]]
    got  <- three[[i]]$titles
    seen <- c(seen, got)
    missing <- setdiff(want, got)
    extra   <- setdiff(got, want)
    if (length(missing)) problems <- c(problems, paste0(sides[i], " is missing: ", paste(missing, collapse = ", ")))
    if (length(extra))   problems <- c(problems, paste0(sides[i], " has unexpected: ", paste(extra, collapse = ", ")))
  }
  declared <- unlist(m$layout[sides])
  orphan   <- setdiff(seen, declared)
  if (length(orphan)) problems <- c(problems, paste0("box not in the registry: ", paste(unique(orphan), collapse = ", ")))

  if (length(problems)) {
    bad <- bad + 1L
    say(m$id, ":")
    for (p in problems) say("  ", p)
  } else {
    ok_n <- ok_n + 1L
    say(m$id, ": ok (", paste(vapply(three, function(c) c$width, integer(1)), collapse = "/"), ")")
  }
}

say("")
say(ok_n, " ok, ", bad, " with problems")
quit(status = if (bad > 0L) 1L else 0L)
