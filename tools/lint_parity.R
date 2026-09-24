#!/usr/bin/env Rscript
# lint_parity.R -- did the conversion lose a control?
#
#   Rscript tools/lint_parity.R [module_id ...]
#
# For each converted module, compare the set of input and output ids against
# the legacy app it came from, and report the ones that disappeared.
#
# This exists because metadata-editor lost its "Add Column" and "Build metadata
# table" buttons in conversion. The module parsed, passed the namespace linter
# and started under the smoke test; the buttons were simply not on the page,
# and the server still listened for one of them. Nothing else could see that.
#
# Deliberate removals go in tools/parity-exceptions.txt, one per line:
#   <module_id> <id>   # why

source("R/config.R")
source("R/registry.R")

ID_EXTRA <- c("actionButton", "actionLink", "downloadButton", "downloadLink",
              "radioButtons")

ids_of <- function(path) {
  found <- character(0)
  walk <- function(x) {
    if (!is.call(x)) return(invisible(NULL))
    fn <- x[[1]]
    if (is.call(fn) && is.symbol(fn[[1]]) &&
        as.character(fn[[1]]) %in% c("::", ":::")) fn <- fn[[3]]
    if (is.symbol(fn)) {
      nm <- as.character(fn)
      if (!startsWith(nm, "update") &&
          (grepl("(Input|Output)$", nm) || nm %in% ID_EXTRA || nm == "add_rank_list")) {
        a <- as.list(x)[-1]
        nms <- names(a); if (is.null(nms)) nms <- rep("", length(a))
        pick <- NULL
        if (nm == "add_rank_list") {
          k <- which(nms == "input_id"); if (length(k)) pick <- a[[k[1]]]
        } else {
          for (want in c("inputId", "outputId")) {
            k <- which(nms == want); if (length(k)) { pick <- a[[k[1]]]; break }
          }
          if (is.null(pick)) { k <- which(nms == ""); if (length(k)) pick <- a[[k[1]]] }
        }
        # ns("x") and session$ns("x") count as the id "x"
        if (is.call(pick) && length(pick) == 2L) pick <- pick[[2]]
        if (is.character(pick) && length(pick) == 1L) found[[length(found) + 1L]] <<- pick
      }
    }
    for (i in seq_along(x)) walk(x[[i]])
    invisible(NULL)
  }
  for (e in parse(path)) walk(e)
  unique(found)
}

exc_file <- file.path("tools", "parity-exceptions.txt")
exceptions <- list()
if (file.exists(exc_file)) {
  for (l in readLines(exc_file, warn = FALSE)) {
    l <- trimws(sub("#.*$", "", l))
    if (!nzchar(l)) next
    parts <- strsplit(l, "[[:space:]]+")[[1]]
    if (length(parts) >= 2) exceptions[[parts[1]]] <- c(exceptions[[parts[1]]], parts[2])
  }
}

pick <- commandArgs(TRUE)
mods <- if (length(pick)) MODULES[pick] else MODULES
bad <- 0L

for (m in mods) {
  f <- module_file(m)
  legacy <- file.path(legacy_dir(m), "app.R")
  if (!file.exists(f) || !file.exists(legacy)) next
  lost <- setdiff(ids_of(legacy), c(ids_of(f), exceptions[[m$id]]))
  if (length(lost)) {
    bad <- bad + 1L
    cat(m$id, ": ", length(lost), " control(s) lost: ",
        paste(lost, collapse = ", "), "\n", sep = "")
  }
}

if (bad == 0L) cat("every module keeps its legacy app's controls\n")
quit(status = if (bad > 0L) 1L else 0L)
