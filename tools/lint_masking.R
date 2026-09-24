#!/usr/bin/env Rscript
# lint_masking.R -- catch a dplyr argument that compares a name with itself.
#
#   Rscript tools/lint_masking.R FILE.R [FILE.R ...]
#
# Why this exists: converting an app into a module renames things, and one
# rename produced
#
#     get_numeric_columns <- function(dataset) meta() %>% filter(dataset == dataset)
#
# Under dplyr's data masking the left-hand `dataset` is the column, so the test
# is column-against-itself, every row survives, and the tab offers columns
# belonging to datasets nobody asked about. It parses, it runs, it plots, and
# the answer is wrong. Base R subsetting -- metadata[metadata$dataset == dataset, ]
# -- is fine and is not reported, because `[` does not mask.

MASKING <- c("filter", "mutate", "summarise", "summarize", "transmute",
             "arrange", "group_by", "count", "slice_max", "slice_min", "case_when")

USAGE <- "Usage: Rscript tools/lint_masking.R FILE.R [FILE.R ...]"

self_compare <- function(x) {
  is.call(x) && length(x) == 3L && is.symbol(x[[1]]) &&
    as.character(x[[1]]) == "==" &&
    is.symbol(x[[2]]) && is.symbol(x[[3]]) &&
    identical(as.character(x[[2]]), as.character(x[[3]]))
}

lint_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  exprs <- tryCatch(parse(path, keep.source = TRUE), error = function(e) e)
  if (inherits(exprs, "error")) {
    cat("cannot parse ", path, "\n", sep = ""); quit(status = 2)
  }
  found <- character(0)

  walk <- function(x, masked) {
    if (!is.call(x)) return(invisible(NULL))
    fn <- x[[1]]
    # dplyr::filter(...) is a call to `::`, not a symbol; take the name from it
    # or the verb goes unnoticed, which is how the first draft of this file
    # reported nothing at all.
    if (is.call(fn) && is.symbol(fn[[1]]) &&
        as.character(fn[[1]]) %in% c("::", ":::")) fn <- fn[[3]]
    fnm <- if (is.symbol(fn)) as.character(fn) else ""
    here <- masked || fnm %in% MASKING
    if (here && self_compare(x)) {
      found[[length(found) + 1L]] <<- as.character(x[[2]])
    }
    for (i in seq_along(x)) walk(x[[i]], here)
    invisible(NULL)
  }
  for (e in exprs) walk(e, FALSE)

  if (!length(found)) return(0L)
  for (nm in unique(found)) {
    k <- which(grepl(paste0(nm, " == ", nm), lines, fixed = TRUE))
    cat(path, ":", if (length(k)) k[1] else 0L, ": ",
        sprintf("`%s == %s` inside a dplyr verb compares the column with itself", nm, nm),
        "\n", sep = "")
  }
  length(unique(found))
}

args <- commandArgs(TRUE)
if (!length(args) || "--help" %in% args) { cat(USAGE, "\n"); quit(status = if (length(args)) 0 else 2) }
bad <- sum(vapply(args, lint_file, integer(1)))
quit(status = if (bad > 0L) 1L else 0L)
