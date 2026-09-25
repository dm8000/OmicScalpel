# tools/lintlib.R -- what more than one linter needs.
#
# ids_of() was written for lint_parity.R and is now also what proves the
# machine interface in R/ai_tools.R names controls that really exist. Copying
# it would mean two parsers drifting apart, which is the defect the linters
# exist to catch.

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
