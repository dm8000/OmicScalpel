#!/usr/bin/env Rscript
# lint_ns.R -- check that a Shiny module namespaces every input/output id.
#
# Converting nine standalone apps into modules is a mechanical edit over ~4700
# lines, and its failure mode is silent: an id left un-namespaced still parses,
# still renders, and simply stops talking to its server. This is the only thing
# standing between that and a green test suite, so it is deliberately strict.
#
# Usage: Rscript lint_ns.R [--single-dataset] FILE.R [FILE.R ...]

USAGE <- c(
  "Usage: Rscript lint_ns.R [--single-dataset] FILE.R [FILE.R ...]",
  "",
  "  --single-dataset  also reject input$dataset: a module that takes the",
  "                    shared dataset must read ds(), not its own input.",
  "",
  "Exit 0 = clean (and silent). 1 = problems found. 2 = usage or parse error."
)

# --- what counts as an id-bearing call ------------------------------------
# Surveyed across the nine apps rather than guessed. update*() is excluded on
# purpose: inside moduleServer it takes a bare id and the session namespaces it,
# so flagging it would be a false positive on correct code.
ID_EXTRA   <- c("actionButton", "actionLink", "downloadButton",
                "downloadLink", "radioButtons")

# A module fills a tab; it does not build a page. A tabItem() left inside a
# module nests a tab-pane in a tab-pane: the page still renders, still returns
# 200, and the tab is simply always blank. Neither parse nor a smoke test sees
# it, which is why it is checked here.
PAGE_SHELL <- c("dashboardPage", "dashboardHeader", "dashboardSidebar",
                "dashboardBody", "tabItems", "tabItem", "navbarPage",
                "shinyApp", "runApp")
NAMED_ONLY <- c(add_rank_list = "input_id")   # first positional arg is a label

is_id_call <- function(fn) {
  if (!is.symbol(fn)) return(FALSE)
  n <- as.character(fn)
  if (startsWith(n, "update")) return(FALSE)
  n %in% names(NAMED_ONLY) || grepl("(Input|Output)$", n) || n %in% ID_EXTRA
}

arg_names <- function(cl) {
  a <- as.list(cl)[-1]
  n <- names(a)
  if (is.null(n)) rep("", length(a)) else n
}

id_arg <- function(cl, fname) {
  a <- as.list(cl)[-1]
  nms <- arg_names(cl)
  if (fname %in% names(NAMED_ONLY)) {          # named form only, never positional
    k <- which(nms == NAMED_ONLY[[fname]])
    return(if (length(k)) a[[k[1]]] else NULL)
  }
  for (want in c("inputId", "outputId")) {
    k <- which(nms == want)
    if (length(k)) return(a[[k[1]]])
  }
  k <- which(nms == "")
  if (length(k)) a[[k[1]]] else NULL
}

# ns("x") or session$ns("x")
is_namespaced <- function(x) {
  if (!is.call(x)) return(FALSE)
  f <- x[[1]]
  if (is.symbol(f) && as.character(f) == "ns") return(TRUE)
  is.call(f) && identical(f[[1]], as.name("$")) &&
    is.symbol(f[[3]]) && as.character(f[[3]]) == "ns"
}

# --- reporting -------------------------------------------------------------
# srcrefs only reach top-level expressions and function definitions, so the
# line is recovered by finding the offending text instead of carrying it down.
find_line <- function(lines, needle) {
  k <- which(grepl(needle, lines, fixed = TRUE))
  if (length(k)) k[1] else 0L
}

lint_file <- function(path, single_dataset) {
  lines <- readLines(path, warn = FALSE)
  exprs <- tryCatch(parse(path, keep.source = TRUE), error = function(e) e)
  if (inherits(exprs, "error")) {
    cat("cannot parse ", path, ": ", conditionMessage(exprs), "\n", sep = "")
    quit(status = 2)
  }

  found <- list()
  report <- function(needle, msg) {
    found[[length(found) + 1L]] <<- list(line = find_line(lines, needle), msg = msg)
  }

  walk <- function(x, fname) {
    if (!is.call(x)) return(invisible(NULL))
    fn  <- x[[1]]
    fnm <- if (is.symbol(fn)) as.character(fn) else ""

    # a function definition: walk the body, keeping the name it was assigned to
    if (fnm == "function") return(walk(x[[3]], fname))

    if (fnm %in% c("<-", "=", "<<-") && length(x) == 3L && is.symbol(x[[2]])) {
      rhs <- x[[3]]
      if (is.call(rhs) && is.symbol(rhs[[1]]) && as.character(rhs[[1]]) == "function") {
        return(walk(rhs, as.character(x[[2]])))
      }
    }

    # E: a module must not build its own page shell
    if (fnm %in% PAGE_SHELL) {
      report(paste0(fnm, "("),
             sprintf("%s() belongs to the app, not to a module", fnm))
    }

    # B: a conditionalPanel inside a module needs ns to read its own inputs
    if (fnm == "conditionalPanel" && !("ns" %in% arg_names(x))) {
      report("conditionalPanel", "conditionalPanel without ns = ns")
    }

    # D: the shared dataset arrives as ds(), not as an input of this module
    if (single_dataset && fnm == "$" && length(x) == 3L &&
        is.symbol(x[[2]]) && as.character(x[[2]]) == "input" &&
        is.symbol(x[[3]]) && as.character(x[[3]]) == "dataset") {
      report("input$dataset", "input$dataset must be the shared ds() reactive")
    }

    # A and C: a literal id anywhere in a module is unreachable from its server
    if (is_id_call(fn)) {
      a <- id_arg(x, fnm)
      if (!is.null(a) && is.character(a) && length(a) == 1L && !is_namespaced(a)) {
        if (endsWith(fname, "UI")) {
          report(a, sprintf('id "%s" is not wrapped in ns()', a))
        } else if (endsWith(fname, "Server")) {
          report(a, sprintf('id "%s" built in the server must use session$ns()', a))
        }
      }
    }

    for (i in seq_along(x)) walk(x[[i]], fname)
    invisible(NULL)
  }

  for (e in exprs) walk(e, "")

  if (!length(found)) return(0L)
  # Repeated occurrences of one problem all resolve to the same line, so they
  # are grouped: seven reports of input$dataset on line 167 is noise, "line 167,
  # 7 occurrences" is a fact you can act on.
  key <- vapply(found, function(f) paste(f$line, f$msg), character(1))
  uniq <- found[!duplicated(key)]
  n    <- table(key)[key[!duplicated(key)]]
  ord  <- order(vapply(uniq, function(f) f$line, integer(1)))
  for (i in ord) {
    times <- if (n[i] > 1L) sprintf(" (%d occurrences)", n[i]) else ""
    cat(path, ":", uniq[[i]]$line, ": ", uniq[[i]]$msg, times, "\n", sep = "")
  }
  length(found)
}

# --- main ------------------------------------------------------------------
main <- function() {
  args <- commandArgs(TRUE)
  if ("--help" %in% args || "-h" %in% args) {
    cat(paste(USAGE, collapse = "\n"), "\n", sep = "")
    quit(status = 0)
  }
  single <- "--single-dataset" %in% args
  files  <- args[!startsWith(args, "-")]
  if (!length(files)) {
    cat(paste(USAGE, collapse = "\n"), "\n", sep = "")
    quit(status = 2)
  }
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    cat("no such file: ", paste(missing, collapse = ", "), "\n", sep = "")
    quit(status = 2)
  }
  total <- sum(vapply(files, lint_file, integer(1), single_dataset = single))
  quit(status = if (total > 0L) 1L else 0L)
}

main()
