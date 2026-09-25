#!/usr/bin/env Rscript
# Does each module actually run? Usage, from the project root:
#
#   Rscript tools/smoke_test.R [module_id ...]     (default: every module built)
#
# lint_ns.R proves the ids are namespaced by reading the source. This proves the
# module starts: its UI renders, its server instantiates against the fixtures,
# and it survives being told which dataset is active.

source("global.R")

pick <- commandArgs(TRUE)
built <- Filter(function(m) file.exists(module_file(m)), MODULES)
if (length(pick)) {
  unknown <- setdiff(pick, names(MODULES))
  if (length(unknown)) { cat("unknown module: ", paste(unknown, collapse = ", "), "\n", sep = ""); quit(status = 2) }
  built <- MODULES[pick]
  absent <- Filter(function(m) !file.exists(module_file(m)), built)
  if (length(absent)) { cat("not built yet: ", paste(names(absent), collapse = ", "), "\n", sep = ""); quit(status = 2) }
}
if (!length(built)) { cat("no module built yet\n"); quit(status = 2) }

meta_df  <- load_metadata()
datasets <- unique(meta_df$dataset)
if (length(datasets) < 2) {
  cat("the fixture must hold at least 2 datasets; found ", length(datasets), "\n", sep = "")
  quit(status = 2)
}

bad <- 0L
for (m in built) {
  cat(m$id, ": ", sep = "")
  problems <- character()

  for (fn in c(m$ui, m$server)) {
    if (!exists(fn, mode = "function")) problems <- c(problems, paste0("missing ", fn, "()"))
  }

  if (!length(problems)) {
    html <- tryCatch(as.character(get(m$ui)("t")), error = function(e) e)
    if (inherits(html, "error")) {
      problems <- c(problems, paste0("UI failed: ", conditionMessage(html)))
    } else if (!any(grepl('"t-', html, fixed = TRUE))) {
      problems <- c(problems, "UI rendered no namespaced id (expected \"t-...\")")
    }
  }

  # Instantiate against more than one dataset: a module that silently works for
  # one dataset only would otherwise pass. Three is enough to show that, and
  # against the real hub "all of them" means 17 datasets times nine modules,
  # several of which load a 54,000 x 869 matrix.
  probe <- if (length(datasets) <= 3) datasets else datasets[c(1, 2, length(datasets))]
  if (!length(problems)) {
    for (d in probe) {
      e <- tryCatch({
        shiny::testServer(
          get(m$server),
          args = list(ds   = shiny::reactiveVal(d),
                      meta = shiny::reactive(meta_df),
                      go_to = function(tab, dataset) invisible(NULL)),
          expr = { session$flushReact() }
        )
        NULL
      }, error = function(e) e)
      if (!is.null(e)) {
        problems <- c(problems, paste0("server failed on ", d, ": ", conditionMessage(e)))
        break
      }
    }
  }

  if (length(problems)) {
    bad <- bad + 1L
    cat("FAIL\n")
    for (p in problems) cat("    ", p, "\n", sep = "")
  } else {
    cat("ok\n")
  }
}

cat("\n", length(built) - bad, "/", length(built), " modules ok\n", sep = "")
quit(status = if (bad > 0L) 1L else 0L)
