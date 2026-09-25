#!/usr/bin/env Rscript
# Behaviour test for app.R itself: the shell that owns the shared state.
#
# Every bug this project has had so far lived here, not in a module, and none
# of them were visible to parse() or to the smoke test.
#
#   Rscript tools/behaviour/app_shell.R

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

# a writable hub, so the "the active dataset vanished" case can be produced
tmp <- file.path(tempdir(), paste0("os-app-", Sys.getpid()))
dir.create(file.path(tmp, "hub"), recursive = TRUE)
invisible(file.copy(list.files("data-sample", full.names = TRUE),
                    file.path(tmp, "hub"), recursive = TRUE))
writeLines(c(paste0("lib     = ", .libPaths()[1]),
             paste0("hubdata = ", file.path(tmp, "hub")),
             paste0("backups = ", file.path(tmp, "bk")),
             paste0("logs    = ", file.path(tmp, "logs"))),
           file.path(tmp, "config.txt"))
Sys.setenv(OMICSCALPEL_CONFIG = file.path(tmp, "config.txt"))
# app.R sources global.R itself; the config must already point at the temp hub
# when it does, because the UI is built at source time.
source("app.R")
invisible(read_config(reload = TRUE))

# The UI has to render at all. The tab-contents bug served 200 with nine empty
# tabs, so "it starts" is not the check -- "the modules are in the page" is.
html <- as.character(ui)
chk(grepl('id="active_dataset"', html, fixed = TRUE), "the sidebar has the dataset selector")
built <- Filter(function(m) exists(m$ui, mode = "function"), MODULES)
for (m in built) {
  chk(grepl(paste0('"', m$id, '-'), html, fixed = TRUE),
      paste0(m$id, "'s namespaced ids reach the page"))
}

testServer(app = ".", expr = {
  session$flushReact()

  chk(!is.null(active_dataset()), "an active dataset is chosen at startup",
      active_dataset())
  chk(active_dataset() %in% unique(shared_meta()$dataset),
      "and it is one that exists", active_dataset())

  first <- active_dataset()
  other <- setdiff(sort(unique(shared_meta()$dataset)), first)[1]

  session$setInputs(active_dataset = other)
  chk(identical(active_dataset(), other), "the sidebar selection becomes the active dataset",
      active_dataset())

  go_to("export_matrix", first)
  chk(identical(active_dataset(), first),
      "go_to() switches the dataset as well as the tab", active_dataset())

  # The sidebar's re-read button must actually re-read, for the case where the
  # spreadsheet was changed outside the app and nothing here could notice.
  before <- metadata_version()
  session$setInputs(reload_metadata = 1)
  chk(metadata_version() > before, "the re-read button bumps the metadata version",
      before, " -> ", metadata_version())

  # The active dataset disappears from the file. It must fall back, not sit
  # pointing at a dataset that is gone.
  m <- load_metadata()
  save_metadata(NULL, m[m$dataset != first, , drop = FALSE])
  session$flushReact()
  chk(!identical(active_dataset(), first),
      "a vanished dataset is not kept as active", active_dataset())
  chk(active_dataset() %in% unique(shared_meta()$dataset),
      "the fallback is a dataset that exists", active_dataset())
})

# Collapsible panels are driven by a hidden checkbox and a <label for=...>,
# which acts on the first element with that id. Deriving the id from the panel
# title gave 76 panels 22 ids -- "Labels" exists in three modules -- so folding
# one tab's panel folded a hidden one in another and the click appeared to do
# nothing.
cids <- regmatches(html, gregexpr('id="os-c-[^"]+"', html))[[1]]
chk(length(cids) == length(unique(cids)),
    "every collapsible panel has its own id",
    length(cids) - length(unique(cids)), " duplicated")
chk(length(cids) > 20, "and there are collapsible panels to check", length(cids))

# Last, because it only holds once every module has stopped building its own
# page: a leftover tab shows up here as an extra pane. Counts tabsetPanel's
# markup -- shinydashboard's id="shiny-tab-" is gone with dashboardPage.
panes <- length(gregexpr('class="tab-pane', html, fixed = TRUE)[[1]])
chk(panes == length(MODULES),
    "one tab pane per registered module, no module building its own",
    panes, " panes for ", length(MODULES), " modules")

unlink(tmp, recursive = TRUE)
cat("\n", n, " checks passed\n", sep = "")
