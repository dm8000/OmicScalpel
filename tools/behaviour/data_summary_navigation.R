#!/usr/bin/env Rscript
# Behaviour test for the dataset-summary tab's Load button.
#
# Six buttons, one per analysis tab, became one: select a row, load that
# dataset, stay where you are. The guard about datasets with no expression
# matrix went with them -- loading a metadata-only dataset is legitimate, and
# the tab that needs a matrix is the one that should refuse.
#
#   Rscript tools/behaviour/data_summary_navigation.R

source("global.R")

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

meta_df <- load_metadata()
both    <- sort(unique(meta_df$dataset))
chk(length(both) >= 2, "the fixture has more than one dataset to choose between", length(both))

# An environment, not a plain variable: `calls <- list()` inside testServer's
# expr would create a local of that name while the spy kept writing to the one
# out here, and every check would read an empty list.
bus <- new.env(parent = emptyenv())
bus$calls <- list()
spy <- function(tab = NULL, dataset = NULL) {
  bus$calls <- c(bus$calls, list(list(tab = tab, dataset = dataset)))
}

testServer(
  dataSummaryServer,
  args = list(ds = reactiveVal(both[1]), meta = reactive(meta_df), go_to = spy),
  {
    session$setInputs(dataset_selector = both)
    session$flushReact()

    bus$calls <- list()
    session$setInputs(summary_table_rows_selected = integer(0))
    session$setInputs(load_dataset = 1)
    chk(length(bus$calls) == 0, "no row selected means nothing is loaded", length(bus$calls))

    # each displayed row loads its own dataset, discovered by pressing the
    # button rather than by reaching inside the module for a reactive's name
    click <- 1L
    seen <- character(0)
    for (i in seq_along(both)) {
      bus$calls <- list()
      session$setInputs(summary_table_rows_selected = i)
      click <- click + 1L
      session$setInputs(load_dataset = click)
      seen[i] <- if (length(bus$calls)) bus$calls[[1]]$dataset else NA_character_
    }
    chk(setequal(stats::na.omit(seen), both),
        "each row loads the dataset on that row", paste(seen, collapse = ", "))

    # it loads, it does not navigate
    bus$calls <- list()
    session$setInputs(summary_table_rows_selected = 1L)
    click <- click + 1L
    session$setInputs(load_dataset = click)
    chk(length(bus$calls) == 1 && is.null(bus$calls[[1]]$tab),
        "loading does not switch tab",
        if (!length(bus$calls)) "no call" else paste("tab =", bus$calls[[1]]$tab))
  }
)

cat("\n", n, " checks passed\n", sep = "")
