#!/usr/bin/env Rscript
# Behaviour test for the dataset-summary tab's navigation buttons.
#
# Written before the buttons exist, so they cannot be built to fit a weaker
# test. It checks what the researcher actually needs: pick a row, press a
# button, land on that tab with that dataset already active.
#
#   Rscript tools/behaviour/data_summary_navigation.R

source("global.R")

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

# A hub with a third dataset that has metadata but no expression file. Without
# it the guard "this tab needs a matrix" has nothing to refuse and would pass
# untested.
tmp <- file.path(tempdir(), paste0("os-nav-", Sys.getpid()))
dir.create(file.path(tmp, "hub"), recursive = TRUE)
invisible(file.copy(list.files("data-sample", full.names = TRUE),
                    file.path(tmp, "hub"), recursive = TRUE))
writeLines(c(paste0("lib     = ", .libPaths()[1]),
             paste0("hubdata = ", file.path(tmp, "hub")),
             paste0("backups = ", file.path(tmp, "bk")),
             paste0("logs    = ", file.path(tmp, "logs"))),
           file.path(tmp, "config.txt"))
Sys.setenv(OMICSCALPEL_CONFIG = file.path(tmp, "config.txt"))
invisible(read_config(reload = TRUE))

m <- load_metadata()
ghost <- m[1, , drop = FALSE]
ghost$dataset  <- "DEMO_NoMatrix"
ghost$SampleID <- "DEMO_NoMatrix_S01"
save_metadata(NULL, rbind(m, ghost))

s <- load_datasets_summary()
gs <- s[1, , drop = FALSE]; gs$dataset <- "DEMO_NoMatrix"
save_datasets_summary(NULL, rbind(s, gs))

meta_df <- load_metadata()
chk(length(list_units("DEMO_NoMatrix")) == 0, "the fixture ghost has no expression file")
chk(length(list_units("DEMO_RNAseq")) > 0,   "and a real one still has")

calls <- list()
spy <- function(tab, dataset) calls[[length(calls) + 1L]] <<- list(tab = tab, dataset = dataset)

testServer(
  dataSummaryServer,
  args = list(ds = reactiveVal("DEMO_RNAseq"), meta = reactive(meta_df), go_to = spy),
  {
    click <- 0L
    session$setInputs(dataset_selector = c("DEMO_RNAseq", "DEMO_Array", "DEMO_NoMatrix"))
    session$flushReact()

    # nothing selected: pressing a button must not navigate
    session$setInputs(summary_table_rows_selected = integer(0))
    session$setInputs(goto_compare_genes = 1)
    chk(length(calls) == 0, "no row selected means no navigation", length(calls))

    # Which displayed row holds which dataset, discovered by pressing a button
    # that is always allowed. The test never reaches inside the module for a
    # reactive's name: it only uses what a user can do.
    row_of <- character(0)
    for (i in 1:3) {
      calls <- list()
      session$setInputs(summary_table_rows_selected = i)
      click <- click + 1L
      do.call(session$setInputs, setNames(list(click), "goto_metadata_editor"))
      row_of[i] <- if (length(calls)) calls[[1]]$dataset else NA_character_
    }
    chk(setequal(stats::na.omit(row_of),
                 c("DEMO_RNAseq", "DEMO_Array", "DEMO_NoMatrix")),
        "each displayed row navigates with its own dataset",
        paste(row_of, collapse = ", "))

    row_rna   <- which(row_of == "DEMO_RNAseq")[1]
    row_ghost <- which(row_of == "DEMO_NoMatrix")[1]

    # a real dataset: every button navigates, and carries the row's dataset
    for (tab in c("compare_genes", "compare_samples", "correlation_analysis",
                  "export_matrix", "metadata_editor", "cutoff_maker")) {
      calls <- list()
      session$setInputs(summary_table_rows_selected = row_rna)
      do.call(session$setInputs, setNames(list(click <- click + 1L), paste0("goto_", tab)))
      chk(length(calls) == 1 && calls[[1]]$tab == tab &&
            calls[[1]]$dataset == "DEMO_RNAseq",
          paste0("goto_", tab, " navigates with the selected dataset"),
          if (!length(calls)) "no call" else paste(calls[[1]]$tab, calls[[1]]$dataset))
    }

    # a dataset with no matrix: the four analysis tabs refuse, the two
    # metadata-only tabs still work
    for (tab in c("compare_genes", "compare_samples", "correlation_analysis", "export_matrix")) {
      calls <- list()
      session$setInputs(summary_table_rows_selected = row_ghost)
      do.call(session$setInputs, setNames(list(click <- click + 1L), paste0("goto_", tab)))
      chk(length(calls) == 0, paste0("goto_", tab, " refuses a dataset with no matrix"),
          length(calls))
    }
    for (tab in c("metadata_editor", "cutoff_maker")) {
      calls <- list()
      session$setInputs(summary_table_rows_selected = row_ghost)
      do.call(session$setInputs, setNames(list(click <- click + 1L), paste0("goto_", tab)))
      chk(length(calls) == 1 && calls[[1]]$dataset == "DEMO_NoMatrix",
          paste0("goto_", tab, " works without a matrix"), length(calls))
    }
  }
)

unlink(tmp, recursive = TRUE)
cat("\n", n, " checks passed\n", sep = "")
