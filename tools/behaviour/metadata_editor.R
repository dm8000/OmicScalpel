#!/usr/bin/env Rscript
# Behaviour test for the metadata editor's save path.
#
# It rewrites the whole spreadsheet, not the edited rows, so the failure that
# would hurt is losing the samples of every dataset the editor was not showing.
#
#   Rscript tools/behaviour/metadata_editor.R

source("global.R")

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

tmp <- file.path(tempdir(), paste0("os-me-", Sys.getpid()))
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

before  <- load_metadata()
DS      <- "DEMO_RNAseq"
OTHER   <- setdiff(unique(before$dataset), DS)[1]
n_other <- sum(before$dataset == OTHER)
old_val <- before$Tissue[before$dataset == DS][1]

testServer(
  metadataEditorServer,
  args = list(ds = reactiveVal(DS), meta = reactive(before), go_to = NULL),
  {
    session$flushReact()
    chk(!is.null(rv$data) && nrow(rv$data) == nrow(before),
        "the editor starts holding every row, not just the shown dataset",
        if (is.null(rv$data)) "NULL" else nrow(rv$data))

    # what a cell edit in the table amounts to: the value changes and the tab
    # marks itself dirty
    i <- which(rv$data$dataset == DS)[1]
    rv$data$Tissue[i] <- "EDITED_BY_TEST"
    rv$data_modified  <- TRUE
    session$flushReact()
    chk(identical(rv$data$Tissue[i], "EDITED_BY_TEST"),
        "an unsaved edit survives a reactive flush", rv$data$Tissue[i])

    session$setInputs(save = 1)
    session$flushReact()
  }
)

after <- load_metadata()
chk(nrow(after) == nrow(before), "saving keeps every row",
    nrow(after), " vs ", nrow(before))
chk(sum(after$dataset == OTHER) == n_other,
    "the other dataset's samples all survive",
    sum(after$dataset == OTHER), " vs ", n_other)
chk("EDITED_BY_TEST" %in% after$Tissue, "the edit reached the file")
chk(ncol(after) == ncol(before), "no column is dropped", ncol(after), " vs ", ncol(before))

bk <- list.files(file.path(tmp, "bk"), pattern = "^Metadata_backup_.*\\.xlsx$", full.names = TRUE)
chk(length(bk) == 1, "exactly one backup was taken", length(bk))
saved <- as.data.frame(readxl::read_excel(bk[1], .name_repair = "minimal"))
chk(identical(saved$Tissue[saved$dataset == DS][1], old_val),
    "and it holds the value from before the edit", saved$Tissue[saved$dataset == DS][1])

unlink(tmp, recursive = TRUE)
cat("\n", n, " checks passed\n", sep = "")
