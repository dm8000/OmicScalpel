#!/usr/bin/env Rscript
# Behaviour test for the cutoffs tab.
#
# This tab writes a new column into the shared metadata, so the failure that
# matters is not "the plot looks wrong" -- it is "it classified the wrong rows".
# The check that earns its place is the last one: the samples of the *other*
# dataset must be untouched.
#
#   Rscript tools/behaviour/cutoff_maker.R

source("global.R")

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

# its own hub: this tab writes
tmp <- file.path(tempdir(), paste0("os-cut-", Sys.getpid()))
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

DS   <- "DEMO_RNAseq"
COL  <- "Age"
CUT  <- 50
meta_df <- load_metadata()
ages <- suppressWarnings(as.numeric(meta_df[[COL]][meta_df$dataset == DS]))
chk(sum(!is.na(ages)) > 5, "the fixture has ages to cut on", sum(!is.na(ages)))
chk(any(ages < CUT) && any(ages >= CUT), "and they fall on both sides of the cutoff",
    sum(ages < CUT), " below, ", sum(ages >= CUT), " at or above")

testServer(
  cutoffMakerServer,
  args = list(ds = reactiveVal(DS), meta = reactive(meta_df), go_to = NULL),
  {
    session$flushReact()
    session$setInputs(numeric_col = COL, cutoff_1 = CUT)
    session$setInputs(make_cutoff = 1)
    session$flushReact()

    new_col <- paste0(COL, ".", gsub("\\.", "_", format(round(CUT, 2), nsmall = 2)))
    d <- rv$data
    chk(new_col %in% names(d), "the cutoff column is created with the expected name",
        new_col, "; got: ", paste(setdiff(names(d), names(meta_df)), collapse = ", "))

    mine   <- d$dataset == DS
    theirs <- d$dataset != DS

    expected <- ifelse(suppressWarnings(as.numeric(d[[COL]][mine])) < CUT,
                       paste0("< ", CUT), paste0("≥ ", CUT))
    chk(identical(as.character(d[[new_col]][mine]), expected),
        "every sample of the active dataset lands on the right side",
        paste(head(d[[new_col]][mine], 3), collapse = " | "))

    chk(all(is.na(d[[new_col]][theirs])),
        "no sample of any other dataset is touched",
        paste(unique(d[[new_col]][theirs]), collapse = ", "))
  }
)

unlink(tmp, recursive = TRUE)
cat("\n", n, " checks passed\n", sep = "")
