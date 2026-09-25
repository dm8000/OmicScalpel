#!/usr/bin/env Rscript
# Exercise R/data_io.R against a throwaway copy of data-sample/.
#
# It writes, so it never touches the real fixtures: the hub is copied into a
# temporary directory and a config pointed at it.

args <- commandArgs(FALSE)
here <- dirname(sub("^--file=", "", grep("^--file=", args, value = TRUE)[1]))
root <- normalizePath(file.path(here, ".."))

tmp <- file.path(tempdir(), paste0("os-test-", Sys.getpid()))
dir.create(file.path(tmp, "hub"), recursive = TRUE)
invisible(file.copy(list.files(file.path(root, "data-sample"), full.names = TRUE),
                    file.path(tmp, "hub"), recursive = TRUE))
writeLines(c(paste0("lib     = ", .libPaths()[1]),
             paste0("hubdata = ", file.path(tmp, "hub")),
             paste0("backups = ", file.path(tmp, "bk")),
             paste0("logs    = ", file.path(tmp, "logs"))),
           file.path(tmp, "config.txt"))
Sys.setenv(OMICSCALPEL_ROOT = root, OMICSCALPEL_CONFIG = file.path(tmp, "config.txt"))

source(file.path(root, "R", "config.R"))
source(file.path(root, "R", "data_io.R"))

n <- 0L
ok <- function(what) { n <<- n + 1L; cat("  ok  ", what, "\n", sep = "") }
no <- function(what, ...) { cat("FAIL  ", what, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(cond, what, ...) if (isTRUE(cond)) ok(what) else no(what, ...)

# --- metadata ---------------------------------------------------------------
m <- load_metadata()
# An exact column count is a tripwire for a fixture rebuilt against a different
# schema; it moves whenever the fixture legitimately gains a column, and the
# tab that needed the column is what says whether that was legitimate.
chk(nrow(m) == 31 && ncol(m) == 201, "load_metadata shape",
    nrow(m), "x", ncol(m))
chk(all(c("DEMO.Marker", "DEMO.OS.time", "DEMO.OS.event", "DEMO.Responder")
        %in% names(m)),
    "the fixture carries survival columns for the cutoff tabs")
chk(setequal(unique(m$dataset), c("DEMO_RNAseq", "DEMO_Array")), "both datasets present",
    paste(unique(m$dataset), collapse = ", "))

# the na_as_missing flag has to change the result, not just exist
raw  <- load_metadata(na_as_missing = FALSE)
conv <- load_metadata(na_as_missing = TRUE)
n_raw  <- sum(raw  == "NA", na.rm = TRUE)
n_conv <- sum(conv == "NA", na.rm = TRUE)
chk(n_raw > 0 && n_conv == 0, "na_as_missing changes the result",
    "literal \"NA\" cells: ", n_raw, " -> ", n_conv)

s <- load_datasets_summary()
chk(nrow(s) == 2 && ncol(s) == 28, "load_datasets_summary shape", nrow(s), "x", ncol(s))

# --- units ------------------------------------------------------------------
u1 <- list_units("DEMO_RNAseq"); u2 <- list_units("DEMO_Array")
chk(identical(u1, c("TPM", "count")), "list_units preference order",
    paste(u1, collapse = ", "))
chk(!identical(u1, u2), "two datasets give two different unit sets",
    paste(u1, collapse = "+"), " vs ", paste(u2, collapse = "+"))
chk(length(list_units("no_such_dataset")) == 0, "unknown dataset has no units")

# A file matching <dataset>_<unit>.txt that is not an expression matrix must
# not be offered as a unit. The real hub has GTEX_adipose_meta.txt -- sample
# metadata -- which was showing up as a normalization unit called "meta".
fake <- file.path(tmp, "hub", "DEMO_RNAseq", "DEMO_RNAseq_meta.txt")
writeLines(c("\t#CLASS:Sex\t#CLASS:Age", "S1\tfemale\t50"), fake)
chk(!("meta" %in% list_units("DEMO_RNAseq")),
    "a metadata file is not offered as a normalization unit",
    paste(list_units("DEMO_RNAseq"), collapse = ", "))
unlink(fake)

# --- expression: values, not shapes -----------------------------------------
e <- load_expression("DEMO_RNAseq")                       # no unit -> first preferred
direct <- read.delim(file.path(tmp, "hub", "DEMO_RNAseq", "DEMO_RNAseq_TPM.txt"),
                     check.names = FALSE)
chk(identical(e, direct), "unit-less load reads the preferred file unchanged")
# derived from the metadata, not hardcoded: the fixture's sample count depends
# on which real dataset was closest in size when it was generated
n_samples <- sum(m$dataset == "DEMO_RNAseq")
chk(ncol(e) == n_samples + 1L, "one column per sample plus the feature column",
    ncol(e), " columns for ", n_samples, " samples")
chk(identical(names(e)[-1], m$SampleID[m$dataset == "DEMO_RNAseq"]),
    "columns are exactly this dataset's samples, in order")
# 200 sampled symbols plus the housekeeping genes the Across datasets tab
# needs a fixture to contain.
chk(nrow(e) == 205, "expression row count", nrow(e))
chk(identical(as.numeric(e[[2]][1:5]), as.numeric(direct[[2]][1:5])),
    "first five values match the file on disk")

# the unit argument must select a different file, not be decorative
tpm <- load_expression("DEMO_RNAseq", "TPM")
cnt <- load_expression("DEMO_RNAseq", "count")
chk(!identical(tpm[[2]], cnt[[2]]), "unit argument selects a different matrix")
chk(grepl("_count\\.txt$", expression_path("DEMO_RNAseq", "count")) &&
    grepl("_TPM\\.txt$",   expression_path("DEMO_RNAseq", "TPM")),
    "expression_path follows its unit argument")

err <- tryCatch({ load_expression("DEMO_RNAseq", "FPKM"); "" },
                error = function(e) conditionMessage(e))
chk(grepl("TPM", err) && grepl("count", err), "missing unit lists what does exist", err)

err <- tryCatch({ load_expression("no_such_dataset"); "" },
                error = function(e) conditionMessage(e))
chk(grepl("no_such_dataset", err), "missing dataset is named in the error", err)

# --- writing ----------------------------------------------------------------
before <- load_metadata()
edited <- before; edited$Tissue[1] <- "CHANGED_BY_TEST"
bk <- save_metadata(before, edited)

chk(file.exists(bk) && dirname(bk) == file.path(tmp, "bk"),
    "backup lands in backups/, not in hubdata/", bk)
chk(load_metadata()$Tissue[1] == "CHANGED_BY_TEST", "edit reached the file")
chk(as.data.frame(readxl::read_excel(bk, .name_repair = "minimal"))$Tissue[1] ==
      before$Tissue[1], "backup holds the pre-edit value")

# NA must come back as the literal the rest of the app filters on
edited2 <- load_metadata(); edited2$Tissue[2] <- NA
save_metadata(NULL, edited2)
chk(identical(load_metadata()$Tissue[2], "NA"), "NA is written back as the literal \"NA\"",
    load_metadata()$Tissue[2])

# --- change notification ----------------------------------------------------
# A writer that does not bump the version leaves every other tab showing a file
# that is no longer on disk.
v0 <- metadata_version()
save_datasets_summary(load_datasets_summary(), load_datasets_summary())
chk(metadata_version() == v0 + 1L, "save bumps the metadata version",
    v0, " -> ", metadata_version())

# --- log --------------------------------------------------------------------
log_download(Dataset = "DEMO_RNAseq", Unit = "TPM", Genes = c("A", "B"))
lg <- file.path(tmp, "logs", "downloads.log")
chk(file.exists(lg), "log file created")
line <- readLines(lg)
chk(grepl("DEMO_RNAseq", line[1]) && grepl("A, B", line[1]), "log line carries the fields",
    line[1])

unlink(tmp, recursive = TRUE)
cat("\n", n, " checks passed\n", sep = "")
