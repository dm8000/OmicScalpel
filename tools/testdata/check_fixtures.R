#!/usr/bin/env Rscript
# Prove data-sample/ carries no real identifier. Run after make-fixtures.R and
# before committing a regenerated fixture.
#
# The failure this guards against is silent: a fixture that still holds a real
# sample id or study name looks fine and gets committed to a repository.

suppressMessages(library(readxl))
args <- commandArgs(FALSE)
here <- dirname(sub("^--file=", "", grep("^--file=", args, value = TRUE)[1]))
source(file.path(here, "..", "..", "R", "config.R"))

root <- os_root()
fix  <- file.path(root, "data-sample")
fail <- function(...) { cat("FAIL: ", ..., "\n", sep = ""); quit(status = 1) }

# The real spreadsheets live in the hub, not in the project root. This check
# can only run where the real data is: it proves the fixture contains none of
# it, which is meaningless if there is nothing to compare against -- and
# comparing data-sample with itself would report every one of its own names as
# a leak.
real_path <- os_path("hubdata", "Metadata.xlsx")
if (normalizePath(dirname(real_path), mustWork = FALSE) ==
    normalizePath(fix, mustWork = FALSE) || !file.exists(real_path)) {
  cat("skipped: no real hub on this machine to compare against (", real_path, ")\n", sep = "")
  quit(status = 0)
}
real_meta <- as.data.frame(read_excel(real_path, .name_repair = "minimal"))
fx_meta   <- as.data.frame(read_excel(file.path(fix,  "Metadata.xlsx"), .name_repair = "minimal"))
fx_summ   <- as.data.frame(read_excel(file.path(fix,  "Datasets_summary.xlsx"), .name_repair = "minimal"))

# every cell of the fixtures, plus every expression file, as one character pile
pile <- c(unlist(lapply(fx_meta, as.character)),
          unlist(lapply(fx_summ, as.character)),
          names(fx_meta), names(fx_summ))
# Only the header (sample names) and the Symbol column. The numeric matrix is
# excluded on purpose: an expression count of 2371 is not a leaked TsengID of
# 2371, and comparing it as one makes this check cry wolf.
for (f in list.files(fix, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)) {
  txt <- read.delim(f, check.names = FALSE, colClasses = "character")
  pile <- c(pile, names(txt), txt[[1]])
}
pile <- unique(pile[!is.na(pile)])

leak <- intersect(pile, unique(na.omit(real_meta$dataset)))
if (length(leak)) fail("real dataset name in fixture: ", paste(leak, collapse = ", "))

leak <- intersect(pile, unique(na.omit(real_meta$SampleID)))
if (length(leak)) fail(length(leak), " real SampleID(s) in fixture, e.g. ", leak[1])

leak <- intersect(pile, unique(na.omit(as.character(real_meta$TsengID))))
if (length(leak)) fail(length(leak), " real TsengID(s) in fixture, e.g. ", leak[1])

for (col in c("Author", "publication")) {
  if (col %in% names(real_meta)) {
    leak <- intersect(pile, unique(na.omit(as.character(real_meta[[col]]))))
    if (length(leak)) fail("real ", col, " value in fixture: ", leak[1])
  }
}

# shape the apps depend on
if (ncol(fx_meta) != ncol(real_meta)) fail("fixture has ", ncol(fx_meta), " metadata columns, real file has ", ncol(real_meta))
if (length(unique(fx_meta$dataset)) != 2) fail("fixture must hold exactly 2 datasets")
if (length(unique(fx_meta$`Data.type`)) < 2) fail("the 2 fixture datasets must differ in Data.type")
if (anyDuplicated(fx_meta$SampleID)) fail("duplicate SampleID in fixture")

# The cutoff and correlation tabs need a continuous variable. A fixture without
# one lets them pass every test by never having anything to do.
numeric_cols <- names(fx_meta)[vapply(fx_meta, function(c)
  sum(!is.na(suppressWarnings(as.numeric(as.character(c))))) >= nrow(fx_meta),
  logical(1))]
if (length(numeric_cols) < 2) {
  fail("fixture has ", length(numeric_cols),
       " fully numeric column(s); the cutoff and correlation tabs need at least 2")
}

# the two datasets must expose different unit sets, or a helper that ignores
# its argument would pass unnoticed
u <- lapply(unique(fx_meta$dataset), function(d)
  sort(sub(paste0("^", d, "_(.*)\\.txt$"), "\\1", list.files(file.path(fix, d), pattern = "\\.txt$"))))
if (length(u) != 2 || identical(u[[1]], u[[2]])) fail("both fixture datasets expose the same units: ", paste(unlist(u), collapse = ", "))

cat("data-sample OK: ", nrow(fx_meta), " samples, ",
    length(unique(fx_meta$dataset)), " datasets (",
    paste(unique(fx_meta$dataset), collapse = ", "), "), types ",
    paste(unique(fx_meta$`Data.type`), collapse = "/"), ", units ",
    paste(sapply(u, paste, collapse = "+"), collapse = " vs "), "\n", sep = "")
