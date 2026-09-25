#!/usr/bin/env Rscript
# Round every expression matrix in the hub to the house rule.
#
#   Rscript tools/round-matrices.R [--dry] [--keep-backup]
#
# The matrices carry up to 18 decimal places, which is float noise written as
# text, not measurement. os_round_expression() is the rule and lives in
# R/data_io.R, so this script and the upload path cannot drift apart.
#
# Order: copy, rewrite, verify, report, and only then delete the copy. The
# disk is nearly full, which is why the copy does not stay -- but it does not
# go until every file has been checked against the original.

source("global.R")

args        <- commandArgs(TRUE)
dry         <- "--dry" %in% args
keep_backup <- "--keep-backup" %in% args

CHUNK <- 2000L

# The rewrite itself is os_write_rounded_matrix() in R/data_io.R, the same
# function the upload tab calls. One rule, one implementation, two callers.
round_file <- function(inp, out) {
  res <- os_write_rounded_matrix(inp, out)
  if (!isTRUE(res$rounded)) stop(res$why, call. = FALSE)
  res$rows
}

# Symbols and shape from both files, and the values on a sample of lines. The
# sample is what makes this affordable on a 402 MB file; the symbols and the
# shape are compared in full, because a row lost or reordered is the failure
# that would quietly ruin every analysis afterwards.
verify_file <- function(orig, new, sample_n = 400L) {
  syms <- function(p) system2("cut", c("-f1", shQuote(p)), stdout = TRUE)
  a <- syms(orig); b <- syms(new)
  if (!identical(length(a), length(b))) return(sprintf("rows %d -> %d", length(a), length(b)))
  if (!identical(a, b)) return("the symbols changed")

  nf <- function(p) length(strsplit(readLines(p, n = 1L, warn = FALSE), "\t", fixed = TRUE)[[1]])
  if (!identical(nf(orig), nf(new))) return("the number of columns changed")

  idx <- unique(round(seq(2, length(a), length.out = min(sample_n, length(a) - 1))))
  oa <- readLines(orig); ob <- readLines(new)
  for (i in idx) {
    x <- suppressWarnings(as.numeric(strsplit(oa[i], "\t", fixed = TRUE)[[1]][-1]))
    y <- suppressWarnings(as.numeric(strsplit(ob[i], "\t", fixed = TRUE)[[1]][-1]))
    if (length(x) != length(y)) return(sprintf("line %d has %d values, not %d", i, length(y), length(x)))
    want <- os_round_expression(x)
    bad <- which(!is.na(want) & !is.na(y) & abs(want - y) > 1e-9)
    if (length(bad)) {
      return(sprintf("line %d value %d: %s should be %s, is %s", i, bad[1],
                     format(x[bad[1]]), format(want[bad[1]]), format(y[bad[1]])))
    }
  }
  NA_character_
}

# --- what to touch ------------------------------------------------------------

md <- load_metadata()
jobs <- list()
for (d in sort(unique(md$dataset))) {
  for (u in tryCatch(list_units(d), error = function(e) character(0))) {
    p <- tryCatch(expression_path(d, u), error = function(e) NA_character_)
    if (!is.na(p) && file.exists(p)) {
      jobs[[length(jobs) + 1L]] <- list(dataset = d, unit = u, path = p,
                                        size = file.size(p))
    }
  }
}
jobs <- jobs[order(-vapply(jobs, function(j) j$size, numeric(1)))]

cat(length(jobs), " matrices, ",
    format(round(sum(vapply(jobs, function(j) j$size, numeric(1))) / 1e6), big.mark = ","),
    " MB\n\n", sep = "")

if (dry) {
  for (j in jobs) cat(sprintf("  %-34s %-14s %6.0f MB\n", j$dataset, j$unit, j$size / 1e6))
  quit(status = 0)
}

stamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup <- file.path(os_path("backups"), paste0("matrices_", stamp))
dir.create(backup, recursive = TRUE, showWarnings = FALSE)
cat("backup: ", backup, "\n\n", sep = "")

fails <- character(0)
for (j in jobs) {
  t0 <- Sys.time()
  bdir <- file.path(backup, j$dataset)
  dir.create(bdir, recursive = TRUE, showWarnings = FALSE)
  bcopy <- file.path(bdir, basename(j$path))
  if (!file.copy(j$path, bcopy, overwrite = TRUE)) {
    fails <- c(fails, paste(j$dataset, j$unit, "could not be backed up"))
    cat(sprintf("  %-34s %-12s BACKUP FAILED\n", j$dataset, j$unit)); next
  }

  tmp <- paste0(j$path, ".rounding")
  nrows <- tryCatch(round_file(bcopy, tmp), error = function(e) conditionMessage(e))
  if (is.character(nrows)) {
    unlink(tmp); fails <- c(fails, paste(j$dataset, j$unit, nrows))
    cat(sprintf("  %-34s %-12s REWRITE FAILED: %s\n", j$dataset, j$unit, nrows)); next
  }

  problem <- verify_file(bcopy, tmp)
  if (!is.na(problem)) {
    unlink(tmp); fails <- c(fails, paste(j$dataset, j$unit, problem))
    cat(sprintf("  %-34s %-12s VERIFY FAILED: %s\n", j$dataset, j$unit, problem)); next
  }

  before <- file.size(tmp)
  file.rename(tmp, j$path)
  cat(sprintf("  %-34s %-12s %6.0f -> %4.0f MB  %5.0f rows  %4.0f s\n",
              j$dataset, j$unit, j$size / 1e6, before / 1e6, nrows,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

cat("\n")
if (length(fails)) {
  cat("FAILED on ", length(fails), " file(s); the backup is KEPT at\n  ", backup, "\n", sep = "")
  for (f in fails) cat("  - ", f, "\n", sep = "")
  quit(status = 1)
}

if (keep_backup) {
  cat("every file verified. backup kept at ", backup, "\n", sep = "")
} else {
  unlink(backup, recursive = TRUE)
  cat("every file verified, backup deleted (disk was the reason).\n")
}
cat("Run tools/build-gene-index.R next if any symbol changed -- none should have.\n")
