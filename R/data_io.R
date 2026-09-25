# R/data_io.R -- the one place that reads and writes the hub.
#
# It replaces three divergent copies of load_data/save_data. They disagreed in
# ways worth recording, because each disagreement was a decision someone has to
# make once instead of three times:
#
#  - metadata-editor wrote its backup into hubdata/ itself, next to the file it
#    was backing up; cutoff-maker and upload-dataset wrote into backups/.
#    Everything backs up into backups/ now.
#  - cutoff-maker turned the literal string "NA" into a real NA on load and back
#    on save; the other two left it alone. The file's own convention is the
#    literal string -- export-matrix filters on `field != "NA"` -- so that is
#    what is written back, and reading it as missing is opt-in.
#  - five apps looked for _TMM/_CPM/_TPM/_FPKM/_count/_unknown_unit, data-summary
#    for _TMM/_TPM/_counts/_combat, export-matrix for any <ds>_*.txt. The union
#    is below, and anything else on disk is still reported, just last.
#
# Requires: readxl, writexl. Paths come from R/config.R.

METADATA_FILE <- "Metadata.xlsx"
SUMMARY_FILE  <- "Datasets_summary.xlsx"

UNIT_PREFERENCE <- c("TMM", "CPM", "TPM", "FPKM", "count", "counts",
                     "combat", "unknown_unit")

# Suffixes that match <dataset>_<unit>.txt but are not expression matrices.
# GTEX_adipose_meta.txt is sample metadata -- 870 rows of #CLASS: columns --
# and was being offered as a normalization unit called "meta". Choosing it
# would have loaded metadata as if it were expression.
UNIT_EXCLUDE <- c("meta", "metadata", "samples", "series_matrix")

metadata_path <- function() os_path("hubdata", METADATA_FILE)
summary_path  <- function() os_path("hubdata", SUMMARY_FILE)

# --- reading ---------------------------------------------------------------

# na_as_missing = TRUE turns the literal "NA" into R's NA. Off by default: most
# of the app compares against the string, and flipping that silently would
# change which samples get filtered.
load_metadata <- function(na_as_missing = FALSE) {
  df <- as.data.frame(readxl::read_excel(metadata_path(), .name_repair = "minimal"))
  if (na_as_missing) df[df == "NA"] <- NA
  df
}

load_datasets_summary <- function() {
  as.data.frame(readxl::read_excel(summary_path(), .name_repair = "minimal"))
}

# --- writing ---------------------------------------------------------------

# Writes `edited` over `target`, after putting `original` in backups/ under a
# timestamp. NA goes back as the literal "NA", the convention the file already
# uses and the rest of the app reads.
save_xlsx_with_backup <- function(original, edited, target, label) {
  backup_dir <- os_path("backups")
  if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)

  stamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup <- file.path(backup_dir, sprintf("%s_backup_%s.xlsx", label, stamp))
  if (!is.null(original)) writexl::write_xlsx(original, backup)

  edited <- as.data.frame(edited)
  edited[is.na(edited)] <- "NA"
  writexl::write_xlsx(edited, target)

  invisible(backup)
}

save_metadata <- function(original, edited) {
  backup <- save_xlsx_with_backup(original, edited, metadata_path(), "Metadata")
  invalidate_metadata()
  invisible(backup)
}

save_datasets_summary <- function(original, edited) {
  backup <- save_xlsx_with_backup(original, edited, summary_path(), "Datasets_summary")
  invalidate_metadata()
  invisible(backup)
}

# --- expression matrices ---------------------------------------------------

dataset_dir <- function(dataset) os_path("hubdata", dataset)

# The units actually on disk for this dataset, known ones first in the order the
# apps preferred them, then anything else found.
list_units <- function(dataset) {
  dir <- dataset_dir(dataset)
  if (!dir.exists(dir)) return(character())
  files <- list.files(dir, pattern = paste0("^", dataset, "_.*\\.txt$"))
  units <- sub(paste0("^", dataset, "_(.*)\\.txt$"), "\\1", files)
  units <- setdiff(units, UNIT_EXCLUDE)
  c(intersect(UNIT_PREFERENCE, units), sort(setdiff(units, UNIT_PREFERENCE)))
}

# unit = NULL picks the first available, preferred order.
expression_path <- function(dataset, unit = NULL) {
  if (is.null(unit) || !nzchar(unit)) {
    unit <- list_units(dataset)[1]
    if (is.na(unit)) {
      stop("no expression file for dataset '", dataset, "' in ", dataset_dir(dataset),
           call. = FALSE)
    }
  }
  file.path(dataset_dir(dataset), paste0(dataset, "_", unit, ".txt"))
}

# The first column holds the feature name; the rest are samples.
#
# Most matrices call that column Symbol, but some are written with the feature
# names as row names and no header for them -- WT1_CL_treatment_Yang's three
# files are like that. read.delim then gives the column an empty or made-up
# name, expr$Symbol is NULL, and every caller silently sees no genes at all:
# no autocomplete, nothing to plot, no error. Naming it here fixes it once for
# every module.
load_expression <- function(dataset, unit = NULL) {
  p <- expression_path(dataset, unit)
  if (!file.exists(p)) {
    have <- list_units(dataset)
    stop("no file ", basename(p), "; this dataset has: ",
         if (length(have)) paste(have, collapse = ", ") else "nothing",
         call. = FALSE)
  }
  df <- read.delim(p, check.names = FALSE)
  if (!ncol(df)) stop("empty expression file: ", basename(p), call. = FALSE)
  first <- names(df)[1]
  if (is.na(first) || !nzchar(trimws(first)) || first %in% c("X", "V1")) {
    names(df)[1] <- "Symbol"
  }
  df
}

# --- expression values ------------------------------------------------------

# The matrices carry up to 18 decimal places -- 4945.053081999999 is not
# precision, it is the binary remainder of a float written out as text. One
# decimal is all anyone reads off a plot.
#
# But a flat round(x, 1) is not safe on TPM: 4.5% of Rozen.BMI's non-zero
# values are below 0.05, and 330 of its genes would become all zero, which
# reads as "not expressed" rather than "barely expressed". So below 0.1 the
# value keeps three significant digits, which costs nothing in size and loses
# no gene.
os_round_expression <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  small <- !is.na(v) & v != 0 & abs(v) < 0.1
  out <- round(v, 1)
  out[small] <- signif(v[small], 3)
  out
}

# Format for writing back, element by element.
#
# format(v, scientific = FALSE) looked right and is a trap twice over: it picks
# one format for the whole vector, so a single small value pads every number in
# the row, and on a value like 1e-300 it writes three hundred characters. Below
# 0.1 the value is written with three significant digits, which lets the very
# small ones fall back to 1e-06 -- read.delim and as.numeric take that back
# exactly, and it is shorter and truer than a row of zeros.
os_format_expression <- function(x) {
  v <- os_round_expression(x)
  out <- character(length(v))
  small <- !is.na(v) & v != 0 & abs(v) < 0.1
  out[small]  <- formatC(v[small], format = "g", digits = 3)
  out[!small] <- formatC(v[!small], format = "f", digits = 1, drop0trailing = TRUE)
  out[is.na(v)] <- "NA"
  out
}

# Copy a matrix into the hub with the values rounded. Used by the upload tab
# and by tools/round-matrices.R, so the rule is applied in exactly one way.
#
# Streams: a 400 MB matrix does not need to be in memory to be rewritten.
os_write_rounded_matrix <- function(inp, out, chunk = 2000L) {
  header <- tryCatch(readLines(inp, n = 1L, warn = FALSE), error = function(e) character(0))
  if (!length(header) || !grepl("\t", header)) {
    file.copy(inp, out, overwrite = TRUE)
    return(list(rounded = FALSE, why = "it is not a tab-separated matrix"))
  }
  con_in  <- file(inp, "r"); on.exit(close(con_in), add = TRUE)
  con_out <- file(out, "w"); on.exit(close(con_out), add = TRUE)
  writeLines(readLines(con_in, n = 1L, warn = FALSE), con_out)
  rows <- 0L
  repeat {
    lines <- readLines(con_in, n = chunk, warn = FALSE)
    if (!length(lines)) break
    parts <- strsplit(lines, "\t", fixed = TRUE)
    writeLines(vapply(parts, function(p) {
      paste0(p[1], "\t", paste(os_format_expression(p[-1]), collapse = "\t"))
    }, character(1)), con_out)
    rows <- rows + length(lines)
  }
  list(rounded = TRUE, why = NA_character_, rows = rows)
}

# The unit to actually read, given the one a control is holding.
#
# A unit selector is filled from the dataset, so the moment the dataset changes
# the control still holds the previous one -- and anything that reads it before
# the browser answers asks for a file that does not exist. Loading Civelek
# while TPM was selected killed the session with "no file
# Civelek et al. 2017_TPM.txt", and the same for McMaster, DoD and El-Sayed,
# which have units no other dataset has.
os_valid_unit <- function(dataset, unit = NULL) {
  units <- tryCatch(list_units(dataset), error = function(e) character(0))
  if (!length(units)) return(NULL)
  if (!is.null(unit) && length(unit) == 1L && !is.na(unit) && unit %in% units) return(unit)
  units[1]
}

# One gene's row, without reading the matrix.
#
# Measured on GTEX_adipose_TMM.txt (402 MB): load_expression() takes 14.1 s and
# 530 MB of memory; this takes 0.02 s and returns the same numbers. Every
# caller that wants one gene out of 54,000 was paying the former.
#
# grep, not a byte-offset index: an index that goes stale returns a different
# gene's numbers and says nothing, which is worse than being slow. The symbol
# that comes back is checked against the one asked for.
load_expression_row <- function(dataset, symbols, unit = NULL) {
  # A control can still be holding the unit of the dataset we just left.
  unit <- os_valid_unit(dataset, unit)
  if (is.null(unit)) return(NULL)
  p <- expression_path(dataset, unit)
  if (!file.exists(p) || !length(symbols)) return(NULL)

  header <- strsplit(readLines(p, n = 1L, warn = FALSE), "\t", fixed = TRUE)[[1]]
  if (length(header) < 2L) return(NULL)
  # The first column may be unnamed, exactly as load_expression() handles.
  samples <- header[-1]

  # ^SYMBOL\t anchored: without the tab, LEP also matches LEPR and LEPROT.
  pat <- paste0("^(", paste(vapply(symbols, function(s)
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", s), character(1)), collapse = "|"),
    ")\t")
  lines <- suppressWarnings(system2("grep", c("-P", shQuote(pat), shQuote(p)),
                                    stdout = TRUE, stderr = FALSE))
  if (!length(lines) || !is.null(attr(lines, "status"))) return(NULL)

  rows <- lapply(lines, function(l) strsplit(l, "\t", fixed = TRUE)[[1]])
  found <- vapply(rows, `[`, character(1), 1L)
  keep <- found %in% symbols            # never trust the pattern alone
  rows <- rows[keep]; found <- found[keep]
  if (!length(rows)) return(NULL)

  mat <- do.call(rbind, lapply(rows, function(r) {
    v <- suppressWarnings(as.numeric(r[-1]))
    length(v) <- length(samples)
    v
  }))
  out <- data.frame(Symbol = found, mat, check.names = FALSE,
                    stringsAsFactors = FALSE)
  names(out)[-1] <- samples
  out
}

# The gene names of a dataset, for a menu.
#
# Five tabs used to call load_expression() for this and throw the matrix away:
# on GTEX that is 14 seconds and 530 MB to fill a dropdown. The gene index
# already holds the symbols, so it answers instead -- and falls back to the
# matrix when the index has not been built or does not know the dataset, so a
# fresh install still works, only slowly.
os_gene_choices <- function(dataset, unit = NULL) {
  idx <- tryCatch(ai_gene_index(), error = function(e) NULL)
  v <- if (!is.null(idx)) idx$datasets[[dataset]] else NULL
  if (!is.null(v) && length(v)) return(unique(v))
  expr <- tryCatch(load_expression(dataset, os_valid_unit(dataset, unit)),
                   error = function(e) NULL)
  if (is.null(expr) || !"Symbol" %in% names(expr)) return(character(0))
  unique(expr$Symbol)
}

# Does this matrix hold negative values?
#
# The answer decides whether a module log-transforms, and it used to be taken
# by sampling a matrix that had been read in full for the purpose. The first
# 200 rows and 10 sample columns carry the same information at no cost, which
# is what the sampling was already settling for.
os_matrix_has_negative <- function(dataset, unit = NULL, rows = 200L, cols = 10L) {
  p <- expression_path(dataset, unit)
  if (!file.exists(p)) return(FALSE)
  key <- paste0("omicscalpel.neg.", basename(p))
  cached <- getOption(key)
  if (!is.null(cached) && identical(cached$mtime, file.mtime(p))) return(cached$value)

  lines <- readLines(p, n = rows + 1L, warn = FALSE)[-1]
  vals <- unlist(lapply(lines, function(l) {
    f <- strsplit(l, "\t", fixed = TRUE)[[1]]
    suppressWarnings(as.numeric(utils::head(f[-1], cols)))
  }), use.names = FALSE)
  value <- isTRUE(any(vals < 0, na.rm = TRUE))
  do.call(options, stats::setNames(list(list(mtime = file.mtime(p), value = value)), key))
  value
}

# --- download log ----------------------------------------------------------

log_download <- function(...) {
  dir <- os_path("logs")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  fields <- vapply(list(...), function(x) paste(x, collapse = ", "), character(1))
  nm <- names(fields)
  if (is.null(nm)) nm <- rep("", length(fields))
  entry <- paste0(format(Sys.time()), " | ",
                  paste(ifelse(nzchar(nm), paste0(nm, ": "), ""), fields,
                        sep = "", collapse = " | "), "\n")
  cat(entry, file = file.path(dir, "downloads.log"), append = TRUE)
  invisible(entry)
}

# --- change notification ---------------------------------------------------
#
# Nine separate apps could each keep a stale copy of Metadata.xlsx and nobody
# noticed, because nobody could see two of them at once. In one app with nine
# tabs an edit that does not propagate is obvious, so the writers announce it.
# The announcement lives inside save_*() rather than in each module: a module
# cannot forget to call what it does not have to call.

# The counter lives in options(), not in a file-level environment, because this
# file gets sourced twice into two different environments: shiny autoloads R/*.R
# into its shared env and global.R sources it again. With a file-level env there
# would be two counters -- save_metadata() would bump one while the app watched
# the other, and an edit would silently never propagate. options() is
# process-wide, so every copy of this code shares one counter.
.metadata_version_val <- function() {
  v <- getOption("omicscalpel.metadata_version")
  if (is.null(v)) {
    # withReactiveDomain(NULL): a reactiveVal captures the session it is created
    # in, and this one outlives any single session because it lives in
    # options(). Created inside the first session, it dies with that session and
    # every later one fails with "its module session has been destroyed" -- the
    # app works for the first browser to connect and is broken for the second.
    v <- shiny::withReactiveDomain(NULL, shiny::reactiveVal(0L))
    options(omicscalpel.metadata_version = v)
  }
  v
}

# Depend on this inside a reactive that reads the metadata, and the read repeats
# whenever something writes it. Outside a reactive context -- a script, a test,
# the console -- reading a reactiveVal is an error, so there it just reports the
# number instead of blowing up.
metadata_version <- function() {
  v <- .metadata_version_val()
  tryCatch(v(), error = function(e) shiny::isolate(v()))
}

invalidate_metadata <- function() {
  v <- .metadata_version_val()
  v(shiny::isolate(v()) + 1L)
  invisible(NULL)
}
