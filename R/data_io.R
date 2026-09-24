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
  save_xlsx_with_backup(original, edited, metadata_path(), "Metadata")
}

save_datasets_summary <- function(original, edited) {
  save_xlsx_with_backup(original, edited, summary_path(), "Datasets_summary")
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
load_expression <- function(dataset, unit = NULL) {
  p <- expression_path(dataset, unit)
  if (!file.exists(p)) {
    have <- list_units(dataset)
    stop("no file ", basename(p), "; this dataset has: ",
         if (length(have)) paste(have, collapse = ", ") else "nothing",
         call. = FALSE)
  }
  read.delim(p, check.names = FALSE)
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
