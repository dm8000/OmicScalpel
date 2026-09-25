#!/usr/bin/env Rscript
# Rename datasets to "first author et al. year".
#
#   Rscript tools/rename-datasets.R [--dry] [--keep-backup]
#
# The dataset name is three things at once here: a value in two spreadsheets, a
# directory, and the prefix of every matrix file inside it. All three move
# together or the dataset disappears.
#
# config/dataset-renames.txt is the map, and it records where each attribution
# came from -- two of the old nicknames did not name the paper's first author.

source("global.R")

args <- commandArgs(TRUE)
dry  <- "--dry" %in% args
keep_backup <- "--keep-backup" %in% args

renames <- local({
  path <- file.path(os_root(), "config", "dataset-renames.txt")
  if (!file.exists(path)) stop("no config/dataset-renames.txt", call. = FALSE)
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^\\s*(#|$)", lines)]
  parts <- strsplit(lines, "\\s*\\|\\s*")
  keep <- lengths(parts) >= 2
  stats::setNames(trimws(vapply(parts[keep], `[`, character(1), 2)),
                  trimws(vapply(parts[keep], `[`, character(1), 1)))
})

md <- load_metadata()
su <- tryCatch(load_datasets_summary(), error = function(e) NULL)
have <- unique(md$dataset)

missing <- setdiff(names(renames), have)
if (length(missing)) {
  cat("not in the metadata: ", paste(missing, collapse = ", "), "\n", sep = "")
  quit(status = 1)
}
clash <- intersect(unname(renames), have)
if (length(clash)) {
  cat("refusing: the new name is already taken: ", paste(clash, collapse = ", "), "\n", sep = "")
  quit(status = 1)
}

hub <- os_path("hubdata")
for (old in names(renames)) {
  new <- renames[[old]]
  files <- list.files(file.path(hub, old))
  cat(sprintf("  %-32s -> %-30s %d rows, %d file(s)\n", old, new,
              sum(md$dataset == old), length(files)))
}
if (dry) quit(status = 0)

stamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup <- file.path(os_path("backups"), paste0("rename_", stamp))
dir.create(backup, recursive = TRUE, showWarnings = FALSE)

# The directories first: a failure there must not leave the spreadsheets
# pointing at names that do not exist.
moved <- list()
for (old in names(renames)) {
  new <- renames[[old]]
  od <- file.path(hub, old); nd <- file.path(hub, new)
  if (!dir.exists(od)) { cat("  no directory for ", old, ", skipping files\n", sep = ""); next }
  if (!file.rename(od, nd)) { cat("FAILED to rename directory ", od, "\n", sep = ""); quit(status = 1) }
  for (f in list.files(nd)) {
    if (startsWith(f, old)) {
      file.rename(file.path(nd, f), file.path(nd, sub(paste0("^", old), new, f)))
    }
  }
  moved[[old]] <- new
}

# Then the spreadsheets, through the data layer so both get a backup.
md_new <- md
md_new$dataset <- ifelse(md_new$dataset %in% names(renames),
                         unname(renames[md_new$dataset]), md_new$dataset)
save_metadata(md, md_new)

if (!is.null(su) && "dataset" %in% names(su)) {
  su_new <- su
  su_new$dataset <- ifelse(su_new$dataset %in% names(renames),
                           unname(renames[su_new$dataset]), su_new$dataset)
  save_datasets_summary(su, su_new)
}

# --- verify -------------------------------------------------------------------
after <- load_metadata()
bad <- character(0)
for (old in names(renames)) {
  new <- renames[[old]]
  if (any(after$dataset == old)) bad <- c(bad, paste(old, "still in the metadata"))
  if (sum(after$dataset == new) != sum(md$dataset == old)) {
    bad <- c(bad, paste(new, "has the wrong number of rows"))
  }
  if (!dir.exists(file.path(hub, new))) bad <- c(bad, paste(new, "has no directory"))
  units <- tryCatch(list_units(new), error = function(e) character(0))
  if (!length(units)) bad <- c(bad, paste(new, "has no expression file the app can see"))
}
if (nrow(after) != nrow(md)) bad <- c(bad, "the metadata changed length")

cat("\n")
if (length(bad)) {
  cat("FAILED:\n"); for (b in bad) cat("  - ", b, "\n", sep = "")
  cat("The spreadsheet backups are in ", os_path("backups"), "\n", sep = "")
  quit(status = 1)
}
unlink(backup, recursive = TRUE)
cat("renamed and verified: ", length(moved), " dataset(s)\n", sep = "")
cat("Now run tools/build-gene-index.R -- the index is keyed by dataset name.\n")
