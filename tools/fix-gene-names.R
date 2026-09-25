#!/usr/bin/env Rscript
# Put back the gene symbols a spreadsheet turned into dates.
#
#   Rscript tools/fix-gene-names.R [--dry] [--keep-backup]
#
# SEPT1 becomes "1-Sep" on the way into Excel, and if the file is saved again
# it becomes 45352 -- the serial number of that date. config/gene-name-fixes.txt
# is the map, by exact literal: DEC1 and SEP15 are real symbols and a pattern
# that looks for month names destroys both.
#
# Order: copy, rewrite, verify, report, delete the copy. The gene index has to
# be rebuilt afterwards, and this says so.

source("global.R")

args        <- commandArgs(TRUE)
dry         <- "--dry" %in% args
keep_backup <- "--keep-backup" %in% args

fixes <- local({
  path <- file.path(os_root(), "config", "gene-name-fixes.txt")
  if (!file.exists(path)) stop("no config/gene-name-fixes.txt", call. = FALSE)
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^\\s*(#|$)", lines)]
  parts <- strsplit(lines, "\\s*\\|\\s*")
  keep <- lengths(parts) >= 2
  stats::setNames(trimws(vapply(parts[keep], `[`, character(1), 2)),
                  trimws(vapply(parts[keep], `[`, character(1), 1)))
})
cat(length(fixes), " replacements known\n\n", sep = "")

md <- load_metadata()
jobs <- list()
for (d in sort(unique(md$dataset))) {
  for (u in tryCatch(list_units(d), error = function(e) character(0))) {
    p <- tryCatch(expression_path(d, u), error = function(e) NA_character_)
    if (!is.na(p) && file.exists(p)) jobs[[length(jobs) + 1L]] <- list(dataset = d, unit = u, path = p)
  }
}

# Which files are affected, and whether the repair would collide with a symbol
# the file already has -- two rows called MARCH1 is worse than one called 45352.
plan <- list()
for (j in jobs) {
  syms <- system2("cut", c("-f1", shQuote(j$path)), stdout = TRUE)[-1]
  hit <- intersect(syms, names(fixes))
  if (!length(hit)) next
  want <- unname(fixes[hit])
  clash <- intersect(want, syms)
  plan[[length(plan) + 1L]] <- list(job = j, hit = hit, want = want, clash = clash,
                                    rows = length(syms))
  cat(sprintf("  %-34s %-12s %2d to fix%s\n", j$dataset, j$unit, length(hit),
              if (length(clash)) paste0("  CLASH with existing ", paste(clash, collapse = ", ")) else ""))
}

if (!length(plan)) { cat("\nnothing to fix\n"); quit(status = 0) }
if (dry) quit(status = 0)

clashes <- Filter(function(p) length(p$clash) > 0, plan)
if (length(clashes)) {
  cat("\nrefusing: repairing these would give a matrix two rows with the same symbol.\n")
  quit(status = 1)
}

stamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup <- file.path(os_path("backups"), paste0("genenames_", stamp))
dir.create(backup, recursive = TRUE, showWarnings = FALSE)
cat("\nbackup: ", backup, "\n\n", sep = "")

fails <- character(0)
for (p in plan) {
  j <- p$job
  bdir <- file.path(backup, j$dataset)
  dir.create(bdir, recursive = TRUE, showWarnings = FALSE)
  bcopy <- file.path(bdir, basename(j$path))
  if (!file.copy(j$path, bcopy, overwrite = TRUE)) {
    fails <- c(fails, paste(j$dataset, j$unit, "backup failed")); next
  }

  tmp <- paste0(j$path, ".renaming")
  con_in <- file(bcopy, "r"); con_out <- file(tmp, "w")
  writeLines(readLines(con_in, n = 1L, warn = FALSE), con_out)
  changed <- 0L
  repeat {
    lines <- readLines(con_in, n = 5000L, warn = FALSE)
    if (!length(lines)) break
    # Only the first field, and only an exact match.
    sym <- sub("\t.*$", "", lines)
    k <- match(sym, names(fixes))
    hit <- !is.na(k)
    if (any(hit)) {
      lines[hit] <- paste0(unname(fixes[k[hit]]), sub("^[^\t]*", "", lines[hit]))
      changed <- changed + sum(hit)
    }
    writeLines(lines, con_out)
  }
  close(con_in); close(con_out)

  # every intended replacement happened, the shape did not move, and no symbol
  # now appears twice
  new_syms <- system2("cut", c("-f1", shQuote(tmp)), stdout = TRUE)[-1]
  problem <- NULL
  if (changed != length(p$hit)) problem <- sprintf("changed %d rows, expected %d", changed, length(p$hit))
  else if (length(new_syms) != p$rows) problem <- "the row count moved"
  else if (any(duplicated(new_syms[new_syms %in% p$want]))) problem <- "a repaired symbol now appears twice"
  else if (length(intersect(new_syms, names(fixes)))) problem <- "a corrupted name survived"

  if (!is.null(problem)) {
    unlink(tmp); fails <- c(fails, paste(j$dataset, j$unit, problem))
    cat(sprintf("  %-34s %-12s FAILED: %s\n", j$dataset, j$unit, problem)); next
  }
  file.rename(tmp, j$path)
  cat(sprintf("  %-34s %-12s %2d repaired\n", j$dataset, j$unit, changed))
}

cat("\n")
if (length(fails)) {
  cat("FAILED; backup kept at ", backup, "\n", sep = "")
  for (f in fails) cat("  - ", f, "\n", sep = "")
  quit(status = 1)
}
if (keep_backup) {
  cat("backup kept at ", backup, "\n", sep = "")
} else {
  unlink(backup, recursive = TRUE)
  cat("verified, backup deleted.\n")
}
cat("Now run tools/build-gene-index.R: the index still has the old names.\n")
