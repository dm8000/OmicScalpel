#!/usr/bin/env Rscript
# lint_parity.R -- did the conversion lose a control?
#
#   Rscript tools/lint_parity.R [module_id ...]
#
# For each converted module, compare the set of input and output ids against
# the legacy app it came from, and report the ones that disappeared.
#
# This exists because metadata-editor lost its "Add Column" and "Build metadata
# table" buttons in conversion. The module parsed, passed the namespace linter
# and started under the smoke test; the buttons were simply not on the page,
# and the server still listened for one of them. Nothing else could see that.
#
# Deliberate removals go in tools/parity-exceptions.txt, one per line:
#   <module_id> <id>   # why

source("R/config.R")
source("R/registry.R")

source(file.path("tools", "lintlib.R"))

exc_file <- file.path("tools", "parity-exceptions.txt")
exceptions <- list()
if (file.exists(exc_file)) {
  for (l in readLines(exc_file, warn = FALSE)) {
    l <- trimws(sub("#.*$", "", l))
    if (!nzchar(l)) next
    parts <- strsplit(l, "[[:space:]]+")[[1]]
    if (length(parts) >= 2) exceptions[[parts[1]]] <- c(exceptions[[parts[1]]], parts[2])
  }
}

pick <- commandArgs(TRUE)
mods <- if (length(pick)) MODULES[pick] else MODULES
bad <- 0L

for (m in mods) {
  f <- module_file(m)
  legacy <- file.path(legacy_dir(m), "app.R")
  if (!file.exists(f) || !file.exists(legacy)) next
  lost <- setdiff(ids_of(legacy), c(ids_of(f), exceptions[[m$id]]))
  if (length(lost)) {
    bad <- bad + 1L
    cat(m$id, ": ", length(lost), " control(s) lost: ",
        paste(lost, collapse = ", "), "\n", sep = "")
  }
}

if (bad == 0L) cat("every module keeps its legacy app's controls\n")
quit(status = if (bad > 0L) 1L else 0L)
