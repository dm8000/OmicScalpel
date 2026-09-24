#!/usr/bin/env Rscript
# Is this machine able to run every tab? Exits 1 if not.
#
# global.R warns and carries on, which is what you want while developing.
# Deployment wants the opposite, so this is what the deploy script runs.

source("R/config.R")
source("R/registry.R")
lib <- os_path("lib")
if (dir.exists(lib)) .libPaths(lib)

need <- unique(c("shiny", "shinydashboard", "readxl", "writexl",
                 unlist(lapply(MODULES, function(m) m$pkgs))))
have <- rownames(installed.packages())
absent <- setdiff(need, have)

cat("library path : ", paste(.libPaths(), collapse = "\n               "), "\n", sep = "")
cat("packages     : ", length(need) - length(absent), "/", length(need), " present\n", sep = "")

if (!length(absent)) { cat("all tabs can run\n"); quit(status = 0) }

cat("\nmissing: ", paste(absent, collapse = ", "), "\n\nblocks:\n", sep = "")
for (m in MODULES) {
  bad <- intersect(m$pkgs, absent)
  if (length(bad)) cat("  ", m$id, " <- ", paste(bad, collapse = ", "), "\n", sep = "")
}
quit(status = 1)
