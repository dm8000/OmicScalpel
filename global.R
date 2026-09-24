# global.R -- shiny sources this before app.R, in the same environment.
#
# Order matters: config first (base R only), then the library path it names,
# then packages, then project code.

source("R/config.R")

lib <- os_path("lib")
if (dir.exists(lib)) .libPaths(lib)

source("R/registry.R")

# Core: the shell and the data layer. Without these nothing runs.
CORE_PKGS <- c("shiny", "shinydashboard", "readxl", "writexl")

# Load what is here and say precisely what is not. A development machine often
# has eight of nine modules' dependencies, and dying inside an anonymous
# library() call tells you neither which package nor which tab it cost you.
.load <- function(p) {
  suppressPackageStartupMessages(
    isTRUE(tryCatch({ library(p, character.only = TRUE); TRUE },
                    error = function(e) FALSE)))
}

missing_core <- CORE_PKGS[!vapply(CORE_PKGS, .load, logical(1))]
if (length(missing_core)) {
  stop("OmicScalpel cannot start; missing: ", paste(missing_core, collapse = ", "),
       "\n  library path: ", paste(.libPaths(), collapse = ", "), call. = FALSE)
}

MODULE_PKG_MISSING <- list()
for (m in MODULES) {
  absent <- m$pkgs[!vapply(m$pkgs, .load, logical(1))]
  if (length(absent)) MODULE_PKG_MISSING[[m$id]] <- absent
}
if (length(MODULE_PKG_MISSING)) {
  message("OmicScalpel: some tabs are missing packages and will fail if opened:")
  for (id in names(MODULE_PKG_MISSING)) {
    message("  ", id, ": ", paste(MODULE_PKG_MISSING[[id]], collapse = ", "))
  }
}

source("R/data_io.R")

for (m in MODULES) {
  f <- module_file(m)
  if (file.exists(f)) source(f)
}
