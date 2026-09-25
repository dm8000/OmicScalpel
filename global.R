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

source("R/ui_helpers.R")
source("R/meta_stats.R")
source("R/cutoff_finder.R")
source("R/cutoff_plots.R")
source("R/jev.R")
source("R/ai_catalog.R")
source("R/ai_genes.R")
source("R/ai_tools.R")
source("R/ai_plan.R")
source("R/data_io.R")

# One look for every figure, set once rather than asked for per plot.
if (requireNamespace("ggplot2", quietly = TRUE)) os_set_plot_defaults()

# A module that fails to source must not take the other eight with it. One
# broken file used to make every later module in a conversion chain fail on
# someone else's syntax error, which is a very confusing way to be told about
# a stray parenthesis. Loudly skipped instead, and recorded so the checks can
# report it rather than crash.
MODULE_LOAD_FAILED <- list()
for (m in MODULES) {
  f <- module_file(m)
  if (!file.exists(f)) next
  e <- tryCatch({ source(f); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(e)) {
    MODULE_LOAD_FAILED[[m$id]] <- e
    message("OmicScalpel: ", m$id, " failed to load: ", e)
  }
}
