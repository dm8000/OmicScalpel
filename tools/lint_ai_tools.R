#!/usr/bin/env Rscript
# lint_ai_tools.R -- does the machine interface still name real controls?
#
#   Rscript tools/lint_ai_tools.R
#
# R/ai_tools.R tells the question tab which controls it may set in each module,
# by their real input ids. Rename a control in the module and that list becomes
# a list of ids that do not exist: the plan applies nothing, the tab draws the
# previous plot, and nothing errors. This is the check that turns that silence
# into a failure.

source("R/config.R")
source("R/registry.R")
source(file.path("tools", "lintlib.R"))

# AI_TOOLS is data, so it can be read without starting the app.
ai_src <- parse("R/ai_tools.R")
env <- new.env(parent = baseenv())
for (e in ai_src) {
  if (is.call(e) && identical(as.character(e[[1]]), "<-") &&
      identical(as.character(e[[2]]), "AI_TOOLS")) {
    eval(e, envir = env)
    break
  }
}
if (!exists("AI_TOOLS", envir = env)) {
  cat("AI_TOOLS not found in R/ai_tools.R\n"); quit(status = 2)
}
AI_TOOLS <- get("AI_TOOLS", envir = env)

bad <- 0L
for (tab in names(AI_TOOLS)) {
  spec <- AI_TOOLS[[tab]]
  m <- MODULES[[tab]]
  if (is.null(m)) {
    cat(tab, ": no such module in R/registry.R\n", sep = ""); bad <- bad + 1L; next
  }
  f <- module_file(m)
  if (!file.exists(f)) {
    cat(tab, ": ", f, " does not exist\n", sep = ""); bad <- bad + 1L; next
  }
  have <- ids_of(f)

  want <- names(spec$controls)
  if (!is.na(spec$draw)) want <- c(want, spec$draw)
  missing <- setdiff(want, have)

  # `needs` has to be a subset of what can actually be set, or the planner can
  # declare a requirement it has no way of meeting.
  unmeetable <- setdiff(spec$needs, names(spec$controls))

  if (!identical(m$scope, spec$scope)) {
    cat(tab, ": scope says \"", spec$scope, "\" but the registry says \"",
        m$scope, "\"\n", sep = ""); bad <- bad + 1L
  }
  if (length(missing)) {
    cat(tab, ": not in the module's UI: ", paste(missing, collapse = ", "),
        "\n", sep = ""); bad <- bad + 1L
  }
  if (length(unmeetable)) {
    cat(tab, ": needs a control it cannot set: ",
        paste(unmeetable, collapse = ", "), "\n", sep = ""); bad <- bad + 1L
  }
  if (!length(missing) && !length(unmeetable)) {
    cat(tab, ": ok (", length(want), " controls)\n", sep = "")
  }
}

if (bad) {
  cat("\n", bad, " tab(s) whose machine interface does not match the GUI\n", sep = "")
  quit(status = 1)
}
cat("\nthe machine interface names only controls that exist\n")
