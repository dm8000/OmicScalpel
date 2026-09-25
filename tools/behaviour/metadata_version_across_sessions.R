#!/usr/bin/env Rscript
# The metadata version counter must outlive a session.
#
# It lives in options() so that the two copies of data_io.R -- shiny autoloads
# R/*.R and global.R sources it again -- share one counter. But a reactiveVal
# captures the session it was created in, so one created inside the first
# session dies with it: the app then works for the first browser to connect and
# throws "its module session has been destroyed" for every one after.
#
#   Rscript tools/behaviour/metadata_version_across_sessions.R

source("global.R")

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

options(omicscalpel.metadata_version = NULL)   # start from nothing

srv <- function(input, output, session) {
  observe({ metadata_version() })
}

# first session creates the counter and then ends
r1 <- tryCatch({ testServer(srv, { session$flushReact(); metadata_version() }) ; "ok" },
               error = function(e) conditionMessage(e))
chk(identical(r1, "ok"), "the first session can read the version", r1)

# a second session, in the same process, must still be able to
r2 <- tryCatch({ testServer(srv, { session$flushReact(); metadata_version() }) ; "ok" },
               error = function(e) conditionMessage(e))
chk(identical(r2, "ok"), "and so can the second", r2)

# and a third, after a write bumped it
invisible(invalidate_metadata())
r3 <- tryCatch({ testServer(srv, { session$flushReact(); metadata_version() }) ; "ok" },
               error = function(e) conditionMessage(e))
chk(identical(r3, "ok"), "and a third after a write", r3)

cat("\n", n, " checks passed\n", sep = "")
