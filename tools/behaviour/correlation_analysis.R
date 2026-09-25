#!/usr/bin/env Rscript
# Behaviour test for the correlation tab.
#
# The check that matters is the last one. The conversion produced
# `function(dataset) { filter(dataset == dataset) }` -- a parameter named after
# the column it filters on, so the comparison was column-to-itself and every
# row of every dataset came back. The tab still ran, still plotted, and offered
# the wrong columns. Nothing but a value check can see that.
#
#   Rscript tools/behaviour/correlation_analysis.R

source("global.R")
source("tools/behaviour/_fixture.R")
use_fixture_hub()

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

meta_df <- load_metadata()
both <- sort(unique(meta_df$dataset))

testServer(
  correlationServer,
  args = list(ds = reactiveVal(both[1]), meta = reactive(meta_df), go_to = NULL),
  {
    session$flushReact()

    a_cond <- get_conditions_with_multiple_values(both[1])
    b_cond <- get_conditions_with_multiple_values(both[2])
    chk(length(a_cond) > 0, "the first dataset offers conditions", length(a_cond))

    # Build the same answer independently, from the metadata, and require the
    # module to agree with it.
    expect <- function(d) {
      sel  <- meta_df[meta_df$dataset == d, , drop = FALSE]
      cols <- names(sel)[4:ncol(sel)]
      cols[vapply(cols, function(cn) {
        v <- unique(sel[[cn]]); v <- v[!is.na(v) & v != "NA"]; length(v) > 1
      }, logical(1))]
    }
    chk(setequal(a_cond, expect(both[1])),
        "and they are the ones that vary within that dataset",
        length(a_cond), " vs ", length(expect(both[1])))

    chk(!setequal(a_cond, b_cond),
        "two datasets give two different condition lists",
        length(a_cond), " vs ", length(b_cond))

    a_num <- get_numeric_columns_with_multiple_values(both[1])
    chk(length(a_num) > 0, "numeric columns are found", length(a_num))
  }
)

cat("\n", n, " checks passed\n", sep = "")
