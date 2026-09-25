#!/usr/bin/env Rscript
# The Cutoff finder tab, end to end: the wiring, not the statistics.
#
#   Rscript tools/behaviour/cutoff_finder_module.R
#
# tools/behaviour/cutoff_finder.R already checks the maths against
# independently computed answers. What can still be wrong here is everything
# between the screen and those functions: which column feeds which argument,
# whether a gene's expression is lined up with the right metadata rows, and
# whether the split that gets saved is the split that was shown.

source("global.R")
source("tools/behaviour/_fixture.R")
tmp <- use_fixture_hub(writable = TRUE)

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

DS <- "DEMO_RNAseq"

# --- a dataset big enough for the search to be honest ------------------------
# The fixture has 19 RNAseq samples, below the tab's own floor. That floor is
# checked further down; the statistics need a cohort, so this one is built.
set.seed(909)
N   <- 150L
CUT <- 6
mk  <- round(runif(N, 0, 10), 3)
big <- data.frame(
  SampleID       = sprintf("%s_B%03d", DS, seq_len(N)),
  dataset        = DS,
  `Data.type`    = "RNAseq",
  Author         = "demo",
  DEMO.Marker    = as.character(mk),
  DEMO.OS.time   = as.character(round(pmax(rexp(N, ifelse(mk > CUT, 3, 1) / 40), .1), 1)),
  DEMO.OS.event  = as.character(rbinom(N, 1, .8)),
  DEMO.Responder = ifelse(mk > CUT, "yes", "no"),
  DEMO.Bimodal   = as.character(round(c(rnorm(N / 2, 3, .8), rnorm(N / 2, 9, .8)), 3)),
  check.names = FALSE, stringsAsFactors = FALSE
)
big$DEMO.Responder[sample.int(N, 15)] <- sample(c("yes", "no"), 15, TRUE)

# Reading an output renders it. The tab once shipped a survival result whose
# two plot panels both read "Error: is.character(txt) is not TRUE", because a
# validate() on the happy path threw before any plot was drawn -- invisible to
# a test that only reads reactives.
draw <- function(out, what) {
  r <- tryCatch(out, error = function(e) conditionMessage(e))
  if (is.character(r)) no(what, r)
  if (is.null(r)) no(what, "nothing was rendered")
  ok(what)
}

run <- function(md, dataset, inputs) {
  res <- NULL
  testServer(cutoffFinderServer,
             args = list(ds = reactiveVal(dataset), meta = reactive(md), go_to = NULL), {
    do.call(session$setInputs, inputs)
    # found() is an eventReactive: without a click there is nothing to read,
    # and the gene case below only wants the variable.
    got <- if (isTRUE(inputs$find > 0)) found() else NULL
    # There is no browser here, so updateTextInput() never comes back as
    # input$col_name. The name it sends is a reactive, which can be read.
    res <<- list(found = got, var = variable(),
                 name = if (is.null(got)) "" else default_name())
  })
  res
}

surv_inputs <- list(source = "meta", meta_col = "DEMO.Marker", method = "survival",
                    time_col = "DEMO.OS.time", event_col = "DEMO.OS.event",
                    min_frac = 0.1, perm = 60, find = 1)

r <- run(big, DS, surv_inputs)
f <- r$found
chk(is.null(f$error), "survival on a metadata column runs", f$error)
chk(abs(f$cutoff - CUT) <= 1, "and finds the planted cutpoint",
    "got ", f$cutoff, ", planted ", CUT)
chk(f$n_low + f$n_high == N, "every sample lands on one side",
    f$n_low, " + ", f$n_high, " != ", N)
chk(f$best$effect > 1.5 && f$best$pvalue < 1e-3, "the hazard ratio is the planted one",
    "HR ", round(f$best$effect, 2), " p ", f$best$pvalue)
chk(f$perm$p >= f$best$pvalue, "the corrected p is never smaller than the raw one",
    f$perm$p, " < ", f$best$pvalue)
chk(f$perm$B == 60, "it ran the permutations asked for, not a default", f$perm$B)

# The name offered for saving must carry the variable and the cutoff: two
# different cutoffs on one variable are two different columns.
chk(grepl("DEMO.Marker", r$name, fixed = TRUE) && grepl("\\d", r$name),
    "the column name offered names the variable and the cutoff", r$name)

# and the three panels a researcher actually looks at
testServer(cutoffFinderServer,
           args = list(ds = reactiveVal(DS), meta = reactive(big), go_to = NULL), {
  do.call(session$setInputs, modifyList(surv_inputs, list(perm = 0)))
  draw(output$scan_plot,    "the cutpoint scan renders")
  draw(output$outcome_plot, "the Kaplan-Meier renders")
  draw(output$result,       "the result table renders")
})

# --- the floor on group size -------------------------------------------------
wide <- run(big, DS, modifyList(surv_inputs, list(min_frac = 0.4, perm = 0)))$found
chk(is.null(wide$error), "a 40% floor still finds a cutpoint here", wide$error)
chk(min(wide$n_low, wide$n_high) >= 0.4 * N,
    "and no group is smaller than the floor allows",
    min(wide$n_low, wide$n_high), " < ", 0.4 * N)
chk(min(f$n_low, f$n_high) >= 0.1 * N, "the default floor holds too")

# --- binary outcome ----------------------------------------------------------
b <- run(big, DS, list(source = "meta", meta_col = "DEMO.Marker", method = "binary",
                       outcome_col = "DEMO.Responder", positive = "yes",
                       rule = "euclidean", min_frac = 0.1, perm = 0, find = 1))$found
chk(is.null(b$error), "binary outcome runs", b$error)
chk(abs(b$cutoff - CUT) <= 1, "the ROC corner lands on the planted cutpoint", b$cutoff)
chk(b$auc > 0.85, "and the AUC reflects a real separation", b$auc)
chk(!is.null(b$best$sensitivity) && b$best$sensitivity > 0.7,
    "sensitivity comes back with it")

# --- distribution ------------------------------------------------------------
d <- run(big, DS, list(source = "meta", meta_col = "DEMO.Bimodal",
                       method = "distribution", find = 1))$found
chk(is.null(d$error), "the mixture method runs with no outcome at all", d$error)
chk(d$cutoff > 4 && d$cutoff < 8, "and cuts between the two components", d$cutoff)

# --- expression as the variable ----------------------------------------------
# The bug this guards: expression columns are in file order and metadata rows
# are in spreadsheet order. Reading one as if it were the other silently pairs
# each sample's expression with somebody else's outcome.
# Shuffled on purpose. With the rows in file order the two orders coincide and
# this check passes whether or not the module aligns anything -- which is how a
# test proves nothing. The spreadsheet guarantees no order, so shuffling is a
# metadata file the app must already handle.
fx   <- load_metadata()
fx   <- fx[sample.int(nrow(fx)), , drop = FALSE]
unit <- list_units(DS)[1]
expr <- load_expression(DS, unit)
gene <- expr$Symbol[1]
g <- run(fx, DS, list(source = "gene", unit = unit, gene = gene,
                      method = "survival", time_col = "DEMO.OS.time",
                      event_col = "DEMO.OS.event", min_frac = 0.1, perm = 0))

rows <- fx[fx$dataset == DS, , drop = FALSE]
want <- as.numeric(expr[expr$Symbol == gene, rows$SampleID])
chk(identical(length(g$var$values), nrow(rows)),
    "the gene gives one value per metadata row of the dataset")
chk(isTRUE(all.equal(g$var$values, want)),
    "each value sits on its own sample's row, in metadata order")
chk(grepl(gene, g$var$name, fixed = TRUE) && grepl(unit, g$var$name, fixed = TRUE),
    "the variable is named after the gene and the unit", g$var$name)

# --- and it refuses a cohort too small to mean anything ----------------------
small <- run(fx, DS, list(source = "meta", meta_col = "DEMO.Marker",
                          method = "survival", time_col = "DEMO.OS.time",
                          event_col = "DEMO.OS.event", min_frac = 0.1,
                          perm = 0, find = 1))$found
chk(!is.null(small$error), "19 samples are refused, not fitted")
chk(grepl("at least", small$error), "and the refusal says what it needs",
    small$error)

# --- saving ------------------------------------------------------------------
saved <- NULL
testServer(cutoffFinderServer,
           args = list(ds = reactiveVal(DS), meta = reactive(big), go_to = NULL), {
  do.call(session$setInputs, surv_inputs)
  session$setInputs(col_name = "DEMO.Test.Split", save = 1)
  saved <<- found()
})
after <- load_metadata()
chk("DEMO.Test.Split" %in% names(after), "the split reaches the metadata file")
got <- after$DEMO.Test.Split[after$dataset == DS]
chk(setequal(unique(got), c("low", "high")), "written as low/high",
    paste(unique(got), collapse = ", "))
expected <- as.character(cf_split(as.numeric(big$DEMO.Marker), saved$cutoff))
chk(identical(got, expected), "and it is the split that was shown, sample by sample")

cat("\n", n, " checks passed\n", sep = "")
