#!/usr/bin/env Rscript
# The Cutoff finder tab, end to end: the wiring, not the statistics.
#
#   Rscript tools/behaviour/cutoff_finder_module.R
#
# tools/behaviour/cutoff_finder.R already checks the maths against
# independently computed answers. What can still be wrong here is everything
# between the screen and those functions: which column feeds which argument,
# whether a gene's expression is lined up with the right metadata rows, whether
# a split really runs separate searches, and whether the split that gets saved
# is the split that was shown.

source("global.R")
source("tools/behaviour/_fixture.R")
tmp <- use_fixture_hub(writable = TRUE)

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

# Reading an output renders it. Named renders(), not draw(): the modules now
# have a reactive called draw, and inside testServer the module's environment
# wins. The tab once shipped a survival result whose
# two plot panels both read "Error: is.character(txt) is not TRUE", because a
# validate() on the happy path threw before any plot was drawn -- invisible to
# a test that only reads reactives.
renders <- function(out, what) {
  r <- tryCatch(out, error = function(e) conditionMessage(e))
  if (is.character(r)) no(what, r)
  if (is.null(r)) no(what, "nothing was rendered")
  ok(what)
}

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

run <- function(md, dataset, inputs) {
  res <- NULL
  testServer(cutoffFinderServer,
             args = list(ds = reactiveVal(dataset), meta = reactive(md), go_to = NULL), {
    do.call(session$setInputs, inputs)
    got <- if (isTRUE(inputs$find > 0)) found() else NULL
    # There is no browser here, so updateTextInput() never comes back as
    # input$col_name. The name it sends is a reactive, which can be read.
    res <<- list(found = got, var = variable(), cats = categorical_cols(),
                 name = if (is.null(got)) "" else default_name())
  })
  res
}
one <- function(r) r$found$groups[[1]]

surv_inputs <- list(source = "meta", meta_col = "DEMO.Marker", method = "survival",
                    time_col = "DEMO.OS.time", event_col = "DEMO.OS.event",
                    min_frac = 0.1, perm = 60, find = 1)

r <- run(big, DS, surv_inputs)
f <- one(r)
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
  renders(output$scan_plot,    "the cutpoint scan renders")
  renders(output$outcome_plot, "the Kaplan-Meier renders")
  renders(output$result,       "the result table renders")
})

# --- the floor on group size -------------------------------------------------
wide <- one(run(big, DS, modifyList(surv_inputs, list(min_frac = 0.4, perm = 0))))
chk(is.null(wide$error), "a 40% floor still finds a cutpoint here", wide$error)
chk(min(wide$n_low, wide$n_high) >= 0.4 * N,
    "and no group is smaller than the floor allows",
    min(wide$n_low, wide$n_high), " < ", 0.4 * N)
chk(min(f$n_low, f$n_high) >= 0.1 * N, "the default floor holds too")

# --- binary outcome ----------------------------------------------------------
b <- one(run(big, DS, list(source = "meta", meta_col = "DEMO.Marker", method = "binary",
                           outcome_col = "DEMO.Responder", positive = "yes",
                           rule = "euclidean", min_frac = 0.1, perm = 0, find = 1)))
chk(is.null(b$error), "binary outcome runs", b$error)
chk(abs(b$cutoff - CUT) <= 1, "the ROC corner lands on the planted cutpoint", b$cutoff)
chk(b$auc > 0.85, "and the AUC reflects a real separation", b$auc)
chk(!is.null(b$best$sensitivity) && b$best$sensitivity > 0.7,
    "sensitivity comes back with it")

# --- distribution ------------------------------------------------------------
d <- one(run(big, DS, list(source = "meta", meta_col = "DEMO.Bimodal",
                           method = "distribution", find = 1)))
chk(is.null(d$error), "the mixture method runs with no outcome at all", d$error)
chk(d$cutoff > 4 && d$cutoff < 8, "and cuts between the two components", d$cutoff)

# --- splitting the analysis --------------------------------------------------
# The cutpoint is deliberately different in the two sexes. If the split is real
# the groups come back with different cutoffs; if the tab analysed everybody
# twice under two labels, they come back the same.
set.seed(77)
M    <- 240L
sex  <- rep(c("female", "male"), each = M / 2)
cuts <- c(female = 3, male = 7)
mk2  <- round(runif(M, 0, 10), 3)
spl  <- data.frame(
  SampleID      = sprintf("%s_X%03d", DS, seq_len(M)),
  dataset       = DS,
  DEMO.Sex      = sex,
  DEMO.Marker   = as.character(mk2),
  DEMO.Age      = as.character(round(runif(M, 20, 80), 1)),
  DEMO.OS.time  = as.character(round(pmax(rexp(M, ifelse(mk2 > cuts[sex], 4, 1) / 40), .1), 1)),
  DEMO.OS.event = as.character(rbinom(M, 1, .85)),
  check.names = FALSE, stringsAsFactors = FALSE
)

split_inputs <- list(source = "meta", meta_col = "DEMO.Marker", method = "survival",
                     time_col = "DEMO.OS.time", event_col = "DEMO.OS.event",
                     min_frac = 0.1, perm = 0, split_col = "DEMO.Sex",
                     split_levels = c("female", "male"), find = 1)
s <- run(spl, DS, split_inputs)

chk("DEMO.Sex" %in% s$cats, "a categorical column is offered as a split")
chk(!any(c("DEMO.Marker", "DEMO.Age", "DEMO.OS.time") %in% s$cats),
    "and no numeric column is, since making groups out of numbers is this tab's job",
    paste(intersect(c("DEMO.Marker", "DEMO.Age", "DEMO.OS.time"), s$cats), collapse = ", "))
chk(length(s$found$groups) == 2, "two groups, two searches",
    length(s$found$groups))
chk(identical(vapply(s$found$groups, function(g) g$label, character(1)),
              c("female", "male")), "labelled by the level they came from")

by <- setNames(lapply(s$found$groups, identity),
               vapply(s$found$groups, function(g) g$label, character(1)))
chk(is.null(by$female$error) && is.null(by$male$error), "both groups ran",
    by$female$error, by$male$error)
chk(abs(by$female$cutoff - 3) <= 1.2 && abs(by$male$cutoff - 7) <= 1.2,
    "each group finds its own planted cutpoint",
    "female ", by$female$cutoff, " (3), male ", by$male$cutoff, " (7)")
chk(by$female$n_low + by$female$n_high == M / 2,
    "a group only sees its own samples",
    by$female$n_low + by$female$n_high, " of ", M / 2)
chk(identical(s$name, "DEMO.Marker.DEMO.Sex_split"),
    "the column is named after the grouping, not one of the cutoffs", s$name)

half <- run(spl, DS, modifyList(split_inputs, list(split_levels = "female")))
chk(length(half$found$groups) == 1 && half$found$groups[[1]]$label == "female",
    "unticking a group leaves it out of the analysis")

testServer(cutoffFinderServer,
           args = list(ds = reactiveVal(DS), meta = reactive(spl), go_to = NULL), {
  do.call(session$setInputs, split_inputs)
  renders(output$scan_plot,    "a split draws one scan panel per group")
  renders(output$outcome_plot, "and one Kaplan-Meier per group")
  renders(output$result,       "and reports each group separately")
})

# --- expression as the variable ----------------------------------------------
# The bug this guards: expression columns are in file order and metadata rows
# are in spreadsheet order. Reading one as if it were the other silently pairs
# each sample's expression with somebody else's outcome.
#
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
small <- one(run(fx, DS, list(source = "meta", meta_col = "DEMO.Marker",
                              method = "survival", time_col = "DEMO.OS.time",
                              event_col = "DEMO.OS.event", min_frac = 0.1,
                              perm = 0, find = 1)))
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
expected <- as.character(cf_split(as.numeric(big$DEMO.Marker),
                                  saved$groups[[1]]$cutoff))
chk(identical(got, expected), "and it is the split that was shown, sample by sample")

# A split saves one column, but each sample is cut at its own group's
# threshold: the same marker value is "high" for a female and "low" for a male.
testServer(cutoffFinderServer,
           args = list(ds = reactiveVal(DS), meta = reactive(spl), go_to = NULL), {
  do.call(session$setInputs, split_inputs)
  session$setInputs(col_name = "DEMO.Sex.Split", save = 1)
})
sv  <- load_metadata()
sv  <- sv[match(spl$SampleID, sv$SampleID), ]
mkv <- as.numeric(spl$DEMO.Marker)
want <- ifelse(mkv > cuts[spl$DEMO.Sex], "high", "low")
agree <- mean(sv$DEMO.Sex.Split == want, na.rm = TRUE)
chk(agree > 0.9, "a split saves each sample at its own group's cutoff",
    round(agree, 3), " agreement with the planted cutoffs")
mid <- which(mkv > 3.5 & mkv < 6.5)
chk(length(unique(sv$DEMO.Sex.Split[mid])) == 2,
    "so one marker value can be high in one group and low in the other")

cat("\n", n, " checks passed\n", sep = "")
