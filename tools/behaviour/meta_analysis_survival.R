#!/usr/bin/env Rscript
# Censored survival is not another continuous column.
#
#   Rscript tools/behaviour/meta_analysis_survival.R
#
# The meta-analysis offers every metadata column as the condition, and a
# follow-up time is numeric, so it used to be split at its median and compared
# by t-test. That puts an early death and someone who left the study in the
# same group, and draws a forest plot that looks exactly like a real one. The
# check is that a time column with a censoring flag takes a Cox model instead,
# and that the numbers are the ones coxph gives.

source("global.R")
source("tools/behaviour/_fixture.R")
use_fixture_hub()

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

md   <- load_metadata()
DS   <- "DEMO_RNAseq"
UNIT <- "TPM"
expr <- load_expression(DS, UNIT)
GENE <- expr$Symbol[3]

# --- finding the pair ---------------------------------------------------------

chk(identical(os_survival_event_for("DEMO.OS.time", md), "DEMO.OS.event"),
    "a follow-up time finds the flag that goes with it",
    os_survival_event_for("DEMO.OS.time", md))
chk(is.null(os_survival_event_for("Age", md)),
    "an ordinary number is not mistaken for one")
chk(is.null(os_survival_event_for("DEMO.OS.time", md[, c("SampleID", "DEMO.OS.time")])),
    "and a time with no flag beside it is not either")

# A column whose partner is not 0/1 is not a censoring flag.
fake <- md
fake$Trial.time <- "5"
fake$Trial.grade <- "high"
chk(is.null(os_survival_event_for("Trial.time", fake)),
    "a partner that is not zero-or-one is rejected")

# --- the catalog has to see it too -------------------------------------------
# The censoring flag is 0/1, which ai_kind() calls neither numeric (two
# distinct values) nor categorical (they parse as numbers), so it never reaches
# the catalog's list of variables. A detector that looked there concluded the
# dataset had no survival data, and a question about it was refused.
ct <- ai_catalog(md)
sv <- ct[[DS]]$survival
chk(!is.null(sv) && identical(sv$time, "DEMO.OS.time") &&
    identical(sv$event, "DEMO.OS.event"),
    "the catalog finds the follow-up pair from the values, not the column list",
    if (is.null(sv)) "(none)" else paste(sv$time, sv$event))
chk(!"DEMO.OS.event" %in% names(ct[[DS]]$variables),
    "-- and the flag really is absent from that list, which is why it must")
chk(identical(ai_survival_columns(ct[[DS]]), sv),
    "the planner reads the pair the catalog worked out")

# --- the analysis takes the Cox path -----------------------------------------

run <- function(condition) {
  out <- NULL
  testServer(metaAnalysisServer,
             args = list(ds = reactiveVal(DS), meta = reactive(md), go_to = NULL), {
    session$setInputs(biomolecule = GENE, condition = condition,
                      data_preference = UNIT, stat_test = "ttest",
                      numeric_split = "median",
                      filter_conditions = character(0), adjust_for = character(0),
                      generate_plot = 1)
    out <<- list(res = perform_analysis(), pair = survival_pair(),
                 plot = tryCatch(output$forest_plot, error = function(e) conditionMessage(e)))
  })
  out
}

s <- run("DEMO.OS.time")
chk(identical(s$res$kind, "survival"), "a time column runs a survival analysis",
    s$res$kind %||% "(none)")
chk(length(s$res$results) >= 1, "with at least one dataset in it",
    length(s$res$results))
chk(!is.character(s$plot) && !is.null(s$plot), "and the forest plot renders",
    if (is.character(s$plot)) s$plot else "nothing rendered")

r <- s$res$results[[DS]]
chk(identical(r$kind, "survival"), "each result says what it is")
chk(r$n_group2 <= r$n_group1,
    "and reports events, not a second group, beside the sample count",
    r$n_group1, "/", r$n_group2)

# --- the numbers are coxph's --------------------------------------------------

rows    <- md[md$dataset == DS, , drop = FALSE]
samples <- intersect(rows$SampleID, names(expr))
e       <- as.numeric(expr[expr$Symbol == GENE, samples])
idx     <- match(samples, rows$SampleID)
time    <- as.numeric(rows$DEMO.OS.time[idx])
event   <- as.numeric(rows$DEMO.OS.event[idx])
# the module log-transforms when the matrix holds no negatives, as the group
# path does
e <- log2(pmax(e, 0.001))
fit  <- survival::coxph(survival::Surv(time, event) ~ scale(e))
want <- unname(coef(fit))

chk(abs(r$effect_size - want) < 1e-8,
    "the effect is the log hazard ratio coxph gives, per SD of expression",
    r$effect_size, " vs ", want)
chk(abs(r$se - unname(summary(fit)$coefficients[1, "se(coef)"])) < 1e-8,
    "and so is its standard error")
chk(r$n_group2 == sum(event == 1), "the event count is the events",
    r$n_group2, " vs ", sum(event == 1))

# Standardising matters: one dataset reports TPM and the next TMM, so a hazard
# ratio per unit of expression is a different quantity in each.
raw <- survival::coxph(survival::Surv(time, event) ~ e)
chk(abs(r$effect_size - unname(coef(raw))) > 1e-6,
    "the effect is per standard deviation, not per raw unit",
    r$effect_size, " vs ", unname(coef(raw)))

# --- an ordinary numeric column is untouched ---------------------------------

a <- run("Age")
chk(is.null(a$pair), "an ordinary number is not treated as survival")
chk(!identical(a$res$kind, "survival"),
    "and still goes through the group split", a$res$kind %||% "groups")
chk(length(a$res$results) >= 1, "which still produces a result",
    length(a$res$results))

cat("\n", n, " checks passed\n", sep = "")
