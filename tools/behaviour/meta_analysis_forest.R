#!/usr/bin/env Rscript
# The forest plot has to find the gene.
#
#   Rscript tools/behaviour/meta_analysis_forest.R
#
# It did not, for months. The legacy app read the matrix with row.names = 1, so
# the symbols were the row names; the conversion moved to load_expression(),
# which keeps them in a Symbol column, and this loop kept
#
#     if (!input$biomolecule %in% rownames(data_info)) next
#
# so every dataset was skipped, `results` stayed empty and the plot rendered as
# nothing. No test opened a session, so nothing said so. This one opens a
# session, asks for a gene that is in the fixture, and checks the effect size
# against one computed here from the same numbers.

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
COND <- "DEMO.Responder"

out <- NULL
testServer(metaAnalysisServer,
           args = list(ds = reactiveVal(DS), meta = reactive(md), go_to = NULL), {
  session$setInputs(biomolecule = GENE, condition = COND,
                    group1_categories = "non-responder",
                    group2_categories = "responder",
                    stat_test = "ttest", data_preference = UNIT,
                    filter_conditions = character(0), adjust_for = character(0),
                    generate_plot = 1)
  out <<- perform_analysis()
})

chk(length(out$results) >= 1, "the analysis finds the gene and returns a dataset",
    "results: ", length(out$results))
chk(DS %in% names(out$results), "and it is the dataset that has the gene",
    paste(names(out$results), collapse = ", "))

r <- out$results[[DS]]

# The same effect size, computed here. log2 because the fixture's TPM has no
# negative values, which is the branch the module takes.
rows <- md[md$dataset == DS, , drop = FALSE]
s1 <- intersect(rows$SampleID[rows[[COND]] == "non-responder"], names(expr))
s2 <- intersect(rows$SampleID[rows[[COND]] == "responder"], names(expr))
v  <- expr[expr$Symbol == GENE, , drop = FALSE]
x1 <- log2(pmax(as.numeric(v[1, s1]), 0.001))
x2 <- log2(pmax(as.numeric(v[1, s2]), 0.001))
pooled <- sqrt(((length(x1) - 1) * var(x1) + (length(x2) - 1) * var(x2)) /
               (length(x1) + length(x2) - 2))
want_d <- (mean(x2) - mean(x1)) / pooled

chk(abs(r$effect_size - want_d) < 1e-8, "the effect size is the one in the data",
    r$effect_size, " vs ", want_d)
chk(r$n_group1 == length(x1) && r$n_group2 == length(x2),
    "with every sample of each group counted",
    r$n_group1, "/", r$n_group2, " vs ", length(x1), "/", length(x2))
chk(abs(r$pvalue - t.test(x2, x1)$p.value) < 1e-8, "and the p of that same test")

# The adjusted path, through a real session. tools/behaviour/meta_analysis_adjust.R
# checks the arithmetic of adjusted_effect() on its own; what could still be
# wrong here is the wiring around it, which is what went unnoticed before.
adj <- NULL
testServer(metaAnalysisServer,
           args = list(ds = reactiveVal(DS), meta = reactive(md), go_to = NULL), {
  session$setInputs(biomolecule = GENE, condition = COND,
                    group1_categories = "non-responder",
                    group2_categories = "responder",
                    stat_test = "ttest", data_preference = UNIT,
                    filter_conditions = character(0), adjust_for = "Age",
                    generate_plot = 1)
  adj <<- perform_analysis()
})
chk(length(adj$results) >= 1, "adjusting for a covariate still produces a result",
    length(adj$results))
ra <- adj$results[[DS]]
chk(identical(ra$adjusted, "Age"), "which reports the covariate it used",
    paste(ra$adjusted, collapse = ", "))
chk(!isTRUE(all.equal(ra$effect_size, r$effect_size)),
    "and is not simply the unadjusted number relabelled",
    ra$effect_size, " vs ", r$effect_size)

# A gene that is in no matrix must come back empty rather than wrong.
none <- NULL
testServer(metaAnalysisServer,
           args = list(ds = reactiveVal(DS), meta = reactive(md), go_to = NULL), {
  session$setInputs(biomolecule = "NOTAGENE999", condition = COND,
                    group1_categories = "non-responder",
                    group2_categories = "responder",
                    stat_test = "ttest", data_preference = UNIT,
                    filter_conditions = character(0), adjust_for = character(0),
                    generate_plot = 1)
  none <<- perform_analysis()
})
chk(length(none$results) == 0, "a gene in no dataset returns nothing, not an error",
    length(none$results))

cat("\n", n, " checks passed\n", sep = "")
