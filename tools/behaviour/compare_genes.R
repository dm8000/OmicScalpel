#!/usr/bin/env Rscript
# Behaviour test for the compare-genes tab: the data behind the plot.
#
# The plot itself is the researcher's to judge. What can be checked here is
# that the numbers going into it are the right ones -- the chosen genes, only
# the samples of the active dataset, and the expression values that are on disk.
#
#   Rscript tools/behaviour/compare_genes.R

source("global.R")

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

meta_df <- load_metadata()
DS      <- "DEMO_RNAseq"
UNIT    <- list_units(DS)[1]
expr    <- load_expression(DS, UNIT)
genes   <- expr[[1]][1:2]
cond    <- "Tissue"

testServer(
  compareGenesServer,
  args = list(ds = reactiveVal(DS), meta = reactive(meta_df), go_to = NULL),
  {
    session$flushReact()
    unit_reactive(UNIT)

    sel   <- meta_df[meta_df$dataset == DS, , drop = FALSE]
    conds <- unique(as.character(sel[[cond]]))

    # the plot path reads several toggles; in a browser they arrive with the
    # page, here the test has to supply them
    session$setInputs(conditions = cond, visible_conditions = conds,
                      genes = genes, log2_transform = FALSE,
                      show_wilcox = FALSE, facet_plot = FALSE, plot = 1)
    session$flushReact()

    d <- plot_data_reactive()
    chk(setequal(unique(d$Gene), genes), "the plot data holds exactly the chosen genes",
        paste(unique(d$Gene), collapse = ", "))
    chk(all(d$SampleID %in% sel$SampleID),
        "and only samples of the active dataset",
        paste(setdiff(unique(d$SampleID), sel$SampleID), collapse = ", "))
    chk(nrow(d) == length(genes) * nrow(sel),
        "one row per gene per sample", nrow(d), " vs ", length(genes) * nrow(sel))

    # values, not shapes
    g1 <- genes[1]
    got <- d$Expression[d$Gene == g1][match(sel$SampleID, d$SampleID[d$Gene == g1])]
    want <- as.numeric(expr[expr[[1]] == g1, sel$SampleID])
    chk(isTRUE(all.equal(got, want)), "the expression values are the ones on disk")
  }
)

cat("\n", n, " checks passed\n", sep = "")
