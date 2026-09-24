#!/usr/bin/env Rscript
# Behaviour test for the export-matrix tab.
#
# smoke_test.R proves a module starts. This proves it computes: the matrix it
# builds has to hold the numbers that are on disk, the unit selector has to
# change which numbers, and the transforms have to transform.
#
#   Rscript tools/behaviour/export_matrix.R

source("global.R")

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

meta_df <- load_metadata()
DS      <- "DEMO_RNAseq"
disk    <- load_expression(DS, "TPM")
genes   <- disk[[1]][1:3]

active <- reactiveVal(DS)

testServer(
  exportMatrixServer,
  args = list(ds = active, meta = reactive(meta_df), go_to = NULL),
  {
    session$setInputs(file_unit = "TPM", genes_list = genes,
                      metadata_fields = character(0),
                      do_log = FALSE, do_zscore = FALSE)

    m <- selected_matrix()
    chk(nrow(m) == 3L, "gene filter keeps exactly the genes asked for", nrow(m))
    chk(setequal(rownames(m), genes), "and keeps the right ones",
        paste(rownames(m), collapse = ", "))

    # values, not shapes: the matrix must be what is on disk
    expected <- as.numeric(disk[disk[[1]] == genes[1], -1])
    chk(isTRUE(all.equal(as.numeric(m[genes[1], ]), expected)),
        "untransformed values match the file on disk")

    # the transform flags have to change the numbers
    session$setInputs(do_log = TRUE)
    lg <- selected_matrix()
    chk(isTRUE(all.equal(as.numeric(lg[genes[1], ]), log2(expected + 0.001))),
        "log2 flag applies log2(x + 0.001)")

    session$setInputs(do_log = FALSE, do_zscore = TRUE)
    z <- selected_matrix()
    chk(abs(mean(as.numeric(z[genes[1], ]))) < 1e-8,
        "z-score flag centres each gene", mean(as.numeric(z[genes[1], ])))

    # the unit selector has to select
    session$setInputs(do_zscore = FALSE, file_unit = "count")
    cnt <- selected_matrix()
    chk(!isTRUE(all.equal(as.numeric(cnt[genes[1], ]), expected)),
        "changing the unit changes the matrix")

    # The shared dataset propagates. This is the whole point of the merge: the
    # tab has no selector of its own, so if ds() did not reach it the tab would
    # quietly keep showing the first dataset forever.
    active("DEMO_Array")
    session$setInputs(file_unit = "TMM", genes_list = character(0))
    session$flushReact()
    other <- load_expression("DEMO_Array", "TMM")
    m2 <- selected_matrix()
    chk(setequal(colnames(m2), names(other)[-1]),
        "switching the shared dataset switches the samples",
        paste(head(colnames(m2), 2), collapse = ", "))
    chk(isTRUE(all.equal(as.numeric(m2[other[[1]][1], ]),
                         as.numeric(other[other[[1]] == other[[1]][1], -1]))),
        "and the values are the other dataset's file")

    active(DS)
    session$setInputs(file_unit = "TPM")
    session$flushReact()

    # metadata rows get prepended, in the order of the samples
    session$setInputs(file_unit = "TPM", metadata_fields = "Tissue")
    comb <- create_combined()
    chk(rownames(comb)[1] == "SampleID" && "Tissue" %in% rownames(comb),
        "combined table carries SampleID and the chosen metadata row",
        paste(head(rownames(comb), 3), collapse = ", "))
  }
)

cat("\n", n, " checks passed\n", sep = "")
