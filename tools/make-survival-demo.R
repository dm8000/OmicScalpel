#!/usr/bin/env Rscript
# Build the synthetic survival data the Cutoff Finder tab is tested against.
#
#   Rscript tools/make-survival-demo.R [--fixtures]
#
# Everything it writes is invented. Columns carry a DEMO. prefix and the new
# dataset is called Example3_survival, so nothing here can be mistaken for a
# measurement.
#
# It follows the app's own rules rather than editing the spreadsheet: a matrix
# at hubdata/<ds>/<ds>_<unit>.txt whose first column is Symbol, metadata rows
# carrying the four required columns (SampleID, dataset, Data.type, Author),
# missing values written as the literal "NA", and the merge done through
# save_metadata(), which takes a backup.

suppressMessages({ library(dplyr) })
args <- commandArgs(FALSE)
here <- dirname(sub("^--file=", "", grep("^--file=", args, value = TRUE)[1]))
setwd(normalizePath(file.path(here, "..")))
source("global.R")

pos      <- commandArgs(TRUE)
fixtures <- "--fixtures" %in% pos

set.seed(20260925)

N_NEW      <- 150L
N_GENES    <- 2000L
MARKER     <- "DEMOMARK1"
TRUE_CUT   <- 6          # expression above this has the worse survival
HAZARD_HI  <- 3          # threefold, so the effect is unmistakable
MEDIAN_OS  <- 40         # months in the low group

# --- survival for a set of samples -------------------------------------------
# A marker value per sample, then a survival time whose rate depends on which
# side of TRUE_CUT it falls. Censoring is independent of everything.
demo_survival <- function(marker) {
  hazard <- ifelse(marker > TRUE_CUT, HAZARD_HI, 1)
  time   <- round(rexp(length(marker), rate = hazard / MEDIAN_OS), 1)
  time   <- pmax(time, 0.1)
  event  <- rbinom(length(marker), 1, 0.75)
  list(time = time, event = event)
}

# --- the new dataset ---------------------------------------------------------
build_dataset <- function(hub) {
  ds   <- "Example3_survival"
  dir.create(file.path(hub, ds), showWarnings = FALSE, recursive = TRUE)

  ids    <- sprintf("%s_S%03d", ds, seq_len(N_NEW))
  marker <- round(runif(N_NEW, 0, 10), 3)
  surv   <- demo_survival(marker)

  genes <- c(MARKER, sprintf("DEMOGENE%04d", seq_len(N_GENES - 1)))
  mat   <- matrix(round(rlnorm(length(genes) * N_NEW, 1.5, 1.6), 3),
                  nrow = length(genes), dimnames = list(NULL, ids))
  mat[1, ] <- marker                       # the planted marker, as expression
  expr <- cbind(Symbol = genes, as.data.frame(mat, check.names = FALSE))
  write.table(expr, file.path(hub, ds, paste0(ds, "_TPM.txt")),
              sep = "\t", quote = FALSE, row.names = FALSE)

  # a responder flag that follows the same cutpoint, for the binary method
  responder <- ifelse(marker > TRUE_CUT, "responder", "non-responder")
  flip <- sample.int(N_NEW, round(N_NEW * 0.15))
  responder[flip] <- ifelse(responder[flip] == "responder", "non-responder", "responder")

  data.frame(
    SampleID        = ids,
    dataset         = ds,
    Data.type       = "RNAseq",
    Author          = "synthetic",
    Species         = "Mmu",
    Tissue          = "BAT",
    Cell.type       = "brown.adipocytes",
    DEMO.Marker     = marker,
    DEMO.OS.time    = surv$time,
    DEMO.OS.event   = surv$event,
    DEMO.Responder  = responder,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# --- survival for samples that already exist ---------------------------------
# No marker to key off, so the times are drawn flat: these two datasets are
# there to see the tab refuse politely at n = 6 and n = 13, not to find
# anything.
extend_existing <- function(md, datasets) {
  out <- lapply(datasets, function(ds) {
    rows <- md[md$dataset == ds, , drop = FALSE]
    if (!nrow(rows)) return(NULL)
    marker <- round(runif(nrow(rows), 0, 10), 3)
    s <- demo_survival(marker)
    data.frame(
      SampleID      = rows$SampleID,
      dataset       = ds,
      Data.type     = rows$`Data.type`,
      Author        = rows$Author,
      DEMO.Marker   = marker,
      DEMO.OS.time  = s$time,
      DEMO.OS.event = s$event,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), out))
}

# --- merge, the way the app does ---------------------------------------------
merge_rows <- function(md, new_rows) {
  for (col in setdiff(names(new_rows), names(md))) md[[col]] <- "NA"
  for (i in seq_len(nrow(new_rows))) {
    idx <- which(md$SampleID == new_rows$SampleID[i] &
                 md$dataset  == new_rows$dataset[i])
    if (length(idx)) {
      # only the columns this row carries, exactly as the upload tab now does
      for (col in names(new_rows)) md[idx[1], col] <- as.character(new_rows[i, col])
    } else {
      row <- md[1, , drop = FALSE]
      row[] <- "NA"
      for (col in names(new_rows)) row[[col]] <- as.character(new_rows[i, col])
      md <- rbind(md, row)
    }
  }
  md
}

hub <- os_path("hubdata")
cat("hub: ", hub, "\n", sep = "")

before   <- load_metadata()
new_rows <- build_dataset(hub)
existing <- extend_existing(before, c("Example_dataset", "Example2_dataset"))

after <- merge_rows(before, rbind(
  new_rows[, intersect(names(new_rows), union(names(before), names(new_rows)))],
  { e <- existing
    for (col in setdiff(names(new_rows), names(e))) e[[col]] <- "NA"
    e[, names(new_rows)] }
))

save_metadata(before, after)

cat("Example3_survival: ", N_NEW, " samples, ", N_GENES, " genes, marker ",
    MARKER, " cut at ", TRUE_CUT, "\n", sep = "")
cat("survival added to: ", paste(unique(existing$dataset), collapse = ", "), "\n", sep = "")
cat("rows: ", nrow(before), " -> ", nrow(after), "\n", sep = "")
cat("\nEverything above is invented. Columns are prefixed DEMO. and the new\n")
cat("dataset is named Example3_survival.\n")
