#!/usr/bin/env Rscript
# Build data-sample/: a small, synthetic stand-in for the hub, safe to version.
#
# It borrows the real spreadsheets' *shape* -- all 192 metadata columns, all 28
# summary columns, the value vocabulary of each column -- and none of their
# identity: dataset names, sample ids and every free-text column are replaced.
# Nothing that could name a subject, a study or a person survives.
#
# Usage: Rscript tools/make-fixtures.R [SOURCE_DIR]   (default: the hub)

suppressMessages({ library(readxl); library(writexl) })

args <- commandArgs(FALSE)
here <- dirname(sub("^--file=", "", grep("^--file=", args, value = TRUE)[1]))
source(file.path(here, "..", "R", "config.R"))

pos <- commandArgs(TRUE)
src <- if (length(pos)) pos[1] else os_path("hubdata")
out <- file.path(os_root(), "data-sample")

N_SAMPLES <- 20L
N_GENES   <- 200L
set.seed(20260924)   # fixtures must be byte-identical on every rebuild

meta <- as.data.frame(read_excel(file.path(src, "Metadata.xlsx"), .name_repair = "minimal"))
summ <- as.data.frame(read_excel(file.path(src, "Datasets_summary.xlsx"), .name_repair = "minimal"))
gene <- as.data.frame(read_excel(file.path(src, "Genemetadata.xlsx"), .name_repair = "minimal"))

# Two datasets of different Data.type, so that "does the tab follow the global
# dataset" and "is an incompatible type refused" both have something to bite on.
pick_one <- function(type) {
  cand <- unique(meta$dataset[meta$`Data.type` %in% type])
  cand <- cand[!is.na(cand)]
  if (!length(cand)) stop("no dataset of Data.type ", type, " in ", src)
  sizes <- vapply(cand, function(d) sum(meta$dataset == d, na.rm = TRUE), integer(1))
  cand[order(abs(sizes - N_SAMPLES))][1]        # closest to the fixture size
}
real <- c(RNAseq = pick_one("RNAseq"), Array = pick_one(c("Array", "Microarray")))
fake <- c(RNAseq = "DEMO_RNAseq", Array = "DEMO_Array")

# Which units each fixture dataset carries. Deliberately different, so a helper
# that ignores its argument and always returns the same list fails the test.
units <- list(DEMO_RNAseq = c("TPM", "count"), DEMO_Array = "TMM")

# Columns that may carry a name, an accession, a lab or a location.
FREE_TEXT <- c("TsengID", "Author", "publication", "Data.location.&.ELN",
               "Description/observation", "Fellow.who.generated/uploaded.dataset",
               "Fellow who generated/uploaded dataset", "Date.sequenced",
               "Date.of.collection", "Folder.Name.in.TsengLab/Datasets", "Project")

anonymize <- function(df, fake_name) {
  for (col in intersect(FREE_TEXT, names(df))) {
    df[[col]] <- ifelse(is.na(df[[col]]), NA_character_, paste0("demo_", col))
  }
  df$dataset <- fake_name
  df
}

meta_out <- do.call(rbind, lapply(names(real), function(k) {
  rows <- meta[which(meta$dataset == real[[k]]), , drop = FALSE]
  rows <- head(rows, N_SAMPLES)
  rows <- anonymize(rows, fake[[k]])
  rows$SampleID <- sprintf("%s_S%02d", fake[[k]], seq_len(nrow(rows)))
  rows
}))
rownames(meta_out) <- NULL

# Give the fixture real numbers. The two datasets that happen to be closest to
# the fixture size carried no numeric measurement at all, which left the cutoff
# and correlation tabs with nothing to work on -- they could not be tested, or
# even demonstrated, against data-sample. These are synthetic values in columns
# that already exist, in plausible ranges.
CONTINUOUS <- list(Age = c(20, 75), BMI = c(18, 45),
                   Adipocyte.size = c(30, 150), Adipocytes.score = c(0, 1))
for (col in intersect(names(CONTINUOUS), names(meta_out))) {
  r <- CONTINUOUS[[col]]
  meta_out[[col]] <- round(runif(nrow(meta_out), r[1], r[2]), 2)
}

summ_out <- do.call(rbind, lapply(names(real), function(k) {
  rows <- summ[which(summ$dataset == real[[k]]), , drop = FALSE]
  if (!nrow(rows)) rows <- summ[1, , drop = FALSE]       # keep the 28 columns
  rows <- anonymize(head(rows, 1), fake[[k]])
  rows$Sample_size <- N_SAMPLES
  rows
}))
rownames(summ_out) <- NULL

# Survival, so the cutoff-finder tests have something to find without the real
# hub. A marker column with a cutpoint planted at 6, the same one
# tools/make-survival-demo.R uses, and a responder flag that follows it.
set.seed(4242)
DEMO_CUT <- 6
marker   <- round(runif(nrow(meta_out), 0, 10), 3)
hazard   <- ifelse(marker > DEMO_CUT, 3, 1)
meta_out$DEMO.Marker    <- marker
meta_out$DEMO.OS.time   <- round(pmax(rexp(nrow(meta_out), hazard / 40), 0.1), 1)
meta_out$DEMO.OS.event  <- rbinom(nrow(meta_out), 1, 0.75)
meta_out$DEMO.Responder <- ifelse(marker > DEMO_CUT, "responder", "non-responder")

dir.create(out, showWarnings = FALSE, recursive = TRUE)
write_xlsx(meta_out, file.path(out, "Metadata.xlsx"))
write_xlsx(summ_out, file.path(out, "Datasets_summary.xlsx"))

symbols <- head(unique(gene$Symbol[!is.na(gene$Symbol)]), N_GENES)

for (ds in names(units)) {
  dir.create(file.path(out, ds), showWarnings = FALSE)
  samples <- meta_out$SampleID[meta_out$dataset == ds]
  for (u in units[[ds]]) {
    m <- matrix(round(rlnorm(length(symbols) * length(samples), 1.5, 1.8), 3),
                nrow = length(symbols), dimnames = list(NULL, samples))
    if (u == "count") m <- round(m * 10)
    df <- cbind(Symbol = symbols, as.data.frame(m, check.names = FALSE))
    write.table(df, file.path(out, ds, paste0(ds, "_", u, ".txt")),
                sep = "\t", quote = FALSE, row.names = FALSE)
  }
}

cat("wrote ", out, "\n", sep = "")
cat("  Metadata.xlsx        ", nrow(meta_out), "rows x", ncol(meta_out), "cols\n")
cat("  Datasets_summary.xlsx", nrow(summ_out), "rows x", ncol(summ_out), "cols\n")
for (ds in names(units)) cat("  ", ds, ": ", paste(units[[ds]], collapse = ", "), "\n", sep = "")
