#!/usr/bin/env Rscript
# Which datasets carry which gene symbol.
#
#   Rscript tools/build-gene-index.R
#
# Answering that by reading the matrices costs about 1.2 GB and 45 seconds --
# GTEX alone is 11 seconds and 505 MB in memory -- so the question tab cannot
# ask it live. This reads only the FIRST COLUMN of each matrix and writes the
# answer to the hub. Run it when a dataset is added; the tab says so itself if
# the index is older than the metadata.

source("global.R")

hub <- os_path("hubdata")
md  <- load_metadata()
out <- file.path(hub, "gene-index.rds")

datasets <- sort(unique(md$dataset))
index <- list()
for (d in datasets) {
  unit <- tryCatch(list_units(d)[1], error = function(e) NA_character_)
  if (is.na(unit)) { cat(sprintf("%-34s no expression file\n", d)); next }
  path <- expression_path(d, unit)
  # cut(1) instead of read.delim: the whole matrix is not wanted, and on the
  # 400 MB files the difference is minutes.
  syms <- tryCatch(
    utils::read.delim(pipe(paste("cut -f1", shQuote(path))), check.names = FALSE,
                      colClasses = "character")[[1]],
    error = function(e) character(0))
  syms <- unique(syms[!is.na(syms) & nzchar(syms)])
  index[[d]] <- syms
  cat(sprintf("%-34s %-6s %6d symbols\n", d, unit, length(syms)))
}

saveRDS(list(built = Sys.time(), source = normalizePath(metadata_path()),
             datasets = index), out)

cat("\nwrote ", out, " (", length(index), " datasets, ",
    length(unique(unlist(index, use.names = FALSE))), " distinct symbols)\n", sep = "")
