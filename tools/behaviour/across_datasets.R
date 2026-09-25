#!/usr/bin/env Rscript
# The tab that shows a gene in several datasets without comparing them.
#
#   Rscript tools/behaviour/across_datasets.R
#
# Two things could be wrong and look fine: it could read more than it needs,
# which is the whole reason the tab is affordable, and it could quietly drop a
# dataset that does not carry the gene instead of saying so.

source("global.R")
source("tools/behaviour/_fixture.R")
use_fixture_hub()

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

md   <- load_metadata()
GENE <- load_expression("DEMO_RNAseq", "TPM")$Symbol[7]
HK   <- "GAPDH"

run <- function(inputs) {
  out <- NULL
  testServer(acrossDatasetsServer,
             args = list(id = "across_datasets", ds = reactiveVal("DEMO_RNAseq"),
                         meta = reactive(md), go_to = NULL, ai = NULL), {
    do.call(session$setInputs, inputs)
    session$flushReact()
    out <<- list(g = tryCatch(gathered(), error = function(e) conditionMessage(e)),
                 panels = tryCatch(output$panels, error = function(e) conditionMessage(e)),
                 caveat = tryCatch(output$caveat, error = function(e) conditionMessage(e)))
  })
  out
}

both <- c("DEMO_RNAseq", "DEMO_Array")

r <- run(list(genes = GENE, datasets = both, housekeeping = TRUE,
              log_y = TRUE, split_col = "", plot = 1))
chk(!is.character(r$g) && !is.null(r$g$long), "it gathers something", r$g)
long <- r$g$long

chk(setequal(unique(long$dataset), both), "one dataset's rows for each dataset asked for",
    paste(unique(long$dataset), collapse = ", "))
chk(GENE %in% long$gene, "the gene asked for is there")
chk(HK %in% long$gene, "and the housekeeping ruler beside it")

# The point of the tab: it reads the rows, not the matrices.
full <- load_expression("DEMO_RNAseq", "TPM")
chk(length(unique(long$gene)) <= length(r$g$asked),
    "and nothing beyond the genes asked for",
    paste(setdiff(unique(long$gene), r$g$asked), collapse = ", "))
chk(nrow(full) > 200 && length(unique(long$gene)) < 10,
    "-- the matrix has hundreds of genes and only a handful were read",
    length(unique(long$gene)), " of ", nrow(full))

# The values are the ones in the file.
v_row <- long[long$dataset == "DEMO_RNAseq" & long$gene == GENE, ]
v_full <- as.numeric(full[full$Symbol == GENE, v_row$sample])
chk(isTRUE(all.equal(v_row$value, v_full)), "with the values the full read gives")

chk(!is.character(r$panels) && !is.null(r$panels),
    "the panels render", if (is.character(r$panels)) r$panels else "nothing rendered")

# Every unit is named, because they are not the same unit.
units <- unique(long$unit)
chk(length(units) == 2 && setequal(units, c("TPM", "TMM")),
    "each dataset carries its own unit, and they differ",
    paste(units, collapse = ", "))

html <- paste(as.character(r$caveat), collapse = " ")
chk(grepl("not comparable", html, fixed = TRUE),
    "the answer says in words that the panels are not comparable")

# --- splitting within a dataset ----------------------------------------------

rs <- run(list(genes = GENE, datasets = both, housekeeping = FALSE, log_y = TRUE,
               split_col = "DEMO.Responder", plot = 1))
groups <- unique(rs$g$long$group)
chk(length(groups) >= 2 && all(groups %in% c("responder", "non-responder")),
    "a split divides the samples inside each dataset",
    paste(groups, collapse = ", "))
for (d in both) {
  g <- unique(rs$g$long$group[rs$g$long$dataset == d])
  if (length(g) < 2) no("every dataset is split, not just the first", d)
}
ok("and does so in each of them")

# --- a dataset that does not carry the gene ----------------------------------

r2 <- run(list(genes = "NOTAGENE999", datasets = both, housekeeping = FALSE,
               log_y = TRUE, split_col = "", plot = 1))
chk(is.null(r2$g$long) || !nrow(r2$g$long), "a gene nobody has gathers nothing")
notes <- r2$g$notes
chk(length(notes) == length(both) &&
    all(vapply(notes, function(nt) length(nt$found) == 0, logical(1))),
    "and every dataset is still reported, with nothing found",
    length(notes))
chk(is.character(r2$panels) || grepl("None of the datasets",
                                     paste(as.character(r2$panels), collapse = " ")),
    "the panel says so instead of drawing an empty box")

cat("\n", n, " checks passed\n", sep = "")
