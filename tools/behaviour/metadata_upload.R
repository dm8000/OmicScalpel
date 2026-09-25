#!/usr/bin/env Rscript
# Uploading metadata must not blank the columns the file leaves out.
#
#   Rscript tools/behaviour/metadata_upload.R
#
# The tab tells the user "you can add your own classifiers (treatment, etc.)".
# A file with the four required columns and one new one is exactly what that
# invites, and the merge used to replace the whole row -- writing "NA" into
# every other column of those samples.

source("global.R")
source("tools/behaviour/_fixture.R")
tmp <- use_fixture_hub(writable = TRUE)

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

before <- load_metadata()
DS     <- before$dataset[1]
SID    <- before$SampleID[1]

# what a researcher following the instructions would send: the four required
# columns and the one they want to add
up <- data.frame(
  SampleID    = SID,
  dataset     = DS,
  Data.type   = before$`Data.type`[1],
  Author      = before$Author[1],
  DEMO.OS.time = "42",
  check.names = FALSE, stringsAsFactors = FALSE
)
path <- file.path(tmp, "add_one_column.txt")
write.table(up, path, sep = "\t", quote = FALSE, row.names = FALSE)

testServer(
  metadataEditorServer,
  args = list(ds = reactiveVal(DS), meta = reactive(before), go_to = NULL),
  {
    session$flushReact()
    session$setInputs(upload_file = list(name = basename(path), datapath = path))
    session$flushReact()
  }
)

after <- load_metadata()
row_b <- before[before$SampleID == SID, , drop = FALSE]
row_a <- after[after$SampleID == SID, , drop = FALSE]

chk(nrow(row_a) == 1, "the sample is still there exactly once", nrow(row_a))
chk(nrow(after) == nrow(before), "no row was added or lost",
    nrow(after), " vs ", nrow(before))
chk("DEMO.OS.time" %in% names(after), "the new column arrived")
chk(identical(as.character(row_a$DEMO.OS.time), "42"), "with the uploaded value",
    row_a$DEMO.OS.time)

# the point of the test: everything the file did not mention must survive
untouched <- setdiff(names(row_b), c("DEMO.OS.time", "LABEID", "TsengID"))
lost <- untouched[vapply(untouched, function(cn) {
  !identical(as.character(row_b[[cn]]), as.character(row_a[[cn]]))
}, logical(1))]
chk(length(lost) == 0,
    "every column the file left out is unchanged",
    length(lost), " changed, e.g. ", paste(head(lost, 4), collapse = ", "))

# and the other samples are untouched too
# compared by content: subsetting renumbers row names, and identical() on a
# data frame compares those too
strip <- function(d) { d <- d[order(d$SampleID), , drop = FALSE]
                       rownames(d) <- NULL
                       as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE) }
others_b <- strip(before[before$SampleID != SID, names(before), drop = FALSE])
others_a <- strip(after[after$SampleID != SID, names(before), drop = FALSE])
chk(identical(others_b, others_a), "and so is every other sample",
    sum(vapply(seq_along(others_b), function(j)
      !identical(others_b[[j]], others_a[[j]]), logical(1))), " columns differ")

unlink(tmp, recursive = TRUE)
cat("\n", n, " checks passed\n", sep = "")
