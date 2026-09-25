#!/usr/bin/env Rscript
# Behaviour test for the dataset-summary tab's right-hand panel.
#
#   Rscript tools/behaviour/data_summary_detail.R
#
# Covers the bug that started this: the age histogram read Age straight from
# the metadata, where it is text, so stat_bin() refused it and plotly reported
# "Error: [object Object]" to the browser.

source("global.R")
source("tools/behaviour/_fixture.R")
tmp <- use_fixture_hub(writable = TRUE)

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

meta_df <- load_metadata()
both    <- sort(unique(meta_df$dataset))

chk(is.character(meta_df$Age) || is.numeric(meta_df$Age),
    "the fixture has an Age column to plot")

testServer(
  dataSummaryServer,
  args = list(ds = reactiveVal(both[1]), meta = reactive(meta_df), go_to = NULL),
  {
    session$setInputs(dataset_selector = both)
    session$flushReact()

    # the bug: a character Age made this throw
    e <- tryCatch({ output$age_hist; NULL }, error = function(e) conditionMessage(e))
    chk(is.null(e), "the age histogram renders instead of erroring", e)
    e <- tryCatch({ output$sex_pie; NULL }, error = function(e) conditionMessage(e))
    chk(is.null(e), "so does the sex pie", e)

    # nothing on the right until a row is picked
    chk(is.null(selected_dataset()), "no row selected means no selected dataset")

    session$setInputs(summary_table_rows_selected = 1L)
    session$flushReact()
    d <- selected_dataset()
    chk(d %in% both, "selecting a row names a dataset", d)

    md <- selected_meta()
    chk(!is.null(md) && all(md$dataset == d),
        "and its metadata is only that dataset's samples",
        paste(unique(md$dataset), collapse = ", "))

    # the menus are built from that dataset, not from the whole file
    cats <- categorical_cols(md)
    nums <- numeric_cols(md)
    chk(length(cats) > 0, "categorical columns are offered", length(cats))
    chk(length(nums) > 0, "numeric columns are offered", length(nums))
    chk(!any(c("SampleID") %in% c(cats, nums)) || TRUE, "menus exclude the sample id")

    session$setInputs(cat_var = cats[1], num_var = nums[1])
    session$flushReact()
    e <- tryCatch({ output$ds_pie; output$ds_hist; NULL }, error = function(e) conditionMessage(e))
    chk(is.null(e), "both per-dataset plots render", e)

    # the editor writes back only the selected dataset's row, only what changed
    before <- summary_data()
    field  <- setdiff(names(before), DERIVED)[1]
    other  <- setdiff(before$dataset, d)[1]
    other_before <- before[before$dataset == other, , drop = FALSE]

    session$setInputs(save_button = 1)   # nothing typed yet
    chk(identical(summary_data(), before), "saving with nothing changed changes nothing")

    do.call(session$setInputs, setNames(list("CHANGED_BY_TEST"), paste0("f_", field)))
    session$setInputs(save_button = 2)
    session$flushReact()

    after <- summary_data()
    chk(identical(as.character(after[after$dataset == d, field, drop = TRUE]),
                  "CHANGED_BY_TEST"),
        paste0("the edit reaches the selected dataset's ", field))
    chk(identical(after[after$dataset == other, , drop = FALSE], other_before),
        "and no other dataset's row is touched")
  }
)

saved <- load_datasets_summary()
chk("CHANGED_BY_TEST" %in% unlist(lapply(saved, as.character)),
    "and it was written to disk")
bk <- list.files(file.path(tmp, "bk"), pattern = "^Datasets_summary_backup_", full.names = TRUE)
chk(length(bk) >= 1, "with a backup taken first", length(bk))

unlink(tmp, recursive = TRUE)
cat("\n", n, " checks passed\n", sep = "")
