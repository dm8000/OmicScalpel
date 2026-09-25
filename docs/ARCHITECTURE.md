# Architecture

## Where things live

    app.R          the only entry point. Builds the dashboard, owns the two
                   pieces of shared state, and starts the nine modules.
    global.R       shiny sources this first: config, library path, packages,
                   then R/. It warns about missing packages instead of dying,
                   so a machine with eight of nine tabs' dependencies still runs.
    R/config.R     base R only. It runs before the library path is set, so it
                   cannot use a package.
    R/data_io.R    the only code that reads or writes the hub.
    R/registry.R   what modules exist. app.R, the smoke test and the runner all
                   read it; adding a module means adding a row.
    R/modules/     one file per tab.

## Look and layout

The shell is `fluidPage`, not `dashboardPage`, and the markup is the app's own:
`os_panel()` and `os_layout()` in `R/ui_helpers.R`, styled by
`www/omicscalpel.css`. shinydashboard was dropped after AdminLTE kept painting
the body of a `status="primary"` box white through a literal colour at high
specificity with `!important` and through a pseudo-element underneath -- cache,
CSS variables, rule order and specificity each ruled out by test. Owning the
markup ended it in one move, and the header block and sidebar-collapse button
that used to overlap the left panel went with it.

Every analysis tab is three columns: data and selection on the left, the
analysis in the middle, appearance and export on the right. Which panel goes
where is declared per module in `R/registry.R` and checked by
`tools/lint_layout.R`.

**Plots stay light on purpose.** A figure is the exported artefact; it goes
into a paper. Making the ggplot theme dark would change every downloaded file,
not just the screen.

## One trap, written down because it cost three attempts

**Shiny does not source `global.R` for this app.** Because the project has an
`R/` directory, shiny turns on `shiny.autoload.r`, calls
`loadSupport(globalrenv = NULL)`, auto-sources `R/*.R` (not `R/modules/`, which
is a subdirectory) and skips `global.R` entirely. The app then starts with no
packages attached and dies on the first `dashboardPage()`.

`app.R` therefore sources `global.R` itself, unconditionally. Do not guard that
with `exists("MODULES")`: autoload has already defined it, so the guard is
always true and the source never runs.

Its second consequence is worse and took longer to find: `R/*.R` therefore gets
sourced **twice**, into two different environments. Any state kept in a
file-level environment exists twice, and the two copies drift. That is exactly
what happened to the metadata version counter -- the writer bumped one, the app
watched the other, and an edit in one tab never reached the others, which is
the whole reason the apps were merged. Process-wide state lives in `options()`
now.

The smoke test does not catch any of this, because it sources `global.R`
itself. Only starting the app does. The same applies to `tabItems()`: pass it an *unnamed*
list, or htmltools turns each tab into an escaped HTML attribute and the page
serves 200 with nine empty tabs.

## Shared state

Two things are shared, and they are the reason the nine apps were merged.

**The active dataset.** Chosen once, in the sidebar, held in a `reactiveVal`
and handed to every module as `ds`. Six of the nine modules work on a single
dataset and used to ask for it separately; they now read `ds()` and have no
selector of their own. The other three (dataset summary, meta-analysis, upload)
choose their own sets and ignore `ds`.

**The metadata.** `Metadata.xlsx` is 1.7 MB and used to be read once per app,
per launch. `app.R` reads it once into `shared_meta` and re-reads only when
`metadata_version()` changes. That version is bumped inside `save_metadata()`
and `save_datasets_summary()`, not by the modules: a module cannot forget to
call something it does not have to call.

`go_to(tab, dataset)` is passed down so the dataset-summary tab can send someone
to an analysis with the dataset already selected. It is a callback rather than
the parent session, so a module never learns there is a dashboard around it.

## The hub

    hubdata/
      Metadata.xlsx            one row per sample, 192 columns
      Datasets_summary.xlsx    one row per dataset, 28 columns
      <dataset>/<dataset>_<unit>.txt   expression matrix, tab separated,
                                       first column is the feature name
    backups/                   timestamped copy taken before every overwrite
    logs/downloads.log         one line per exported matrix

Units seen in the wild: TMM, CPM, TPM, FPKM, count, counts, combat,
unknown_unit. The apps disagreed about which of these existed -- five looked for
`_count`, the summary tab for `_counts` and `_combat` -- so `list_units()`
returns the union, known suffixes first, and reports anything else it finds.

Metadata cells use the literal string `"NA"`, not an empty cell; parts of the
app filter on `field != "NA"`. `save_metadata()` writes it back that way.
`load_metadata(na_as_missing = TRUE)` converts it for code that prefers real NA.

## Known and accepted

**Concurrent writes are last-write-wins.** Two researchers saving the metadata
at the same time will lose one of the two edits, and nothing detects it. The
timestamped backups are the mitigation, not a fix. Making this safe means a lock
around read-modify-write (the `filelock` package, or an atomic rename) and is
separate work. It was true of the nine separate apps too; merging them makes it
easier to notice, not more likely.

**`validate()` in this app is jsonlite's, not shiny's.** `global.R` attaches
jsonlite after shiny, so a bare `validate(need(...))` calls the JSON validator.
`need(TRUE, msg)` returns NULL and it errors with "is.character(txt) is not
TRUE"; `need(FALSE, msg)` returns the message, jsonlite says it is not JSON, and
**the render carries on past the guard** -- which is the dangerous half. Every
call is written `shiny::validate(...)` and `tools/verify.sh` fails on a bare one.

**readxl segfaults on Metadata.xlsx now and then.** Twice while this work was
going on, `load_metadata()` brought the whole R process down inside
`readxl::read_excel` with "memory not mapped" -- and the same file read cleanly
twelve times out of twelve immediately afterwards, with and without the app's
packages loaded. It is roughly two failures in fifteen, it predates any of this
code, and a segfault cannot be caught from R, so scripts that must not die
should be run in a shell loop that retries. It deserves its own investigation:
first suspects are the readxl/tibble/vctrs versions in this library.

**The metadata is a spreadsheet.** 2609 rows and 192 columns in an .xlsx that
the interface rewrites in full on every save. It works, and it is why the
backups exist.

## Asking the data a question

Four layers, one of which touches the network.

    R/jev.R          the HTTP client for typesafe.ai; transport is injectable,
                     so everything above it is testable offline and for free
    R/ai_catalog.R   the hub reduced to what a decision needs: per dataset the
                     species, tissue, type, units and which metadata columns
                     are really filled in. About 3,000 tokens for 18 datasets.
    R/ai_genes.R     the symbol vocabulary and the gene -> datasets index
    R/ai_plan.R      the decision tree: typed answers + catalog -> a plan
    R/ai_tools.R     which controls each tab exposes to a plan, and os_ai_gate()

**Jev classifies; it does not generate.** It answers `noul` (is this true),
`choice` (pick one of these, with a probability for each) and `score`
(rate on this rubric) questions about a state, all in one request. So the model
never names a tool or writes code: R hands it the options and reads back typed
answers with confidences, and the tool is chosen by ordinary code that can be
read and tested. `tools/behaviour/ai_plan.R` runs that whole tree with
fabricated answers and no network.

Three rules in `ai_filter_facet()` are not style, they are what this metadata
forces:

- **Silence is not a different answer.** Nine of eighteen datasets record no
  tissue at all. A tissue filter that excluded them threw away most of the
  collection and then truthfully reported finding nothing.
- **An unsure answer excludes nothing.** "Does leptin expression increase with
  BMI" was read as RNAseq at 59% confidence, which dropped a 770-sample
  microarray dataset that had both the gene and the variable. The platform
  filter now needs 80%.
- **A depot is its tissue.** GTEX records "Subcutaneous" and "Visceral", not
  "Adipose". `config/ai-tissues.txt` groups the values into families.

And two about genes: the symbol vocabulary is `Genemetadata.xlsx` plus every
symbol in the index, because that file holds 17,525 symbols while the GTEX
matrix alone has 54,593; and matching is case-insensitive, because human
matrices spell it `UCP1` and mouse ones `Ucp1`.

`docs/JEV-COST.md` is measured, not estimated: about $0.0005 a question.

## Expression values

**One decimal, except below 0.1.** The matrices carried up to 18 decimal places
-- `4945.053081999999` is the binary remainder of a float written as text, not
precision. `os_round_expression()` in `R/data_io.R` is the rule and
`tools/round-matrices.R` applied it to all 29 matrices, taking the hub from
1.6 GB to 1.2 GB.

A flat `round(x, 1)` is not safe on TPM: 4.5% of Rozen.BMI's non-zero values
are below 0.05 and 330 of its genes would have become all zero, which reads as
"not expressed" rather than "barely expressed". Below 0.1 the value keeps three
significant digits instead. The upload tab applies the same rule through the
same function, so the two entry points cannot drift.

**One row, not the matrix.** `load_expression_row()` greps the rows it needs
and checks the symbol that comes back is the one asked for. Measured on
GTEX_adipose_TMM.txt (402 MB):

| | before | after |
|---|---|---|
| forest plot, LEP against BMI | 35 s | 1.2 s |
| filling a gene menu | 14 s, 530 MB | 0.08 s |
| one gene's row | 14.1 s | 0.02 s |

No byte-offset index: an index that goes stale returns a different gene's
numbers and says nothing, which is worse than being slow.

**Parallelism was not the answer.** Sixteen cores did sit idle while one worked,
but the cause was reading whole matrices single-threaded. With the row read,
eighteen datasets take 0.38 s to read and spreading that over eight cores saved
0.13 s, so the fork went back out. Measured, not assumed.

## Survival is not another continuous column

The meta-analysis offers every metadata column as the condition, and a
follow-up time is a number, so `DEMO.OS.time` used to be split at its median
and compared by t-test. That puts an early death and someone who left the study
in the same group, and draws a forest plot that looks exactly like a real one.

`os_survival_event_for()` in `R/meta_stats.R` finds the censoring flag that
goes with a time column -- by name first, but by its values in the end: the
partner has to be 0/1. When there is one, the tab fits `coxph` per dataset and
pools hazard ratios instead (`sm = "HR"`, and the labels change with it).

The expression is standardised within each dataset before the model, so the
effect is **per standard deviation**. One study reports TPM and the next TMM; a
hazard ratio per unit of expression is a different quantity in each, and
pooling them would be arithmetic on incomparable numbers.

A Cox model needs samples and events, so cohorts get dropped -- and a forest
plot with one row and no explanation looks like a bug, so the summary names
each cohort that could not be fitted and why.

## A plan describes a whole screen

`os_ai_state()` fills in every control the target tab declares a default for
and the plan does not mention. Without it the tab kept whatever the researcher
had left there: a question answered after someone had split Across datasets by
sex came back still split by sex, because the plan never mentioned `split_col`
so nobody cleared it. Controls with no declared default are left alone --
`numeric_columns` has no sensible empty value, and the plans that use it always
set it.

## Roadmap

**Inline help on the other tabs.** `os_help()` and `os_field()` in
`R/ui_helpers.R` put a `?` at the right edge of a control's label line and show
a box on hover and on keyboard focus (CSS only, no tooltip library). Only the
Cutoff finder uses them so far, because that is the tab where a wrong choice
produces a plausible-looking wrong number. The rest of the tabs should follow,
one at a time: the help text is the expensive part, not the markup.

Two rules learned writing the first set. The mark goes next to the control it
explains, never in the panel title -- `lint_layout.R` reads panel titles as
text and a mark inside one becomes part of the title. And the text says what
the choice *does to the result*, not what the widget is: "the unit becomes part
of the saved column's name" is worth reading, "select the normalization unit"
is not.
