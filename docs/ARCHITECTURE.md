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

**The metadata is a spreadsheet.** 2609 rows and 192 columns in an .xlsx that
the interface rewrites in full on every save. It works, and it is why the
backups exist.
