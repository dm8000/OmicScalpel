# OmicScalpel

A web interface for exploring the lab's omics datasets — RNAseq, microarray and
lipidomics — and for adding new datasets and sample metadata without touching a
file by hand.

Pick a dataset once, in the sidebar. Every tab that works on a single dataset
follows it.

## The tabs

**Explore**

| tab | what it does |
|---|---|
| Dataset summary | One row per dataset: sample size, tissue, species, which normalized files exist. Sex and age distributions for the datasets you select. The summary sheet is editable, with the derived columns locked. |
| Compare genes | Boxplots of selected genes across the conditions of one dataset. Groups can be reordered and recoloured; labels, fonts and sizes are all adjustable, and Wilcoxon comparisons can be drawn on the plot. |
| Compare samples | The same boxplots resolved per sample rather than per condition, with groups that can be hidden as well as reordered. |
| Correlation | Gene expression against a continuous variable of the dataset, with the same plot controls. |
| Meta-analysis | Forest plot of one biomolecule across several datasets at once, from two groups of samples you define by condition. |

**Export**

| tab | what it does |
|---|---|
| Export matrix | Build an expression matrix: choose the dataset, the normalization unit, the genes and which metadata rows to prepend, optionally log-transform or z-score it, then download it as a tab-separated file. Downloads are logged. |

**Manage**

| tab | what it does |
|---|---|
| Upload dataset | Add a new dataset: its expression files, its row in the dataset summary, and the metadata of its samples. |
| Edit metadata | Edit, create or upload sample metadata for a dataset, column by column. |
| Cutoffs | Define numeric cutoffs on a dataset's continuous columns and see the distribution they split. |

Every save takes a timestamped backup first. **Two people saving the same sheet
at the same time will lose one of the two edits** — see
[docs/ARCHITECTURE.md](docs/ARCHITECTURE.md).

## Running it

Everything the app needs to know about the filesystem is in one file:

    cp config/config.txt.example config/config.txt

    lib     = /n/shiny/OmicScalpel/lib       # R packages
    hubdata = /n/shiny/OmicScalpel/hubdata   # the data
    backups = /n/shiny/OmicScalpel/backups
    logs    = /n/shiny/OmicScalpel/logs

Then:

    Rscript tools/sync-renviron.R    # writes .Renviron from the above
    Rscript tools/check_deps.R       # says which tab a missing package blocks
    R -e 'shiny::runApp(".", port = 8787)'

A relative path resolves against the project directory, which is how you run
against the small fixtures in `data-sample/` instead of the real hub:

    hubdata = data-sample

Deployment on the HPC: [docs/DEPLOY.md](docs/DEPLOY.md).

## For maintainers

- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) — what is shared, how the hub is
  laid out, what is knowingly left unfixed.
- [docs/ADDING_A_MODULE.md](docs/ADDING_A_MODULE.md) — a tab is two functions
  and one row in `R/registry.R`.
- `legacy/` holds the nine apps this replaced, unmodified apart from their
  paths. They still run, and they are the reference when a tab is doubted.
