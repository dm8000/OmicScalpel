# OmicScalpel

A web interface for exploring the lab's omics datasets — RNAseq, microarray and
lipidomics — and for adding new datasets and sample metadata without touching a
file by hand.

Pick a dataset once, in the sidebar. Every tab that works on a single dataset
follows it.

## The tabs

**Ask**

| tab | what it does |
|---|---|
| Ask the data | Ask a scientific question in words. The tab classifies it with Jev (typesafe.ai) into typed answers -- organism, tissue, variable, gene, what kind of comparison -- then a decision tree in `R/ai_plan.R` filters the catalog, checks the gene is really in the matrices, picks the tool that fits and fills in its controls. The answer is a button that opens the finished plot in that tab, plus the search path it took. No model writes code or chooses a tool. A question that nothing can answer comes back with the criteria it searched on and where the last dataset was lost. |

**Explore**

| tab | what it does |
|---|---|
| Dataset summary | One row per dataset: sample size, tissue, species, which normalized files exist. Sex and age distributions for the datasets you select. The summary sheet is editable, with the derived columns locked. Select a row and one of the buttons underneath to open that dataset straight in an analysis tab; a dataset with no expression matrix says so rather than opening a tab that cannot work. |
| Compare genes | Boxplots with the **genes on the x axis and one panel per condition** — for asking how several genes compare within each group. Groups can be reordered and recoloured; labels, fonts and sizes are adjustable, and Wilcoxon comparisons can be drawn on the plot. |
| Compare samples | The transpose: **conditions on the x axis, one panel per gene** — for asking how one gene differs between groups. Same plot controls, plus groups that can be hidden as well as reordered. |
| Correlation | Gene expression against a continuous variable of the dataset, with the same plot controls. |
| Across datasets | One gene's expression in each dataset separately, with housekeeping genes beside it as a ruler for abundance. Deliberately not a comparison: the batch effect between studies is larger than most biological differences, so each panel names its own unit and nothing is pooled. Reads only the rows for the genes asked for, which is what makes several datasets at once affordable. |
| Meta-analysis | Forest plot of one biomolecule across several datasets at once, from two groups of samples you define by condition. |
| Cutoff finder | Let the data choose the cutpoint on a metadata column or a gene's expression: the split with the strongest log-rank separation in survival, the best 2x2 or ROC corner for a binary outcome, or the crossing of a two-component mixture. Shows the effect across every candidate cutpoint, and a permutation-corrected p beside the optimistic one. The analysis can be split by any categorical column -- sex, treatment, batch -- and then each group gets its own cutoff, its own curves and its own p. Controls carry a `?` with an explanation on hover. Distilled from Cutoff Finder (Budczies et al., PLoS ONE 2012;7:e51862). |

**Export**

| tab | what it does |
|---|---|
| Export matrix | Build an expression matrix: choose the dataset, the normalization unit, the genes and which metadata rows to prepend, optionally log-transform or z-score it, then download it as a tab-separated file. Downloads are logged. |

**Manage**

| tab | what it does |
|---|---|
| Upload dataset | Add a new dataset: its expression files, its row in the dataset summary, and the metadata of its samples. |
| Edit metadata | Edit, create or upload sample metadata for a dataset, column by column. |
| Manual cutoffs | Define numeric cutoffs on a dataset's continuous columns by hand and see the distribution they split. The Cutoff finder tab is the other direction: there the data picks the cutpoint. |

The **Re-read metadata** button in the sidebar is only needed when the
spreadsheet was changed outside the app -- saving from a tab already refreshes
the others.

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
