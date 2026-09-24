# R/registry.R -- what modules exist. The single source of truth.
#
# app.R builds its menu from this, tools/smoke_test.R walks it, and
# tools/run_module.R resolves a name through it. Adding a module means adding a
# row here and nothing else.
#
# scope:
#   "one"  -- works on a single dataset, and therefore reads the shared ds()
#             instead of owning a selector. lint_ns.R is run with
#             --single-dataset over these, so a surviving input$dataset fails.
#   "many" -- decides its own set of datasets; ds is passed but ignored.

MODULES <- list(
  list(id = "data_summary",         scope = "many", group = "Explore",
       title = "Dataset summary",   icon = "table",
       ui = "dataSummaryUI",        server = "dataSummaryServer",
       pkgs = c("datamods", "dplyr", "DT", "forcats", "ggplot2", "openxlsx", "plotly", "RColorBrewer")),

  list(id = "compare_genes",        scope = "one",  group = "Explore",
       title = "Compare genes",     icon = "chart-bar",
       ui = "compareGenesUI",       server = "compareGenesServer",
       pkgs = c("colourpicker", "dplyr", "ggbreak", "ggplot2", "ggpubr", "jsonlite", "patchwork", "purrr", "RColorBrewer", "rlang", "sortable", "svglite")),

  list(id = "compare_samples",      scope = "one",  group = "Explore",
       title = "Compare samples",   icon = "vials",
       ui = "compareSamplesUI",     server = "compareSamplesServer",
       pkgs = c("colourpicker", "dplyr", "ggbreak", "ggplot2", "ggpubr", "jsonlite", "patchwork", "purrr", "RColorBrewer", "rlang", "sortable", "svglite")),

  list(id = "correlation_analysis", scope = "one",  group = "Explore",
       title = "Correlation",       icon = "chart-line",
       ui = "correlationUI",        server = "correlationServer",
       pkgs = c("colourpicker", "dplyr", "ggplot2", "ggpubr", "jsonlite", "RColorBrewer", "rlang", "sortable", "svglite", "viridis")),

  list(id = "meta_analysis",        scope = "many", group = "Explore",
       title = "Meta-analysis",     icon = "tree",
       ui = "metaAnalysisUI",       server = "metaAnalysisServer",
       pkgs = c("cowplot", "dplyr", "DT", "ggplot2", "grid", "gridExtra", "meta")),

  list(id = "export_matrix",        scope = "one",  group = "Export",
       title = "Export matrix",     icon = "file-export",
       ui = "exportMatrixUI",       server = "exportMatrixServer",
       pkgs = c("dplyr", "DT", "tidyr")),

  list(id = "upload_dataset",       scope = "many", group = "Manage",
       title = "Upload dataset",    icon = "upload",
       ui = "uploadDatasetUI",      server = "uploadDatasetServer",
       pkgs = c("datamods", "DT", "magrittr")),

  list(id = "metadata_editor",      scope = "one",  group = "Manage",
       title = "Edit metadata",     icon = "pen-to-square",
       ui = "metadataEditorUI",     server = "metadataEditorServer",
       pkgs = c("datamods", "DT")),

  list(id = "cutoff_maker",         scope = "one",  group = "Manage",
       title = "Cutoffs",           icon = "sliders",
       ui = "cutoffMakerUI",        server = "cutoffMakerServer",
       pkgs = c("dplyr", "ggplot2", "plotly", "rhandsontable", "shinyWidgets"))
)
names(MODULES) <- vapply(MODULES, function(m) m$id, character(1))

# Where each module's source lives, and which legacy app it came from.
module_file <- function(m) file.path("R", "modules", paste0("mod_", m$id, ".R"))
legacy_dir  <- function(m) gsub("_", "-", m$id)

module_groups <- function() unique(vapply(MODULES, function(m) m$group, character(1)))
