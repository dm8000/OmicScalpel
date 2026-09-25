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
  list(id = "ai_chat",              scope = "many", group = "Ask",
       title = "Ask the data",      icon = "comments",
       ui = "aiChatUI",             server = "aiChatServer",
       pkgs = c("httr", "jsonlite"),
       layout = list(left   = c("Scope"),
                     center = c("Conversation"),
                     right  = c("Cost"))),

  list(id = "data_summary",         scope = "many", group = "Explore",
       title = "Dataset summary",   icon = "table",
       ui = "dataSummaryUI",        server = "dataSummaryServer",
       pkgs = c("datamods", "dplyr", "DT", "forcats", "ggplot2", "openxlsx", "plotly", "RColorBrewer"),
       layout = list(left   = c("Selection", "Sex Distribution", "Age Distribution"),
                     center = c("Dataset Summary"),
                     right  = c("Dataset distributions", "Selected dataset"))),

  list(id = "compare_genes",        scope = "one",  group = "Explore",
       title = "Compare genes",     icon = "chart-bar",
       ui = "compareGenesUI",       server = "compareGenesServer",
       pkgs = c("colourpicker", "dplyr", "ggbreak", "ggplot2", "ggpubr", "jsonlite", "patchwork", "purrr", "RColorBrewer", "rlang", "sortable", "svglite"),
       layout = list(left   = c("Selection", "Reorder groups", "Statistics", "Y axis scale"),
                     center = c("Facet Plot", "Wilcoxon Test Results"),
                     right  = c("Color selection", "Labels", "Size adjustments", "Grid Controls", "Aesthetic Settings"))),

  list(id = "compare_samples",      scope = "one",  group = "Explore",
       title = "Compare samples",   icon = "vials",
       ui = "compareSamplesUI",     server = "compareSamplesServer",
       pkgs = c("colourpicker", "dplyr", "ggbreak", "ggplot2", "ggpubr", "jsonlite", "patchwork", "purrr", "RColorBrewer", "rlang", "sortable", "svglite"),
       layout = list(left   = c("Selection", "Reorder groups", "Statistics", "Y axis scale"),
                     center = c("Facet Plot", "Wilcoxon Test Results"),
                     right  = c("Color selection", "Labels", "Size adjustments", "Grid Controls", "Aesthetic Settings"))),

  list(id = "correlation_analysis", scope = "one",  group = "Explore",
       title = "Correlation",       icon = "chart-line",
       ui = "correlationUI",        server = "correlationServer",
       pkgs = c("colourpicker", "dplyr", "ggplot2", "ggpubr", "jsonlite", "RColorBrewer", "rlang", "sortable", "svglite", "viridis"),
       layout = list(left   = c("Selection", "Reorder & hide groups", "Statistics"),
                     center = c("Facet Plot"),
                     right  = c("Labels", "Size adjustments", "Color selection", "Settings"))),

  list(id = "meta_analysis",        scope = "many", group = "Explore",
       title = "Meta-analysis",     icon = "tree",
       ui = "metaAnalysisUI",       server = "metaAnalysisServer",
       pkgs = c("cowplot", "dplyr", "DT", "ggplot2", "grid", "gridExtra", "meta"),
       layout = list(left   = c("Input Parameters"),
                     center = c("Forest Plot"),
                     right  = c("Analysis Summary", "Selected Metadata"))),

  list(id = "cutoff_finder",        scope = "one",  group = "Explore",
       title = "Cutoff finder",     icon = "scissors",
       ui = "cutoffFinderUI",       server = "cutoffFinderServer",
       pkgs = c("flexmix", "ggplot2", "patchwork", "survival"),
       layout = list(left   = c("Variable", "Method", "Split"),
                     center = c("Cutoff", "Outcome"),
                     right  = c("Result", "Save"))),

  list(id = "export_matrix",        scope = "one",  group = "Export",
       title = "Export matrix",     icon = "file-export",
       ui = "exportMatrixUI",       server = "exportMatrixServer",
       pkgs = c("dplyr", "DT", "tidyr"),
       layout = list(left   = c("Selection"),
                     center = c("Matrix"),
                     right  = c("Checks", "Export"))),

  list(id = "upload_dataset",       scope = "many", group = "Manage",
       title = "Upload dataset",    icon = "upload",
       ui = "uploadDatasetUI",      server = "uploadDatasetServer",
       pkgs = c("datamods", "DT", "magrittr"),
       layout = list(exception = "step-by-step form, one centred column")),

  list(id = "metadata_editor",      scope = "one",  group = "Manage",
       title = "Edit metadata",     icon = "pen-to-square",
       ui = "metadataEditorUI",     server = "metadataEditorServer",
       pkgs = c("datamods", "DT"),
       layout = list(left   = c("Columns", "Upload & Save"),
                     center = c("Metadata"),
                     right  = character(0))),

  list(id = "cutoff_maker",         scope = "one",  group = "Manage",
       title = "Manual cutoffs",    icon = "sliders",
       ui = "cutoffMakerUI",        server = "cutoffMakerServer",
       pkgs = c("dplyr", "ggplot2", "plotly", "rhandsontable", "shinyWidgets"),
       layout = list(left   = c("Cutoffs"),
                     center = c("Distribution", "Data Table"),
                     right  = c("Saving Data")))
)
names(MODULES) <- vapply(MODULES, function(m) m$id, character(1))

# Where each module's source lives, and which legacy app it came from.
module_file <- function(m) file.path("R", "modules", paste0("mod_", m$id, ".R"))
legacy_dir  <- function(m) file.path("legacy", gsub("_", "-", m$id))

module_groups <- function() unique(vapply(MODULES, function(m) m$group, character(1)))
