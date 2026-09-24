# global.R -- shiny sources this before app.R, in the same environment.
#
# Order matters: config first (base R only), then the library path it names,
# then the packages, then project code.

source("R/config.R")

lib <- os_path("lib")
if (dir.exists(lib)) .libPaths(lib)

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(forcats)
  library(rlang)
  library(magrittr)
  library(readxl)
  library(writexl)
  library(openxlsx)
  library(jsonlite)
  library(ggplot2)
  library(ggpubr)
  library(ggbreak)
  library(patchwork)
  library(cowplot)
  library(gridExtra)
  library(grid)
  library(plotly)
  library(RColorBrewer)
  library(viridis)
  library(svglite)
  library(colourpicker)
  library(sortable)
  library(rhandsontable)
  library(datamods)
  library(meta)
})

source("R/data_io.R")
source("R/registry.R")

for (m in MODULES) {
  f <- module_file(m)
  if (file.exists(f)) source(f)
}
