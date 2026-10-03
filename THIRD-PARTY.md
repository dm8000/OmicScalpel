# Third-party software

OmicScalpel's own code is under the PolyForm Noncommercial License 1.0.0 (see
[LICENSE.md](LICENSE.md)). The R packages it runs on are not part of this repository,
are not covered by that license, and keep their own licenses. Nothing below is
vendored: every package is installed from CRAN by whoever runs the app, and so is R
itself, with its base packages (`stats`, `utils`, `tools`, `grid`, `grDevices`).

**Keep it that way when you hand the app to someone.** Install the packages on the
target machine from CRAN; do not ship a bundle that contains them (a Docker image, an
renv library, a tarball with `lib/`). Ten of these packages are GPL or LGPL, and a
distribution that combines them with this code has to meet their terms too.

Licenses as declared in each package's DESCRIPTION.

## Permissive

| package | license |
|---|---|
| colourpicker | MIT |
| DT | MIT |
| dplyr | MIT |
| forcats | MIT |
| ggbreak | Artistic-2.0 |
| ggplot2 | MIT |
| httr | MIT |
| jsonlite | MIT |
| magrittr | MIT |
| openxlsx | MIT |
| patchwork | MIT |
| plotly | MIT |
| purrr | MIT |
| RColorBrewer | Apache-2.0 |
| readxl | MIT |
| rhandsontable | MIT (bundles Handsontable 6.2.2, the last MIT release) |
| rlang | MIT |
| shiny | MIT |
| shinydashboard | MIT |
| sortable | MIT |
| tidyr | MIT |
| viridis | MIT |
| writexl | BSD-2-Clause |

## Copyleft

| package | license |
|---|---|
| cowplot | GPL-2 |
| datamods | GPL-3 |
| flexmix | GPL (>= 2) |
| ggpubr | GPL (>= 2) |
| gridExtra | GPL (>= 2) |
| htmltools | GPL (>= 2) |
| meta | GPL (>= 2) |
| shinyWidgets | GPL-3 |
| survival | LGPL (>= 2) |
| svglite | GPL (>= 2) |

## Methods

The Cutoff finder tab implements the methods of Cutoff Finder (Budczies et al., PLoS
ONE 2012;7(12):e51862). It is an independent implementation written from the paper;
it contains no code from the GPL-3 `CutoffFinder` R package.
