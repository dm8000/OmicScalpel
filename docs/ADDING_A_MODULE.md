# Adding a tab

A tab is a Shiny module: two functions in one file, plus one row in the
registry. Nothing else in the app needs to change.

## 1. The file

`R/modules/mod_<id>.R`, defining exactly two functions and no top-level code:

```r
myThingUI <- function(id) {
  ns <- NS(id)
  os_layout(
    left = os_panel(title = "Selection",
      selectInput(ns("unit"), "Unit", choices = NULL)
    ),
    center = os_panel(title = "Plot",
      plotOutput(ns("plot"))
    ),
    right = os_panel(title = "Appearance",
      checkboxInput(ns("log2"), "log2", FALSE)
    )
  )
}

myThingServer <- function(id, ds, meta, go_to = NULL, ai = NULL) {
  moduleServer(id, function(input, output, session) {
    observeEvent(ds(), {
      updateSelectInput(session, "unit", choices = list_units(ds()))
    })
    output$plot <- renderPlot({
      req(ds(), input$unit)
      plot_something(load_expression(ds(), input$unit))
    })
  })
}
```

The layout is always the same three columns: what selects data on the left,
the analysis in the middle, what changes its appearance or exports it on the
right. `os_layout()` and `os_panel()` are in `R/ui_helpers.R`; there is no
`box()`, no `fluidRow`/`column` at the top level and no shinydashboard.

Five rules that the linters enforce, because breaking them still parses, still
renders, and simply stops working:

- every id in the UI goes through `ns()`;
- an id built in the server -- in `renderUI`, in `showModal`, in a helper --
  goes through `session$ns()`;
- `update*(session, "id", ...)` keeps its bare id. The module session
  namespaces it already;
- **no page shell.** No `dashboardPage`, `dashboardBody`, `tabItems`,
  `tabItem`, `fluidPage`, `navbarPage` or `shinyApp`. A module fills one tab.
  A leftover `tabItem` nests a tab-pane inside a tab-pane: the page renders,
  returns 200, and that tab is blank forever;
- **each panel in the column the registry declares.** `layout` in
  `R/registry.R` says where every panel goes, and `lint_layout.R` renders the
  UI and checks where they landed -- including refusing a panel that is not in
  the map at all.

Read the hub only through `R/data_io.R`. A module that calls `read_excel` or
`write_xlsx` itself has a bug.

Do not keep module state in a file-level environment. `R/*.R` is sourced twice
-- shiny autoloads it and `global.R` sources it again -- so such an environment
exists twice and the two copies drift apart silently. Anything process-wide
goes in `options()`; see `metadata_version()` in `R/data_io.R`.

## 2. The row

In `R/registry.R`:

```r
list(id = "my_thing", scope = "one", group = "Explore",
     title = "My thing", icon = "flask",
     ui = "myThingUI", server = "myThingServer",
     pkgs = c("ggplot2", "dplyr"))
```

`scope = "one"` means the module works on a single dataset and reads `ds()`;
it must not have a dataset selector. `scope = "many"` means it chooses its own
and `ds` is ignored. `pkgs` is what makes `check_deps.R` able to say which tab
a missing package blocks.

## 3. Before committing

```sh
./tools/verify.sh                        # everything, cheapest first
Rscript tools/run_module.R my_thing      # then open the printed URL
```

Or the individual checks, if you want them one at a time:

```sh
Rscript tools/lint_ns.R --single-dataset R/modules/mod_my_thing.R  # --single-dataset only for scope "one"
Rscript tools/lint_masking.R R/modules/mod_my_thing.R
Rscript tools/lint_parity.R my_thing
Rscript tools/smoke_test.R my_thing
```

Each of the three linters exists because of a defect that shipped:

- `lint_ns.R` -- an un-namespaced id parses, renders, and stops talking to the
  server.
- `lint_masking.R` -- `function(dataset) filter(dataset == dataset)` compares
  the column with itself under data masking, keeps every row of every dataset,
  and plots a plausible wrong answer.
- `lint_parity.R` -- a control can simply go missing. It compares the module's
  ids against the legacy app's; anything deliberately dropped is declared, with
  a reason, in `tools/parity-exceptions.txt`.

The smoke test instantiates the module against every dataset in the fixture,
not just the first. Keep at least two datasets in `data-sample/`: with one, a
module that ignores the shared dataset passes by accident.
