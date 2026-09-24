# Adding a tab

A tab is a Shiny module: two functions in one file, plus one row in the
registry. Nothing else in the app needs to change.

## 1. The file

`R/modules/mod_<id>.R`, defining exactly two functions and no top-level code:

```r
myThingUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(3, selectInput(ns("unit"), "Unit", choices = NULL)),
    column(9, plotOutput(ns("plot")))
  )
}

myThingServer <- function(id, ds, meta, go_to = NULL) {
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

Four rules that the linter enforces, because breaking them still parses, still
renders, and simply stops working:

- every id in the UI goes through `ns()`;
- an id built in the server -- in `renderUI`, in `showModal`, in a helper --
  goes through `session$ns()`;
- `update*(session, "id", ...)` keeps its bare id. The module session
  namespaces it already;
- **no page shell.** No `dashboardPage`, `dashboardBody`, `tabItems`,
  `tabItem`, `fluidPage`, `navbarPage` or `shinyApp`. A module fills one tab.
  A leftover `tabItem` nests a tab-pane inside a tab-pane: the page renders,
  returns 200, and that tab is blank forever.

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
Rscript tools/lint_ns.R --single-dataset R/modules/mod_my_thing.R   # --single-dataset only for scope "one"
Rscript tools/smoke_test.R my_thing
Rscript tools/run_module.R my_thing      # then open the printed URL
```

The smoke test instantiates the module against every dataset in the fixture,
not just the first. Keep at least two datasets in `data-sample/`: with one, a
module that ignores the shared dataset passes by accident.
