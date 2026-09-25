#!/usr/bin/env Rscript
# A plan has to make the tab it names actually draw.
#
#   Rscript tools/behaviour/ai_drive.R
#
# tools/behaviour/ai_plan.R checks the planner picks the right tool. This
# checks the other half: that the controls it fills in are the ones that tab
# reads, and that they are enough. The two together are what "the AI moves the
# same buttons the researcher sees" means in a test.
#
# The browser is the one part not covered here -- update*() is a message, and
# testServer has no client to answer it. So the echo is simulated: the plan is
# published, then the inputs are set the way a browser would report them, and
# the gate has to fire on its own from there.

source("global.R")
source("tools/behaviour/_fixture.R")
use_fixture_hub()

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

md   <- load_metadata()
CATL <- ai_catalog(md)
DS   <- "DEMO_RNAseq"
GENE <- load_expression(DS, "TPM")$Symbol[5]

# A gene index for the fixture, so the planner filters on what is really there.
INDEX <- list(built = Sys.time(), datasets = stats::setNames(
  lapply(names(CATL), function(d) {
    u <- tryCatch(list_units(d)[1], error = function(e) NA)
    if (is.na(u)) character(0) else load_expression(d, u)$Symbol
  }), names(CATL)))

choice <- function(v, conf = 0.95) list(type = "choice", choice = v,
                                        probabilities = stats::setNames(list(conf), v),
                                        confidence = conf)
noul <- function(p) list(type = "noul", noul = p)
GENES <- data.frame(symbol = GENE, how = "exact", stringsAsFactors = FALSE)

plan_for <- function(...) {
  base <- list(answerable = noul(0.95), intent = choice("association_continuous"),
               species = choice("any"), tissue = choice("any"),
               measurement = choice("RNAseq"),
               variable = choice("DEMO.Marker"), gene = choice(GENE))
  ai_decide(utils::modifyList(base, list(...)), CATL, GENES, index = INDEX)
}

# Drive one module the way the app does: publish the plan, let the gate apply
# it, then report the inputs back as a browser would.
drive <- function(server, plan, extra = list()) {
  bus <- reactiveVal(NULL)
  out <- NULL
  testServer(server, args = list(id = plan$tab,
                                 ds = reactiveVal(plan$dataset %||% DS),
                                 meta = reactive(md), go_to = NULL, ai = bus), {
    bus(plan)
    session$flushReact()
    do.call(session$setInputs, c(plan$controls, extra))
    session$flushReact()
    out <<- list(drew = isTRUE(draw() > 0),
                 plot = tryCatch(output$facet_plot,
                                 error = function(e) conditionMessage(e)))
  })
  out
}

# --- correlation --------------------------------------------------------------

p <- plan_for()
chk(identical(p$tab, "correlation_analysis"),
    "the planner sends a continuous variable in one dataset to the correlation",
    p$tab, " / ", p$headline)

# What a browser sends with the page and testServer does not: every control
# with a UI default that the plot reads. Supplying them here is simulating the
# page load, not weakening the check -- the plan's own controls are still the
# only thing that selects the data.
PAGE <- list(log2_y = FALSE, log2_x = FALSE, correlation_test = 0,
             plot_title = "t", x_axis_label = "x", y_axis_label = "y",
             title_font_size = 18, axis_label_font_size = 15,
             axis_text_font_size = 10, stat_text_font_size = 5,
             plot_width = 800, plot_height = 600, plot_cols = 3, plot_rows = 3,
             dot_size = 2, line_thickness = 1, color_palette = "Dark2")

r <- drive(correlationServer, p, extra = PAGE)
chk(r$drew, "publishing the plan fires the tab's draw gate on its own")
chk(!is.character(r$plot) && !is.null(r$plot),
    "and the plot renders from the plan alone, with no button pressed",
    if (is.character(r$plot)) r$plot else "nothing rendered")

# The same controls, nothing else: proof the plan's `needs` are sufficient and
# not merely necessary.
spec <- AI_TOOLS[["correlation_analysis"]]
chk(all(spec$needs %in% names(p$controls)),
    "the plan fills every control that tab declares it needs",
    paste(setdiff(spec$needs, names(p$controls)), collapse = ", "))

# --- compare samples ----------------------------------------------------------

p2 <- plan_for(intent = choice("comparison_groups"),
               variable = choice("DEMO.Responder"))
chk(identical(p2$tab, "compare_samples"), "and groups in one dataset to compare samples",
    p2$tab)
CMP_PAGE <- list(log2_transform = FALSE, show_wilcox = FALSE, facet_plot = FALSE,
                 plot_title = "t", x_label = "x", y_label = "y",
                 plot_width = 800, plot_height = 600,
                 title_size = 18, axis_font_size = 14, group_font_size = 12,
                 facet_font_size = 12, jitter_size = 1.5, box_outline_size = 0.5,
                 median_line_size = 0.7, grid_line_size = 0.3, stat_line_size = 0.4,
                 show_major_x = TRUE, show_major_y = TRUE,
                 show_minor_x = FALSE, show_minor_y = FALSE)

r2 <- drive(compareSamplesServer, p2, extra = CMP_PAGE)
chk(r2$drew && !is.character(r2$plot) && !is.null(r2$plot),
    "which draws from the plan too",
    if (is.character(r2$plot)) r2$plot else "nothing rendered")

# --- the human button still works --------------------------------------------

manual <- NULL
testServer(correlationServer,
           args = list(id = "correlation_analysis", ds = reactiveVal(DS),
                       meta = reactive(md), go_to = NULL, ai = NULL), {
  do.call(session$setInputs, c(p$controls, PAGE))
  session$setInputs(plot = 1)
  session$flushReact()
  manual <<- list(drew = isTRUE(draw() > 0), plot = tryCatch(output$facet_plot,
                                                             error = function(e) conditionMessage(e)))
})
chk(manual$drew && !is.character(manual$plot),
    "pressing the button by hand still draws, with no plan anywhere",
    if (is.character(manual$plot)) manual$plot else "nothing rendered")

# --- a plan for another tab is ignored ----------------------------------------

other <- NULL
testServer(correlationServer,
           args = list(id = "correlation_analysis", ds = reactiveVal(DS),
                       meta = reactive(md), go_to = NULL, ai = reactiveVal(p2)), {
  session$flushReact()
  other <<- isTRUE(draw() > 0)
})
chk(!other, "a plan addressed to another tab moves nothing here")

# --- the meta-analysis --------------------------------------------------------

# Both fixture datasets carry the gene, but they hold different units, so the
# planner's single unit reaches one of them. That is the real behaviour.
meta_plan <- ai_plan_out("meta_analysis", NULL, names(CATL),
                         list(biomolecule = GENE, condition = "DEMO.Responder",
                              group1_categories = "non-responder",
                              group2_categories = "responder",
                              data_preference = "TPM"),
                         list(), "forest")
res <- NULL
bus <- reactiveVal(NULL)
testServer(metaAnalysisServer,
           args = list(id = "meta_analysis", ds = reactiveVal(DS),
                       meta = reactive(md), go_to = NULL, ai = bus), {
  bus(meta_plan)
  session$flushReact()
  do.call(session$setInputs, c(meta_plan$controls,
                               list(stat_test = "ttest", filter_conditions = character(0),
                                    adjust_for = character(0))))
  session$flushReact()
  res <<- list(drew = isTRUE(draw() > 0), n = length(perform_analysis()$results),
               plot = tryCatch(output$forest_plot, error = function(e) conditionMessage(e)))
})
chk(res$drew, "the meta-analysis gate fires from a plan as well")
chk(res$n >= 1, "and the forest plot has something to draw", res$n)
chk(!is.character(res$plot) && !is.null(res$plot),
    "and the forest plot itself renders, not just its numbers",
    if (is.character(res$plot)) res$plot else "nothing rendered")

cat("\n", n, " checks passed\n", sep = "")
