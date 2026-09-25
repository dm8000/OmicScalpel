#!/usr/bin/env Rscript
# The four plots the Cutoff finder draws.
#
#   Rscript tools/behaviour/cutoff_plots.R
#
# A plot test cannot check that a figure is readable. It can check that the
# figure is drawn at all -- these are rendered to a file, not merely built --
# and that the few things a reader relies on are really in it.

source("global.R")

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

set.seed(1)
N     <- 200
x     <- round(runif(N, 0, 10), 2)
time  <- pmax(round(rexp(N, ifelse(x > 6, 3, 1) / 40), 1), 0.1)
event <- rbinom(N, 1, 0.8)
y     <- ifelse(x > 6, "yes", "no"); y[sample.int(N, 20)] <- "no"

sc  <- cf_scan_survival(x, time, event)
bs  <- cf_best(sc)
scb <- cf_scan_binary(x, y, positive = "yes")
bb  <- cf_best(scb, "euclidean")
xm  <- c(rnorm(150, 3, 1), rnorm(150, 8, 1))
di  <- cf_distribution(xm)

# Rendered, not just built: a ggplot with an impossible aesthetic constructs
# fine and only fails when something tries to draw it.
drawn <- function(p, what) {
  if (!inherits(p, "ggplot")) no(what, "not a ggplot")
  f <- tempfile(fileext = ".png")
  e <- tryCatch({ suppressMessages(ggplot2::ggsave(f, p, width = 5, height = 4, dpi = 72)); NULL },
                error = function(e) conditionMessage(e))
  if (!is.null(e)) no(what, e)
  chk(file.exists(f) && file.size(f) > 2000, what, "nothing was written")
}

drawn(cf_plot_scan(sc,  bs$cutoff), "the survival scan draws")
drawn(cf_plot_scan(scb, bb$cutoff), "the binary scan draws")
drawn(cf_plot_scan(sc,  NA_real_),  "a scan with no cutpoint chosen draws")
drawn(cf_plot_km(x, time, event, bs$cutoff), "the Kaplan-Meier draws")
drawn(cf_plot_roc(scb, bb$cutoff), "the ROC draws")
drawn(cf_plot_mixture(xm, di), "the mixture draws")

# What a reader takes from the KM legend is which curve is which and how many
# samples are behind it. The stratum names survfit invents ("grp=low") are not
# that, and they are what showed until this was checked.
km  <- cf_plot_km(x, time, event, bs$cutoff)
leg <- ggplot2::get_guide_data(km, "colour")
chk(any(grepl("^low \\(n=\\d+\\)$", leg$.label)) &&
    any(grepl("^high \\(n=\\d+\\)$", leg$.label)),
    "the KM legend names the groups and their sizes",
    paste(leg$.label, collapse = " | "))
chk(identical(leg$.value[1], "grp=low"),
    "and low comes first, whatever the alphabet says",
    paste(leg$.value, collapse = " | "))

# The ROC reference line is one diagonal. Written as a segment inheriting the
# plot data it became one dashed line per candidate cutpoint -- a fan across
# the panel that hid the curve.
roc  <- cf_plot_roc(scb, bb$cutoff)
refs <- Filter(function(d) all(c("slope", "intercept") %in% names(d)),
               ggplot2::ggplot_build(roc)$data)
chk(length(refs) == 1 && nrow(refs[[1]]) == 1,
    "the ROC draws exactly one reference diagonal",
    "layers: ", length(refs), ", rows: ",
    if (length(refs)) nrow(refs[[1]]) else 0)

# Refusals, which the tab renders as an empty panel rather than an error.
chk(is.null(cf_plot_scan(cf_empty_scan("hr"), NA_real_)), "an empty scan draws nothing")
chk(is.null(cf_plot_roc(sc, bs$cutoff)), "the ROC refuses a survival scan")
chk(is.null(cf_plot_mixture(xm, NULL)), "the mixture refuses a failed fit")
chk(is.null(cf_plot_km(x, time, event, max(x) + 1)),
    "a cutpoint with nothing above it draws nothing")

cat("\n", n, " checks passed\n", sep = "")
