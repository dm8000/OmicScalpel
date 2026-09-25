#!/usr/bin/env Rscript
# The unit label and the facet grid.
#
#   Rscript tools/behaviour/plot_layout.R

source("global.R")
source("tools/behaviour/_fixture.R")
use_fixture_hub()

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

# --- the unit that ends up on an axis label --------------------------------
# The correlation tab derived it with sub("^_|\\.txt$", ...), which removes
# only the first match, so "_TMM.txt" became "TMM.TXT" and the file extension
# was printed on the y axis.
for (d in unique(load_metadata()$dataset)) {
  u <- list_units(d)
  if (!length(u)) next
  chk(!any(grepl("\\.txt$|\\.TXT$", u)), paste0(d, "'s units carry no file extension"),
      paste(u, collapse = ", "))
}

# --- the facet grid --------------------------------------------------------
chk(os_facet_cols(1) == 1 && os_facet_rows(1) == 1, "one panel is one by one")
chk(os_facet_cols(3) == 3 && os_facet_rows(3) == 1, "three panels sit in a row")

# every count must fit, and never in more columns than panels
for (k in 1:40) {
  cols <- os_facet_cols(k); rows <- os_facet_rows(k, cols)
  if (cols * rows < k) no("the grid does not fit ", k, " panels: ", cols, "x", rows)
  if (cols > max(k, 1)) no("more columns than panels at ", k, ": ", cols)
}
ok("every count from 1 to 40 fits, with no column left over")

# wider than tall, because a screen and a printed figure both are
wide <- vapply(4:40, function(k) os_facet_cols(k) >= os_facet_rows(k, os_facet_cols(k)),
               logical(1))
chk(all(wide), "the grid is never taller than it is wide", sum(!wide), " exceptions")

chk(os_facet_cols(200) <= 10, "and it stops widening at some point",
    os_facet_cols(200))

cat("\n", n, " checks passed\n", sep = "")
