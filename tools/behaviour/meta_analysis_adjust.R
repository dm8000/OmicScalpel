#!/usr/bin/env Rscript
# Behaviour test for covariate adjustment in the meta-analysis tab.
#
#   Rscript tools/behaviour/meta_analysis_adjust.R
#
# Checks the arithmetic against answers worked out independently, not against
# whatever the function happens to return.
#
# This test used to reach adjusted_effect() by slicing mod_meta_analysis.R and
# eval()ing the slice, so it never opened a session -- and the tab's gene
# lookup was broken the whole time it passed. The functions now live in
# R/meta_stats.R, and the session-level check is in
# tools/behaviour/meta_analysis_forest.R.

source("global.R")
source("tools/behaviour/_fixture.R")
use_fixture_hub()

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

set.seed(11)

# n is large on purpose. At 40 per group this seed gives a sample d of 1.56
# for a simulated effect of 1 -- the check would then be measuring luck.
N  <- 400L

# --- with no covariate it must be the ordinary standardised difference ------
g  <- rep(c("g1", "g2"), each = N)
y  <- c(rnorm(N, 0, 1), rnorm(N, 1, 1))
r  <- adjusted_effect(y, g, list())
fit <- lm(y ~ factor(g))
want_d  <- unname(coef(fit)[2]) / sigma(fit)
want_se <- unname(summary(fit)$coefficients[2, 2]) / sigma(fit)
chk(abs(r$d - want_d) < 1e-9, "with no covariate the effect is b/residual SD",
    r$d, " vs ", want_d)
chk(abs(r$se - want_se) < 1e-9, "and so is its standard error")
chk(abs(r$d - 1) < 0.15, "which lands near the simulated effect of 1", round(r$d, 3))

# --- a confounder: the covariate must change the answer ---------------------
# sex is unbalanced between groups and shifts expression on its own, so the
# raw difference is inflated and adjustment should pull it back
sex   <- c(rep("F", round(N * .8)), rep("M", N - round(N * .8)),
           rep("F", N - round(N * .8)), rep("M", round(N * .8)))
shift <- ifelse(sex == "M", 2, 0)
y2    <- c(rnorm(N, 0, 1), rnorm(N, 0.5, 1)) + shift

raw <- adjusted_effect(y2, g, list())
adj <- adjusted_effect(y2, g, list(Sex = sex))

chk(raw$d > adj$d, "adjusting for a confounder pulls the effect back",
    round(raw$d, 3), " -> ", round(adj$d, 3))
chk(abs(adj$d - 0.5) < abs(raw$d - 0.5),
    "and closer to the effect that was simulated",
    round(adj$d, 3), " vs ", round(raw$d, 3))
chk(identical(adj$used, "Sex"), "the covariate is reported as used",
    paste(adj$used, collapse = ", "))

# the adjusted p-value comes from the same model, not from a raw t-test
want_p <- summary(lm(y2 ~ factor(g) + factor(sex)))$coefficients[2, 4]
chk(abs(adj$pvalue - want_p) < 1e-9, "the p-value is the model's, not a raw t-test's")

# --- a covariate that does not vary is dropped, not fatal -------------------
flat <- adjusted_effect(y, g, list(Sex = rep("F", 2L * N)))
chk(!is.null(flat) && identical(flat$dropped, "Sex"),
    "a covariate constant in this dataset is dropped and reported",
    if (is.null(flat)) "returned NULL" else paste(flat$dropped, collapse = ", "))
chk(abs(flat$d - want_d) < 1e-9, "and the estimate matches the unadjusted one")

# --- a numeric covariate is used as a number, not as levels ----------------
age <- runif(2L * N, 20, 70)
num <- adjusted_effect(y, g, list(Age = as.character(age)))
want_num <- { f <- lm(y ~ factor(g) + age); unname(coef(f)[2]) / sigma(f) }
chk(abs(num$d - want_num) < 1e-9, "a numeric covariate enters as a number",
    round(num$d, 4), " vs ", round(want_num, 4))

# --- too few samples to fit must refuse rather than invent -----------------
tiny <- adjusted_effect(c(1, 2, 3, 4), rep(c("g1", "g2"), each = 2),
                        list(Sex = c("F", "M", "F", "M"), Age = c("1", "2", "3", "4")))
chk(is.null(tiny), "too few samples for the model returns nothing")

cat("\n", n, " checks passed\n", sep = "")
