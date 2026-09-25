#!/usr/bin/env Rscript
# The statistics behind the Cutoff Finder tab.
#
#   Rscript tools/behaviour/cutoff_finder.R
#
# Every check compares against an answer worked out independently -- a planted
# cutpoint, or survival/fisher computed separately -- not against whatever the
# function returns.

source("global.R")

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

set.seed(7)

# --- candidates -------------------------------------------------------------
x <- c(rep(1, 50), 2:60)
cand <- cf_candidates(x, min_frac = 0.1)
sides <- vapply(cand, function(cut) min(sum(x <= cut), sum(x > cut)), numeric(1))
chk(all(sides >= ceiling(0.1 * length(x))),
    "no candidate leaves less than min_frac on a side", min(sides))
chk(length(cf_candidates(x, 0)) > length(cand),
    "and relaxing min_frac admits more of them",
    length(cf_candidates(x, 0)), " vs ", length(cand))
chk(length(cf_candidates(rep(3, 20))) == 0, "a constant variable has no candidates")

# --- survival: the planted cutpoint has to be found -------------------------
N <- 300L
marker <- runif(N, 0, 10)
TRUE_CUT <- 6
hazard  <- ifelse(marker > TRUE_CUT, 3, 1)          # threefold above the cut
time    <- rexp(N, rate = hazard / 40)
event   <- rbinom(N, 1, .8)

scan <- cf_scan_survival(marker, time, event, min_frac = .1)
best <- cf_best(scan, "significance")
chk(nrow(scan) > 30, "the survival scan produces candidates", nrow(scan))
chk(abs(best$cutoff - TRUE_CUT) < 1,
    "and the most significant split lands on the planted cutpoint",
    round(best$cutoff, 2), " vs ", TRUE_CUT)
chk(best$effect > 1.5, "with a hazard ratio in the simulated direction",
    round(best$effect, 2))

# the HR and p at a fixed cut must equal what survival gives on its own
fixed <- 5
grp   <- cf_split(marker, fixed)
sm    <- summary(survival::coxph(survival::Surv(time, event) ~ grp))
row   <- scan[which.min(abs(scan$cutoff - fixed)), ]
chk(abs(row$effect - unname(sm$conf.int[1, "exp(coef)"])) < 1e-8,
    "the hazard ratio matches a Cox fit done separately")
chk(abs(row$pvalue - unname(sm$sctest["pvalue"])) < 1e-8,
    "and so does the log-rank p")

# --- binary outcome ---------------------------------------------------------
y <- ifelse(marker > TRUE_CUT, "responder", "non")
y[sample.int(N, 30)] <- sample(c("responder", "non"), 30, TRUE)   # some noise
bs   <- cf_scan_binary(marker, y, positive = "responder", min_frac = .1)
bbest <- cf_best(bs, "significance")
chk(abs(bbest$cutoff - TRUE_CUT) < 1,
    "the binary scan finds the planted cutpoint too", round(bbest$cutoff, 2))

# odds ratio, sensitivity and specificity against a table built by hand
r  <- bs[which.min(abs(bs$cutoff - fixed)), ]
hi <- marker > r$cutoff; pos <- y == "responder"
tp <- sum(hi & pos); fp <- sum(hi & !pos); fn <- sum(!hi & pos); tn <- sum(!hi & !pos)
ft <- stats::fisher.test(matrix(c(tp, fp, fn, tn), nrow = 2))
chk(abs(r$effect - unname(ft$estimate)) < 1e-8, "the odds ratio matches a 2x2 by hand")
chk(abs(r$pvalue - ft$p.value) < 1e-12, "and so does Fisher's p")
chk(abs(r$sensitivity - tp / (tp + fn)) < 1e-12, "sensitivity is tp/(tp+fn)")
chk(abs(r$specificity - tn / (tn + fp)) < 1e-12, "specificity is tn/(tn+fp)")

auc <- cf_auc(bs)
chk(auc > .8 && auc <= 1, "the AUC reflects a marker that separates well", round(auc, 3))

# the three rules are allowed to disagree, but each must return a candidate
for (rule in c("significance", "euclidean", "manhattan")) {
  b <- cf_best(bs, rule)
  if (is.null(b) || !b$cutoff %in% bs$cutoff) no("rule ", rule, " returned nothing usable")
}
ok("all three selection rules return a candidate from the scan")

# --- the corrected p --------------------------------------------------------
# With no signal at all, the best of several hundred splits still looks good.
noise <- runif(150, 0, 10)
t2 <- rexp(150, 1 / 40); e2 <- rbinom(150, 1, .8)
sc_noise <- cf_scan_survival(noise, t2, e2, min_frac = .1)
perm <- cf_permutation_p(noise, list(time = t2, event = e2),
                         kind = "survival", min_frac = .1, B = 60L, seed = 3)
chk(perm$p > perm$observed,
    "the permutation p is larger than the optimised one",
    signif(perm$observed, 3), " -> ", signif(perm$p, 3))
chk(perm$p > 0, "and never exactly zero, which 60 draws cannot support", perm$p)

# on the planted data it should still call the signal real
perm_real <- cf_permutation_p(marker, list(time = time, event = event),
                              kind = "survival", min_frac = .1, B = 60L, seed = 3)
chk(perm_real$p < 0.05, "a real effect survives the correction", signif(perm_real$p, 3))

# --- the mixture ------------------------------------------------------------
bimodal <- c(rnorm(200, 2, .6), rnorm(200, 8, .6))
mix <- cf_distribution(bimodal)
chk(!is.null(mix), "the mixture fits two components")
if (!is.null(mix)) {
  chk(mix$cutoff > 3 && mix$cutoff < 7,
      "and puts the cutoff between the two simulated means", round(mix$cutoff, 2))
  chk(abs(sort(mix$mu)[1] - 2) < .5 && abs(sort(mix$mu)[2] - 8) < .5,
      "recovering both means", paste(round(sort(mix$mu), 2), collapse = " and "))
}

cat("\n", n, " checks passed\n", sep = "")
