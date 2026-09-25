# R/cutoff_finder.R -- finding a cutpoint in a continuous variable.
#
# Distilled from Cutoff Finder (Budczies et al., PLoS ONE 2012;7(12):e51862).
# Pure functions, no Shiny, so the statistics can be checked without a session.
#
# The paper is explicit about what this costs:
#
#   "Cutoff optimization was demonstrated to contribute to overestimation of
#    results when multiple cutoff points are investigated and the problem of
#    multiple hypothesis testing is ignored."
#
# It answers that by plotting the effect across every cutpoint and advising
# validation in an independent cohort. We do that too, and add
# cf_permutation_p(), because a p-value read off the best of several hundred
# splits is not a p-value and someone is going to put it in a paper.

# --- candidate cutpoints ----------------------------------------------------

# Every distinct value that leaves at least min_frac of the samples on each
# side. A split with two samples on one side produces a huge hazard ratio with
# a uselessly wide interval, and that is the main way this method misleads.
cf_candidates <- function(x, min_frac = 0.1) {
  x <- suppressWarnings(as.numeric(x))
  ok <- is.finite(x)
  v  <- sort(unique(x[ok]))
  if (length(v) < 2) return(numeric(0))
  n     <- sum(ok)
  floor_n <- max(2L, ceiling(min_frac * n))
  keep  <- vapply(v, function(cut) {
    lo <- sum(x[ok] <= cut)
    lo >= floor_n && (n - lo) >= floor_n
  }, logical(1))
  v[keep]
}

cf_split <- function(x, cut) {
  factor(ifelse(suppressWarnings(as.numeric(x)) > cut, "high", "low"),
         levels = c("low", "high"))
}

# --- survival ---------------------------------------------------------------

# One Cox model per candidate. The paper picks "the point with the most
# significant (log-rank test) split"; the score test of a Cox model with a
# single binary term is the log-rank test, which is what sctest reports.
cf_scan_survival <- function(x, time, event, min_frac = 0.1) {
  x     <- suppressWarnings(as.numeric(x))
  time  <- suppressWarnings(as.numeric(time))
  event <- suppressWarnings(as.numeric(event))

  ok <- is.finite(x) & is.finite(time) & time > 0 & event %in% c(0, 1)
  x <- x[ok]; time <- time[ok]; event <- event[ok]

  cuts <- cf_candidates(x, min_frac)
  if (!length(cuts)) return(cf_empty_scan("hr"))

  rows <- lapply(cuts, function(cut) {
    grp <- cf_split(x, cut)
    if (nlevels(droplevels(grp)) < 2) return(NULL)
    fit <- tryCatch(survival::coxph(survival::Surv(time, event) ~ grp),
                    error = function(e) NULL, warning = function(w) NULL)
    if (is.null(fit)) return(NULL)
    sm <- summary(fit)
    data.frame(
      cutoff   = cut,
      n_low    = sum(grp == "low"),
      n_high   = sum(grp == "high"),
      effect   = unname(sm$conf.int[1, "exp(coef)"]),
      lower    = unname(sm$conf.int[1, "lower .95"]),
      upper    = unname(sm$conf.int[1, "upper .95"]),
      pvalue   = unname(sm$sctest["pvalue"]),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(cf_empty_scan("hr"))
  out <- do.call(rbind, rows)
  attr(out, "effect_name") <- "HR"
  out
}

# --- binary outcome ---------------------------------------------------------

# One 2x2 table per candidate: odds ratio, Fisher's p, and the sensitivity and
# specificity the ROC rules are chosen on.
cf_scan_binary <- function(x, y, positive = NULL, min_frac = 0.1) {
  x <- suppressWarnings(as.numeric(x))
  y <- as.character(y)
  y[y == "NA"] <- NA

  ok <- is.finite(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  lv <- sort(unique(y))
  if (length(lv) != 2) return(cf_empty_scan("or"))
  if (is.null(positive)) positive <- lv[2]
  pos <- y == positive

  cuts <- cf_candidates(x, min_frac)
  if (!length(cuts)) return(cf_empty_scan("or"))

  rows <- lapply(cuts, function(cut) {
    hi <- x > cut
    tp <- sum(hi & pos); fp <- sum(hi & !pos)
    fn <- sum(!hi & pos); tn <- sum(!hi & !pos)
    tab <- matrix(c(tp, fp, fn, tn), nrow = 2)
    ft  <- tryCatch(stats::fisher.test(tab), error = function(e) NULL)
    if (is.null(ft)) return(NULL)
    data.frame(
      cutoff      = cut,
      n_low       = tn + fn,
      n_high      = tp + fp,
      effect      = unname(ft$estimate),
      lower       = ft$conf.int[1],
      upper       = ft$conf.int[2],
      pvalue      = ft$p.value,
      sensitivity = if ((tp + fn) > 0) tp / (tp + fn) else NA_real_,
      specificity = if ((tn + fp) > 0) tn / (tn + fp) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(cf_empty_scan("or"))
  out <- do.call(rbind, rows)
  attr(out, "effect_name") <- "OR"
  attr(out, "positive")    <- positive
  out
}

cf_empty_scan <- function(kind) {
  base <- data.frame(cutoff = numeric(0), n_low = integer(0), n_high = integer(0),
                     effect = numeric(0), lower = numeric(0), upper = numeric(0),
                     pvalue = numeric(0))
  if (kind == "or") {
    base$sensitivity <- numeric(0)
    base$specificity <- numeric(0)
  }
  attr(base, "effect_name") <- toupper(kind)
  base
}

# --- choosing one ------------------------------------------------------------

# "significance" is the paper's default for both endpoints. The two distance
# rules are its ROC methods: the cutpoint closest to the top-left corner,
# measured straight or along the axes.
cf_best <- function(scan, rule = c("significance", "euclidean", "manhattan")) {
  rule <- match.arg(rule)
  if (!nrow(scan)) return(NULL)

  i <- if (rule == "significance") {
    which.min(scan$pvalue)
  } else {
    if (!all(c("sensitivity", "specificity") %in% names(scan))) {
      stop("the distance rules need sensitivity and specificity, which only ",
           "the binary scan produces", call. = FALSE)
    }
    d <- if (rule == "euclidean") {
      sqrt((1 - scan$sensitivity)^2 + (1 - scan$specificity)^2)
    } else {
      (1 - scan$sensitivity) + (1 - scan$specificity)
    }
    which.min(d)
  }
  scan[i, , drop = FALSE]
}

# --- how optimistic is that p ------------------------------------------------

# Permute the outcome, run the whole scan again, and keep the smallest p it
# finds. The corrected p is how often chance alone beats what the real data
# gave. This is the number to quote.
cf_permutation_p <- function(x, outcome, kind = c("survival", "binary"),
                             min_frac = 0.1, B = 200L, seed = NULL) {
  kind <- match.arg(kind)
  if (!is.null(seed)) set.seed(seed)

  observed <- if (kind == "survival") {
    cf_scan_survival(x, outcome$time, outcome$event, min_frac)
  } else {
    cf_scan_binary(x, outcome$y, min_frac = min_frac)
  }
  if (!nrow(observed)) return(list(p = NA_real_, observed = NA_real_, B = 0L))
  obs_p <- min(observed$pvalue, na.rm = TRUE)

  hits <- 0L; done <- 0L
  for (b in seq_len(B)) {
    perm <- sample.int(length(x))
    sc <- if (kind == "survival") {
      cf_scan_survival(x, outcome$time[perm], outcome$event[perm], min_frac)
    } else {
      cf_scan_binary(x, outcome$y[perm], min_frac = min_frac)
    }
    if (!nrow(sc)) next
    done <- done + 1L
    if (min(sc$pvalue, na.rm = TRUE) <= obs_p) hits <- hits + 1L
  }
  if (!done) return(list(p = NA_real_, observed = obs_p, B = 0L))

  # (hits + 1) / (B + 1): a permutation p is never exactly zero, and reporting
  # zero from 200 draws would be its own overstatement
  list(p = (hits + 1) / (done + 1), observed = obs_p, B = done)
}

# --- outcome-independent -----------------------------------------------------

# "The optimal cutoff is determined as the value where the probability density
# functions of the mixing distribution coincide." Of the crossings, the paper's
# implementation keeps the one nearest the median.
cf_distribution <- function(x, n_grid = 1000L) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) < 10 || length(unique(x)) < 5) return(NULL)
  if (!requireNamespace("flexmix", quietly = TRUE)) return(NULL)

  fit <- tryCatch(
    flexmix::flexmix(x ~ 1, k = 2,
                     cluster = cbind(as.integer(x <= stats::median(x)),
                                     as.integer(x >  stats::median(x)))),
    error = function(e) NULL)
  if (is.null(fit) || length(unique(flexmix::clusters(fit))) < 2) return(NULL)

  par <- flexmix::parameters(fit)
  mu  <- par[1, ]; sigma <- par[2, ]
  w   <- flexmix::prior(fit)
  if (any(!is.finite(mu)) || any(!is.finite(sigma)) || any(sigma <= 0)) return(NULL)

  grid <- seq(min(x), max(x), length.out = n_grid)
  d1 <- w[1] * stats::dnorm(grid, mu[1], sigma[1])
  d2 <- w[2] * stats::dnorm(grid, mu[2], sigma[2])
  diff <- d1 - d2
  cross <- which(diff[-length(diff)] * diff[-1] <= 0)
  if (!length(cross)) return(NULL)

  cuts <- grid[cross]
  cut  <- cuts[which.min(abs(cuts - stats::median(x)))]
  list(cutoff = cut, mu = mu, sigma = sigma, weight = w, grid = grid,
       d1 = d1, d2 = d2)
}

# --- area under the ROC ------------------------------------------------------

# Trapezoid over the scan's own sensitivity and specificity, so no new
# dependency and no second definition of the same curve.
cf_auc <- function(scan) {
  if (!nrow(scan) || !all(c("sensitivity", "specificity") %in% names(scan))) {
    return(NA_real_)
  }
  fpr <- c(1, 1 - scan$specificity, 0)
  tpr <- c(1, scan$sensitivity, 0)
  o <- order(fpr)
  fpr <- fpr[o]; tpr <- tpr[o]
  sum(diff(fpr) * (utils::head(tpr, -1) + utils::tail(tpr, -1)) / 2)
}
