# R/meta_stats.R -- the effect sizes the meta-analysis pools.
#
# Pure functions, no Shiny, so they can be checked against an independently
# fitted model instead of against themselves. They used to live inside
# mod_meta_analysis.R, and the test reached them by slicing the source file and
# eval()ing the slice -- which meant the test never opened a session, and the
# tab could be broken in every other respect while it passed. It was.

calculate_cohens_d <- function(group1_values, group2_values) {
  n1 <- length(group1_values)
  n2 <- length(group2_values)
  
  if (n1 < 2 || n2 < 2) {
    return(list(d = NA, se = NA))
  }
  
  mean1 <- mean(group1_values, na.rm = TRUE)
  mean2 <- mean(group2_values, na.rm = TRUE)
  sd1 <- sd(group1_values, na.rm = TRUE)
  sd2 <- sd(group2_values, na.rm = TRUE)
  
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  if (pooled_sd == 0) {
    return(list(d = 0, se = 0))
  }
  
  cohens_d <- (mean2 - mean1) / pooled_sd
  se_d <- sqrt((n1 + n2) / (n1 * n2) + cohens_d^2 / (2 * (n1 + n2 - 2)))
  
  return(list(d = cohens_d, se = se_d))
}

adjusted_effect <- function(expr, group, covars) {
  df <- data.frame(expr = expr, group = factor(group, levels = c("g1", "g2")))
  usable <- character(0)

  for (nm in names(covars)) {
    v <- covars[[nm]]
    num <- suppressWarnings(as.numeric(as.character(v)))
    v <- if (mean(!is.na(num)) > 0.8) num else factor(as.character(v))
    # A covariate that does not vary in this dataset explains nothing and
    # makes the model rank-deficient. Dropped here, reported to the user.
    if (length(unique(v[!is.na(v)])) < 2) next
    df[[nm]] <- v
    usable <- c(usable, nm)
  }

  keep <- stats::complete.cases(df)
  df   <- df[keep, , drop = FALSE]
  if (nlevels(droplevels(df$group)) < 2) return(NULL)
  if (sum(df$group == "g1") < 2 || sum(df$group == "g2") < 2) return(NULL)

  # one parameter per covariate level, plus the intercept and the group
  n_par <- 2 + sum(vapply(usable, function(nm) {
    v <- df[[nm]]; if (is.factor(v)) nlevels(droplevels(v)) - 1 else 1
  }, numeric(1)))
  if (nrow(df) <= n_par + 1) return(NULL)

  fml <- stats::as.formula(
    paste("expr ~ group", if (length(usable)) paste("+", paste(usable, collapse = " + ")) else "")
  )
  fit <- tryCatch(stats::lm(fml, data = df), error = function(e) NULL)
  if (is.null(fit)) return(NULL)

  co <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
  if (is.null(co) || !("groupg2" %in% rownames(co))) return(NULL)

  resid_sd <- stats::sigma(fit)
  if (!is.finite(resid_sd) || resid_sd == 0) return(NULL)

  list(
    d       = unname(co["groupg2", "Estimate"]) / resid_sd,
    se      = unname(co["groupg2", "Std. Error"]) / resid_sd,
    pvalue  = unname(co["groupg2", "Pr(>|t|)"]),
    used    = usable,
    dropped = setdiff(names(covars), usable),
    n1      = sum(df$group == "g1"),
    n2      = sum(df$group == "g2")
  )
}

# --- survival, which is not just another continuous column -------------------

# Follow-up time and a censoring flag are two columns describing one thing, and
# treating the time as an ordinary number is wrong in a way that still draws a
# plot: a short time is either an early death or someone who left the study,
# and splitting at the median puts those two in the same group.
#
# Given the time column, find the event column that goes with it.
os_survival_event_for <- function(time_col, md) {
  if (is.null(time_col) || !nzchar(time_col)) return(NULL)
  if (!grepl("time|fu|follow", time_col, ignore.case = TRUE)) return(NULL)
  stem <- sub("[._ -]?(time|fu|followup|follow.up).*$", "", time_col, ignore.case = TRUE)
  cands <- setdiff(names(md)[startsWith(names(md), stem)], time_col)
  if (!length(cands)) return(NULL)
  # A name that says what it is comes first, but the test is the values.
  says <- grepl("event|status|censor|death|dead", cands, ignore.case = TRUE)
  for (cn in c(cands[says], cands[!says])) {
    v <- suppressWarnings(as.numeric(as.character(md[[cn]])))
    v <- v[!is.na(v)]
    if (length(v) >= 2 && all(v %in% c(0, 1)) && length(unique(v)) == 2) return(cn)
  }
  NULL
}

# One dataset's contribution to a survival meta-analysis: the log hazard ratio
# for expression, and its standard error, from a Cox model.
#
# The expression is standardised within the dataset first. One study reports
# TPM and the next TMM, so a hazard ratio "per unit of expression" means a
# different thing in each and pooling them would be arithmetic on incomparable
# numbers. Per standard deviation is the same quantity everywhere.
cox_effect <- function(expr, time, event) {
  expr  <- suppressWarnings(as.numeric(expr))
  time  <- suppressWarnings(as.numeric(time))
  event <- suppressWarnings(as.numeric(event))

  ok <- is.finite(expr) & is.finite(time) & time > 0 & !is.na(event) & event %in% c(0, 1)
  expr <- expr[ok]; time <- time[ok]; event <- event[ok]
  if (length(expr) < 10 || sum(event) < 3) return(NULL)

  s <- stats::sd(expr)
  if (!is.finite(s) || s == 0) return(NULL)
  z <- (expr - mean(expr)) / s

  fit <- tryCatch(survival::coxph(survival::Surv(time, event) ~ z),
                  error = function(e) NULL, warning = function(w) NULL)
  if (is.null(fit)) return(NULL)
  sm <- summary(fit)
  list(d      = unname(sm$coefficients[1, "coef"]),      # log hazard ratio per SD
       se     = unname(sm$coefficients[1, "se(coef)"]),
       pvalue = unname(sm$coefficients[1, "Pr(>|z|)"]),
       n      = length(expr),
       events = sum(event))
}
