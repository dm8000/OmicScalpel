cf_plot_scan <- function(scan, chosen = NA_real_) {
  if (nrow(scan) == 0) return(NULL)

  eff_name <- attr(scan, "effect_name")
  if (is.null(eff_name)) eff_name <- "HR"

  # clamp non-finite / non-positive bounds to finite effect range
  finite_eff <- scan$effect[is.finite(scan$effect) & scan$effect > 0]
  lo <- min(finite_eff, na.rm = TRUE)
  hi <- max(finite_eff, na.rm = TRUE)
  if (is.na(lo) || is.na(hi)) return(NULL)
  lo <- max(lo, 1e-6)
  hi <- max(hi, lo)

  scan$lower <- pmax(scan$lower, lo, na.rm = TRUE)
  scan$upper <- pmin(scan$upper, hi, na.rm = TRUE)
  scan$lower[!is.finite(scan$lower)] <- lo
  scan$upper[!is.finite(scan$upper)] <- hi
  scan$effect[!is.finite(scan$effect)] <- lo

  p <- ggplot(scan, aes(x = cutoff)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = OS_PLOT$accent, alpha = 0.25) +
    geom_line(aes(y = effect), colour = OS_PLOT$accent) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = OS_PLOT$muted) +
    scale_y_log10() +
    labs(y = paste0(eff_name, " (95% CI)"), x = "Cutoff") +
    os_theme()

  if (is.finite(chosen)) {
    p <- p + geom_vline(xintercept = chosen, linetype = "dashed", colour = OS_PLOT$accent)
  }
  p
}

cf_plot_km <- function(x, time, event, cut, var_name = "variable") {
  # drop non-finite rows
  keep <- is.finite(x) & is.finite(time) & is.finite(event)
  if (sum(keep) < 2) return(NULL)
  x <- x[keep]; time <- time[keep]; event <- event[keep]

  grp <- cf_split(x, cut)
  if (length(unique(grp)) < 2) return(NULL)

  fit <- tryCatch(
    survival::survfit(survival::Surv(time, event) ~ grp),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  # build data.frame from fit
  km <- data.frame(
    time = fit$time,
    surv = fit$surv,
    strata = rep(names(fit$strata), fit$strata)
  )
  km <- km[is.finite(km$time) & is.finite(km$surv), ]
  if (nrow(km) == 0) return(NULL)

  # survfit names its strata "grp=low"/"grp=high", with no spaces. Guessing
  # that name instead of reading it left the legend showing the raw stratum.
  # The factor is ordered too: as character, ggplot sorts alphabetically and
  # "high" would come first.
  lv  <- names(fit$strata)
  n   <- c(low = sum(grp == "low"), high = sum(grp == "high"))
  lab <- setNames(paste0(sub("^grp=", "", lv), " (n=", n[sub("^grp=", "", lv)], ")"), lv)
  km$strata <- factor(km$strata, levels = lv)

  ggplot(km, aes(x = time, y = surv, colour = strata)) +
    geom_step() +
    scale_colour_manual(values = os_palette(2), labels = lab) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(colour = NULL, x = "Time", y = "Survival probability") +
    os_theme() +
    theme(legend.position = "bottom")
}

cf_plot_roc <- function(scan, chosen = NA_real_) {
  if (!"sensitivity" %in% names(scan)) return(NULL)

  roc <- data.frame(
    fp = 1 - scan$specificity,
    tp = scan$sensitivity,
    cutoff = scan$cutoff
  )
  roc <- roc[order(roc$fp), ]
  roc <- rbind(data.frame(fp = 0, tp = 0, cutoff = NA_real_),
               roc,
               data.frame(fp = 1, tp = 1, cutoff = NA_real_))

  auc <- round(cf_auc(scan), 3)

  p <- ggplot(roc, aes(x = fp, y = tp)) +
    geom_line(colour = OS_PLOT$accent) +
    # geom_abline, not geom_segment: a segment inherits the plot data and draws
    # one dashed line per candidate cutpoint, which is a fan, not a diagonal.
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = OS_PLOT$grid) +
    labs(x = "1 - Specificity", y = "Sensitivity", subtitle = paste0("AUC = ", auc)) +
    coord_equal() +
    os_theme()

  if (is.finite(chosen)) {
    idx <- which(scan$cutoff == chosen)
    if (length(idx) > 0) {
      p <- p + geom_point(
        data = data.frame(fp = 1 - scan$specificity[idx], tp = scan$sensitivity[idx]),
        aes(x = fp, y = tp), colour = OS_PLOT$accent, size = 3
      )
    }
  }
  p
}

cf_plot_mixture <- function(x, dist, var_name = "variable") {
  if (is.null(dist)) return(NULL)
  if (is.null(dist$grid) || is.null(dist$d1) || is.null(dist$d2)) return(NULL)
  if (!identical(length(dist$grid), length(dist$d1)) ||
      !identical(length(dist$grid), length(dist$d2))) return(NULL)

  ggplot() +
    geom_histogram(aes(x = x, y = after_stat(density)), bins = 30,
                   fill = OS_PLOT$grid, colour = "white") +
    geom_line(data = data.frame(x = dist$grid, y = dist$d1),
              aes(x = x, y = y), colour = os_palette(2)[1]) +
    geom_line(data = data.frame(x = dist$grid, y = dist$d2),
              aes(x = x, y = y), colour = os_palette(2)[2]) +
    geom_vline(xintercept = dist$cutoff, linetype = "dashed", colour = OS_PLOT$accent) +
    labs(x = var_name, y = "Density") +
    os_theme()
}