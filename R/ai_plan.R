# R/ai_plan.R -- from a question to a plot, algorithmically.
#
# Jev answers typed questions about what the researcher asked. Everything after
# that is here, and it is ordinary R: filter the catalog, check the gene is
# really in the matrices, count what survived, pick the tool that fits, fill in
# its controls. No model chooses a tool and none writes code.
#
# The trace is built as the decision is made, not written afterwards, so what
# the tab shows is what actually happened -- including when the answer is that
# nothing in the hub can answer the question.

# Below this, a choice is not acted on: the tab shows the alternatives and asks.
AI_MIN_CONFIDENCE <- 0.55

# --- the questions -----------------------------------------------------------

AI_INTENTS <- list(
  association_continuous =
    "whether a gene's expression tracks a continuous measurement, such as BMI, age or a score",
  comparison_groups =
    "whether a gene's expression differs between groups of samples, such as obese versus lean, or treated versus control",
  survival =
    "whether a gene's expression predicts survival or an outcome over time",
  expression_level =
    "whether a gene is expressed at all in some samples, and how much -- no comparison between groups is asked for",
  distribution =
    "how a variable is distributed, or where to cut it into high and low",
  catalog =
    "which datasets exist, or what data the collection holds, with no comparison asked for"
)

AI_FOLLOWUP <- list(
  new    = "a new question, unrelated to what was asked before",
  refine = "the same question again, with a detail changed",
  split  = "the previous question asked again separately within each group of a variable, such as within each sex"
)

ai_questions <- function(catalog, question, genes, history = list()) {
  spec <- ai_facet_spec()

  q <- list(
    answerable = list(
      type = "noul",
      instructions = paste("Is this a question about biological measurements -- gene",
                           "expression, a clinical variable, or a group of samples --",
                           "that a dataset could answer?")),
    intent = list(
      type = "choice",
      instructions = "What is the question asking for?",
      criteria = AI_INTENTS)
  )

  # One question per declared facet. config/ai-facets.txt is the list, so a
  # facet the lab starts recording tomorrow becomes askable by editing a text
  # file rather than four R files.
  for (fs in spec) {
    crit <- ai_facet_criteria(catalog, fs)
    if (!length(crit)) next
    q[[fs$id]] <- list(type = "choice", instructions = fs$asks,
                       criteria = c(crit, AI_OTHER,
                                    list(any = "the question does not say")))
  }

  # A question can name something to leave OUT -- "tissues other than adipose"
  # -- and a positive choice cannot express that. Without this the model was
  # asked which tissue the question was about and answered "adipose" at 0.16
  # confidence, because the honest answer was not on offer.
  ex <- ai_exclusion_criteria(catalog, spec)
  if (length(ex)) {
    q$exclude <- list(
      type = "choice",
      instructions = paste(
        "Some questions ask about samples that are NOT something: \"in tissues",
        "other than adipose\", \"outside the brain\", \"anywhere except plasma\",",
        "\"besides white fat\". Does this question do that? If it does, which value",
        "is being ruled out?"),
      criteria = c(ex, list(
        none = "the question names what it wants and rules nothing out")))
  }

  q$variable <- list(
    type = "choice",
    instructions = paste("Which measured variable does the question compare gene",
                         "expression against, or group the samples by?"),
    criteria = c(ai_variable_criteria(catalog),
                 list(none = "the question names no such variable")))

  if (nrow(genes)) {
    q$gene <- list(
      type = "choice",
      instructions = "Which gene is the question about?",
      criteria = c(stats::setNames(
        as.list(paste0("the gene ", genes$symbol,
                       ifelse(genes$how == "synonym", ", matched from its protein name", ""))),
        genes$symbol),
        list(none = "none of these is the gene asked about")))
  }
  if (length(history)) {
    q$followup <- list(
      type = "choice",
      instructions = "How does this question relate to the earlier ones?",
      criteria = AI_FOLLOWUP)
    # Only categorical columns: a split is into groups, and turning a number
    # into groups is the cutoff tab's job, not this one's. It also keeps the
    # request from carrying the same 120 options twice.
    q$split_by <- list(
      type = "choice",
      instructions = paste("If the question asks for the analysis to be done separately",
                           "within groups, which variable defines those groups?"),
      criteria = c(ai_variable_criteria(catalog, kinds = "categorical"),
                   list(none = "the question asks for no such split")))
  }
  q
}

# The values a facet takes across the catalog, described from its families file
# when it has one.
ai_facet_criteria <- function(catalog, fs) {
  fam <- ai_families(fs$families)
  raw <- unlist(lapply(catalog, function(d) ai_facet_of(d, fs$id)), use.names = FALSE)
  v <- sort(unique(ai_family_of(raw, fam$family)))
  if (!length(v)) return(list())
  stats::setNames(as.list(ifelse(is.na(fam$about[v]),
                                 paste0("the value \"", v, "\""),
                                 unname(fam$about[v]))), v)
}

# Everything that could be excluded, labelled with the facet it belongs to so
# the decision knows which filter to drop.
ai_exclusion_criteria <- function(catalog, spec, max_options = 40L) {
  out <- list()
  for (fs in spec) {
    for (v in names(ai_facet_criteria(catalog, fs))) {
      out[[paste0(fs$id, ":", v)]] <- paste0("samples that are NOT ", v)
    }
  }
  utils::head(out, max_options)
}

# A dataset's values for a facet. Catalogs built by hand in the tests carry the
# old flat fields, so both shapes are read.
ai_facet_of <- function(entry, id) {
  v <- entry$facets[[id]]
  if (!is.null(v)) return(v)
  legacy <- c(species = "species", tissue = "tissue", measurement = "data_type",
              cell_type = "cell_type")
  if (!is.na(legacy[id])) entry[[legacy[[id]]]] %||% character(0) else character(0)
}

# "other" has to be offered, or a question about an organism nobody collected
# is answered with the nearest one on the list.
AI_OTHER <- list(other = "an organism, tissue or measurement that is not in this collection")

# One facet filter. Three rules, each of them forced by what this metadata is:
#
#   "other"  -- the question names something the collection does not have, so
#               nothing can match and saying so is the answer.
#   unsure   -- a low-confidence guess does not get to exclude data. The model
#               read "expression" as RNAseq at 0.59 confidence, which would
#               have silently dropped every microarray dataset.
#   silence  -- a dataset that records nothing for a facet is not excluded by
#               it. Nine of eighteen record no tissue at all, and not saying
#               is not the same as saying something else.
ai_filter_facet <- function(keep, catalog, fs, answer, label, min_confidence, tr) {
  if (is.null(answer)) return(list(keep = keep, tr = tr))
  value <- jev_value(answer)
  conf  <- jev_confidence(answer)

  if (identical(value, "any")) {
    return(list(keep = keep,
                tr = ai_trace(tr, label, "not specified, so nothing is excluded",
                              length(keep))))
  }
  if (identical(value, "other")) {
    return(list(keep = character(0),
                tr = ai_trace(tr, label,
                              "the question names one this collection does not hold", 0L)))
  }
  if (is.na(conf) || conf < min_confidence) {
    return(list(keep = keep,
                tr = ai_trace(tr, label,
                              sprintf("%s, but only %.0f%% sure, so nothing was excluded on it",
                                      value, 100 * conf), length(keep))))
  }

  fam <- ai_families(fs$families)$family
  before <- length(keep)
  has <- vapply(catalog[keep], function(d) length(ai_facet_of(d, fs$id)) > 0, logical(1))
  hit <- vapply(catalog[keep],
                function(d) value %in% ai_family_of(ai_facet_of(d, fs$id), fam), logical(1))
  out <- keep[!has | hit]
  silent <- sum(!has)
  list(keep = out,
       tr = ai_trace(tr, label,
                     sprintf("%s: %d of %d%s", value, length(out), before,
                             if (silent) sprintf(", %d of them recording none", silent) else ""),
                     length(out)))
}

# What the trace calls each facet.
ai_facet_label <- function(id) {
  known <- c(species = "Organism", tissue = "Tissue", cell_type = "Cell type",
             region = "Region", measurement = "Measurement", setting = "Tissue or culture")
  if (!is.na(known[id])) unname(known[id]) else
    paste0(toupper(substr(id, 1, 1)), gsub("_", " ", substring(id, 2)))
}

# --- the trace ---------------------------------------------------------------

ai_trace_new <- function() list()

ai_trace <- function(tr, step, detail, kept = NA_integer_) {
  tr[[length(tr) + 1L]] <- list(step = step, detail = detail, kept = kept)
  tr
}

# --- the decision ------------------------------------------------------------

ai_refuse <- function(trace, headline) {
  list(ok = FALSE, kind = "none", headline = headline, trace = trace)
}

ai_ambiguous <- function(trace, headline, options) {
  list(ok = FALSE, kind = "ambiguous", headline = headline, options = options,
       trace = trace)
}

# answers : what Jev returned
# catalog : ai_catalog()
# genes   : the candidate table ai_gene_candidates() produced
# previous: the last plan that worked, so a follow-up does not have to repeat
# what it is about. "Does that differ between the sexes?" names no gene and no
# variable, and without this it would be refused for saying too little.
ai_decide <- function(answers, catalog, genes, index = ai_gene_index(),
                      min_confidence = AI_MIN_CONFIDENCE, previous = NULL) {
  tr <- ai_trace_new()

  # 0.7, not 0.5: a probability near a half is the model saying it does not
  # know, and "I do not know" must not read as yes. A zebrafish question came
  # back at 0.52 and was carried all the way to a plot of human adipose.
  answerable <- jev_value(answers$answerable)
  if (isTRUE(answerable < 0.7)) {
    tr <- ai_trace(tr, "Can measurements answer this?",
                   sprintf("%.0f%% -- too close to a coin toss to act on",
                           100 * answerable))
    return(ai_refuse(tr, paste("I am not confident this is a question these",
                               "datasets can answer.")))
  }

  intent <- jev_value(answers$intent)
  tr <- ai_trace(tr, "What is being asked",
                 sprintf("%s (confidence %.2f)", intent, jev_confidence(answers$intent)))
  if (jev_confidence(answers$intent) < min_confidence) {
    return(ai_ambiguous(tr, "I could not tell what kind of question this is.",
                        jev_runners_up(answers$intent)))
  }

  # --- the facets
  keep <- names(catalog)
  spec <- ai_facet_spec()

  # What the question wants left out, if anything: "facet:value", or none.
  excl <- if (is.null(answers$exclude)) "none" else jev_value(answers$exclude)
  excl_conf <- jev_confidence(answers$exclude)
  if (!identical(excl, "none") && !is.na(excl_conf) && excl_conf >= min_confidence) {
    bits <- strsplit(excl, ":", fixed = TRUE)[[1]]
    excl_facet <- bits[1]; excl_value <- paste(bits[-1], collapse = ":")
  } else {
    excl <- "none"; excl_facet <- NA_character_; excl_value <- NA_character_
  }

  for (fs in spec) {
    if (identical(fs$id, excl_facet)) {
      # The question is about what this facet is NOT. Filtering positively on
      # it as well would ask for both at once.
      before <- length(keep)
      fam <- ai_families(fs$families)$family
      has <- vapply(catalog[keep], function(d) length(ai_facet_of(d, fs$id)) > 0, logical(1))
      hit <- vapply(catalog[keep],
                    function(d) excl_value %in% ai_family_of(ai_facet_of(d, fs$id), fam),
                    logical(1))
      keep <- keep[!hit]
      tr <- ai_trace(tr, paste0("Not ", excl_value),
                     sprintf("the question asks for samples that are not %s: %d of %d left%s",
                             excl_value, length(keep), before,
                             if (any(!has)) sprintf(", including %d that record none", sum(!has & !hit)) else ""),
                     length(keep))
      next
    }
    ans <- answers[[fs$id]]
    if (is.null(ans)) next
    # A much higher bar for the platform than for the organism or the tissue.
    # "Does leptin expression increase with BMI" names no technique, and
    # reading "expression" as RNAseq at 59% confidence threw away a microarray
    # dataset that had both the gene and the variable.
    bar <- if (identical(fs$id, "measurement")) max(min_confidence, 0.8) else min_confidence
    f <- ai_filter_facet(keep, catalog, fs, ans, ai_facet_label(fs$id), bar, tr)
    keep <- f$keep; tr <- f$tr
  }

  # --- what a follow-up inherits
  followup <- if (is.null(answers$followup)) "new" else jev_value(answers$followup)
  split_by <- if (is.null(answers$split_by)) "none" else jev_value(answers$split_by)
  if (!identical(followup, "split")) split_by <- "none"
  inherited <- character(0)

  # --- the variable
  variable <- jev_value(answers$variable)
  if (identical(variable, "none") && !identical(followup, "new") && !is.null(previous)) {
    variable <- ai_previous_variable(previous)
    if (!is.null(variable)) inherited <- c(inherited, paste0("variable ", variable))
    else variable <- "none"
  }
  if (!identical(variable, "none") &&
      jev_confidence(answers$variable) < min_confidence) {
    return(ai_ambiguous(tr, "More than one variable fits the question equally well.",
                        jev_runners_up(answers$variable)))
  }

  # Everything from the variable onwards, so it can be tried again with the
  # model's second choice. That is not guessing twice: a choice answer is a
  # distribution, and when the most likely reading leaves no data the next one
  # is the model's own opinion, not ours.
  attempt <- function(variable, keep, tr) {
  var_kind <- NA_character_
  if (!identical(variable, "none")) {
    before <- length(keep)
    keep <- keep[vapply(catalog[keep], function(d) variable %in% names(d$variables), logical(1))]
    kinds <- unique(vapply(catalog[keep], function(d) d$variables[[variable]]$kind,
                           character(1)))
    var_kind <- if (length(kinds)) kinds[1] else NA_character_
    tr <- ai_trace(tr, "Variable",
                   sprintf("%s, %s, filled in %d of %d datasets", variable,
                           ifelse(is.na(var_kind), "unknown type", var_kind),
                           length(keep), before),
                   length(keep))
  } else {
    tr <- ai_trace(tr, "Variable", "the question names none", length(keep))
  }

  # --- the gene
  gene <- if (is.null(answers$gene)) "none" else jev_value(answers$gene)
  if (identical(gene, "none") && !identical(followup, "new") && !is.null(previous)) {
    g <- ai_previous_gene(previous)
    if (!is.null(g)) { gene <- g; inherited <- c(inherited, paste0("gene ", g)) }
  }
  if (length(inherited)) {
    tr <- ai_trace(tr, "Carried over",
                   paste0("from the previous question: ", paste(inherited, collapse = ", ")))
  }
  needs_gene <- intent %in% c("association_continuous", "comparison_groups",
                              "survival", "expression_level")
  if (identical(gene, "none") && needs_gene) {
    tr <- ai_trace(tr, "Gene",
                   if (nrow(genes)) paste0("none of the ", nrow(genes),
                                           " candidates matched the question")
                   else "no gene symbol could be found in the question", 0L)
    return(ai_refuse(tr, "I could not tell which gene the question is about."))
  }
  if (!identical(gene, "none")) {
    how <- genes$how[match(gene, genes$symbol)]
    if (is.null(index)) {
      # No index means the question cannot be told a gene is absent. Refusing
      # here would report "no dataset has it" when the truth is "nobody looked",
      # so the search goes on and says so.
      tr <- ai_trace(tr, "Gene",
                     sprintf("%s (%s); not checked against the matrices -- no gene index, run tools/build-gene-index.R",
                             gene, if (is.na(how)) "chosen" else how),
                     length(keep))
    } else {
      before <- length(keep)
      keep <- intersect(keep, ai_datasets_with_gene(gene, index))
      tr <- ai_trace(tr, "Gene",
                     sprintf("%s (%s); in %d of %d remaining datasets", gene,
                             if (is.na(how)) "chosen" else how, length(keep), before),
                     length(keep))
    }
  }

  if (!length(keep)) {
    # Name the step that emptied it. "Nothing meets all of those" sends the
    # researcher to read the whole trace to find out which one it was.
    culprit <- NULL
    for (st in tr) if (isTRUE(st$kept == 0L)) { culprit <- st; break }
    return(ai_refuse(tr, if (is.null(culprit))
      "Nothing in the collection meets all of those at once."
      else sprintf("Nothing is left once %s is required: %s",
                   tolower(culprit$step), culprit$detail)))
  }


  ai_pick_tool(intent, variable, var_kind, gene, keep, catalog, split_by, tr, index)
  }

  plan <- attempt(variable, keep, tr)

  # If the reading the model was surest of leaves nothing, try the next one it
  # ranked. "Obesity" is the case this exists for: Obesity.status is filled in
  # one dataset and that one measures no genes, while BMI -- the model's second
  # choice -- is in five.
  if (isFALSE(plan$ok) && identical(plan$kind, "none") && !identical(variable, "none")) {
    for (alt in setdiff(jev_runners_up(answers$variable, 3L), c(variable, "none"))) {
      tr2 <- ai_trace(tr, "Second reading",
                      sprintf("with %s nothing was left, so trying %s, which the model ranked next",
                              variable, alt))
      alt_plan <- attempt(alt, keep, tr2)
      if (isTRUE(alt_plan$ok)) { plan <- alt_plan; break }
    }
  }
  plan
}

# Which tab, and what to set in it. The only place that decides this.
ai_pick_tool <- function(intent, variable, var_kind, gene, keep, catalog,
                         split_by, tr, index = ai_gene_index()) {
  pick_one <- function() {
    # The biggest cohort, because with one dataset to choose the estimate that
    # rests on more samples is the one worth showing.
    # numeric(1), not integer(1): nrow() gives an integer but a catalog built
    # any other way gives a double, and vapply refuses the mismatch outright.
    sizes <- vapply(catalog[keep], function(d) as.numeric(d$n), numeric(1))
    names(sizes)[which.max(sizes)]
  }

  if (identical(intent, "catalog")) {
    tr <- ai_trace(tr, "Tool", "the question asks what exists, not for a comparison")
    return(ai_plan_out("data_summary", pick_one(), keep, list(), tr,
                       sprintf("%d dataset%s match. Here is what they hold.",
                               length(keep), if (length(keep) == 1) "" else "s")))
  }

  # "Is this gene expressed here, and how much?" -- the commonest question
  # there is, and the one the intent list did not have. It needs no variable
  # and no second group: it needs the value, in each dataset, beside a ruler.
  if (identical(intent, "expression_level")) {
    controls <- list(genes = gene, datasets = keep, housekeeping = TRUE)
    if (!identical(split_by, "none")) controls$split_col <- split_by
    tr <- ai_trace(tr, "Tool",
                   sprintf("%s shown in each of %d dataset%s on its own -- they are not comparable with each other",
                           gene, length(keep), if (length(keep) == 1) "" else "s"))
    return(ai_plan_out("across_datasets", NULL, keep, controls, tr,
                       sprintf("%s in %d dataset%s, each on its own scale.", gene,
                               length(keep), if (length(keep) == 1) "" else "s")))
  }

  if (intent %in% c("survival", "distribution")) {
    d <- pick_one()
    surv <- ai_survival_columns(catalog[[d]])
    if (identical(intent, "survival") && is.null(surv)) {
      tr <- ai_trace(tr, "Tool",
                     "survival was asked for, but no dataset here records a time and an event")
      return(ai_refuse(tr, "No dataset with survival data matches the question."))
    }
    controls <- list(source = if (identical(gene, "none")) "meta" else "gene",
                     method = if (identical(intent, "survival")) "survival" else "distribution")
    if (identical(controls$source, "gene")) {
      controls$gene <- ai_gene_as_spelled(gene, d, index)
      controls$unit <- catalog[[d]]$units[1]
    } else if (!identical(variable, "none")) {
      controls$meta_col <- variable
    }
    if (identical(intent, "survival")) {
      controls$time_col  <- surv$time
      controls$event_col <- surv$event
    }
    if (!identical(split_by, "none")) controls$split_col <- split_by
    tr <- ai_trace(tr, "Tool", sprintf("cutoff finder on %s", d))
    return(ai_plan_out("cutoff_finder", d, keep, controls, tr,
                       sprintf("Looking for the cutpoint in %s.", d)))
  }

  # association with a continuous variable, or comparison between groups
  if (identical(variable, "none")) {
    d <- pick_one()
    tr <- ai_trace(tr, "Tool",
                   "no variable to compare against, so the gene is shown as it is")
    controls <- list(genes = ai_gene_as_spelled(gene, d, index))
    if (!identical(split_by, "none")) controls$conditions <- split_by
    return(ai_plan_out("compare_genes", d, keep, controls, tr,
                       sprintf("%s in %s.", gene, d)))
  }

  if (length(keep) > 1L) {
    controls <- list(biomolecule = gene, condition = variable,
                     data_preference = ai_common_unit(keep, catalog))
    if (identical(var_kind, "categorical")) {
      lv <- ai_two_levels(variable, keep, catalog)
      if (is.null(lv)) {
        tr <- ai_trace(tr, "Tool",
                       paste0(variable, " has fewer than two groups in these datasets"))
        return(ai_refuse(tr, "That variable does not split these datasets into two groups."))
      }
      controls$group1_categories <- lv[1]
      controls$group2_categories <- lv[2]
      tr <- ai_trace(tr, "Groups", sprintf("%s versus %s", lv[1], lv[2]))
    } else {
      controls$numeric_split <- "median"
      tr <- ai_trace(tr, "Groups",
                     sprintf("%s split at its median within each dataset", variable))
    }
    if (!identical(split_by, "none")) {
      controls$adjust_for <- split_by
      tr <- ai_trace(tr, "Split",
                     sprintf("%s enters the model as a covariate, so the effect is the one within groups",
                             split_by))
    }
    tr <- ai_trace(tr, "Tool",
                   sprintf("%d datasets qualify, so this is a meta-analysis", length(keep)))
    return(ai_plan_out("meta_analysis", NULL, keep, controls, tr,
                       sprintf("%s against %s across %d datasets.", gene, variable,
                               length(keep))))
  }

  d <- keep[1]
  if (identical(var_kind, "numeric")) {
    controls <- list(genes = ai_gene_as_spelled(gene, d, index), numeric_columns = variable)
    if (!identical(split_by, "none")) controls$conditions <- split_by
    tr <- ai_trace(tr, "Tool", sprintf("one dataset (%s) and a continuous variable", d))
    return(ai_plan_out("correlation_analysis", d, keep, controls, tr,
                       sprintf("%s against %s in %s.", gene, variable, d)))
  }

  controls <- list(genes = ai_gene_as_spelled(gene, d, index),
                   conditions = unique(c(variable, if (!identical(split_by, "none")) split_by)))
  tr <- ai_trace(tr, "Tool", sprintf("one dataset (%s) and groups of samples", d))
  ai_plan_out("compare_samples", d, keep, controls, tr,
              sprintf("%s between %s groups in %s.", gene, variable, d))
}

# Where each tab keeps the gene and the variable, so a follow-up can read them
# back out of the plan that produced the last plot.
ai_previous_gene <- function(previous) {
  c(previous$controls$biomolecule, previous$controls$genes,
    previous$controls$gene)[1] %||% NULL
}

ai_previous_variable <- function(previous) {
  c(previous$controls$condition, previous$controls$numeric_columns,
    previous$controls$conditions, previous$controls$meta_col)[1] %||% NULL
}

ai_plan_out <- function(tab, dataset, datasets, controls, trace, headline) {
  list(ok = TRUE, kind = "plot", tab = tab, dataset = dataset,
       datasets = datasets, controls = controls, trace = trace,
       headline = headline)
}

# --- small helpers the decision leans on -------------------------------------

# A dataset can only go to the cutoff finder if it records both a time and an
# event, and nothing guarantees they are named alike.
ai_survival_columns <- function(entry) {
  nm <- names(entry$variables)
  time  <- grep("time", nm, ignore.case = TRUE, value = TRUE)
  event <- grep("event|status|death", nm, ignore.case = TRUE, value = TRUE)
  if (!length(time) || !length(event)) return(NULL)
  list(time = time[1], event = event[1])
}

# The meta-analysis reads one unit across every dataset; a dataset without it
# is skipped. Pick the one the most of them have.
ai_common_unit <- function(keep, catalog) {
  units <- unlist(lapply(catalog[keep], function(d) unique(d$units)), use.names = FALSE)
  if (!length(units)) return("TMM")
  names(sort(table(units), decreasing = TRUE))[1]
}

# Two groups to contrast. With exactly two levels there is no choice to make;
# with more, the two most common, and the trace says so.
ai_two_levels <- function(variable, keep, catalog) {
  lv <- unlist(lapply(catalog[keep], function(d) d$variables[[variable]]$levels),
               use.names = FALSE)
  if (length(unique(lv)) < 2) return(NULL)
  tab <- sort(table(lv), decreasing = TRUE)
  utils::head(names(tab), 2)
}

# --- the whole round ---------------------------------------------------------

# What the tab calls. transport is injectable so every test below the client
# runs without a network.
ai_answer <- function(question, catalog = ai_catalog(), history = list(),
                      transport = jev_post, model = JEV_MODEL,
                      min_confidence = AI_MIN_CONFIDENCE, previous = NULL) {
  genes <- ai_gene_candidates(question)
  qs    <- ai_questions(catalog, question, genes, history)
  state <- ai_state(catalog, question, history)
  res   <- jev_ask(state, qs, model = model, transport = transport,
                   label = substr(question, 1, 60))
  plan  <- ai_decide(res$answers, catalog, genes, min_confidence = min_confidence,
                     previous = previous)
  # Kept so an ambiguity can be resolved without asking again: the model has
  # already answered, and only the answer in question needs replacing.
  plan$answers  <- res$answers
  plan$genes    <- genes
  plan$usage    <- res$usage
  plan$cost     <- res$cost
  plan$question <- question
  plan
}
