#!/usr/bin/env Rscript
# The decision tree, without a network and without spending anything.
#
#   Rscript tools/behaviour/ai_plan.R
#
# Everything the question tab does after Jev answers is ordinary R, and this is
# where it is held to account. The catalogs here are planted, the answers are
# fabricated, and each case asserts the tool that should come out and the
# controls that should be set -- because "it produced a plan" and "it produced
# the right plan" are different claims.

source("global.R")

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

# --- a hub to reason about ---------------------------------------------------

num <- function(col, lo, hi, n = 50) list(column = col, kind = "numeric", n = n,
                                          range = c(lo, hi), about = col)
cat_ <- function(col, levels, n = 50) list(column = col, kind = "categorical", n = n,
                                           levels = levels, more = 0L, about = col)
ds <- function(name, n, species, tissue, type, units, vars,
               demo = FALSE, recorded = character(0), constant = list()) {
  nm <- vapply(vars, function(v) v$column, character(1))
  list(dataset = name, n = n, species = species, tissue = tissue,
       data_type = type, cell_type = "adipocytes", units = units,
       demo = demo,
       measures_genes = ai_measures_genes(type),
       # columns with a value, varying or not
       recorded = unique(c(nm, recorded)),
       constant = constant,
       variables = stats::setNames(vars, nm))
}

CAT <- list(
  ds("HumanA", 400, "HSA", "Adipose", "RNAseq", c("TPM", "TMM"),
     list(num("BMI", 18, 60), cat_("Sex", c("Female", "Male")))),
  ds("HumanB", 120, "HSA", "Adipose", "RNAseq", "TMM",
     list(num("BMI", 20, 45), cat_("Obesity.status", c("lean", "obese")))),
  # One names a depot rather than the tissue, one records no tissue at all.
  # Both are adipose datasets and both must survive a question about adipose.
  ds("HumanC", 60,  "HSA", "Subcutaneous", "Array", "TMM",
     list(num("BMI", 22, 38))),
  ds("HumanD", 90,  "HSA", character(0), "RNAseq", "TMM",
     list(num("BMI", 21, 52))),
  ds("MouseA", 20,  "Mmu", "BAT", "RNAseq", "TPM",
     list(cat_("Diet", c("chow", "HFD45")),
          num("DEMO.OS.time", 1, 90), cat_("DEMO.OS.event", c("0", "1")))),
  # lipidomics with both sexes, like Bluher and McMaster: it survives every
  # filter until the one that asks whether it measures genes at all
  ds("PlasmaA", 60, "HSA", "Plasma", "Signal.lipidomics", "TMM",
     list(num("BMI", 19, 41), cat_("Sex", c("Female", "Male")))),
  # records Sex, and every sample is Male: nothing to compare
  ds("HumanE", 500, "HSA", "Adipose", "RNAseq", "TMM",
     list(num("BMI", 20, 50)), recorded = "Sex", constant = list(Sex = "Male")),
  # invented data, with the gene nothing else has
  ds("DemoZ", 100, "HSA", "Adipose", "RNAseq", "TPM",
     list(cat_("Sex", c("female", "male"))), demo = TRUE)
)
names(CAT) <- vapply(CAT, function(d) d$dataset, character(1))

INDEX <- list(built = Sys.time(), datasets = list(
  HumanA = c("LEP", "UCP1", "ADIPOQ"), HumanB = c("LEP", "UCP1"),
  HumanC = c("LEP"), HumanD = c("LEP"), MouseA = c("Ucp1", "LEP"),
  PlasmaA = c("LIPID1"), HumanE = c("LEP"), DemoZ = c("LEP", "DEMOONLY1")))

GENES <- data.frame(symbol = c("LEP", "LEPR"), how = c("synonym", "prefix"),
                    stringsAsFactors = FALSE)

# --- fabricated answers ------------------------------------------------------

choice <- function(value, conf = 0.9, others = character(0), confidence = conf) {
  p <- stats::setNames(as.list(c(confidence, rep((1 - confidence) / max(1, length(others)),
                                                 length(others)))),
                       c(value, others))
  list(type = "choice", choice = value, probabilities = p, confidence = confidence)
}
noul <- function(p) list(type = "noul", noul = p)

answers <- function(...) {
  base <- list(answerable = noul(0.95), intent = choice("association_continuous"),
               species = choice("HSA"), tissue = choice("adipose"),
               variable = choice("BMI"), gene = choice("LEP"))
  utils::modifyList(base, list(...))
}

plan_of <- function(...) ai_decide(answers(...), CAT, GENES, index = INDEX)

# --- several datasets: the forest plot ---------------------------------------

p <- plan_of()
chk(isTRUE(p$ok) && p$tab == "meta_analysis",
    "a continuous variable in several datasets goes to the meta-analysis",
    p$tab, " ", p$headline)
chk(setequal(p$datasets, c("HumanA", "HumanB", "HumanC", "HumanD", "HumanE")),
    "and only the datasets that pass every filter",
    paste(p$datasets, collapse = ", "))
chk("HumanC" %in% p$datasets,
    "a dataset that names a depot counts as that tissue")
chk("HumanD" %in% p$datasets,
    "and one that records no tissue is not excluded by a tissue filter")
chk(identical(p$controls$biomolecule, "LEP") &&
    identical(p$controls$condition, "BMI") &&
    identical(p$controls$numeric_split, "median"),
    "with the gene, the variable and a median split filled in",
    paste(names(p$controls), unlist(p$controls), collapse = "; "))
chk(identical(p$controls$data_preference, "TMM"),
    "and the unit the most of them share", p$controls$data_preference)

# The plasma lipidomics dataset has BMI and is human, and must still be gone:
# it carries 110 lipids and no genes.
chk(!"PlasmaA" %in% p$datasets, "a dataset without the gene is dropped even so")

# --- one dataset: the correlation --------------------------------------------

p1 <- plan_of(gene = choice("ADIPOQ"))
chk(isTRUE(p1$ok) && p1$tab == "correlation_analysis",
    "a continuous variable in a single dataset goes to the correlation", p1$tab)
chk(identical(p1$dataset, "HumanA") &&
    identical(p1$controls$genes, "ADIPOQ") &&
    identical(p1$controls$numeric_columns, "BMI"),
    "on that dataset, with the gene on one axis and the variable on the other",
    p1$dataset)

# --- a categorical variable ---------------------------------------------------

p2 <- plan_of(variable = choice("Obesity.status"), intent = choice("comparison_groups"))
chk(isTRUE(p2$ok) && p2$tab == "compare_samples",
    "groups in a single dataset go to compare samples", p2$tab)
chk(identical(p2$dataset, "HumanB") && identical(p2$controls$conditions, "Obesity.status"),
    "grouped by the column the question named", p2$dataset)

# --- survival -----------------------------------------------------------------

p3 <- plan_of(intent = choice("survival"), species = choice("Mmu"),
              tissue = choice("adipose"), variable = choice("none"))
chk(isTRUE(p3$ok) && p3$tab == "cutoff_finder", "survival goes to the cutoff finder", p3$tab)
chk(identical(p3$controls$time_col, "DEMO.OS.time") &&
    identical(p3$controls$event_col, "DEMO.OS.event") &&
    identical(p3$controls$source, "gene"),
    "with the time and event columns it found in that dataset",
    paste(unlist(p3$controls), collapse = " "))

# --- survival in more than one cohort -----------------------------------------
# One cohort is a cutpoint search; several is a meta-analysis of hazard ratios.
# Sending the second case to the first throws away every cohort but the biggest.
CAT2 <- CAT
CAT2$MouseB <- ds("MouseB", 40, "Mmu", "BAT", "RNAseq", "TPM",
                  list(num("DEMO.OS.time", 1, 80), cat_("DEMO.OS.event", c("0", "1"))))
INDEX2 <- INDEX; INDEX2$datasets$MouseB <- c("LEP", "Ucp1")

psm <- ai_decide(answers(intent = choice("survival"), species = choice("Mmu"),
                         tissue = choice("any"), variable = choice("none")),
                 CAT2, GENES, index = INDEX2)
chk(isTRUE(psm$ok) && identical(psm$tab, "meta_analysis"),
    "survival in two cohorts pools hazard ratios instead of cutting one",
    psm$tab, " / ", psm$headline)
chk(identical(psm$controls$condition, "DEMO.OS.time"),
    "with the follow-up time as the condition, which is what triggers the Cox path",
    psm$controls$condition)
chk(length(psm$datasets) == 2, "and both cohorts in it", length(psm$datasets))

# --- the follow-up: split by a group -----------------------------------------

p4 <- ai_decide(answers(gene = choice("ADIPOQ"), followup = choice("split"),
                        split_by = choice("Sex")), CAT, GENES, index = INDEX)
chk(identical(p4$tab, "correlation_analysis") &&
    identical(p4$controls$conditions, "Sex"),
    "asking for it by sex adds sex as the grouping, not a new question",
    paste(unlist(p4$controls), collapse = " "))

p5 <- ai_decide(answers(followup = choice("split"), split_by = choice("Sex")),
                CAT, GENES, index = INDEX)
chk(identical(p5$tab, "meta_analysis") && identical(p5$controls$adjust_for, "Sex"),
    "and in a meta-analysis it becomes the covariate the forest plot adjusts for",
    paste(names(p5$controls), collapse = ", "))

# A split is only honoured when the question asked for one.
p6 <- ai_decide(answers(followup = choice("new"), split_by = choice("Sex")),
                CAT, GENES, index = INDEX)
chk(is.null(p6$controls$adjust_for), "a new question does not inherit the previous split")

# --- one gene, two spellings --------------------------------------------------
# Human matrices spell it UCP1 and mouse ones Ucp1. A question about the mouse
# gene must find the mouse datasets, and the plan must name the symbol the way
# that dataset spells it, because the tabs match it exactly.
pm <- ai_decide(answers(species = choice("Mmu"), tissue = choice("any"),
                        variable = choice("Diet"), gene = choice("UCP1"),
                        intent = choice("comparison_groups")),
                CAT, data.frame(symbol = "UCP1", how = "exact",
                                stringsAsFactors = FALSE), index = INDEX)
chk(isTRUE(pm$ok) && identical(pm$dataset, "MouseA"),
    "a human-spelled symbol still finds the mouse dataset", pm$headline)
chk(identical(pm$controls$genes, "Ucp1"),
    "and the plan names it the way that dataset spells it", pm$controls$genes)

# --- a follow-up says almost nothing and must still work ----------------------
# "Does that differ between the sexes?" names no gene and no variable. Without
# carrying them over from the last plan it would be refused for saying too
# little, which is exactly what a researcher would not expect.
vague <- list(answerable = noul(0.9), intent = choice("association_continuous"),
              species = choice("HSA"), tissue = choice("adipose"),
              variable = choice("none"), gene = choice("none"),
              followup = choice("split"), split_by = choice("Sex"))
p11 <- ai_decide(vague, CAT, GENES[0, ], index = INDEX, previous = p1)
chk(isTRUE(p11$ok) && identical(p11$controls$genes, "ADIPOQ") &&
    identical(p11$controls$numeric_columns, "BMI"),
    "a follow-up carries the gene and the variable of the last answer",
    p11$headline)
chk(identical(p11$controls$conditions, "Sex"), "and adds the split it asked for")
chk(any(grepl("Carried over", vapply(p11$trace, function(s) s$step, character(1)))),
    "and the trace says what was carried over")

p12 <- ai_decide(utils::modifyList(vague, list(followup = choice("new"))),
                 CAT, GENES[0, ], index = INDEX, previous = p1)
chk(isFALSE(p12$ok), "but a question announced as new inherits nothing")

# --- what the collection is, said out loud ------------------------------------
# Two questions about sex landed on the same dataset whatever the gene was.
# That was not the router preferring it: it is the only one that records sex
# varying and measures genes. The trace has to say each of those things, and
# say them apart.

sx <- plan_of(intent = choice("comparison_groups"), variable = choice("Sex"),
              species = choice("any"), tissue = choice("any"), gene = choice("LEP"))
steps <- vapply(sx$trace, function(s) s$step, character(1))
details <- vapply(sx$trace, function(s) s$detail, character(1))

chk("Demonstration data" %in% steps, "invented data is set aside before anything else",
    paste(steps, collapse = " > "))
chk(!"DemoZ" %in% (sx$datasets %||% character(0)),
    "and does not answer a real question")

chk(any(grepl("record it without varying", details)),
    "a dataset that records the variable without varying is counted apart",
    paste(grep("varies in", details, value = TRUE), collapse = " | "))

chk("Measures genes" %in% steps,
    "lipidomics is ruled out as not measuring genes, not as missing the gene",
    paste(steps, collapse = " > "))
chk(any(grepl("lipid species", details)), "and the trace says so in words")

chk(isTRUE(sx$ok) && length(sx$datasets) == 1 && !is.null(sx$note),
    "when one dataset was the only option the answer says so",
    sx$note %||% "(no note)")
chk(grepl("only dataset", sx$note), "in the answer itself, not just the trace", sx$note)
chk(!grepl("varying", sx$note),
    "in words, not in jargon -- \"records Sex varying\" says nothing to a reader",
    sx$note)
chk(grepl("HumanE", sx$note) && grepl("every one of its 500 samples is Male", sx$note),
    "and it names the dataset that came closest, and what stopped it",
    sx$note)

# --- invented data, when it is the only place the gene exists ------------------
only <- plan_of(intent = choice("expression_level"), variable = choice("none"),
                species = choice("any"), tissue = choice("any"),
                gene = choice("DEMOONLY1"))
chk(isTRUE(only$ok) && isTRUE(only$demo_only),
    "a gene that exists only in invented data still answers", only$headline)
chk(grepl("^Invented data only", only$headline),
    "with the headline saying what it is", only$headline)

# --- "is this expressed here?" ------------------------------------------------
# The commonest question there is, and the one the intent list did not have.
# Both of the questions that failed in use were this, and the model was forced
# to answer `catalog` -- once at 0.40 confidence, which the tab reported as "I
# could not tell what kind of question this is".
pe <- plan_of(intent = choice("expression_level"), variable = choice("none"))
chk(isTRUE(pe$ok) && identical(pe$tab, "across_datasets"),
    "asking whether a gene is expressed goes to Across datasets", pe$tab)
chk(identical(pe$controls$genes, "LEP") && isTRUE(pe$controls$housekeeping),
    "with the gene and the housekeeping ruler",
    paste(names(pe$controls), collapse = ", "))
chk(length(pe$controls$datasets) == length(pe$datasets) && length(pe$datasets) > 1,
    "and every dataset that carries it, each on its own",
    length(pe$datasets))
chk(grepl("not comparable", paste(vapply(pe$trace, function(s) s$detail, character(1)),
                                  collapse = " ")),
    "the trace says the datasets are not comparable with each other")

# --- leaving something out ----------------------------------------------------
# "tissues other than adipose" cannot be said with a positive choice. Asked
# which tissue the question was about, the model answered "adipose" at 0.16 --
# correctly unsure, because the honest answer was not on offer.
px <- ai_decide(answers(intent = choice("expression_level"), variable = choice("none"),
                        tissue = choice("any"), exclude = choice("tissue:adipose")),
                CAT, GENES, index = INDEX)
chk(isTRUE(px$ok) && identical(px$tab, "across_datasets"),
    "excluding a tissue still produces a plot", px$headline)
chk(!"HumanA" %in% px$datasets && !"HumanC" %in% px$datasets,
    "and the adipose datasets are the ones left out",
    paste(px$datasets, collapse = ", "))
chk(any(grepl("^Not ", vapply(px$trace, function(s) s$step, character(1)))),
    "with the exclusion named in the trace")

# An exclusion the model is unsure of excludes nothing, like every other filter.
px2 <- ai_decide(answers(intent = choice("expression_level"), variable = choice("none"),
                         exclude = choice("tissue:adipose", conf = 0.3)),
                 CAT, GENES, index = INDEX)
chk(length(px2$datasets) > length(px$datasets),
    "an unsure exclusion leaves the data alone",
    length(px2$datasets), " vs ", length(px$datasets))

# --- a facet the collection does not have -------------------------------------
# "the stromal vascular fraction" is a cell fraction nobody here collected.
# Saying so is the answer; picking the nearest cell type would not be.
pc <- plan_of(intent = choice("expression_level"), variable = choice("none"),
              cell_type = choice("other"))
chk(isFALSE(pc$ok), "a cell fraction the collection lacks is refused, not approximated")

# --- a facet added without touching code --------------------------------------
# config/ai-facets.txt is the list. This is the check that it really is.
spec <- ai_facet_spec()
chk(all(c("species", "tissue", "cell_type", "region", "measurement", "setting")
        %in% names(spec)),
    "every facet in config/ai-facets.txt is loaded",
    paste(names(spec), collapse = ", "))
qs <- ai_questions(CAT, "does LEP track BMI?", GENES)
chk(all(c("species", "tissue") %in% names(qs)),
    "and each one with values in the catalog becomes a question",
    paste(names(qs), collapse = ", "))
chk(!"setting" %in% names(qs),
    "while a facet no dataset records is not asked about at all")

# --- nothing matches ----------------------------------------------------------

# LEPR is a candidate the question could have meant, and no dataset carries it.
p7 <- plan_of(gene = choice("LEPR"))
chk(isFALSE(p7$ok) && identical(p7$kind, "none"),
    "a gene no dataset carries is refused, not fudged", p7$headline)
steps <- vapply(p7$trace, function(s) s$step, character(1))
chk(all(c("Organism", "Tissue", "Gene") %in% steps),
    "and the refusal carries the criteria it searched with",
    paste(steps, collapse = " > "))
chk(any(vapply(p7$trace, function(s) isTRUE(s$kept == 0L), logical(1))),
    "showing where the last dataset was lost")

# An organism nobody collected has to be sayable, or the nearest one on the
# list gets used instead.
p13 <- plan_of(species = choice("other"))
chk(isFALSE(p13$ok), "a question about an organism the collection lacks is refused")
chk(any(grepl("does not hold", vapply(p13$trace, function(s) s$detail, character(1)))),
    "and says so rather than blaming the gene")

# A guess the model is unsure of must not quietly throw data away.
p14 <- plan_of(measurement = choice("Array", conf = 0.4))
chk(isTRUE(p14$ok) && length(p14$datasets) > 1,
    "a low-confidence facet excludes nothing",
    length(p14$datasets %||% 0))

p8 <- plan_of(answerable = noul(0.1))
chk(isFALSE(p8$ok), "a question that is not about data at all is refused early")

p9 <- plan_of(gene = choice("none"))
chk(isFALSE(p9$ok) && grepl("gene", p9$headline),
    "and so is one whose gene could not be identified", p9$headline)

# --- low confidence asks instead of guessing ---------------------------------

p10 <- plan_of(variable = choice("BMI", confidence = 0.3,
                                 others = c("Obesity.status", "Age")))
chk(isFALSE(p10$ok) && identical(p10$kind, "ambiguous"),
    "a variable the model is unsure of is put back to the researcher", p10$kind)
chk(length(p10$options) >= 2, "with the alternatives it was choosing between",
    paste(p10$options, collapse = ", "))

# --- every plan names controls the target tab really has ---------------------

for (p in list(p, p1, p2, p3, p4, p5)) {
  spec <- AI_TOOLS[[p$tab]]
  unknown <- setdiff(names(p$controls), names(spec$controls))
  if (length(unknown)) no("every control belongs to its tab",
                          p$tab, ": ", paste(unknown, collapse = ", "))
}
ok("every control a plan sets is one that tab declares")

cat("\n", n, " checks passed\n", sep = "")
