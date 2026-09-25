#!/usr/bin/env Rscript
# What a question costs, and whether it lands on the right tool.
#
#   Rscript tools/jev-cost.R [--dry]
#
# Every run here is a real request to Jev and real money. It is not much --
# input tokens are billed at $0.042 per million and output is free -- but the
# point of this script is to say how much rather than to assume, and to say it
# per level of difficulty and per language. --dry prints the questions and the
# state size without calling anything.

source("global.R")

dry <- "--dry" %in% commandArgs(TRUE)

# level: 1 one gene and one variable, 2 needs several datasets pooled,
#        3 a follow-up that splits the previous answer, 4 unanswerable.
QUESTIONS <- list(
  list(level = 1, lang = "en", expect = "plot",
       q = "Does leptin expression increase with BMI in human adipose tissue?"),
  list(level = 1, lang = "pt", expect = "plot",
       q = "A expressao de leptina aumenta com o IMC no tecido adiposo humano?"),
  # Expected to ASK, not to plot: the catalog holds Age, Age.Class and
  # Differentiation.day, the model splits its probability across them, and
  # picking one silently would draw a plausible wrong answer.
  list(level = 1, lang = "en", expect = "ambiguous",
       q = "Is UCP1 expression related to age in brown adipose tissue of mice?"),
  list(level = 1, lang = "pt", expect = "ambiguous",
       q = "A expressao de UCP1 se relaciona com a idade no tecido adiposo marrom de camundongos?"),

  list(level = 2, lang = "en", expect = "plot",
       q = "Is adiponectin expression different between obese and lean people in adipose tissue?"),
  list(level = 2, lang = "pt", expect = "plot",
       q = "A expressao de adiponectina difere entre pessoas obesas e magras no tecido adiposo?"),

  list(level = 3, lang = "en", expect = "plot",
       q = "Does that differ between the sexes?",
       after = "Does leptin expression increase with BMI in human adipose tissue?"),
  list(level = 3, lang = "pt", expect = "plot",
       q = "Isso muda entre os sexos?",
       after = "A expressao de leptina aumenta com o IMC no tecido adiposo humano?"),

  list(level = 4, lang = "en", expect = "none",
       q = "Is shh expression higher in the zebrafish brain after hypoxia?"),
  list(level = 4, lang = "pt", expect = "none",
       q = "A expressao de shh e maior no cerebro de peixe-zebra apos hipoxia?")
)

catalog <- ai_catalog()
cat("catalog: ", length(catalog), " datasets\n", sep = "")

rows <- list()
for (item in QUESTIONS) {
  hist <- if (is.null(item$after)) list() else list(item$after)
  genes <- ai_gene_candidates(item$q)
  state <- ai_state(catalog, item$q, hist)
  qs    <- ai_questions(catalog, item$q, genes, hist)
  size  <- nchar(jsonlite::toJSON(list(state = state, questions = qs), auto_unbox = TRUE))

  if (dry) {
    cat(sprintf("L%d %s  %-58s  %d candidate genes, %d questions, %d chars\n",
                item$level, item$lang, substr(item$q, 1, 58), nrow(genes),
                length(qs), size))
    next
  }

  # A follow-up is only a follow-up if it is asked after something. The
  # earlier question is asked for real first, so the second one inherits a
  # plan the way it would in the tab -- and both requests are counted.
  prev <- NULL
  if (!is.null(item$after)) {
    prev <- tryCatch(ai_answer(item$after, catalog = catalog), error = function(e) NULL)
    if (!is.null(prev)) {
      rows[[length(rows) + 1L]] <- data.frame(
        level = item$level, lang = item$lang,
        question = paste0("(setup) ", item$after),
        questions_asked = NA_integer_, state_chars = NA_integer_,
        input_tokens = as.numeric(prev$usage$input_tokens %||% NA),
        cost_usd = as.numeric(prev$cost %||% NA),
        outcome = if (isTRUE(prev$ok)) "plot" else "none",
        tab = if (isTRUE(prev$ok)) prev$tab else "-",
        datasets = if (isTRUE(prev$ok)) length(prev$datasets) else 0L,
        as_expected = TRUE, headline = prev$headline %||% "",
        stringsAsFactors = FALSE)
    }
  }

  res <- tryCatch(
    ai_answer(item$q, catalog = catalog, history = hist, previous = prev),
    error = function(e) list(ok = FALSE, kind = "error", headline = conditionMessage(e),
                             usage = list(input_tokens = NA), cost = NA_real_))

  # "ambiguous" is its own outcome, not a failure: the tab asking which of two
  # readings was meant is the designed behaviour when the model is unsure.
  got <- if (isTRUE(res$ok)) "plot" else res$kind %||% "none"
  rows[[length(rows) + 1L]] <- data.frame(
    level    = item$level,
    lang     = item$lang,
    question = item$q,
    questions_asked = length(qs),
    state_chars = size,
    input_tokens = as.numeric(res$usage$input_tokens %||% NA),
    cost_usd = as.numeric(res$cost %||% NA),
    outcome  = got,
    tab      = if (isTRUE(res$ok)) res$tab else "-",
    datasets = if (isTRUE(res$ok)) length(res$datasets) else 0L,
    as_expected = identical(got, item$expect),
    headline = res$headline %||% "",
    stringsAsFactors = FALSE)

  cat(sprintf("L%d %s  %-52s -> %-20s %6s tok  $%.6f  %s\n",
              item$level, item$lang, substr(item$q, 1, 52),
              if (isTRUE(res$ok)) res$tab else got,
              format(res$usage$input_tokens %||% NA),
              res$cost %||% NA_real_,
              if (identical(got, item$expect)) "as expected" else "NOT as expected"))
  Sys.sleep(0.5)
}

if (dry) quit(status = 0)

df <- do.call(rbind, rows)

# --- how steady is an answer? -------------------------------------------------
# The same question does not always come back the same way. Where the model's
# confidence sits near the threshold the tab acts on, a rerun can plot, ask, or
# refuse. That is worth a number rather than a shrug, so one question that sits
# there is asked several times in both languages.
STABILITY_Q <- list(
  en = "Is UCP1 expression related to age in brown adipose tissue of mice?",
  pt = "A expressao de UCP1 se relaciona com a idade no tecido adiposo marrom de camundongos?")
REPEATS <- 5L

stab <- list()
for (lang in names(STABILITY_Q)) {
  for (i in seq_len(REPEATS)) {
    q <- STABILITY_Q[[lang]]
    genes <- ai_gene_candidates(q)
    res <- tryCatch(jev_ask(ai_state(catalog, q, list()),
                            ai_questions(catalog, q, genes, list()),
                            label = paste0("stability/", lang)),
                    error = function(e) NULL)
    if (is.null(res)) next
    plan <- ai_decide(res$answers, catalog, genes)
    stab[[length(stab) + 1L]] <- data.frame(
      lang = lang, run = i,
      variable = as.character(jev_value(res$answers$variable)),
      confidence = jev_confidence(res$answers$variable),
      outcome = if (isTRUE(plan$ok)) plan$tab else plan$kind %||% "none",
      tokens = as.numeric(res$usage$input_tokens %||% NA),
      stringsAsFactors = FALSE)
    Sys.sleep(0.3)
  }
}
sdf <- do.call(rbind, stab)

by_level <- do.call(rbind, lapply(split(df, df$level), function(d) data.frame(
  level = d$level[1], questions = nrow(d),
  mean_tokens = round(mean(d$input_tokens, na.rm = TRUE)),
  mean_cost = mean(d$cost_usd, na.rm = TRUE),
  as_expected = sum(d$as_expected), stringsAsFactors = FALSE)))

by_lang <- do.call(rbind, lapply(split(df, df$lang), function(d) data.frame(
  lang = d$lang[1], questions = nrow(d),
  mean_tokens = round(mean(d$input_tokens, na.rm = TRUE)),
  mean_cost = mean(d$cost_usd, na.rm = TRUE),
  as_expected = sum(d$as_expected), stringsAsFactors = FALSE)))

md <- c(
  "# What a question to Jev costs",
  "",
  sprintf("Measured %s against the live API, %d datasets in the catalog.",
          format(Sys.Date()), length(catalog)),
  "Input tokens are billed at $0.042 per million; output tokens are free",
  "(docs.typesafe.ai/models). One question is one request: the catalog goes out",
  "once and every typed question is answered against it in parallel.",
  "",
  "## Per question",
  "",
  "| level | lang | question | questions asked | input tokens | cost | outcome | as expected |",
  "|---|---|---|---|---|---|---|---|",
  apply(df, 1, function(r) sprintf("| %s | %s | %s | %s | %s | $%s | %s | %s |",
        r["level"], r["lang"], substr(r["question"], 1, 64), r["questions_asked"],
        format(as.numeric(r["input_tokens"]), big.mark = ","),
        formatC(as.numeric(r["cost_usd"]), format = "f", digits = 6),
        if (r["tab"] == "-") r["outcome"] else r["tab"],
        if (r["as_expected"] == "TRUE") "yes" else "**no**")),
  "",
  "## Per level",
  "",
  "| level | what it needs | questions | mean tokens | mean cost | as expected |",
  "|---|---|---|---|---|---|",
  sprintf("| %d | %s | %d | %s | $%s | %d/%d |", by_level$level,
          c("one gene, one variable", "several datasets pooled",
            "a follow-up that splits the previous answer",
            "nothing in the hub can answer it")[by_level$level],
          by_level$questions, format(by_level$mean_tokens, big.mark = ","),
          formatC(by_level$mean_cost, format = "f", digits = 6),
          by_level$as_expected, by_level$questions),
  "",
  "## Per language",
  "",
  "| language | questions | mean tokens | mean cost | as expected |",
  "|---|---|---|---|---|",
  sprintf("| %s | %d | %s | $%s | %d/%d |", by_lang$lang, by_lang$questions,
          format(by_lang$mean_tokens, big.mark = ","),
          formatC(by_lang$mean_cost, format = "f", digits = 6),
          by_lang$as_expected, by_lang$questions),
  "",
  sprintf("Total for this run: %s input tokens, $%s.",
          format(sum(df$input_tokens, na.rm = TRUE), big.mark = ","),
          formatC(sum(df$cost_usd, na.rm = TRUE), format = "f", digits = 6)),
  "",
  "## The same question, asked five times",
  "",
  sprintf("\"%s\", and its Portuguese translation, %d times each. This question sits",
          STABILITY_Q$en, REPEATS),
  "near the confidence threshold on purpose: the catalog holds `Age`, `Age.Class`",
  "and `Differentiation.day`, so the model spreads its probability across them.",
  "",
  "| lang | run | variable chosen | confidence | outcome |",
  "|---|---|---|---|---|",
  if (is.null(sdf)) "| - | - | - | - | - |" else
    sprintf("| %s | %d | %s | %.2f | %s |", sdf$lang, sdf$run, sdf$variable,
            sdf$confidence, sdf$outcome),
  "",
  if (is.null(sdf)) "" else sprintf(
    "Distinct outcomes: %d in English, %d in Portuguese. Mean confidence %.2f and %.2f.",
    length(unique(sdf$outcome[sdf$lang == "en"])),
    length(unique(sdf$outcome[sdf$lang == "pt"])),
    mean(sdf$confidence[sdf$lang == "en"], na.rm = TRUE),
    mean(sdf$confidence[sdf$lang == "pt"], na.rm = TRUE)),
  "",
  "## What \"as expected\" means",
  "",
  "The expected outcome is what the tab *should* do, which is not always a plot.",
  "The two UCP1 questions are expected to come back as **ambiguous**: the catalog",
  "holds `Age`, `Age.Class` and `Differentiation.day`, the model spreads its",
  "probability across them, and the tab asks which was meant instead of picking",
  "one and drawing a plausible wrong answer. The zebrafish questions are expected",
  "to be refused, because nothing in the collection is zebrafish.",
  "",
  "Regenerate with `Rscript tools/jev-cost.R`. Every row is a real request.")

writeLines(md, "docs/JEV-COST.md")
cat("\nwrote docs/JEV-COST.md -- total $",
    formatC(sum(df$cost_usd, na.rm = TRUE), format = "f", digits = 6), "\n", sep = "")
