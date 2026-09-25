#!/usr/bin/env Rscript
# The question tab, with the model stubbed out.
#
#   Rscript tools/behaviour/ai_chat.R
#
# No network and no spending: the transport is replaced by a function that
# returns the answers this test wants. What is checked is the tab's own job --
# that an answer becomes an exchange with a trace, that the button opens the
# right tab with the right dataset and publishes the plan the other tabs read,
# and that a refusal still explains itself.

source("global.R")
source("tools/behaviour/_fixture.R")
tmp <- use_fixture_hub(writable = TRUE)

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

md   <- load_metadata()
GENE <- load_expression("DEMO_RNAseq", "TPM")$Symbol[5]

# The search checks a gene is really in the matrices, so the fixture gets its
# own index -- the same file tools/build-gene-index.R writes, in miniature.
saveRDS(list(built = Sys.time(), source = metadata_path(),
             datasets = stats::setNames(lapply(sort(unique(md$dataset)), function(d) {
               u <- tryCatch(list_units(d)[1], error = function(e) NA)
               if (is.na(u)) character(0) else load_expression(d, u)$Symbol
             }), sort(unique(md$dataset)))),
        os_path("hubdata", "gene-index.rds"))
options(omicscalpel.gene_index = NULL)

choice <- function(v, conf = 0.95, others = character(0)) {
  p <- stats::setNames(as.list(c(conf, rep((1 - conf) / max(1, length(others)),
                                           length(others)))), c(v, others))
  list(type = "choice", choice = v, probabilities = p, confidence = conf)
}
noul <- function(p) list(type = "noul", noul = p)

# A transport that answers whatever the test asks it to, and counts the calls.
calls <- new.env(parent = emptyenv()); calls$n <- 0L
stub <- function(answers, tokens = 4200L) {
  function(body, key, timeout = 60) {
    calls$n <- calls$n + 1L
    calls$body <- body
    list(status = 200L, parsed = list(model = "jev-1.13.0", answers = answers,
                                      usage = list(input_tokens = tokens,
                                                   output_tokens = 12L)))
  }
}
Sys.setenv(TYPESAFE_API_KEY = "stub")

found <- list(answerable = noul(0.96), intent = choice("association_continuous"),
              species = choice("any"), tissue = choice("any"),
              measurement = choice("gene or protein expression"), variable = choice("DEMO.Marker"),
              gene = choice(GENE))

# --- a question that can be answered -----------------------------------------

opened <- new.env(parent = emptyenv())
published <- reactiveVal(NULL)
options(omicscalpel.jev_transport = stub(found))

state <- NULL
testServer(aiChatServer,
           args = list(id = "ai_chat", ds = reactiveVal("DEMO_RNAseq"),
                       meta = reactive(md),
                       go_to = function(tab = NULL, dataset = NULL) {
                         opened$tab <- tab; opened$dataset <- dataset
                       },
                       ai = published), {
  session$setInputs(min_confidence = 0.55, question = "does that gene track the marker?",
                    ask = 1)
  session$flushReact()
  state <<- exchanges()
  session$setInputs(open_1 = 1)
  session$flushReact()
  # read inside the session: a reactiveVal cannot be read from outside one
  opened$published <- published()
})

chk(length(state) == 1L, "the question becomes one exchange", length(state))
p <- state[[1]]$plan
chk(isTRUE(p$ok) && p$tab %in% names(AI_TOOLS),
    "carrying a plan for a tab that can answer it", p$tab, " ", p$headline)
chk(length(p$trace) >= 4, "and the path it took to get there", length(p$trace))
chk(!is.null(p$cost) && p$cost > 0, "with what the question cost", p$cost)
chk(identical(calls$n, 1L), "one request for the whole question, not one per question asked",
    calls$n)

chk(identical(opened$tab, p$tab) && identical(opened$dataset, p$dataset),
    "the button opens the tab the plan names, on the dataset it names",
    opened$tab %||% "(none)", "/", opened$dataset %||% "(none)")
chk(identical(opened$published, p),
    "and publishes the same plan the analysis tabs listen for")

# The state that went out carries the catalog and the question, and no sample
# identifiers or expression values.
body <- as.character(calls$body)
chk(grepl("DEMO_RNAseq", body, fixed = TRUE) && grepl("does that gene track", body, fixed = TRUE),
    "the request describes the datasets and the question")
chk(!grepl("DEMO_RNAseq_S01", body, fixed = TRUE),
    "and sends no sample identifiers")

# --- a question nothing can answer -------------------------------------------

calls$n <- 0L
options(omicscalpel.jev_transport = stub(utils::modifyList(found,
  list(species = choice("Mmu"), tissue = choice("Plasma")))))

refused <- NULL
testServer(aiChatServer,
           args = list(id = "ai_chat", ds = reactiveVal("DEMO_RNAseq"),
                       meta = reactive(md), go_to = NULL, ai = NULL), {
  session$setInputs(min_confidence = 0.55, question = "mouse plasma?", ask = 1)
  session$flushReact()
  refused <<- exchanges()[[1]]$plan
})
chk(isFALSE(refused$ok), "an impossible question is refused")
chk(length(refused$trace) >= 3 &&
    any(vapply(refused$trace, function(s) isTRUE(s$kept == 0L), logical(1))),
    "and the refusal shows the criteria and where nothing was left")

# --- the model failing is not a crash ----------------------------------------

options(omicscalpel.jev_transport = function(body, key, timeout = 60) {
  list(status = 429L, parsed = list(message = "slow down"))
})
broke <- NULL
testServer(aiChatServer,
           args = list(id = "ai_chat", ds = reactiveVal("DEMO_RNAseq"),
                       meta = reactive(md), go_to = NULL, ai = NULL), {
  session$setInputs(min_confidence = 0.55, question = "anything", ask = 1)
  session$flushReact()
  broke <<- exchanges()[[1]]$plan
})
chk(isFALSE(broke$ok) && grepl("429", broke$headline),
    "an API that refuses becomes a message, not a traceback", broke$headline)

options(omicscalpel.jev_transport = NULL)
cat("\n", n, " checks passed\n", sep = "")
