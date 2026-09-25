# R/jev.R -- the client for TypeSafe's Jev model.
#
# Jev does not write text and does not write code. It answers typed questions
# about a state, in parallel, in one request:
#
#   noul   "is this true?"        -> a probability
#   choice "pick one of these"    -> the option, a distribution, a confidence
#   score  "rate on this rubric"  -> a weighted score, a confidence
#
# That is the whole reason this tab can be trusted: the model classifies, and
# R decides. Nothing here ever asks it what to do, only what the question says.
#
# API: POST https://api.typesafe.ai/v1/systemone, Bearer key, and the body
# {state, model, questions}. Input tokens are billed, output tokens are not.

JEV_URL   <- "https://api.typesafe.ai/v1/systemone"
JEV_MODEL <- "jev-latest"

# USD 42 per billion input tokens, output free (docs.typesafe.ai/models).
JEV_USD_PER_INPUT_TOKEN <- 42e-9

# --- the key ----------------------------------------------------------------

# The environment wins, so the HPC can hold the key outside the filesystem and
# apikeys.txt never has to exist there. apikeys.txt is gitignored; if it ever
# stops being, tools/verify.sh fails before a commit can carry it.
jev_key <- function() {
  env <- Sys.getenv("TYPESAFE_API_KEY", "")
  if (nzchar(env)) return(env)

  path <- file.path(os_root(), "apikeys.txt")
  if (!file.exists(path)) {
    stop("No Jev key. Set TYPESAFE_API_KEY, or copy apikeys.txt.example to ",
         "apikeys.txt and fill it in.", call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  hit <- grep("^\\s*Jev\\s*=", lines, value = TRUE)
  if (!length(hit)) {
    stop("apikeys.txt has no line starting with 'Jev ='.", call. = FALSE)
  }
  trimws(sub("^\\s*Jev\\s*=\\s*", "", hit[1]))
}

# --- the transport ----------------------------------------------------------

# Separated from jev_ask() so every test below this line can run without a
# network and without spending anything: pass a function that returns a
# response. It takes the JSON body and returns list(status, parsed).
jev_post <- function(body, key, timeout = 60) {
  res <- httr::POST(
    JEV_URL,
    httr::add_headers(Authorization = paste("Bearer", key),
                      `Content-Type` = "application/json"),
    body = body, encode = "raw",
    httr::timeout(timeout)
  )
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  list(status = httr::status_code(res),
       parsed = tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                         error = function(e) list(raw = txt)))
}

# --- asking -----------------------------------------------------------------

# state    : anything jsonlite can write -- a string, or a list, which is what
#            the catalog is.
# questions: a named list of question objects, each with $type and
#            $instructions, and $criteria for choice and score.
#
# Returns the answers plus what the request cost, and writes one line to the
# log. Errors carry the API's own message: a 422 that says which question is
# malformed is worth more than "request failed".
jev_ask <- function(state, questions, model = JEV_MODEL,
                    transport = jev_post, retries = 3L, label = NA_character_) {
  stopifnot(length(questions) > 0, !is.null(names(questions)))

  body <- jsonlite::toJSON(
    list(state = state, model = model, questions = questions),
    auto_unbox = TRUE, null = "null"
  )

  key <- jev_key()
  t0  <- Sys.time()
  res <- NULL
  for (attempt in seq_len(retries)) {
    res <- transport(body, key)
    # 429 and 529 are the two the documentation says to back off on; anything
    # else is either an answer or a problem that retrying will not fix.
    if (!res$status %in% c(429L, 529L)) break
    if (attempt < retries) Sys.sleep(2^(attempt - 1))
  }

  if (res$status == 401L) {
    stop("Jev refused the key (401). Check TYPESAFE_API_KEY or apikeys.txt.",
         call. = FALSE)
  }
  if (res$status != 200L) {
    msg <- res$parsed$error$message %||% res$parsed$message %||% res$parsed$raw
    stop("Jev returned ", res$status,
         if (!is.null(msg)) paste0(": ", substr(as.character(msg), 1, 300)) else "",
         call. = FALSE)
  }

  answers <- res$parsed$answers
  usage   <- res$parsed$usage
  out <- list(
    answers = answers,
    usage   = usage,
    cost    = jev_cost(usage),
    model   = res$parsed$model %||% model,
    seconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
  jev_log(label = label, model = out$model, questions = length(questions),
          input_tokens = usage$input_tokens %||% NA, cost_usd = out$cost,
          seconds = round(out$seconds, 2), answers = jev_answer_line(answers))
  out
}

jev_cost <- function(usage) {
  tok <- usage$input_tokens
  if (is.null(tok) || !is.finite(as.numeric(tok))) return(NA_real_)
  as.numeric(tok) * JEV_USD_PER_INPUT_TOKEN
}

# --- reading an answer ------------------------------------------------------
#
# The three shapes, each reduced to the two things a decision needs: what was
# chosen, and how sure the model is. A noul has no confidence of its own -- the
# probability is the answer -- so distance from 0.5 stands in for one.

jev_value <- function(answer) {
  if (is.null(answer)) return(NULL)
  switch(answer$type %||% "",
         noul   = answer$noul,
         choice = answer$choice,
         score  = answer$score,
         NULL)
}

jev_confidence <- function(answer) {
  if (is.null(answer)) return(NA_real_)
  if (identical(answer$type, "noul")) return(abs(as.numeric(answer$noul) - 0.5) * 2)
  as.numeric(answer$confidence %||% NA_real_)
}

# The runners-up of a choice, most likely first, for when confidence is too low
# to act on and the tab has to ask the researcher instead of guessing.
jev_runners_up <- function(answer, n = 3L) {
  if (is.null(answer) || !identical(answer$type, "choice")) return(character(0))
  p <- unlist(answer$probabilities)
  if (!length(p)) return(character(0))
  names(sort(p, decreasing = TRUE))[seq_len(min(n, length(p)))]
}

jev_answer_line <- function(answers) {
  if (!length(answers)) return("")
  paste(vapply(names(answers), function(k) {
    a <- answers[[k]]
    v <- jev_value(a)
    conf <- jev_confidence(a)
    sprintf("%s=%s(%.2f)", k,
            if (is.null(v)) "?" else format(v, digits = 3),
            if (is.na(conf)) NA_real_ else conf)
  }, character(1)), collapse = " ")
}

# --- the log ----------------------------------------------------------------
#
# Same shape as log_download() in R/data_io.R, in its own file. You asked for
# the cost of each question to be catalogued; this is where the catalogue comes
# from, and the tab reads it back to show the running total.
jev_log <- function(...) {
  dir <- os_path("logs")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  fields <- vapply(list(...), function(x) paste(x, collapse = ", "), character(1))
  nm <- names(fields)
  if (is.null(nm)) nm <- rep("", length(fields))
  entry <- paste0(format(Sys.time()), " | ",
                  paste(ifelse(nzchar(nm), paste0(nm, ": "), ""), fields,
                        sep = "", collapse = " | "), "\n")
  cat(entry, file = file.path(dir, "jev.log"), append = TRUE)
  invisible(entry)
}

# Every call this session, as a data.frame, for the Cost panel.
jev_log_read <- function() {
  path <- file.path(os_path("logs"), "jev.log")
  empty <- data.frame(time = character(0), input_tokens = numeric(0),
                      cost_usd = numeric(0), stringsAsFactors = FALSE)
  if (!file.exists(path)) return(empty)
  lines <- readLines(path, warn = FALSE)
  if (!length(lines)) return(empty)
  field <- function(l, k) {
    m <- regmatches(l, regexpr(paste0(k, ": [^|]*"), l))
    if (!length(m)) return(NA_character_)
    trimws(sub(paste0(k, ": "), "", m))
  }
  data.frame(
    time         = trimws(vapply(strsplit(lines, " \\| "), `[`, character(1), 1)),
    input_tokens = suppressWarnings(as.numeric(vapply(lines, field, character(1), "input_tokens"))),
    cost_usd     = suppressWarnings(as.numeric(vapply(lines, field, character(1), "cost_usd"))),
    stringsAsFactors = FALSE, row.names = NULL
  )
}
