# R/ai_genes.R -- from the word a researcher types to a symbol in a matrix.
#
# Two different problems, and they need different answers.
#
# VOCABULARY: Jev never invents "LEP" out of "leptin" -- it only chooses among
# options handed to it. So R finds the candidates and the model picks. The
# candidates come from the symbols that exist, plus a small curated file for
# protein names that share no letters with their symbol.
#
# PRESENCE: which datasets actually carry the symbol. That is the gene index,
# built by tools/build-gene-index.R, because reading the matrices to find out
# costs 45 seconds a question.

ai_gene_index_path <- function() os_path("hubdata", "gene-index.rds")

ai_gene_index <- function() {
  cached <- getOption("omicscalpel.gene_index")
  path <- ai_gene_index_path()
  if (!file.exists(path)) return(NULL)
  mtime <- file.mtime(path)
  if (!is.null(cached) && identical(cached$mtime, mtime)) return(cached$index)
  index <- tryCatch(readRDS(path), error = function(e) NULL)
  options(omicscalpel.gene_index = list(mtime = mtime, index = index))
  index
}

# The tab shows this instead of quietly answering from a stale index.
ai_gene_index_status <- function() {
  path <- ai_gene_index_path()
  if (!file.exists(path)) {
    return(list(ok = FALSE, message = paste(
      "No gene index. Run tools/build-gene-index.R once, or the search cannot",
      "tell which datasets carry a gene.")))
  }
  idx <- ai_gene_index()
  meta_mtime <- tryCatch(file.mtime(metadata_path()), error = function(e) NA)
  if (!is.na(meta_mtime) && file.mtime(path) < meta_mtime) {
    return(list(ok = FALSE, message = paste0(
      "The gene index is older than the metadata (built ",
      format(idx$built, "%Y-%m-%d %H:%M"),
      "). Run tools/build-gene-index.R; a dataset added since is invisible to the search.")))
  }
  list(ok = TRUE, message = paste0(length(idx$datasets), " datasets indexed, built ",
                                   format(idx$built, "%Y-%m-%d %H:%M")))
}

# Case-insensitive on purpose. Human matrices spell it UCP1 and mouse ones
# Ucp1, so an exact match answers "no mouse dataset has UCP1" about a gene
# every one of them carries.
ai_datasets_with_gene <- function(symbol, index = ai_gene_index()) {
  if (is.null(index) || !length(symbol)) return(character(0))
  want <- toupper(symbol[1])
  names(Filter(function(v) want %in% toupper(v), index$datasets))
}

# How this dataset spells it. The analysis tabs match the symbol exactly, so a
# plan aimed at a mouse dataset has to say Ucp1, not UCP1.
ai_gene_as_spelled <- function(symbol, dataset, index = ai_gene_index()) {
  if (is.null(index) || is.null(index$datasets[[dataset]])) return(symbol)
  v <- index$datasets[[dataset]]
  hit <- v[toupper(v) == toupper(symbol[1])]
  if (length(hit)) hit[1] else symbol[1]
}

# Genemetadata.xlsx is an annotation table of 17,525 symbols and nothing in the
# app read it before. It is a good starting vocabulary and not a complete one --
# the GTEX matrix alone has 54,593 rows -- so the index's own symbols are added.
ai_gene_vocabulary <- function() {
  cached <- getOption("omicscalpel.gene_vocab")
  if (!is.null(cached)) return(cached)
  syms <- character(0)
  path <- os_path("hubdata", "Genemetadata.xlsx")
  if (file.exists(path)) {
    gm <- tryCatch(as.data.frame(readxl::read_excel(path, .name_repair = "minimal")),
                   error = function(e) NULL)
    if (!is.null(gm) && "Symbol" %in% names(gm)) syms <- as.character(gm$Symbol)
  }
  idx <- ai_gene_index()
  if (!is.null(idx)) syms <- c(syms, unlist(idx$datasets, use.names = FALSE))
  syms <- sort(unique(syms[!is.na(syms) & nzchar(syms)]))
  options(omicscalpel.gene_vocab = syms)
  syms
}

ai_gene_synonyms <- function() {
  path <- file.path(os_root(), "config", "ai-synonyms.txt")
  if (!file.exists(path)) return(character(0))
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^\\s*(#|$)", lines)]
  parts <- strsplit(lines, "\\s*\\|\\s*")
  keep <- lengths(parts) >= 2
  stats::setNames(toupper(trimws(vapply(parts[keep], `[`, character(1), 2))),
                  tolower(trimws(vapply(parts[keep], `[`, character(1), 1))))
}

# Candidate symbols for one question, most likely first. Three routes, kept
# separate so the trace can say which one found the gene:
#   1. a curated synonym       leptin -> LEP
#   2. the word IS a symbol    UCP1   -> UCP1
#   3. a word prefixes symbols lep    -> LEP, LEPR, LEPROT
# Words that are not gene names and prefix half the vocabulary. English and
# Portuguese, since the tab takes both.
AI_STOPWORDS <- c(
  "does", "with", "from", "that", "this", "than", "then", "have", "has", "are",
  "was", "were", "expression", "expressed", "level", "levels", "tissue",
  "tissues", "sample", "samples", "dataset", "datasets", "gene", "genes",
  "human", "mouse", "mice", "patient", "patients", "between", "higher",
  "lower", "increase", "increases", "increased", "decrease", "decreased",
  "correlate", "correlated", "correlation", "associated", "association",
  "difference", "different", "change", "changes", "obesity", "obese", "lean",
  "adipose", "brown", "white", "cold", "diet", "sex", "male", "female", "age",
  "para", "como", "com", "mais", "menos", "entre", "aumenta", "aumento",
  "diminui", "expressao", "tecido", "amostras", "dados", "obesidade", "magro",
  "sexo", "idade", "pacientes", "camundongos", "humano", "humanos", "tem",
  "existe", "qual", "quais", "muda", "difere", "maior", "menor")

ai_gene_candidates <- function(question, vocab = ai_gene_vocabulary(),
                               synonyms = ai_gene_synonyms(), max_options = 200L) {
  q <- tolower(question)
  words <- unique(unlist(strsplit(gsub("[^a-zA-Z0-9 ]+", " ", q), "\\s+")))
  words <- words[nzchar(words)]

  hits <- list()
  add <- function(sym, how) {
    sym <- intersect(sym, vocab)
    for (s in sym) if (is.null(hits[[s]])) hits[[s]] <<- how
  }

  for (term in names(synonyms)) {
    if (grepl(paste0("\\b", term, "\\b"), q, fixed = FALSE)) add(synonyms[[term]], "synonym")
  }
  add(intersect(toupper(words), vocab), "exact")
  # Prefix matching earns its keep on "lep" -> LEP, LEPR, but "fat" pulls in
  # FAT1..FAT4 and the Portuguese "com" pulls in every COMMD. Every junk option
  # is a distractor the model has to rule out, so the common words are dropped
  # first. Four characters, because the shorter a word is the worse it behaves.
  for (w in setdiff(words[nchar(words) >= 4], AI_STOPWORDS)) {
    add(utils::head(grep(paste0("^", toupper(w)), vocab, value = TRUE), 25), "prefix")
  }

  if (!length(hits)) return(data.frame(symbol = character(0), how = character(0),
                                       stringsAsFactors = FALSE))
  ord <- c("synonym", "exact", "prefix")
  df <- data.frame(symbol = names(hits), how = unlist(hits, use.names = FALSE),
                   stringsAsFactors = FALSE)
  df <- df[order(match(df$how, ord), df$symbol), , drop = FALSE]
  utils::head(df, max_options)
}
