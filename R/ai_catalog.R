# R/ai_catalog.R -- what the hub can answer, small enough to hand to a model.
#
# Jev sees this, and nothing else about the data. It is deliberately a summary:
# the state budget is 32k tokens, and more importantly a model should choose
# among facts, not sift 2,759 rows.
#
# Built from Metadata.xlsx rather than Datasets_summary.xlsx, because the
# summary sheet is behind the data: it has no row at all for Example3_survival,
# Tissue is "NA" there for datasets whose samples name their tissue, and its
# Has.*.data flags disagree with what is on disk for six datasets. list_units()
# is the truthful source for units; the per-sample metadata for the rest.

# Identity and provenance: never an outcome to analyse.
AI_ID_COLS <- c("SampleID", "TsengID", "dataset", "Author", "Date.sequenced",
                "Date.of.collection", "Folder.Name.in.TsengLab/Datasets",
                "Project", "publication", "Data.location.&.ELN",
                "Fellow.who.generated/uploaded.dataset",
                "Fellow who generated/uploaded dataset", "Description/observation")

# What a dataset *is*, as opposed to what varies inside it.
AI_FACET_COLS <- c("Species", "Tissue", "Data.type", "Cell.type", "Strain",
                   "Anatomical_region", "Data_avaiability")

# --- reading values ----------------------------------------------------------

# Missing is the literal string "NA" in this spreadsheet -- 87% of its cells.
# Everything here goes through this, or it counts absence as data.
ai_real <- function(x) {
  v <- as.character(x)
  v[!is.na(v) & v != "NA" & nzchar(trimws(v))]
}

# Some cells hold the R source text of a vector, c("Subcutaneous", "Visceral"),
# because that is how an earlier tool wrote them. Others hold "a, b".
ai_split_multi <- function(v) {
  v <- trimws(v)
  out <- unlist(lapply(v, function(x) {
    if (grepl('^c\\(".*"\\)$', x)) {
      x <- sub('^c\\(', '', sub('\\)$', '', x))
      x <- gsub('"', '', x)
    }
    trimws(strsplit(x, "\\s*,\\s*")[[1]])
  }), use.names = FALSE)
  unique(out[nzchar(out)])
}

# The same test the tabs use to decide what a column is good for.
ai_kind <- function(x) {
  v <- ai_real(x)
  if (length(v) < 3) return(NA_character_)
  num <- suppressWarnings(as.numeric(v))
  share <- mean(!is.na(num))
  u <- unique(v)
  if (share > 0.8 && length(unique(num[!is.na(num)])) > 2) return("numeric")
  if (share <= 0.8 && length(u) >= 2 && length(u) <= 20) return("categorical")
  NA_character_
}

# --- the facets, declared rather than written into the code -------------------

# config/ai-facets.txt says which columns the question tab may ask about, and
# where each one lives. A facet added there needs no code: ai_questions()
# generates its question and ai_filter_facet() already filters generically.
ai_facet_spec <- function() {
  path <- file.path(os_root(), "config", "ai-facets.txt")
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^\\s*(#|$)", lines)]
  out <- list()
  for (l in lines) {
    f <- trimws(strsplit(l, "\\|")[[1]])
    if (length(f) < 5) next
    out[[f[1]]] <- list(
      id       = f[1],
      source   = f[2],
      columns  = trimws(strsplit(f[3], ",")[[1]]),
      families = if (identical(f[4], "-")) NA_character_ else f[4],
      asks     = f[5]
    )
  }
  out
}

# Values to family, from whichever file the facet declares. Cached per file.
ai_families <- function(file) {
  if (is.na(file)) return(list(family = character(0), about = character(0)))
  path <- file.path(os_root(), "config", file)
  if (!file.exists(path)) return(list(family = character(0), about = character(0)))
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^\\s*(#|$)", lines)]
  parts <- strsplit(lines, "\\s*\\|\\s*")
  keep <- lengths(parts) >= 3
  list(family = stats::setNames(vapply(parts[keep], `[`, character(1), 2),
                                vapply(parts[keep], `[`, character(1), 1)),
       about  = stats::setNames(vapply(parts[keep], `[`, character(1), 3),
                                vapply(parts[keep], `[`, character(1), 2)))
}

# --- what a column means -----------------------------------------------------

ai_column_docs <- function() {
  path <- file.path(os_root(), "config", "ai-columns.txt")
  if (!file.exists(path)) return(character(0))
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^\\s*(#|$)", lines)]
  parts <- strsplit(lines, "\\s*\\|\\s*")
  keep <- lengths(parts) >= 2
  stats::setNames(vapply(parts[keep], `[`, character(1), 2),
                  vapply(parts[keep], `[`, character(1), 1))
}

# A column with no line in the file still gets described, from what it is.
ai_describe_column <- function(name, kind, levels, docs) {
  if (!is.na(docs[name]) && nzchar(docs[name] %||% "")) return(unname(docs[name]))
  if (identical(kind, "numeric")) return(paste0("the metadata column ", name, ", a number"))
  paste0("the metadata column ", name,
         if (length(levels)) paste0(", one of: ", paste(levels, collapse = ", ")) else "")
}

# Datasets holding invented data, by name. A search leaves them out: an answer
# drawn from Example3_survival's planted cutpoint looks exactly like one drawn
# from a real cohort.
ai_demo_datasets <- function() {
  path <- file.path(os_root(), "config", "ai-demo-datasets.txt")
  if (!file.exists(path)) return(character(0))
  v <- trimws(readLines(path, warn = FALSE))
  v[nzchar(v) & !startsWith(v, "#")]
}

# Does this dataset measure genes at all?
#
# Five of the eighteen are lipidomics: 110 lipid species, no gene symbols.
# Reporting "IL6 is in 1 of 3 datasets" about them implies they were searched
# for IL6 and did not have it, when they measure nothing of the kind.
#
# Proteomics counts: a question about whether a gene is expressed is answered
# by the protein as well as the transcript, and those matrices are keyed by
# gene symbol like the rest.
ai_measures_genes <- function(data_type, symbols = NULL) {
  lipid <- any(grepl("lipid", data_type, ignore.case = TRUE))
  if (lipid) return(FALSE)
  if (!length(data_type) && !is.null(symbols)) return(length(symbols) > 500)
  TRUE
}

# --- the catalog -------------------------------------------------------------

# One entry per dataset. Cached against metadata_version(), the same counter
# save_metadata() bumps, so an edit in any tab invalidates it.
ai_catalog <- function(md = NULL, max_levels = 12L) {
  version <- tryCatch(metadata_version(), error = function(e) 0L)
  cached <- getOption("omicscalpel.ai_catalog")
  if (is.null(md) && !is.null(cached) && identical(cached$version, version)) {
    return(cached$catalog)
  }
  if (is.null(md)) md <- load_metadata()
  docs <- ai_column_docs()
  spec <- ai_facet_spec()
  demo <- ai_demo_datasets()
  index <- tryCatch(ai_gene_index(), error = function(e) NULL)
  # in.vivo.or.in.vitro and Type.of.samples exist only in the summary sheet, so
  # a catalog built from the per-sample metadata alone cannot tell tissue from
  # cultured cells. Missing or unreadable, the facets that need it are simply
  # empty, which the filters already treat as "not recorded".
  summ <- tryCatch(load_datasets_summary(), error = function(e) NULL)

  cols <- setdiff(names(md), c(AI_ID_COLS, AI_FACET_COLS))
  out <- lapply(sort(unique(md$dataset)), function(d) {
    rows <- md[!is.na(md$dataset) & md$dataset == d, , drop = FALSE]

    srow <- if (!is.null(summ) && "dataset" %in% names(summ)) {
      summ[!is.na(summ$dataset) & summ$dataset == d, , drop = FALSE]
    } else NULL

    from <- function(df, cn) {
      if (is.null(df) || !nrow(df) || !cn %in% names(df)) return(character(0))
      ai_split_multi(unique(ai_real(df[[cn]])))
    }

    # The first column with values wins, metadata before summary: the per-sample
    # file is the one that gets edited, so it is the one that is current.
    facet_values <- function(fs) {
      for (cn in fs$columns) {
        v <- switch(fs$source,
                    metadata = from(rows, cn),
                    summary  = from(srow, cn),
                    union(from(rows, cn), from(srow, cn)))
        if (length(v)) return(v)
      }
      character(0)
    }

    facets <- lapply(spec, facet_values)

    facet <- function(cn) {
      if (!cn %in% names(rows)) return(character(0))
      ai_split_multi(unique(ai_real(rows[[cn]])))
    }

    vars <- list()
    for (cn in cols) {
      k <- ai_kind(rows[[cn]])
      if (is.na(k)) next
      v <- ai_real(rows[[cn]])
      entry <- list(column = cn, kind = k, n = length(v))
      if (k == "numeric") {
        num <- suppressWarnings(as.numeric(v))
        entry$range <- round(range(num, na.rm = TRUE), 2)
      } else {
        lv <- sort(unique(v))
        entry$levels <- utils::head(lv, max_levels)
        entry$more <- max(0L, length(lv) - max_levels)
      }
      entry$about <- ai_describe_column(cn, k, entry$levels, docs)
      vars[[cn]] <- entry
    }

    # Columns with any value at all, as opposed to columns that vary. Civelek
    # records Sex for all 770 samples and every one of them is Male: saying it
    # "does not have Sex" is wrong, and offering it for a sex comparison is
    # worse.
    recorded <- names(rows)[vapply(names(rows), function(cn) length(ai_real(rows[[cn]])) > 0,
                                   logical(1))]

    # For a column that is recorded but never varies, the single value it takes.
    # "Civelek records Sex" and "every one of its 770 samples is male" are
    # different sentences, and only the second one answers "why not Civelek?".
    constant <- list()
    for (cn in recorded) {
      v <- unique(ai_real(rows[[cn]]))
      if (length(v) == 1L) constant[[cn]] <- v
    }

    # Follow-up time and its censoring flag, found with the values in hand. The
    # flag is 0/1, which ai_kind() calls neither numeric (two distinct values)
    # nor categorical (they parse as numbers), so it never reaches `variables`
    # -- and a detector looking there concluded the dataset had no survival
    # data at all.
    surv <- NULL
    for (cn in recorded) {
      ev <- os_survival_event_for(cn, rows)
      if (!is.null(ev)) { surv <- list(time = cn, event = ev); break }
    }

    list(dataset   = d,
         n         = nrow(rows),
         facets    = facets,
         recorded  = recorded,
         constant  = constant,
         survival  = surv,
         demo      = d %in% demo,
         measures_genes = ai_measures_genes(facets$measurement,
                                            if (!is.null(index)) index$datasets[[d]] else NULL),
         # kept beside facets because ai_state() and the tabs read them by name
         species   = facets$species %||% facet("Species"),
         tissue    = facets$tissue %||% facet("Tissue"),
         data_type = facets$measurement %||% facet("Data.type"),
         cell_type = facets$cell_type %||% facet("Cell.type"),
         units     = tryCatch(list_units(d), error = function(e) character(0)),
         variables = vars)
  })
  names(out) <- vapply(out, function(x) x$dataset, character(1))

  if (is.null(md) || TRUE) {
    options(omicscalpel.ai_catalog = list(version = version, catalog = out))
  }
  out
}

# --- the vocabularies the model chooses from ---------------------------------

# Every value a facet takes anywhere, for the choice criteria.
ai_facet_values <- function(catalog, field) {
  sort(unique(unlist(lapply(catalog, function(x) x[[field]]), use.names = FALSE)))
}

# Every analysable column, with what it means and where it is. This is the
# criteria map of the `variable` question -- the one that turns "obesity" into
# BMI without anybody hard-coding that.
ai_variable_criteria <- function(catalog, max_options = 120L, kinds = NULL) {
  seen <- list()
  for (d in catalog) {
    for (v in d$variables) {
      if (!is.null(kinds) && !v$kind %in% kinds) next
      if (is.null(seen[[v$column]])) {
        seen[[v$column]] <- list(about = v$about, kind = v$kind, datasets = character(0))
      }
      seen[[v$column]]$datasets <- c(seen[[v$column]]$datasets, d$dataset)
    }
  }
  # Most widely populated first: with a cap on options, a column in six
  # datasets is worth more to a question than one in a single dataset.
  ord <- order(-vapply(seen, function(x) length(x$datasets), integer(1)), names(seen))
  seen <- seen[ord]
  seen <- utils::head(seen, max_options)
  stats::setNames(
    lapply(names(seen), function(cn) {
      s <- seen[[cn]]
      paste0(s$about, " (", s$kind, "; in ", length(s$datasets), " dataset",
             if (length(s$datasets) == 1) "" else "s", ")")
    }),
    names(seen)
  )
}

# Tissue values grouped into families. Without this, "adipose tissue" matches
# only the datasets that spell it that way and misses the ones that name a
# depot -- and a dataset that records no tissue at all is not evidence that it
# is the wrong tissue, so it is never excluded by this filter.
ai_tissue_families <- function() ai_families("ai-tissues.txt")

ai_family_of <- function(values, fam = ai_tissue_families()$family) {
  if (!length(values)) return(character(0))
  unname(ifelse(is.na(fam[values]), values, fam[values]))
}

# --- the state ---------------------------------------------------------------

# What Jev is shown: the question, the conversation so far, and the catalog
# flattened to one line per dataset. No sample rows, no expression values.
ai_state <- function(catalog, question, history = list()) {
  datasets <- lapply(catalog, function(d) {
    list(
      dataset   = d$dataset,
      samples   = d$n,
      species   = d$species,
      tissue    = d$tissue,
      data_type = d$data_type,
      cell_type = d$cell_type,
      units     = d$units,
      variables = unname(vapply(d$variables, function(v) {
        if (identical(v$kind, "numeric")) {
          sprintf("%s (number, %s to %s, n=%d)", v$column, v$range[1], v$range[2], v$n)
        } else {
          sprintf("%s (groups: %s%s, n=%d)", v$column,
                  paste(v$levels, collapse = "/"),
                  if (v$more > 0) sprintf(" +%d more", v$more) else "", v$n)
        }
      }, character(1)))
    )
  })
  list(
    question = question,
    earlier_questions = if (length(history)) unlist(history, use.names = FALSE) else NULL,
    datasets = unname(datasets)
  )
}
