# OmicScalpel -- path configuration.
#
# Single source of truth for every filesystem path the app touches. Values come
# from config/config.txt; see config/config.txt.example for the format.
#
# Base R only, on purpose: this file is sourced *before* .libPaths() is pointed
# at the deployment library, so at this moment nothing under lib/ is loadable.

.os_cache <- new.env(parent = emptyenv())

# Walk up from `start` until a directory holding config/config.txt.example is
# found. Works both from the project root (unified app) and from a subdirectory
# (a legacy app in its own folder).
.os_find_root <- function(start = getwd()) {
  marker <- file.path("config", "config.txt.example")
  dir <- normalizePath(start, mustWork = FALSE)
  repeat {
    if (file.exists(file.path(dir, marker))) return(dir)
    parent <- dirname(dir)
    if (identical(parent, dir)) break
    dir <- parent
  }
  stop("OmicScalpel: project root not found at or above '", start,
       "' (looked for ", marker, "). Set OMICSCALPEL_ROOT to point at it.",
       call. = FALSE)
}

os_root <- function() {
  if (is.null(.os_cache$root)) {
    env <- Sys.getenv("OMICSCALPEL_ROOT", "")
    .os_cache$root <- if (nzchar(env)) normalizePath(env, mustWork = TRUE) else .os_find_root()
  }
  .os_cache$root
}

# Resolution order: $OMICSCALPEL_CONFIG, then config/config.txt, then the
# committed example. The example is the fallback so a fresh checkout runs.
.os_config_file <- function() {
  env <- Sys.getenv("OMICSCALPEL_CONFIG", "")
  if (nzchar(env)) {
    if (!file.exists(env)) {
      stop("OmicScalpel: OMICSCALPEL_CONFIG points at a missing file: ", env, call. = FALSE)
    }
    return(env)
  }
  local <- file.path(os_root(), "config", "config.txt")
  if (file.exists(local)) return(local)
  file.path(os_root(), "config", "config.txt.example")
}

read_config <- function(reload = FALSE) {
  if (reload) .os_cache$config <- NULL
  if (!is.null(.os_cache$config)) return(.os_cache$config)

  f <- .os_config_file()
  lines <- trimws(readLines(f, warn = FALSE))
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]

  bad <- !grepl("=", lines, fixed = TRUE)
  if (any(bad)) {
    stop("OmicScalpel: malformed line(s) in ", f, ": ",
         paste(lines[bad], collapse = " | "), call. = FALSE)
  }

  keys <- trimws(sub("=.*$", "", lines))
  vals <- trimws(sub("^[^=]*=", "", lines))
  if (any(!nzchar(keys))) {
    stop("OmicScalpel: line with empty key in ", f, call. = FALSE)
  }
  if (anyDuplicated(keys)) {
    stop("OmicScalpel: duplicate key(s) in ", f, ": ",
         paste(unique(keys[duplicated(keys)]), collapse = ", "), call. = FALSE)
  }

  cfg <- as.list(vals)
  names(cfg) <- keys
  cfg[[".file"]] <- f
  .os_cache$config <- cfg
  cfg
}

# os_path("hubdata")                  -> "<hubdata value from config.txt>"
# os_path("hubdata", "Metadata.xlsx") -> "<that value>/Metadata.xlsx"
#
# A relative value in config.txt resolves against the project root. That is how
# a local checkout points hubdata at data-sample/ and runs without the HPC.
os_path <- function(key, ...) {
  cfg <- read_config()
  known <- setdiff(names(cfg), ".file")
  if (!key %in% known) {
    stop("OmicScalpel: no path named '", key, "' in ", cfg[[".file"]],
         ". Known keys: ", paste(known, collapse = ", "), call. = FALSE)
  }
  p <- cfg[[key]]
  if (!nzchar(p)) {
    stop("OmicScalpel: path '", key, "' is empty in ", cfg[[".file"]], call. = FALSE)
  }
  if (!grepl("^(/|~)", p)) p <- file.path(os_root(), p)
  if (length(list(...))) file.path(p, ...) else p
}

# Which file the values actually came from. Used by docs, deploy checks and the
# "why is it reading the wrong hubdata" question.
os_config_file <- function() read_config()[[".file"]]
