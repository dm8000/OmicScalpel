#!/usr/bin/env Rscript
# The map that puts back the symbols a spreadsheet turned into dates.
#
#   Rscript tools/behaviour/gene_names.R
#
# The danger here is not missing a corrupted name; it is repairing one that was
# never broken. DEC1 and SEP15 are real symbols -- deleted-in-esophageal-
# cancer-1 and the old name for SELENOF -- and any rule written as a pattern
# over month names destroys both. So the map is literal, and this is the check
# that it stays that way.

source("global.R")

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

path <- file.path(os_root(), "config", "gene-name-fixes.txt")
chk(file.exists(path), "the map exists", path)
lines <- readLines(path, warn = FALSE)
lines <- lines[!grepl("^\\s*(#|$)", lines)]
parts <- strsplit(lines, "\\s*\\|\\s*")
chk(all(lengths(parts) >= 2), "every line has a from and a to",
    sum(lengths(parts) < 2), " malformed")

from <- trimws(vapply(parts, `[`, character(1), 1))
to   <- trimws(vapply(parts, `[`, character(1), 2))

chk(!any(duplicated(from)), "no name is mapped twice",
    paste(from[duplicated(from)], collapse = ", "))

# The whole point.
REAL <- c("DEC1", "SEP15", "SEPT9", "MARCH1", "MARC1", "MARC2", "SELENOF")
chk(!any(REAL %in% from),
    "no real gene symbol is treated as corruption",
    paste(intersect(REAL, from), collapse = ", "))

chk(all(grepl("^[A-Za-z][A-Za-z0-9._-]*$", to)),
    "every repaired name looks like a symbol and not like a date",
    paste(to[!grepl("^[A-Za-z]", to)], collapse = ", "))

# A date written as a serial number is a date written twice; both spellings of
# the same gene must land on the same symbol.
chk(identical(to[from == "45352"], to[from == "1-Mar"]),
    "the serial and the text of one date repair to the same gene",
    to[from == "45352"], " vs ", to[from == "1-Mar"])
chk(identical(to[from == "45627"], "DEC1"),
    "and 2024-12-01 is DEC1, which is why DEC1 must never be a key")

# --- against the hub, when there is one ---------------------------------------
# Four symbols in GTEX are serial numbers that decode to the first of August in
# four different years, and no gene family is named that way. They are left
# alone deliberately: a wrong symbol is worse than an obviously broken one.
UNDECIDED <- c("37104", "37469", "37834", "38200")

idx <- tryCatch(ai_gene_index(), error = function(e) NULL)
if (is.null(idx)) {
  cat("  --   no gene index here, skipping the hub check\n")
} else {
  mo <- "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
  left <- list()
  for (d in names(idx$datasets)) {
    s <- idx$datasets[[d]]
    bad <- c(grep(paste0("^[0-9]{1,2}[-/ ]?", mo, "$|^", mo, "[-/ ]?[0-9]{1,2}$"), s,
                  value = TRUE, ignore.case = TRUE),
             s[!grepl("[A-Za-z]", s)])
    bad <- setdiff(bad, c(REAL, UNDECIDED))
    bad <- bad[nzchar(trimws(bad))]
    if (length(bad)) left[[d]] <- bad
  }
  chk(!length(left), "no dataset still carries a repairable date as a gene name",
      paste(names(left), unlist(left), collapse = "; "))
  chk(length(ai_datasets_with_gene("MARCH1")) > 0 &&
      length(ai_datasets_with_gene("SEPT9")) > 0,
      "and the repaired genes can be found again")
}

cat("\n", n, " checks passed\n", sep = "")
