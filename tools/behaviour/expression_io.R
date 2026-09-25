#!/usr/bin/env Rscript
# Rounding the values, and reading one gene without the matrix.
#
#   Rscript tools/behaviour/expression_io.R
#
# Both of these touch every number the app shows, so both are checked against
# the full read they replace rather than against themselves.

source("global.R")
source("tools/behaviour/_fixture.R")
use_fixture_hub()

n <- 0L
ok  <- function(w) { n <<- n + 1L; cat("  ok  ", w, "\n", sep = "") }
no  <- function(w, ...) { cat("FAIL  ", w, ": ", ..., "\n", sep = ""); quit(status = 1) }
chk <- function(c, w, ...) if (isTRUE(c)) ok(w) else no(w, ...)

# --- the rounding rule --------------------------------------------------------

chk(identical(os_round_expression(11270.910037), 11270.9),
    "a large value keeps one decimal", os_round_expression(11270.910037))
chk(identical(os_round_expression(4945.053081999999), 4945.1),
    "and the float noise goes with it")
chk(identical(os_round_expression(0.023291), 0.0233),
    "a value below 0.1 keeps three significant digits, not zero",
    os_round_expression(0.023291))
chk(identical(os_round_expression(0), 0), "zero stays zero")
chk(identical(os_round_expression(-0.0456), -0.0456), "and so does a small negative")
chk(is.na(os_round_expression(NA)), "missing stays missing")

# The whole point of the exception. With a flat round(x, 1) these become one
# gene of zeros, which reads as "not expressed" rather than "barely expressed".
faint <- c(0.041, 0.012, 0.0009, 0.033)
chk(all(os_round_expression(faint) != 0),
    "a faint gene survives the rounding", paste(os_round_expression(faint), collapse = " "))
chk(all(round(faint, 1) == 0), "where a flat one decimal would have erased it")

# format(v, scientific = FALSE) formats the whole vector alike, so one small
# value pads every number in the row -- and on 1e-300 it writes three hundred
# characters. Element by element, with three significant digits below 0.1.
chk(max(nchar(os_format_expression(c(11270.910037, 1e-300, 0.0233)))) <= 8,
    "a tiny value does not write three hundred characters",
    paste(os_format_expression(c(11270.910037, 1e-300, 0.0233)), collapse = " "))
chk(identical(os_format_expression(0.023291), "0.0233"),
    "and a small one keeps its three digits", os_format_expression(0.023291))
back <- as.numeric(os_format_expression(c(11270.910037, 0.023291, 1.2e-5, 0)))
chk(isTRUE(all.equal(back, os_round_expression(c(11270.910037, 0.023291, 1.2e-5, 0)))),
    "what is written reads back as what was rounded")
chk(identical(os_format_expression(11270.910037), "11270.9"),
    "and drops the zeros it does not need")

# --- one row against the whole matrix ----------------------------------------

DS <- "DEMO_RNAseq"
full <- load_expression(DS, "TPM")
genes <- full$Symbol[c(1, 5, nrow(full))]

row <- load_expression_row(DS, genes, "TPM")
chk(!is.null(row) && nrow(row) == length(genes),
    "every gene asked for comes back", if (is.null(row)) 0 else nrow(row))
chk(setequal(row$Symbol, genes), "and only those")
chk(identical(names(row), names(full)),
    "with the same columns, in the same order as the full read")

for (g in genes) {
  a <- as.numeric(row[row$Symbol == g, -1])
  b <- as.numeric(full[full$Symbol == g, -1])
  if (!isTRUE(all.equal(a, b))) no("the values match the full read", g)
}
ok("the values are the ones the full read gives, gene by gene")

# --- the trap ----------------------------------------------------------------
# ^LEP without the tab also matches LEPR and LEPROT. This is the mistake that
# would be caught by nobody: the plot draws, with another gene's numbers.
tmp <- file.path(tempdir(), "prefix_test")
dir.create(file.path(tmp, "PREFIX"), recursive = TRUE, showWarnings = FALSE)
writeLines(c("Symbol\ts1\ts2", "LEP\t1\t2", "LEPR\t30\t40", "LEPROT\t50\t60"),
           file.path(tmp, "PREFIX", "PREFIX_TPM.txt"))
old_cfg <- Sys.getenv("OMICSCALPEL_CONFIG")
cfg <- file.path(tmp, "config.txt")
writeLines(c(paste0("lib     = ", .libPaths()[1]), paste0("hubdata = ", tmp),
             paste0("backups = ", tmp), paste0("logs    = ", tmp)), cfg)
Sys.setenv(OMICSCALPEL_CONFIG = cfg); invisible(read_config(reload = TRUE))

r <- load_expression_row("PREFIX", "LEP", "TPM")
chk(identical(r$Symbol, "LEP") && identical(as.numeric(r[1, -1]), c(1, 2)),
    "asking for LEP does not return LEPR", paste(r$Symbol, collapse = ","))
r2 <- load_expression_row("PREFIX", c("LEP", "LEPROT"), "TPM")
chk(setequal(r2$Symbol, c("LEP", "LEPROT")) && nrow(r2) == 2,
    "two genes that share a prefix both come back, once each",
    paste(r2$Symbol, collapse = ","))
chk(is.null(load_expression_row("PREFIX", "NOTAGENE", "TPM")),
    "a gene that is not there returns nothing, not an error")

Sys.setenv(OMICSCALPEL_CONFIG = old_cfg); invisible(read_config(reload = TRUE))

cat("\n", n, " checks passed\n", sep = "")
