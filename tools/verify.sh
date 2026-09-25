#!/bin/sh
# Run every check this project has. From the project root:
#
#   ./tools/verify.sh
#
# Order is cheapest-first, so a syntax error does not wait behind a smoke test.
# Nothing here needs the HPC: it all runs against data-sample/.

cd "$(dirname "$0")/.." || exit 2
fails=0
step() {
  printf '\n=== %s\n' "$1"
  shift
  if "$@"; then :; else fails=$((fails + 1)); echo "    ^ FAILED"; fi
}

step "parse" sh -c 'for f in app.R global.R R/*.R R/modules/*.R tools/*.R; do
  [ -e "$f" ] || continue
  Rscript -e "invisible(parse(commandArgs(TRUE)[1]))" "$f" || exit 1
done; echo "all files parse"'

step "no hardcoded paths" sh -c '
  if grep -rn "/n/shiny" --include=*.R app.R global.R R tools 2>/dev/null; then
    echo "a path escaped config/config.txt"; exit 1
  fi; echo "none"'

step "dependencies" Rscript tools/check_deps.R

step "namespaces" sh -c '
  Rscript -e '"'"'
    source("R/registry.R")
    bad <- 0
    for (m in MODULES) {
      f <- module_file(m)
      if (!file.exists(f)) next
      flag <- if (identical(m$scope, "one")) "--single-dataset" else ""
      cmd <- paste("Rscript tools/lint_ns.R", flag, shQuote(f))
      if (system(cmd) != 0) bad <- bad + 1
    }
    if (bad) { cat(bad, "module(s) with namespace problems\n"); quit(status = 1) }
    cat("every built module is namespaced\n")
  '"'"''

step "no control was lost in conversion" Rscript tools/lint_parity.R

step "three-column layout" Rscript tools/lint_layout.R

step "dplyr masking" sh -c '
  Rscript tools/lint_masking.R app.R R/*.R R/modules/*.R 2>/dev/null &&
  echo "no verb compares a column with itself"'

step "the linter can still fail" sh -c '
  if Rscript tools/lint_ns.R --single-dataset legacy/export-matrix/app.R >/dev/null 2>&1 \
     || Rscript tools/lint_ns.R --single-dataset export-matrix/app.R >/dev/null 2>&1; then
    echo "lint_ns.R passed an unconverted app; it is not checking anything"; exit 1
  fi; echo "it still rejects an unconverted app"'

step "fixtures carry no real identifier" Rscript tools/testdata/check_fixtures.R
step "the namespace linter itself" sh -c 'cd tools && sh testdata/check_lint.sh'
step "data layer" Rscript tools/test_data_io.R

step "modules start" Rscript tools/smoke_test.R

for b in tools/behaviour/*.R; do
  case "$(basename "$b")" in _*) continue;; esac
  [ -e "$b" ] || continue
  step "behaviour: $(basename "$b" .R)" Rscript "$b"
done

printf '\n'
if [ "$fails" -eq 0 ]; then echo "everything passed"; else echo "$fails step(s) failed"; fi
exit "$fails"
