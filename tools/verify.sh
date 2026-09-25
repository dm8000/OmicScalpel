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

step "the machine interface matches the GUI" Rscript tools/lint_ai_tools.R

step "shiny is not masked" sh -c '
  # global.R attaches jsonlite after shiny, so a bare validate() is jsonlite\'"'"'s
  # -- it checks whether a string is valid JSON. A guard written as
  # validate(need(...)) then either errors or, worse, lets the render carry on
  # past it. Both happened here.
  hits=$(grep -rn "[^:a-zA-Z._]validate(" R --include=*.R | grep -v "shiny::validate" \
         | grep -v "^[^:]*:[0-9]*: *#" || true)
  if [ -n "$hits" ]; then
    echo "validate() here is jsonlite::validate; write shiny::validate:"; echo "$hits"; exit 1
  fi
  echo "no module calls a masked validate()"'

step "dplyr masking" sh -c '
  Rscript tools/lint_masking.R app.R R/*.R R/modules/*.R 2>/dev/null &&
  echo "no verb compares a column with itself"'

step "the linter can still fail" sh -c '
  if Rscript tools/lint_ns.R --single-dataset legacy/export-matrix/app.R >/dev/null 2>&1 \
     || Rscript tools/lint_ns.R --single-dataset export-matrix/app.R >/dev/null 2>&1; then
    echo "lint_ns.R passed an unconverted app; it is not checking anything"; exit 1
  fi; echo "it still rejects an unconverted app"'

step "no secret is tracked" sh -c '
  if git ls-files --error-unmatch apikeys.txt >/dev/null 2>&1; then
    echo "apikeys.txt is tracked -- remove it from the index and rotate the key"; exit 1
  fi
  hits=$(git ls-files -z | xargs -0 grep -l -I -E "apikey_[A-Za-z0-9]{8}|sk-[A-Za-z0-9]{20}" 2>/dev/null)
  if [ -n "$hits" ]; then echo "a key is in a tracked file:"; echo "$hits"; exit 1; fi
  echo "no key in the index"'

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
