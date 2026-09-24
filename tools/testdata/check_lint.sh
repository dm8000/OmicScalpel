#!/bin/sh
# Verifier for lint_ns.R. Run from the tools/ directory.
# Proves the linter can fail, not only that it runs.
fail() { echo "FAIL: $1"; [ -n "$2" ] && echo "--- output ---" && cat "$2"; exit 1; }
tmp=$(mktemp)

# 1. the entry point must actually execute
Rscript lint_ns.R --help >/dev/null 2>&1 || fail "--help did not exit 0"

# 2. a correct module: exit 0 and say nothing
out=$(Rscript lint_ns.R testdata/good_module.R 2>&1)
[ $? -eq 0 ] || fail "good_module.R reported problems: $out"
[ -z "$out" ] || fail "good_module.R produced output: $out"

# 3. a broken module: exit non-zero and name every one of the 7 problems
if Rscript lint_ns.R --single-dataset testdata/bad_module.R >"$tmp" 2>&1; then
  fail "bad_module.R exited 0" "$tmp"
fi
for tok in 'dataset' 'go' 'plot' 'conditionalPanel' 'cutoff' 'new_name'; do
  grep -qF "$tok" "$tmp" || fail "did not report $tok" "$tmp"
done
grep -qF 'input$dataset' "$tmp" || fail "did not report input\$dataset" "$tmp"

# 4. rule D fires only under --single-dataset
if Rscript lint_ns.R testdata/bad_module.R 2>&1 | grep -qF 'input$dataset'; then
  fail "rule D fired without --single-dataset"
fi

# 5. update*() legitimately takes a bare id inside a module: never flag it
if Rscript lint_ns.R --single-dataset testdata/bad_module.R 2>&1 | grep -qF '"unit"'; then
  fail "flagged the bare id of an update*() call"
fi

# 6. two different inputs must give two different verdicts (not a constant)
a=$(Rscript lint_ns.R testdata/good_module.R >/dev/null 2>&1; echo $?)
b=$(Rscript lint_ns.R testdata/bad_module.R  >/dev/null 2>&1; echo $?)
[ "$a" = "0" ] && [ "$b" != "0" ] || fail "same verdict for good ($a) and bad ($b)"

rm -f "$tmp"
echo "lint_ns.R OK"
