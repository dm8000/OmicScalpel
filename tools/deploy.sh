#!/bin/sh
# Copy the app to a shiny-server directory, leaving machine-local files alone.
#
#   ./tools/deploy.sh [--dry-run] /srv/shiny-server/omicscalpel
#
# Never copies: config/config.txt or .Renviron (each machine keeps its own),
# data-sample/ and tools/testdata/ (fixtures), legacy/ (the old apps, kept in
# git rather than shipped), .git.

set -e
dry=""
if [ "$1" = "--dry-run" ]; then dry="--dry-run"; shift; fi
dest="$1"
if [ -z "$dest" ]; then
  echo "usage: $0 [--dry-run] DESTINATION" >&2
  exit 2
fi

here=$(cd "$(dirname "$0")/.." && pwd)

rsync -av --delete $dry \
  --exclude '.git/' \
  --exclude 'config/config.txt' \
  --exclude '.Renviron' \
  --exclude 'data-sample/' \
  --exclude 'tools/testdata/' \
  --exclude 'legacy/' \
  --exclude '.Rhistory' \
  --exclude '.RData' \
  --exclude '.local/' \
  "$here/" "$dest/"

if [ -n "$dry" ]; then
  echo
  echo "dry run; nothing was copied."
  exit 0
fi

echo
echo "Copied. On the server, still to do:"
echo "  1. cp config/config.txt.example config/config.txt   (first time only)"
echo "  2. Rscript tools/sync-renviron.R                    (if lib changed)"
echo "  3. Rscript tools/check_deps.R"
