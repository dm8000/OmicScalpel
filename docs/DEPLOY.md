# Deploying on the HPC

The app is one shiny-server folder now, not nine. Everything below assumes
shiny-server serves a directory per URL, which is how the nine apps were served.

## What changes for the researcher

One URL instead of nine. The dataset is picked once, in the sidebar, and every
single-dataset tab follows it.

## First deployment

1. **Copy the tree** to the server, without the machine-local files:

       ./tools/deploy.sh /srv/shiny-server/omicscalpel

   It refuses to copy `config/config.txt`, `data-sample/`, `legacy/`, `.git`
   and the test fixtures. Pass `--dry-run` first to see what it would do.

2. **Write the server's config.** On the server:

       cd /srv/shiny-server/omicscalpel
       cp config/config.txt.example config/config.txt
       $EDITOR config/config.txt

   Four paths, and they are the only place any path is written by hand:

       lib     = /n/shiny/OmicScalpel/lib
       hubdata = /n/shiny/OmicScalpel/hubdata
       backups = /n/shiny/OmicScalpel/backups
       logs    = /n/shiny/OmicScalpel/logs

3. **Generate .Renviron**:

       Rscript tools/sync-renviron.R

   This is not optional and not decorative. shiny-server starts R and loads
   shiny *before* it reaches `global.R`, and on the HPC shiny lives inside
   `lib/`. R only learns about `lib/` from `.Renviron`, which it reads at
   startup. `.Renviron` is generated from `config.txt` rather than edited, so
   there is still one place to change a path.

4. **Check the packages**:

       Rscript tools/check_deps.R

   Exits 0 when every tab can run. Otherwise it prints which module each
   missing package blocks.

5. **Permissions.** The shiny-server user needs read on `hubdata/` and its
   dataset folders, and **write** on `hubdata/Metadata.xlsx`,
   `hubdata/Datasets_summary.xlsx`, `backups/` and `logs/`. The upload tab also
   needs to create directories under `hubdata/`.

## Changing a path later

Edit `config/config.txt`, then:

    Rscript tools/sync-renviron.R

and restart the app. Nothing else refers to a path.

## Updating

    ./tools/deploy.sh /srv/shiny-server/omicscalpel

`config/config.txt` and `.Renviron` on the server are left alone. Run
`sync-renviron.R` again only if `lib` changed.

## If something is missing after the merge

The nine original apps are in `legacy/`, unmodified except for their paths,
and each still runs on its own:

    cd legacy/export-matrix && R -e 'shiny::runApp(".")'

They are the reference: if a tab disagrees with its legacy app on the same
data, the legacy app is right until proven otherwise. They are also what
`docs/ARCHITECTURE.md` means by "parity".

## Rolling back

    git log --oneline

One commit per module, so `git revert` on a single conversion puts one tab back
without touching the other eight.
