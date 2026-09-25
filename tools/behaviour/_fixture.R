# Pin a behaviour test to data-sample, whatever config/config.txt points at.
#
# Several tests read load_metadata() straight from the configured hub. That was
# fine while the config pointed at the fixtures and wrong the moment it pointed
# at the real data: the same test then asked different questions and failed for
# reasons that had nothing to do with the code. A test must not change meaning
# with local configuration.
#
# Call use_fixture_hub() right after source("global.R"). Pass writable = TRUE
# when the test saves, and it gets a throwaway copy instead of the real folder.

use_fixture_hub <- function(writable = FALSE) {
  root <- normalizePath(".")
  if (!writable) {
    cfg <- file.path(tempdir(), paste0("os-ro-", Sys.getpid(), ".txt"))
    writeLines(c(paste0("lib     = ", .libPaths()[1]),
                 paste0("hubdata = ", file.path(root, "data-sample")),
                 paste0("backups = ", file.path(tempdir(), "os-bk")),
                 paste0("logs    = ", file.path(tempdir(), "os-log"))), cfg)
    Sys.setenv(OMICSCALPEL_CONFIG = cfg)
    invisible(read_config(reload = TRUE))
    return(invisible(file.path(root, "data-sample")))
  }

  tmp <- file.path(tempdir(), paste0("os-hub-", Sys.getpid()))
  unlink(tmp, recursive = TRUE)
  dir.create(file.path(tmp, "hub"), recursive = TRUE)
  invisible(file.copy(list.files(file.path(root, "data-sample"), full.names = TRUE),
                      file.path(tmp, "hub"), recursive = TRUE))
  writeLines(c(paste0("lib     = ", .libPaths()[1]),
               paste0("hubdata = ", file.path(tmp, "hub")),
               paste0("backups = ", file.path(tmp, "bk")),
               paste0("logs    = ", file.path(tmp, "logs"))),
             file.path(tmp, "config.txt"))
  Sys.setenv(OMICSCALPEL_CONFIG = file.path(tmp, "config.txt"))
  invisible(read_config(reload = TRUE))
  invisible(tmp)
}
