# One-time setup:  Rscript install_packages.R
pkgs <- c("readr", "DBI", "RSQLite", "jsonlite", "digest", "curl", "testthat", "withr")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
lib <- Sys.getenv("R_LIBS_USER")
if (length(missing)) {
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  install.packages(missing, lib = lib, repos = "https://cloud.r-project.org")
}
library(DBI)
con <- dbConnect(RSQLite::SQLite(), ":memory:")
ok <- tryCatch({ dbExecute(con, "CREATE VIRTUAL TABLE t USING fts5(x)"); TRUE }, error = function(e) FALSE)
dbDisconnect(con)
if (!ok) stop("This RSQLite build has no FTS5 support; the bundle needs it. Update RSQLite.")
message("All packages present; RSQLite ", packageVersion("RSQLite"), " with FTS5.")
