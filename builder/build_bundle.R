#!/usr/bin/env Rscript
# Build a HealthScope Mobile data bundle for one country.
#
#   Rscript build_bundle.R --country TZA --etl-dir <ETL country_standardized folder> --out <output folder>
#
# Run with --help for all options. See README.md.

usage <- '
Usage:
  Rscript build_bundle.R --country ISO3 --etl-dir DIR --out DIR [options]
  Rscript build_bundle.R --country TZA --hfr-csv FILE --out DIR [options]

Required:
  --country ISO3            Country code from config/countries.csv, e.g. TZA
  --etl-dir DIR             Folder holding {country}_standardized.csv and
                            {country}_services_standardized.csv
    or --hfr-csv FILE       The HFR export (hfr-pipeline, Tanzania only), e.g.
                            hfr-facilities-2026-10-18.csv; its services are
                            mapped with config/hfr_service_map.csv
    or --climate-api URL    The climate-sensitive facilities API (Tanzania only),
                            https://hfr-pipeline.vercel.app/api/v1/climate.
                            The download is kept as climate-api-snapshot.json
                            in --out; services follow config/climate_tiers.csv
                            and config/climate_service_map.csv
    or --climate-json FILE  Build from a saved climate API snapshot instead
  --out DIR                 Output folder (manifest.json in it is updated)

Options:
  --config-dir DIR          Config folder (default: config/ next to this script)
  --base-url URL            Public base URL the files will be uploaded under
  --github-repo OWNER/REPO  Host on GitHub Releases instead: one release per
                            bundle version (tag {country}-{version}) and per
                            basemap; publish with publish_github.ps1
  --etl-source-date DATE    YYYY-MM-DD; default: date of the latest ETL git commit,
                            or the date in the HFR export file name
  --etl-commit HASH         Default: hash of the latest ETL git commit
  --bundle-version N        Default: YYYYMMDDNN, increasing within a day
  --min-app-version X.Y.Z   Default: 1.0.0
  --basemap FILE            Offline basemap (.pmtiles) to publish with this build;
                            without it, an already-published basemap is kept
  --basemap-date DATE       YYYY-MM-DD the basemap was extracted (default: file date)
  --release                 Refuse to build while labels are untranslated or
                            flagged for review; requires --base-url or --github-repo
  --quiet                   Less output
  --help                    Show this text
'

script_dir <- function() {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
}

parse_args <- function(args) {
  flags <- c("--release", "--quiet", "--help")
  out <- list()
  i <- 1
  while (i <= length(args)) {
    a <- args[i]
    if (a %in% flags) { out[[sub("^--", "", a)]] <- TRUE; i <- i + 1; next }
    if (!startsWith(a, "--") || i == length(args)) stop(sprintf("Unexpected or incomplete argument: %s", a), call. = FALSE)
    out[[sub("^--", "", a)]] <- args[i + 1]
    i <- i + 2
  }
  out
}

main <- function() {
  args <- tryCatch(parse_args(commandArgs(TRUE)), error = function(e) { message(conditionMessage(e)); cat(usage); quit(status = 2) })
  if (isTRUE(args$help)) { cat(usage); quit(status = 0) }
  other_source <- !is.null(args[["hfr-csv"]]) || !is.null(args[["climate-api"]]) || !is.null(args[["climate-json"]])
  missing <- setdiff(c("country", "out", if (!other_source) "etl-dir"), names(args))
  if (length(missing)) { message("Missing: ", paste0("--", missing, collapse = ", ")); cat(usage); quit(status = 2) }

  here <- script_dir()
  for (f in list.files(file.path(here, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
  options(bundle_builder.quiet = isTRUE(args$quiet))

  result <- tryCatch(
    build_bundle(
      country = args$country,
      etl_dir = args[["etl-dir"]],
      out_dir = args$out,
      config_dir = args[["config-dir"]] %||% file.path(here, "config"),
      release = isTRUE(args$release),
      base_url = args[["base-url"]],
      etl_source_date = args[["etl-source-date"]],
      etl_commit = args[["etl-commit"]],
      bundle_version = if (!is.null(args[["bundle-version"]])) as.numeric(args[["bundle-version"]]) else NULL,
      min_app_version = args[["min-app-version"]] %||% "1.0.0",
      basemap = args$basemap,
      basemap_date = args[["basemap-date"]],
      github_repo = args[["github-repo"]],
      hfr_csv = args[["hfr-csv"]],
      climate_json = args[["climate-json"]],
      climate_api = args[["climate-api"]]
    ),
    bundle_validation_error = function(e) {
      message("\nBUILD FAILED: ", conditionMessage(e))
      quit(status = 1)
    }
  )
  invisible(result)
}

if (sys.nframe() == 0L) main()
