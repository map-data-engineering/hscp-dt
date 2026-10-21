# Orchestrates one country build. Outputs are written to temporary names and
# renamed only after everything succeeded, so a failed build never replaces a
# previous good bundle.

DEFAULT_BASE_URL <- "https://REPLACE-WITH-HOST/bundles"

# GitHub Releases host files flat under one tag per release, so with
# github_repo each bundle version and each basemap becomes its own release:
#   https://github.com/OWNER/REPO/releases/download/{slug}-{bundle_version}/{file}
#   https://github.com/OWNER/REPO/releases/download/{slug}-basemap-{sha12}/{slug}.pmtiles
# The app reads the manifest from .../releases/latest/download/manifest.json.
github_base_url <- function(repo) {
  if (!grepl("^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+$", repo))
    validation_error(sprintf("--github-repo must look like OWNER/REPO, got '%s'", repo))
  sprintf("https://github.com/%s/releases/download", repo)
}

build_bundle <- function(country, etl_dir, out_dir, config_dir,
                         release = FALSE, base_url = NULL, etl_source_date = NULL,
                         etl_commit = NULL, bundle_version = NULL,
                         min_app_version = "1.0.0", today = Sys.Date(),
                         basemap = NULL, basemap_date = NULL, github_repo = NULL,
                         hfr_csv = NULL, climate_json = NULL, climate_api = NULL) {
  climate <- !is.null(climate_json) || !is.null(climate_api)
  if (sum(!is.null(hfr_csv), climate) > 1) validation_error("Give one source: --etl-dir, --hfr-csv, or --climate-api / --climate-json.")
  if (!is.null(climate_json) && !is.null(climate_api)) validation_error("Give either --climate-api or --climate-json, not both.")
  if (!is.null(github_repo)) {
    if (!is.null(base_url)) validation_error("Give either --base-url or --github-repo, not both.")
    base_url <- github_base_url(github_repo)
  }
  # Folder (and URL path) for one release's files: nested for plain static
  # hosts, a single flat release tag for GitHub.
  release_dir <- function(slug, name) if (is.null(github_repo)) file.path(slug, name) else paste0(slug, "-", name)
  if (!is.null(basemap) && !file.exists(basemap)) validation_error(sprintf("Basemap file not found: %s", basemap))
  basemap_info <- if (!is.null(basemap)) pmtiles_header(basemap) else NULL
  started <- Sys.time()
  if (is.null(hfr_csv) && !climate && !dir.exists(etl_dir)) validation_error(sprintf("ETL directory not found: %s", etl_dir))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  cfg  <- load_config(config_dir)
  ctry <- country_config(cfg, country)
  iso  <- toupper(ctry$country_iso3)
  slug <- ctry$country_slug
  log_msg("Building %s (%s) from %s", ctry$country_name, iso, climate_json %||% climate_api %||% hfr_csv %||% etl_dir)

  report <- new_report()
  source_checksum <- NULL
  if (climate) {
    # The climate API replaces the ETL CSVs: the download is kept next to the
    # outputs as climate-api-snapshot.json, and the build reads that file.
    if (!is.null(climate_api)) {
      climate_json <- file.path(out_dir, "climate-api-snapshot.json")
      write_climate_snapshot(fetch_climate_api(climate_api), climate_json)
    }
    snapshot <- read_climate_snapshot(climate_json)
    cl <- read_climate(snapshot, ctry, config_dir)
    fac_raw <- cl$facilities
    svc_raw <- NULL
    report$excluded <- c(report$excluded, cl$excluded)
    report$counts <- c(report$counts, cl$counts)
    for (w in cl$warnings) report <- add_warning(report, w)
    cfg$checksum <- digest::digest(paste(c(cfg$checksum, vapply(CLIMATE_CONFIG_FILES, function(f) sha256_file(file.path(config_dir, f)), "")),
                                         collapse = ""), algo = "sha256", serialize = FALSE)
    source_checksum <- climate_source_checksum(snapshot, cfg$checksum)
    ctry$source_attribution <- snapshot$attribution %||% CLIMATE_SOURCE_NAME
  } else if (is.null(hfr_csv)) {
    fac_raw <- read_facilities(etl_dir, ctry)
    svc_raw <- read_services(etl_dir, ctry)
  } else {
    # The HFR export replaces the ETL CSVs: its own reader, service map and attribution.
    hfr <- read_hfr(hfr_csv, ctry)
    fac_raw <- hfr$facilities
    svc_raw <- hfr$services
    report$excluded <- c(report$excluded, hfr$excluded)
    report$counts <- c(report$counts, hfr$counts)
    cfg$map <- load_hfr_service_map(config_dir, cfg)
    cfg$checksum <- digest::digest(paste(cfg$checksum, sha256_file(file.path(config_dir, "hfr_service_map.csv"))),
                                   algo = "sha256", serialize = FALSE)
    ctry$source_attribution <- HFR_ATTRIBUTION
  }
  n_svc <- if (climate) nrow(cl$links) else if (is.null(svc_raw)) NA else nrow(svc_raw)
  log_msg("Read %d facility rows, %s service rows", nrow(fac_raw), if (is.na(n_svc)) "no" else format(n_svc, big.mark = ","))

  tf  <- transform_facilities(fac_raw, ctry, cfg, report); report <- tf$report
  svc <- if (climate) transform_climate_services(cl, tf$facilities, report) else transform_services(svc_raw, tf$facilities, ctry, cfg, report)
  report <- svc$report
  fac <- finalise_facilities(tf$facilities, svc)
  admin_units <- build_admin_units(fac)
  report$counts$nearest_eligible <- sum(fac$nearest_eligible)

  types <- cfg$types[toupper(cfg$types$country_iso3) == iso & cfg$types$type_key %in% fac$type_key, , drop = FALSE]
  types <- types[!duplicated(types$type_key), ]
  types <- types[order(as.integer(types$sort_order)), ]

  if (release) {
    check_release_labels(
      used_services = if (isTRUE(svc$available)) svc$services else cfg$labels[0, ],
      used_domains  = if (isTRUE(svc$available)) svc$domains else cfg$domains[0, ],
      used_types    = types)
    if (is.null(base_url)) validation_error("--release needs --base-url or --github-repo (the public host for the bundle files).")
  }

  git <- if (climate) {
    list(commit = "climate-api", date = substr(snapshot$fetched_at, 1, 10))
  } else if (is.null(hfr_csv)) {
    etl_git_info(etl_dir)
  } else {
    list(commit = "hfr-export", date = hfr_snapshot_date(hfr_csv))
  }
  etl_commit <- etl_commit %||% git$commit
  etl_source_date <- etl_source_date %||% git$date
  if (is.null(etl_source_date) || is.na(etl_source_date))
    validation_error("Could not read the ETL commit date (is --etl-dir inside the ETL git repo?). Pass --etl-source-date YYYY-MM-DD.")
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", etl_source_date))
    validation_error(sprintf("--etl-source-date must be YYYY-MM-DD, got '%s'", etl_source_date))

  manifest_path <- file.path(out_dir, "manifest.json")
  bundle_version <- bundle_version %||% next_bundle_version(manifest_path, iso, today)
  if (is.null(base_url)) {
    report <- add_warning(report, sprintf("No --base-url given; manifest URLs use the placeholder %s", DEFAULT_BASE_URL))
    base_url <- DEFAULT_BASE_URL
  }
  base_url <- sub("/+$", "", base_url)
  kept_map <- if (is.null(basemap)) existing_basemap(manifest_path, iso) else NULL
  if (!is.null(kept_map) && !startsWith(kept_map$url, paste0(base_url, "/"))) {
    report <- add_warning(report, sprintf(
      "The kept basemap URL (%s) is not under %s; pass --basemap again to republish it there.", kept_map$url, base_url))
  }
  built_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  # I() keeps warnings a JSON array even when there is exactly one.
  report$warnings <- I(report$warnings)
  report_for_meta <- list(excluded = report$excluded, warnings = report$warnings)
  meta <- list(
    schema_version = SCHEMA_VERSION,
    bundle_version = format(bundle_version, scientific = FALSE),
    country_iso3 = iso,
    country_slug = slug,
    country_name = ctry$country_name,
    built_at = built_at,
    etl_source_date = etl_source_date,
    etl_commit = etl_commit %||% "unknown",
    source_attribution = ctry$source_attribution,
    services_available = as.integer(isTRUE(svc$available)),
    admin_level_keys = paste(ctry$admin_levels, collapse = ","),
    builder_version = BUILDER_VERSION,
    labels_checksum = cfg$checksum,
    build_report = as.character(jsonlite::toJSON(report_for_meta, auto_unbox = TRUE))
  )
  if (!is.null(source_checksum)) meta$source_checksum <- source_checksum

  # ---- write everything under temporary names ----
  f <- function(name) file.path(out_dir, name)
  final <- c(sqlite = f(paste0(slug, ".sqlite")), sqlite_gz = f(paste0(slug, ".sqlite.gz")),
             summ = f(paste0(slug, "_summaries.json")), summ_gz = f(paste0(slug, "_summaries.json.gz")))
  tmp <- stats::setNames(paste0(final, ".building"), names(final))
  on.exit(unlink(tmp[file.exists(tmp)]), add = TRUE)

  log_msg("Writing SQLite bundle")
  table_counts <- write_bundle_sqlite(tmp[["sqlite"]], fac, admin_units, types, svc, meta)
  write_json_file(build_summaries(fac, svc, meta), tmp[["summ"]])
  gzip_file(tmp[["sqlite"]], tmp[["sqlite_gz"]])
  gzip_file(tmp[["summ"]], tmp[["summ_gz"]])

  for (k in names(final)) {
    if (file.exists(final[[k]])) file.remove(final[[k]])
    if (!file.rename(tmp[[k]], final[[k]])) stop(sprintf("Could not rename %s", tmp[[k]]))
  }

  data_dir <- release_dir(slug, format(bundle_version, scientific = FALSE))
  url <- function(file) sprintf("%s/%s/%s", base_url, gsub("\\\\", "/", data_dir), file)

  # Optional offline basemap: published under its own content-addressed folder.
  basemap_entry <- existing_basemap(manifest_path, iso)
  if (!is.null(basemap)) {
    sha <- sha256_file(basemap)
    map_dir <- release_dir(slug, paste0("basemap-", substr(sha, 1, 12)))
    dir.create(file.path(out_dir, "publish", map_dir), recursive = TRUE, showWarnings = FALSE)
    file.copy(basemap, file.path(out_dir, "publish", map_dir, paste0(slug, ".pmtiles")), overwrite = TRUE)
    basemap_entry <- list(
      url = sprintf("%s/%s/%s.pmtiles", base_url, gsub("\\\\", "/", map_dir), slug),
      bytes = file.info(basemap)$size,
      sha256 = sha,
      maxzoom = basemap_info$max_zoom,
      built_on = basemap_date %||% format(as.Date(file.info(basemap)$mtime), "%Y-%m-%d")
    )
    log_msg("Basemap: %.1f MB, max zoom %d", basemap_entry$bytes / 1e6, basemap_entry$maxzoom)
  }

  entry <- list(
    country_iso3 = iso, country_slug = slug,
    bundle_version = bundle_version, schema_version = SCHEMA_VERSION,
    min_app_version = min_app_version,
    etl_source_date = etl_source_date, etl_commit = etl_commit %||% "unknown",
    built_at = built_at, services_available = isTRUE(svc$available),
    bbox = list(min_lat = ctry$min_lat, max_lat = ctry$max_lat, min_lon = ctry$min_lon, max_lon = ctry$max_lon),
    files = list(
      bundle = file_entry(final[["sqlite_gz"]], final[["sqlite"]], url(basename(final[["sqlite_gz"]]))),
      summaries = file_entry(final[["summ_gz"]], final[["summ"]], url(basename(final[["summ_gz"]])))
    )
  )
  if (!is.null(basemap_entry)) entry$files$basemap <- basemap_entry
  # Lets a scheduled build tell whether anything changed since the published bundle.
  if (!is.null(source_checksum)) entry$source_checksum <- source_checksum
  update_manifest(manifest_path, entry)

  # out/publish/ mirrors the URL layout, ready to upload or serve as-is:
  #   publish/manifest.json, publish/{slug}/{bundle_version}/{files}.gz
  # or, with github_repo, one folder per release: publish/{slug}-{bundle_version}/
  version_dir <- file.path(out_dir, "publish", data_dir)
  dir.create(version_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(c(final[["sqlite_gz"]], final[["summ_gz"]]), version_dir, overwrite = TRUE)
  file.copy(manifest_path, file.path(out_dir, "publish", "manifest.json"), overwrite = TRUE)

  report$country_iso3 <- iso
  report$bundle_version <- bundle_version
  report$source_checksum <- source_checksum
  report$release <- release
  report$table_counts <- table_counts
  report$files <- list(
    sqlite_bytes = file.info(final[["sqlite"]])$size, sqlite_gz_bytes = file.info(final[["sqlite_gz"]])$size,
    summaries_bytes = file.info(final[["summ"]])$size, summaries_gz_bytes = file.info(final[["summ_gz"]])$size)
  report$seconds <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
  write_json_file(report, f(paste0(slug, "_build_report.json")), pretty = TRUE)

  log_msg("Done in %.1f s: %s (%.1f MB, %.1f MB gzipped), %d warning(s)", report$seconds, basename(final[["sqlite"]]),
          report$files$sqlite_bytes / 1e6, report$files$sqlite_gz_bytes / 1e6, length(report$warnings))
  invisible(report)
}
