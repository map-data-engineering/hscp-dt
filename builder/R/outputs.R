# Summaries JSON, compression, checksums and manifest.json.

# Named list -> JSON object, even when empty (jsonlite would write [] otherwise).
as_object <- function(x) if (length(x)) as.list(x) else structure(list(), names = character(0))

count_block <- function(fac_uids, links, svc_domain) {
  block <- list(facilities = length(fac_uids))
  if (is.null(links)) {
    block["facilities_with_service_data"] <- list(NULL)
    block["by_domain"]  <- list(NULL)
    block["by_service"] <- list(NULL)
    return(block)
  }
  l <- links[links$facility_uid %in% fac_uids, , drop = FALSE]
  block$facilities_with_service_data <- length(unique(l$facility_uid))
  by_service <- table(l$public_key)
  dom <- unique(data.frame(facility_uid = l$facility_uid, domain = svc_domain[l$public_key], stringsAsFactors = FALSE))
  by_domain <- table(dom$domain)
  block$by_domain  <- as_object(stats::setNames(as.integer(by_domain), names(by_domain)))
  block$by_service <- as_object(stats::setNames(as.integer(by_service), names(by_service)))
  block
}

build_summaries <- function(fac, svc, meta) {
  counted <- fac[fac$status_key %in% c("operating", "unknown"), , drop = FALSE]
  links <- if (isTRUE(svc$available)) svc$links else NULL
  svc_domain <- if (isTRUE(svc$available)) stats::setNames(svc$services$public_domain_key, svc$services$public_key) else NULL

  totals <- count_block(counted$uid, links, svc_domain)
  regions <- lapply(sort(unique(counted$admin1_id)), function(a1) {
    in1 <- counted[counted$admin1_id == a1, , drop = FALSE]
    r <- c(list(admin_id = a1), count_block(in1$uid, links, svc_domain))
    a2s <- sort(unique(stats::na.omit(in1$admin2_id)))
    r$districts <- lapply(a2s, function(a2) c(list(admin_id = a2), count_block(in1$uid[in1$admin2_id %in% a2], links, svc_domain)))
    r
  })

  list(
    schema_version = SCHEMA_VERSION,
    country_iso3 = meta$country_iso3,
    bundle_version = as.numeric(meta$bundle_version),
    built_at = meta$built_at,
    etl_source_date = meta$etl_source_date,
    services_available = isTRUE(svc$available),
    totals = totals,
    regions = regions
  )
}

write_json_file <- function(x, path, pretty = FALSE) {
  txt <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", pretty = pretty, digits = NA)
  writeLines(txt, path, useBytes = TRUE)
  invisible(path)
}

file_entry <- function(gz_path, raw_path, url) {
  list(url = url,
       bytes = file.info(gz_path)$size,
       sha256 = sha256_file(gz_path),
       uncompressed_bytes = file.info(raw_path)$size,
       uncompressed_sha256 = sha256_file(raw_path))
}

# Next bundle_version: YYYYMMDDNN, NN incrementing within a day.
next_bundle_version <- function(manifest_path, iso, today = Sys.Date()) {
  base <- as.numeric(format(today, "%Y%m%d")) * 100
  prev <- 0
  if (file.exists(manifest_path)) {
    m <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
    for (c in m$countries) if (identical(c$country_iso3, iso)) prev <- as.numeric(c$bundle_version)
  }
  if (prev >= base + 1) prev + 1 else base + 1
}

# Reads min/max zoom from a PMTiles v3 header (127 bytes; zooms at offsets 100 and 101).
pmtiles_header <- function(path) {
  raw <- readBin(path, what = "raw", n = 127)
  if (length(raw) < 127 || rawToChar(raw[1:7]) != "PMTiles")
    validation_error(sprintf("Not a PMTiles file: %s", path))
  if (as.integer(raw[8]) != 3)
    validation_error(sprintf("Unsupported PMTiles version %d in %s (need 3)", as.integer(raw[8]), path))
  list(min_zoom = as.integer(raw[101]), max_zoom = as.integer(raw[102]))
}

# The basemap entry already published for a country, so rebuilding the data
# bundle without --basemap keeps the map.
existing_basemap <- function(manifest_path, iso) {
  if (!file.exists(manifest_path)) return(NULL)
  m <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  for (c in m$countries) if (identical(c$country_iso3, iso)) return(c$files$basemap)
  NULL
}

update_manifest <- function(manifest_path, entry) {
  countries <- list()
  if (file.exists(manifest_path)) {
    m <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
    countries <- Filter(function(c) !identical(c$country_iso3, entry$country_iso3), m$countries)
  }
  countries <- c(countries, list(entry))
  countries <- countries[order(vapply(countries, function(c) c$country_iso3, ""))]
  manifest <- list(manifest_version = 1L,
                   generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                   countries = countries)
  tmp <- paste0(manifest_path, ".tmp")
  write_json_file(manifest, tmp, pretty = TRUE)
  file.rename(tmp, manifest_path)
  manifest
}
