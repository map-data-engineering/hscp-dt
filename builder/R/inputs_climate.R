# Read the climate-sensitive facilities API (hfr-pipeline, Tanzania only): the
# El Niño subset of the Health Facility Registry, with each facility's
# climate-sensitive services in five priority tiers. See docs/data-discovery.md,
# "Climate API", and ADR 009. Phones never call the API: the builder downloads
# it once per build (fetch_climate_api) and keeps the download as a snapshot
# file, so a build can be repeated from it (--climate-json) and every row traced
# back to the API by facility code and service code.
#
# Which tiers the app shows is config/climate_tiers.csv; which codes are one
# service, and which are kept out of the menus, is config/climate_service_map.csv.
# Service names are the API's own (English and Swahili), never ours.

CLIMATE_API_URL <- "https://hfr-pipeline.vercel.app/api/v1/climate"
CLIMATE_PAGE_SIZE <- 200L
CLIMATE_SOURCE_NAME <- "Tanzania Health Facility Registry (HFR), climate-sensitive subset via hfr-pipeline"
CLIMATE_CONFIG_FILES <- c("climate_service_map.csv", "climate_tiers.csv")
CLIMATE_CONFIDENCE <- c(high = "high", medium = "medium", low = "low", unchecked = "unknown")

# GET one API URL as parsed JSON. 429 and 5xx answers are retried after the
# Retry-After header (or a growing pause); the API asks clients to stay well
# under about 120 requests a minute.
climate_get <- function(url, tries = 5, pause = Sys.sleep) {
  handle <- curl::new_handle(useragent = paste0("FastHuduma-bundle-builder/", BUILDER_VERSION), timeout = 90)
  for (i in seq_len(tries)) {
    r <- tryCatch(curl::curl_fetch_memory(url, handle = handle), error = function(e) NULL)
    status <- if (is.null(r)) 0L else r$status_code
    if (status == 200L) {
      txt <- rawToChar(r$content)
      Encoding(txt) <- "UTF-8"
      return(jsonlite::fromJSON(txt, simplifyVector = FALSE))
    }
    if (status != 0L && status != 429L && status < 500L)
      validation_error(sprintf("The climate API answered HTTP %d for %s", status, url))
    wait <- 5 * i
    if (!is.null(r)) {
      h <- curl::parse_headers_list(r$headers)
      ra <- suppressWarnings(as.numeric(h[["retry-after"]]))
      if (length(ra) == 1 && !is.na(ra)) wait <- ra
    }
    if (i < tries) pause(wait)
  }
  validation_error(sprintf("The climate API did not answer after %d tries: %s", tries, url))
}

# Downloads the whole API: the service list in both languages and every
# facility (English; facility and place names are not translated, and service
# names in Swahili come from the service list). Returns the snapshot as a list.
fetch_climate_api <- function(base_url = CLIMATE_API_URL, get = climate_get, pause = Sys.sleep, now = Sys.time(),
                              page_size = CLIMATE_PAGE_SIZE) {
  base_url <- sub("/+$", "", base_url)
  log_msg("Downloading %s", base_url)
  services_en <- get(paste0(base_url, "/services?lang=en"))
  services_sw <- get(paste0(base_url, "/services?lang=sw"))
  facilities <- list()
  attribution <- services_en$meta$attribution
  offset <- 0L
  repeat {
    page <- get(sprintf("%s/facilities?limit=%d&offset=%d&lang=en", base_url, page_size, offset))
    facilities <- c(facilities, page$data)
    total <- page$meta$total
    attribution <- page$meta$attribution %||% attribution
    offset <- offset + page_size
    if (length(page$data) == 0 || offset >= total) break
    pause(0.5)
  }
  if (length(facilities) != total)
    validation_error(sprintf("The climate API listed %d facilities but returned %d; the data may have changed while paging. Run the build again.",
                             total, length(facilities)))
  log_msg("Downloaded %d facilities and %d services", length(facilities), length(services_en$data))
  list(fetched_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
       base_url = base_url,
       attribution = attribution,
       services_en = services_en$data,
       services_sw = services_sw$data,
       facilities = facilities)
}

write_climate_snapshot <- function(snapshot, path) {
  txt <- jsonlite::toJSON(snapshot, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(txt, con, useBytes = TRUE)
  invisible(path)
}

read_climate_snapshot <- function(path) {
  if (!file.exists(path)) validation_error(sprintf("Climate snapshot not found: %s", path))
  s <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  missing <- setdiff(c("fetched_at", "attribution", "services_en", "services_sw", "facilities"), names(s))
  if (length(missing)) validation_error(sprintf("%s is not a climate API snapshot; missing:", basename(path)), missing)
  s
}

# Hash of what the bundle is built from (the downloaded data, the config and
# the builder version), so an unchanged weekly download can be skipped.
climate_source_checksum <- function(snapshot, config_checksum) {
  fac <- snapshot$facilities
  fac <- fac[order(vapply(fac, function(f) as.character(f$code %||% ""), ""))]
  body <- jsonlite::toJSON(list(snapshot$attribution, snapshot$services_en, snapshot$services_sw, fac),
                           auto_unbox = TRUE, null = "null", digits = NA)
  digest::digest(paste(body, config_checksum, BUILDER_VERSION), algo = "sha256", serialize = FALSE)
}

str_or_na <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)[1]
num_or_na <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x)[1]

same_text <- function(x) tolower(gsub("\\s+", " ", trimws(x)))

# One row per API service code: its names, tier, and the service it belongs to
# in the app (service_key), checked against the two config files.
climate_catalogue <- function(snapshot, config_dir) {
  as_df <- function(items) {
    data.frame(
      code = vapply(items, function(x) str_or_na(x$code), ""),
      name = vapply(items, function(x) str_or_na(x$name), ""),
      category = vapply(items, function(x) str_or_na(x$category), ""),
      tier = vapply(items, function(x) as.integer(num_or_na(x$tier)), 0L),
      tier_name = vapply(items, function(x) str_or_na(x$tier_name), ""),
      facilities = vapply(items, function(x) as.integer(num_or_na(x$facilities)), 0L),
      stringsAsFactors = FALSE)
  }
  en <- as_df(snapshot$services_en)
  sw <- as_df(snapshot$services_sw)
  if (!nrow(en)) validation_error("The climate snapshot lists no services.")
  d <- dupes(en$code)
  if (length(d)) validation_error("The climate API lists a service code more than once:", d)
  no_sw <- setdiff(en$code, sw$code)
  if (length(no_sw)) validation_error("Service codes without a Swahili entry in the climate API:", no_sw)
  i <- match(en$code, sw$code)
  cat <- data.frame(code = en$code, tier = en$tier, facilities = en$facilities,
                    name_en = en$name, name_sw = sw$name[i], category_en = en$category,
                    tier_name_en = en$tier_name, tier_name_sw = sw$tier_name[i], stringsAsFactors = FALSE)
  blank <- cat$code[is_blank(cat$name_en) | is_blank(cat$name_sw) | is.na(cat$tier)]
  if (length(blank)) validation_error("Climate services without a name or tier:", blank)

  map <- read_config_csv(file.path(config_dir, "climate_service_map.csv"))
  require_cols(map, c("code", "service_key", "in_menus"), "climate_service_map.csv")
  tiers <- read_config_csv(file.path(config_dir, "climate_tiers.csv"))
  require_cols(tiers, c("tier", "include"), "climate_tiers.csv")

  problems <- character(0)
  add <- function(cond, msg, vals) if (cond) problems <<- c(problems, paste0(msg, ": ", paste(vals, collapse = ", ")))
  d <- dupes(map$code); add(length(d) > 0, "climate_service_map.csv repeats a code", d)
  bad <- map$service_key[!grepl("^[a-z][a-z0-9_]*$", map$service_key)]
  add(length(bad) > 0, "climate_service_map.csv service_key must be lower-case letters, digits and _", unique(bad))
  bad <- setdiff(map$in_menus, c("yes", "no")); add(length(bad) > 0, "climate_service_map.csv in_menus must be yes/no, found", bad)
  bad <- setdiff(tiers$include, c("yes", "no")); add(length(bad) > 0, "climate_tiers.csv include must be yes/no, found", bad)
  d <- dupes(tiers$tier); add(length(d) > 0, "climate_tiers.csv repeats a tier", d)
  if (length(problems)) validation_error("Climate config files are inconsistent:", problems)

  unmapped <- setdiff(cat$code, map$code)
  if (length(unmapped))
    validation_error("Climate API service codes missing from climate_service_map.csv. Add a row for each (a service_key, and in_menus yes or no):",
                     sprintf("%s '%s' (tier %d)", unmapped, cat$name_en[match(unmapped, cat$code)], cat$tier[match(unmapped, cat$code)]))
  untiered <- setdiff(as.character(cat$tier), tiers$tier)
  if (length(untiered)) validation_error("Climate API tiers missing from climate_tiers.csv:", untiered)

  m <- match(cat$code, map$code)
  cat$service_key <- map$service_key[m]
  cat$in_menus <- map$in_menus[m] == "yes"
  cat$included <- tiers$include[match(as.character(cat$tier), tiers$tier)] == "yes"

  warnings <- character(0)
  stale <- setdiff(map$code, cat$code)
  if (length(stale)) warnings <- c(warnings, sprintf("climate_service_map.csv codes no longer in the API: %s", paste(stale, collapse = ", ")))
  if ("reference_name" %in% names(map)) {
    renamed <- cat$code[!is_blank(map$reference_name[m]) & map$reference_name[m] != cat$name_en]
    if (length(renamed))
      warnings <- c(warnings, sprintf("The API renamed %s since climate_service_map.csv was written; the app shows the new names. Update reference_name.",
                                      paste(renamed, collapse = ", ")))
  }

  # One service per key: one tier, one menu setting, and codes the public would
  # read as the same name must share a key (the list shows each name once).
  inc <- cat[cat$included, , drop = FALSE]
  for (k in unique(inc$service_key)) {
    g <- cat[cat$service_key == k, , drop = FALSE]
    if (length(unique(g$tier)) > 1)
      problems <- c(problems, sprintf("service_key '%s' joins codes from different tiers: %s", k, paste(g$code, collapse = ", ")))
    if (length(unique(g$in_menus)) > 1)
      problems <- c(problems, sprintf("service_key '%s' has both in_menus yes and no: %s", k, paste(g$code, collapse = ", ")))
  }
  for (lang in c("name_en", "name_sw")) {
    keys <- tapply(inc$service_key, same_text(inc[[lang]]), function(k) length(unique(k)))
    for (n in names(keys)[keys > 1])
      problems <- c(problems, sprintf("codes named '%s' (%s) have different service_keys: %s", n, lang,
                                      paste(inc$code[same_text(inc[[lang]]) == n], collapse = ", ")))
  }
  if (length(problems)) validation_error("climate_service_map.csv does not group the API's codes consistently:", problems)

  attr(cat, "warnings") <- warnings
  cat
}

# Returns list(facilities, links, catalogue, excluded, counts, warnings) for
# Tanzania. Facilities come in the columns transform_facilities() expects,
# plus location_approximate.
read_climate <- function(snapshot, country, config_dir) {
  if (toupper(country$country_iso3) != "TZA") validation_error("The climate API covers Tanzania only (--country TZA).")
  cat <- climate_catalogue(snapshot, config_dir)
  f <- snapshot$facilities
  if (!length(f)) validation_error("The climate snapshot lists no facilities.")
  chr <- function(get) vapply(f, function(x) str_or_na(get(x)), "")
  code <- trimws(chr(function(x) x$code))
  d <- dupes(code)
  if (length(d)) validation_error("Facility codes are not unique in the climate API:", d)
  if (any(is_blank(code))) validation_error("The climate API returned facilities without a code.")

  confidence <- tolower(trimws(chr(function(x) x$location$confidence)))
  unknown_conf <- setdiff(stats::na.omit(confidence), names(CLIMATE_CONFIDENCE))
  if (length(unknown_conf)) validation_error("Unknown location confidence values in the climate API:", unknown_conf)
  lat <- vapply(f, function(x) num_or_na(x$location$lat), 0)
  lng <- vapply(f, function(x) num_or_na(x$location$lng), 0)
  approximate <- vapply(f, function(x) isTRUE(x$location$approximate), logical(1))
  ownership <- chr(function(x) x$ownership)

  fac <- data.frame(
    uid = paste0(toupper(country$country_iso3), "-", code),
    facility_code = code,
    facility_name = chr(function(x) x$name),
    facility_type = chr(function(x) x$type),
    # The API gives Public or Private only (no faith-based or NGO split).
    ownership = ifelse(ownership %in% c("Public", "Private"), ownership, "Other"),
    ownership_detail = NA_character_,
    # The API returns operating facilities only.
    status = "Operating",
    admin1 = sub("\\s+Region$", "", trimws(chr(function(x) x$region))),
    admin2 = sub("\\s+District$", "", blank_to_na(chr(function(x) x$district))),
    admin3_name = blank_to_na(chr(function(x) x$ward), also = "-"),
    admin4_name = blank_to_na(chr(function(x) x$village), also = c("-", "null", "NULL")),
    latitude = as.character(lat),
    longitude = as.character(lng),
    coordinate_valid = ifelse(is.na(lat) | is.na(lng), "FALSE", "TRUE"),
    data_quality_flag = unname(CLIMATE_CONFIDENCE[confidence]),
    data_source = CLIMATE_SOURCE_NAME,
    phone = blank_to_na(chr(function(x) x$phone)),
    location_approximate = approximate,
    stringsAsFactors = FALSE
  )

  # One row per facility and service code.
  codes <- lapply(f, function(x) vapply(x$services, function(s) str_or_na(s$code), ""))
  links <- data.frame(facility_uid = rep(fac$uid, lengths(codes)), code = toupper(unlist(codes, use.names = FALSE)),
                      stringsAsFactors = FALSE)
  unknown <- setdiff(links$code, cat$code)
  if (length(unknown)) validation_error("Facilities list service codes missing from the API's service list:", unknown)

  excluded <- list()
  no_region <- is_blank(fac$admin1)
  excluded$facilities_without_region <- sum(no_region)
  included <- links$code %in% cat$code[cat$included]
  excluded$service_rows_excluded_tier <- sum(!included)
  links <- links[included, , drop = FALSE]
  keep <- !no_region & fac$uid %in% links$facility_uid
  excluded$facilities_without_included_tier <- sum(!no_region & !keep)
  fac <- fac[keep, , drop = FALSE]
  links <- links[links$facility_uid %in% fac$uid, , drop = FALSE]
  attr(fac, "path") <- "climate API snapshot"

  list(
    facilities = fac,
    links = links,
    catalogue = cat,
    excluded = excluded,
    counts = list(
      climate_api_facilities = length(f),
      climate_tiers_included = sort(unique(cat$tier[cat$included])),
      climate_location_confidence = as.list(table(fac$data_quality_flag)),
      climate_locations_approximate = sum(fac$location_approximate)
    ),
    warnings = attr(cat, "warnings")
  )
}

# The services part of the bundle, in the shape transform_services() returns.
# One service per service_key, named after its code with the most facilities
# (the API's own English and Swahili names); one domain per included tier,
# named after the tier.
transform_climate_services <- function(climate, fac, report) {
  cat <- climate$catalogue[climate$catalogue$included, , drop = FALSE]
  l <- climate$links[climate$links$facility_uid %in% fac$uid, , drop = FALSE]
  l$public_key <- cat$service_key[match(l$code, cat$code)]
  report$counts$services_available <- TRUE
  report$counts$service_rows_in <- nrow(l)
  pair <- paste(l$facility_uid, l$public_key, sep = "|")
  dup <- duplicated(pair)
  report$excluded$service_rows_duplicate_after_mapping <- sum(dup)
  links <- data.frame(facility_uid = l$facility_uid[!dup], public_key = l$public_key[!dup], stringsAsFactors = FALSE)

  # The code that names each service: the one most facilities report, then the lowest code.
  ord <- cat[order(cat$service_key, -cat$facilities, cat$code), , drop = FALSE]
  main <- ord[!duplicated(ord$service_key), , drop = FALSE]
  fc <- table(links$public_key)
  main <- main[main$service_key %in% names(fc), , drop = FALSE]
  malaria <- tapply(cat$category_en == "Malaria Diagnosis and Treatment" | grepl("malaria|mrdt", cat$name_en, ignore.case = TRUE),
                    cat$service_key, any)
  services <- data.frame(
    public_key = main$service_key,
    public_domain_key = paste0("tier", main$tier),
    label_en = main$name_en,
    label_sw = main$name_sw,
    # The API's Swahili is the pipeline team's first draft (API guide, section 9).
    sw_status = "DRAFT",
    sort_order = as.integer(sub("^CS", "", main$code)),
    facility_count = as.integer(fc[main$service_key]),
    is_malaria_related = as.integer(malaria[main$service_key]),
    in_menus = as.integer(main$in_menus),
    stringsAsFactors = FALSE)
  services <- services[order(services$public_domain_key, services$sort_order), , drop = FALSE]

  t <- unique(cat[cat$service_key %in% services$public_key, c("tier", "tier_name_en", "tier_name_sw")])
  if (any(duplicated(t$tier))) validation_error("The climate API gives one tier more than one name:", t$tier[duplicated(t$tier)])
  t <- t[order(t$tier), , drop = FALSE]
  domains <- data.frame(domain_key = paste0("tier", t$tier), label_en = t$tier_name_en, label_sw = t$tier_name_sw,
                        sw_status = "DRAFT", sort_order = t$tier, stringsAsFactors = FALSE)

  # Traceability: each code that reached the bundle, under its API category.
  used <- cat[cat$code %in% l$code, , drop = FALSE]
  etl <- data.frame(service_domain = used$code, service_group = used$category_en, service_name = used$name_en,
                    public_key = used$service_key, stringsAsFactors = FALSE)

  report$counts$public_services <- nrow(services)
  report$counts$public_services_in_menus <- sum(services$in_menus)
  report$counts$facility_service_links <- nrow(links)
  report$counts$facilities_with_service_data <- length(unique(links$facility_uid))
  list(available = TRUE, links = links, services = services, domains = domains, etl = etl, report = report)
}
