# Turn ETL rows into bundle rows. Every exclusion is counted in `report`.

OWNERSHIP_MAP <- c("Public" = "public", "Private" = "private",
                   "Faith-Based / NGO" = "faith_ngo", "Other" = "other")
QUALITY_FLAGS <- c("high", "medium", "low", "unknown")

value_counts <- function(x) {
  t <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
  sprintf("'%s' (%d rows)", names(t), as.integer(t))
}

new_report <- function() {
  list(excluded = list(), warnings = character(0), counts = list())
}

add_warning <- function(report, msg) {
  log_msg("WARNING: %s", msg)
  report$warnings <- c(report$warnings, msg)
  report
}

transform_facilities <- function(fac, country, cfg, report) {
  iso <- toupper(country$country_iso3)
  report$counts$facility_rows_in <- nrow(fac)

  no_uid <- is_blank(fac$uid)
  report$excluded$facilities_without_uid <- sum(no_uid)
  fac <- fac[!no_uid, , drop = FALSE]

  d <- dupes(fac$uid)
  if (length(d)) validation_error("Facility uid values are not unique:", d)

  for (col in c("facility_name", "admin1", "admin2", "admin3_name", "admin4_name", "ownership_detail", "phone"))
    fac[[col]] <- clean_text(fac[[col]])
  fac$admin2[is_blank(fac$admin2)] <- NA_character_

  bad <- fac$uid[is_blank(fac$admin1)]
  if (length(bad)) validation_error("Facilities with an empty admin1:", bad)
  bad <- fac$uid[is_blank(fac$facility_name)]
  if (length(bad)) validation_error("Facilities with an empty facility_name:", bad)

  # Facility type -> public type key
  types <- cfg$types[toupper(cfg$types$country_iso3) == iso, , drop = FALSE]
  raw_type <- clean_text(fac$facility_type)
  unmapped <- raw_type[!raw_type %in% types$etl_facility_type]
  if (length(unmapped))
    validation_error(sprintf("Facility types missing from facility_type_labels.csv for %s:", iso), value_counts(unmapped))
  fac$type_key <- types$type_key[match(raw_type, types$etl_facility_type)]

  unmapped <- fac$ownership[!fac$ownership %in% names(OWNERSHIP_MAP)]
  if (length(unmapped)) validation_error("Unknown ownership values:", value_counts(unmapped))
  fac$ownership_key <- unname(OWNERSHIP_MAP[fac$ownership])

  st <- trimws(fac$status)
  unmapped <- st[!is_blank(st) & !st %in% names(country$status_lookup)]
  if (length(unmapped))
    validation_error(sprintf("Status values missing from status_map in countries.csv for %s:", iso), value_counts(unmapped))
  fac$status_key <- ifelse(is_blank(st), "unknown", unname(country$status_lookup[st]))

  fac$lat <- suppressWarnings(as.numeric(fac$latitude))
  fac$lon <- suppressWarnings(as.numeric(fac$longitude))
  in_box <- !is.na(fac$lat) & !is.na(fac$lon) &
    fac$lat >= country$min_lat & fac$lat <= country$max_lat &
    fac$lon >= country$min_lon & fac$lon <= country$max_lon
  flagged <- toupper(trimws(fac$coordinate_valid)) == "TRUE" & !is.na(fac$coordinate_valid)
  wrong_flag <- flagged & !in_box
  if (any(wrong_flag))
    report <- add_warning(report, sprintf(
      "%d facilities are flagged coordinate_valid = TRUE but are missing coordinates or lie outside the %s bounding box; set to invalid. First: %s",
      sum(wrong_flag), iso, paste(utils::head(fac$uid[wrong_flag], 10), collapse = ", ")))
  fac$coords_valid <- as.integer(flagged & in_box)

  q <- tolower(trimws(fac$data_quality_flag))
  q[is_blank(q)] <- "unknown"
  unmapped <- q[!q %in% QUALITY_FLAGS]
  if (length(unmapped)) validation_error("Unknown data_quality_flag values:", value_counts(unmapped))
  fac$quality_flag <- q

  fac$data_source <- clean_text(fac$data_source)
  fac$data_source[is_blank(fac$data_source)] <- "Unknown source"

  # Admin units: stable path keys
  fac$admin1_id <- paste0(iso, ".", slugify(fac$admin1))
  fac$admin2_id <- ifelse(is.na(fac$admin2), NA_character_, paste0(fac$admin1_id, ".", slugify(fac$admin2)))
  check_admin_slugs(fac$admin1, fac$admin1_id, "admin1")
  check_admin_slugs(fac$admin2[!is.na(fac$admin2)], fac$admin2_id[!is.na(fac$admin2)], "admin2")

  report$counts$facilities <- nrow(fac)
  report$counts$facilities_coords_invalid <- sum(fac$coords_valid == 0L)
  list(facilities = fac, report = report)
}

check_admin_slugs <- function(names, ids, level) {
  if (!length(names)) return(invisible())
  empty <- unique(names[slugify(names) == ""])
  if (length(empty)) validation_error(sprintf("%s names that produce an empty key:", level), empty)
  pairs <- unique(data.frame(name = names, id = ids, stringsAsFactors = FALSE))
  clash <- pairs$id[duplicated(pairs$id)]
  if (length(clash)) {
    detail <- vapply(unique(clash), function(i) paste0(i, " <- ", paste(sprintf("'%s'", pairs$name[pairs$id == i]), collapse = " / ")), "")
    validation_error(sprintf("Different %s names produce the same admin key:", level), detail)
  }
}

build_admin_units <- function(fac) {
  l1 <- stats::aggregate(list(facility_count = fac$uid), by = list(admin_id = fac$admin1_id, name = fac$admin1), FUN = length)
  l1$level <- 1L; l1$parent_id <- NA_character_
  has2 <- !is.na(fac$admin2_id)
  if (any(has2)) {
    l2 <- stats::aggregate(list(facility_count = fac$uid[has2]),
                           by = list(admin_id = fac$admin2_id[has2], name = fac$admin2[has2], parent_id = fac$admin1_id[has2]),
                           FUN = length)
    l2$level <- 2L
  } else {
    l2 <- l1[0, ]
  }
  out <- rbind(l1[, c("admin_id", "level", "name", "parent_id", "facility_count")],
               l2[, c("admin_id", "level", "name", "parent_id", "facility_count")])
  out[order(out$level, out$admin_id), ]
}

truthy <- function(x) !is.na(x) & toupper(trimws(x)) == "TRUE"

transform_services <- function(svc, fac, country, cfg, report) {
  iso <- toupper(country$country_iso3)
  if (is.null(svc)) {
    report$counts$services_available <- FALSE
    return(list(available = FALSE, links = NULL, services = NULL, domains = NULL, etl = NULL, report = report))
  }
  report$counts$services_available <- TRUE
  report$counts$service_rows_in <- nrow(svc)
  svc$triple <- paste(svc$service_domain, svc$service_group, svc$service_name, sep = " > ")
  cfg_map <- cfg$map
  cfg_map$triple <- paste(cfg_map$etl_service_domain, cfg_map$etl_service_group, cfg_map$etl_service_name, sep = " > ")
  all_triples <- unique(svc$triple)

  orphan <- is_blank(svc$facility_uid) | !svc$facility_uid %in% fac$uid
  report$excluded$service_rows_orphan <- sum(orphan)
  svc <- svc[!orphan, , drop = FALSE]

  no_hier <- is_blank(svc$service_domain) | is_blank(svc$service_group)
  report$excluded$service_rows_unmapped_in_etl <- sum(no_hier)
  svc <- svc[!no_hier, , drop = FALSE]

  blank <- is_blank(svc$service_name)
  report$excluded$service_rows_blank_name <- sum(blank)
  svc <- svc[!blank, , drop = FALSE]

  unmapped <- svc$triple[!svc$triple %in% cfg_map$triple]
  if (length(unmapped))
    validation_error(sprintf("ETL services missing from service_map.csv (%d distinct). Add a row for each:", length(unique(unmapped))),
                     value_counts(unmapped))
  svc$public_key <- cfg_map$public_key[match(svc$triple, cfg_map$triple)]

  lab <- cfg$labels
  shown <- lab$show_public[match(svc$public_key, lab$public_key)] == "yes"
  report$excluded$service_rows_hidden <- sum(!shown)
  hidden_keys <- sort(table(svc$public_key[!shown]), decreasing = TRUE)
  report$counts$hidden_rows_by_service <- as.list(stats::setNames(as.integer(hidden_keys), names(hidden_keys)))
  svc <- svc[shown, , drop = FALSE]

  pair <- paste(svc$facility_uid, svc$public_key, sep = "|")
  dup <- duplicated(pair)
  report$excluded$service_rows_duplicate_after_mapping <- sum(dup)
  links <- data.frame(facility_uid = svc$facility_uid[!dup], public_key = svc$public_key[!dup], stringsAsFactors = FALSE)

  fc <- table(links$public_key)
  malaria <- tapply(truthy(svc$is_malaria_related), svc$public_key, any)
  services <- lab[lab$public_key %in% names(fc), , drop = FALSE]
  services$facility_count <- as.integer(fc[services$public_key])
  services$is_malaria_related <- as.integer(malaria[services$public_key])
  services <- services[order(match(services$public_domain_key, cfg$domains$domain_key), as.integer(services$sort_order), services$public_key), ]

  domains <- cfg$domains[cfg$domains$domain_key %in% services$public_domain_key, , drop = FALSE]
  domains <- domains[order(as.integer(domains$sort_order)), ]

  etl <- unique(svc[, c("service_domain", "service_group", "service_name", "public_key")])

  # Warnings: shown services this country's data maps to but that ended up empty,
  # and map rows tagged for this country that no longer occur in the data.
  mapped_shown <- unique(cfg_map$public_key[cfg_map$triple %in% all_triples])
  mapped_shown <- mapped_shown[lab$show_public[match(mapped_shown, lab$public_key)] == "yes"]
  empty <- setdiff(mapped_shown, services$public_key)
  if (length(empty))
    report <- add_warning(report, sprintf("Shown services with no facilities left after filtering: %s", paste(empty, collapse = ", ")))
  if ("countries" %in% names(cfg_map)) {
    tagged <- cfg_map$triple[grepl(paste0("(^|[;, ])", iso, "($|[;, ])"), cfg_map$countries)]
    stale <- setdiff(tagged, all_triples)
    if (length(stale))
      report <- add_warning(report, sprintf("%d service_map.csv rows tagged %s do not occur in the data: %s",
                                            length(stale), iso, paste(utils::head(stale, 10), collapse = "; ")))
  }

  report$counts$public_services <- nrow(services)
  report$counts$facility_service_links <- nrow(links)
  report$counts$facilities_with_service_data <- length(unique(links$facility_uid))
  list(available = TRUE, links = links, services = services, domains = domains, etl = etl, report = report)
}

finalise_facilities <- function(fac, svc_result) {
  fac$has_service_data <- if (isTRUE(svc_result$available)) as.integer(fac$uid %in% svc_result$links$facility_uid) else 0L
  fac$nearest_eligible <- as.integer(fac$coords_valid == 1L & fac$status_key %in% c("operating", "unknown"))
  fac
}
