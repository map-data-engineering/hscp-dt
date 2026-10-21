# Read the HFR export: the Tanzania Health Facility Registry as scraped and
# geo-validated by hfr-pipeline (one row per facility, its services packed
# into one column as "Category: Service|Category: Service|..."). See
# docs/data-discovery.md, "HFR export". The result has the same columns as
# read_facilities() and read_services() give for the ETL CSVs, so the rest of
# the build is shared.

HFR_REQUIRED <- c("facility_code", "name", "facility_type", "operating_status",
                  "ownership_category", "ownership_authority", "region_name", "district_name",
                  "ward_name", "village_name", "phone_e164", "final_lat", "final_lng",
                  "confidence", "sanity_flags", "geocode_source", "services")

# sanity_flags that make the validated point unusable, unless a person reviewed
# the location (geocode_source "review"). Other flags (village_ward_differs,
# shared_point, ...) are kept: the point is still the best location the
# pipeline has. "missing" describes the published HFR point: with no review,
# final_lat/final_lng are then empty anyway.
HFR_BAD_POINT_FLAGS <- c("outside_tanzania", "portal_default")

HFR_SOURCE_NAME <- "Tanzania Health Facility Registry (HFR), via hfr-pipeline"
# – is an en dash; written as an escape so the file reads the same in any encoding.
HFR_ATTRIBUTION <- "Ministry of Health Tanzania – Health Facility Registry (HFR), scraped and geo-validated by hfr-pipeline"

# The snapshot date in the export's file name (hfr-facilities-YYYY-MM-DD.csv), or NA.
hfr_snapshot_date <- function(path) {
  m <- regmatches(basename(path), regexpr("\\d{4}-\\d{2}-\\d{2}", basename(path)))
  if (length(m)) m else NA_character_
}

blank_to_na <- function(x, also = character(0)) {
  x <- trimws(x)
  x[is_blank(x) | x %in% also] <- NA_character_
  x
}

# Returns list(facilities, services, excluded) for one country (Tanzania only).
read_hfr <- function(path, country) {
  if (!file.exists(path)) validation_error(sprintf("HFR export not found: %s", path))
  if (toupper(country$country_iso3) != "TZA") validation_error("The HFR export covers Tanzania only (--country TZA).")
  r <- read_etl_csv(path, HFR_REQUIRED)
  missing <- setdiff(HFR_REQUIRED, r$header)
  if (length(missing)) validation_error(sprintf("%s is missing required columns:", basename(path)), missing)
  h <- r$data
  iso <- toupper(country$country_iso3)

  d <- dupes(h$facility_code)
  if (length(d)) validation_error("facility_code values are not unique in the HFR export:", d)

  # The builder never guesses an admin area: a facility without a region is left out and counted.
  no_region <- is_blank(h$region_name)
  h <- h[!no_region, , drop = FALSE]

  flags <- strsplit(ifelse(is.na(h$sanity_flags), "", h$sanity_flags), "|", fixed = TRUE)
  reviewed <- !is.na(h$geocode_source) & trimws(h$geocode_source) == "review"
  bad_point <- vapply(flags, function(f) any(f %in% HFR_BAD_POINT_FLAGS), logical(1)) & !reviewed
  has_point <- !is_blank(h$final_lat) & !is_blank(h$final_lng)

  authority <- blank_to_na(h$ownership_authority, also = "Not Set")
  ownership <- ifelse(authority %in% c("Faith Based Organization (FBO)", "NGOs"), "Faith-Based / NGO",
                      ifelse(h$ownership_category %in% c("Public", "Private"), h$ownership_category, "Other"))

  fac <- data.frame(
    uid = paste0(iso, "-", trimws(h$facility_code)),
    facility_code = trimws(h$facility_code),
    facility_name = h$name,
    facility_type = h$facility_type,
    ownership = ownership,
    ownership_detail = authority,
    status = h$operating_status,
    admin1 = sub("\\s+Region$", "", trimws(h$region_name)),
    admin2 = sub("\\s+District$", "", blank_to_na(h$district_name)),
    admin3_name = blank_to_na(h$ward_name, also = "-"),
    admin4_name = blank_to_na(h$village_name, also = c("-", "null", "NULL")),
    latitude = h$final_lat,
    longitude = h$final_lng,
    coordinate_valid = ifelse(has_point & !bad_point, "TRUE", "FALSE"),
    data_quality_flag = tolower(trimws(h$confidence)),
    data_source = HFR_SOURCE_NAME,
    phone = blank_to_na(h$phone_e164),
    stringsAsFactors = FALSE
  )
  attr(fac, "path") <- path

  # One row per facility and service.
  items <- strsplit(ifelse(is.na(h$services), "", h$services), "|", fixed = TRUE)
  n <- lengths(items)
  item <- trimws(unlist(items, use.names = FALSE))
  sep <- regexpr(": ", item, fixed = TRUE)
  no_sep <- sep < 0
  if (any(no_sep)) validation_error("HFR service entries without a 'Category: Service' separator:", unique(item[no_sep]))
  category <- trimws(substr(item, 1, sep - 1))
  service <- trimws(substr(item, sep + 2, nchar(item)))
  svc <- data.frame(
    facility_uid = rep(fac$uid, n),
    service_domain = "HFR",
    service_group = category,
    service_name = service,
    is_malaria_related = ifelse(category == "Malaria Diagnosis and Treatment" | grepl("malaria|mrdt", service, ignore.case = TRUE), "TRUE", "FALSE"),
    include_in_analysis = "TRUE",
    needs_review = "FALSE",
    stringsAsFactors = FALSE
  )
  attr(svc, "path") <- path

  list(
    facilities = fac,
    services = svc,
    excluded = list(facilities_without_region = sum(no_region)),
    counts = list(
      hfr_points_unusable = sum(!(has_point & !bad_point)),
      hfr_location_confidence = as.list(table(fac$data_quality_flag))
    )
  )
}

# hfr_service_map.csv in the shape of service_map.csv (domain "HFR"), so
# transform_services() maps HFR services with the same code.
load_hfr_service_map <- function(config_dir, cfg) {
  path <- file.path(config_dir, "hfr_service_map.csv")
  m <- read_config_csv(path)
  require_cols(m, c("hfr_category", "hfr_service", "public_key"), "hfr_service_map.csv")
  unknown <- setdiff(m$public_key, cfg$labels$public_key)
  if (length(unknown)) validation_error("hfr_service_map.csv uses public keys missing from service_labels.csv:", unknown)
  d <- dupes(paste(m$hfr_category, m$hfr_service, sep = " > "))
  if (length(d)) validation_error("hfr_service_map.csv has duplicate rows:", d)
  data.frame(etl_service_domain = "HFR", etl_service_group = m$hfr_category, etl_service_name = m$hfr_service,
             public_key = m$public_key, countries = "TZA", stringsAsFactors = FALSE)
}
