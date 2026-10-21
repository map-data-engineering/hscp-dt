# Load and cross-check the human-edited config files in config/.

CONFIG_FILES <- c("countries.csv", "service_domain_labels.csv", "service_labels.csv",
                  "service_map.csv", "facility_type_labels.csv")

SW_STATUSES    <- c("NEEDS_TRANSLATION", "DRAFT", "DONE", "NOT_REQUIRED")
REVIEW_FLAGS   <- c("", "CLINICAL_REVIEW", "PRODUCT_REVIEW")
STATUS_KEYS    <- c("operating", "non_operational", "closed", "unknown")

read_config_csv <- function(path) {
  if (!file.exists(path)) validation_error(sprintf("Config file not found: %s", path))
  df <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                        na = character(0), trim_ws = TRUE, progress = FALSE,
                        locale = readr::locale(encoding = "UTF-8"))
  as.data.frame(df, stringsAsFactors = FALSE)
}

require_cols <- function(df, cols, what) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) validation_error(sprintf("%s is missing required columns:", what), missing)
}

dupes <- function(x) unique(x[duplicated(x)])

load_config <- function(config_dir) {
  p <- function(f) file.path(config_dir, f)
  cfg <- list(
    countries = read_config_csv(p("countries.csv")),
    domains   = read_config_csv(p("service_domain_labels.csv")),
    labels    = read_config_csv(p("service_labels.csv")),
    map       = read_config_csv(p("service_map.csv")),
    types     = read_config_csv(p("facility_type_labels.csv"))
  )
  require_cols(cfg$countries, c("country_iso3", "country_slug", "country_name", "min_lat", "max_lat",
                                "min_lon", "max_lon", "admin_level_keys", "admin3_column",
                                "admin4_column", "phone_column", "status_map", "source_attribution"),
               "countries.csv")
  require_cols(cfg$domains, c("domain_key", "label_en", "label_sw", "sw_status", "sort_order"),
               "service_domain_labels.csv")
  require_cols(cfg$labels, c("public_key", "public_domain_key", "label_en", "label_sw", "sw_status",
                             "show_public", "review_flag", "sort_order"), "service_labels.csv")
  require_cols(cfg$map, c("etl_service_domain", "etl_service_group", "etl_service_name", "public_key"),
               "service_map.csv")
  require_cols(cfg$types, c("country_iso3", "etl_facility_type", "type_key", "label_en", "label_sw",
                            "sw_status", "sort_order"), "facility_type_labels.csv")

  problems <- character(0)
  add <- function(cond, msg, vals) if (cond) problems <<- c(problems, paste0(msg, ": ", paste(vals, collapse = ", ")))

  d <- dupes(cfg$domains$domain_key);  add(length(d) > 0, "service_domain_labels.csv repeats domain_key", d)
  d <- dupes(cfg$labels$public_key);   add(length(d) > 0, "service_labels.csv repeats public_key", d)
  trip <- paste(cfg$map$etl_service_domain, cfg$map$etl_service_group, cfg$map$etl_service_name, sep = " > ")
  d <- dupes(trip);                    add(length(d) > 0, "service_map.csv repeats an ETL service", d)
  d <- dupes(paste(cfg$types$country_iso3, cfg$types$etl_facility_type, sep = " / "))
  add(length(d) > 0, "facility_type_labels.csv repeats a country/type pair", d)
  d <- dupes(cfg$countries$country_iso3); add(length(d) > 0, "countries.csv repeats country_iso3", d)

  m <- setdiff(cfg$map$public_key, cfg$labels$public_key)
  add(length(m) > 0, "service_map.csv uses public_key values missing from service_labels.csv", m)
  m <- setdiff(cfg$labels$public_domain_key, cfg$domains$domain_key)
  add(length(m) > 0, "service_labels.csv uses domains missing from service_domain_labels.csv", m)
  m <- setdiff(cfg$labels$show_public, c("yes", "no"))
  add(length(m) > 0, "service_labels.csv show_public must be yes/no, found", m)
  m <- setdiff(c(cfg$labels$sw_status, cfg$domains$sw_status, cfg$types$sw_status), SW_STATUSES)
  add(length(m) > 0, "Unknown sw_status values", m)
  m <- setdiff(cfg$labels$review_flag, REVIEW_FLAGS)
  add(length(m) > 0, "Unknown review_flag values", m)
  for (f in c("domains", "labels", "types")) {
    bad <- cfg[[f]]$sort_order[is.na(suppressWarnings(as.integer(cfg[[f]]$sort_order)))]
    add(length(bad) > 0, sprintf("Non-integer sort_order in %s", f), bad)
  }
  # One type_key must always carry the same labels.
  tl <- unique(cfg$types[, c("type_key", "label_en", "label_sw", "sw_status")])
  d <- dupes(tl$type_key); add(length(d) > 0, "facility_type_labels.csv gives one type_key different labels", d)

  if (length(problems)) validation_error("Config files are inconsistent:", problems)

  cfg$checksum <- digest::digest(paste(vapply(CONFIG_FILES, function(f) sha256_file(p(f)), ""), collapse = ""),
                                 algo = "sha256", serialize = FALSE)
  cfg
}

country_config <- function(cfg, iso3) {
  row <- cfg$countries[toupper(cfg$countries$country_iso3) == toupper(iso3), , drop = FALSE]
  if (nrow(row) != 1) validation_error(sprintf("Country '%s' not found in countries.csv", iso3))
  row <- as.list(row[1, ])
  for (k in c("min_lat", "max_lat", "min_lon", "max_lon")) row[[k]] <- as.numeric(row[[k]])
  row$admin_levels <- strsplit(row$admin_level_keys, ";", fixed = TRUE)[[1]]
  row$status_lookup <- parse_status_map(row$status_map)
  row
}

# "Operating=operating;Closed=closed" -> named character vector
parse_status_map <- function(s) {
  if (is_blank(s)) return(stats::setNames(character(0), character(0)))
  pairs <- strsplit(strsplit(s, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
  keys  <- vapply(pairs, function(p) trimws(p[1]), "")
  vals  <- vapply(pairs, function(p) trimws(p[2]), "")
  bad <- setdiff(vals, STATUS_KEYS)
  if (length(bad)) validation_error("countries.csv status_map uses unknown status keys:", bad)
  stats::setNames(vals, keys)
}

# Release builds must not ship untranslated or unreviewed labels.
check_release_labels <- function(used_services, used_domains, used_types) {
  problems <- c(
    sprintf("service '%s': sw_status %s", used_services$public_key[used_services$sw_status != "DONE"],
            used_services$sw_status[used_services$sw_status != "DONE"]),
    sprintf("service '%s': review_flag %s", used_services$public_key[used_services$review_flag != ""],
            used_services$review_flag[used_services$review_flag != ""]),
    sprintf("domain '%s': sw_status %s", used_domains$domain_key[used_domains$sw_status != "DONE"],
            used_domains$sw_status[used_domains$sw_status != "DONE"]),
    sprintf("facility type '%s': sw_status %s", used_types$type_key[used_types$sw_status != "DONE"],
            used_types$sw_status[used_types$sw_status != "DONE"])
  )
  if (length(problems))
    validation_error(sprintf("--release refused: %d labels are not ready for the public:", length(problems)),
                     problems)
}
