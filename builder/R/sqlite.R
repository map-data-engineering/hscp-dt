# Write the SQLite bundle (schema in docs/bundle-schema.md, section 2).

SCHEMA_SQL <- c(
  "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT NOT NULL) WITHOUT ROWID",
  "CREATE TABLE sources (source_id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE)",
  "CREATE TABLE admin_units (
     admin_id TEXT PRIMARY KEY,
     level INTEGER NOT NULL CHECK (level IN (1, 2)),
     name TEXT NOT NULL,
     parent_id TEXT REFERENCES admin_units(admin_id),
     facility_count INTEGER NOT NULL
   ) WITHOUT ROWID",
  "CREATE TABLE facility_types (
     type_key TEXT PRIMARY KEY, label_en TEXT NOT NULL, label_sw TEXT, sort_order INTEGER NOT NULL
   ) WITHOUT ROWID",
  "CREATE TABLE facilities (
     fid INTEGER PRIMARY KEY,
     facility_id TEXT NOT NULL UNIQUE,
     facility_code TEXT,
     name TEXT NOT NULL,
     type_key TEXT NOT NULL REFERENCES facility_types(type_key),
     ownership_key TEXT NOT NULL CHECK (ownership_key IN ('public','private','faith_ngo','other')),
     ownership_detail TEXT,
     status_key TEXT NOT NULL CHECK (status_key IN ('operating','non_operational','closed','unknown')),
     admin1_id TEXT NOT NULL REFERENCES admin_units(admin_id),
     admin2_id TEXT REFERENCES admin_units(admin_id),
     admin1_name TEXT NOT NULL,
     admin2_name TEXT,
     admin3_name TEXT,
     admin4_name TEXT,
     lat REAL,
     lon REAL,
     coords_valid INTEGER NOT NULL CHECK (coords_valid IN (0,1)),
     quality_flag TEXT NOT NULL CHECK (quality_flag IN ('high','medium','low','unknown')),
     phone TEXT,
     source_id INTEGER NOT NULL REFERENCES sources(source_id),
     has_service_data INTEGER NOT NULL CHECK (has_service_data IN (0,1)),
     nearest_eligible INTEGER NOT NULL CHECK (nearest_eligible IN (0,1)),
     location_approximate INTEGER NOT NULL DEFAULT 0 CHECK (location_approximate IN (0,1))
   )",
  "CREATE TABLE service_domains (
     domain_id INTEGER PRIMARY KEY, domain_key TEXT NOT NULL UNIQUE,
     label_en TEXT NOT NULL, label_sw TEXT, sort_order INTEGER NOT NULL
   )",
  "CREATE TABLE services (
     service_id INTEGER PRIMARY KEY,
     service_key TEXT NOT NULL UNIQUE,
     domain_id INTEGER NOT NULL REFERENCES service_domains(domain_id),
     label_en TEXT NOT NULL,
     label_sw TEXT,
     sort_order INTEGER NOT NULL,
     is_malaria_related INTEGER NOT NULL CHECK (is_malaria_related IN (0,1)),
     facility_count INTEGER NOT NULL,
     in_menus INTEGER NOT NULL DEFAULT 1 CHECK (in_menus IN (0,1))
   )",
  "CREATE TABLE service_groups (
     group_id INTEGER PRIMARY KEY, etl_domain TEXT NOT NULL, etl_group TEXT NOT NULL,
     UNIQUE (etl_domain, etl_group)
   )",
  "CREATE TABLE etl_services (
     etl_service_id INTEGER PRIMARY KEY,
     group_id INTEGER NOT NULL REFERENCES service_groups(group_id),
     etl_service_name TEXT NOT NULL,
     service_id INTEGER NOT NULL REFERENCES services(service_id),
     UNIQUE (group_id, etl_service_name)
   )",
  "CREATE TABLE facility_services (
     service_id INTEGER NOT NULL REFERENCES services(service_id),
     fid INTEGER NOT NULL REFERENCES facilities(fid),
     PRIMARY KEY (service_id, fid)
   ) WITHOUT ROWID",
  # Contentless: rowid = facilities.fid; the app joins back to facilities for display.
  "CREATE VIRTUAL TABLE facilities_fts USING fts5(
     name, admin1_name, admin2_name, admin3_name, admin4_name, type_label,
     content = '',
     tokenize = 'unicode61 remove_diacritics 2', prefix = '2 3'
   )"
)

FTS_FILL_SQL <- "
  INSERT INTO facilities_fts (rowid, name, admin1_name, admin2_name, admin3_name, admin4_name, type_label)
  SELECT f.fid, f.name, f.admin1_name, f.admin2_name, f.admin3_name, f.admin4_name,
         t.label_en || COALESCE(' ' || t.label_sw, '')
  FROM facilities f JOIN facility_types t USING (type_key)"

INDEX_SQL <- c(
  "CREATE INDEX idx_fac_geo ON facilities(lat, lon) WHERE nearest_eligible = 1",
  "CREATE INDEX idx_fs_fid ON facility_services(fid, service_id)",
  "CREATE INDEX idx_fac_admin1 ON facilities(admin1_id)",
  "CREATE INDEX idx_fac_admin2 ON facilities(admin2_id)",
  "CREATE INDEX idx_fac_type ON facilities(type_key)"
)

# A Swahili label is only stored once someone has entered one (DONE or DRAFT).
sw_or_null <- function(label, status) ifelse(status %in% c("DONE", "DRAFT") & !is_blank(label), label, NA_character_)
na_if_blank <- function(x) ifelse(is_blank(x), NA_character_, x)

write_bundle_sqlite <- function(path, fac, admin_units, types, svc, meta) {
  if (file.exists(path)) file.remove(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ex <- function(sql) DBI::dbExecute(con, sql)

  ex("PRAGMA page_size = 4096")
  ex("PRAGMA journal_mode = OFF")
  ex("PRAGMA synchronous = OFF")
  ex("PRAGMA foreign_keys = ON")
  for (s in SCHEMA_SQL) ex(s)

  ins <- function(table, df) DBI::dbAppendTable(con, table, df)

  DBI::dbBegin(con)
  sources <- data.frame(source_id = seq_along(unique(fac$data_source)), name = unique(fac$data_source), stringsAsFactors = FALSE)
  ins("sources", sources)
  ins("admin_units", admin_units)
  ins("facility_types", data.frame(
    type_key = types$type_key, label_en = types$label_en,
    label_sw = sw_or_null(types$label_sw, types$sw_status),
    sort_order = as.integer(types$sort_order), stringsAsFactors = FALSE))

  fac <- fac[order(fac$uid), ]
  fac$fid <- seq_len(nrow(fac))
  ins("facilities", data.frame(
    fid = fac$fid, facility_id = fac$uid, facility_code = na_if_blank(fac$facility_code),
    name = fac$facility_name, type_key = fac$type_key, ownership_key = fac$ownership_key,
    ownership_detail = na_if_blank(fac$ownership_detail), status_key = fac$status_key,
    admin1_id = fac$admin1_id, admin2_id = fac$admin2_id,
    admin1_name = fac$admin1, admin2_name = fac$admin2,
    admin3_name = na_if_blank(fac$admin3_name), admin4_name = na_if_blank(fac$admin4_name),
    lat = fac$lat, lon = fac$lon, coords_valid = fac$coords_valid, quality_flag = fac$quality_flag,
    phone = na_if_blank(fac$phone), source_id = match(fac$data_source, sources$name),
    has_service_data = fac$has_service_data, nearest_eligible = fac$nearest_eligible,
    location_approximate = as.integer(if (is.null(fac$location_approximate)) FALSE else fac$location_approximate %in% TRUE),
    stringsAsFactors = FALSE))

  if (isTRUE(svc$available)) {
    d <- svc$domains
    d$domain_id <- seq_len(nrow(d))
    ins("service_domains", data.frame(
      domain_id = d$domain_id, domain_key = d$domain_key, label_en = d$label_en,
      label_sw = sw_or_null(d$label_sw, d$sw_status), sort_order = as.integer(d$sort_order),
      stringsAsFactors = FALSE))

    s <- svc$services
    s$service_id <- seq_len(nrow(s))
    ins("services", data.frame(
      service_id = s$service_id, service_key = s$public_key,
      domain_id = d$domain_id[match(s$public_domain_key, d$domain_key)],
      label_en = s$label_en, label_sw = sw_or_null(s$label_sw, s$sw_status),
      sort_order = as.integer(s$sort_order), is_malaria_related = s$is_malaria_related,
      facility_count = s$facility_count, in_menus = if (is.null(s$in_menus)) 1L else as.integer(s$in_menus),
      stringsAsFactors = FALSE))

    g <- unique(svc$etl[, c("service_domain", "service_group")])
    g <- g[order(g$service_domain, g$service_group), ]
    g$group_id <- seq_len(nrow(g))
    ins("service_groups", data.frame(group_id = g$group_id, etl_domain = g$service_domain,
                                     etl_group = g$service_group, stringsAsFactors = FALSE))
    e <- svc$etl
    e <- e[order(e$service_domain, e$service_group, e$service_name), ]
    ins("etl_services", data.frame(
      etl_service_id = seq_len(nrow(e)),
      group_id = g$group_id[match(paste(e$service_domain, e$service_group), paste(g$service_domain, g$service_group))],
      etl_service_name = e$service_name,
      service_id = s$service_id[match(e$public_key, s$public_key)], stringsAsFactors = FALSE))

    l <- svc$links
    ins("facility_services", data.frame(
      service_id = s$service_id[match(l$public_key, s$public_key)],
      fid = fac$fid[match(l$facility_uid, fac$uid)]))
  }

  ins("meta", data.frame(key = names(meta), value = vapply(meta, as.character, ""), stringsAsFactors = FALSE))
  DBI::dbCommit(con)

  ex(FTS_FILL_SQL)
  for (s in INDEX_SQL) ex(s)

  fk <- DBI::dbGetQuery(con, "PRAGMA foreign_key_check")
  if (nrow(fk)) validation_error("Foreign key check failed in the built bundle:", utils::capture.output(print(utils::head(fk, 20))))
  ic <- DBI::dbGetQuery(con, "PRAGMA integrity_check")[[1]]
  if (!identical(ic, "ok")) validation_error("SQLite integrity check failed:", ic)

  ex(sprintf("PRAGMA user_version = %d", SCHEMA_VERSION))
  ex("ANALYZE")
  ex("VACUUM")

  counts <- list()
  for (t in c("facilities", "admin_units", "facility_types", "service_domains", "services",
              "service_groups", "etl_services", "facility_services"))
    counts[[t]] <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", t))$n
  counts
}
