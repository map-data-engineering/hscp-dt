# Read the ETL CSVs. Only the columns the bundle needs are kept.

FACILITY_REQUIRED <- c("uid", "facility_code", "facility_name", "facility_type", "ownership",
                       "admin1", "latitude", "longitude", "coordinate_valid",
                       "data_quality_flag", "data_source")
FACILITY_OPTIONAL <- c("ownership_detail", "status", "admin2")
SERVICE_REQUIRED  <- c("facility_uid", "service_domain", "service_group", "service_name",
                       "is_malaria_related", "include_in_analysis", "needs_review")

read_etl_csv <- function(path, keep) {
  header <- names(readr::read_csv(path, n_max = 0, show_col_types = FALSE, progress = FALSE))
  cols <- intersect(keep, header)
  df <- readr::read_csv(
    path,
    col_types = do.call(readr::cols_only, stats::setNames(rep(list(readr::col_character()), length(cols)), cols)),
    na = c("", "NA"), progress = FALSE, locale = readr::locale(encoding = "UTF-8")
  )
  list(data = as.data.frame(df, stringsAsFactors = FALSE), header = header)
}

read_facilities <- function(etl_dir, country) {
  path <- file.path(etl_dir, paste0(country$country_slug, "_standardized.csv"))
  if (!file.exists(path)) validation_error(sprintf("Facilities file not found: %s", path))
  extra <- Filter(function(x) !is_blank(x), c(country$admin3_column, country$admin4_column, country$phone_column))
  r <- read_etl_csv(path, c(FACILITY_REQUIRED, FACILITY_OPTIONAL, extra))
  missing <- setdiff(c(FACILITY_REQUIRED, extra), r$header)
  if (length(missing)) validation_error(sprintf("%s is missing required columns:", basename(path)), missing)
  df <- r$data
  for (col in FACILITY_OPTIONAL) if (!col %in% names(df)) df[[col]] <- NA_character_
  df$admin3_name <- if (!is_blank(country$admin3_column)) df[[country$admin3_column]] else NA_character_
  df$admin4_name <- if (!is_blank(country$admin4_column)) df[[country$admin4_column]] else NA_character_
  df$phone       <- if (!is_blank(country$phone_column))  df[[country$phone_column]]  else NA_character_
  attr(df, "path") <- path
  df
}

# Returns NULL when the country has no services file (Nigeria, Uganda, Zambia).
read_services <- function(etl_dir, country) {
  path <- file.path(etl_dir, paste0(country$country_slug, "_services_standardized.csv"))
  if (!file.exists(path)) return(NULL)
  r <- read_etl_csv(path, SERVICE_REQUIRED)
  missing <- setdiff(SERVICE_REQUIRED, r$header)
  if (length(missing)) validation_error(sprintf("%s is missing required columns:", basename(path)), missing)
  df <- r$data
  attr(df, "path") <- path
  df
}

# ETL provenance: commit hash and date of the git repo holding the CSVs, if any.
etl_git_info <- function(etl_dir) {
  git <- Sys.which("git")
  if (!nzchar(git)) return(list(commit = NA_character_, date = NA_character_))
  out <- tryCatch(
    suppressWarnings(system2(git, c("-C", shQuote(etl_dir), "log", "-1", "--format=%h|%cs"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0))
  if (!length(out) || !grepl("|", out[1], fixed = TRUE)) return(list(commit = NA_character_, date = NA_character_))
  parts <- strsplit(out[1], "|", fixed = TRUE)[[1]]
  list(commit = parts[1], date = parts[2])
}
