# Shared helpers: errors, logging, slugs, hashing, gzip.

BUILDER_VERSION <- "0.1.0"
SCHEMA_VERSION  <- 1L

# Validation failures carry this class so callers (CLI, tests) can tell them
# apart from programming errors.
validation_error <- function(msg, details = character(0)) {
  if (length(details)) {
    shown <- utils::head(details, 50)
    more  <- if (length(details) > 50) sprintf("\n  ... and %d more", length(details) - 50) else ""
    msg <- paste0(msg, "\n  - ", paste(shown, collapse = "\n  - "), more)
  }
  stop(structure(class = c("bundle_validation_error", "error", "condition"),
                 list(message = msg, call = NULL)))
}

log_msg <- function(...) {
  if (!isTRUE(getOption("bundle_builder.quiet"))) message("[bundle] ", sprintf(...))
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

is_blank <- function(x) is.na(x) | trimws(x) == ""

# Stable path-key component: ASCII, lower case, runs of other characters -> "-".
slugify <- function(x) {
  s <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  s <- tolower(s)
  s <- gsub("[^a-z0-9]+", "-", s)
  gsub("^-+|-+$", "", s)
}

sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

gzip_file <- function(src, dest) {
  size <- file.info(src)$size
  raw  <- readBin(src, what = "raw", n = size)
  con  <- gzfile(dest, open = "wb", compression = 9)
  on.exit(close(con))
  writeBin(raw, con)
  invisible(dest)
}

gunzip_to_raw <- function(path) {
  con <- gzfile(path, open = "rb")
  on.exit(close(con))
  out <- raw(0)
  repeat {
    chunk <- readBin(con, what = "raw", n = 1e7)
    if (!length(chunk)) break
    out <- c(out, chunk)
  }
  out
}

# Some sources contain the literal six characters `&` instead of "&".
clean_text <- function(x) trimws(gsub("\\u0026", "&", x, fixed = TRUE))
