#' @title Null coalescing operator
#' @description Returns the fallback value when the left-hand value is NULL or empty.
#' @param x Primary value.
#' @param y Fallback value.
#' @return x when it is non-empty; otherwise y.
#' @export
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }

  x
}

#' @title Stop with an ETL error
#' @description Raises an error without attaching the call, keeping ETL failure messages concise.
#' @param message Error message to raise.
#' @return This function does not return; it stops execution.
#' @export
fail <- function(message) {
  stop(message, call. = FALSE)
}

#' @title Trim a scalar string
#' @description Converts a value to a scalar character string and trims surrounding whitespace.
#' @param value Value to trim.
#' @return Character scalar.
#' @export
trim_string <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return("")
  }

  trimws(as.character(value[[1]]))
}

is_blank <- function(value) {
  !nzchar(trim_string(value))
}

#' @title Truncate a string
#' @description Shortens a string to a target display width and appends an ellipsis when needed.
#' @param value Value to truncate.
#' @param width Maximum display width.
#' @return Character scalar.
#' @export
truncate_string <- function(value, width = 200L) {
  value <- trim_string(value)

  if (!nzchar(value)) {
    return("")
  }

  width <- as.integer(width)
  if (is.na(width) || width < 10L) {
    width <- 10L
  }

  if (nchar(value, type = "width") <= width) {
    return(value)
  }

  paste0(substr(value, 1L, width - 3L), "...")
}

#' @title Check whether a path is absolute
#' @description Detects Unix, Windows drive-letter, and UNC absolute paths.
#' @param path Path string to check.
#' @return Logical scalar indicating whether the path is absolute.
is_absolute_path <- function(path) {
  grepl("^(?:[A-Za-z]:|/|\\\\\\\\)", path)
}

#' @title Resolve a path against a base directory
#' @description Converts relative paths to normalized paths under a base directory while leaving absolute paths unchanged.
#' @param path Path to resolve.
#' @param base_dir Base directory used for relative paths.
#' @return Normalized path string.
#' @export
resolve_path <- function(path, base_dir) {
  if (is_blank(path)) {
    fail("Получен пустой путь к файлу конфигурации.")
  }

  resolved <- if (is_absolute_path(path)) path else file.path(base_dir, path)
  normalizePath(resolved, winslash = "/", mustWork = FALSE)
}

#' @title Ensure a directory exists
#' @description Creates a directory recursively when it does not already exist.
#' @param path Directory path.
#' @return Invisibly returns the directory path.
#' @export
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(path)
}

#' @title Write UTF-8 CSV
#' @description Writes a data frame to CSV using UTF-8 encoding and creates the parent directory if needed.
#' @param data Data frame or table-like object to write.
#' @param path Output CSV path.
#' @return Invisibly returns the output path.
#' @export
write_csv_utf8 <- function(data, path) {
  ensure_dir(dirname(path))
  utils::write.csv(data, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(path)
}

#' @title Format UTC timestamp
#' @description Converts a date-time value to a UTC ISO-8601 timestamp string.
#' @param value Date-time value to format.
#' @return Character scalar timestamp or NA when conversion fails.
#' @export
format_utc_timestamp <- function(value = Sys.time()) {
  datetime <- tryCatch(
    as.POSIXct(value, origin = "1970-01-01", tz = "UTC"),
    error = function(e) NA
  )

  if (is.na(datetime)) {
    return(NA_character_)
  }

  format(datetime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' @title Write pretty JSON
#' @description Writes an object as pretty JSON and creates the parent directory if needed.
#' @param data Object to serialize to JSON.
#' @param path Output JSON path.
#' @return Invisibly returns the output path.
#' @export
write_json_pretty <- function(data, path) {
  ensure_dir(dirname(path))

  json_text <- jsonlite::toJSON(
    data,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    force = TRUE
  )

  writeLines(json_text, path, useBytes = TRUE)
  invisible(path)
}

log_message <- function(level, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = Sys.timezone() %||% "UTC")
  output <- sprintf("[%s] [%s] %s\n", timestamp, level, message)
  stream <- if (identical(level, "ERROR")) stderr() else stdout()

  cat(output, file = stream)
}

#' @title Log an informational ETL message
#' @description Writes an informational ETL message to the standard output stream.
#' @param message Message to write.
#' @return No meaningful return value.
#' @export
log_info <- function(message) {
  log_message("INFO", message)
}

#' @title Log an ETL warning message
#' @description Writes a warning ETL message to the standard output stream.
#' @param message Message to write.
#' @return No meaningful return value.
#' @export
log_warn <- function(message) {
  log_message("WARN", message)
}

#' @title Log an ETL error message
#' @description Writes an error ETL message to the standard error stream.
#' @param message Message to write.
#' @return No meaningful return value.
#' @export
log_error <- function(message) {
  log_message("ERROR", message)
}

ensure_required_packages <- function(packages) {
  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    fail(
      sprintf(
        "Не хватает R-пакетов: %s. Установите их перед запуском ETL.",
        paste(missing_packages, collapse = ", ")
      )
    )
  }

  invisible(TRUE)
}
