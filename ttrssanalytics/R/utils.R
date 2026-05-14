`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }

  x
}

fail <- function(message) {
  stop(message, call. = FALSE)
}

trim_string <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return("")
  }

  trimws(as.character(value[[1]]))
}

is_blank <- function(value) {
  !nzchar(trim_string(value))
}

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

is_absolute_path <- function(path) {
  grepl("^(?:[A-Za-z]:|/|\\\\\\\\)", path)
}

resolve_path <- function(path, base_dir) {
  if (is_blank(path)) {
    fail("Получен пустой путь к файлу конфигурации.")
  }

  resolved <- if (is_absolute_path(path)) path else file.path(base_dir, path)
  normalizePath(resolved, winslash = "/", mustWork = FALSE)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(path)
}

write_csv_utf8 <- function(data, path) {
  ensure_dir(dirname(path))
  utils::write.csv(data, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(path)
}

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

log_info <- function(message) {
  log_message("INFO", message)
}

log_warn <- function(message) {
  log_message("WARN", message)
}

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
