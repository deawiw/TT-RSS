#' Объединение NULL-значений
#'
#' @param x Первое значение
#' @param y Второе значение
#' @return Если x не NULL, то x, иначе y
#' @export
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  x
}

#' Остановка выполнения с ошибкой
#'
#' @param message Текст ошибки
#' @return Останавливает выполнение
#' @export
fail <- function(message) {
  stop(message, call. = FALSE)
}

#' Удаление пробелов в начале и конце строки
#'
#' @param value Входная строка
#' @return Строка без пробелов по краям
#' @export
trim_string <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return("")
  }
  trimws(as.character(value[[1]]))
}

#' Проверка пустой строки
#'
#' @param value Проверяемое значение
#' @return TRUE если пустое, иначе FALSE
#' @export
is_blank <- function(value) {
  !nzchar(trim_string(value))
}

#' Обрезание длинной строки с добавлением троеточия
#'
#' @param value Входная строка
#' @param width Максимальная длина
#' @return Обрезанная строка
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

#' Проверка абсолютного пути
#'
#' @param path Путь для проверки
#' @return TRUE если путь абсолютный
#' @keywords internal
is_absolute_path <- function(path) {
  grepl("^(?:[A-Za-z]:|/|\\\\\\\\)", path)
}

#' Нормализация пути
#'
#' @param path Исходный путь
#' @param base_dir Базовая директория
#' @return Нормализованный путь
#' @export
resolve_path <- function(path, base_dir) {
  if (is_blank(path)) {
    fail("Получен пустой путь к файлу конфигурации.")
  }
  resolved <- if (is_absolute_path(path)) path else file.path(base_dir, path)
  normalizePath(resolved, winslash = "/", mustWork = FALSE)
}

#' Создание директории (если не существует)
#'
#' @param path Путь к директории
#' @return Путь к созданной директории
#' @export
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

#' Сохранение данных в CSV (UTF-8)
#'
#' @param data Датафрейм для сохранения
#' @param path Путь к файлу
#' @return Путь к сохранённому файлу
#' @export
write_csv_utf8 <- function(data, path) {
  ensure_dir(dirname(path))
  utils::write.csv(data, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(path)
}

#' Форматирование времени в UTC
#'
#' @param value Время для форматирования
#' @return Строка в формате UTC
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

#' Сохранение данных в JSON
#'
#' @param data Данные для сохранения
#' @param path Путь к файлу
#' @return Путь к сохранённому файлу
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

#' Внутренняя функция логирования
#'
#' @param level Уровень логирования
#' @param message Сообщение
#' @keywords internal
log_message <- function(level, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = Sys.timezone() %||% "UTC")
  output <- sprintf("[%s] [%s] %s\n", timestamp, level, message)
  stream <- if (identical(level, "ERROR")) stderr() else stdout()
  cat(output, file = stream)
}

#' Информационное сообщение
#'
#' @param message Сообщение для вывода
#' @export
log_info <- function(message) {
  log_message("INFO", message)
}

#' Предупреждение
#'
#' @param message Сообщение для вывода
#' @export
log_warn <- function(message) {
  log_message("WARN", message)
}

#' Сообщение об ошибке
#'
#' @param message Сообщение для вывода
#' @export
log_error <- function(message) {
  log_message("ERROR", message)
}

#' Проверка наличия необходимых пакетов
#'
#' @param packages Вектор с именами пакетов
#' @keywords internal
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
