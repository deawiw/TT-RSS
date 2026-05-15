strip_optional_quotes <- function(value) {
  if (!nzchar(value)) {
    return(value)
  }

  has_double_quotes <- startsWith(value, "\"") && endsWith(value, "\"")
  has_single_quotes <- startsWith(value, "'") && endsWith(value, "'")

  if (has_double_quotes || has_single_quotes) {
    return(substr(value, 2L, nchar(value) - 1L))
  }

  value
}

#' @title Read an ETL environment file
#' @description Parses key-value pairs from an .env-style ETL configuration file.
#' @param path Path to the environment file.
#' @param required Logical flag indicating whether a missing file should be an error.
#' @return Named list of values parsed from the file.
#' @export
read_env_file <- function(path, required = TRUE) {
  if (!file.exists(path)) {
    if (!isTRUE(required)) {
      return(list())
    }

    fail(
      sprintf(
        "Не найден .env.etl: %s. Создайте его из шаблона .env.etl.example.",
        path
      )
    )
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  values <- list()

  for (line_index in seq_along(lines)) {
    raw_line <- lines[[line_index]]
    line <- trimws(raw_line)

    if (!nzchar(line) || startsWith(line, "#")) {
      next
    }

    line <- sub("^export\\s+", "", line)

    eq_pos <- regexpr("=", line, fixed = TRUE)[[1]]
    if (eq_pos < 2L) {
      fail(
        sprintf(
          "Некорректная строка в %s (line %s): %s",
          path,
          line_index,
          raw_line
        )
      )
    }

    key <- trimws(substr(line, 1L, eq_pos - 1L))
    value <- trimws(substr(line, eq_pos + 1L, nchar(line)))
    value <- strip_optional_quotes(value)

    values[[key]] <- value
  }

  values
}

etl_required_env_vars <- function() {
  c(
    "TTRSS_BASE_URL",
    "TTRSS_USER",
    "TTRSS_PASSWORD",
    "DB_HOST",
    "DB_PORT",
    "DB_NAME",
    "DB_USER",
    "DB_PASSWORD"
  )
}

merge_process_env <- function(values, var_names) {
  for (var_name in var_names) {
    env_value <- Sys.getenv(var_name, unset = NA_character_)

    if (!is.na(env_value) && nzchar(trimws(env_value))) {
      values[[var_name]] <- env_value
    }
  }

  values
}

#' @title Load ETL secrets
#' @description Loads TT-RSS and PostgreSQL settings from an env file and lets process environment values override them.
#' @param env_path Path to the ETL environment file.
#' @return List with TT-RSS credentials, base URL, and database connection settings.
#' @export
load_etl_secrets <- function(env_path) {
  env_path <- normalizePath(env_path, winslash = "/", mustWork = FALSE)
  required_vars <- etl_required_env_vars()
  process_env_values <- Sys.getenv(required_vars, unset = "")
  has_required_process_env <- all(nzchar(trimws(process_env_values)))
  values <- read_env_file(env_path, required = !has_required_process_env)
  values <- merge_process_env(values, required_vars)

  for (var_name in required_vars) {
    value <- values[[var_name]] %||% ""

    if (!nzchar(trimws(value))) {
      fail(
        sprintf(
          "Отсутствует обязательная переменная %s в %s.",
          var_name,
          env_path
        )
      )
    }
  }

  do.call(Sys.setenv, values)

  db_port <- suppressWarnings(as.integer(trimws(values$DB_PORT)))

  if (is.na(db_port) || db_port <= 0L) {
    fail(sprintf("DB_PORT in %s must be a positive integer.", env_path))
  }

  list(
    env_path = if (file.exists(env_path)) env_path else "process environment",
    base_url = values$TTRSS_BASE_URL,
    user = values$TTRSS_USER,
    password = values$TTRSS_PASSWORD,
    db = list(
      host = values$DB_HOST,
      port = db_port,
      name = values$DB_NAME,
      user = values$DB_USER,
      password = values$DB_PASSWORD
    )
  )
}

require_scalar_number <- function(name, value, min_value = NULL) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
    fail(sprintf("Параметр %s должен быть числом.", name))
  }

  if (!is.null(min_value) && value < min_value) {
    fail(sprintf("Параметр %s должен быть >= %s.", name, min_value))
  }

  value
}

require_scalar_logical <- function(name, value) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    fail(sprintf("Параметр %s должен быть TRUE/FALSE.", name))
  }

  value
}

require_scalar_string <- function(name, value) {
  if (!is.character(value) || length(value) != 1L || !nzchar(trimws(value))) {
    fail(sprintf("Параметр %s должен быть непустой строкой.", name))
  }

  trimws(value)
}

normalize_runtime_config <- function(config, project_root, config_path) {
  required_fields <- c(
    "headlines_limit",
    "max_articles_per_feed",
    "include_virtual_feeds",
    "sample_full_articles_per_feed",
    "request_pause_sec",
    "article_batch_size",
    "timeout_sec",
    "output_dir"
  )

  missing_fields <- required_fields[
    !vapply(required_fields, function(field_name) !is.null(config[[field_name]]), logical(1))
  ]

  if (length(missing_fields) > 0) {
    fail(
      sprintf(
        "В %s отсутствуют параметры: %s.",
        config_path,
        paste(missing_fields, collapse = ", ")
      )
    )
  }

  list(
    headlines_limit = as.integer(require_scalar_number("headlines_limit", config$headlines_limit, 1)),
    max_articles_per_feed = as.integer(
      require_scalar_number("max_articles_per_feed", config$max_articles_per_feed, 1)
    ),
    include_virtual_feeds = require_scalar_logical(
      "include_virtual_feeds",
      config$include_virtual_feeds
    ),
    sample_full_articles_per_feed = as.integer(
      require_scalar_number(
        "sample_full_articles_per_feed",
        config$sample_full_articles_per_feed,
        0
      )
    ),
    request_pause_sec = as.numeric(
      require_scalar_number("request_pause_sec", config$request_pause_sec, 0)
    ),
    article_batch_size = as.integer(
      require_scalar_number("article_batch_size", config$article_batch_size, 1)
    ),
    timeout_sec = as.numeric(require_scalar_number("timeout_sec", config$timeout_sec, 1)),
    output_dir = resolve_path(
      require_scalar_string("output_dir", config$output_dir),
      project_root
    ),
    config_path = normalizePath(config_path, winslash = "/", mustWork = FALSE)
  )
}

#' @title Load ETL runtime config
#' @description Loads and validates an ETL runtime config.R file.
#' @param config_path Path to the runtime config.R file.
#' @param project_root Base project directory used to resolve relative paths.
#' @return Normalized runtime configuration list.
#' @export
load_runtime_config <- function(config_path, project_root) {
  if (!file.exists(config_path)) {
    fail(sprintf("Не найден ETL-конфиг: %s.", config_path))
  }

  config_env <- new.env(parent = baseenv())
  sys.source(config_path, envir = config_env)

  if (!exists("etl_config", envir = config_env, inherits = FALSE)) {
    fail(
      sprintf(
        "Файл %s должен определять объект etl_config со списком параметров.",
        config_path
      )
    )
  }

  config <- get("etl_config", envir = config_env, inherits = FALSE)

  if (!is.list(config)) {
    fail(sprintf("Объект etl_config в %s должен быть списком.", config_path))
  }

  normalize_runtime_config(config, project_root, config_path)
}

#' @title Load complete ETL settings
#' @description Combines ETL secrets and runtime config into a single normalized settings object.
#' @param project_root Base project directory used to resolve relative paths.
#' @param env_path Path to the ETL environment file.
#' @param config_path Path to the ETL runtime config.R file.
#' @return List with all settings needed by the ETL workflow.
#' @export
load_etl_settings <- function(project_root, env_path, config_path) {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = FALSE)
  env_path <- resolve_path(env_path, project_root)
  config_path <- resolve_path(config_path, project_root)

  secrets <- load_etl_secrets(env_path)
  runtime_config <- load_runtime_config(config_path, project_root)

  ensure_dir(runtime_config$output_dir)

  list(
    project_root = project_root,
    env_path = secrets$env_path,
    config_path = runtime_config$config_path,
    base_url = secrets$base_url,
    api_url = build_api_url(secrets$base_url),
    user = secrets$user,
    password = secrets$password,
    headlines_limit = runtime_config$headlines_limit,
    max_articles_per_feed = runtime_config$max_articles_per_feed,
    include_virtual_feeds = runtime_config$include_virtual_feeds,
    sample_full_articles_per_feed = runtime_config$sample_full_articles_per_feed,
    request_pause_sec = runtime_config$request_pause_sec,
    article_batch_size = runtime_config$article_batch_size,
    timeout_sec = runtime_config$timeout_sec,
    output_dir = runtime_config$output_dir,
    db = secrets$db
  )
}
