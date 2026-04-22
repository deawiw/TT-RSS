#!/usr/bin/env Rscript

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_flag <- "--file="
  file_arg <- grep(paste0("^", file_flag), cmd_args, value = TRUE)

  if (length(file_arg) > 0) {
    return(normalizePath(sub(file_flag, "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
  }

  current_frame <- sys.frames()[[1]]
  if (!is.null(current_frame$ofile)) {
    return(normalizePath(current_frame$ofile, winslash = "/", mustWork = TRUE))
  }

  stop("Не удалось определить путь к run_etl.R. Запустите скрипт через Rscript.", call. = FALSE)
}

script_path <- get_script_path()
etl_dir <- dirname(script_path)
project_root <- dirname(etl_dir)

source(file.path(etl_dir, "R", "utils.R"))
ensure_required_packages(c("httr2", "jsonlite"))
source(file.path(etl_dir, "R", "api_client.R"))
source(file.path(etl_dir, "R", "config.R"))

print_usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript etl/run_etl.R [--env-file .env.etl] [--config-file etl/config/config.R]",
      sep = "\n"
    ),
    "\n"
  )
}

parse_args <- function(args, project_root) {
  options <- list(
    env_file = file.path(project_root, ".env.etl"),
    config_file = file.path(project_root, "etl", "config", "config.R")
  )

  index <- 1L

  while (index <= length(args)) {
    arg <- args[[index]]

    if (arg %in% c("-h", "--help")) {
      print_usage()
      quit(save = "no", status = 0)
    }

    if (arg == "--env-file") {
      if (index == length(args)) {
        fail("После --env-file нужно указать путь к файлу.")
      }

      options$env_file <- resolve_path(args[[index + 1L]], project_root)
      index <- index + 2L
      next
    }

    if (arg == "--config-file") {
      if (index == length(args)) {
        fail("После --config-file нужно указать путь к файлу.")
      }

      options$config_file <- resolve_path(args[[index + 1L]], project_root)
      index <- index + 2L
      next
    }

    fail(sprintf("Неизвестный аргумент командной строки: %s", arg))
  }

  options
}

run_etl_smoke_test <- function() {
  cli_options <- parse_args(commandArgs(trailingOnly = TRUE), project_root)
  settings <- load_etl_settings(
    project_root = project_root,
    env_path = cli_options$env_file,
    config_path = cli_options$config_file
  )

  log_info(sprintf("ETL-секреты загружены из %s", settings$env_path))
  log_info(sprintf("ETL-конфиг загружен из %s", settings$config_path))

  output_dir <- settings$output_dir
  smoke_test_dir <- file.path(output_dir, "smoke-test")
  ensure_dir(smoke_test_dir)

  session_id <- NULL

  on.exit(
    {
      if (!is.null(session_id)) {
        if (isTRUE(tt_logout(settings$api_url, session_id, timeout_sec = settings$timeout_sec))) {
          log_info("Сессия TT-RSS успешно завершена.")
        }
      }
    },
    add = TRUE
  )

  log_info(sprintf("Подключаюсь к TT-RSS API: %s", settings$api_url))

  login_result <- tt_login(
    api_url = settings$api_url,
    user = settings$user,
    password = settings$password,
    timeout_sec = settings$timeout_sec
  )

  session_id <- login_result$session_id
  api_level_message <- if (is.na(login_result$api_level)) "не указан" else as.character(login_result$api_level)

  log_info(sprintf("Авторизация успешна. API level: %s", api_level_message))

  feeds_result <- tt_get_feeds(
    api_url = settings$api_url,
    session_id = session_id,
    include_virtual_feeds = settings$include_virtual_feeds,
    timeout_sec = settings$timeout_sec
  )

  feeds_count <- length(feeds_result$records)
  output_file <- file.path(smoke_test_dir, "getFeeds.json")
  write_json_pretty(feeds_result$raw, output_file)

  if (feeds_count == 0L) {
    log_warn("getFeeds выполнен успешно, но список лент пуст.")
  } else {
    log_info(sprintf("getFeeds выполнен успешно. Получено лент: %s", feeds_count))
  }

  log_info(sprintf("Сырой ответ getFeeds сохранен в %s", output_file))
  log_info("Текущая версия ETL — это каркас: логин, базовый getFeeds и инфраструктура конфигурации.")

  invisible(TRUE)
}

tryCatch(
  run_etl_smoke_test(),
  error = function(e) {
    log_error(conditionMessage(e))
    quit(save = "no", status = 1)
  }
)
