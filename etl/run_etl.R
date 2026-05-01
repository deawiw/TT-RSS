#!/usr/bin/env Rscript

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_flag <- "--file="
  file_arg <- grep(paste0("^", file_flag), cmd_args, value = TRUE)

  if (length(file_arg) > 0L) {
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
ensure_required_packages(c("httr2", "jsonlite", "DBI", "RPostgres"))
source(file.path(etl_dir, "R", "api_client.R"))
source(file.path(etl_dir, "R", "config.R"))
source(file.path(etl_dir, "R", "transform_articles.R"))
source(file.path(etl_dir, "R", "validate_articles.R"))
source(file.path(etl_dir, "R", "db_client.R"))
source(file.path(etl_dir, "R", "fraud_keywords.R"))

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

extract_feed_index <- function(feed_records) {
  if (length(feed_records) == 0L) {
    return(data.frame(feed_id = integer(), feed_title = character(), stringsAsFactors = FALSE))
  }

  feed_rows <- lapply(feed_records, function(record) {
    feed_id <- suppressWarnings(as.integer(trim_string(record$id)))
    feed_title <- clean_html_text(extract_candidate_value(record, c("title", "feed_title")))

    data.frame(
      feed_id = feed_id,
      feed_title = feed_title,
      stringsAsFactors = FALSE
    )
  })

  feed_index <- do.call(rbind, feed_rows)
  feed_index <- feed_index[!is.na(feed_index$feed_id) & feed_index$feed_id > 0L, , drop = FALSE]

  if (nrow(feed_index) == 0L) {
    return(feed_index)
  }

  feed_index <- feed_index[!duplicated(feed_index$feed_id), , drop = FALSE]
  rownames(feed_index) <- NULL
  feed_index
}

apply_feed_context <- function(records, feed_title) {
  lapply(records, function(record) {
    if (is.null(record$feed_title) || !nzchar(trim_string(record$feed_title))) {
      record$feed_title <- feed_title
    }

    record
  })
}

resolve_per_feed_limit <- function(settings) {
  configured_limits <- c(
    as.integer(settings$headlines_limit),
    as.integer(settings$max_articles_per_feed)
  )

  configured_limits <- configured_limits[!is.na(configured_limits) & configured_limits > 0L]

  if (length(configured_limits) == 0L) {
    fail("Не удалось определить рабочий лимит статей на ленту из конфигурации.")
  }

  min(configured_limits)
}

log_validation_report <- function(report) {
  status_message <- if (isTRUE(report$ok)) "Validation status: ok" else "Validation status: failed"
  log_info(status_message)

  warning_messages <- unlist(report$warnings, use.names = FALSE)
  error_messages <- unlist(report$errors, use.names = FALSE)

  if (length(warning_messages) > 0L) {
    for (warning_message in warning_messages) {
      log_warn(warning_message)
    }
  }

  if (length(error_messages) > 0L) {
    for (error_message in error_messages) {
      log_error(error_message)
    }
  }
}

compose_etl_run_message <- function(
  validation_report,
  transform_stats,
  db_load_result = list(inserted_count = 0L, duplicate_count = 0L),
  status = "success"
) {
  validation_errors <- unlist(validation_report$errors %||% list(), use.names = FALSE)
  validation_warnings <- unlist(validation_report$warnings %||% list(), use.names = FALSE)

  message_parts <- c(
    sprintf("status=%s", status),
    sprintf("transform_duplicates=%s", transform_stats$dropped_duplicates %||% 0L),
    sprintf("db_duplicates=%s", db_load_result$duplicate_count %||% 0L),
    sprintf("validation_warnings=%s", length(validation_warnings))
  )

  if (length(validation_errors) > 0L) {
    message_parts <- c(
      message_parts,
      sprintf("validation_errors=%s", paste(validation_errors, collapse = " | "))
    )
  }

  paste(message_parts, collapse = "; ")
}

run_etl_pipeline <- function() {
  cli_options <- parse_args(commandArgs(trailingOnly = TRUE), project_root)
  settings <- load_etl_settings(
    project_root = project_root,
    env_path = cli_options$env_file,
    config_path = cli_options$config_file
  )

  log_info(sprintf("ETL-секреты загружены из %s", settings$env_path))
  log_info(sprintf("ETL-конфиг загружен из %s", settings$config_path))

  output_dir <- settings$output_dir
  validation_report_file <- file.path(output_dir, "validation_report.json")
  data_raw_dir <- file.path(project_root, "data-raw")
  normalized_output_file <- file.path(data_raw_dir, "normalized_articles.csv")

  ensure_dir(output_dir)
  ensure_dir(data_raw_dir)

  session_id <- NULL
  db_conn <- NULL
  etl_run_id <- NULL
  run_started_at <- Sys.time()
  pipeline_status <- "failed"
  pipeline_message <- "ETL не завершен."
  final_counts <- list(
    raw_count = 0L,
    normalized_count = 0L,
    inserted_count = 0L,
    duplicate_count = 0L,
    invalid_count = 0L
  )

  on.exit(
    {
      if (!is.null(etl_run_id) && !is.null(db_conn) && DBI::dbIsValid(db_conn)) {
        tryCatch(
          {
            update_etl_run(
              conn = db_conn,
              run_id = etl_run_id,
              finished_at = Sys.time(),
              status = pipeline_status,
              raw_count = final_counts$raw_count,
              normalized_count = final_counts$normalized_count,
              inserted_count = final_counts$inserted_count,
              duplicate_count = final_counts$duplicate_count,
              invalid_count = final_counts$invalid_count,
              message = pipeline_message
            )
          },
          error = function(e) {
            log_error(sprintf("Не удалось обновить etl_runs: %s", conditionMessage(e)))
          }
        )
      }

      if (!is.null(session_id)) {
        if (isTRUE(tt_logout(settings$api_url, session_id, timeout_sec = settings$timeout_sec))) {
          log_info("Сессия TT-RSS успешно завершена.")
        }
      }

      if (!is.null(db_conn) && DBI::dbIsValid(db_conn)) {
        disconnect_postgres(db_conn)
        log_info("Соединение с PostgreSQL корректно закрыто.")
      }
    },
    add = TRUE
  )

  log_info(
    sprintf(
      "Подключаюсь к PostgreSQL %s:%s/%s пользователем %s",
      settings$db$host,
      settings$db$port,
      settings$db$name,
      settings$db$user
    )
  )

  pipeline_message <- "Подключение к PostgreSQL."
  db_conn <- connect_postgres(settings$db, timeout_sec = settings$timeout_sec)
  etl_run_id <- create_etl_run(
    conn = db_conn,
    started_at = run_started_at,
    message = "ETL запущен."
  )

  log_info(sprintf("Запуск ETL зарегистрирован в etl_runs: run_id=%s", etl_run_id))

  log_info(sprintf("Подключаюсь к TT-RSS API: %s", settings$api_url))
  pipeline_message <- "Авторизация в TT-RSS API."
  login_result <- tt_login(
    api_url = settings$api_url,
    user = settings$user,
    password = settings$password,
    timeout_sec = settings$timeout_sec
  )

  session_id <- login_result$session_id
  api_level_message <- if (is.na(login_result$api_level)) "не указан" else as.character(login_result$api_level)

  log_info(sprintf("Авторизация успешна. API level: %s", api_level_message))

  pipeline_message <- "Получение списка лент TT-RSS."
  feeds_result <- tt_get_feeds(
    api_url = settings$api_url,
    session_id = session_id,
    include_virtual_feeds = settings$include_virtual_feeds,
    timeout_sec = settings$timeout_sec
  )

  feed_index <- extract_feed_index(feeds_result$records)
  available_feed_count <- nrow(feed_index)

  if (available_feed_count == 0L) {
    fail("TT-RSS API не вернул ни одной обычной ленты с положительным feed_id.")
  }

  per_feed_limit <- resolve_per_feed_limit(settings)
  log_info(sprintf("Обычных лент найдено: %s. Рабочий лимит статей на ленту: %s.", available_feed_count, per_feed_limit))

  pipeline_message <- "Загрузка headlines из TT-RSS."
  all_headline_records <- list()

  for (feed_index_position in seq_len(nrow(feed_index))) {
    feed_id <- feed_index$feed_id[[feed_index_position]]
    feed_title <- feed_index$feed_title[[feed_index_position]]

    log_info(
      sprintf(
        "[%s/%s] Загружаю headlines для feed_id=%s (%s).",
        feed_index_position,
        nrow(feed_index),
        feed_id,
        truncate_string(feed_title, 120L)
      )
    )

    headlines_result <- tt_get_headlines(
      api_url = settings$api_url,
      session_id = session_id,
      feed_id = feed_id,
      limit = per_feed_limit,
      timeout_sec = settings$timeout_sec
    )

    contextualized_records <- apply_feed_context(headlines_result$records, feed_title = feed_title)
    all_headline_records <- c(all_headline_records, contextualized_records)

    log_info(sprintf("Получено headlines из ленты %s: %s", feed_id, length(contextualized_records)))

    if (settings$request_pause_sec > 0) {
      Sys.sleep(settings$request_pause_sec)
    }
  }

  if (length(all_headline_records) == 0L) {
    fail("TT-RSS не вернул ни одной headline-записи для выбранных лент.")
  }

  pipeline_message <- "Трансформация сырых headlines."
  extracted_at <- Sys.time()
  transform_result <- transform_headlines_records(
    raw_records = all_headline_records,
    extracted_at = extracted_at,
    drop_invalid_rows = TRUE
  )

  # ---- Первичный отбор ключевыми словами ----
  articles_df <- transform_result$data
  articles_df <- apply_fraud_keyword_filter(articles_df)

  write_csv_utf8(articles_df, normalized_output_file)

  pipeline_message <- "Валидация нормализованного датафрейма."
  validation_report <- validate_articles_df(
    articles_df = articles_df,
    transform_stats = transform_result$stats
  )

  write_json_pretty(validation_report, validation_report_file)

  final_counts$raw_count <- as.integer(transform_result$stats$total_records_raw %||% 0L)
  final_counts$normalized_count <- as.integer(nrow(articles_df))
  final_counts$invalid_count <- as.integer(transform_result$stats$dropped_invalid_rows %||% 0L)
  final_counts$duplicate_count <- as.integer(transform_result$stats$dropped_duplicates %||% 0L)

  log_info(sprintf("Финальный нормализованный датафрейм сохранен в %s", normalized_output_file))
  log_info(sprintf("Служебный validation report сохранен в %s", validation_report_file))
  log_info(
    sprintf(
      "Transform stats: raw=%s, after_transform=%s, dropped_duplicates=%s, dropped_invalid_rows=%s",
      transform_result$stats$total_records_raw,
      transform_result$stats$total_records_after_transform,
      transform_result$stats$dropped_duplicates,
      transform_result$stats$dropped_invalid_rows
    )
  )
  log_info(sprintf("После фильтрации ключевыми словами осталось %s статей", nrow(articles_df)))

  if (length(transform_result$warnings) > 0L) {
    for (warning_message in transform_result$warnings) {
      log_warn(warning_message)
    }
  }

  log_validation_report(validation_report)
  assert_validation_ok(validation_report)

  pipeline_message <- "Загрузка нормализованных статей в PostgreSQL."
  db_load_result <- load_articles_to_db(
    conn = db_conn,
    articles_df = articles_df,
    batch_size = settings$article_batch_size
  )

  final_counts$inserted_count <- db_load_result$inserted_count
  final_counts$duplicate_count <- as.integer(
    (transform_result$stats$dropped_duplicates %||% 0L) + db_load_result$duplicate_count
  )

  log_info(
    sprintf(
      "Загрузка в PostgreSQL завершена: prepared=%s, inserted=%s, skipped_duplicates=%s",
      db_load_result$total_rows,
      db_load_result$inserted_count,
      db_load_result$duplicate_count
    )
  )

  pipeline_status <- "success"
  pipeline_message <- compose_etl_run_message(
    validation_report = validation_report,
    transform_stats = transform_result$stats,
    db_load_result = db_load_result,
    status = pipeline_status
  )

  log_info(
    sprintf(
      "ETL завершен успешно: raw=%s, normalized=%s, inserted=%s, skipped_duplicates=%s, invalid=%s.",
      final_counts$raw_count,
      final_counts$normalized_count,
      final_counts$inserted_count,
      db_load_result$duplicate_count,
      final_counts$invalid_count
    )
  )

  invisible(TRUE)
}

tryCatch(
  run_etl_pipeline(),
  error = function(e) {
    log_error(conditionMessage(e))
    quit(save = "no", status = 1)
  }
)
