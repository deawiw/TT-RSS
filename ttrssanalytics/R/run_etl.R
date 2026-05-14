#' Run ETL Pipeline
#' 
#' Extracts articles from TT-RSS API, transforms them, detects fraud-related content,
#' and loads into PostgreSQL database.
#' 
#' @param env_path Path to .env file with credentials (default: looks for .env.etl)
#' @param config_path Path to config.R file (default: uses built-in config)
#' @return Logical indicating success
#' @export
run_etl_pipeline <- function(env_path = NULL, config_path = NULL) {
  # Определяем корневую директорию пакета
  package_root <- system.file(package = "ttrssanalytics")
  
  # Пути по умолчанию
  if (is.null(env_path)) {
    env_path <- file.path(package_root, ".env.etl")
    # Если нет в пакете, ищем в рабочей директории
    if (!file.exists(env_path)) {
      env_path <- file.path(getwd(), ".env.etl")
    }
  }
  
  if (is.null(config_path)) {
    config_path <- system.file("config/config.R", package = "ttrssanalytics")
  }
  
  # Загружаем настройки
  settings <- load_etl_settings(
    package_root = package_root,
    env_path = env_path,
    config_path = config_path
  )
  
  # Создаем директории для выходных файлов
  data_raw_dir <- file.path(getwd(), "data-raw")
  output_dir <- file.path(getwd(), settings$output_dir)
  
  ensure_dir(data_raw_dir)
  ensure_dir(output_dir)
  
  validation_report_file <- file.path(output_dir, "validation_report.json")
  normalized_output_file <- file.path(data_raw_dir, "normalized_articles.csv")
  
  session_id <- NULL
  db_conn <- NULL
  run_started_at <- Sys.time()
  pipeline_status <- "failed"
  pipeline_message <- "ETL не завершен."
  
  on.exit({
    if (!is.null(session_id)) {
      tt_logout(settings$api_url, session_id, timeout_sec = settings$timeout_sec)
    }
    if (!is.null(db_conn) && DBI::dbIsValid(db_conn)) {
      disconnect_postgres(db_conn)
    }
  }, add = TRUE)
  
  # Подключение к БД
  log_info(sprintf("Подключаюсь к PostgreSQL %s:%s/%s", 
                   settings$db$host, settings$db$port, settings$db$name))
  db_conn <- connect_postgres(settings$db, timeout_sec = settings$timeout_sec)
  
  # Авторизация в TT-RSS
  log_info(sprintf("Подключаюсь к TT-RSS API: %s", settings$api_url))
  login_result <- tt_login(
    api_url = settings$api_url,
    user = settings$user,
    password = settings$password,
    timeout_sec = settings$timeout_sec
  )
  session_id <- login_result$session_id
  log_info("Авторизация успешна")
  
  # Получение списка лент
  log_info("Получение списка лент...")
  feeds_result <- tt_get_feeds(
    api_url = settings$api_url,
    session_id = session_id,
    include_virtual_feeds = settings$include_virtual_feeds,
    timeout_sec = settings$timeout_sec
  )
  
  # Извлечение индекса лент
  feed_index <- extract_feed_index(feeds_result$records)
  log_info(sprintf("Найдено лент: %d", nrow(feed_index$feeds)))
  
  if (nrow(feed_index$feeds) == 0) {
    fail("Не найдено ни одной ленты для обработки")
  }
  
  # Загрузка статей
  all_records <- list()
  per_feed_limit <- min(settings$headlines_limit, settings$max_articles_per_feed)
  
  for (i in seq_len(nrow(feed_index$feeds))) {
    feed_id <- feed_index$feeds$feed_id[i]
    feed_title <- feed_index$feeds$feed_title[i]
    
    log_info(sprintf("[%d/%d] Загрузка feed_id=%s (%s)", 
                     i, nrow(feed_index$feeds), feed_id, 
                     truncate_string(feed_title, 50)))
    
    headlines <- tt_get_headlines(
      api_url = settings$api_url,
      session_id = session_id,
      feed_id = feed_id,
      limit = per_feed_limit,
      timeout_sec = settings$timeout_sec
    )
    
    all_records <- c(all_records, headlines$records)
    
    if (settings$request_pause_sec > 0) {
      Sys.sleep(settings$request_pause_sec)
    }
  }
  
  log_info(sprintf("Всего загружено записей: %d", length(all_records)))
  
  # Трансформация
  log_info("Трансформация данных...")
  transform_result <- transform_headlines_records(
    raw_records = all_records,
    extracted_at = Sys.time(),
    drop_invalid_rows = TRUE
  )
  
  # Фильтрация по ключевым словам (мошенничество)
  articles_df <- transform_result$data
  articles_df <- apply_fraud_keyword_filter(articles_df)
  
  log_info(sprintf("После фильтрации мошенничества осталось: %d статей", nrow(articles_df)))
  
  # Сохранение результатов
  write_csv_utf8(articles_df, normalized_output_file)
  log_info(sprintf("Сохранено в %s", normalized_output_file))
  
  # Валидация
  log_info("Валидация данных...")
  validation_report <- validate_articles_df(
    articles_df = articles_df,
    transform_stats = transform_result$stats
  )
  write_json_pretty(validation_report, validation_report_file)
  
  if (validation_report$ok) {
    log_info("Валидация успешна")
    pipeline_status <- "success"
  } else {
    log_warn("Валидация предупредила о проблемах")
    pipeline_status <- "warning"
  }
  
  log_info(sprintf("ETL завершен. Статус: %s", pipeline_status))
  invisible(TRUE)
}

#' Test package connection
#' @export
test_connection <- function() {
  log_info("Пакет ttrssanalytics загружен успешно!")
  log_info("Доступные функции:")
  log_info("  - run_etl_pipeline() - запуск ETL")
  log_info("  - apply_fraud_keyword_filter() - фильтрация мошенничества")
  log_info("  - tt_login() / tt_get_headlines() / tt_get_feeds() - работа с TT-RSS")
  return(TRUE)
}
