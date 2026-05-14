#' Load ETL settings from .env and config.R
#' 
#' @param package_root Root directory of the package
#' @param env_path Path to .env file
#' @param config_path Path to config.R file
#' @return List with all settings
#' @keywords internal
load_etl_settings <- function(package_root, env_path, config_path) {
  # Загрузка переменных окружения
  if (file.exists(env_path)) {
    readRenviron(env_path)
    log_info(sprintf("Загружены переменные окружения из %s", env_path))
  } else {
    log_warn(sprintf("Файл .env не найден: %s", env_path))
  }
  
  # Загрузка конфигурации
  etl_config <- list(
    headlines_limit = 200L,
    max_articles_per_feed = 1000L,
    include_virtual_feeds = TRUE,
    request_pause_sec = 0.15,
    article_batch_size = 50L,
    timeout_sec = 60L,
    output_dir = "etl/output"
  )
  
  if (file.exists(config_path)) {
    source(config_path, local = TRUE)
    if (exists("etl_config")) {
      etl_config <- modifyList(etl_config, etl_config)
    }
    log_info(sprintf("Загружена конфигурация из %s", config_path))
  }
  
  # Формируем настройки
  settings <- list(
    api_url = Sys.getenv("TTRSS_API_URL", "http://localhost:8283/api/"),
    user = Sys.getenv("TTRSS_USER", ""),
    password = Sys.getenv("TTRSS_PASSWORD", ""),
    db = list(
      host = Sys.getenv("DB_HOST", "localhost"),
      port = Sys.getenv("DB_PORT", "5433"),
      name = Sys.getenv("DB_NAME", "news_analytics"),
      user = Sys.getenv("DB_USER", ""),
      password = Sys.getenv("DB_PASSWORD", "")
    ),
    env_path = env_path,
    config_path = config_path,
    headlines_limit = etl_config$headlines_limit,
    max_articles_per_feed = etl_config$max_articles_per_feed,
    include_virtual_feeds = etl_config$include_virtual_feeds,
    request_pause_sec = etl_config$request_pause_sec,
    article_batch_size = etl_config$article_batch_size,
    timeout_sec = etl_config$timeout_sec,
    output_dir = etl_config$output_dir
  )
  
  # Проверка обязательных параметров
  if (settings$user == "" || settings$password == "") {
    fail("Не заданы TTRSS_USER или TTRSS_PASSWORD в .env файле")
  }
  
  return(settings)
}
