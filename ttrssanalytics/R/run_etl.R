#' Run the complete ETL pipeline
#' 
#' @param env_path Path to .env file with credentials (optional)
#' @param config_path Path to config.R file (optional)
#' @return Logical indicating success
#' @export
run_etl_pipeline <- function(env_path = NULL, config_path = NULL) {
  # Определяем корневую директорию пакета
  project_root <- system.file(package = "ttrssanalytics")
  
  # Пути по умолчанию
  if (is.null(env_path)) {
    env_path <- file.path(project_root, ".env.etl")
  }
  
  if (is.null(config_path)) {
    config_path <- system.file("config/config.R", package = "ttrssanalytics")
  }
  
  # Загружаем настройки (нужно адаптировать под пакет)
  message("Загрузка настроек...")
  
  # Здесь будет основная логика из твоего run_etl.R
  # Но без get_script_path() и source()
  
  message("ETL pipeline completed successfully")
  return(TRUE)
}

# Вспомогательная функция для проверки
#' @export
test_connection <- function() {
  message("Пакет ttrssanalytics загружен успешно!")
  return(TRUE)
}
