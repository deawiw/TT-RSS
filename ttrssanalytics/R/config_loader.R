#' @title Load package ETL settings
#' @description Loads ETL settings using the current working directory as the project root and the package config as the default runtime config.
#' @param env_path Path to the ETL environment file.
#' @param config_path Path to the ETL runtime config.R file.
#' @return List with all settings needed by the ETL workflow.
#' @export
load_package_etl_settings <- function(
  env_path = file.path(getwd(), ".env.etl"),
  config_path = system.file("config/config.R", package = "ttrssanalytics")
) {
  load_etl_settings(
    project_root = getwd(),
    env_path = env_path,
    config_path = config_path
  )
}
