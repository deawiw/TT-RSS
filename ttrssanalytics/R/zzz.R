#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  # Путь к конфигу внутри пакета
  config_path <- system.file("config/config.R", package = pkgname)
  
  if (file.exists(config_path)) {
    source(config_path, local = TRUE)
    assign("etl_config", etl_config, envir = parent.env(environment()))
  }
}
