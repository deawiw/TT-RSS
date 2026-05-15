#' @keywords internal
"_PACKAGE"

#' @title Sample normalized articles
#' @description Example normalized TT-RSS article data created from the first rows of data-raw/normalized_articles.csv.
#' @format A data frame with normalized article columns.
"sample_articles"

.onLoad <- function(libname, pkgname) {
  # Путь к конфигу внутри пакета
  config_path <- system.file("config/config.R", package = pkgname)
  
  if (file.exists(config_path)) {
    source(config_path, local = TRUE)
    assign("etl_config", etl_config, envir = parent.env(environment()))
  }
}
