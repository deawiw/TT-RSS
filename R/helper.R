#' Вспомогательные функции для анализа мошеннических статей
#'
#' Этот файл содержит утилиты для постобработки результатов ETL.
#' Не влияет на основной пайплайн.
#'

#' @title Подсчёт мошеннических статей по источникам
#' @description Анализирует, какие источники чаще всего публикуют мошеннические новости
#' @param df Датафрейм с колонками source и is_fraud_related
#' @return Датафрейм с количеством статей по источникам
#' @examples
#' fraud_by_source <- count_fraud_by_source(articles)
count_fraud_by_source <- function(df) {
  if (!is.data.frame(df)) {
    stop("df должен быть датафреймом")
  }
  if (!"source" %in% colnames(df) || !"is_fraud_related" %in% colnames(df)) {
    stop("Датафрейм должен содержать колонки 'source' и 'is_fraud_related'")
  }
  
  result <- aggregate(df$is_fraud_related, 
                      by = list(Source = df$source), 
                      FUN = function(x) c(Total = length(x), Fraud = sum(x)))
  
  result$Total <- result$x[,1]
  result$Fraud <- result$x[,2]
  result$Percent <- round(result$Fraud / result$Total * 100, 2)
  result$x <- NULL
  
  result <- result[order(-result$Percent), ]
  return(result)
}

#' @title Фильтрация статей по дате
#' @description Оставляет статьи за последние N дней
#' @param df Датафрейм с колонкой published_at
#' @param days Количество дней (по умолчанию 7)
#' @return Отфильтрованный датафрейм
filter_by_days <- function(df, days = 7) {
  if (!"published_at" %in% colnames(df)) {
    stop("Датафрейм должен содержать колонку 'published_at'")
  }
  
  cutoff <- Sys.time() - (days * 86400)
  df[df$published_at >= cutoff, ]
}

#' @title Вывод статистики по мошенничеству
#' @description Печатает сводку по количеству мошеннических статей
#' @param df Датафрейм с колонкой is_fraud_related
print_fraud_summary <- function(df) {
  if (!"is_fraud_related" %in% colnames(df)) {
    stop("Датафрейм должен содержать колонку 'is_fraud_related'")
  }
  
  total <- nrow(df)
  fraud <- sum(df$is_fraud_related, na.rm = TRUE)
  
  cat("\n========== СТАТИСТИКА МОШЕННИЧЕСТВА ==========\n")
  cat(sprintf("Всего статей: %d\n", total))
  cat(sprintf("Мошеннических: %d\n", fraud))
  cat(sprintf("Обычных: %d\n", total - fraud))
  cat(sprintf("Доля: %.1f%%\n", fraud / total * 100))
  cat("==============================================\n")
}

#' @title Поиск по ключевым словам в заголовках
#' @description Ищет статьи с заданными ключевыми словами в заголовке
#' @param df Датафрейм с колонкой title
#' @param keywords Вектор ключевых слов для поиска
#' @return Датафрейм с найденными статьями
search_by_keywords <- function(df, keywords) {
  if (!"title" %in% colnames(df)) {
    stop("Датафрейм должен содержать колонку 'title'")
  }
  
  pattern <- paste(tolower(keywords), collapse = "|")
  matches <- grepl(pattern, tolower(df$title))
  df[matches, ]
}
