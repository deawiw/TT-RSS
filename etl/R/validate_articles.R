required_article_value_columns <- function() {
  c("news_id", "source", "title", "published_at", "url", "extracted_at")
}

as_message_list <- function(messages) {
  as.list(unname(as.character(messages)))
}

merge_named_lists <- function(base_list, override_list) {
  merged <- base_list

  for (field_name in names(override_list)) {
    merged[[field_name]] <- override_list[[field_name]]
  }

  merged
}

is_normalized_datetime_string <- function(values) {
  values <- as.character(values)
  pattern_ok <- grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", values)
  parsed_ok <- !is.na(
    as.POSIXct(values, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  pattern_ok & parsed_ok
}

is_valid_http_url <- function(values) {
  values <- as.character(values)
  grepl("^https?://\\S+$", values, ignore.case = TRUE)
}

detect_article_duplicates <- function(articles_df) {
  if (nrow(articles_df) == 0) {
    return(list(exact = 0L, by_key = 0L))
  }

  exact_duplicates <- sum(duplicated(articles_df))
  dedup_key <- ifelse(
    nzchar(articles_df$url),
    paste0("url:", articles_df$url),
    paste0("news_id:", articles_df$news_id)
  )
  key_duplicates <- sum(duplicated(dedup_key))

  list(
    exact = as.integer(exact_duplicates),
    by_key = as.integer(key_duplicates)
  )
}

validate_articles_df <- function(
  articles_df,
  transform_stats = list(),
  suspicious_text_length = 50L
) {
  errors <- character()
  warnings <- character()
  required_columns <- article_required_columns()
  missing_columns <- setdiff(required_columns, names(articles_df))

  if (length(missing_columns) > 0) {
    errors <- c(
      errors,
      sprintf(
        "Отсутствуют обязательные колонки: %s.",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  if (length(errors) > 0) {
    return(list(
      ok = FALSE,
      failed = TRUE,
      errors = as_message_list(errors),
      warnings = as_message_list(warnings),
      stats = c(transform_stats, list(total_records_validated = 0L))
    ))
  }

  total_records <- as.integer(nrow(articles_df))

  if (total_records == 0L) {
    errors <- c(errors, "После трансформации не осталось валидных записей.")
  }

  empty_required_counts <- vapply(
    required_article_value_columns(),
    function(column_name) {
      values <- trimws(as.character(articles_df[[column_name]]))
      sum(!nzchar(values))
    },
    integer(1)
  )

  if (any(empty_required_counts > 0)) {
    failing_columns <- names(empty_required_counts)[empty_required_counts > 0]
    errors <- c(
      errors,
      sprintf(
        "Пустые значения найдены в обязательных полях: %s.",
        paste(failing_columns, collapse = ", ")
      )
    )
  }

  duplicate_info <- detect_article_duplicates(articles_df)

  if (duplicate_info$exact > 0L || duplicate_info$by_key > 0L) {
    errors <- c(
      errors,
      sprintf(
        "После трансформации остались дубликаты: exact=%s, by_key=%s.",
        duplicate_info$exact,
        duplicate_info$by_key
      )
    )
  }

  published_at_valid <- is_normalized_datetime_string(articles_df$published_at)
  invalid_published_at_count <- sum(!published_at_valid)

  if (invalid_published_at_count > 0L) {
    errors <- c(
      errors,
      sprintf(
        "published_at не приведен к нормальному формату у %s записей.",
        invalid_published_at_count
      )
    )
  }

  url_blank_count <- sum(!nzchar(trimws(as.character(articles_df$url))))
  url_invalid_count <- sum(
    nzchar(trimws(as.character(articles_df$url))) & !is_valid_http_url(articles_df$url)
  )

  if (url_blank_count > 0L) {
    errors <- c(
      errors,
      sprintf("Пустой url найден у %s записей.", url_blank_count)
    )
  }

  if (url_invalid_count > 0L) {
    errors <- c(
      errors,
      sprintf("Некорректный url найден у %s записей.", url_invalid_count)
    )
  }

  html_tag_count <- sum(grepl("<[^>]+>", articles_df$content_text, perl = TRUE))
  html_entity_count <- sum(
    grepl(
      "&(?:#[0-9]+|#[xX][0-9A-Fa-f]+|[A-Za-z][A-Za-z0-9]+);",
      articles_df$content_text,
      perl = TRUE
    )
  )

  if (html_tag_count > 0L) {
    errors <- c(
      errors,
      sprintf("В content_text остались HTML-теги у %s записей.", html_tag_count)
    )
  }

  if (html_entity_count > 0L) {
    errors <- c(
      errors,
      sprintf("В content_text остались HTML-сущности у %s записей.", html_entity_count)
    )
  }

  empty_content_count <- sum(!nzchar(trimws(as.character(articles_df$content_text))))
  empty_topic_tags_count <- sum(!nzchar(trimws(as.character(articles_df$topic_tags))))
  short_content_count <- sum(
    nzchar(trimws(as.character(articles_df$content_text))) &
      nchar(as.character(articles_df$content_text), type = "width") < suspicious_text_length
  )

  if (empty_content_count > 0L) {
    warnings <- c(
      warnings,
      sprintf("Пустой content_text у %s записей.", empty_content_count)
    )
  }

  if (empty_topic_tags_count > 0L) {
    warnings <- c(
      warnings,
      sprintf("Пустой topic_tags у %s записей.", empty_topic_tags_count)
    )
  }

  if (short_content_count > 0L) {
    warnings <- c(
      warnings,
      sprintf(
        "Подозрительно короткий content_text у %s записей (меньше %s символов).",
        short_content_count,
        suspicious_text_length
      )
    )
  }

  dropped_invalid_rows <- as.integer(transform_stats$dropped_invalid_rows %||% 0L)
  if (dropped_invalid_rows > 0L) {
    warnings <- c(
      warnings,
      sprintf(
        "Часть записей была отброшена во время transform: %s.",
        dropped_invalid_rows
      )
    )
  }

  stats <- merge_named_lists(
    transform_stats,
    list(
      total_records_validated = total_records,
      residual_exact_duplicates = duplicate_info$exact,
      residual_key_duplicates = duplicate_info$by_key,
      invalid_published_at_count = as.integer(invalid_published_at_count),
      invalid_url_count = as.integer(url_invalid_count),
      html_tag_residue_count = as.integer(html_tag_count),
      html_entity_residue_count = as.integer(html_entity_count),
      empty_content_count = as.integer(empty_content_count),
      empty_topic_tags_count = as.integer(empty_topic_tags_count),
      short_content_count = as.integer(short_content_count)
    )
  )

  list(
    ok = length(errors) == 0L,
    failed = length(errors) > 0L,
    errors = as_message_list(errors),
    warnings = as_message_list(warnings),
    stats = stats
  )
}

assert_validation_ok <- function(report) {
  if (!isTRUE(report$ok)) {
    error_messages <- unlist(report$errors, use.names = FALSE)

    fail(
      sprintf(
        "Валидация ETL завершилась с ошибками: %s",
        paste(error_messages, collapse = " | ")
      )
    )
  }

  invisible(report)
}
