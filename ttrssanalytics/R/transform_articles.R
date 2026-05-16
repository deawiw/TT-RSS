#' @title Get required article columns
#' @description Returns the canonical column names required for normalized article data.
#' @return Character vector of required article column names.
#' @export
article_required_columns <- function() {
  c(
    "news_id",
    "source",
    "title",
    "content_text",
    "published_at",
    "url",
    "topic_tags",
    "extracted_at",
    "is_fraud_related"
  )
}

#' @title Get article output columns
#' @description Returns the canonical output column names for normalized article data.
#' @return Character vector of article output column names.
#' @export
article_output_columns <- function() {
  article_required_columns()
}

#' @title Create an empty article data frame
#' @description Builds an empty normalized article data frame with the expected columns and types.
#' @return Empty data frame with normalized article columns.
#' @export
empty_articles_df <- function() {
  data.frame(
    news_id = character(),
    source = character(),
    title = character(),
    content_text = character(),
    published_at = character(),
    url = character(),
    topic_tags = character(),
    extracted_at = character(),
    is_fraud_related = logical(),
    stringsAsFactors = FALSE
  )
}

coerce_records_list <- function(raw_records) {
  if (is.null(raw_records)) {
    return(list())
  }

  if (is.data.frame(raw_records)) {
    return(lapply(seq_len(nrow(raw_records)), function(row_index) {
      as.list(raw_records[row_index, , drop = FALSE])
    }))
  }

  if (!is.list(raw_records)) {
    return(list())
  }

  record_names <- names(raw_records)
  is_named_record <- !is.null(record_names) && any(nzchar(record_names))

  if (is_named_record) {
    return(list(raw_records))
  }

  raw_records
}

#' @title Extract the first available candidate value
#' @description Searches a record for candidate field names and returns the first non-empty value found.
#' @param record List-like record to inspect.
#' @param candidate_names Character vector of field names to try in order.
#' @return The first matching value, or NULL when none is present.
#' @export
extract_candidate_value <- function(record, candidate_names) {
  for (field_name in candidate_names) {
    value <- record[[field_name]]

    if (!is.null(value) && length(value) > 0) {
      return(value)
    }
  }

  NULL
}

coerce_character_vector <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(character())
  }

  if (is.list(value)) {
    if (length(value) == 0) {
      return(character())
    }

    flattened <- unlist(
      lapply(value, function(one_value) {
        if (is.null(one_value) || length(one_value) == 0) {
          return(character())
        }

        if (is.list(one_value)) {
          nested_value <- extract_candidate_value(
            one_value,
            c("caption", "name", "value", "title", "text", "label")
          )

          if (!is.null(nested_value)) {
            return(coerce_character_vector(nested_value))
          }

          return(jsonlite::toJSON(one_value, auto_unbox = TRUE, null = "null"))
        }

        as.character(one_value)
      }),
      use.names = FALSE
    )

    return(as.character(flattened))
  }

  as.character(value)
}

normalize_text_whitespace <- function(text) {
  if (!nzchar(text)) {
    return("")
  }

  text <- gsub("\r\n?", "\n", text, perl = TRUE)
  text <- gsub("\t+", " ", text, perl = TRUE)
  text <- gsub("[[:cntrl:]]+", " ", text, perl = TRUE)
  text <- gsub("\n+", " ", text, perl = TRUE)
  text <- gsub("[[:space:]]+", " ", text, perl = TRUE)
  trimws(text)
}

replace_regex_matches <- function(text, pattern, replacement_fun) {
  match_positions <- gregexpr(pattern, text, perl = TRUE)
  matched_values <- regmatches(text, match_positions)[[1]]

  if (length(matched_values) == 0) {
    return(text)
  }

  replacements <- vapply(matched_values, replacement_fun, character(1))
  regmatches(text, match_positions) <- list(replacements)
  text
}

decode_html_entities_once <- function(text) {
  entity_map <- c(
    amp = "&",
    lt = "<",
    gt = ">",
    quot = "\"",
    apos = "'",
    nbsp = " ",
    ensp = " ",
    emsp = " ",
    thinsp = " ",
    zwnj = "",
    zwj = "",
    lrm = "",
    rlm = "",
    euro = intToUtf8(8364),
    laquo = intToUtf8(171),
    raquo = intToUtf8(187),
    sbquo = intToUtf8(8218),
    ndash = intToUtf8(8211),
    mdash = intToUtf8(8212),
    hellip = intToUtf8(8230),
    bdquo = intToUtf8(8222),
    ldquo = intToUtf8(8220),
    rdquo = intToUtf8(8221),
    lsquo = intToUtf8(8216),
    rsquo = intToUtf8(8217),
    prime = intToUtf8(8242),
    Prime = intToUtf8(8243),
    bull = intToUtf8(8226),
    deg = intToUtf8(176),
    plusmn = intToUtf8(177),
    times = intToUtf8(215),
    divide = intToUtf8(247),
    copy = intToUtf8(169),
    reg = intToUtf8(174),
    trade = intToUtf8(8482),
    sect = intToUtf8(167),
    para = intToUtf8(182),
    middot = intToUtf8(183),
    rsaquo = intToUtf8(8250),
    lsaquo = intToUtf8(8249),
    rarr = intToUtf8(8594),
    larr = intToUtf8(8592)
  )

  decode_codepoint <- function(codepoint, original_match) {
    if (is.na(codepoint) || codepoint < 0L) {
      return(original_match)
    }

    tryCatch(
      intToUtf8(codepoint),
      error = function(e) original_match
    )
  }

  text <- replace_regex_matches(text, "&#[0-9]+;", function(match) {
    codepoint <- suppressWarnings(as.integer(sub("^&#([0-9]+);$", "\\1", match)))
    decode_codepoint(codepoint, match)
  })

  text <- replace_regex_matches(text, "&#[xX][0-9A-Fa-f]+;", function(match) {
    codepoint <- suppressWarnings(
      strtoi(sub("^&#[xX]([0-9A-Fa-f]+);$", "\\1", match), base = 16L)
    )
    decode_codepoint(codepoint, match)
  })

  text <- replace_regex_matches(text, "&[A-Za-z][A-Za-z0-9]+;", function(match) {
    entity_name <- sub("^&([A-Za-z][A-Za-z0-9]+);$", "\\1", match)
    normalized_entity_name <- tolower(entity_name)
    replacement <- if (entity_name %in% names(entity_map)) {
      entity_map[[entity_name]]
    } else if (normalized_entity_name %in% names(entity_map)) {
      entity_map[[normalized_entity_name]]
    } else {
      match
    }
    as.character(replacement)
  })

  text
}

remove_residual_html_entities <- function(text) {
  if (!nzchar(text)) {
    return("")
  }

  text <- gsub("&#[0-9]+;", " ", text, perl = TRUE)
  text <- gsub("&#[xX][0-9A-Fa-f]+;", " ", text, perl = TRUE)
  gsub("&[A-Za-z][A-Za-z0-9]+;", " ", text, perl = TRUE)
}

decode_html_entities <- function(text, max_passes = 3L) {
  if (!nzchar(text)) {
    return("")
  }

  for (pass_index in seq_len(max_passes)) {
    updated_text <- decode_html_entities_once(text)

    if (identical(updated_text, text)) {
      break
    }

    text <- updated_text
  }

  text
}

strip_html_tags <- function(text) {
  if (!nzchar(text)) {
    return("")
  }

  text <- gsub("(?is)<(script|style)[^>]*>.*?</\\1>", " ", text, perl = TRUE)
  text <- gsub(
    "(?i)</?(p|div|br|li|ul|ol|tr|td|th|h[1-6]|article|section)[^>]*>",
    " ",
    text,
    perl = TRUE
  )
  text <- gsub("(?i)<[^>]+>", " ", text, perl = TRUE)
  text
}

#' @title Clean HTML text
#' @description Converts possibly nested HTML-like values into normalized plain text.
#' @param value Value or list of values to clean.
#' @return Character scalar with normalized text.
#' @export
clean_html_text <- function(value) {
  text <- paste(coerce_character_vector(value), collapse = " ")
  text <- trim_string(text)

  if (!nzchar(text)) {
    return("")
  }

  text <- decode_html_entities(text)
  text <- strip_html_tags(text)
  text <- decode_html_entities(text)
  text <- strip_html_tags(text)
  text <- remove_residual_html_entities(text)
  text <- gsub(intToUtf8(160), " ", text, fixed = TRUE)
  normalize_text_whitespace(text)
}

extract_host_from_url <- function(url) {
  cleaned_url <- trim_string(url)

  if (!grepl("^https?://", cleaned_url, ignore.case = TRUE)) {
    return("")
  }

  host <- sub("^https?://([^/]+).*$", "\\1", cleaned_url, ignore.case = TRUE)
  host <- sub("^www\\.", "", host, ignore.case = TRUE)
  trimws(host)
}

#' @title Normalize datetime value
#' @description Converts numeric, character, or POSIX-like datetime values to UTC ISO-8601 strings.
#' @param value Datetime value to normalize.
#' @return Character scalar timestamp in UTC or NA when parsing fails.
#' @export
normalize_datetime_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }

  if (inherits(value, "POSIXt")) {
    return(format_utc_timestamp(value))
  }

  scalar_value <- trim_string(value)

  if (!nzchar(scalar_value)) {
    return(NA_character_)
  }

  if (is.numeric(value) || grepl("^[0-9]+$", scalar_value)) {
    timestamp_numeric <- suppressWarnings(as.numeric(scalar_value))

    if (!is.na(timestamp_numeric)) {
      if (timestamp_numeric > 9999999999) {
        timestamp_numeric <- timestamp_numeric / 1000
      }

      parsed_time <- as.POSIXct(timestamp_numeric, origin = "1970-01-01", tz = "UTC")
      return(format_utc_timestamp(parsed_time))
    }
  }

  parsed_time <- tryCatch(
    as.POSIXct(
      scalar_value,
      tz = "UTC",
      tryFormats = c(
        "%Y-%m-%dT%H:%M:%OSZ",
        "%Y-%m-%dT%H:%M:%OS%z",
        "%Y-%m-%d %H:%M:%OS",
        "%Y-%m-%d %H:%M:%OS %z",
        "%a, %d %b %Y %H:%M:%OS %z",
        "%Y-%m-%d"
      )
    ),
    error = function(e) NA
  )

  format_utc_timestamp(parsed_time)
}

normalize_optional_flag <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA)
  }

  if (is.logical(value)) {
    return(as.logical(value[[1]]))
  }

  numeric_value <- suppressWarnings(as.numeric(trim_string(value)))
  if (!is.na(numeric_value)) {
    return(numeric_value != 0)
  }

  text_value <- tolower(trim_string(value))

  if (text_value %in% c("true", "t", "yes", "y")) {
    return(TRUE)
  }

  if (text_value %in% c("false", "f", "no", "n")) {
    return(FALSE)
  }

  NA
}

extract_topic_tags <- function(record) {
  tag_values <- c(
    coerce_character_vector(extract_candidate_value(record, c("topic_tags"))),
    coerce_character_vector(extract_candidate_value(record, c("tags"))),
    coerce_character_vector(extract_candidate_value(record, c("labels")))
  )

  cleaned_tags <- unique(
    vapply(tag_values, clean_html_text, character(1), USE.NAMES = FALSE)
  )
  cleaned_tags <- cleaned_tags[nzchar(cleaned_tags)]

  if (length(cleaned_tags) == 0) {
    return("")
  }

  paste(cleaned_tags, collapse = " | ")
}

build_article_row <- function(record, extracted_at) {
  news_id <- trim_string(
    extract_candidate_value(record, c("news_id", "article_id", "id"))
  )
  url <- trim_string(
    extract_candidate_value(record, c("url", "link", "article_url", "orig_url", "orig_link"))
  )
  source <- clean_html_text(
    extract_candidate_value(record, c("source", "feed_title", "feed", "site_title"))
  )

  if (!nzchar(source) && nzchar(url)) {
    source <- extract_host_from_url(url)
  }

  title <- clean_html_text(
    extract_candidate_value(record, c("title", "headline", "name"))
  )
  content_text <- clean_html_text(
    extract_candidate_value(
      record,
      c("content_text", "content", "body", "summary", "excerpt", "description")
    )
  )
  published_at <- normalize_datetime_value(
    extract_candidate_value(
      record,
      c("published_at", "updated", "pub_date", "updated_at", "date", "created_at")
    )
  )
  topic_tags <- extract_topic_tags(record)
  is_fraud_related <- normalize_optional_flag(
    extract_candidate_value(record, c("is_fraud_related", "fraud_related"))
  )

  invalid_reasons <- character()

  if (!nzchar(news_id)) {
    invalid_reasons <- c(invalid_reasons, "пустой news_id")
  }

  if (!nzchar(source)) {
    invalid_reasons <- c(invalid_reasons, "пустой source")
  }

  if (!nzchar(title)) {
    invalid_reasons <- c(invalid_reasons, "пустой title")
  }

  if (!nzchar(url)) {
    invalid_reasons <- c(invalid_reasons, "пустой url")
  }

  if (is.na(published_at) || !nzchar(published_at)) {
    invalid_reasons <- c(invalid_reasons, "некорректный published_at")
  }

  if (!nzchar(extracted_at)) {
    invalid_reasons <- c(invalid_reasons, "некорректный extracted_at")
  }

  list(
    valid = length(invalid_reasons) == 0,
    invalid_reasons = invalid_reasons,
    row = list(
      news_id = news_id,
      source = source,
      title = title,
      content_text = content_text,
      published_at = published_at,
      url = url,
      topic_tags = topic_tags,
      extracted_at = extracted_at,
      is_fraud_related = is_fraud_related
    )
  )
}

assemble_articles_df <- function(article_rows) {
  if (length(article_rows) == 0) {
    return(empty_articles_df())
  }

  data.frame(
    news_id = vapply(article_rows, function(row) row$news_id, character(1)),
    source = vapply(article_rows, function(row) row$source, character(1)),
    title = vapply(article_rows, function(row) row$title, character(1)),
    content_text = vapply(article_rows, function(row) row$content_text, character(1)),
    published_at = vapply(article_rows, function(row) row$published_at, character(1)),
    url = vapply(article_rows, function(row) row$url, character(1)),
    topic_tags = vapply(article_rows, function(row) row$topic_tags, character(1)),
    extracted_at = vapply(article_rows, function(row) row$extracted_at, character(1)),
    is_fraud_related = vapply(article_rows, function(row) row$is_fraud_related, logical(1)),
    stringsAsFactors = FALSE
  )
}

deduplicate_articles_df <- function(articles_df) {
  if (nrow(articles_df) == 0) {
    return(list(
      data = articles_df,
      dropped_exact_duplicates = 0L,
      dropped_duplicates_by_url = 0L,
      dropped_duplicates_by_news_id = 0L
    ))
  }

  exact_duplicate_flags <- duplicated(articles_df)
  dropped_exact_duplicates <- sum(exact_duplicate_flags)
  articles_df <- articles_df[!exact_duplicate_flags, , drop = FALSE]

  has_url <- nzchar(articles_df$url)
  duplicate_url_flags <- duplicated(articles_df$url) & has_url
  dropped_duplicates_by_url <- sum(duplicate_url_flags)
  articles_df <- articles_df[!duplicate_url_flags, , drop = FALSE]

  blank_url_flags <- !nzchar(articles_df$url)
  duplicate_news_id_flags <- duplicated(articles_df$news_id) & blank_url_flags
  dropped_duplicates_by_news_id <- sum(duplicate_news_id_flags)
  articles_df <- articles_df[!duplicate_news_id_flags, , drop = FALSE]

  rownames(articles_df) <- NULL

  list(
    data = articles_df,
    dropped_exact_duplicates = as.integer(dropped_exact_duplicates),
    dropped_duplicates_by_url = as.integer(dropped_duplicates_by_url),
    dropped_duplicates_by_news_id = as.integer(dropped_duplicates_by_news_id)
  )
}

#' @title Transform TT-RSS headline records
#' @description Converts raw TT-RSS headline records into a normalized article data frame with transform statistics.
#' @param raw_records List or data frame of raw headline records.
#' @param extracted_at Timestamp used as the extraction time.
#' @param drop_invalid_rows Logical flag controlling whether invalid rows are removed.
#' @return List with normalized data, warnings, dropped rows, and transform statistics.
#' @export
transform_headlines_records <- function(
  raw_records,
  extracted_at = Sys.time(),
  drop_invalid_rows = TRUE
) {
  records <- coerce_records_list(raw_records)
  extracted_at <- format_utc_timestamp(extracted_at)

  article_rows <- list()
  dropped_row_messages <- character()

  for (record_index in seq_along(records)) {
    transformed <- tryCatch(
      build_article_row(records[[record_index]], extracted_at = extracted_at),
      error = function(e) {
        dropped_row_messages <<- c(
          dropped_row_messages,
          sprintf(
            "Запись %s отброшена из-за ошибки transform: %s.",
            record_index,
            conditionMessage(e)
          )
        )

        NULL
      }
    )

    if (is.null(transformed)) {
      next
    }

    if (!transformed$valid && isTRUE(drop_invalid_rows)) {
      dropped_row_messages <- c(
        dropped_row_messages,
        sprintf(
          "Запись %s отброшена: %s.",
          record_index,
          paste(transformed$invalid_reasons, collapse = ", ")
        )
      )
      next
    }

    article_rows[[length(article_rows) + 1L]] <- transformed$row
  }

  articles_df <- assemble_articles_df(article_rows)
  deduplication_result <- deduplicate_articles_df(articles_df)
  articles_df <- deduplication_result$data

  empty_content_count <- if (nrow(articles_df) == 0) 0L else sum(!nzchar(articles_df$content_text))
  empty_topic_tags_count <- if (nrow(articles_df) == 0) 0L else sum(!nzchar(articles_df$topic_tags))

  stats <- list(
    total_records_raw = as.integer(length(records)),
    total_records_after_transform = as.integer(nrow(articles_df)),
    dropped_invalid_rows = as.integer(length(dropped_row_messages)),
    dropped_exact_duplicates = deduplication_result$dropped_exact_duplicates,
    dropped_duplicates_by_url = deduplication_result$dropped_duplicates_by_url,
    dropped_duplicates_by_news_id = deduplication_result$dropped_duplicates_by_news_id,
    dropped_duplicates = as.integer(
      deduplication_result$dropped_exact_duplicates +
        deduplication_result$dropped_duplicates_by_url +
        deduplication_result$dropped_duplicates_by_news_id
    ),
    empty_content_count = as.integer(empty_content_count),
    empty_topic_tags_count = as.integer(empty_topic_tags_count)
  )

  warnings <- character()

  if (length(dropped_row_messages) > 0) {
    warnings <- c(
      warnings,
      sprintf(
        "Отброшено записей из-за некорректных обязательных значений: %s.",
        length(dropped_row_messages)
      )
    )
  }

  if (stats$dropped_duplicates > 0) {
    warnings <- c(
      warnings,
      sprintf("Удалено дубликатов на этапе transform: %s.", stats$dropped_duplicates)
    )
  }

  list(
    data = articles_df,
    warnings = warnings,
    dropped_rows = dropped_row_messages,
    stats = stats
  )
}
