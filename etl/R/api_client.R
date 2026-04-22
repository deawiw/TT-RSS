build_api_url <- function(base_url) {
  cleaned_base_url <- sub("/+$", "", trim_string(base_url))

  if (!nzchar(cleaned_base_url)) {
    fail("TTRSS_BASE_URL пустой. Проверьте .env.etl.")
  }

  paste0(cleaned_base_url, "/api/")
}

extract_api_error_message <- function(parsed_body) {
  candidates <- c(
    trim_string(parsed_body$error),
    trim_string(parsed_body$content$error),
    trim_string(parsed_body$content$message),
    trim_string(parsed_body$content)
  )

  candidates <- unique(candidates[nzchar(candidates)])

  if (length(candidates) == 0) {
    return("описание ошибки отсутствует")
  }

  candidates[[1]]
}

tt_rss_api_post <- function(api_url, op, payload = list(), session_id = NULL, timeout_sec = 60L) {
  request_body <- c(list(op = op), payload)

  if (!is.null(session_id) && nzchar(trim_string(session_id))) {
    request_body$sid <- session_id
  }

  request <- httr2::request(api_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      Accept = "application/json"
    ) |>
    httr2::req_body_raw(
      jsonlite::toJSON(
        request_body,
        auto_unbox = TRUE,
        null = "null",
        force = TRUE
      ),
      type = "application/json"
    ) |>
    httr2::req_timeout(as.numeric(timeout_sec))

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(e) {
      fail(sprintf("Сбой HTTP-запроса '%s' к TT-RSS API: %s", op, conditionMessage(e)))
    }
  )

  http_status <- httr2::resp_status(response)
  response_text <- tryCatch(
    httr2::resp_body_string(response),
    error = function(e) {
      fail(sprintf("Не удалось прочитать ответ TT-RSS API для '%s': %s", op, conditionMessage(e)))
    }
  )

  if (!nzchar(trimws(response_text))) {
    fail(sprintf("TT-RSS API вернул пустой ответ для '%s'.", op))
  }

  if (http_status >= 400L) {
    fail(
      sprintf(
        "TT-RSS API вернул HTTP %s для '%s'. Тело ответа: %s",
        http_status,
        op,
        truncate_string(response_text, width = 400L)
      )
    )
  }

  parsed_body <- tryCatch(
    jsonlite::fromJSON(response_text, simplifyVector = FALSE),
    error = function(e) {
      fail(sprintf("Не удалось разобрать JSON-ответ для '%s': %s", op, conditionMessage(e)))
    }
  )

  status_value <- parsed_body$status %||% NA_integer_
  status_int <- suppressWarnings(as.integer(status_value))

  if (is.na(status_int)) {
    fail(sprintf("TT-RSS API вернул некорректный status для '%s'.", op))
  }

  if (status_int != 0L) {
    fail(
      sprintf(
        "TT-RSS API вернул status != 0 для '%s' (status=%s): %s",
        op,
        status_int,
        extract_api_error_message(parsed_body)
      )
    )
  }

  list(
    op = op,
    status = status_int,
    raw = parsed_body,
    content = parsed_body$content %||% NULL
  )
}

tt_try_request_variants <- function(api_url, session_id, op, payload_variants, timeout_sec = 60L) {
  last_error <- NULL

  for (payload in payload_variants) {
    result <- tryCatch(
      tt_rss_api_post(
        api_url = api_url,
        op = op,
        payload = payload,
        session_id = session_id,
        timeout_sec = timeout_sec
      ),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(result)) {
      return(result)
    }
  }

  fail(
    sprintf(
      "Все варианты запроса '%s' завершились ошибкой: %s",
      op,
      conditionMessage(last_error)
    )
  )
}

as_records_list <- function(content) {
  if (is.null(content)) {
    return(list())
  }

  if (!is.list(content)) {
    return(list(list(value = content)))
  }

  content_names <- names(content)
  is_named_list <- !is.null(content_names) && any(nzchar(content_names))

  if (is_named_list) {
    return(list(content))
  }

  content
}

tt_login <- function(api_url, user, password, timeout_sec = 60L) {
  if (is_blank(user)) {
    fail("Отсутствует TTRSS_USER. Проверьте .env.etl.")
  }

  if (is_blank(password)) {
    fail("Отсутствует TTRSS_PASSWORD. Проверьте .env.etl.")
  }

  login_response <- tryCatch(
    tt_rss_api_post(
      api_url = api_url,
      op = "login",
      payload = list(user = user, password = password),
      timeout_sec = timeout_sec
    ),
    error = function(e) {
      fail(sprintf("Ошибка авторизации в API: %s", conditionMessage(e)))
    }
  )

  session_id <- trim_string(login_response$content$session_id)

  if (!nzchar(session_id)) {
    fail("Авторизация прошла, но TT-RSS не вернул session_id.")
  }

  api_level <- suppressWarnings(as.integer(login_response$content$api_level %||% NA_integer_))

  list(
    session_id = session_id,
    api_level = api_level,
    raw = login_response$raw
  )
}

tt_logout <- function(api_url, session_id, timeout_sec = 60L) {
  if (is_blank(session_id)) {
    return(invisible(FALSE))
  }

  tryCatch(
    {
      tt_rss_api_post(
        api_url = api_url,
        op = "logout",
        session_id = session_id,
        timeout_sec = timeout_sec
      )

      TRUE
    },
    error = function(e) {
      log_warn(sprintf("Logout завершился с ошибкой: %s", conditionMessage(e)))
      FALSE
    }
  )
}

tt_get_feeds <- function(api_url, session_id, include_virtual_feeds = TRUE, timeout_sec = 60L) {
  cat_id <- if (isTRUE(include_virtual_feeds)) "-4" else "-3"

  payload_variants <- list(
    list(cat_id = cat_id, unread_only = FALSE, include_nested = TRUE),
    list(cat_id = cat_id, unread_only = FALSE)
  )

  response <- tt_try_request_variants(
    api_url = api_url,
    session_id = session_id,
    op = "getFeeds",
    payload_variants = payload_variants,
    timeout_sec = timeout_sec
  )

  list(
    raw = response$raw,
    records = as_records_list(response$content)
  )
}

tt_get_headlines <- function(
  api_url,
  session_id,
  feed_id,
  limit = 50L,
  skip = 0L,
  timeout_sec = 60L
) {
  payload_variants <- list(
    list(
      feed_id = as.character(feed_id),
      limit = as.character(limit),
      skip = as.character(skip),
      is_cat = FALSE,
      view_mode = "all_articles",
      show_excerpt = TRUE
    )
  )

  response <- tt_try_request_variants(
    api_url = api_url,
    session_id = session_id,
    op = "getHeadlines",
    payload_variants = payload_variants,
    timeout_sec = timeout_sec
  )

  list(
    raw = response$raw,
    records = as_records_list(response$content)
  )
}
