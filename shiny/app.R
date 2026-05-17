library(shiny)
library(DBI)
library(RPostgres)
library(httr)

# ---------- ПОДКЛЮЧЕНИЕ К БД ----------
get_fraud_articles <- function(limit = 100) {
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST", "db"),
    port = as.integer(Sys.getenv("DB_PORT", "5432")),
    dbname = Sys.getenv("DB_NAME", "news_analytics"),
    user = Sys.getenv("DB_USER", "ttrss"),
    password = Sys.getenv("DB_PASSWORD", "change_me_db_password")
  )
  on.exit(dbDisconnect(con))

  df <- dbGetQuery(con, sprintf("
    SELECT
      fa.news_id,
      a.title,
      a.source,
      a.published_at,
      a.content_text,
      a.url,
      fa.theme_category,
      fa.selection_method,
      fa.is_fraud_scheme,
      fa.summary,
      fa.advice
    FROM fraud_articles fa
    JOIN articles a ON fa.news_id = a.news_id
    WHERE fa.is_fraud_scheme = TRUE
    ORDER BY fa.selected_at DESC
    LIMIT %d
  ", limit))

  if (nrow(df) > 0) {
    df$published_at <- as.POSIXct(df$published_at, tz = "UTC")
  }

  return(df)
}

articles_data <- get_fraud_articles(limit = 100)

# Функция для обрезания текста с троеточием (для всех статей)
truncate_text <- function(text, max_length = 500) {
  if (is.na(text) || text == "") return("Текст статьи отсутствует")
  if (nchar(text) <= max_length) return(paste0(text, "..."))
  paste0(substr(text, 1, max_length), "...")
}

# ---------- UI ----------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: Arial, sans-serif; background-color: #f5f5f5; }
      .article-card {
        background-color: #fff3f0;
        border-left: 4px solid #dc3545;
        margin-bottom: 20px;
        padding: 15px;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .article-title {
        color: #dc3545;
        margin-top: 0;
        margin-bottom: 10px;
        font-size: 20px;
      }
      .article-meta {
        color: #666;
        font-size: 12px;
        margin-bottom: 10px;
      }
      .fraud-badge {
        background-color: #dc3545;
        color: white;
        padding: 2px 8px;
        border-radius: 12px;
        font-size: 11px;
        display: inline-block;
        margin-left: 10px;
      }
      .btn-link-custom {
        background-color: #6c757d;
        color: white;
        border: none;
        padding: 5px 15px;
        border-radius: 3px;
        text-decoration: none;
        display: inline-block;
        margin-left: 10px;
      }
      .btn-link-custom:hover {
        background-color: #5a6268;
        color: white;
        text-decoration: none;
      }
      .article-summary {
        color: #333;
        font-size: 14px;
        line-height: 1.5;
        margin-bottom: 0px;
      }
      .button-group {
        margin-top: 5px;
      }
    "))
  ),

  titlePanel(
    h1("⚠️ Лента новостей о мошенничестве",
       style = "color: #dc3545; border-bottom: 2px solid #dc3545; padding-bottom: 10px;")
  ),

  fluidRow(
    column(12,
           p("Показаны статьи, которые система (LLM) определила как связанные с мошенничеством.",
             style = "color: #666; margin-bottom: 20px;")
    )
  ),
  fluidRow(
    column(12,
      wellPanel(
        h4("💬 Задайте вопрос о мошеннических схемах"),
        textInput("user_question", NULL, 
                  placeholder = "Например: как защититься от фишинга?"),
        actionButton("ask_btn", "Спросить", class = "btn-danger"),
        br(), br(),
        h4("Ответ ИИ:"),
        textOutput("mcp_answer")
      )
    )
  ),
  uiOutput("articles_list")
)

# ---------- SERVER ----------
server <- function(input, output, session) {

  output$articles_list <- renderUI({
    if (nrow(articles_data) == 0) {
      return(
        div(
          style = "text-align: center; margin-top: 100px;",
          h3("✅ Нет новых статей о мошенничестве"),
          p("В базе данных пока нет записей с is_fraud_scheme = TRUE")
        )
      )
    }

    articles_list <- lapply(1:nrow(articles_data), function(i) {
      article <- articles_data[i, ]

      # На главной странице показываем summary
      summary_text <- ifelse(!is.na(article$summary) && nchar(article$summary) > 0,
                             article$summary,
                             "Краткое содержание отсутствует")

      pub_date <- format(article$published_at, "%d.%m.%Y %H:%M")

      div(
        class = "article-card",
        div(
          h3(class = "article-title",
             icon("warning"), " ", article$title,
             span(class = "fraud-badge", icon("exclamation-triangle"), " Мошенничество")
          )
        ),
        div(class = "article-meta",
            icon("newspaper"), " ", article$source,
            " | ", icon("calendar"), " ", pub_date,
            " | ", icon("brain"), " ", article$selection_method,
            " | ", icon("tag"), " ", article$theme_category
        ),
        div(class = "article-summary", summary_text),
        div(class = "button-group",
            actionButton(
              inputId = paste0("btn_", article$news_id),
              label = "📖 Читать статью",
              class = "btn-danger btn-sm",
              style = "background-color: #dc3545; color: white; border: none; padding: 5px 15px; border-radius: 3px; cursor: pointer;"
            ),
            if (!is.na(article$url) && nchar(article$url) > 5) {
              tags$a(
                href = article$url,
                target = "_blank",
                class = "btn-link-custom",
                "🔗 Открыть на сайте"
              )
            } else {
              NULL
            }
        )
      )
    })

    div(
      style = "max-width: 900px; margin: 0 auto; padding: 20px;",
      articles_list
    )
  })
  observeEvent(input$ask_btn, {
    req(input$user_question)
    
    response <- httr::GET(
      url = "http://mcp:8000/ask",
      query = list(question = input$user_question)
    )
    
    if (httr::status_code(response) == 200) {
      result <- httr::content(response, as = "text", encoding = "UTF-8")
      parsed <- jsonlite::fromJSON(result)
      output$mcp_answer <- renderText(parsed$answer[[1]])
    } else {
      output$mcp_answer <- renderText("Не удалось получить ответ. Попробуйте позже.")
    }
  })
  
  # Обработчик кнопки "Читать статью"
  observe({
    if (nrow(articles_data) > 0) {
      for (i in 1:nrow(articles_data)) {
        local({
          my_i <- i
          btn_id <- paste0("btn_", articles_data$news_id[my_i])

          observeEvent(input[[btn_id]], {
            article <- articles_data[my_i, ]

            # Обрезаем текст статьи с троеточием (для всех статей)
            full_text <- ifelse(is.na(article$content_text) || article$content_text == "",
                                "Текст статьи отсутствует",
                                article$content_text)

            # Обрезаем до 500 символов и добавляем троеточие всегда
            truncated_text <- truncate_text(full_text, 500)

            # Совет от ИИ
            advice_text <- ifelse(!is.na(article$advice) && nchar(article$advice) > 0,
                                  article$advice,
                                  "⚠️ Будьте осторожны и проверяйте информацию из официальных источников.")

            showModal(modalDialog(
              title = div(icon("warning"), " ", article$title),
              size = "l",
              easyClose = TRUE,
              footer = modalButton("Закрыть"),
              div(
                style = "font-size: 16px; line-height: 1.6;",
                p(style = "color: #dc3545; margin-bottom: 20px;",
                  icon("newspaper"), " ", article$source,
                  " | ", icon("calendar"), " ", format(article$published_at, "%d.%m.%Y %H:%M"),
                  " | ", icon("brain"), " ", article$selection_method,
                  " | ", icon("tag"), " ", article$theme_category,
                  span(style = "background-color: #dc3545; color: white; padding: 2px 8px; border-radius: 12px; margin-left: 10px;",
                       "Мошенничество")
                ),
                hr(),

                # Текст статьи с троеточием
                h5(icon("file-text"), " Текст статьи:"),
                p(style = "white-space: pre-wrap; line-height: 1.8;", truncated_text),

                p(style = "color: #6c757d; font-style: italic; margin-top: 10px;",
                  "📄 Полная версия доступна на сайте источника."),

                hr(),

                # Ссылка на оригинал
                if (!is.na(article$url) && nchar(article$url) > 5) {
                  div(
                    style = "text-align: center; margin: 15px 0;",
                    tags$a(
                      href = article$url,
                      target = "_blank",
                      style = "background-color: #007bff; color: white; padding: 12px 30px; border-radius: 5px; text-decoration: none; display: inline-block; font-size: 16px;",
                      "🔗 Читать полную версию на сайте источника"
                    )
                  )
                } else {
                  NULL
                },

                hr(),
                div(
                  style = "background-color: #fff0f0; padding: 15px; border-left: 4px solid #dc3545; border-radius: 5px; margin-top: 15px;",
                  h4(icon("robot"), " Совет от ИИ:"),
                  p(advice_text)
                )
              )
            ))
          })
        })
      }
    }
  })
}

# ---------- ЗАПУСК ----------
shinyApp(ui = ui, server = server)
