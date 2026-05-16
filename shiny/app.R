
library(shiny)
library(DBI)
library(RPostgres)

# ---------- ПОДКЛЮЧЕНИЕ К БД ----------
get_fraud_articles <- function(limit = 100) {
  con <- dbConnect(
    RPostgres::Postgres(),
    host = "217.144.184.2",
    port = 5433,
    dbname = "news_analytics",
    user = "ttrss",
    password = "change_me_db_password"
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
      fa.is_fraud_scheme
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
          p("В базе данных пока нет записей с is_fraud_scheme = TRUE"),
          p("Запустите MCP обработку для обновления данных")
        )
      )
    }
    
    articles_list <- lapply(1:nrow(articles_data), function(i) {
      article <- articles_data[i, ]
      
      # Обрезаем текст для краткого содержания
      full_text <- ifelse(is.na(article$content_text), "", article$content_text)
      short_text <- substr(full_text, 1, 300)
      if (nchar(full_text) > 300) short_text <- paste0(short_text, "...")
      if (short_text == "") short_text <- "Краткое содержание отсутствует"
      
      # Форматируем дату
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
        div(class = "article-summary", short_text),
        # Кнопка "Читать полностью"
        actionButton(
          inputId = paste0("btn_", article$news_id),
          label = "📖 Читать полностью",
          class = "btn-danger btn-sm",
          style = "background-color: #dc3545; color: white; border: none; padding: 5px 15px; border-radius: 3px; cursor: pointer;"
        ),
        
        # Кнопка со ссылкой на сайт (если есть url)
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
    })
    
    div(
      style = "max-width: 900px; margin: 0 auto; padding: 20px;",
      articles_list
    )
  })
  
  # Обработчик кнопки "Читать полностью"
  observe({
    if (nrow(articles_data) > 0) {
      for (i in 1:nrow(articles_data)) {
        local({
          my_i <- i
          btn_id <- paste0("btn_", articles_data$news_id[my_i])
          
          observeEvent(input[[btn_id]], {
            article <- articles_data[my_i, ]
            
            # Проверяем, есть ли контент
            has_content <- !is.na(article$content_text) && nchar(article$content_text) > 10
            
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
                
                # Если есть контент - показываем его
                if (has_content) {
                  p(style = "white-space: pre-wrap;", article$content_text)
                } else {
                  # Если контента нет - показываем сообщение
                  p(style = "color: #dc3545;", "⚠️ Полный текст статьи отсутствует в базе данных.")
                },
                
                hr(),
                
                # Ссылка на оригинал (всегда показываем, если есть)
                if (!is.na(article$url) && nchar(article$url) > 5) {
                  div(
                    style = "text-align: center;",
                    tags$a(
                      href = article$url,
                      target = "_blank",
                      style = "background-color: #007bff; color: white; padding: 10px 20px; border-radius: 5px; text-decoration: none; display: inline-block;",
                      "🔗 Читать на сайте источника"
                    )
                  )
                } else {
                  NULL
                },
                
                hr(),
                div(
                  style = "background-color: #fff0f0; padding: 15px; border-left: 4px solid #dc3545; border-radius: 5px; margin-top: 15px;",
                  h4(icon("robot"), " Совет от ИИ:"),
                  p("⚠️ Эта статья была отмечена LLM как потенциально связанная с мошенничеством. Будьте осторожны и проверяйте информацию из официальных источников.")
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