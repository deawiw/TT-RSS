library(shiny)
library(DBI)
library(RPostgres)
library(httr)

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

truncate_text <- function(text, max_length = 500) {
  if (is.na(text) || text == "") return("Текст статьи отсутствует")
  if (nchar(text) <= max_length) return(paste0(text, "..."))
  paste0(substr(text, 1, max_length), "...")
}

library(ggplot2)
library(dplyr)
library(DT)
library(shinyWidgets)
library(lubridate)
library(plotly)
library(wordcloud2)
library(shinycssloaders)

get_time_series_data <- function(df) {
  if (nrow(df) == 0) return(data.frame())
  df %>%
    mutate(date = as.Date(published_at)) %>%
    group_by(date) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(date)
}

get_category_stats <- function(df) {
  if (nrow(df) == 0) return(data.frame())
  df %>%
    group_by(theme_category) %>%
    summarise(
      count = n(),
      percentage = round(n() / nrow(df) * 100, 1)
    ) %>%
    arrange(desc(count))
}

prepare_wordcloud_data <- function(df) {
  if (nrow(df) == 0) return(data.frame(word = character(), freq = numeric()))
  all_text <- paste(df$summary[!is.na(df$summary)], collapse = " ")
  words <- unlist(strsplit(tolower(all_text), "\\W+"))
  words <- words[nchar(words) > 3]
  word_freq <- as.data.frame(table(words))
  colnames(word_freq) <- c("word", "freq")
  word_freq <- word_freq[order(-word_freq$freq), ]
  head(word_freq, 50)
}

additional_css <- HTML("
  .dashboard-section {
    background-color: white;
    padding: 20px;
    border-radius: 10px;
    margin-bottom: 20px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  .kpi-card {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 20px;
    border-radius: 10px;
    margin-bottom: 20px;
    text-align: center;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    transition: transform 0.3s;
  }
  .kpi-card:hover { transform: translateY(-5px); }
  .kpi-number { font-size: 36px; font-weight: bold; margin: 10px 0; }
  .kpi-label { font-size: 14px; opacity: 0.9; }
  @media (max-width: 768px) { .kpi-number { font-size: 24px; } }
")

ui <- navbarPage(
  title = div(icon("shield-alt"), " Fraud Monitor System"),
  id = "navbar",
  collapsible = TRUE,

  tabPanel(
    title = tagList(icon("home"), " Главная"),
    value = "home",
    fluidPage(
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
        ")),
        tags$style(additional_css)
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
  ),

  tabPanel(
    title = tagList(icon("chart-line"), " Аналитика"),
    value = "dashboard",
    fluidPage(
      br(),
      fluidRow(
        valueBoxOutput("total_articles_box"),
        valueBoxOutput("unique_categories_box"),
        valueBoxOutput("unique_sources_box"),
        valueBoxOutput("avg_articles_per_day_box")
      ),
      fluidRow(
        column(6,
               div(class = "dashboard-section",
                   h4(icon("calendar"), " Временной тренд"),
                   withSpinner(plotlyOutput("time_trend_plot", height = "400px"))
               )
        ),
        column(6,
               div(class = "dashboard-section",
                   h4(icon("chart-pie"), " Распределение по категориям"),
                   withSpinner(plotlyOutput("category_pie_plot", height = "400px"))
               )
        )
      ),
      fluidRow(
        column(6,
               div(class = "dashboard-section",
                   h4(icon("chart-bar"), " Топ-10 категорий"),
                   withSpinner(plotOutput("category_bar_plot", height = "400px"))
               )
        ),
        column(6,
               div(class = "dashboard-section",
                   h4(icon("cloud"), " Облако ключевых слов"),
                   withSpinner(wordcloud2Output("wordcloud_plot", height = "400px"))
               )
        )
      ),
      fluidRow(
        column(12,
               div(class = "dashboard-section",
                   h4(icon("table"), " Детальная таблица статей"),
                   withSpinner(DTOutput("articles_table"))
               )
        )
      )
    )
  ),

  tabPanel(
    title = tagList(icon("download"), " Экспорт"),
    value = "export",
    fluidPage(
      br(),
      fluidRow(
        column(12,
               div(class = "dashboard-section",
                   h3(icon("file-csv"), " Экспорт данных о мошенничестве"),
                   hr(),
                   fluidRow(
                     column(6,
                            h4("Фильтры перед экспортом:"),
                            dateRangeInput("export_date_range",
                                           "Диапазон дат:",
                                           start = if(nrow(articles_data)>0) min(articles_data$published_at) else Sys.Date()-7,
                                           end = if(nrow(articles_data)>0) max(articles_data$published_at) else Sys.Date()),
                            selectInput("export_category",
                                        "Категория:",
                                        choices = c("Все", if(nrow(articles_data)>0) unique(articles_data$theme_category) else "Нет данных"),
                                        selected = "Все"),
                            selectInput("export_source",
                                        "Источник:",
                                        choices = c("Все", if(nrow(articles_data)>0) unique(articles_data$source) else "Нет данных"),
                                        selected = "Все"),
                            br(),
                            downloadButton("download_csv", "📥 Скачать CSV", class = "btn-success btn-lg"),
                            br(), br(),
                            downloadButton("download_report", "📄 Скачать отчёт (TXT)", class = "btn-info btn-lg")
                     ),
                     column(6,
                            h4("Статистика экспорта:"),
                            verbatimTextOutput("export_stats"),
                            br(),
                            h4("Пример данных:"),
                            withSpinner(DTOutput("export_preview"))
                     )
                   )
               )
        )
      )
    )
  ),

  tabPanel(
    title = tagList(icon("question-circle"), " Помощь"),
    value = "help",
    fluidPage(
      br(),
      fluidRow(
        column(12,
               div(class = "dashboard-section",
                   h3(icon("info-circle"), " О системе мониторинга мошенничества"),
                   hr(),
                   h4("📌 Как пользоваться системой:"),
                   tags$ul(
                     tags$li("На вкладке «Главная» вы видите ленту новостей о мошенничестве"),
                     tags$li("Можете задать вопрос ИИ о схемах мошенничества"),
                     tags$li("На вкладке «Аналитика» представлена статистика и графики"),
                     tags$li("На вкладке «Экспорт» можно выгрузить данные в CSV/TXT"),
                     tags$li("Карточки статей содержат краткое содержание и советы от ИИ")
                   ),
                   br(),
                   h4("🤖 Как работает ИИ-анализ:"),
                   tags$ul(
                     tags$li("LLM-модель анализирует каждую новость"),
                     tags$li("Определяет категорию мошенничества"),
                     tags$li("Генерирует краткое содержание (summary)"),
                     tags$li("Даёт практические советы по защите (advice)")
                   ),
                   br(),
                   h4("⚠️ Важное предупреждение:"),
                   div(
                     style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px;",
                     icon("exclamation-triangle"),
                     " Информация предоставляется в ознакомительных целях.
                     Всегда проверяйте информацию из официальных источников
                     и при подозрении на мошенничество обращайтесь в правоохранительные органы."
                   ),
                   br(),
                   h4("📞 Полезные контакты:"),
                   tags$ul(
                     tags$li("Горячая линия МВД по борьбе с мошенничеством: 102"),
                     tags$li("Банк России (кибербезопасность): 8-800-300-30-00"),
                     tags$li("Платформа «Мошеловка»: moshelovka.ru")
                   )
               )
        )
      )
    )
  )
)

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

  observe({
    if (nrow(articles_data) > 0) {
      for (i in 1:nrow(articles_data)) {
        local({
          my_i <- i
          btn_id <- paste0("btn_", articles_data$news_id[my_i])

          observeEvent(input[[btn_id]], {
            article <- articles_data[my_i, ]

            full_text <- ifelse(is.na(article$content_text) || article$content_text == "",
                                "Текст статьи отсутствует",
                                article$content_text)

            truncated_text <- truncate_text(full_text, 500)

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
                h5(icon("file-text"), " Текст статьи:"),
                p(style = "white-space: pre-wrap; line-height: 1.8;", truncated_text),
                p(style = "color: #6c757d; font-style: italic; margin-top: 10px;",
                  "📄 Полная версия доступна на сайте источника."),
                hr(),
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

  export_data <- reactive({
    df <- articles_data
    if (nrow(df) == 0) return(df)
    df <- df %>%
      filter(published_at >= input$export_date_range[1],
             published_at <= input$export_date_range[2])
    if (input$export_category != "Все") {
      df <- df %>% filter(theme_category == input$export_category)
    }
    if (input$export_source != "Все") {
      df <- df %>% filter(source == input$export_source)
    }
    df %>% select(title, source, published_at, theme_category, selection_method, summary, advice)
  })

  output$export_stats <- renderPrint({
    df <- export_data()
    cat("Количество статей для экспорта:", nrow(df), "\n")
    if (nrow(df) > 0) {
      cat("Категорий:", length(unique(df$theme_category)), "\n")
      cat("Источников:", length(unique(df$source)), "\n")
      cat("Период:", format(min(df$published_at), "%d.%m.%Y"), "-",
          format(max(df$published_at), "%d.%m.%Y"))
    }
  })

  output$export_preview <- renderDT({
    req(nrow(export_data()) > 0)
    datatable(export_data()[1:min(10, nrow(export_data())), ],
              options = list(scrollX = TRUE, pageLength = 5),
              class = "display nowrap")
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("fraud_articles_", Sys.Date(), ".csv"),
    content = function(file) write.csv(export_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  output$download_report <- downloadHandler(
    filename = function() paste0("fraud_report_", Sys.Date(), ".txt"),
    content = function(file) {
      df <- export_data()
      sink(file)
      cat("ОТЧЁТ О МОШЕННИЧЕСТВЕ\n=====================\n\n")
      cat("Дата генерации:", Sys.time(), "\n\n")
      cat("Всего статей:", nrow(df), "\n\n")
      cat("ДЕТАЛИЗАЦИЯ:\n------------\n\n")
      for (i in 1:min(nrow(df), 50)) {
        cat(i, ". ", df$title[i], "\n")
        cat("   Источник:", df$source[i], "\n")
        cat("   Категория:", df$theme_category[i], "\n")
        if (!is.na(df$advice[i])) cat("   Совет ИИ:", substr(df$advice[i], 1, 100), "...\n\n")
      }
      sink()
    }
  )

  output$total_articles_box <- renderValueBox({
    valueBox(value = nrow(articles_data),
             subtitle = "Всего статей о мошенничестве",
             icon = icon("newspaper"), color = "red")
  })

  output$unique_categories_box <- renderValueBox({
    valueBox(value = length(unique(articles_data$theme_category)),
             subtitle = "Категорий мошенничества",
             icon = icon("tags"), color = "purple")
  })

  output$unique_sources_box <- renderValueBox({
    valueBox(value = length(unique(articles_data$source)),
             subtitle = "Источников новостей",
             icon = icon("globe"), color = "blue")
  })

  output$avg_articles_per_day_box <- renderValueBox({
    if (nrow(articles_data) > 1) {
      days_span <- as.numeric(difftime(max(articles_data$published_at),
                                       min(articles_data$published_at),
                                       units = "days"))
      avg <- ifelse(days_span > 0, round(nrow(articles_data) / days_span, 1), nrow(articles_data))
    } else {
      avg <- nrow(articles_data)
    }
    valueBox(value = avg,
             subtitle = "Статей в день (в среднем)",
             icon = icon("chart-line"), color = "green")
  })

  output$time_trend_plot <- renderPlotly({
    req(nrow(articles_data) > 0)
    time_data <- get_time_series_data(articles_data)
    p <- ggplot(time_data, aes(x = date, y = count)) +
      geom_line(color = "#dc3545", size = 1.5) +
      geom_point(color = "#dc3545", size = 2) +
      labs(x = "Дата", y = "Количество статей",
           title = "Динамика публикаций о мошенничестве") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$category_pie_plot <- renderPlotly({
    req(nrow(articles_data) > 0)
    cat_stats <- get_category_stats(articles_data)
    plot_ly(cat_stats, labels = ~theme_category, values = ~count, type = "pie",
            textinfo = "label+percent", hoverinfo = "text",
            text = ~paste(theme_category, "<br>", count, "статей (", percentage, "%)"),
            marker = list(colors = c("#dc3545", "#fd7e14", "#ffc107", "#28a745", "#17a2b8", "#6f42c1"))) %>%
      layout(title = "Распределение статей по категориям")
  })

  output$category_bar_plot <- renderPlot({
    req(nrow(articles_data) > 0)
    cat_stats <- head(get_category_stats(articles_data), 10)
    ggplot(cat_stats, aes(x = reorder(theme_category, count), y = count, fill = theme_category)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      labs(x = "Категория", y = "Количество статей",
           title = "Топ-10 категорий мошенничества") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_fill_manual(values = c("#dc3545", "#fd7e14", "#ffc107", "#28a745", "#17a2b8", "#6f42c1"))
  })

  output$wordcloud_plot <- renderWordcloud2({
    req(nrow(articles_data) > 0)
    word_data <- prepare_wordcloud_data(articles_data)
    if (nrow(word_data) > 0) wordcloud2(word_data, size = 0.8, shape = 'circle')
  })

  output$articles_table <- renderDT({
    req(nrow(articles_data) > 0)
    df_display <- articles_data %>%
      select(title, source, published_at, theme_category, selection_method, summary) %>%
      mutate(published_at = format(published_at, "%d.%m.%Y %H:%M"))
    datatable(df_display,
              options = list(pageLength = 10, scrollX = TRUE,
                             order = list(list(2, 'desc')),
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json')),
              class = "display nowrap", rownames = FALSE, filter = "top",
              caption = htmltools::tags$caption(
                style = "caption-side: top; text-align: center; color: #dc3545; font-size: 16px;",
                "📋 Полный список статей о мошенничестве"
              ))
  })

}

shinyApp(ui = ui, server = server)
