# shiny/app.R — Антифрод-лента для обычных пользователей
library(shiny)
library(shinythemes)
library(dplyr)
library(DT)

# ============================================================
# UI — простой и понятный интерфейс
# ============================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Заголовок
  div(
    style = "background-color: #e74c3c; padding: 20px; color: white; text-align: center;",
    h1("🛡️ Будь в курсе мошеннических схем"),
    p("Последние новости о мошенничестве, собранные автоматически")
  ),
  
  br(),
  
  # Объяснение для пользователя
  div(
    class = "alert alert-info",
    "📌 Здесь собраны новости о новых схемах обмана: фальшивые звонки, фишинг, поддельные сайты. "
    , "Мы отслеживаем это за вас — чтобы вы не попались."
  ),
  
  # Карточки с предупреждениями
  uiOutput("fraud_cards"),
  
  br(),
  
  # Простая таблица (если статей много)
  h3("📋 Все предупреждения"),
  DTOutput("fraud_table"),
  
  br(),
  
  # Футер с пояснением
  div(
    class = "text-muted",
    style = "text-align: center;",
    "Данные обновляются автоматически из новостных источников. ",
    "Обновлено: ", textOutput("last_update", inline = TRUE)
  )
)

# ============================================================
# Server — загрузка и отображение только мошеннических статей
# ============================================================
server <- function(input, output, session) {
  
  # Читаем CSV
  all_articles <- reactiveFileReader(
    intervalMillis = 60000,  # обновление каждую минуту
    session = session,
    filePath = "../data-raw/normalized_articles.csv",
    readFunc = function(path) {
      tryCatch(
        read.csv(path, stringsAsFactors = FALSE),
        error = function(e) data.frame()
      )
    }
  )
  
  # Фильтруем ТОЛЬКО мошеннические статьи
  fraud_articles <- reactive({
    req(nrow(all_articles()) > 0)
    all_articles() %>%
      filter(is_fraud_related == "TRUE" | grepl("мошенни|фишинг|обман|звонок|рассылка|схем", 
                                                  tolower(paste(title, content_text))))
  })
  
  # Карточки с последними 5 предупреждениями
  output$fraud_cards <- renderUI({
    req(nrow(fraud_articles()) > 0)
    
    latest <- fraud_articles() %>%
      arrange(desc(published_at)) %>%
      head(5)
    
    card_list <- lapply(1:nrow(latest), function(i) {
      article <- latest[i, ]
      
      div(
        class = "panel panel-danger",
        div(
          class = "panel-heading",
          h4(class = "panel-title", 
             icon("exclamation-triangle"), 
             article$title
          )
        ),
        div(
          class = "panel-body",
          p(strong("📰 Источник: "), article$source),
          p(strong("📅 Опубликовано: "), substr(article$published_at, 1, 10)),
          p(strong("🏷️ Тема: "), article$topic_tags),
          hr(),
          p(article$content_text %>% substr(1, 300) %>% paste0("...")),
          a(href = article$url, target = "_blank", 
            class = "btn btn-danger btn-sm",
            "Читать полностью →")
        )
      )
    })
    
    do.call(tagList, card_list)
  })
  
  # Таблица со всеми предупреждениями
  output$fraud_table <- renderDT({
    req(nrow(fraud_articles()) > 0)
    datatable(
      fraud_articles() %>% 
        select(Дата = published_at, Заголовок = title, Источник = source, Тема = topic_tags) %>%
        mutate(Дата = substr(Дата, 1, 10)),
      options = list(
        pageLength = 10,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json")
      ),
      rownames = FALSE
    )
  })
  
  # Время последнего обновления
  output$last_update <- renderText({
    format(Sys.time(), "%d.%m.%Y %H:%M")
  })
}

shinyApp(ui, server)
