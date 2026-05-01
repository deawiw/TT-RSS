library(shiny)
library(shinythemes)
library(dplyr)
library(DT)

ui <- fluidPage(
  theme = shinytheme("flatly"),

  div(
    style = "background-color: #e74c3c; padding: 20px; color: white; text-align: center;",
    h1("Будь в курсе мошеннических схем"),
    p("Новости о мошенничестве из новостных лент")
  ),

  br(),

  div(
    class = "alert alert-info",
    "Здесь собраны новости о схемах обмана: фальшивые звонки, фишинг, поддельные сайты. Мы отслеживаем это за вас."
  ),

  uiOutput("fraud_cards"),

  br(),

  h3("Все предупреждения"),
  DTOutput("fraud_table"),

  br(),

  p(textOutput("last_update"), style = "text-align: center; color: gray;")
)

server <- function(input, output, session) {

  all_articles <- reactiveFileReader(
    intervalMillis = 60000,
    session = session,
    filePath = "../data-raw/normalized_articles.csv",
    readFunc = function(path) {
      tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())
    }
  )

  fraud_articles <- reactive({
    req(nrow(all_articles()) > 0)
    all_articles() %>% filter(is_fraud_related == "TRUE")
  })

  output$fraud_cards <- renderUI({
    req(nrow(fraud_articles()) > 0)

    latest <- fraud_articles() %>% arrange(desc(published_at)) %>% head(5)

    card_list <- lapply(1:nrow(latest), function(i) {
      article <- latest[i, ]
      div(
        class = "panel panel-danger",
        div(class = "panel-heading", h4(icon("exclamation-triangle"), article$title)),
        div(
          class = "panel-body",
          p(strong("Источник: "), article$source),
          p(strong("Дата: "), substr(article$published_at, 1, 10)),
          p(strong("Тема: "), article$topic_tags),
          hr(),
          p(substr(article$content_text, 1, 300), "..."),
          a(href = article$url, target = "_blank", class = "btn btn-danger btn-sm", "Читать полностью")
        )
      )
    })
    do.call(tagList, card_list)
  })

  output$fraud_table <- renderDT({
    req(nrow(fraud_articles()) > 0)
    fraud_articles() %>%
      select(Дата = published_at, Заголовок = title, Источник = source, Тема = topic_tags) %>%
      mutate(Дата = substr(Дата, 1, 10)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })

  output$last_update <- renderText({
    paste("Обновлено:", format(Sys.time(), "%d.%m.%Y %H:%M"))
  })
}

shinyApp(ui, server)
