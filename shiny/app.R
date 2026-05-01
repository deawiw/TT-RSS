# shiny/app.R — Дашборд TT-RSS News Analyzer
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# ============================================================
# UI
# ============================================================
ui <- navbarPage(
  title = "TT-RSS News Analyzer",
  theme = shinytheme("cosmo"),
  
  # Вкладка 1: Обзор данных
  tabPanel("Обзор",
    fluidRow(
      column(4, valueBoxOutput("total_articles")),
      column(4, valueBoxOutput("total_sources")),
      column(4, valueBoxOutput("total_topics"))
    ),
    br(),
    fluidRow(
      column(6, plotlyOutput("articles_by_topic")),
      column(6, plotlyOutput("articles_by_source"))
    ),
    br(),
    fluidRow(
      column(12, 
        h4("Все статьи"),
        DTOutput("articles_table"),
        br(),
        downloadButton("download_csv", "Скачать CSV")
      )
    )
  ),
  
  # Вкладка 2: Анализ по темам
  tabPanel("Темы",
    sidebarLayout(
      sidebarPanel(
        selectInput("topic_select", "Выберите тему:", choices = NULL),
        br(),
        h4("Статистика:"),
        verbatimTextOutput("topic_stats")
      ),
      mainPanel(
        h4("Статьи по теме:"),
        DTOutput("topic_articles")
      )
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {
  
  # Читаем CSV из папки data-raw (путь относительно корня проекта)
  articles <- reactiveFileReader(
    intervalMillis = 60000,
    session = session,
    filePath = "../data-raw/normalized_articles.csv",
    readFunc = function(path) {
      tryCatch(
        read.csv(path, stringsAsFactors = FALSE),
        error = function(e) data.frame()
      )
    }
  )
  
  # ==================== KPI ====================
  output$total_articles <- renderValueBox({
    valueBox(nrow(articles()), "Всего статей", icon = icon("newspaper"), color = "blue")
  })
  
  output$total_sources <- renderValueBox({
    valueBox(n_distinct(articles()$source), "Источников", icon = icon("rss"), color = "green")
  })
  
  output$total_topics <- renderValueBox({
    valueBox(n_distinct(articles()$topic_tags), "Тем", icon = icon("tags"), color = "yellow")
  })
  
  # ==================== Графики ====================
  output$articles_by_topic <- renderPlotly({
    req(nrow(articles()) > 0)
    topic_counts <- articles() %>%
      count(topic_tags, sort = TRUE) %>%
      head(10)
    
    p <- ggplot(topic_counts, aes(x = reorder(topic_tags, n), y = n, fill = topic_tags)) +
      geom_col() +
      coord_flip() +
      labs(title = "Топ-10 тем", x = "", y = "Статей") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$articles_by_source <- renderPlotly({
    req(nrow(articles()) > 0)
    source_counts <- articles() %>%
      count(source, sort = TRUE) %>%
      head(10)
    
    p <- ggplot(source_counts, aes(x = reorder(source, n), y = n, fill = source)) +
      geom_col() +
      coord_flip() +
      labs(title = "Топ-10 источников", x = "", y = "Статей") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # ==================== Таблица ====================
  output$articles_table <- renderDT({
    req(nrow(articles()) > 0)
    datatable(
      articles() %>% select(source, title, topic_tags, published_at, url),
      options = list(pageLength = 25, scrollX = TRUE),
      filter = "top"
    )
  })
  
  # ==================== Скачивание ====================
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("ttrss_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(articles(), file, row.names = FALSE)
    }
  )
  
  # ==================== Вкладка "Темы" ====================
  observe({
    req(nrow(articles()) > 0)
    topics <- sort(unique(articles()$topic_tags))
    updateSelectInput(session, "topic_select", choices = topics)
  })
  
  output$topic_stats <- renderPrint({
    req(input$topic_select)
    topic_data <- articles() %>% filter(topic_tags == input$topic_select)
    cat("Статей:", nrow(topic_data), "\n")
    cat("Источников:", n_distinct(topic_data$source), "\n")
  })
  
  output$topic_articles <- renderDT({
    req(input$topic_select)
    articles() %>%
      filter(topic_tags == input$topic_select) %>%
      select(source, title, published_at, url) %>%
      datatable(options = list(pageLength = 25))
  })
}

shinyApp(ui, server)
