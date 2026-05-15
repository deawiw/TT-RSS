# mcp_server.R
# Fraud News MCP Server (R + plumber)
# Использует Cloud.ru API (Qwen3-Next-80B-A3B-Instruct)

library(plumber)
library(httr2)
library(jsonlite)
library(DBI)
library(RPostgres)
library(dotenv)

# Загружаем переменные из .env
dotenv::load_dot_env(".env")

API_KEY <- Sys.getenv("CLOUD_RU_API_KEY")
if (is.null(API_KEY) || nchar(API_KEY) == 0) {
  stop("CLOUD_RU_API_KEY not found in .env file!")
}

MODEL_NAME <- "Qwen/Qwen3-Next-80B-A3B-Instruct"
API_URL <- "https://foundation-models.api.cloud.ru/v1/chat/completions"

system_prompt <- paste(
  "You are a cybersecurity expert. Analyze the news article and determine whether it describes",
  "a specific fraud scheme, social engineering method, phishing, or cyber fraud.",
  "If it does NOT describe a specific scheme (e.g., just statistics, court case, technology review), return:",
  '{"is_fraud_scheme": false}',
  "If it DOES describe a new or existing fraud scheme, return:",
  '{"is_fraud_scheme": true, "summary": "Brief description of the scheme (2-3 sentences).", "advice": ["Practical tip 1", "Practical tip 2", "Practical tip 3"]}',
  "Answer ONLY in JSON format, no other text.",
  sep = "\n"
)

# Database connection
get_db_connection <- function() {
  tryCatch({
    dbConnect(Postgres(),
              host = Sys.getenv("DB_HOST", "localhost"),
              port = Sys.getenv("DB_PORT", "5433"),
              dbname = Sys.getenv("DB_NAME", "news_analytics"),
              user = Sys.getenv("DB_USER", "change_me_db_user"),
              password = Sys.getenv("DB_PASSWORD", "change_me_db_password"))
  }, error = function(e) {
    message("DB connection error: ", e$message)
    return(NULL)
  })
}

# Analyze article using Cloud.ru API
analyze_article <- function(title, content) {
  if (is.null(title) && is.null(content)) return(NULL)
  
  body <- list(
    model = MODEL_NAME,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = paste0("Title: ", title, "\n\nText: ", content))
    ),
    temperature = 0.3,
    max_tokens = 500
  )
  
  resp <- tryCatch({
    request(API_URL) |>
      req_headers(
        Authorization = paste("Bearer", API_KEY),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(body) |>
      req_timeout(60) |>
      req_perform()
  }, error = function(e) {
    message("API request error: ", e$message)
    return(NULL)
  })
  
  if (is.null(resp)) return(NULL)
  
  parsed <- fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  answer <- parsed$choices[[1]]$message$content
  
  # Extract JSON object from response
  json_start <- regexpr("\\{", answer)[1]
  json_end <- regexpr("\\}[^}]*$", answer)[1]
  if (json_start > 0 && json_end > 0) {
    json_str <- substr(answer, json_start, json_end)
    tryCatch(fromJSON(json_str, simplifyVector = FALSE),
             error = function(e) {
               message("JSON parse error: ", e$message)
               return(NULL)
             })
  } else {
    message("No JSON found in response: ", answer)
    NULL
  }
}

# Handler: process unchecked articles
process_unchecked_articles <- function(limit = 5) {
  message("Starting analysis of up to ", limit, " unchecked articles")
  
  conn <- get_db_connection()
  if (is.null(conn)) return(list(error = "Database connection failed"))
  on.exit(dbDisconnect(conn))
  
  query <- sprintf("
    SELECT a.news_id, a.title, a.content_text, a.url
    FROM articles a
    WHERE a.is_fraud_related = TRUE
      AND NOT EXISTS (
        SELECT 1 FROM fraud_articles f WHERE f.news_id = a.news_id
      )
    LIMIT %s
  ", limit)
  
  unchecked <- dbGetQuery(conn, query)
  if (nrow(unchecked) == 0) return(list(message = "All articles already checked"))
  
  analyzed <- 0
  for (i in seq_len(nrow(unchecked))) {
    article <- unchecked[i, ]
    message("Analyzing article ID: ", article$news_id)
    
    result <- analyze_article(article$title, article$content_text)
    if (is.null(result)) {
      message("Skipping article ID: ", article$news_id)
      next
    }
    
    is_fraud <- isTRUE(result$is_fraud_scheme)
    summary <- ifelse(is.null(result$summary), "", result$summary)
    advice <- paste(unlist(result$advice), collapse = "\n")
    
    dbExecute(conn, sprintf("
      INSERT INTO fraud_articles (news_id, theme_category, selection_method, is_fraud_scheme, summary, advice)
      VALUES (%s, 'social_engineering', 'LLM:Qwen', %s, '%s', '%s')
      ON CONFLICT (news_id) DO NOTHING
    ", article$news_id,
       ifelse(is_fraud, "TRUE", "FALSE"),
       gsub("'", "''", summary),
       gsub("'", "''", advice)))
    
    analyzed <- analyzed + 1
    message("Successfully processed article ID: ", article$news_id)
  }
  
  list(message = sprintf("Analysis complete. Processed %d new articles.", analyzed))
}

# Handler: get fraud articles
get_fraud_articles <- function(limit = 10) {
  conn <- get_db_connection()
  if (is.null(conn)) return(list(error = "Database connection failed"))
  on.exit(dbDisconnect(conn))
  
  articles <- dbGetQuery(conn, sprintf("
    SELECT f.news_id, f.summary, f.advice, a.title, a.url
    FROM fraud_articles f
    JOIN articles a ON f.news_id = a.news_id
    WHERE f.is_fraud_scheme = TRUE
    ORDER BY f.selected_at DESC
    LIMIT %s
  ", limit))
  
  if (nrow(articles) == 0) return(list(message = "No fraud articles found"))
  
  list(
    count = nrow(articles),
    articles = lapply(seq_len(nrow(articles)), function(i) {
      list(
        title = articles$title[i],
        summary = articles$summary[i],
        url = articles$url[i],
        advice = strsplit(articles$advice[i], "\n")[[1]]
      )
    })
  )
}

# Plumber API
#* @apiTitle Fraud News MCP Server

#* Process unchecked articles
#* @param limit Number of articles to analyze
#* @get /process_unchecked_articles
function(limit = 5) {
  process_unchecked_articles(as.integer(limit))
}

#* Get list of fraud articles
#* @param limit Number of articles to return
#* @get /get_fraud_articles
function(limit = 10) {
  get_fraud_articles(as.integer(limit))
}
