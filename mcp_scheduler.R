# mcp_scheduler.R
# Автоматический анализ всех непроверенных статей

source("mcp_server.R")

# Обработать все статьи (лимит 10000 = все)
process_unchecked_articles(limit = 10000)
