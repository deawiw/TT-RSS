# Обычные ETL-параметры без секретов.
# Файл versioned и отделен от Docker .env и от ETL-секретов в .env.etl.
# output_dir используется только для служебных ETL-артефактов и диагностики.
# Финальный нормализованный датасет сохраняется в data-raw/ на уровне проекта.

etl_config <- list(
  headlines_limit = 200L,
  max_articles_per_feed = 1000L,
  include_virtual_feeds = TRUE,
  sample_full_articles_per_feed = 200L,
  request_pause_sec = 0.15,
  article_batch_size = 50L,
  timeout_sec = 60L,
  output_dir = "etl/output"
)
