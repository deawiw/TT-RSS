# ttrssanalytics
R пакет для ETL-обработки новостей из TT-RSS с автоматической детекцией мошенничества.

## Установка

```r
devtools::install("ttrssanalytics")
```

## Основные функции

| Функция | Описание |
|---------|----------|
| `apply_fraud_keyword_filter()` | Фильтрация мошеннических статей по ключевым словам |
| `tt_login()` | Авторизация в TT-RSS API |
| `tt_get_headlines()` | Получение заголовков статей из ленты |
| `tt_get_feeds()` | Получение списка RSS-лент |
| `connect_postgres()` | Подключение к PostgreSQL |
| `load_articles_to_db()` | Загрузка статей в базу данных |
| `validate_articles_df()` | Валидация данных перед загрузкой |

## Трансформация и очистка данных

| Функция | Описание |
|---------|----------|
| `transform_headlines_records()` | Трансформация сырых headline-записей |
| `clean_html_text()` | Очистка HTML-тегов и сущностей |
| `normalize_datetime_value()` | Нормализация дат в UTC формат |
| `truncate_string()` | Обрезание длинных строк |
| `trim_string()` | Удаление лишних пробелов |

## Логирование

| Функция | Описание |
|---------|----------|
| `log_info()` | Информационное сообщение |
| `log_warn()` | Предупреждение |
| `log_error()` | Сообщение об ошибке |

## Работа с файлами

| Функция | Описание |
|---------|----------|
| `write_csv_utf8()` | Сохранение датафрейма в CSV (UTF-8) |
| `write_json_pretty()` | Сохранение в JSON с форматированием |
| `ensure_dir()` | Создание директории (если не существует) |

## Пример использования

```r
library(ttrssanalytics)

articles <- read.csv("data-raw/normalized_articles.csv")
result <- apply_fraud_keyword_filter(articles)
table(result$is_fraud_related)

fraud_only <- result[result$is_fraud_related == TRUE, ]
write.csv(fraud_only, "fraud_articles.csv", row.names = FALSE)
```

## Структура пакета

```
ttrssanalytics/
├── R/                 # R скрипты с функциями
├── inst/config/       # Конфигурационные файлы
├── data/              # Встроенные данные
├── man/               # Документация
└── vignettes/         # Статьи
```

