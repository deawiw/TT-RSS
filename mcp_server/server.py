# server.py
import os
import json
import asyncio
import logging
from typing import Optional

import requests
from dotenv import load_dotenv
from mcp.server import Server
from mcp.server import stdio
from mcp.types import Tool, TextContent
import psycopg2
from psycopg2.extras import RealDictCursor

# --- 1. Настройка логирования и загрузка переменных окружения ---
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
load_dotenv()

DEEPSEEK_API_KEY = os.getenv("DEEPSEEK_API_KEY")
if not DEEPSEEK_API_KEY:
    logging.error("❌ Не найден DEEPSEEK_API_KEY. Пожалуйста, добавьте его в файл .env")
    exit(1)

# --- 2. Функция для подключения к базе данных ---
def get_db_connection():
    """Создает новое подключение к PostgreSQL."""
    try:
        conn = psycopg2.connect(
            host=os.getenv("DB_HOST", "localhost"),
            port=os.getenv("DB_PORT", "5432"),
            dbname=os.getenv("DB_NAME", "ttrss"),
            user=os.getenv("DB_USER", "ttrss"),
            password=os.getenv("DB_PASSWORD", "password")
        )
        return conn
    except psycopg2.Error as e:
        logging.error(f"❌ Ошибка подключения к БД: {e}")
        return None

# --- 3. Основная функция анализа с помощью DeepSeek ---
async def analyze_article_with_deepseek(title: str, content: str) -> Optional[dict]:
    """
    Отправляет текст статьи в DeepSeek и возвращает решение в виде словаря.
    """
    if not title and not content:
        logging.warning("⚠️ Передан пустой заголовок и текст. Пропускаем.")
        return None

    system_prompt = """
    Ты — эксперт по кибербезопасности. Твоя задача — проанализировать новость и решить, описывает ли она конкретную мошенническую схему, метод социальной инженерии, фишинга или кибермошенничества.
    
    Если новость не содержит описания конкретной схемы (например, это просто статистика, судебное дело, обзор технологий или общие слова), ты должен вернуть:
    {
      "is_fraud_scheme": false
    }
    
    Если новость ДЕЙСТВИТЕЛЬНО описывает новую или существующую мошенническую схему, ты должен вернуть:
    {
      "is_fraud_scheme": true,
      "summary": "Краткое (2-3 предложения) и простое описание сути схемы.",
      "advice": ["Практический совет 1", "Практический совет 2", "Практический совет 3"]
    }
    
    Отвечай ТОЛЬКО в формате JSON, без каких-либо других пояснений.
    """

    headers = {
        "Authorization": f"Bearer {DEEPSEEK_API_KEY}",
        "Content-Type": "application/json"
    }
    
    payload = {
        "model": "deepseek-chat",
        "messages": [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": f"Заголовок: {title}\n\nТекст новости: {content}"}
        ],
        "temperature": 0.3,
        "stream": False
    }

    try:
        response = await asyncio.to_thread(
            requests.post,
            "https://api.deepseek.com/v1/chat/completions",
            headers=headers,
            json=payload,
            timeout=60
        )
        response.raise_for_status()
        
        response_data = response.json()
        analysis_result = response_data['choices'][0]['message']['content']
        
        # Извлекаем JSON-объект из ответа
        json_start = analysis_result.find('{')
        json_end = analysis_result.rfind('}') + 1
        if json_start != -1 and json_end != -1:
            json_str = analysis_result[json_start:json_end]
            return json.loads(json_str)
        else:
            logging.error(f"❌ Не удалось найти JSON в ответе DeepSeek: {analysis_result}")
            return None
            
    except requests.exceptions.RequestException as e:
        logging.error(f"❌ Ошибка при запросе к DeepSeek API: {e}")
        return None
    except Exception as e:
        logging.error(f"❌ Непредвиденная ошибка при анализе статьи: {e}")
        return None

# --- 4. Обработчики инструментов (без привязки к mcp) ---
async def process_unchecked_articles_handler(limit: int = 5) -> str:
    """Анализирует непроверенные статьи с помощью ИИ и сохраняет результат."""
    logging.info(f"🚀 Запущен анализ {limit} непроверенных статей...")
    
    conn = get_db_connection()
    if not conn:
        return "❌ Ошибка: Не удалось подключиться к базе данных."
    
    try:
        with conn.cursor(cursor_factory=RealDictCursor) as cur:
            cur.execute("""
                SELECT a.news_id, a.title, a.content_text, a.url
                FROM articles a
                WHERE a.is_fraud_related = TRUE
                  AND NOT EXISTS (
                      SELECT 1 FROM fraud_articles f WHERE f.news_id = a.news_id
                  )
                LIMIT %s
            """, (limit,))
            unchecked_articles = cur.fetchall()

        if not unchecked_articles:
            logging.info("✅ Нет непроверенных статей.")
            return "✅ Все статьи уже проверены."

        analyzed_count = 0
        for article in unchecked_articles:
            logging.info(f"🔍 Анализирую статью ID: {article['news_id']}...")
            
            analysis_result = await analyze_article_with_deepseek(
                article['title'], article['content_text']
            )
            
            if analysis_result is None:
                logging.warning(f"⚠️ Не удалось проанализировать статью ID: {article['news_id']}. Пропускаем.")
                continue

            with conn.cursor() as cur:
                cur.execute("""
                    INSERT INTO fraud_articles (news_id, theme_category, selection_method, is_fraud_scheme, summary, advice)
                    VALUES (%s, %s, 'LLM:DeepSeek', %s, %s, %s)
                    ON CONFLICT (news_id) DO NOTHING
                """, (
                    article['news_id'],
                    'social_engineering',
                    analysis_result.get('is_fraud_scheme', False),
                    analysis_result.get('summary', ''),
                    '\n'.join(analysis_result.get('advice', []))
                ))
            conn.commit()
            analyzed_count += 1
            logging.info(f"✅ Статья ID: {article['news_id']} успешно обработана.")

        return f"✅ Анализ завершен. Проверено {analyzed_count} новых статей."
            
    except psycopg2.Error as e:
        logging.error(f"❌ Ошибка при работе с БД: {e}")
        return f"❌ Ошибка базы данных: {e}"
    finally:
        if conn:
            conn.close()

async def get_fraud_articles_handler(limit: int = 10) -> str:
    """Возвращает последние проверенные статьи о мошенничестве."""
    conn = get_db_connection()
    if not conn:
        return "❌ Ошибка: Не удалось подключиться к базе данных."
    
    try:
        with conn.cursor(cursor_factory=RealDictCursor) as cur:
            cur.execute("""
                SELECT f.news_id, f.summary, f.advice, a.title, a.url
                FROM fraud_articles f
                JOIN articles a ON f.news_id = a.news_id
                WHERE f.is_fraud_scheme = TRUE
                ORDER BY f.selected_at DESC
                LIMIT %s
            """, (limit,))
            articles = cur.fetchall()
        
        if not articles:
            return "📭 Нет проверенных статей о мошенничестве."
        
        response_text = f"📰 **Последние {len(articles)} статей о мошенничестве:**\n\n"
        for i, article in enumerate(articles, 1):
            response_text += f"{i}. {article['title']}\n"
            response_text += f"   📝 *Суть схемы:* {article['summary']}\n"
            response_text += f"   🔗 *Подробнее:* {article['url']}\n\n"
        
        return response_text
        
    except Exception as e:
        logging.error(f"❌ Ошибка при получении статей: {e}")
        return f"❌ Произошла ошибка: {e}"
    finally:
        if conn:
            conn.close()

# --- 5. Создание и настройка MCP-сервера (актуальный API) ---
app = Server("fraud-news-analyzer")
logging.info("✅ MCP-сервер 'fraud-news-analyzer' создан.")

@app.list_tools()
async def list_tools() -> list[Tool]:
    return [
        Tool(
            name="process_unchecked_articles",
            description="Анализирует непроверенные статьи с помощью ИИ и сохраняет результат.",
            inputSchema={
                "type": "object",
                "properties": {
                    "limit": {
                        "type": "integer",
                        "description": "Количество статей для анализа (по умолчанию 5)"
                    }
                }
            }
        ),
        Tool(
            name="get_fraud_articles",
            description="Возвращает последние проверенные статьи о мошенничестве.",
            inputSchema={
                "type": "object",
                "properties": {
                    "limit": {
                        "type": "integer",
                        "description": "Количество статей для вывода (по умолчанию 10)"
                    }
                }
            }
        )
    ]

@app.call_tool()
async def call_tool(name: str, arguments: dict) -> str:
    if name == "process_unchecked_articles":
        limit = arguments.get("limit", 5)
        return await process_unchecked_articles_handler(limit)
    elif name == "get_fraud_articles":
        limit = arguments.get("limit", 10)
        return await get_fraud_articles_handler(limit)
    else:
        raise ValueError(f"Неизвестный инструмент: {name}")

# --- 6. Запуск MCP-сервера ---
async def main():
    async with stdio.stdio_server() as (read_stream, write_stream):
        await app.run(
            read_stream,
            write_stream,
            app.create_initialization_options()
        )

if __name__ == "__main__":
    logging.info("🚀 Запуск MCP-сервера...")
    asyncio.run(main())
