import asyncio
from server import process_unchecked_articles_handler, get_fraud_articles_handler

async def test():
    print("=" * 50)
    print("ТЕСТ 1: Получение уже проверенных статей")
    print("=" * 50)
    result = await get_fraud_articles_handler(limit=5)
    print(result)
    
    print("\n" + "=" * 50)
    print("ТЕСТ 2: Запуск анализа двух непроверенных статей")
    print("=" * 50)
    result = await process_unchecked_articles_handler(limit=2)
    print(result)

if __name__ == "__main__":
    asyncio.run(test())
