import os
import requests
from dotenv import load_dotenv

load_dotenv()
API_KEY = os.getenv("DEEPSEEK_API_KEY")

headers = {
    "Authorization": f"Bearer {API_KEY}",
    "Content-Type": "application/json",
    "HTTP-Referer": "http://localhost",   # обязательно для OpenRouter
    "X-Title": "TT-RSS Fraud Analyzer"
}

payload = {
    "model": "nvidia/nemotron-3-super-120b-a12b:free",
    "messages": [
        {"role": "user", "content": "Привет, мир! Ответь кратко."}
    ],
    "temperature": 0.0,
    "max_tokens": 50
}

response = requests.post(
    "https://openrouter.ai/api/v1/chat/completions",   # новый URL
    headers=headers,
    json=payload,
    timeout=30
)

print("HTTP Status:", response.status_code)
print("Response JSON:")
print(response.json())
