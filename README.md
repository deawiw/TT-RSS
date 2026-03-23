# Запуск TT-RSS в Docker

Ниже короткая инструкция, как поднять TT-RSS локально и зайти в веб-интерфейс.

## Что должно быть установлено

- Docker
- Docker Compose
- Git

Проверить можно так:

```bash
docker --version
docker compose version
git --version
```

Клонировать так:

```bash
git clone https://github.com/deawiw/TT-RSS.git
cd TT-RSS
```
Подключены к нужному репозиторию:

```bash
git remote -v
```

В выводе должен быть адрес:

https://github.com/deawiw/TT-RSS.git

## Быстрый запуск

### 1. Перейти в папку проекта

```bash
cd TT-RSS
```

### 2. Создать локальный файл с переменными

Linux/macOS:

```bash
cp .env.example .env
```

PowerShell:

```powershell
Copy-Item .env.example .env
```

После этого при желании можно открыть `.env` и поменять пароли и порт.

По умолчанию там указано:

```env
TTRSS_HTTP_PORT=8280
TTRSS_SELF_URL_PATH=http://localhost:8280/
TTRSS_ADMIN_PASSWORD=change_me_admin_password
POSTGRES_DB=ttrss
POSTGRES_USER=ttrss
POSTGRES_PASSWORD=change_me_db_password
```

Если ничего не менять, стенд запустится с этими значениями.

### 3. Поднять контейнеры

```bash
docker compose up -d
```

### 4. Проверить, что всё стартовало

```bash
docker compose ps
```

### 5. Открыть TT-RSS в браузере

Откройте:

```text
http://localhost:8280/
```

Если меняли `TTRSS_HTTP_PORT`, используйте свой порт.

## Как делать коммиты

Посмотреть, что изменилось:

```bash
git status
```

Добавить изменения в коммит:

```bash
git add .
```

Если хотите добавить только конкретные файлы:

```bash
git add docker-compose.yml .env.example .gitignore README.md docs/architecture.md
```

Создать коммит:

```bash
git commit -m "Обновил конфигурацию TT-RSS"
```

Отправить изменения в репозиторий:

```bash
git push origin main
```

Если основная ветка у вас называется не `main`, а иначе, сначала проверьте:

```bash
git branch
```

НЕ КОМИТЕТЬ .env !!!


