#!/usr/bin/env bash
# Выкладка VK Mini App (`web/`). Приложение собирается внутри образа caddy,
# поэтому новая версия — пересборка caddy, и только его: `--no-deps` не даёт
# compose заодно пересобрать и перезапустить бота (это пауза в ходах). Бот
# выкладывается своим скриптом, scripts/deploy-bot.sh, и приложение он не
# обновляет. Приложение зовёт запросы, которых у бота ещё нет, — сначала бот,
# потом этот скрипт.
#
#   scripts/deploy-app.sh
#
# caddy недоступен несколько секунд: колбэк ВК и уведомления кассы в эти
# секунды повторят сами ВК и касса.
set -euo pipefail

cd "$(dirname "$(readlink -f "$0")")/.."

git pull --ff-only
echo "$(date +%T) Пересобираю приложение (caddy), бота не трогаю"
docker compose up -d --build --no-deps caddy
echo "$(date +%T) Готово: $(docker compose ps --format '{{.Name}} — {{.Status}}' caddy)"
