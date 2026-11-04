#!/bin/sh
set -e

GOOSE_URL="postgres://${POSTGRES_USER}:${POSTGRES_PASSWORD}@postgres:5432/${POSTGRES_DB}?sslmode=disable"

# Схему базы ведёт бот (ROLE=bot или all). Сборщик (ROLE=collector) миграций не
# катит: иначе два контейнера гоняли бы их одновременно, а выкладка бота с новой
# миграцией сборщика не касается — ему нужна лишь очередь vk_inbox.
if [ "${ROLE:-all}" = "collector" ]; then
  echo "Collector: migrations are run by the bot"
else
  echo "Running migrations..."
  goose -dir /app/migrations postgres "$GOOSE_URL" up
fi

echo "Starting application..."
exec java -Dfile.encoding=UTF-8 -Dsun.jnu.encoding=UTF-8 -jar /app/app.jar
