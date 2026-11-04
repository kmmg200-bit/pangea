#!/usr/bin/env bash
# Бэкап базы Пангеи на Google Drive. Каждый день в 3:00 его запускает cron (см.
# scripts/pangea-backup.cron); руками — так же: scripts/backup-db.sh.
#
# Дамп снимается внутри уже работающего контейнера postgres (docker compose
# exec): ничего не пересоздаётся и не перезапускается, бот не задевается.
# Postgres снимает согласованный снимок — останавливать игру не нужно.
#
# Шаги: дамп (pg_dump -Fc) → проверка, что архив читается → копия на Google
# Drive (rclone, remote «gdrive») → там удаляются копии старше 30 дней, на
# сервере остаются 3 последние.
#
# Данные vk_inbox (очередь входящих событий ВК: неделя сырых событий, ~90
# тысяч в день) и vk_seen_messages (прежние отметки «уже обработано») в дамп не
# идут: это служебное, восстанавливать его незачем, а дамп раздулся бы в
# десятки раз. Схемы таблиц в дампе остаются. Сломался любой шаг — сообщение в ВК всем из
# BACKUP_ALERT_VK_IDS (.env) и выход с ошибкой; старые копии тогда не трогаются.
#
# BACKUP_REMOTE — куда класть копии вместо gdrive:pangea-backups. Нужен, чтобы
# проверить сообщение о сбое: BACKUP_REMOTE=nope: scripts/backup-db.sh
set -euo pipefail

cd "$(dirname "$(readlink -f "$0")")/.."

LOCAL_DIR=/root/backups/pangea
REMOTE=${BACKUP_REMOTE:-gdrive:pangea-backups}
KEEP_DAYS=30
KEEP_LOCAL=3

# Значение из .env: без кавычек вокруг, пусто — если его нет.
env_value() {
  grep -E "^$1=" .env | tail -1 | cut -d= -f2- | sed -e 's/^"\(.*\)"$/\1/' -e "s/^'\(.*\)'$/\1/" || true
}

alert() {
  local token ids text
  token=$(env_value VK_TOKEN)
  ids=$(env_value BACKUP_ALERT_VK_IDS)
  text="⚠️ Бэкап базы Пангеи не сделан ($(date '+%d.%m %H:%M')): $1. Подробности — /var/log/pangea-backup.log на сервере."
  echo "$(date '+%F %T') ОШИБКА: $1"
  if [ -z "$token" ] || [ -z "$ids" ]; then
    echo "сообщить некому: в .env нет VK_TOKEN или BACKUP_ALERT_VK_IDS"
    return
  fi
  curl -sS --max-time 20 https://api.vk.com/method/messages.send \
    --data-urlencode "peer_ids=$ids" \
    --data-urlencode "random_id=$RANDOM$RANDOM" \
    --data-urlencode "message=$text" \
    --data-urlencode "access_token=$token" \
    --data-urlencode "v=5.199" | grep -o '"error".*' || true
}

# Два запуска разом (cron и руками) не нужны: второй просто выходит.
exec 9>/run/pangea-backup.lock
flock -n 9 || { echo "$(date '+%F %T') бэкап уже идёт — выхожу"; exit 0; }

step="подготовка"
file=""
trap 'alert "упал шаг «${step}»"; [ -n "$file" ] && rm -f "$file.part"' ERR

echo "$(date '+%F %T') начинаю"
mkdir -p "$LOCAL_DIR"
file="$LOCAL_DIR/pangea_$(date +%F_%H%M).dump"

step="дамп базы"
docker compose exec -T postgres sh -c \
  'pg_dump -U "$POSTGRES_USER" -d "$POSTGRES_DB" -Fc --exclude-table-data=vk_inbox --exclude-table-data=vk_seen_messages' > "$file.part"
mv "$file.part" "$file"

step="проверка архива"
docker compose exec -T postgres pg_restore --list < "$file" > /dev/null

step="загрузка на Google Drive"
rclone copy "$file" "$REMOTE" --retries 5 --low-level-retries 10

step="уборка старых копий"
rclone delete "$REMOTE" --min-age "${KEEP_DAYS}d" --include "pangea_*.dump"
ls -1t "$LOCAL_DIR"/pangea_*.dump | tail -n +$((KEEP_LOCAL + 1)) | xargs -r rm --

echo "$(date '+%F %T') готово: $(basename "$file"), $(du -h "$file" | cut -f1)"
