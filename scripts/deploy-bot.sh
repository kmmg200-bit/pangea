#!/usr/bin/env bash
# Выкладка бота без паузы в ходах: новый бот поднимается рядом со старым, и,
# когда он готов (healthcheck в docker-compose), старый гасится — доделывает
# начатые ходы и выходит. Пока их двое, ходы одного игрока разводит лок в
# Postgres, а одиночные фоновые дела делает только ведущий (см. README,
# «Выкладка бота»). Сборщик, caddy и база не трогаются.
#
#   scripts/deploy-bot.sh
#
# Новый не поднялся — гасим его, старый работает дальше, скрипт выходит с
# ошибкой.
set -euo pipefail

cd "$(dirname "$(readlink -f "$0")")/.."

git pull --ff-only
docker compose build app

old=$(docker compose ps -q app)
if [ -z "$old" ]; then
  echo "Бот не запущен — обычный старт"
  docker compose up -d --no-deps app
  exit 0
fi
if [ "$(echo "$old" | wc -l)" -ne 1 ]; then
  echo "Ботов уже несколько — разберитесь руками: docker compose ps app"
  exit 1
fi

echo "$(date +%T) Поднимаю новый бот рядом со старым ($(docker inspect -f '{{.Name}}' "$old" | tr -d /))"
docker compose up -d --no-deps --no-recreate --scale app=2 app
new=$(docker compose ps -q app | grep -v "$old")

echo "$(date +%T) Жду, пока новый ($(docker inspect -f '{{.Name}}' "$new" | tr -d /)) будет готов"
status=starting
for _ in $(seq 1 60); do
  status=$(docker inspect -f '{{.State.Health.Status}}' "$new")
  [ "$status" = healthy ] || [ "$status" = unhealthy ] && break
  sleep 5
done
if [ "$status" != healthy ]; then
  echo "$(date +%T) Новый бот не поднялся ($status) — гашу его, старый работает дальше. Его лог:"
  docker logs --tail 50 "$new"
  docker stop -t 30 "$new" >/dev/null && docker rm "$new" >/dev/null
  exit 1
fi

echo "$(date +%T) Новый готов. Гашу старый: доделает начатые ходы и выйдет"
docker stop -t 30 "$old" >/dev/null

# Как остановился старый — до удаления, пока его лог ещё есть: 137 значит, что
# за 30 секунд он не вышел сам и Docker его добил. Лог за 5 минут — в файл.
code=$(docker inspect -f '{{.State.ExitCode}}' "$old")
saved="/var/log/pangea-deploy/$(docker inspect -f '{{.Name}}' "$old" | tr -d /)-$(date +%F_%H%M%S).log"
mkdir -p /var/log/pangea-deploy
docker logs --since 5m "$old" > "$saved" 2>&1 || true
if [ "$code" = 137 ]; then
  echo "$(date +%T) Старый не вышел сам за 30 с — Docker его добил (exit 137). Как шла остановка:"
else
  echo "$(date +%T) Старый вышел сам (exit $code). Как шла остановка:"
fi
grep -E "Остановка|фон|ERROR" "$saved" | tail -10 || true
echo "Лог старого за 5 минут: $saved"
docker rm "$old" >/dev/null
docker compose up -d --no-deps --no-recreate --scale app=1 app
echo "$(date +%T) Готово: $(docker compose ps app --format '{{.Name}} — {{.Status}}')"

# Кэш сборки: каждая сборка бота оставляет ~290 МБ слоёв, которые следующей уже
# не нужны, — копию app.jar с шагами после неё, `sbt app/assembly`, исходники и
# контекст. А когда меняются зависимости или базовый образ, заново собирается и
# подготовка — `apt-get`, `sbt update` (~400 МБ), — и прежняя тоже не нужна.
# Остаётся то, чем пользовалась последняя сборка (until=1h), и всё, на что оно
# опирается: запись с потомками BuildKit не удаляет. Поэтому — от последнего
# шага к первому; номера шагов — по Dockerfile. Базовые образы не трогаем. Сбой
# здесь выкладку не трогает: она готова.
for f in "chmod.+entrypoint" "stage-1.8/9" "stage-1.7/9" "app/target/scala-2.13/app.jar" \
         "keytool.-printcert" "stage-1.4/9" \
         "sbt.app/assembly" "builder.+9/10.+COPY" "sbt.update" \
         "builder.+7/10" "builder.+6/10" "builder.+5/10" "builder.+4/10" \
         "apt-get.update" "local.source.for.context"; do
  docker builder prune -f --filter "description~=$f" --filter until=1h >/dev/null 2>&1 || true
done
echo "$(date +%T) Кэш сборки: $(docker builder du 2>/dev/null | awk '/^Total:/{print $2}')"
