#!/bin/sh
# Зеркала Docker Hub для Docker на сервере. С российских серверов Docker Hub
# периодически пропадает целиком («TLS handshake timeout» на auth.docker.io),
# и тогда не скачать ни новый образ, ни базовый для сборки бота. С зеркалами
# Docker сам перебирает их по порядку, а Docker Hub пробует последним.
#
# Запускать на сервере один раз, от root:
#   sh docker/registry-mirrors.sh                  — зеркала по умолчанию
#   sh docker/registry-mirrors.sh https://a https://b  — свои
#
# Прочие настройки в /etc/docker/daemon.json сохраняются (старый файл — рядом,
# .bak). Docker перечитывает список зеркал без перезапуска: контейнеры, а с
# ними и бот, продолжают работать.
set -eu

CONFIG=/etc/docker/daemon.json
# Зеркала крупных компаний: Google и хостинга Timeweb. Чужое зеркало может
# подсунуть свой образ — поэтому только те, кому есть смысл доверять.
MIRRORS="${*:-https://mirror.gcr.io https://dockerhub.timeweb.cloud}"

[ "$(id -u)" -eq 0 ] || { echo "Нужен root: sudo sh $0"; exit 1; }
command -v python3 >/dev/null || { echo "Нужен python3: им правится daemon.json"; exit 1; }

# Отвечающие — вперёд: Docker ждёт каждое зеркало по очереди, и мёртвое первым
# замедляло бы каждое скачивание. Не отвечающие остаются в конце — вдруг оживут.
alive=""
dead=""
echo "Проверяю зеркала:"
for m in $MIRRORS; do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 10 "$m/v2/" 2>/dev/null || true)
  case "$code" in
    200|401) echo "  ✔ $m"; alive="$alive $m" ;;
    *)       echo "  ✘ $m (ответ: ${code:-нет})"; dead="$dead $m" ;;
  esac
done
[ -n "$alive" ] || echo "Сейчас не отвечает ни одно — всё равно пропишу: Docker будет пробовать их по очереди."

# shellcheck disable=SC2086 # список зеркал — отдельными аргументами
python3 - "$CONFIG" $alive $dead <<'PY'
import json, os, shutil, sys

path, mirrors = sys.argv[1], sys.argv[2:]
config = {}
if os.path.exists(path):
    with open(path) as f:
        text = f.read().strip()
    config = json.loads(text) if text else {}
    shutil.copyfile(path, path + ".bak")
config["registry-mirrors"] = mirrors
os.makedirs(os.path.dirname(path), exist_ok=True)
with open(path + ".tmp", "w") as f:
    json.dump(config, f, indent=2)
    f.write("\n")
os.replace(path + ".tmp", path)
print("Записал в %s: %s" % (path, ", ".join(mirrors)))
PY

# SIGHUP: список зеркал Docker перечитывает на лету, без остановки контейнеров.
systemctl reload docker
echo "Docker теперь видит зеркала:"
docker info --format '{{range .RegistryConfig.Mirrors}}  {{.}}{{"\n"}}{{end}}'
echo "Готово. Если сборка бота всё равно стучится в docker.io (load metadata …"
echo "TLS handshake timeout), значит, сборщик зеркала не перечитал — тогда"
echo "systemctl restart docker: это перезапустит контейнеры, бот встанет на 10–30 с."
