-- +goose Up
-- Когда игрок зарегистрировался: по этой дате дашборд «Воронка» в Графане
-- раскладывает игроков по дням прихода и показывает, докуда дошёл каждый день.
-- Новым её проставляет сама база (DEFAULT now()), код бота колонку не пишет.
--
-- Точной даты у старых игроков нет — ставим оценку сверху. Игрок
-- зарегистрировался не позже своей первой вещи (items.created_at — время UTC)
-- и не позже первого сохранившегося сообщения боту (vk_inbox хранит их не всё
-- время), а номера игроков идут по порядку регистрации — значит, и не позже
-- любого, кто пришёл после него. Тем, после кого следов нет вовсе, остаётся
-- время миграции.
--
-- Оценки считаются заранее, во временную таблицу: users блокируется только на
-- добавление колонки и запись, а не на проход по vk_inbox.
-- +goose StatementBegin
CREATE TEMP TABLE users_registered AS
WITH first_item AS (
    SELECT h.user_id, min(i.created_at AT TIME ZONE 'UTC') AS t
    FROM items i
    JOIN heroes h ON h.id = i.hero_id
    GROUP BY h.user_id
), first_message AS (
    SELECT peer_id::text AS vk_id, to_timestamp(min(received_at) / 1000.0) AS t
    FROM vk_inbox
    WHERE kind = 'message'
    GROUP BY peer_id
), bound AS (
    SELECT u.id, least(fi.t, fm.t) AS t
    FROM users u
    LEFT JOIN first_item fi ON fi.user_id = u.id
    LEFT JOIN first_message fm ON fm.vk_id = u.vk_id
)
SELECT id, min(t) OVER (ORDER BY id DESC ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS t
FROM bound;
-- +goose StatementEnd
-- +goose StatementBegin
ALTER TABLE users ADD COLUMN created_at TIMESTAMPTZ NOT NULL DEFAULT now();
-- +goose StatementEnd
-- +goose StatementBegin
UPDATE users u SET created_at = r.t
FROM users_registered r
WHERE r.id = u.id AND r.t IS NOT NULL;
-- +goose StatementEnd
-- +goose StatementBegin
DROP TABLE users_registered;
-- +goose StatementEnd
-- +goose StatementBegin
-- Графане — дата регистрации и scene_data: по отметкам пролога в ней видно,
-- докуда дошёл тот, кто ещё в прологе (выбрал смерть, нашёл тайник).
GRANT SELECT (created_at) ON users TO pangea_grafana;
-- +goose StatementEnd
-- +goose StatementBegin
GRANT SELECT (scene_data) ON heroes TO pangea_grafana;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
REVOKE SELECT (scene_data) ON heroes FROM pangea_grafana;
-- +goose StatementEnd
-- +goose StatementBegin
ALTER TABLE users DROP COLUMN created_at;
-- +goose StatementEnd
