-- +goose Up
-- +goose StatementBegin
-- Когда герой впервые достиг текущей мерки топа (уровень, глубина, убийства
-- и т.д.). При равенстве в рейтинге выше тот, у кого метка раньше. Ключи —
-- `TopKind.entryName` (level, depth, kills, …). Старые герои — пустой объект,
-- тогда порядок у равенства по `h.id`.
ALTER TABLE heroes ADD COLUMN top_since JSONB NOT NULL DEFAULT '{}'::jsonb;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN top_since;
-- +goose StatementEnd