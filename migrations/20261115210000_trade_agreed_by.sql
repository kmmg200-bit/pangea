-- +goose Up
-- Обмен в три шага и больше: кому предложили, тот соглашается (или отвечает
-- своим), а завершает обмен автор последних условий — подтверждением. Здесь —
-- кто уже согласился с условиями, какие лежат сейчас; любая правка стороны его
-- снимает. NULL — никто: так и читаются предложения, открытые до этой
-- миграции.
-- +goose StatementBegin
ALTER TABLE trade_offers ADD COLUMN agreed_by BIGINT NULL;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE trade_offers DROP COLUMN agreed_by;
-- +goose StatementEnd
