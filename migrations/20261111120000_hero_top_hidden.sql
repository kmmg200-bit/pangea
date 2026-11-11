-- +goose Up
-- +goose StatementBegin
-- Зелье исчезновения Фета: до этого момента (мс с эпохи) герой не виден в
-- рейтингах приложения. Пусто — виден. Каждая склянка прибавляет неделю к
-- остатку, а развеять туман раньше срока можно у Фета (см. FetShopState).
ALTER TABLE heroes ADD COLUMN top_hidden_until BIGINT;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN top_hidden_until;
-- +goose StatementEnd
