-- +goose Up
-- +goose StatementBegin
-- Обмен между игроками — предложением. Каждая сторона — заклад: вещи целиком,
-- какими их положили, серебро и дублоны с комиссией сверху и записка (JSONB, см.
-- TradeSide). Положенное уже ушло из сумки и ждёт здесь, пока предложение не
-- примут, не отклонят, не отзовут или не выйдет срок. turn — чей ход: тот, кто
-- может принять предложение или поменять свою сторону. version растёт с каждой
-- записью: двое, что разом меняют одно предложение, не затрут друг друга.
CREATE TABLE trade_offers (
    id         BIGSERIAL   PRIMARY KEY,
    from_user  BIGINT      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    to_user    BIGINT      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    from_side  JSONB       NOT NULL,
    to_side    JSONB       NOT NULL,
    turn       BIGINT      NOT NULL,
    status     VARCHAR(16) NOT NULL,
    created_at BIGINT      NOT NULL,
    updated_at BIGINT      NOT NULL,
    expires_at BIGINT      NOT NULL,
    done_at    BIGINT,
    version    INT         NOT NULL DEFAULT 0
);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX trade_offers_open_from ON trade_offers (from_user) WHERE status = 'Open';
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX trade_offers_open_to ON trade_offers (to_user) WHERE status = 'Open';
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX trade_offers_expiring ON trade_offers (expires_at) WHERE status = 'Open';
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE trade_offers;
-- +goose StatementEnd
