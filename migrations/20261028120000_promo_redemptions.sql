-- +goose Up
-- +goose StatementBegin
-- Промокоды: кто какой код ввёл. Ввести код можно один раз — отсюда ключ по
-- (user_id, code). Запись по игроку, а не по герою: /restart её не стирает.
-- Сами коды, сроки и награды — в коде (см. pangea.model.promo.PromoCode).
CREATE TABLE promo_redemptions (
    user_id     BIGINT      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    code        VARCHAR(32) NOT NULL,
    redeemed_at BIGINT      NOT NULL,
    PRIMARY KEY (user_id, code)
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE promo_redemptions;
-- +goose StatementEnd
