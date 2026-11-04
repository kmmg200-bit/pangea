-- +goose Up
-- +goose StatementBegin
-- Доплата за смену курса доната: с ноября 2026 дублон стоит 1 ₽ вместо 2 ₽, и
-- каждому заказу по старому курсу доплачивается столько же дублонов, сколько по
-- нему выдано. Строка — доплаченный заказ: ключ по заказу и есть гарантия, что
-- дважды не доплатить. Вставка строки и начисление герою идут одной транзакцией
-- (PaymentDao.grantRateBonus), запускает админ-панель (RateBonus).
CREATE TABLE doubloon_rate_bonus(
  order_id   TEXT   PRIMARY KEY REFERENCES payments(order_id),
  user_id    BIGINT NOT NULL,
  doubloons  BIGINT NOT NULL,
  granted_at BIGINT NOT NULL
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE doubloon_rate_bonus;
-- +goose StatementEnd
