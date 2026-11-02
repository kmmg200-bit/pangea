-- +goose Up
-- +goose StatementBegin
-- Девушка с картой отца ждёт в таверне, пока герой соберёт серебро: цена,
-- уровень карты и до какого часа она ждёт (см. GirlDeal). NULL — никто не ждёт.
ALTER TABLE heroes ADD COLUMN girl_deal JSONB;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN girl_deal;
-- +goose StatementEnd
