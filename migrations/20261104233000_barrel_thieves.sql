-- +goose Up
-- +goose StatementBegin
-- Воры у бочки («Вскрытая бочка»): когда последний раз бросали жребий на кражу
-- и что украдено, пока банда не найдена (см. BarrelThieves). NULL — ни
-- броска, ни кражи.
ALTER TABLE barrels ADD COLUMN thieves JSONB;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE barrels DROP COLUMN thieves;
-- +goose StatementEnd
