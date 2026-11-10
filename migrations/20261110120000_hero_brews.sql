-- +goose Up
-- +goose StatementBegin
-- Сколько зелий герой сварил за всю жизнь: куб, Живая сумка, верстак. По нему
-- «Промышленный аптекарь» I–V (10/100/250/500/1000). Прошлых варок нигде не
-- сохранилось — счёт идёт с нуля.
ALTER TABLE heroes ADD COLUMN brews BIGINT NOT NULL DEFAULT 0;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN brews;
-- +goose StatementEnd
