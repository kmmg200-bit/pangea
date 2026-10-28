-- +goose Up
-- +goose StatementBegin
-- Имя героя: выбирается в конце пролога, меняется купоном Фета (см. HeroName).
-- Пусто у тех, кто прошёл пролог до имён: им показывается имя из ВК.
ALTER TABLE heroes ADD COLUMN name VARCHAR(16);
-- +goose StatementEnd
-- +goose StatementBegin
-- Имя уникально без учёта регистра: «Вася» и «вася» — одно имя. Пустые имена
-- друг другу не мешают.
CREATE UNIQUE INDEX heroes_name_lower_uniq ON heroes (lower(name));
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP INDEX heroes_name_lower_uniq;
ALTER TABLE heroes DROP COLUMN name;
-- +goose StatementEnd
