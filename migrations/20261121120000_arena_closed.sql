-- +goose Up
-- Арена закрыта: бои между игроками перестраиваются заново, старый код арены
-- убран целиком. Что после неё осталось в базе:
--  - задачи «зов к экрану» и «минута на ход вышла» — их видов в коде больше
--    нет, и планировщик споткнулся бы о такую строку;
--  - бой на арене у героя посреди боя: он читался бы как бой с мобом из статов
--    соперника — снимаем его, герой встаёт на закрытую арену;
--  - строки боёв арены — таблица больше не нужна.
-- +goose StatementBegin
DELETE FROM scheduled_tasks WHERE kind IN ('ArenaPoke', 'ArenaTurn');
-- +goose StatementEnd
-- +goose StatementBegin
UPDATE heroes
   SET state = 'Arena', active_battle = NULL
 WHERE active_battle IS NOT NULL
   AND active_battle ? 'arena'
   AND active_battle -> 'arena' <> 'null'::jsonb;
-- +goose StatementEnd
-- +goose StatementBegin
DROP TABLE IF EXISTS arena_fights;
-- +goose StatementEnd

-- +goose Down
-- Задачи и бои арены не вернуть; таблица — пустая, как при первой миграции.
-- +goose StatementBegin
CREATE TABLE IF NOT EXISTS arena_fights (
    id         BIGSERIAL PRIMARY KEY,
    code       TEXT   NOT NULL UNIQUE,
    status     TEXT   NOT NULL,
    a_user_id  BIGINT NOT NULL,
    b_user_id  BIGINT,
    data       JSONB  NOT NULL,
    updated_at BIGINT NOT NULL
);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX IF NOT EXISTS arena_fights_status_idx ON arena_fights (status, updated_at);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX IF NOT EXISTS arena_fights_users_idx ON arena_fights (a_user_id, b_user_id);
-- +goose StatementEnd
