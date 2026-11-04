-- +goose Up
-- +goose StatementBegin
-- Аренда задачи планировщика: до какого момента (мс) задача захвачена ботом,
-- который её исполняет. Захватывают через FOR UPDATE SKIP LOCKED — так задачу
-- не возьмут два бота сразу; бот упал посреди задачи — по истечении аренды её
-- подхватит другой. NULL — никем не захвачена.
ALTER TABLE scheduled_tasks ADD COLUMN locked_until BIGINT;
-- +goose StatementEnd
-- +goose StatementBegin
-- Ждущие задачи игрока: при захвате пропускаем игроков, чья задача уже в
-- работе, и по игроку же их отменяет Scheduler.
CREATE INDEX idx_scheduled_pending_user ON scheduled_tasks (user_id) WHERE status = 'Pending';
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP INDEX idx_scheduled_pending_user;
-- +goose StatementEnd
-- +goose StatementBegin
ALTER TABLE scheduled_tasks DROP COLUMN locked_until;
-- +goose StatementEnd
