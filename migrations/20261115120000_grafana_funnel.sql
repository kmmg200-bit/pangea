-- +goose Up
-- Дашборд «Воронка» в Графане: сколько героев дошло до какого уровня и кто из
-- них заходил за сутки и за неделю, плюс сводка как в админ-панели. Роли
-- pangea_grafana — ещё несколько колонок только на чтение, по-прежнему ни
-- почты, ни вещей. Повторная выдача прав ничего не меняет — миграцию можно
-- выполнить руками раньше, чем её накатит бот.
-- +goose StatementBegin
GRANT SELECT (lvl, state, silver, doubloons) ON heroes TO pangea_grafana;
-- +goose StatementEnd
-- +goose StatementBegin
GRANT SELECT (last_seen_at) ON users TO pangea_grafana;
-- +goose StatementEnd
-- +goose StatementBegin
GRANT SELECT (silver) ON bank_vaults TO pangea_grafana;
-- +goose StatementEnd
-- +goose StatementBegin
GRANT SELECT (event_type, occurred_at) ON event_log TO pangea_grafana;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
REVOKE SELECT (event_type, occurred_at) ON event_log FROM pangea_grafana;
-- +goose StatementEnd
-- +goose StatementBegin
REVOKE SELECT (silver) ON bank_vaults FROM pangea_grafana;
-- +goose StatementEnd
-- +goose StatementBegin
REVOKE SELECT (last_seen_at) ON users FROM pangea_grafana;
-- +goose StatementEnd
-- +goose StatementBegin
REVOKE SELECT (lvl, state, silver, doubloons) ON heroes FROM pangea_grafana;
-- +goose StatementEnd
