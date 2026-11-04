-- +goose Up
-- Графана читает донаты прямо из базы (дашборд «Донат»). Для неё — отдельная
-- роль: только чтение и только нужные колонки — ни почты для чека, ни остальной
-- базы. Пароля в репозитории нет: его один раз задают на сервере
-- (\password pangea_grafana), и он же лежит в .env как GRAFANA_DB_PASSWORD.
-- Пока пароль не задан, войти под ролью нельзя.
-- +goose StatementBegin
DO $$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'pangea_grafana') THEN
    CREATE ROLE pangea_grafana LOGIN CONNECTION LIMIT 5;
  END IF;
END
$$;
-- +goose StatementEnd
-- +goose StatementBegin
-- Даже с лишним правом писать не выйдет, а тяжёлый запрос с дашборда не
-- повиснет на базе, где играют.
ALTER ROLE pangea_grafana SET default_transaction_read_only = on;
-- +goose StatementEnd
-- +goose StatementBegin
ALTER ROLE pangea_grafana SET statement_timeout = '10s';
-- +goose StatementEnd
-- +goose StatementBegin
GRANT SELECT (user_id, sku, amount_kopecks, doubloons, status, granted, created_at) ON payments TO pangea_grafana;
-- +goose StatementEnd
-- +goose StatementBegin
GRANT SELECT (id, vk_id) ON users TO pangea_grafana;
-- +goose StatementEnd
-- +goose StatementBegin
GRANT SELECT (user_id, name) ON heroes TO pangea_grafana;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
REVOKE ALL ON payments, users, heroes FROM pangea_grafana;
-- +goose StatementEnd
-- +goose StatementBegin
DROP ROLE IF EXISTS pangea_grafana;
-- +goose StatementEnd
