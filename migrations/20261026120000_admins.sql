-- +goose Up
-- +goose StatementBegin
-- Права в админ-панели: у кого какой уровень. «Админ 1» входит в панель без
-- пароля и получает в личку сообщения игроков с /home (см.
-- pangea.service.admin.AdminPanel). Права висят на пользователе, а не на
-- герое: рестарт их не снимает.
CREATE TABLE admins (
    user_id    BIGINT PRIMARY KEY REFERENCES users(id),
    level      INT NOT NULL,
    granted_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE admins;
-- +goose StatementEnd
