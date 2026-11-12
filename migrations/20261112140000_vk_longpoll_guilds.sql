-- +goose Up
-- +goose StatementBegin
-- Место Long Poll гильдейского сообщества (VK_TOKEN_FOR_GROUP_CHATS) — второй
-- строкой рядом с основным: id 1 — основное, id 2 — гильдейское.
ALTER TABLE vk_longpoll DROP CONSTRAINT vk_longpoll_id_check;
-- +goose StatementEnd
-- +goose StatementBegin
ALTER TABLE vk_longpoll ADD CONSTRAINT vk_longpoll_id_check CHECK (id IN (1, 2));
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DELETE FROM vk_longpoll WHERE id = 2;
-- +goose StatementEnd
-- +goose StatementBegin
ALTER TABLE vk_longpoll DROP CONSTRAINT vk_longpoll_id_check;
-- +goose StatementEnd
-- +goose StatementBegin
ALTER TABLE vk_longpoll ADD CONSTRAINT vk_longpoll_id_check CHECK (id = 1);
-- +goose StatementEnd
