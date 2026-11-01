-- +goose Up
-- +goose StatementBegin
-- Сообщения, которые бот уже взял в работу. Одно сообщение приходит дважды —
-- по Callback API и по Long Poll, — и в работу идёт только то, что пришло
-- первым. Сообщение — это беседа (peer_id) и его номер: в личке ВК шлёт `id`,
-- в беседе `id` нулевой, а номер — в conversation_message_id (нет — 0).
-- Хранятся сутки: опоздание ВК — минуты.
CREATE TABLE vk_seen_messages(
  peer_id                 BIGINT NOT NULL,
  message_id              BIGINT NOT NULL,
  conversation_message_id BIGINT NOT NULL,
  seen_at                 BIGINT NOT NULL,
  PRIMARY KEY (peer_id, message_id, conversation_message_id)
);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX idx_vk_seen_messages_seen_at ON vk_seen_messages(seen_at);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE vk_seen_messages;
-- +goose StatementEnd
