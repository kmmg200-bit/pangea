-- +goose Up
-- +goose StatementBegin
-- Очередь входящих событий ВК. Callback и Long Poll только кладут сюда событие
-- (и лишь потом отвечают ВК), а боты разбирают очередь: захват — FOR UPDATE
-- SKIP LOCKED с арендой (locked_until), у каждой очереди (peer_id) — только её
-- самое раннее ждущее событие, так ходы игрока идут по порядку и при нескольких
-- ботах. Заменяет vk_seen_messages: повторы отсекает dedup_key.
--
-- dedup_key — «m:<peer>:<id>:<cmid>» у сообщений, «e:<event_id>» у событий
-- группы; peer_id — игрок, беседа или 0 (события группы); kind — message, chat
-- или group; source — callback или longpoll: каким путём событие пришло
-- первым; event — событие ВК целиком (после обработки — без секретов); sent_at,
-- received_at, done_at — мс.
CREATE TABLE vk_inbox (
    id           BIGSERIAL PRIMARY KEY,
    dedup_key    TEXT   NOT NULL UNIQUE,
    peer_id      BIGINT NOT NULL,
    kind         TEXT   NOT NULL,
    source       TEXT   NOT NULL,
    event        JSONB  NOT NULL,
    sent_at      BIGINT,
    received_at  BIGINT NOT NULL,
    status       TEXT   NOT NULL DEFAULT 'Pending',
    attempts     INT    NOT NULL DEFAULT 0,
    locked_until BIGINT,
    done_at      BIGINT
);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX vk_inbox_pending ON vk_inbox (peer_id, id) WHERE status = 'Pending';
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX vk_inbox_done ON vk_inbox (done_at) WHERE status <> 'Pending';
-- +goose StatementEnd
-- +goose StatementBegin
-- Место Long Poll в очереди ВК (ts): после перезапуска чтение продолжается с
-- него, и всё, что пришло за паузу, забирается сразу. Строка одна.
CREATE TABLE vk_longpoll (
    id         INT    PRIMARY KEY CHECK (id = 1),
    ts         TEXT   NOT NULL,
    updated_at BIGINT NOT NULL
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE vk_longpoll;
-- +goose StatementEnd
-- +goose StatementBegin
DROP TABLE vk_inbox;
-- +goose StatementEnd
