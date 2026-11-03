-- +goose Up
-- +goose StatementBegin
-- Билеты за активность в группе ВК вместо очков конкурса. Золотой — за лайк
-- свежего поста, по одному на пост (`slot` — номер поста); серебряный — за
-- комментарий под свежим постом, по одному в сутки (`slot` — начало московских
-- суток, мс). `week` — начало недели (мс), в которую билет выдан: по ней билеты
-- и оплачиваются в конце недели. Подведённые недели — в прежней activity_prizes.
CREATE TABLE activity_tickets (
    user_id BIGINT      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    kind    VARCHAR(16) NOT NULL,
    slot    BIGINT      NOT NULL,
    week    BIGINT      NOT NULL,
    at      BIGINT      NOT NULL,
    PRIMARY KEY (user_id, kind, slot)
);

CREATE INDEX activity_tickets_week_idx ON activity_tickets (week);

-- Очки прежнего конкурса больше не считаются.
DROP TABLE activity_actions;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
CREATE TABLE activity_actions (
    user_id BIGINT      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    post_id BIGINT      NOT NULL,
    kind    VARCHAR(16) NOT NULL,
    points  BIGINT      NOT NULL,
    week    BIGINT      NOT NULL,
    at      BIGINT      NOT NULL,
    PRIMARY KEY (user_id, post_id, kind)
);

CREATE INDEX activity_actions_week_idx ON activity_actions (week);

DROP TABLE activity_tickets;
-- +goose StatementEnd
