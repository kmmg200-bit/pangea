-- +goose Up
-- Учёт напоминаний о прологе (админ-панель, «Напомнить о прологе», см.
-- PrologueReminder): по строке на рассылку и по строке на адресата — чем
-- кончилась отправка и когда он после неё вернулся (нажал что-то в прологе),
-- выбрал расу и прошёл пролог (вышел из сторожки). Отметки ставит игра
-- (RegistrationState) — последней рассылке игрока, если она не старше недели.
-- По адресатам же рассылка пропускает тех, кому напоминали за неделю.
--
-- outcome адресата: pending — письмо ещё в пути, sent — дошло, closed — игрок
-- закрыл сообщения от сообщества (900–902), failed — сбой ВК.
-- +goose StatementBegin
CREATE TABLE prologue_reminders (
    id            BIGSERIAL   PRIMARY KEY,
    started_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
    finished_at   TIMESTAMPTZ,
    admin_user_id BIGINT,
    idle_minutes  INT         NOT NULL,
    message       TEXT        NOT NULL,
    skipped       INT         NOT NULL DEFAULT 0,
    sent          INT         NOT NULL DEFAULT 0,
    closed        INT         NOT NULL DEFAULT 0,
    failed        INT         NOT NULL DEFAULT 0
);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE TABLE prologue_reminder_recipients (
    reminder_id  BIGINT      NOT NULL REFERENCES prologue_reminders (id),
    user_id      BIGINT      NOT NULL,
    outcome      TEXT        NOT NULL DEFAULT 'pending',
    returned_at  TIMESTAMPTZ,
    race_at      TIMESTAMPTZ,
    completed_at TIMESTAMPTZ,
    PRIMARY KEY (reminder_id, user_id)
);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX prologue_reminder_recipients_user ON prologue_reminder_recipients (user_id, reminder_id);
-- +goose StatementEnd
-- Дашборд «Воронка» читает обе таблицы целиком: в них ни почты, ни вещей.
-- +goose StatementBegin
GRANT SELECT ON prologue_reminders TO pangea_grafana;
-- +goose StatementEnd
-- +goose StatementBegin
GRANT SELECT ON prologue_reminder_recipients TO pangea_grafana;
-- +goose StatementEnd
-- Рассылка 21.11.2026 (20:45 МСК) ушла до учёта — восстанавливаем её по базе.
-- Начало — первое недошедшее письмо (send_failures, после повтора через 5 с)
-- минус 30 с. Адресаты — кто в начале был в прологе и за час до него не писал
-- боту: всё ещё в регистрации или вышел из сторожки (ArrivalStand) уже после;
-- плюс все, кому письмо не дошло. Вернулся — первое входящее после начала,
-- прошёл — первое нажатие ArrivalStand, раса — race_selected в журнале.
-- Входящие хранятся неделю (InboxWorker.Keep), так что позже 28.11 часть
-- возвращений не восстановится. Не было рассылки (чистая база) — ничего.
-- +goose StatementBegin
WITH t AS (
    SELECT min(created_at) - 30000 AS ms, max(created_at) AS last_ms
    FROM send_failures
    WHERE message_text LIKE '💭 Ваш герой о чём-то задумался%'
),
closed AS (
    SELECT DISTINCT user_id FROM send_failures
    WHERE message_text LIKE '💭 Ваш герой о чём-то задумался%' AND user_id IS NOT NULL
),
inbox AS (
    SELECT i.peer_id,
           bool_or(i.received_at < t.ms) AS wrote_before,
           min(i.received_at) FILTER (WHERE i.received_at >= t.ms) AS returned_ms,
           min(i.received_at) FILTER (WHERE i.received_at >= t.ms
                                        AND i.event -> 'object' -> 'message' ->> 'payload' LIKE '%"ArrivalStand"%') AS completed_ms
    FROM vk_inbox i, t
    WHERE t.ms IS NOT NULL AND i.kind = 'message' AND i.received_at >= t.ms - 3600000
    GROUP BY i.peer_id
),
race AS (
    SELECT e.user_id, min(e.occurred_at) AS at
    FROM event_log e, t
    WHERE t.ms IS NOT NULL AND e.event_type = 'race_selected'
      AND e.occurred_at >= to_timestamp(t.ms / 1000.0) AT TIME ZONE current_setting('TimeZone')
    GROUP BY e.user_id
),
cohort AS (
    SELECT u.id AS user_id,
           c.user_id IS NOT NULL AS closed,
           to_timestamp(i.returned_ms / 1000.0) AS returned_at,
           r.at AT TIME ZONE current_setting('TimeZone') AS race_at,
           to_timestamp(i.completed_ms / 1000.0) AS completed_at
    FROM users u
    JOIN heroes h ON h.user_id = u.id
    CROSS JOIN t
    LEFT JOIN inbox i ON i.peer_id = CASE WHEN u.vk_id ~ '^[0-9]+$' THEN u.vk_id::bigint END
    LEFT JOIN race r ON r.user_id = u.id
    LEFT JOIN closed c ON c.user_id = u.id
    WHERE t.ms IS NOT NULL
      AND (c.user_id IS NOT NULL
           OR (u.vk_id ~ '^[0-9]+$'
               AND u.created_at < to_timestamp((t.ms - 3600000) / 1000.0)
               AND NOT coalesce(i.wrote_before, false)
               AND (h.state = 'Registration' OR i.completed_ms IS NOT NULL)))
),
run AS (
    INSERT INTO prologue_reminders (started_at, finished_at, admin_user_id, idle_minutes, message, sent, closed, failed)
    SELECT to_timestamp(t.ms / 1000.0), to_timestamp((t.last_ms + 5000) / 1000.0), NULL, 60,
           '💭 Ваш герой о чём-то задумался и застыл на полпути. Пора продолжить — кнопки ждут внизу, под окном чата.',
           (SELECT count(*) FROM cohort WHERE NOT closed),
           (SELECT count(*) FROM cohort WHERE closed),
           0
    FROM t
    WHERE t.ms IS NOT NULL
    RETURNING id
)
INSERT INTO prologue_reminder_recipients (reminder_id, user_id, outcome, returned_at, race_at, completed_at)
SELECT run.id, c.user_id, CASE WHEN c.closed THEN 'closed' ELSE 'sent' END, c.returned_at, c.race_at, c.completed_at
FROM run CROSS JOIN cohort c;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE IF EXISTS prologue_reminder_recipients;
-- +goose StatementEnd
-- +goose StatementBegin
DROP TABLE IF EXISTS prologue_reminders;
-- +goose StatementEnd
