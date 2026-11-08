-- +goose Up
-- +goose StatementBegin
-- Чем герой закончил задание с развилкой — «Письмо Марисе», «Деревня
-- Мурлоков», «Пропавшая дружина» (см. QuestOutcome): по этим записям после
-- задания считается, сколько героев прошли его тем же путём. Одна запись на
-- героя и задание. `quest` — ключ задания (NpcQuest.key), `outcome` — исход
-- (QuestOutcome.entryName), `at` — когда закончил, мс; NULL — закончил раньше,
-- чем исходы начали записывать (перенесено ниже).
CREATE TABLE quest_outcomes (
    hero_id BIGINT      NOT NULL REFERENCES heroes (id) ON DELETE CASCADE,
    quest   VARCHAR(32) NOT NULL,
    outcome VARCHAR(32) NOT NULL,
    at      BIGINT,
    PRIMARY KEY (hero_id, quest)
);

CREATE INDEX quest_outcomes_quest_idx ON quest_outcomes (quest, outcome);

-- Старые прохождения. Исход восстанавливается по достижениям: каждое
-- выдаётся ровно на своей развилке (heroes.achievements — имена через
-- запятую). Концовки Марисы без достижения — тайник без Марисы, гибель от
-- коллектора — не восстановить: их не переносим.
INSERT INTO quest_outcomes (hero_id, quest, outcome)
SELECT id, 'marisa',
       CASE WHEN 'MarisaSavior' = ANY (string_to_array(achievements, ','))
            THEN 'saved' ELSE 'abandoned' END
FROM heroes
WHERE (npc_quests -> 'quests' -> 'marisa' ->> 'done') = 'true'
  AND string_to_array(achievements, ',') && ARRAY['MarisaSavior', 'Scoundrel'];

-- Мурлоки: «Гроза Мурлоков» после сданного снаряжения (счётчик сданного в
-- npc_quests больше нуля) — «сначала помогали, потом вырезали».
INSERT INTO quest_outcomes (hero_id, quest, outcome)
SELECT id, 'murloc',
       CASE WHEN 'MurlocFavorite' = ANY (string_to_array(achievements, ',')) THEN 'helped'
            WHEN COALESCE((npc_quests -> 'quests' -> 'murloc' ->> 'counter')::BIGINT, 0) > 0 THEN 'betrayed'
            ELSE 'raided' END
FROM heroes
WHERE (npc_quests -> 'quests' -> 'murloc' ->> 'done') = 'true'
  AND string_to_array(achievements, ',') && ARRAY['MurlocFavorite', 'MurlocBane'];

INSERT INTO quest_outcomes (hero_id, quest, outcome)
SELECT id, 'lostSquad',
       CASE WHEN 'SquadFriend' = ANY (string_to_array(achievements, ','))
            THEN 'ledOut' ELSE 'looted' END
FROM heroes
WHERE (npc_quests -> 'quests' -> 'lostSquad' ->> 'done') = 'true'
  AND string_to_array(achievements, ',') && ARRAY['SquadFriend', 'MineLooter'];
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE quest_outcomes;
-- +goose StatementEnd
