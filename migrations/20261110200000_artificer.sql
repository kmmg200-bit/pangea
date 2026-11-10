-- +goose Up
-- +goose StatementBegin
-- «Артефактор» I–V — по числу разных записей в книге рецептов дома
-- (heroes.home_data -> 'recipes'): 10, 25, 50, 100, 150. Новым ранги выдаёт
-- куб при активации (AzatCube.activate), а тем, у кого книга уже собрана, —
-- эта миграция. Ключи в heroes.achievements — строкой через запятую.
WITH book AS (
  SELECT h.user_id,
         (SELECT count(DISTINCT e.v) FROM jsonb_array_elements(h.home_data -> 'recipes') AS e(v)) AS n
  FROM heroes h
  WHERE jsonb_typeof(h.home_data -> 'recipes') = 'array'
),
due AS (
  SELECT b.user_id, t.key
  FROM book b
  JOIN (VALUES (10, 'Artificer1'), (25, 'Artificer2'), (50, 'Artificer3'),
               (100, 'Artificer4'), (150, 'Artificer5')) AS t(need, key) ON b.n >= t.need
),
missing AS (
  SELECT d.user_id, string_agg(d.key, ',' ORDER BY d.key) AS keys
  FROM due d
  JOIN heroes h ON h.user_id = d.user_id
  WHERE NOT (d.key = ANY (string_to_array(h.achievements, ',')))
  GROUP BY d.user_id
)
UPDATE heroes h
SET achievements = CASE WHEN h.achievements = '' THEN m.keys ELSE h.achievements || ',' || m.keys END
FROM missing m
WHERE h.user_id = m.user_id;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
-- Достижения остаются: прошлая версия кода незнакомые ключи просто не видит.
SELECT 1;
-- +goose StatementEnd
