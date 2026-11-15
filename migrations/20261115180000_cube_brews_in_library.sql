-- +goose Up
-- +goose StatementBegin
-- То, что варит только куб (BrewKind.cubeOnly: в рецепте не одни травы), —
-- не стандартный отвар, а рецепт куба: он в книге рецептов библиотеки
-- (heroes.home_data -> 'recipes') и в её счёте — «Артефактор», рейтинг
-- «Рецепты». С 11 ноября (#430) куб такие записи не делал. Кто «Яд красного
-- паука», «Паучью сыворотку» или «Гнилой зов» уже варил (heroes.lore_data ->
-- 'brewed'), а записи о нём в книге нет, — тому запись, ровно такая, какую
-- пишет куб (AzatCube.note; сверяет CubeBrewLibrarySpec). По отвару за раз:
-- одним UPDATE с тремя строками герой, варивший два из них, получил бы одну.
UPDATE heroes h
SET home_data = jsonb_set(coalesce(h.home_data, '{}'::jsonb), '{recipes}',
      (CASE WHEN jsonb_typeof(h.home_data -> 'recipes') = 'array' THEN h.home_data -> 'recipes' ELSE '[]'::jsonb END)
        || jsonb_build_array('{"ingredients":["Железа красного паука","Ядовитая железа"],"result":"Яд красного паука","count":1}'::jsonb))
WHERE jsonb_typeof(h.lore_data -> 'brewed') = 'array'
  AND h.lore_data -> 'brewed' @> '["RedVenomSalve"]'::jsonb
  AND NOT EXISTS (
    SELECT 1
    FROM jsonb_array_elements(CASE WHEN jsonb_typeof(h.home_data -> 'recipes') = 'array'
                                   THEN h.home_data -> 'recipes' ELSE '[]'::jsonb END) AS e(v)
    WHERE e.v ->> 'result' = 'Яд красного паука');

UPDATE heroes h
SET home_data = jsonb_set(coalesce(h.home_data, '{}'::jsonb), '{recipes}',
      (CASE WHEN jsonb_typeof(h.home_data -> 'recipes') = 'array' THEN h.home_data -> 'recipes' ELSE '[]'::jsonb END)
        || jsonb_build_array('{"ingredients":["Паучий цвет","Ядовитая железа","Шалфей"],"result":"Паучья сыворотка","count":1}'::jsonb))
WHERE jsonb_typeof(h.lore_data -> 'brewed') = 'array'
  AND h.lore_data -> 'brewed' @> '["SpiderSerum"]'::jsonb
  AND NOT EXISTS (
    SELECT 1
    FROM jsonb_array_elements(CASE WHEN jsonb_typeof(h.home_data -> 'recipes') = 'array'
                                   THEN h.home_data -> 'recipes' ELSE '[]'::jsonb END) AS e(v)
    WHERE e.v ->> 'result' = 'Паучья сыворотка');

UPDATE heroes h
SET home_data = jsonb_set(coalesce(h.home_data, '{}'::jsonb), '{recipes}',
      (CASE WHEN jsonb_typeof(h.home_data -> 'recipes') = 'array' THEN h.home_data -> 'recipes' ELSE '[]'::jsonb END)
        || jsonb_build_array('{"ingredients":["Чумные черви","Крысиный хвост","Крысиный хвост","Шкурка крысы","Шкурка крысы","Шкурка крысы"],"result":"Гнилой зов","count":1}'::jsonb))
WHERE jsonb_typeof(h.lore_data -> 'brewed') = 'array'
  AND h.lore_data -> 'brewed' @> '["RotCall"]'::jsonb
  AND NOT EXISTS (
    SELECT 1
    FROM jsonb_array_elements(CASE WHEN jsonb_typeof(h.home_data -> 'recipes') = 'array'
                                   THEN h.home_data -> 'recipes' ELSE '[]'::jsonb END) AS e(v)
    WHERE e.v ->> 'result' = 'Гнилой зов');
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
-- Записи остаются: прошлая версия кода считает их отварами — не показывает и
-- не считает (см. RecipeNote.BrewNames).
SELECT 1;
-- +goose StatementEnd
