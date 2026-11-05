-- +goose Up
-- Пыль от «Сломать камень» на надетой вещи и от рассыпавшихся Огня с Холодом
-- ложилась в сумку не сохранённой в базу — с номером −1. Горсти разных видов
-- делили этот номер, и снятие одной (в куб, в ячейку, в бочку) убирало все
-- разом. Код теперь сохраняет каждую горсть, а уже лежащим вещам с
-- отрицательным номером даём свой — так же, как это сделал бы
-- ItemRepository.persist: строка в items, её номер — вещи. Номер 0 не трогаем:
-- у него в коде свой смысл.
-- +goose StatementBegin
DO $$
DECLARE
  tbl    text;
  r      record;
  e      jsonb;
  acc    jsonb;
  new_id bigint;
BEGIN
  FOREACH tbl IN ARRAY ARRAY['inventories', 'barrels', 'bank_vaults'] LOOP
    FOR r IN EXECUTE format(
      'SELECT id, hero_id, items FROM %I t
        WHERE EXISTS (SELECT 1 FROM jsonb_array_elements(t.items->''data'') x WHERE (x->>''id'')::bigint < 0)', tbl)
    LOOP
      acc := '[]'::jsonb;
      FOR e IN SELECT value FROM jsonb_array_elements(r.items->'data') LOOP
        IF (e->>'id')::bigint < 0 THEN
          INSERT INTO items (hero_id, name, lvl, rarity, item_type, attack, accuracy, energy, armor, defence, evasion)
          VALUES (r.hero_id, e->>'name', (e->>'lvl')::bigint, e->>'rarity', e->>'itemType',
                  (e->>'attack')::bigint, (e->>'accuracy')::bigint, (e->>'energy')::bigint,
                  (e->>'armor')::bigint, (e->>'defence')::bigint, (e->>'evasion')::bigint)
          RETURNING id INTO new_id;
          e := jsonb_set(e, '{id}', to_jsonb(new_id));
        END IF;
        acc := acc || jsonb_build_array(e);
      END LOOP;
      EXECUTE format('UPDATE %I SET items = jsonb_set(items, ''{data}'', $1) WHERE id = $2', tbl) USING acc, r.id;
    END LOOP;
  END LOOP;
END $$;
-- +goose StatementEnd

-- +goose Down
SELECT 1;
