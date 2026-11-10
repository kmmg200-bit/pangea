-- +goose Up
-- +goose StatementBegin
-- «Любимец Проекта» — тому, кто хоть раз купил каждую из семи сумм доната
-- (200, 500, 1000, 2000, 4000, 6000 и 10 000 ₽; см. DonationTiers). Новым его
-- ставит сама выдача (PaymentDaoLive.grantOnce), а тем, кто собрал все семь
-- раньше, — эта миграция. Ключи в heroes.achievements — строкой через запятую.
UPDATE heroes h
SET achievements = CASE WHEN h.achievements = '' THEN 'ProjectFavorite'
                        ELSE h.achievements || ',ProjectFavorite' END
WHERE (SELECT count(DISTINCT p.amount_kopecks) FROM payments p
       WHERE p.user_id = h.user_id AND p.granted
         AND p.amount_kopecks IN (20000, 50000, 100000, 200000, 400000, 600000, 1000000)) = 7
  AND NOT ('ProjectFavorite' = ANY (string_to_array(h.achievements, ',')));
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
-- Достижение остаётся: прошлая версия кода незнакомый ключ просто не видит.
SELECT 1;
-- +goose StatementEnd
