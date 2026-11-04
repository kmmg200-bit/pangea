-- +goose Up
-- При одностадийной оплате банк шлёт AUTHORIZED и CONFIRMED почти разом, и
-- нотификации разбирались параллельно: опоздавший AUTHORIZED затирал CONFIRMED.
-- Дублоны при этом выданы (выдача — только по CONFIRMED, у каждого такого заказа
-- есть donation_granted в event_log), а заказ в базе числился «деньги ещё в
-- пути». Теперь статус назад не ходит (PaymentStatus.replaces), а уже затёртые
-- возвращаем: выданный заказ в промежуточном статусе — подтверждённый.
UPDATE payments
   SET status = 'Confirmed', bank_status = 'CONFIRMED'
 WHERE granted
   AND status IN ('New', 'FormShowed', 'Authorized');

-- +goose Down
SELECT 1;
