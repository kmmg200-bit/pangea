-- +goose Up
-- +goose StatementBegin
-- Когда посылка легла на почту: банковская ячейка её не приняла. Пусто —
-- посылка ещё в пути: её только что отправили, или сразу разложить не вышло,
-- и она дождётся хода получателя. Почта хранит вещи 7 дней с того, как на неё
-- легла первая: у всех вещей на почте одно и то же время, и сгорают они
-- вместе, сами, без хода хозяина (см. pangea.service.parcel.Parcels).
ALTER TABLE parcels ADD COLUMN mailed_at BIGINT;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE parcels DROP COLUMN mailed_at;
-- +goose StatementEnd
