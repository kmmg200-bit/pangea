-- +goose Up
-- +goose StatementBegin
-- Бой перед прологом убран: «Начать» ведёт сразу в пролог. Кто стоит в этом
-- бою (heroes.active_battle -> 'story' = 'prologueHouse', см. PrologueFight),
-- возвращается в начало регистрации: бой и отметка его победы в scene_data
-- сброшены. Настоящий герой боем не тронут — дрался в чужом теле, которое
-- лежит в записи боя (BattleBody), — откатывать больше нечего. На первое же
-- нажатие регистрация покажет приветствие с «Начать».
UPDATE heroes
SET state = 'Registration', active_battle = NULL, scene_data = NULL
WHERE state = 'Battle'
  AND active_battle ->> 'story' = 'prologueHouse';
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
-- Бой не вернуть, да и незачем: прошлая версия кода ведёт героя в регистрации
-- так же — приветствие, «Начать».
SELECT 1;
-- +goose StatementEnd
