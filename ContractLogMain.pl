% Логика проверки договора, тест №1

% '1' - номер контракта, с которым работаем
contract('1').
% Дата начала договора 13.01.2000
contract_start_date(C, D) :- contract(C), D = date(2000, 01, 13).
% Дата конца договора 31.03.2000
contract_end_date(C, D) :- contract(C), D = date(2000, 03, 31).

% Этапы договора: 1 и 2
contract_stage(C, 1) :- contract(C).
contract_stage(C, 2) :- contract(C).

% Даты начала и завершения этапов договора
contract_stage_start_date(C, S, D) :- contract_stage(C, S), S = 1, D = date(2000, 01, 13), !.
contract_stage_start_date(C, S, D) :- contract_stage(C, S), S = 2, D = date(2000, 02, 21), !.

contract_stage_end_date(C, S, D) :- contract_stage(C, S), S = 1, D = date(2000, 02, 20), !.
contract_stage_end_date(C, S, D) :- contract_stage(C, S), S = 2, D = date(2000, 03, 15), !.

% Проверка, что дата Date истекла на дату Now
is_date_expired(Date, Now) :- date_time_stamp(Now, S2), date_time_stamp(Date, S1), S2 @> S1.

% Возвращает сегодняшнюю дату
today(Date) :- get_time(Stamp),
    stamp_date_time(Stamp, D, 0),
    date_time_value(date, D, Date).

% Проверка, что контракт просрочен на дату Now
is_contract_expired(C, Now) :- contract_end_date(C, D), is_date_expired(D, Now).
% Проверка, что контракт просрочен на сегодня
is_contract_expired(C) :- today(D), is_contract_expired(C, D).

% Проверка, что этап контракта просрочен на дату Now
is_contract_stage_expired(C, S, Now) :- contract_stage_end_date(C, S, D), is_date_expired(D, Now).

% Проверка, что этап контракта просрочен на сегодня
is_contract_stage_expired(C, S) :- today(D), is_contract_stage_expired(C, S, D).

% main для проверки в SublimeText
main :- is_contract_stage_expired('1', 1).
