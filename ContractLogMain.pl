% Логика проверки договора, тест №1
% Стандартные переменные:
% C - номер договора, строка
% S - номер этапа, целое число
% D - дата в формате Prolog Date(YYYY, MM, DD)
% Now - дата, на которую выполняем проверку 

% Зададим № договора; '1' - номер контракта, с которым работаем
contract('1').
current_contract(C) :- contract(C), C = '1'.

% Зададим дату начала договора
contract_start_date(C, D) :- current_contract(C), D = date(2016, 11, 18).
% Зададим дату конца договора 
contract_end_date(C, D) :- current_contract(C), D = date(2016, 12, 31).
% Зададим сумму договора
contract_sum(C, Sum) :- current_contract(C), Sum = 38143437.5.

% Зададим этапы договора: [1 и 2]
contract_stages(C, [1, 2]) :- current_contract(C).
contract_stage(C, S) :- contract_stages(C, SS), member(S, SS).

% Зададим даты начала и завершения этапов договора
contract_stage_start_date(C, S, D) :- contract_stage(C, S), S = 1, D = date(2016, 11, 18).
contract_stage_start_date(C, S, D) :- contract_stage(C, S), S = 2, D = date(2016, 11, 20).

contract_stage_end_date(C, S, D) :- contract_stage(C, S), S = 1, D = date(2016, 11, 20), !.
contract_stage_end_date(C, S, D) :- contract_stage(C, S), S = 2, D = date(2016, 11, 25), !.

% Проверка что дата завершения любого этапа меньше даты завершения договора
validate_contract_dates(C) :- contract_stages(C, L), contract_end_date(C, D), !, validate_contract_dates(C, L, D).
validate_contract_dates(C, [], _) :- current_contract(C).
validate_contract_dates(C, [H|T], D) :- contract_stage_end_date(C, H, D2), D2 @< D, validate_contract_dates(C, T, D).

% Зададим суммы этапов договора
contract_stage_sum(C, S, Sum) :- contract_stage(C, S), S = 1, Sum = 37380568.75.
contract_stage_sum(C, S, Sum) :- contract_stage(C, S), S = 2, Sum =   762868.75.

% Проверка, что дата Date истекла на дату Now
is_date_expired(Date, Now) :- date_time_stamp(Now, S2), date_time_stamp(Date, S1), S2 @> S1.
is_date_expired(Date, Now, DelayDays) :- date_time_stamp(Now, S2), date_time_stamp(Date, S1), S2+DelayDays*86400 @> S1.

% Возвращает сегодняшнюю дату
today(Date) :- get_time(Stamp), stamp_date_time(Stamp, D, 0), date_time_value(date, D, Date).

% Вычисляет количетство дней между датами
days_between(Date, Now, Days) :- date_time_stamp(Now, S2), date_time_stamp(Date, S1), Secs is S2 - S1, Days is Secs / 86400.

% Проверка, что контракт просрочен на дату Now
is_contract_expired(C, Now) :- contract_end_date(C, D), is_date_expired(D, Now).
% Проверка, что контракт просрочен на сегодня
is_contract_expired(C) :- today(D), is_contract_expired(C, D).

% Проверка, что этап контракта просрочен на дату Now
is_contract_stage_expired(C, S, Now) :- contract_stage_end_date(C, S, D), is_date_expired(D, Now).
% Проверка, что этап контракта просрочен
is_contract_stage_expired(C, S) :- contract_stage_end_date(C, S, Date),
	contract_stage_delivery_act(C, S, Now, _),
	is_date_expired(Date, Now).

% Проверка что сумма договора равна сумме сумм этапов
validate_contract_sum(C) :- contract_stages(C, [H|T]), contract_sum(C, CSum), sum_contract_stages_sum(C, [H|T], SSum), CSum == SSum, !.
sum_contract_stages_sum(C, [], Sum) :- current_contract(C), Sum = 0.
sum_contract_stages_sum(C, [H|T], Sum) :- contract_stage_sum(C, H, Sum1), sum_contract_stages_sum(C, T, Sum2), Sum is Sum1 + Sum2.

% Зададим условие оплаты по договору (отсрочка в днях)
contract_payment_condition(C, Days) :- current_contract(C), Days = 0.

% Зададим пени по контракту (10% годовых за каждый день просрочки)
contract_delivery_fine(C, Percent) :- current_contract(C), Percent = 10.
contract_payment_fine(C, Percent) :-  current_contract(C), Percent = 10.

% Зададим даты поставки по этапу (факт доставки)
contract_stage_delivery(C, S, D) :- contract_stage(C, S), S = 1, D = date(2016, 12, 20).
contract_stage_delivery(C, S, D) :- contract_stage(C, S), S = 2, D = date(2017, 01, 26).

% Зададим даты подписания актов на поставку
contract_stage_delivery_act(C, S, Date, Sum) :- S = 1, Date = date(2016, 12, 30), contract_stage_sum(C, S, Sum).
contract_stage_delivery_act(C, S, Date, Sum) :- S = 2, Date = date(2017, 02, 07), contract_stage_sum(C, S, Sum).

% Зададим дату оплаты по этапам
contract_payment(C, S, Date, Sum) :- contract_stage(C, S), S = 1, Date = date(2016, 12, 30), contract_stage_sum(C, S, Sum).
contract_payment(C, S, Date, Sum) :- contract_stage(C, S), S = 2, Date = date(2017, 10, 27), contract_stage_sum(C, S, Sum).

% Проверка, что оплата по этапу просрочена
is_contract_payment_expired(C, S) :- contract_stage_delivery_act(C, S, Date, _),
	contract_payment(C, S, Now, _),
	contract_payment_condition(C, PayDelayDays),
	is_date_expired(Date, Now, PayDelayDays).

% Вычисление количества дней просрочки поставки по этапу на заданную дату Now
% contract_delivery_delay(C, S, Now, Days) :- contract_stage_end_date(C, S, Date), days_between(Date, Now, Days).
% Вычисление количества дней просрочки поставки по этапу на сегодня
% contract_delivery_delay(C, S, Days) :- today(Now), contract_delivery_delay(C, S, Now, Days).

% Вычисление количества дней задержки между даной окончания этапа и датой подписания акта
calculate_delivery_delay(C, S, Days) :- contract_stage_delivery_act(C, S, Now, _),
	is_contract_stage_expired(C, S, Now),
	contract_stage_end_date(C, S, Date), 
	days_between(Date, Now, Days).

% Вычисление количества дней просрочки оплаты по этапу на заданную дату Now и на сегодня
% contract_payment_delay(C, S, Now, Days) :- contract_stage_delivery_act(C, S, Date, _), days_between(Date, Now, Days).
% contract_payment_delay(C, S, Days) :- today(Now), contract_payment_delay(C, S, Now, Days).
calculate_payment_delay(C, S, Days) :- is_contract_payment_expired(C, S),
	contract_stage_delivery_act(C, S, Date, _),
	contract_payment(C, S, Now, _),
	days_between(Date, Now, Days).

% Вычислим размер пени Fine за поставку по этапу
calulate_delivery_fine(C, S, Fine) :- is_contract_stage_expired(C, S), 
	contract_delivery_fine(C, Percent), 
	contract_stage_sum(C, S, Sum),
	calculate_delivery_delay(C, S, Days),
	Fine is Sum*Percent/100*Days/365.

% Вычислим размер пени Fine за оплату по этапу
calulate_payment_fine(C, S, Fine) :- is_contract_payment_expired(C, S), 
	contract_payment_fine(C, Percent), 
	contract_stage_sum(C, S, Sum),
	contract_payment_condition(C, PayDelayDays),
	calculate_payment_delay(C, S, Days),
	Fine is Sum*Percent/100*(Days-PayDelayDays)/365.

% Зададим срок исковой давности (1 год = 365 дней)
contract_statue_of_limitations(C, Days) :- current_contract(C), Days = 365.

% Вычислим срок исковой давности по поставке для договора (пока тупо от даты окончания договора)
get_contract_statute_of_delivery(C, Date) :- contract_end_date(C, D1),
	contract_statue_of_limitations(C, Days),
	date_time_stamp(D1, S1),
	S2 is S1 + (Days + 1)*86400,
	stamp_date_time(S2, D2, 0), date_time_value(date, D2, Date).

% Проверим что срок исковой давности по поставке не вышел
is_contract_in_statute_of_delivery(C) :- get_contract_statute_of_delivery(C, Date),
	today(Now),
	not(is_date_expired(Date, Now)).

% Вычислим срок исковой давности по оплате для договора (пока тупо от даты окончания договора)
get_contract_statute_of_payment(C, Date) :- contract_end_date(C, D1),
	contract_statue_of_limitations(C, Days),
	date_time_stamp(D1, S1),
	S2 is S1 + (Days + 1)*86400,
	stamp_date_time(S2, D2, 0), date_time_value(date, D2, Date).

% Проверим что срок исковой давности по оплате не вышел
is_contract_in_statute_of_payment(C) :- get_contract_statute_of_payment(C, Date),
	today(Now),
	not(is_date_expired(Date, Now)).

% main для проверки условий договора // требуется в SublimeText
main :- validate_contract_sum('1'), validate_contract_dates('1').
