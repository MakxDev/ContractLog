%
% Библиотека по работе с датами
%

:- module(date_lib,
	[
		is_date_expired/2,
		is_date_expired/3,
		today/1,
		days_between/3,
		date_from_text/2
	]).


% Для коррекной работы с русским языком нужно сохранять в кодировке UTF-8 BOM
encoding(utf8).

%:- use_module(library(error)).

% Проверка, что дата Date истекла на дату Now
is_date_expired(Date, Now) :- date_time_stamp(Now, S2), date_time_stamp(Date, S1), S2 @> S1.

% Проверка, что дата Date истекла на дату Now + DelayDays
is_date_expired(Date, Now, DelayDays) :- date_time_stamp(Now, S2), date_time_stamp(Date, S1), S2+DelayDays*86400 @> S1.

% Возвращает сегодняшнюю дату
today(Date) :- get_time(Stamp), stamp_date_time(Stamp, D, 0), date_time_value(date, D, Date).

% Вычисляет количетство дней между датами
days_between(Date, Now, Days) :- date_time_stamp(Now, S2), date_time_stamp(Date, S1), Secs is S2 - S1, Days is Secs / 86400.

% Формирует дату из тестовой строки в формате YYYY-MM-DD
date_from_text(TXT, Date) :- parse_time(TXT, iso_8601, Stamp), stamp_date_time(Stamp, D, 0), date_time_value(date, D, Date).
