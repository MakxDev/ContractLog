%
% Библиотека по работе с датами
%

:- module(date_lib,
	[
		is_date_expired/2,
		is_date_expired/3,
		is_date_expired_ymd/2,
		today/1,
		days_between/3,
		date_from_text/2,
		get_date_value/3,
		get_today_date_value/2,
		date2ymd/2,
		ymd2date/2,
		find_last_date/2
	]).


% Для коррекной работы с русским языком нужно сохранять в кодировке UTF-8 BOM
encoding(utf8).

%:- use_module(library(error)).

% Проверка, что дата Date истекла на дату Now
is_date_expired(Date, Now) :- date_time_stamp(Now, S2), date_time_stamp(Date, S1), S2 @> S1.

% Проверка, что дата DateYMD в формате YYYY-MM-DD истекла на дату NowYMD в формате YYYY-MM-DD
is_date_expired_ymd(DateYMD, NowYMD) :- ymd2date(DateYMD, Date), ymd2date(NowYMD, Now), is_date_expired(Date, Now).

% Проверка, что дата Date истекла на дату Now + DelayDays
is_date_expired(Date, Now, DelayDays) :- date_time_stamp(Now, S2), date_time_stamp(Date, S1), S2+DelayDays*86400 @> S1.

% Возвращает сегодняшнюю дату
today(Date) :- get_time(Stamp), stamp_date_time(Stamp, D, local), date_time_value(date, D, Date).

% Вычисляет количетство дней между датами
days_between(Date, Now, Days) :- date_time_stamp(Now, S2), date_time_stamp(Date, S1), Secs is S2 - S1, Days is Secs / 86400.

% Формирует дату из тестовой строки в формате YYYY-MM-DD
date_from_text(TXT, Date) :- parse_time(TXT, iso_8601, Stamp), stamp_date_time(Stamp, D, local), date_time_value(date, D, Date).

% Возвращает в Value элемент даты DateTime, заданный параметром key: day, month, year, date
get_date_value(Key, DateTime, Value) :- date_time_value(Key, DateTime, Value).

% Возвращает элемент сегодняшей даты, заданный параметром key: day, month, year, date
get_today_date_value(Key, Value) :- get_time(Stamp), stamp_date_time(Stamp, DateTime, local), get_date_value(Key, DateTime, Value).

% Конвертируем переменную типа date(Y,M,D) в YYYY-MM-DD: date2YMD(+Date, -YMD)
date2ymd(Date, YMD) :- date_time_stamp(Date, Stamp), stamp_date_time(Stamp, D, local),
	D = date(YYYY,MM,DD,_,_,_,_,_,_),
	YMD = YYYY-MM-DD.
% Конвертируем переменную типа YYYY-MM-DD в date(Y,M,D): ymd2date(YMD, -Date)
ymd2date(YYYY-MM-DD, Date) :- Date = date(YYYY, MM, DD),!.

% Находит максимальную дату LastDate в списке ListOfDates: find_last_date(+[ListOfDates], -LastDate)
find_last_date(ListOfDates, LastDate) :-
	max_member(LastDate, ListOfDates).

