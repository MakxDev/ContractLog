% Веб-сервер

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).

%:- consult(subleaseexample).

:- http_handler(root(sublease_info), show_sublease_info, []).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

show_sublease_info(Request) :-
        reply(Request).

get_sublease_info(НомерДоговора, ТипДоговора, ДатаДоговора, ТипСтороны1, КодЮрлица1,ТипСтороны2, КодЮрлица2) :-
	атрибуты_договора(НомерДоговора, тип_договора(ТипДоговора), ДатаДоговора,
		сторона_договора(НомерДоговора, тип_стороны_договора(ТипСтороны1), КодЮрлица1),
		сторона_договора(НомерДоговора, тип_стороны_договора(ТипСтороны2), КодЮрлица2)).

get_next_payment_info(НомерДоговора, ДатаОплаты, СуммаОплаты, КодВалюты) :-
	атрибуты_следующей_оплаты(НомерДоговора, ДатаОплаты, СуммаОплаты, Валюта),
	код_валюты(Валюта, КодВалюты).

reply(Request) :-
	format('Content-type: text/html~n~n', []),
	format('<html>~n', []),
	%format('<table border=1>~n'),
	print_request(Request),
	%format('~n</table>~n'),
	format('</html>~n', []).

print_request(_Request) :-
        задать_текущий_договор('01-18/САР'),
        get_sublease_info(НомерДоговора, ТипДоговора, ДатаДоговора, ТипСтороны1, КодЮрлица1,ТипСтороны2, КодЮрлица2),
        format('<h2>НомерДоговора: ~w~n</h2><br>ТипДоговора: ~w~n<br>ДатаДоговора: ~w~n<br>~w: ~w~n<br>~w: ~w~n<br>', [НомерДоговора, ТипДоговора, ДатаДоговора, ТипСтороны1, КодЮрлица1, ТипСтороны2, КодЮрлица2]),
        get_next_payment_info(НомерДоговора, ДатаОплаты, СуммаОплаты, Валюта),
        format('Дата оплаты: ~w~nСумма оплаты: ~w ~w~n<br>', [ДатаОплаты, СуммаОплаты, Валюта]).
