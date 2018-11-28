# Пример договора субаренды

Условия договора, описанные в файле subleaseexample.pl:

сторона_договора('01-18/САР', тип_стороны_договора(арендодатель),'Масковия').
сторона_договора('01-18/САР', тип_стороны_договора(арендатор), 'ТоргТех').

% Определение договора: договор(+НомерДоговора, +ТипДоговора, +ДатаДоговора, +СторонаЗакзачик, +СторонаИсполнитель)
договор('01-18/САР', тип_договора(субаренда), 2018-02-01,
	сторона_договора('01-18/САР', тип_стороны_договора(арендатор), 'ТоргТех'),
	сторона_договора('01-18/САР', тип_стороны_договора(арендодатель),'Масковия')).

% Договор аренды - основание для субаренды
договор('01-02/АР', тип_договора(аренда), 2017-03-31,
	сторона_договора('01-02/АР', тип_стороны_договора(арендатор), 'ТоргТех'),
	сторона_договора('01-02/АР', тип_стороны_договора(арендодатель), '(нет)')).
  
...

Инициализация:

1. Задать текущий договор:
?- [user].
задать_текущий_договор('01-18/САР').
^d


Проверки:

1. Получим сумму оплаты по договору на заданную дату когда есть уведомления на изменение платы на эту дату:
``` Prolog
?- сумма_оплаты_надату(НомерДоговора, 2018-11-25, ДатаИзменения, Сумма, Валюта).

НомерДоговора = '01-18/САР',
ДатаИзменения = 2018-10-5,
Сумма = 351000,
Валюта = валюта(rur) 
```

2. Получить параметры текущего договора
``` Prolog
?- атрибуты_договора(НомерДоговора, ТипДоговора, ДатаДоговора, Заказчик, Исполнитель).

НомерДоговора = '01-18/САР',
ТипДоговора = тип_договора(субаренда),
ДатаДоговора = 2018-2-1,
Заказчик = сторона_договора('01-18/САР', тип_стороны_договора(арендатор), 'ТоргТех'),
Исполнитель = сторона_договора('01-18/САР', тип_стороны_договора(арендодатель), 'Масковия').
```

3. Получить данные об оплате в следующем месяце:
``` Prolog
?- атрибуты_следующей_оплаты(НомерДоговора, ДатаОплаты, СуммаОплаты, Валюта).

НомерДоговора = '01-18/САР',
ДатаОплаты = 2018-12-5,
СуммаОплаты = 351000,
Валюта = валюта(rur)
```