{ **** UBPFD *********** by delphibase.endimus.com ****
>> Запуск одной копии приложения (Mutex-базированный)

Единственная функция Init_Mutex этого модуля (only_one.pas) создает мьютекс
с именем, переданным в параметре mid.
Возврат: true, если мьютекс создан (запущен первый экземпляр приложения)
или false, если уже имеется мьютекс с подобным именем (mid).

Особенности:
1. даже при "гибели" приложения все, относящиеся к нему мьютексы удаляются
с большой степенью вероятности.
2. Желательно "отметить" приложение в системе так, как указано в примере.
При таком подходе Ваше приложение почти со стапроцентной вероятностью
не будет запущено два раза.

Зависимости: Windows
Автор:       Роман Василенко, romix@nm.ru, Пятигорск
Copyright:   Роман Василенко
Дата:        14 июня 2002 г.
***************************************************** }

unit Only_one;

interface

function Init_Mutex(mid: string): boolean;

implementation

uses Windows;

var
  mut: thandle;

function mut_id(s: string): string;
var
  f: integer;
begin
  result := s;
  for f := 1 to length(s) do
    if result[f] = '\' then
      result[f] := '_';
end;

function Init_Mutex(mid: string): boolean;
begin
  Mut := CreateMutex(nil, false, pchar(mut_id(mid)));
  Result := not ((Mut = 0) or (GetLastError = ERROR_ALREADY_EXISTS));
end;

initialization
  mut := 0;
finalization
  if mut <> 0 then
    CloseHandle(mut);
end.
