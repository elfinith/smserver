{***********************************************************************}
{                         Класс TRyTimer.                               }
{                                                                       }
{ Автор   : Алексей Румянцев.                                           }
{ E-mail  : skitl@mail.ru                                               }
{-----------------------------------------------------------------------}
{ Описание:                                                             }
{   Обертка для стандартного Windows'таймера.                           }
{ Отличия от TTimer'а:                                                  }
{   * не тянет за сабой TComponent, uses Forms, Application             }
{-----------------------------------------------------------------------}
{    Специально для Королевства Дельфи http://www.delphikingdom.com     }
{-----------------------------------------------------------------------}
{ Написано на Delphi5. Тестировалось на Win98. WinXP.                   }
{ В случае обнаружения ошибки или несовместимости с другими версиями    }
{ Delphi и Windows, просьба сообщить автору.                            }
{-----------------------------------------------------------------------}
{ Тестировал путем личного использования (т.е. в личных целях использую }
{ только TRyTimer), поэтому, все обнаруженные ошибки сразу же и устранял}
{***********************************************************************}

unit RyTimer;

interface

uses
  Windows, SysUtils, Classes;

type

  { TRyTimer }

  TRyTimer = class(TObject)
  private
    FActive: Boolean;
    FHandle: Integer;
    FInterval: Integer;
    FOnTimer: TNotifyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetInterval(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property  Active: Boolean read FActive write SetActive;
    property  Interval: Integer read FInterval write SetInterval;
    property  OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

{Дополнительные процедуры------------------------------------------------------}

{Процедура вызывает досрочно OnTimer.}
procedure TimerExecute(Timer: TRyTimer; CheckActive: Boolean = True);
  {Указатель на Timer__|                | }
  {"Проверятель" активности Timer'а_____| }
  {Если Timer не активный и
     а. CheckActive = True (т.е. "проверятель" включен),
        то никаких действий предприниматься не будет.
     б. CheckActive = False (т.е. "проверятель" выключен),
        то будет вызван Timer.OnTimer}

{Процедура останавливает Timer и, если DoActive = True,
 запускает его занового}{при этом OnTimer не вызывается}
procedure TimerReFresh(Timer : TRyTimer; DoActive: Boolean = True);
{не знаю, может быть несовсем удачное имя для функции подобрал,
 но другого в голову не приходит}
{------------------------------------------------------------------------------}

implementation

type
  TRyTimerList = class(TList)
  protected
    function  Get(Index: Integer): TRyTimer;
    procedure Put(Index: Integer; Item: TRyTimer);
  public
    property  Items[Index: Integer]: TRyTimer read Get write Put; default;
  end;

var {Список всех созданныт таймеров в приложении}
  TimerList: TRyTimerList;

{процедура, обрабытывающая сооющения, поступающие от таймеров.}
procedure TimerProc(HWND, uMsg, idEvent, dwTime : Integer); stdcall;
var
  I: Integer;
  Timer: TRyTimer;
begin
  {Бежим по всем созданным Timer'ам}
  I := 0;
  while I < TimerList.Count do
  begin
    Timer := TimerList.Items[I];
    if (Timer <> nil) and {нулевые (удаленные) пропускаем}
       (idEvent = Timer.FHandle) and {ищем Timer на чье имя пришло сообщение}
       (Assigned(Timer.OnTimer)) then {если Timer не холостой}
    begin
      Timer.OnTimer(Timer);
      Exit; {выполняем и уходим}
    end;
    Inc(I);
  end;
end;

{ TRyTimer }

constructor TRyTimer.Create;
begin
  inherited;
  FActive := False;
  FInterval := 1000; {}
  FOnTimer := nil;
  TimerList.Add(Self); {расширяем список таймеров}
end;

destructor TRyTimer.Destroy;
begin
  Active := False;
  TimerList.Remove(Self); {удаляем таймер из списка}
  inherited;
end;

procedure TRyTimer.SetActive(const Value: Boolean);
begin
  if FActive = Value then Exit;
  if FActive then {если до этого таймер работал}
  begin
    KillTimer(0, FHandle); {убиваем WinTimer}
    FHandle := 0; {!}
  end;
  if Value then {если активизируем наш Timer}
     {создаем WinTimer и получаем его Handle}
     FHandle := SetTimer(0, 0, FInterval, @TimerProc);
  FActive := Value;
end;

procedure TRyTimer.SetInterval(const Value: Integer);
var
  OldActive: Boolean;
begin
  OldActive := Active;
  Active := False;
  FInterval := Value;
  Active := OldActive;
end;

procedure TRyTimer.Start;
begin
  Active := True
end;

procedure TRyTimer.Stop;
begin
  Active := False
end;

{Дополнительные процедуры------------------------------------------------------}

procedure TimerExecute(Timer: TRyTimer; CheckActive: Boolean = True);
begin
  if Timer <> nil then
  with Timer do
    if CheckActive and (not Active) then Exit
    else
    if Assigned(OnTimer) then OnTimer(Timer)
end;

procedure TimerReFresh(Timer : TRyTimer; DoActive: Boolean = True);
begin
  if Timer <> nil then
  with Timer do
  begin
    Stop;
    if DoActive then Start;
  end
end;{--------------------------------------------------------------------------}

{ TRyTimerList }

function TRyTimerList.Get(Index: Integer): TRyTimer;
begin
  Result := TRyTimer(inherited Get(Index))
end;

procedure TRyTimerList.Put(Index: Integer; Item: TRyTimer);
begin
  inherited Put(Index, Item)
end;

initialization
  TimerList := TRyTimerList.Create;

finalization
  while TimerList.Count > 0 do {если Count > 0, то где-то вы забыли
  уничтожить какой-то из таймеров или несколько сразу, вобщем это
  не страшно, но если представить, что у вас сотню раз создается
  какой-то объект и для него каждый раз создается таймер,
  а при уничтожении этого объекта забывается уничтожить его таймер,
  то это не есть хорошо, и ради порядка лучше найти то место и
  освободить ресурсы - MyTimer.Free}
    TimerList.Items[TimerList.Count - 1].Free; {здесь, при закрытии
    приложения, мы всеже, на всякий случай, проследим за забытыми
    таймерами и уничтожим их}
  TimerList.Free; {The End}

end.

