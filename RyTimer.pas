{***********************************************************************}
{                         ����� TRyTimer.                               }
{                                                                       }
{ �����   : ������� ��������.                                           }
{ E-mail  : skitl@mail.ru                                               }
{-----------------------------------------------------------------------}
{ ��������:                                                             }
{   ������� ��� ������������ Windows'�������.                           }
{ ������� �� TTimer'�:                                                  }
{   * �� ����� �� ����� TComponent, uses Forms, Application             }
{-----------------------------------------------------------------------}
{    ���������� ��� ����������� ������ http://www.delphikingdom.com     }
{-----------------------------------------------------------------------}
{ �������� �� Delphi5. ������������� �� Win98. WinXP.                   }
{ � ������ ����������� ������ ��� ��������������� � ������� ��������    }
{ Delphi � Windows, ������� �������� ������.                            }
{-----------------------------------------------------------------------}
{ ���������� ����� ������� ������������� (�.�. � ������ ����� ��������� }
{ ������ TRyTimer), �������, ��� ������������ ������ ����� �� � ��������}
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

{�������������� ���������------------------------------------------------------}

{��������� �������� �������� OnTimer.}
procedure TimerExecute(Timer: TRyTimer; CheckActive: Boolean = True);
  {��������� �� Timer__|                | }
  {"�����������" ���������� Timer'�_____| }
  {���� Timer �� �������� �
     �. CheckActive = True (�.�. "�����������" �������),
        �� ������� �������� ��������������� �� �����.
     �. CheckActive = False (�.�. "�����������" ��������),
        �� ����� ������ Timer.OnTimer}

{��������� ������������� Timer �, ���� DoActive = True,
 ��������� ��� ��������}{��� ���� OnTimer �� ����������}
procedure TimerReFresh(Timer : TRyTimer; DoActive: Boolean = True);
{�� ����, ����� ���� �������� ������� ��� ��� ������� ��������,
 �� ������� � ������ �� ��������}
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

var {������ ���� ��������� �������� � ����������}
  TimerList: TRyTimerList;

{���������, �������������� ���������, ����������� �� ��������.}
procedure TimerProc(HWND, uMsg, idEvent, dwTime : Integer); stdcall;
var
  I: Integer;
  Timer: TRyTimer;
begin
  {����� �� ���� ��������� Timer'��}
  I := 0;
  while I < TimerList.Count do
  begin
    Timer := TimerList.Items[I];
    if (Timer <> nil) and {������� (���������) ����������}
       (idEvent = Timer.FHandle) and {���� Timer �� ��� ��� ������ ���������}
       (Assigned(Timer.OnTimer)) then {���� Timer �� ��������}
    begin
      Timer.OnTimer(Timer);
      Exit; {��������� � ������}
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
  TimerList.Add(Self); {��������� ������ ��������}
end;

destructor TRyTimer.Destroy;
begin
  Active := False;
  TimerList.Remove(Self); {������� ������ �� ������}
  inherited;
end;

procedure TRyTimer.SetActive(const Value: Boolean);
begin
  if FActive = Value then Exit;
  if FActive then {���� �� ����� ������ �������}
  begin
    KillTimer(0, FHandle); {������� WinTimer}
    FHandle := 0; {!}
  end;
  if Value then {���� ������������ ��� Timer}
     {������� WinTimer � �������� ��� Handle}
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

{�������������� ���������------------------------------------------------------}

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
  while TimerList.Count > 0 do {���� Count > 0, �� ���-�� �� ������
  ���������� �����-�� �� �������� ��� ��������� �����, ������ ���
  �� �������, �� ���� �����������, ��� � ��� ����� ��� ���������
  �����-�� ������ � ��� ���� ������ ��� ��������� ������,
  � ��� ����������� ����� ������� ���������� ���������� ��� ������,
  �� ��� �� ���� ������, � ���� ������� ����� ����� �� ����� �
  ���������� ������� - MyTimer.Free}
    TimerList.Items[TimerList.Count - 1].Free; {�����, ��� ��������
    ����������, �� �����, �� ������ ������, ��������� �� ��������
    ��������� � ��������� ��}
  TimerList.Free; {The End}

end.

