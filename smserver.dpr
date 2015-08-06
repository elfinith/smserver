program smserver;

uses
  Forms,
  main in 'main.pas' {Form1},
  device in 'device.pas' {DeviceForm},
  interval in 'interval.pas' {IntervalForm},
  Only_one in 'Only_one.pas',
  data in 'data.pas' {DataForm};

{$R *.res}

const
  UniqueString = 'SMServerMutex';
    {Может быть любое слово. Желательно латинскими буквами.}

begin
  if not init_mutex(UniqueString) then
    exit; {Выходим до инициализации, если мьютекс уже есть}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Simatic Monitoring Server';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
