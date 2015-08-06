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
    {����� ���� ����� �����. ���������� ���������� �������.}

begin
  if not init_mutex(UniqueString) then
    exit; {������� �� �������������, ���� ������� ��� ����}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Simatic Monitoring Server';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
