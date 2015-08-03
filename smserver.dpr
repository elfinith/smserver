program smserver;

uses
  Forms,
  main in 'main.pas' {Form1},
  device in 'device.pas' {DeviceForm},
  interval in 'interval.pas' {IntervalForm},
  data in 'data.pas' {DataForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Simatic Monitoring Server';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
