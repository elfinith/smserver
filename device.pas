unit device;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, usnap7, ExtCtrls;

const
  strChangeDeviceProperties = 'Изменить данные об устройстве?';

type
  TDeviceForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Image1: TImage;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    Device : TSnap7Device;
  end;

implementation

{$R *.dfm}

procedure TDeviceForm.SpeedButton1Click(Sender: TObject);
begin
  if MessageDlg(strChangeDeviceProperties, mtConfirmation, [mbyes, mbno], 0) = mryes then begin
    Device.Name := Edit1.Text;
    Device.Addr := Edit2.Text;
    Device.Rack := StrToInt(Edit3.Text);
    Device.Slot := StrToInt(Edit4.Text);
  end;
  Close;
end;

procedure TDeviceForm.SpeedButton2Click(Sender: TObject);
begin
  Close;
end;

end.
