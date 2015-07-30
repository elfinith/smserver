unit device;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, usnap7;

const
  strChangeDeviceProperties = 'Изменить данные об устройстве?';

type
  TDeviceForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    Device : TSnap7Device;
  end;

implementation

{$R *.dfm}

procedure TDeviceForm.BitBtn1Click(Sender: TObject);
begin
  if MessageDlg(strChangeDeviceProperties, mtConfirmation, [mbyes, mbno], 0) = mryes then begin
    Device.Name := Edit1.Text;
    Device.Addr := Edit2.Text;
    Device.Rack := StrToInt(Edit3.Text);
    Device.Slot := StrToInt(Edit4.Text);
  end;
  Close;
end;

procedure TDeviceForm.BitBtn2Click(Sender: TObject);
begin
  Close;
end;

end.
