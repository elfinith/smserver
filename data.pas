unit data;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, usnap7, Buttons, ExtCtrls;

const
  strChangeDataProperties = 'Изменить сведения о блоке данных?';

type
  TDataForm = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Image1: TImage;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    Data : TSnap7Data;
  end;

var
  DataForm: TDataForm;



implementation

{$R *.dfm}

procedure TDataForm.SpeedButton1Click(Sender: TObject);
begin
  if MessageDlg(strChangeDataProperties, mtConfirmation, [mbyes, mbno], 0) = mryes then begin
    Data.Name := Edit1.Text;
    Data.Area := ComboBox1.ItemIndex;
    Data.DBNum := StrToInt(Edit3.Text);
    Data.DataStart := StrToInt(Edit4.Text);
    Data.DataAmount := StrToInt(Edit5.Text);
    Data.WLen := ComboBox2.ItemIndex;
  end;
  Close;
end;

procedure TDataForm.SpeedButton2Click(Sender: TObject);
begin
  Close;
end;

end.
