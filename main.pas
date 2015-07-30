unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, usnap7, StdCtrls, ExtCtrls, ComCtrls, ToolWin, ImgList, device, interval, data, Contnrs;

const
  bDebugMode = true;
  strDateTimeFormat = 'dd-mm-yyyy hh:MM:ss';

type

  TimersList = class(TObjectList)
  private
    function GetItems(Index: Integer): TSnap7Poll;
    function GetById(DM_ID : Integer): TSnap7Poll;
    procedure SetItems(Index: Integer; const Value: TSnap7Poll);
  published
  public
    property Items[Index: Integer]: TSnap7Poll read GetItems write SetItems; default;
  end;


  TForm1 = class(TForm)
    TrayIcon1: TTrayIcon;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    TreeView1: TTreeView;
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    ListBox1: TListBox;
    ImageList1: TImageList;
    ImageList2: TImageList;
    ToolButton3: TToolButton;
    procedure ToolButton1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeView1DblClick(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
    { Private declarations }
    Devices : array of TSnap7Device;
    Timers : TimersList;
    procedure DisplayMessage(str : string);
    procedure RefreshDeviceTree;
    procedure ShowDetails(Node: TTreeNode);
    procedure AddPollFor(DM_ID, Interval : integer);
    procedure RemovePollFor(DM_ID : integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  DeviceForm : TDeviceForm;
  DataForm : TDataForm;

implementation

{$R *.dfm}

procedure TForm1.AddPollFor(DM_ID, Interval : integer);
var
  newPoll : TSnap7Poll;
begin
  newPoll := TSnap7Poll.Create(DM_ID, Interval);
  Timers.Add(newPoll);
  Timers[Timers.Count - 1].Start;

  if bDebugMode then DisplayMessage('AddPollFor(' + IntToStr(DM_ID)
    + ',' + IntToStr(Interval) + '), Length(Timers)=' + IntToStr(Timers.Count));
end;

procedure TForm1.RemovePollFor(DM_ID : integer);
begin
  Timers.GetById(DM_ID).Stop;
  Timers.Remove(Timers.GetById(DM_ID));

  if bDebugMode then DisplayMessage('RemovePollFor(' + IntToStr(DM_ID)
    + '), Length(Timers)=' + IntToStr(Timers.Count));
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i : integer;
begin
  for i := 0 to Length(Devices) - 1 do Devices[i].Destroy;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Timers := TimersList.Create(true);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timers.Destroy;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  RefreshDeviceTree;
end;

procedure TForm1.RefreshDeviceTree;
var
  i, j : integer;
  tvItem, tvChild : TTreeNode;
begin
  TreeView1.Items.Clear;
  with TSnap7WorkArea.Create do try
    SetLength(Devices, EnumDeviceIDs.Count);
    for i := 0 to EnumDeviceIDs.Count - 1 do begin
      Devices[i] := TSnap7Device.Create(StrToInt(EnumDeviceIDs[i]));
      with Devices[i] do begin
        TreeView1.Items.Add(nil, Name);
        if not(isConnected) then DisplayMessage(strErrorDeviceConnect + ' ' + Name);
        for j := 0 to EnumDataIDs.Count - 1 do begin
          with TSnap7Data.Create(StrToInt(EnumDataIDs[j])) do try
            tvItem := TreeView1.Items.AddChild(TreeView1.Items[i], Name);
            tvItem.ImageIndex := 1;
            tvItem.SelectedIndex := 1;
            tvChild := TreeView1.Items.AddChild(tvItem, IntToStr(id));
            tvChild.ImageIndex := 3;
            tvChild.SelectedIndex := 3;
          finally
            Free;
          end;
        end; // for j
      end; // with Devices[i]
    end;
  finally
    Free;
  end; // with TSnap7WorkArea.Create
  TreeView1.SetFocus;
end;

procedure TForm1.ShowDetails(Node: TTreeNode);
begin
  case TreeView1.Selected.Level of
  0 : begin
    DeviceForm := TDeviceForm.Create(Self);
    with DeviceForm do begin
      Device := Devices[TreeView1.Selected.Index];
      Edit1.Text := Devices[TreeView1.Selected.Index].Name;
      Edit2.Text := Devices[TreeView1.Selected.Index].Addr;
      Edit3.Text := IntToStr(Devices[TreeView1.Selected.Index].Rack);
      Edit4.Text := IntToStr(Devices[TreeView1.Selected.Index].Slot);
      ShowModal;
    end; // with DeviceForm
  end;
  1 : begin
    DataForm := TDataForm.Create(Self);
    with DataForm do begin
      Data := TSnap7Data.Create(StrToInt(TreeView1.Selected.getFirstChild.Text));
      Edit1.Text := Data.Name;
      ComboBox1.ItemIndex := Data.Area;
      Edit3.Text := IntToStr(Data.DBNum);
      Edit4.Text := IntToStr(Data.DataStart);
      Edit5.Text := IntToStr(Data.DataAmount);
      ComboBox2.ItemIndex := Data.WLen;
      ShowModal;
    end; // with DataForm
  end;

  end;
end;


procedure TForm1.DisplayMessage(str: string);
begin
  ListBox1.Items.Add('[' + FormatDateTime(strDateTimeFormat, Now()) + '] ' + str);
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  RefreshDeviceTree;
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
var
  Timer : TSnap7Poll;
  ivlForm : TIntervalForm;
begin
  if TreeView1.Selected.Level = 1 then begin
    if TreeView1.Selected.ImageIndex = 1 then begin
      ivlForm := TIntervalForm.Create(Self);
      ivlForm.ShowModal;
      AddPollFor(StrToInt(TreeView1.Selected.getFirstChild.Text),StrToInt(ivlForm.Edit1.Text));
      TreeView1.Selected.ImageIndex := 2;
      ToolButton2.ImageIndex := 2;
    end
    else begin
      RemovePollFor(StrToInt(TreeView1.Selected.getFirstChild.Text));
      TreeView1.Selected.ImageIndex := 1;
      ToolButton2.ImageIndex := 1;
    end;
    TreeView1.Selected.SelectedIndex := TreeView1.Selected.ImageIndex;
  end;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  ShowDetails(TreeView1.Selected);
end;

procedure TForm1.TreeView1Click(Sender: TObject);
begin
  if TreeView1.Selected.Level = 1 then case TreeView1.Selected.ImageIndex of
    1 : ToolButton2.ImageIndex := 1;
    2 : ToolButton2.ImageIndex := 2;
  end;
end;

procedure TForm1.TreeView1DblClick(Sender: TObject);
begin
  ShowDetails(TreeView1.Selected);
end;

{ TimersList }

function TimersList.GetItems(Index: Integer): TSnap7Poll;
begin
  Result := TSnap7Poll(inherited GetItem(Index));
end;

function TimersList.GetById(DM_ID: Integer): TSnap7Poll;
var
  i : integer;
begin
  for i := 0 to Count - 1 do if Items[i].Id = DM_ID then Result := Items[i];
end;

procedure TimersList.SetItems(Index: Integer; const Value: TSnap7Poll);
begin
  inherited SetItem(Index, Value);
end;

end.
