unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, usnap7, StdCtrls, ExtCtrls, ComCtrls, ToolWin, ImgList, device, interval, data, Contnrs,
  Menus, IniFiles;

const
  bDebugMode = true;
  strDateTimeFormat = 'dd-mm-yyyy hh:MM:ss';
  strNewDeviceName = 'Новое устройство';
  strNewDeviceAddr = '127.0.0.1';
  iNewDeviceRack = 0;
  iNewDeviceSlot = 2;
  strDeleteDevice = 'Удалить устройство, все его блоки данных и все собранные с них данные?';
  strDeleteData = 'Удалить блок данных и все собранные с него данные?';
  strAddDevice = 'Добавить устройство';
  strAddDataFor = 'Создать блок данных для ';
  strConfigFile = 'config.ini';
  strErrLoadConfig = 'Ошибка загрузки конфигурации из файла ';
  iLvlDevices = 0;
  iLvlDataBlocks = 1;
  iLvlDetails = 2;
  iBtnImgRefresh = 0;
  iBtnImgRun = 1;
  iBtnImgStop = 2;
  iBtnImgProperties = 3;
  iBtnImgAdd = 4;
  iBtnImgDelete = 5;
  iBtnImgPause = 6;
  iTNodeImgDevice = 0;
  iTNodeImgData = 1;
  iTNodeImgRunning = 2;
  iTNodeImgId = 3;
  iTNodeImgPaused = 4;
  iTNodeImgLocation = 5;
  iTNodeImgStart = 6;
  iTNodeImgAmount = 7;

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
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    procedure ToolButton1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeView1DblClick(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure ToolButton6Click(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
  private
    { Private declarations }
    Devices : array of TSnap7Device;
    Timers : TimersList;
    procedure DisplayMessage(str : string);
    procedure RefreshDeviceTree;
    procedure ShowDetails(Node: TTreeNode);
    procedure AddPollFor(DM_ID, Interval : integer);
    procedure RemovePollFor(DM_ID : integer);
    procedure PausePollFor(DM_ID : integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  DeviceForm : TDeviceForm;
  DataForm : TDataForm;
  Config : TIniFile;

implementation

{$R *.dfm}

procedure LoadConfig;
begin
  with Config do try

  except
    raise Exception.Create(strErrLoadConfig + FileName);
  end; // with Config
end;

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

procedure TForm1.PausePollFor(DM_ID : integer);
begin
  Timers.GetById(DM_ID).Pause;
  Timers.Remove(Timers.GetById(DM_ID));

  if bDebugMode then DisplayMessage('PausePollFor(' + IntToStr(DM_ID)
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
  Config := TIniFile.Create(ExtractFilePath(Application.ExeName) + '\' + strConfigFile);
  LoadConfig;
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

procedure TForm1.N1Click(Sender: TObject);
var
  newDevId : integer;
begin
  with TSnap7WorkArea.Create do try
    newDevId := AddDevice(strNewDeviceName, strNewDeviceAddr,
      iNewDeviceRack, iNewDeviceSlot);
  finally
    Destroy;
  end;
  DeviceForm := TDeviceForm.Create(Self);
  with DeviceForm do begin
    Device := TSnap7Device.Create(newDevId);
    Edit1.Text := Device.Name;
    Edit2.Text := Device.Addr;
    Edit3.Text := IntToStr(Device.Rack);
    Edit4.Text := IntToStr(Device.Slot);
    ShowModal;
  end;
  RefreshDeviceTree;
end;

procedure TForm1.N2Click(Sender: TObject);
var
  DEV_ID : integer;
begin
  case TreeView1.Selected.Level of
  0 : begin
    DEV_ID := TreeView1.Selected.Index;
  end;
  1 : begin
    DEV_ID := TreeView1.Selected.Parent.Index;
  end;
  2 : begin
    DEV_ID := TreeView1.Selected.Parent.Parent.Index;
  end;
  end;
  DataForm := TDataForm.Create(Self);
  with DataForm do begin
    Data := TSnap7Data.Create(Devices[DEV_ID].AddData('Новый блок данных',0,1,1,1,1));
    Edit1.Text := Data.Name;
    ComboBox1.ItemIndex := Data.Area;
    Edit3.Text := IntToStr(Data.DBNum);
    Edit4.Text := IntToStr(Data.DataStart);
    Edit5.Text := IntToStr(Data.DataAmount);
    ComboBox2.ItemIndex := Data.WLen;
    ShowModal;
  end; // with DataForm
  RefreshDeviceTree;
end;

procedure TForm1.RefreshDeviceTree;
var
  i, j : integer;
  tvItem, tvChild, tvDevice : TTreeNode;
begin
  TreeView1.Items.Clear;
  with TSnap7WorkArea.Create do try
    SetLength(Devices, EnumDeviceIDs.Count);
    for i := 0 to EnumDeviceIDs.Count - 1 do begin
      Devices[i] := TSnap7Device.Create(StrToInt(EnumDeviceIDs[i]));
      with Devices[i] do begin
        tvDevice := TreeView1.Items.Add(nil, Name);
        if not(isConnected) then DisplayMessage(strErrorDeviceConnect + ' ' + Name);
      end; // with Devices[i]
      for j := 0 to Devices[i].EnumDataIDs.Count - 1 do begin
        with TSnap7Data.Create(StrToInt(Devices[i].EnumDataIDs[j])) do try
          tvItem := TreeView1.Items.AddChild(tvDevice, Name);
          with tvItem do begin
            if not(haveStoredTimer) then
              ImageIndex := iTNodeImgData
            else
              ImageIndex := iTNodeImgPaused;
            SelectedIndex := ImageIndex;
          end; // with tvItem
          with TreeView1.Items.AddChild(tvItem, IntToStr(id)) do begin
            ImageIndex := iTNodeImgId;
            SelectedIndex := ImageIndex;
          end; // TreeView1.Items.AddChild
          tvChild := TreeView1.Items.AddChild(tvItem, 'DB' + IntToStr(DBNum));
          with tvChild do begin
            ImageIndex := iTNodeImgLocation;
            SelectedIndex := ImageIndex;
          end; // with tvChild
          with TreeView1.Items.AddChild(tvChild, 'Start: ' + IntToStr(DataStart)) do begin
            ImageIndex := iTNodeImgStart;
            SelectedIndex := ImageIndex;
          end; // with TreeView1.Items.AddChild
          with TreeView1.Items.AddChild(tvChild, 'Amount: ' + IntToStr(DataAmount)) do begin
            ImageIndex := iTNodeImgAmount;
            SelectedIndex := ImageIndex;
          end; // with TreeView1.Items.AddChild
        finally
          Free;
        end; // with TSnap7Data.Create
        tvDevice.Expand(false);
      end; // for j
    end;
  finally
    Free;
  end; // with TSnap7WorkArea.Create
  TreeView1.SetFocus;
end;

procedure TForm1.ShowDetails(Node: TTreeNode);
begin
  case TreeView1.Selected.Level of
  iLvlDevices : begin
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
  iLvlDataBlocks : begin
    DataForm := TDataForm.Create(Self);
    with DataForm do begin
      Data := TSnap7Data.Create(StrToInt(TreeView1.Selected.getFirstChild.Text));
      Edit1.Text := Data.Name;
      ComboBox1.ItemIndex := Data.Area;
      Edit3.Text := IntToStr(Data.DBNum);
      Edit4.Text := IntToStr(Data.DataStart);
      Edit5.Text := IntToStr(Data.DataAmount);
      ComboBox2.ItemIndex := Data.WLen;
      if Data.haveStoredTimer then begin
        CheckBox1.Checked := Data.haveStoredTimer;
        Edit2.Text := IntToStr(Data.GetStoredTimerInterval)
      end; // if Data.haveStoredTimer
      ShowModal;
    end; // with DataForm
  end;
  end; // case
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
  if TreeView1.Selected.Level = iLvlDataBlocks then begin
    case TreeView1.Selected.ImageIndex of
    iTNodeImgData : begin
      ivlForm := TIntervalForm.Create(Self);
      ivlForm.ShowModal;
      AddPollFor(StrToInt(TreeView1.Selected.getFirstChild.Text),StrToInt(ivlForm.Edit1.Text));
      TreeView1.Selected.ImageIndex := iTNodeImgRunning;
      ToolButton2.ImageIndex := iBtnImgStop;
      ToolButton6.Enabled := true;
    end;
    iTNodeImgRunning : begin
      RemovePollFor(StrToInt(TreeView1.Selected.getFirstChild.Text));
      TreeView1.Selected.ImageIndex := iTNodeImgData;
      ToolButton2.ImageIndex := iBtnImgRun;
    end;
    iTNodeImgPaused : begin
      AddPollFor(StrToInt(TreeView1.Selected.getFirstChild.Text),0);
      TreeView1.Selected.ImageIndex := iTNodeImgRunning;
      ToolButton2.ImageIndex := iBtnImgStop;
      ToolButton6.Enabled := true;
    end;
    end; // case
    TreeView1.Selected.SelectedIndex := TreeView1.Selected.ImageIndex;
  end;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  ShowDetails(TreeView1.Selected);
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
var
  newDevId : integer;
begin
  case TreeView1.Selected.Level of
  iLvlDevices : begin
    N2.Caption := strAddDataFor + '"' + TreeView1.Selected.Text + '"';
  end;
  iLvlDataBlocks : begin
    N2.Caption := strAddDataFor + '"' + TreeView1.Selected.Parent.Text + '"';
  end;
  iLvlDetails : begin
    N2.Caption := strAddDataFor + '"' + TreeView1.Selected.Parent.Parent.Text + '"';
  end;
  end;
  PopupMenu1.Popup(Left + ToolButton4.Left, Top + TreeView1.Top);
end;

procedure TForm1.ToolButton5Click(Sender: TObject);
var
  DEV_ID, DM_ID : integer;
begin
  case TreeView1.Selected.Level of
  iLvlDevices : begin
    if MessageDlg(strDeleteDevice, mtConfirmation, [mbyes, mbno], 0) = mryes
    then with TSnap7WorkArea.Create do try
      DeleteDevice(Devices[TreeView1.Selected.Index].Id);
    finally
      Free;
    end;
  end;
  iLvlDataBlocks : begin
    DEV_ID := TreeView1.Selected.Parent.Index;
    DM_ID := TSnap7Data.Create(StrToInt(TreeView1.Selected.getFirstChild.Text)).Id;
    if MessageDlg(strDeleteData, mtConfirmation, [mbyes, mbno], 0) = mryes
    then with TSnap7Device.Create(DEV_ID) do try
      DelData(DM_ID);
    finally
      Free;
    end;
  end;
  end;
  RefreshDeviceTree;
end;

procedure TForm1.ToolButton6Click(Sender: TObject);
begin
  PausePollFor(StrToInt(TreeView1.Selected.getFirstChild.Text));
  ToolButton2.ImageIndex := iBtnImgRun;
  ToolButton6.Enabled := false;
  with TreeView1.Selected do begin
    ImageIndex := iTNodeImgPaused;
    SelectedIndex := ImageIndex;
  end; // with TreeView1.Selected
end;

procedure TForm1.TrayIcon1Click(Sender: TObject);
begin
  with Application do begin
    Restore;
    BringToFront;
  end; // with Application
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if TreeView1.Selected.Level = 1 then case TreeView1.Selected.ImageIndex of
    iTNodeImgData : begin
      ToolButton2.ImageIndex := iBtnImgRun;
      ToolButton6.Enabled := false;
    end;
    iTNodeImgRunning : begin
      ToolButton2.ImageIndex := iBtnImgStop;
      ToolButton6.Enabled := true;
    end;
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
