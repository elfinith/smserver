unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, usnap7, StdCtrls, ExtCtrls, ComCtrls, ToolWin, ImgList, device,
  interval, data, Contnrs, Menus, IniFiles, Buttons, Grids, ValEdit, CommCtrl;

const
  strDateTimeFormat = 'dd-mm-yyyy hh:MM:ss';
  strNewDeviceName = 'Новое устройство';
  strNewDeviceAddr = '127.0.0.1';
  strNewDataBlock = 'Новый блок данных';
  iNewDeviceRack = 0;
  iNewDeviceSlot = 2;
  strDeleteDevice = 'Удалить устройство, все его блоки данных и все собранные с них данные?';
  strDeleteData = 'Удалить блок данных и все собранные с него данные?';
  strClearData = 'Удалить все сохранённые данные для данного блока?';
  strAddDevice = 'Добавить устройство';
  strAddDataFor = 'Создать блок данных для ';
  strConfigFile = 'config.ini';
  strErrLoadConfig = 'Ошибка загрузки конфигурации из файла ';
  strStartDevicePolling = 'Запустить опрос всех блоков данных на ';
  strStopDevicePolling = 'Остановить опрос всех блоков данных на ';
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
  iBtnImgShowData = 7;
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
    StatusBar1: TStatusBar;
    ImageList1: TImageList;
    ImageList2: TImageList;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    ToolButton7: TToolButton;
    Panel1: TPanel;
    Splitter1: TSplitter;
    TreeView1: TTreeView;
    ListBox1: TListBox;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    ValueListEditor1: TValueListEditor;
    SpeedButton2: TSpeedButton;
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
    procedure ToolButton7Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
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
  bDebugMode : boolean;

implementation

{$R *.dfm}

procedure LoadConfig;
begin
  with Config do try
    bDebugMode := ReadBool('Misc', 'DebugMode', true);
  except
    raise Exception.Create(strErrLoadConfig + FileName);
  end; // with Config
end;

procedure SetNodeState(node: TTreeNode; Flags: Integer);
var tvi: TTVItem;
begin
  FillChar(tvi, Sizeof(tvi), 0);
  with tvi do begin
    hItem := node.ItemID;
    mask := TVIF_STATE;
    stateMask := TVIS_BOLD or TVIS_CUT;
    state := Flags;
  end; // with tvi
  TreeView_SetItem(node.Handle, tvi);
end;

function GetNodeState(node: TTreeNode): integer;
var
  tvi: TTVItem;
  i:integer;
begin
  FillChar(tvi, SizeOf(tvi), 0);
  with tvi do begin
    hItem := node.ItemID;
    Mask := TVIF_STATE;
    stateMask := TVIS_BOLD or TVIS_CUT;
//    StateMask := TVIS_STATEIMAGEMASK;
  end; // with tvi
  TreeView_GetItem(node.Handle, tvi);
  result := tvi.state;
end;

function RusMessageDialog(const Msg: string; DlgType: TMsgDlgType;
   Buttons: TMsgDlgButtons; Captions: array of string): Integer;
var
  aMsgDlg: TForm;
  i: Integer;
  dlgButton: TButton;
  CaptionIndex: Integer;
begin
  aMsgDlg := CreateMessageDialog(Msg, DlgType, Buttons);
  captionIndex := 0;
  // перебор по объектам в диалоге
  for i := 0 to aMsgDlg.ComponentCount - 1 do begin
    // если кнопка
    if (aMsgDlg.Components[i] is TButton) then begin
      dlgButton := TButton(aMsgDlg.Components[i]);
      if CaptionIndex > High(Captions) then Break;
      // загружаем новый заголовок из нашего массива заголовков
      dlgButton.Caption := Captions[CaptionIndex];
      inc(CaptionIndex);
    end;
  end;
  Result := aMsgDlg.ShowModal;
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
  DEV_ID := 0;
  with TreeView1.Selected do case Level of
  iLvlDevices : begin
    DEV_ID := Index;
  end;
  iLvlDataBlocks : begin
    DEV_ID := Parent.Index;
  end;
  iLvlDetails : begin
    DEV_ID := Parent.Parent.Index;
  end;
  end;
  DataForm := TDataForm.Create(Self);
  with DataForm do begin
    Data := TSnap7Data.Create(Devices[DEV_ID].AddData(strNewDataBlock,0,1,1,1,1));
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
        if not(isConnected) then begin
          DisplayMessage(strErrorDeviceConnect + ' ' + Name);
          SetNodeState(tvDevice, TVIS_BOLD or TVIS_CUT);
        end
        else SetNodeState(tvDevice, TVIS_BOLD);
      end; // with Devices[i]
      for j := 0 to Devices[i].EnumDataIDs.Count - 1 do begin
        with TSnap7Data.Create(StrToInt(Devices[i].EnumDataIDs[j])) do try
          tvItem := TreeView1.Items.AddChild(tvDevice, Name);
          if not(Devices[i].isConnected) then SetNodeState(tvItem, TVIS_CUT);
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
  with TreeView1.Selected do case Level of
  iLvlDevices : begin
    DeviceForm := TDeviceForm.Create(Self);
    with DeviceForm do begin
      Device := Devices[Index];
      Edit1.Text := Devices[Index].Name;
      Edit2.Text := Devices[Index].Addr;
      Edit3.Text := IntToStr(Devices[Index].Rack);
      Edit4.Text := IntToStr(Devices[Index].Slot);
      ShowModal;
    end; // with DeviceForm
  end; // with TreeView1.Selected
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


procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  Panel2.Hide;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  if RusMessageDialog(strClearData, mtConfirmation, mbYesNo, ['ОК', 'Отмена']) = mryes then begin
    with TSnap7Data.Create(StrtoInt(TreeView1.Selected.getFirstChild.Text)) do try
      ClearStoredData;
    finally
      Destroy;
    end;
    Panel2.Hide;
  end; // if MessageDlg
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
  ivlForm : TIntervalForm;
  Child : TTreeNode;
  idx : integer;
  isFormShown : boolean;
begin
  if TreeView1.Selected <> nil then with TreeView1.Selected do case Level of
  iLvlDevices : begin
    idx := ToolButton2.ImageIndex;
    case idx of
      iBtnImgRun :
        if RusMessageDialog(strStartDevicePolling + Text + ' ?', mtConfirmation, mbYesNo, ['ОК', 'Отмена']) = mryes then begin
        isFormShown := false;
        Child := getFirstChild;
        while Child <> nil do begin
          if not(isFormShown) then begin
            ivlForm := TIntervalForm.Create(Self);
            ivlForm.ShowModal;
            isFormShown := true;
          end; // if not(isFormShown)
          AddPollFor(StrToInt(Child.getFirstChild.Text), StrToInt(ivlForm.Edit1.Text));
          Child.ImageIndex := iTNodeImgRunning;
          Child := GetNextChild(Child);
        end; // while Child <> nil
        ToolButton2.ImageIndex := iBtnImgStop;
      end;
      iBtnImgStop :
        if RusMessageDialog(strStopDevicePolling + Text + ' ?', mtConfirmation, mbYesNo, ['ОК', 'Отмена']) = mryes then begin
        Child := getFirstChild;
        while Child <> nil do begin
          RemovePollFor(StrToInt(Child.getFirstChild.Text));
          Child.ImageIndex := iTNodeImgData;
          Child := GetNextChild(Child);
        end; // while Child <> nil
        ToolButton2.ImageIndex := iBtnImgRun;
      end;
    end; // case idx
  end; // iLvlDevices :
  iLvlDataBlocks : begin
    case ImageIndex of
    iTNodeImgData : begin
      ivlForm := TIntervalForm.Create(Self);
      ivlForm.ShowModal;
      AddPollFor(StrToInt(getFirstChild.Text), StrToInt(ivlForm.Edit1.Text));
      ImageIndex := iTNodeImgRunning;
      ToolButton2.ImageIndex := iBtnImgStop;
      ToolButton6.Enabled := true;
    end;
    iTNodeImgRunning : begin
      RemovePollFor(StrToInt(getFirstChild.Text));
      ImageIndex := iTNodeImgData;
      ToolButton2.ImageIndex := iBtnImgRun;
      ToolButton6.Enabled := false;
    end;
    iTNodeImgPaused : begin
      AddPollFor(StrToInt(getFirstChild.Text),0);
      ImageIndex := iTNodeImgRunning;
      ToolButton2.ImageIndex := iBtnImgStop;
      ToolButton6.Enabled := true;
    end;
    end; // case ImageIndex
    SelectedIndex := ImageIndex;
  end;
  end;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  if TreeView1.Selected <> nil then ShowDetails(TreeView1.Selected);
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
begin
  with TreeView1.Selected do case Level of
  iLvlDevices : begin
    N2.Caption := strAddDataFor + '"' + Text + '"';
  end;
  iLvlDataBlocks : begin
    N2.Caption := strAddDataFor + '"' + Parent.Text + '"';
  end;
  iLvlDetails : begin
    N2.Caption := strAddDataFor + '"' + Parent.Parent.Text + '"';
  end;
  end; // with TreeView1.Selected
  PopupMenu1.Popup(Left + ToolButton4.Left, Top + TreeView1.Top);
end;

procedure TForm1.ToolButton5Click(Sender: TObject);
var
  DEV_ID, DM_ID : integer;
begin
  with TreeView1.Selected do case Level of
  iLvlDevices : begin
    if RusMessageDialog(strDeleteDevice, mtConfirmation, mbYesNo, ['ОК', 'Отмена']) = mryes
    then with TSnap7WorkArea.Create do try
      DeleteDevice(Devices[Index].Id);
    finally
      Free;
    end;
  end;
  iLvlDataBlocks : begin
    DEV_ID := Parent.Index;
    DM_ID := TSnap7Data.Create(StrToInt(getFirstChild.Text)).Id;
    if RusMessageDialog(strDeleteData, mtConfirmation, mbYesNo, ['ОК', 'Отмена']) = mryes
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
  case TreeView1.Selected.Level of
  iLvlDataBlocks : begin
    PausePollFor(StrToInt(TreeView1.Selected.getFirstChild.Text));
    ToolButton2.ImageIndex := iBtnImgRun;
    ToolButton6.Enabled := false;
    with TreeView1.Selected do begin
      ImageIndex := iTNodeImgPaused;
      SelectedIndex := ImageIndex;
    end; // with TreeView1.Selected
  end;
  end; // case TreeView1.Selected.Level
end;

procedure TForm1.ToolButton7Click(Sender: TObject);
var
  ABuffer: AnsiString;
  AText: WideString;
  l : integer;
begin
  ValueListEditor1.Strings.Clear;
  with TSnap7Data.Create(StrToInt(TreeView1.Selected.getFirstChild.Text)) do try
    l := DataAmount;
  finally
    Destroy;
  end;
  with TSelectQuery.Create('select poll_time, poll_data from data_values where dm_id = '
  + TreeView1.Selected.getFirstChild.Text + ' order by poll_time').Data do try
    while not(EOF) do try
      ABuffer := StringOf(Fields[1].AsBytes);
      SetLength(AText,l*2);
      BinToHex(PAnsiChar(ABuffer), PWideChar(AText), l);
      ValueListEditor1.InsertRow(Fields[0].AsString, AText, true);
      Next;
    except
      on E:Exception do
        DisplayMessage(E.Classname + ': ' + E.Message);
    end; // while not(EOF)
  finally
    Destroy;
  end;
  Panel2.Show;
end;

procedure TForm1.TrayIcon1Click(Sender: TObject);
begin
  with Application do begin
    Restore;
    BringToFront;
  end; // with Application
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  Child : TTreeNode;
begin
  ToolButton2.Enabled := not(GetNodeState(TreeView1.Selected) and TVIS_CUT = TVIS_CUT);
  case TreeView1.Selected.Level of
  iLvlDevices : begin
    ToolButton2.ImageIndex := iBtnImgRun;
    Child := TreeView1.Selected.getFirstChild;
    while Child <> nil do begin
      if Child.ImageIndex = iTNodeImgRunning then ToolButton2.ImageIndex := iBtnImgStop;
      Child := TreeView1.Selected.GetNextChild(Child);
    end;
    ToolButton7.Enabled := false;
  end;
  iLvlDataBlocks : begin
    case TreeView1.Selected.ImageIndex of
    iTNodeImgData : begin
      ToolButton2.ImageIndex := iBtnImgRun;
      ToolButton6.Enabled := false;
    end;
    iTNodeImgRunning : begin
      ToolButton2.ImageIndex := iBtnImgStop;
      ToolButton6.Enabled := true;
    end;
    end; // case TreeView1.Selected.ImageIndex
    ToolButton7.Enabled := true;
  end;
  else ToolButton7.Enabled := false;
  end; // case TreeView1.Selected.Level
  Panel2.Hide;
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
