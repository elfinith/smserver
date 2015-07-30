object DataForm: TDataForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1041#1083#1086#1082' '#1076#1072#1085#1085#1099#1093
  ClientHeight = 128
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 73
    Height = 13
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
  end
  object Label2: TLabel
    Left = 8
    Top = 35
    Width = 23
    Height = 13
    Caption = 'Area'
  end
  object Label3: TLabel
    Left = 219
    Top = 35
    Width = 37
    Height = 13
    Caption = 'DB Num'
  end
  object Label4: TLabel
    Left = 144
    Top = 62
    Width = 24
    Height = 13
    Caption = 'Start'
  end
  object Label5: TLabel
    Left = 221
    Top = 62
    Width = 37
    Height = 13
    Caption = 'Amount'
  end
  object Label6: TLabel
    Left = 8
    Top = 62
    Width = 27
    Height = 13
    Caption = 'WLen'
  end
  object Edit1: TEdit
    Left = 96
    Top = 5
    Width = 217
    Height = 21
    TabOrder = 0
  end
  object Edit3: TEdit
    Left = 264
    Top = 32
    Width = 49
    Height = 21
    TabOrder = 1
  end
  object Edit4: TEdit
    Left = 174
    Top = 59
    Width = 41
    Height = 21
    TabOrder = 2
  end
  object Edit5: TEdit
    Left = 264
    Top = 59
    Width = 49
    Height = 21
    TabOrder = 3
  end
  object Button1: TButton
    Left = 157
    Top = 95
    Width = 75
    Height = 25
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 238
    Top = 95
    Width = 75
    Height = 25
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 5
    OnClick = Button2Click
  end
  object ComboBox1: TComboBox
    Left = 41
    Top = 32
    Width = 90
    Height = 21
    TabOrder = 6
    Text = 'ComboBox1'
    Items.Strings = (
      'DB'
      'PE'
      'PA'
      'MK'
      'TM'
      'CT')
  end
  object ComboBox2: TComboBox
    Left = 41
    Top = 59
    Width = 90
    Height = 21
    TabOrder = 7
    Text = 'ComboBox2'
    Items.Strings = (
      'Bit'
      'Byte'
      'Word'
      'Dword'
      'Real'
      'Counter'
      'Timer')
  end
end
