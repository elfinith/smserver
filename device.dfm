object DeviceForm: TDeviceForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103' '#1086#1073' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1077
  ClientHeight = 163
  ClientWidth = 274
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
    Top = 11
    Width = 73
    Height = 13
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
  end
  object Label2: TLabel
    Left = 8
    Top = 38
    Width = 43
    Height = 13
    Caption = 'IP '#1072#1076#1088#1077#1089
  end
  object Label3: TLabel
    Left = 8
    Top = 65
    Width = 23
    Height = 13
    Caption = 'Rack'
  end
  object Label4: TLabel
    Left = 8
    Top = 92
    Width = 18
    Height = 13
    Caption = 'Slot'
  end
  object BitBtn1: TBitBtn
    Left = 110
    Top = 130
    Width = 75
    Height = 25
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 191
    Top = 130
    Width = 75
    Height = 25
    Caption = #1047#1072#1082#1088#1099#1090#1100
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object Edit1: TEdit
    Left = 97
    Top = 8
    Width = 169
    Height = 21
    TabOrder = 2
  end
  object Edit2: TEdit
    Left = 97
    Top = 35
    Width = 169
    Height = 21
    TabOrder = 3
  end
  object Edit3: TEdit
    Left = 97
    Top = 62
    Width = 169
    Height = 21
    TabOrder = 4
  end
  object Edit4: TEdit
    Left = 97
    Top = 89
    Width = 169
    Height = 21
    TabOrder = 5
  end
end
