object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Screenshot Analyzer Client'
  ClientHeight = 500
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object lblServerIP: TLabel
    Left = 10
    Top = 15
    Width = 49
    Height = 13
    Caption = 'Server IP:'
  end
  object lblPort: TLabel
    Left = 200
    Top = 15
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object lblIntervalText: TLabel
    Left = 308
    Top = 46
    Width = 66
    Height = 13
    Caption = 'Interval (ms):'
  end
  object lblStatus: TLabel
    Left = 10
    Top = 75
    Width = 69
    Height = 13
    Caption = 'Status: Ready'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblConnectionInfo: TLabel
    Left = 10
    Top = 95
    Width = 70
    Height = 13
    Caption = 'Not connected'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblStats: TLabel
    Left = 10
    Top = 115
    Width = 147
    Height = 13
    Caption = 'Sent: 0 bytes | Screenshots: 0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edtServerIP: TEdit
    Left = 70
    Top = 12
    Width = 120
    Height = 21
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object edtServerPort: TEdit
    Left = 230
    Top = 12
    Width = 60
    Height = 21
    TabOrder = 1
    Text = '8080'
  end
  object btnConnect: TButton
    Left = 300
    Top = 10
    Width = 100
    Height = 25
    Caption = 'Connect'
    TabOrder = 2
    OnClick = btnConnectClick
  end
  object btnSendScreenshot: TButton
    Left = 410
    Top = 10
    Width = 120
    Height = 25
    Caption = 'Send Screenshot'
    Enabled = False
    TabOrder = 3
    OnClick = btnSendScreenshotClick
  end
  object chkAutoDelta: TCheckBox
    Left = 10
    Top = 45
    Width = 150
    Height = 17
    Caption = 'Auto Delta Mode'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object chkAutoCapture: TCheckBox
    Left = 170
    Top = 45
    Width = 120
    Height = 17
    Caption = 'Auto Capture'
    TabOrder = 5
    OnClick = chkAutoCaptureClick
  end
  object spnInterval: TSpinEdit
    Left = 380
    Top = 44
    Width = 60
    Height = 22
    MaxValue = 999999
    MinValue = 1
    TabOrder = 6
    Value = 500
  end
  object memoLog: TMemo
    Left = 10
    Top = 140
    Width = 560
    Height = 320
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object ncClientSource1: TncClientSource
    Port = 8080
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ncClientSource1Connected
    OnDisconnected = ncClientSource1Disconnected
    OnHandleCommand = ncClientSource1HandleCommand
    Host = '127.0.0.1'
    Left = 550
    Top = 450
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = Timer1Timer
    Left = 500
    Top = 450
  end
end
