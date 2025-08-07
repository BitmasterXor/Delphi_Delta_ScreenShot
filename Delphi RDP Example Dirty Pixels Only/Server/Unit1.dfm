object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'RDP Server'
  ClientHeight = 659
  ClientWidth = 962
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 962
    Height = 80
    Align = alTop
    TabOrder = 0
    object lblStatus: TLabel
      Left = 370
      Top = 17
      Width = 31
      Height = 13
      Caption = 'Ready'
    end
    object lblTotalData: TLabel
      Left = 8
      Top = 48
      Width = 93
      Height = 13
      Caption = 'Total Received: 0 B'
    end
    object lblCurrentDelta: TLabel
      Left = 200
      Top = 48
      Width = 87
      Height = 13
      Caption = 'Current Delta: 0 B'
    end
    object btnStartServer: TButton
      Left = 8
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Start Server'
      TabOrder = 0
      OnClick = btnStartServerClick
    end
    object btnStopServer: TButton
      Left = 89
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Stop Server'
      Enabled = False
      TabOrder = 1
      OnClick = btnStopServerClick
    end
    object btnToggleMouse: TButton
      Left = 170
      Top = 12
      Width = 90
      Height = 25
      Caption = 'Enable Mouse'
      Enabled = False
      TabOrder = 2
      OnClick = btnToggleMouseClick
    end
    object btnToggleKeyboard: TButton
      Left = 270
      Top = 12
      Width = 90
      Height = 25
      Caption = 'Enable Keyboard'
      Enabled = False
      TabOrder = 3
      OnClick = btnToggleKeyboardClick
    end
  end
  object pnlCenter: TPanel
    Left = 0
    Top = 80
    Width = 962
    Height = 579
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object scrollBox: TScrollBox
      Left = 0
      Top = 0
      Width = 962
      Height = 579
      Align = alClient
      TabOrder = 0
    end
  end
  object ncServerSource1: TncServerSource
    Port = 3389
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ncServerSource1Connected
    OnDisconnected = ncServerSource1Disconnected
    OnHandleCommand = ncServerSource1HandleCommand
    Left = 896
    Top = 16
  end
end
