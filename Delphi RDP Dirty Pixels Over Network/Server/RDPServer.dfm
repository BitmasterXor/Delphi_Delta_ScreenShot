object RDPServerForm: TRDPServerForm
  Left = 0
  Top = 0
  Caption = 'RDP Server'
  ClientHeight = 600
  ClientWidth = 800
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
    Width = 800
    Height = 80
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblStatus: TLabel
      Left = 16
      Top = 50
      Width = 35
      Height = 13
      Caption = 'Status:'
    end
    object lblStats: TLabel
      Left = 400
      Top = 50
      Width = 29
      Height = 13
      Caption = 'Stats:'
    end
    object btnStartServer: TButton
      Left = 16
      Top = 16
      Width = 90
      Height = 25
      Caption = 'Start Server'
      TabOrder = 0
      OnClick = btnStartServerClick
    end
    object btnStopServer: TButton
      Left = 112
      Top = 16
      Width = 90
      Height = 25
      Caption = 'Stop Server'
      TabOrder = 1
      OnClick = btnStopServerClick
    end
    object btnToggleMouse: TButton
      Left = 240
      Top = 16
      Width = 90
      Height = 25
      Caption = 'Enable Mouse'
      TabOrder = 2
      OnClick = btnToggleMouseClick
    end
    object btnToggleKeyboard: TButton
      Left = 336
      Top = 16
      Width = 100
      Height = 25
      Caption = 'Enable Keyboard'
      TabOrder = 3
      OnClick = btnToggleKeyboardClick
    end
  end
  object scrollBox: TScrollBox
    Left = 0
    Top = 80
    Width = 800
    Height = 520
    Align = alClient
    Color = clBlack
    ParentColor = False
    TabOrder = 1
  end
  object ncServerSource1: TncServerSource
    Port = 3389
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ncServerSource1Connected
    OnDisconnected = ncServerSource1Disconnected
    OnHandleCommand = ncServerSource1HandleCommand
    Left = 728
    Top = 24
  end
end
