object RDPClientForm: TRDPClientForm
  Left = 0
  Top = 0
  Caption = 'RDP Client'
  ClientHeight = 150
  ClientWidth = 450
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
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 150
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lblServerIP: TLabel
      Left = 16
      Top = 19
      Width = 49
      Height = 13
      Caption = 'Server IP:'
    end
    object lblStatus: TLabel
      Left = 16
      Top = 75
      Width = 35
      Height = 13
      Caption = 'Status:'
    end
    object lblStats: TLabel
      Left = 16
      Top = 100
      Width = 29
      Height = 13
      Caption = 'Stats:'
    end
    object edtServerIP: TEdit
      Left = 71
      Top = 16
      Width = 150
      Height = 21
      TabOrder = 0
      Text = '127.0.0.1'
    end
    object btnConnect: TButton
      Left = 240
      Top = 14
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 1
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 321
      Top = 14
      Width = 75
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 2
      OnClick = btnDisconnectClick
    end
  end
  object ncClientSource1: TncClientSource
    Port = 3389
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ncClientSource1Connected
    OnDisconnected = ncClientSource1Disconnected
    OnHandleCommand = ncClientSource1HandleCommand
    Host = '127.0.0.1'
    Left = 384
    Top = 96
  end
end
