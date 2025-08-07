object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'RDP Client'
  ClientHeight = 200
  ClientWidth = 400
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
    Width = 400
    Height = 200
    Align = alClient
    TabOrder = 0
    object lblStatus: TLabel
      Left = 16
      Top = 56
      Width = 64
      Height = 13
      Caption = 'Disconnected'
    end
    object lblTotalData: TLabel
      Left = 16
      Top = 80
      Width = 71
      Height = 13
      Caption = 'Total Sent: 0 B'
    end
    object lblCurrentDelta: TLabel
      Left = 16
      Top = 104
      Width = 87
      Height = 13
      Caption = 'Current Delta: 0 B'
    end
    object edtServerIP: TEdit
      Left = 16
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '192.168.1.100'
    end
    object btnConnect: TButton
      Left = 152
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 1
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 240
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Disconnect'
      Enabled = False
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
    Left = 336
    Top = 16
  end
end
