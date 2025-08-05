object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Screenshot Analyzer Server - Waiting for connections'
  ClientHeight = 700
  ClientWidth = 900
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
  object lblServerTitle: TLabel
    Left = 10
    Top = 50
    Width = 144
    Height = 16
    Caption = 'Received Screenshots'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblStatus: TLabel
    Left = 300
    Top = 15
    Width = 113
    Height = 13
    Caption = 'Server Status: Stopped'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblClientInfo: TLabel
    Left = 10
    Top = 580
    Width = 114
    Height = 13
    Caption = 'Connected Clients: 0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblStats: TLabel
    Left = 220
    Top = 580
    Width = 196
    Height = 13
    Caption = 'Total Received: 0 bytes | Screenshots: 0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblPixelChanges: TLabel
    Left = 10
    Top = 600
    Width = 106
    Height = 13
    Caption = 'Last Delta Size: 0 B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clPurple
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ImageReceived: TImage
    Left = 10
    Top = 70
    Width = 600
    Height = 400
    Center = True
    Proportional = True
    Stretch = True
  end
  object btnStartServer: TButton
    Left = 10
    Top = 10
    Width = 120
    Height = 30
    Caption = 'Start Server'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = btnStartServerClick
  end
  object btnRequestScreenshot: TButton
    Left = 140
    Top = 10
    Width = 140
    Height = 30
    Caption = 'Request Screenshot'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = btnRequestScreenshotClick
  end
  object memoLog: TMemo
    Left = 620
    Top = 70
    Width = 260
    Height = 400
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ncServerSource1: TncServerSource
    Port = 8080
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ncServerSource1Connected
    OnDisconnected = ncServerSource1Disconnected
    OnHandleCommand = ncServerSource1HandleCommand
    Left = 750
    Top = 500
  end
end
