object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Screenshot Comparator'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 170
    Top = 15
    Width = 26
    Height = 13
    Caption = 'Size: '
  end
  object Image1: TImage
    Left = 10
    Top = 50
    Width = 760
    Height = 500
    Stretch = True
  end
  object Button1: TButton
    Left = 10
    Top = 10
    Width = 150
    Height = 30
    Caption = 'Take Screenshot'
    TabOrder = 0
    OnClick = Button1Click
  end
end
