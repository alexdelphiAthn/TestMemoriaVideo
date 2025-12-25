object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 245
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object lbl1: TLabel
    Left = 52
    Top = 8
    Width = 79
    Height = 15
    Caption = 'Megas a Llenar'
    Visible = False
  end
  object lbl11: TLabel
    Left = 52
    Top = 66
    Width = 228
    Height = 15
    Caption = 'Segundos a esperar entre lectura y escritura'
  end
  object lbl12: TLabel
    Left = 40
    Top = 124
    Width = 290
    Height = 15
    Caption = 'Durante la espera, se ejecutar'#225' un estr'#233's de video en 2D'
  end
  object ProgressBarRAM: TProgressBar
    Left = 0
    Top = 204
    Width = 628
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 203
    ExplicitWidth = 624
  end
  object EditMegas: TEdit
    Left = 73
    Top = 29
    Width = 121
    Height = 23
    NumbersOnly = True
    TabOrder = 1
    Text = '1024'
    Visible = False
  end
  object BtnTestRAM: TButton
    Left = 184
    Top = 120
    Width = 113
    Height = 25
    Caption = 'Comprobar'
    TabOrder = 2
    Visible = False
    OnClick = BtnTestRAMClick
  end
  object MemoLog: TMemo
    Left = 368
    Top = 0
    Width = 260
    Height = 204
    Align = alRight
    Lines.Strings = (
      'MemoLog')
    ScrollBars = ssVertical
    TabOrder = 3
    ExplicitLeft = 364
    ExplicitHeight = 203
  end
  object btnCompleto: TButton
    Left = 40
    Top = 120
    Width = 113
    Height = 25
    Caption = 'Test Completo'
    TabOrder = 4
    Visible = False
    OnClick = btnCompletoClick
  end
  object BtnFullTest: TButton
    Left = 40
    Top = 160
    Width = 257
    Height = 25
    Caption = 'Test Completo con Pausa'
    TabOrder = 5
    OnClick = BtnFullTestClick
  end
  object edtSeconds: TEdit
    Left = 73
    Top = 87
    Width = 121
    Height = 23
    NumbersOnly = True
    TabOrder = 6
    Text = '5'
  end
  object btn1: TButton
    Left = 256
    Top = 28
    Width = 81
    Height = 25
    Caption = 'Estr'#233's video'
    TabOrder = 7
    Visible = False
    OnClick = btn1Click
  end
end
