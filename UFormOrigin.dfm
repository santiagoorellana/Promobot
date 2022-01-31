object FormOrigin: TFormOrigin
  Left = 217
  Top = 145
  BorderStyle = bsToolWindow
  BorderWidth = 5
  Caption = 'CHIVATO (Origen)'
  ClientHeight = 151
  ClientWidth = 239
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
    0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000}
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelTitle: TLabel
    Left = 0
    Top = 0
    Width = 168
    Height = 54
    Caption = '000000'
    Font.Charset = ANSI_CHARSET
    Font.Color = clNavy
    Font.Height = -48
    Font.Name = 'Bauhaus 93'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelPhone: TLabel
    Left = 6
    Top = 89
    Width = 169
    Height = 18
    Caption = 'Tel'#233'fono: 00000000'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelCountryYear: TLabel
    Left = 7
    Top = 117
    Width = 105
    Height = 18
    Caption = 'Cuba... 2018'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelEmail: TLabel
    Left = 7
    Top = 62
    Width = 139
    Height = 18
    Caption = 'Correo@nauta.cu'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ActionList1: TActionList
    Left = 200
    Top = 56
    object ActionFormOriginClose: TAction
      Hint = 'Cerrar'
      ImageIndex = 0
      ShortCut = 27
      OnExecute = ActionFormOriginCloseExecute
    end
  end
end
