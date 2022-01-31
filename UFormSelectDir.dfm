object FormSelectDir: TFormSelectDir
  Left = 192
  Top = 124
  Width = 350
  Height = 300
  BorderStyle = bsSizeToolWin
  Caption = 'Seleccionar directorio'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoryListBox1: TDirectoryListBox
    Left = 0
    Top = 33
    Width = 334
    Height = 195
    Align = alClient
    ItemHeight = 16
    TabOrder = 0
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 334
    Height = 33
    Caption = 'ToolBar1'
    TabOrder = 1
    object DriveComboBox1: TDriveComboBox
      Left = 0
      Top = 2
      Width = 241
      Height = 22
      DirList = DirectoryListBox1
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object ToolBar2: TToolBar
    Left = 0
    Top = 228
    Width = 334
    Height = 34
    Align = alBottom
    AutoSize = True
    BorderWidth = 2
    ButtonWidth = 96
    Caption = 'ToolBar2'
    TabOrder = 2
    object ToolButton2: TToolButton
      Left = 0
      Top = 2
      Width = 28
      Caption = 'ToolButton2'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object BitBtn1: TBitBtn
      Left = 28
      Top = 2
      Width = 90
      Height = 22
      Action = ActionCancel
      Caption = 'Cancelar'
      TabOrder = 0
    end
    object ToolButton1: TToolButton
      Left = 118
      Top = 2
      Width = 98
      Caption = 'ToolButton1'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object BitBtn2: TBitBtn
      Left = 216
      Top = 2
      Width = 90
      Height = 22
      Action = ActionAcept
      Caption = 'Aceptar'
      TabOrder = 1
    end
  end
  object ActionList1: TActionList
    Left = 352
    Top = 48
    object ActionAcept: TAction
      Caption = 'Aceptar'
      Hint = 'Aceptar'
      ShortCut = 13
      OnExecute = ActionAceptExecute
    end
    object ActionCancel: TAction
      Caption = 'Cancelar'
      Hint = 'Cancelar'
      ShortCut = 27
      OnExecute = ActionCancelExecute
    end
  end
end
