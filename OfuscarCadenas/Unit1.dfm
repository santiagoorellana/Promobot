object Form1: TForm1
  Left = 192
  Top = 124
  Width = 664
  Height = 436
  Caption = 'Codificador/Decodificador de String para Delphi'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    648
    398)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 61
    Top = 13
    Width = 41
    Height = 16
    Caption = 'Plano'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 28
    Top = 45
    Width = 76
    Height = 16
    Caption = 'Codificado'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 11
    Top = 76
    Width = 94
    Height = 16
    Caption = 'Decodificado'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 112
    Top = 8
    Width = 531
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 112
    Top = 40
    Width = 531
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 112
    Top = 72
    Width = 531
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
  end
  object Memo1: TMemo
    Left = 5
    Top = 103
    Width = 638
    Height = 291
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clMoneyGreen
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      
        '//--------------------------------------------------------------' +
        '---------------'
      '// Encriptar cadenas de texto con esteganograf'#237'a.'
      '//'
      '// La encriptaci'#243'n consiste en:'
      '// 1 - Cambiar el primer caracter de la cadena por el '#250'ltimo.'
      
        '// 2 - Aplicar el algoritmo CESAR k = 1 a los caracteres origina' +
        'les.'
      
        '// 3 - Agregar delante de cada caracter original, un caracter al' +
        'eatorio.'
      
        '//--------------------------------------------------------------' +
        '---------------'
      'function EncodeString(c: String): String;'
      'var i,Leng: integer;'
      '    ch: char;'
      'begin'
      'Result := '#39#39';'
      'Leng := length(c);'
      'if Leng = 0 then Exit;'
      'ch := c[1];'
      'c[1] := c[Leng];'
      'c[Leng] := ch;'
      'for i := 1 to Leng do'
      '    if ((i mod 2)<>0) then'
      '       begin'
      
        '       Result := Result + Chr($30 + Random(42)) + Char(Ord(c[i])' +
        ' + 1);'
      '       Result := Result + Chr($30 + Random(42));'
      '       end'
      '    else'
      '       Result := Result + Char(Ord(c[i]) + 1);'
      
        'if (Leng mod 2 = 0) then Result := Result + Chr($30 + Random(42)' +
        ');'
      'end;'
      ''
      
        '//--------------------------------------------------------------' +
        '---------------'
      '// Desencriptar cadenas de texto con esteganograf'#237'a.'
      '//'
      '// La desencriptaci'#243'n consiste en:'
      '// 1 - Eliminar los caracteres aleatorios que se han insertado.'
      
        '// 2 - Invertir el algoritmo CESAR k = 1 aplicado a los caracter' +
        'es originales.'
      '// 3 - Cambiar el primer caracter de la cadena por el '#250'ltimo.'
      
        '//--------------------------------------------------------------' +
        '---------------'
      'function DecodeString(c: String): String;'
      'var i, Leng: integer;'
      '    ch: char;'
      'begin'
      'Result := '#39#39';'
      'Leng := length(c);'
      'if Leng = 0 then Exit;'
      'for i := 2 to Leng do'
      '    if ((i mod 2)=0) then'
      '       Result := Result + Char(Ord(c[i]) - 1);'
      'Leng := Length(Result);'
      'ch := Result[1];'
      'Result[1] := Result[Leng];'
      'Result[Leng] := ch;'
      'end;')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
  end
end
