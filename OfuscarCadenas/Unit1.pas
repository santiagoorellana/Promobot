unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit3: TEdit;
    Label3: TLabel;
    Memo1: TMemo;
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//-----------------------------------------------------------------------------
// Encriptar cadenas de texto con esteganografía.
//
// La encriptación consiste en:
// 1 - Cambiar el primer caracter de la cadena por el último.
// 2 - Aplicar el algoritmo CESAR k = 1 a los caracteres originales.
// 3 - Agregar delante de cada caracter original, un caracter aleatorio.
//-----------------------------------------------------------------------------
function EncodeString(c: String): String;
var i,Leng: integer;
    ch: char;
begin
Result := '';
Leng := length(c);
if Leng = 0 then Exit;
ch := c[1];
c[1] := c[Leng];
c[Leng] := ch;
for i := 1 to Leng do
    if ((i mod 2)<>0) then
       begin
       Result := Result + Chr($30 + Random(42)) + Char(Ord(c[i]) + 1);
       Result := Result + Chr($30 + Random(42));
       end
    else
       Result := Result + Char(Ord(c[i]) + 1);
if (Leng mod 2 = 0) then Result := Result + Chr($30 + Random(42));
end;

//-----------------------------------------------------------------------------
// Desencriptar cadenas de texto con esteganografía.
//
// La desencriptación consiste en:
// 1 - Eliminar los caracteres aleatorios que se han insertado.
// 2 - Invertir el algoritmo CESAR k = 1 aplicado a los caracteres originales.
// 3 - Cambiar el primer caracter de la cadena por el último.
//-----------------------------------------------------------------------------
function DecodeString(c: String): String;
var i, Leng: integer;
    ch: char;
begin
Result := '';
Leng := length(c);
if Leng = 0 then Exit;
for i := 2 to Leng do
    if ((i mod 2)=0) then
       Result := Result + Char(Ord(c[i]) - 1);
Leng := Length(Result);
ch := Result[1];
Result[1] := Result[Leng];
Result[Leng] := ch;
end;

//-----------------------------------------------------------------------------
// Codifica el texto insertado y verifica la validez de la codificación.
//-----------------------------------------------------------------------------
procedure TForm1.Edit1Change(Sender: TObject);
begin
Edit2.Text := EncodeString(Edit1.Text);
Edit3.Text := DecodeString(Edit2.Text);
if Edit3.Text <> Edit1.Text then
   Edit3.Color := RGB(200, 100, 100)
else
   Edit3.Color := clWindow;   
end;

end.
 