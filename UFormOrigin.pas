
///////////////////////////////////////////////////////////////////////////////
// Nombre: UCommon
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 2018
// Objetivo: Muestra la procedencia del programa.
///////////////////////////////////////////////////////////////////////////////

unit UFormOrigin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, Buttons, UCommon;

type
  TFormOrigin = class(TForm)
    LabelTitle: TLabel;
    LabelPhone: TLabel;
    LabelCountryYear: TLabel;
    ActionList1: TActionList;
    ActionFormOriginClose: TAction;
    LabelEmail: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ActionFormOriginCloseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOrigin: TFormOrigin;

implementation

{$R *.dfm}

uses UAntiReversing;

//-----------------------------------------------------------------------------
// Establece dinámicamente los texto de las etiquetas.
//-----------------------------------------------------------------------------
procedure TFormOrigin.FormCreate(Sender: TObject);
begin
LabelTitle.Caption := DecStr(cAppTitle) + ' ' + IntToStr(cAppVersion)+'.'+IntToStr(cAppSubVersion);
LabelPhone.Caption := DecStr(cAppPhone) + DecStr('0p1bTo8u;jLb=h6T?');    //' (Santiago)'
LabelCountryYear.Caption := DecStr(cAppCountryYear);
LabelEmail.Caption := DecStr(cAppMail);
Caption := DecStr(cAppTitle) + ' (Origen)';
end;

//-----------------------------------------------------------------------------
// Cierra el formulario.
//-----------------------------------------------------------------------------
procedure TFormOrigin.ActionFormOriginCloseExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Para que el formulario se libere.
//-----------------------------------------------------------------------------
procedure TFormOrigin.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action := caFree;
end;

//-----------------------------------------------------------------------------
// Redimensiona el formulario según el contenido.
//-----------------------------------------------------------------------------
procedure TFormOrigin.FormShow(Sender: TObject);
begin
AutoSize := True;
end;

end.
