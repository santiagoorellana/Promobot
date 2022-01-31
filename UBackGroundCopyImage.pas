
///////////////////////////////////////////////////////////////////////////////
// Nombre: UBackGroundCopyImage
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 2018
// Objetivo: Implementa un subproceso que realiza la copia de la imagen.
///////////////////////////////////////////////////////////////////////////////

unit UBackGroundCopyImage;

interface

uses SysUtils, Classes, Graphics;

type
  TBackGroundCopyImage = class(TThread)
  private
  protected
    procedure Execute; override;
  public
    Source: TPicture;       //Imagen que se debe copiar.
    Destiny: String;        //Lugar donde se debe colocar la imagen.
  end;


implementation

//-----------------------------------------------------------------------------
// Realiza la copia de la información.
//-----------------------------------------------------------------------------
procedure TBackGroundCopyImage.Execute;
begin
Source.SaveToFile(Destiny + '\SeTV+.bmp');
end;


end.
