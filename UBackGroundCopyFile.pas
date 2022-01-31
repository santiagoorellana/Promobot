
///////////////////////////////////////////////////////////////////////////////
// Nombre: UBackGroundCopyImage
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 2018
// Objetivo: Implementa un subproceso que realiza la copia de los ficheros.
//           Solo copia los ficheros que se encuentren en la carpeta
//           seleccionada por el usuario del programa y que tengan
//           las extensiones siguientes...
//           Imagenes (bmp, dib, gif, png, tif, tiff, jpg, jpeg, jpe, jfif)
//           Texto (txt, pdf)
///////////////////////////////////////////////////////////////////////////////

unit UBackGroundCopyFile;

interface

uses SysUtils, Classes;

type
  TBackGroundCopyFile = class(TThread)
  private
    procedure CopyDirectory(Source: String; Destiny: String; CopySubDir: Boolean);
    procedure CopyFile(Source: String; Destiny: String);
  protected
    procedure Execute; override;
    function CanCopyImage(ext: String): Boolean;
  public
    Source: String;           //Carpeta que contiene los ficheros.
    Destiny: String;          //Lugar donde se deben colocar los ficheros.
  end;


implementation

//-----------------------------------------------------------------------------
// Realiza la copia de la información.
//-----------------------------------------------------------------------------
procedure TBackGroundCopyFile.Execute;
begin
CopyDirectory(Source, Destiny, False);
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si la extensión está permitida para la copia.
//-----------------------------------------------------------------------------
function TBackGroundCopyFile.CanCopyImage(ext: String): Boolean;
var e: String;
begin
Result := True;
e := LowerCase(ext);

if e = 'txt' then Exit;
if e = 'pdf' then Exit;

if e = 'bmp' then Exit;
if e = 'dib' then Exit;
if e = 'gif' then Exit;
if e = 'tif' then Exit;
if e = 'tiff' then Exit;
if e = 'png' then Exit;
if e = 'jpg' then Exit;
if e = 'jpeg' then Exit;
if e = 'jpe' then Exit;
if e = 'jfif' then Exit;
Result := False;
end;

//-----------------------------------------------------------------------------
// Copia el contenido del directorio fuente en el destino.
// Entradas
// Source = Fuente de la copia. No debe terminar con "\".
// Destiny = Directorio destino de la copia. No debe terminar con "\".
// CopySubDir = TRUE si se deben copiar los subdirectorios.
//-----------------------------------------------------------------------------
procedure TBackGroundCopyFile.CopyDirectory(Source: String; Destiny: String; CopySubDir: Boolean);
var Files: integer;
    FOrigen, FDesti : string;
    ok: boolean;
    Search: TSearchRec;
begin
Files := FindFirst(Source + '\*.*', faAnyFile, Search);
while Files = 0 do
      begin
      if Search.Attr <> faDirectory then                    //Si no es un directorio...
         begin
         FOrigen := Source + '\' + Search.Name;             //Crea la ruta fuente.
         FDesti := Destiny + '\' + Search.Name;             //Crea la ruta destino.
         if CanCopyImage(ExtractFileExt(Search.Name)) then  //Si la extensión es válida...
            CopyFile(PChar(FOrigen),PChar(FDesti));         //Hace la copia del fichero.
         end;
      Files := FindNext(Search);
      end;
FindClose(Search);
end;

//-----------------------------------------------------------------------------
// Copia el contenido del directorio fuente en el destino.
// Entradas
// Source = Ruta y nombre del fichero que se desea copiar.
// Destiny = Ruta y nombre del fichero destino.
//-----------------------------------------------------------------------------
procedure TBackGroundCopyFile.CopyFile(Source: String; Destiny: String);
var
   fSource, fDestiny: file of byte;                           //Handles de los ficheros.
   Buffer: array[0..4096] of char;                            //Buffer para copiar los datos.
   Read: Integer;                                             //Bytes que se han leido.
   Longitud: longint;                                         //Longitud del fichero.
begin
if not FileExists(Source) then Exit;                          //Sale si el fichero no existe.
try
   AssignFile(fSource, Source);                               //Establecemos el fichero Origen
   reset(fSource);                                            //y lo abrimos para leerlo.
   AssignFile(fDestiny, Destiny);                             //Establecemos un fichero Destino.
   rewrite(fDestiny);                                         //Creamos el fichero destino.
except                                                        //Si ocurre un error.
  Exit;                                                       //Sale de la función.
end;

Longitud := FileSize(fSource);                                //Hallamos la longitud del fichero a copiar.
while Longitud >0 do                                          //Copia bloque por bloque.
      try
         BlockRead(fSource, Buffer[0], SizeOf(Buffer), Read); //Lee un bloque.
         Longitud := Longitud - Read;                         //Actualiza el contador de la copia.
         BlockWrite(fDestiny, Buffer[0], Read);               //Escribe en el fichero destino.
      except                                                  //Si ocurre un error.
         Exit;                                                //Sale de la función.
      end;
CloseFile(fSource);                                           //Cierra el fichero fuente.
CloseFile(fDestiny);                                          //Cierra el fichero destino.
end;


end.
