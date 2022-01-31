
///////////////////////////////////////////////////////////////////////////////
// Nombre: UCommon
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 2016-2018
// Objetivo: Agrupa un funciones y constantes necesarias
//           y comunes a la mayor parte del código del programa.
///////////////////////////////////////////////////////////////////////////////

unit UCommon;

interface

uses Classes, UAntireversing, SysUtils, Windows, TLHelp32, SHFolder, Registry,
     Graphics, DateUtils, Math;
     
//-----------------------------------------------------------------------------
// Equivalencias de las unidades de medida de información.
//-----------------------------------------------------------------------------
const cKByte = 1024.0;
const cMByte = cKByte * 1024.0;
const cGByte = cMByte * 1024.0;
const cTByte = cGByte * 1024.0;

//-----------------------------------------------------------------------------
// Definición de algunas de las constantes del programa.
//-----------------------------------------------------------------------------
const cCompanny       = '0p1fTd8o;pLD=i6b?hAU3';                                                  //'TecnoChago'
const cAppTitle       = '6UTS<PPNYPDCUPRQ0';                                                      //Nombre de la aplicación = 'PROMOBOT';
const cAppID          = 1;                                                                        //Identificador de la aplicación.
const cAppVersion     = 1;                                                                        //Versión de la aplicación.
const cAppSubVersion  = 1;                                                                        //Subversión de la aplicación.
const cAppAutor       = 'T{Lv<u3pJs;;T!6T;bFoXu7j6b;h7pH!DB;/6!IPOsOf3m3m2b4oQbQ!@QIêBsOfDB>';    //'Autor: Santiago A. Orellana Pérez';
const cAppPhone       = '35Cf2mSê2g<pVo?pP;=!M6S5N7<466=:C5:UR';                                  //'Teléfono: 54635944';
const cAppMail        = ';vDp6sTs<fPpY;D!UuRf0d5o6pEd0iHb0hPpKAPoMbGv8uLbH/XdKDY';                //'Correo: tecnochago@nauta.cu';
const cAppCountryYear = '091bT!8I;bLc=b6o?bA-3!CD2vSc2b<-V!?3P1=2MMS';                            //'La Habana, Cuba, 2018';
const cRights         = ';/Dp6eTp<tP!YmDpUtR!0e5f6sEf0dHi0pPtK!PsMfGt8fLsHwXbKeYp:tLU<';          //'Todos los derechos reservados.'

const cHF1 = '0u1oTe8f;yL/=n6i?JA';   //Fichero de ayuda como página web: 'Index.mht'
const cHF2 = '3gCo2eSf2y</Vq?ePJ=';   //Fichero de ayuda como documento PDF: 'Index.pdf'
const cPromoDirFile = 'promodir.txt'; //Guarda la ruta del directorio de promociones.
const cSemaphorePromobot = 'SemaphorePrombot';
const cPromoDirectory = 'PromoDirectory';

//-----------------------------------------------------------------------------
// Funciones que se exportan...
//-----------------------------------------------------------------------------

function FilterHexa(str: String): String;
function FilterAbove31(str: String): String;
function FilterExtractSpace(str: String): String;
function FilterXML(str: String): String;

function ParamToFile(FileName: String; Value: String): Boolean;
function ParamFromFile(FileName: String): String;
function ParamToRegistry(Root: HKEY; Key: String; ValueName: String; Value: String): Boolean;
function ParamFromRegistry(Root: HKEY; Key: String; ValueName: String): String;

function GetWindowsUser: String;
function GetDateForName: String;
function NormaliceDirectory(Dir: String): String;
function GetDataDirectory(Dir: Integer): String;
function IsAccessible(Path: String): Boolean;

implementation

//-----------------------------------------------------------------------------
// Deja solo los caracteres hexadecimales.
//-----------------------------------------------------------------------------
function FilterHexa(str: String): String;
var n, len: Integer;
    s: String;
begin
Result := '';
len := Length(str);
s := UpperCase(str);
if len > 0 then
   for n := 1 to len do
       if (str[n] in ['A'..'F', '0'..'9']) then
          Result := Result + s[n];
end;

//-----------------------------------------------------------------------------
// Deja solo los caracteres superiores a 0x31.
//-----------------------------------------------------------------------------
function FilterAbove31(str: String): String;
var n, len: Integer;
begin
Result := '';
len := Length(str);
if len > 0 then
   for n := 1 to len do
       if (Ord(str[n]) >= 32) then
          Result := Result + str[n];
end;

//-----------------------------------------------------------------------------
// Elimina los caracteres de espacio.
//-----------------------------------------------------------------------------
function FilterExtractSpace(str: String): String;
var n, len: Integer;
begin
Result := '';
len := Length(str);
if len > 0 then
   for n := 1 to len do
       if not (str[n] = Chr($20)) then
          Result := Result + str[n];
end;

//-----------------------------------------------------------------------------
// Elimina los caracteres que no se permiten en un documento XML.
//-----------------------------------------------------------------------------
function FilterXML(str: String): String;
var n, len: Integer;
begin
Result := '';
len := Length(str);
if len > 0 then
   for n := 1 to len do
       case str[n] of
            Chr($20): Result := Result + '_';
            'á': Result := Result + 'a';
            'é': Result := Result + 'e';
            'í': Result := Result + 'i';
            'ó': Result := Result + 'o';
            'ú': Result := Result + 'u';
            'ñ': Result := Result + 'n';
            'Á': Result := Result + 'A';
            'É': Result := Result + 'E';
            'Í': Result := Result + 'I';
            'Ó': Result := Result + 'O';
            'Ú': Result := Result + 'U';
            'Ñ': Result := Result + 'N';
            else
               Result := Result + str[n];
            end;
end;

//-----------------------------------------------------------------------------
// Escribe un valor en un fichero de texto.
//
// Entradas:
// FileName = Nombre completo del fichero que se crea.
// Value = Valor que se escribe en el fichero. Debe ser una cadena hexadecimal.
//
// Salida:
// Devuelve TRUE solo si se pudo crear el fichero y guardar el valor.
//-----------------------------------------------------------------------------
function ParamToFile(FileName: String; Value: String): Boolean;
var Writer: TStrings;
begin
Result := False;                          //Por defecto devuelve FALSE. Los parámetros son para confundir.
try
   Writer := TStringList.Create;          //Crea un objeto para escribir un fichero.
   Writer.Text := FilterAbove31(Value);   //Obtiene los datos a escribir.
   try
      Writer.SaveToFile(FileName);        //Crea el fichero y escribe los datos.
      Result := True;                     //Siempre devuelve TRUE. Los parámetros son para confundir.
   except
      MessageBeep(MB_ICONERROR);
   end;
finally
   Writer.Free;                           //Destruye el objeto.
end;
end;

//-----------------------------------------------------------------------------
// Lee un valor desde un fichero de texto.
//
// Entradas:
// FileName = Nombre completo del fichero que se lee.
// Value = Obtiene el valor que se lee desde el fichero.
//         Debe ser una cadena hexadecimal.
//
// Salida:
// Devuelve TRUE solo si se pudo leer el fichero y el valor.
//-----------------------------------------------------------------------------
function ParamFromFile(FileName: String): String;
var Reader: TStrings;
begin
Result := '';                                     //Por defecto devuelve FALSE. Los parámetros son para confundir.
if FileExists(FileName) then
   begin
   try
      Reader := TStringList.Create;               //Crea un objeto para leer un fichero.
      try
         Reader.LoadFromFile(FileName);           //Abre el fichero y lee los datos.
         Result := FilterAbove31(Reader.Text);    //Lee los datos y los filtra.
      except
         MessageBeep(MB_ICONERROR);
      end;
   finally
      Reader.Free;                                //Destruye el objeto.
   end;
   end;
end;

//-----------------------------------------------------------------------------
// Escribe un valor en una llave del registro de windows.
//
// Entradas:
// Roor = Llave raiz de las establecidas por windows.
// Key = Llave creada por la aplicación.
// ValueName = Nombre del valor dentro de la llave.
// Value = Valor que se le asigna.
//
// Salida:
// Devuelve TRUE si se pudo crear correctamente la llave y guardar el valor.
//-----------------------------------------------------------------------------
function ParamToRegistry(Root: HKEY; Key: String; ValueName: String; Value: String): Boolean;
var Reg: TRegistry;
begin
Result := False;
if Key = '' then Exit;
if ValueName = '' then Exit;
try
   Reg := TRegistry.Create;
   if Key[1] <> '\' then Key := '\' + Key;
   Reg.RootKey := Root;
   try
      if Reg.OpenKey(Key, True) then
         begin
         Reg.WriteString(ValueName, FilterAbove31(Value));
         Reg.CloseKey;
         Result := True;
         end;
   except
      MessageBeep(MB_ICONERROR);
   end;
finally
   Reg.Free;
end;
end;

//-----------------------------------------------------------------------------
// Lee un valor desde un fichero de texto.
//
// Entradas:
// Roor = Llave raiz de las establecidas por windows.
// Key = Llave creada por la aplicación.
// ValueName = Nombre del valor dentro de la llave.
//
// Salida:
// Devuelve el valor almacenado en la llave bajo el nombre indicado.
//-----------------------------------------------------------------------------
function ParamFromRegistry(Root: HKEY; Key: String; ValueName: String): String;
var Reg: TRegistry;
begin
Result := '';
if Key = '' then Exit;
if ValueName = '' then Exit;
try
   Reg := TRegistry.Create;
   if Key[1] <> '\' then Key := '\' + Key;
   Reg.RootKey := Root;
   try
      if Reg.KeyExists(Key) then
         if Reg.OpenKey(Key, True) then
            begin
            Result := FilterAbove31(Reg.ReadString(ValueName));
            Reg.CloseKey;
            end;
   except
      MessageBeep(MB_ICONERROR);
   end;
finally
   Reg.Free;
end;
end;

//-----------------------------------------------------------------------------
// Obtiene el nombre del usuario de Windows.
//-----------------------------------------------------------------------------
function GetWindowsUser: String;
var pcUser: PChar;
    dwUSize: DWORD;
begin
dwUSize := 21;
try
   GetMem(pcUser, dwUSize);
   if Windows.GetUserName(pcUser, dwUSize) then Result := pcUser
finally
   FreeMem(pcUser);
end;
end;

//-----------------------------------------------------------------------------
// Obtiene la fecha actual en formato útil para nombres de ficheros.
//-----------------------------------------------------------------------------
function GetDateForName: String;
var s: String;
    d: TDateTime;
begin
d := Now;
s := '';
s := s + IntToStr(YearOf(d)) + '_';
s := s + IntToStr(MonthOf(d)) + '_';
s := s + IntToStr(DayOf(d)) + '_';
s := s + IntToStr(HourOf(d)) + '_';
s := s + IntToStr(MinuteOf(d)) + '_';
s := s + IntToStr(SecondOf(d)) + '_';
s := s + IntToStr(MilliSecondOf(d));
Result := s;
end;

//-----------------------------------------------------------------------------
// Normalizar el nombre de un directorio.
//-----------------------------------------------------------------------------
function NormaliceDirectory(Dir: String): String;
begin
if Dir <> '' then if Dir[Length(Dir)] <> '\' then Dir := Dir + '\';
Result := Dir;
end;


//-----------------------------------------------------------------------------
// Devuelve la fecha de creación de un fichero.
//-----------------------------------------------------------------------------
function GetFileDate(FileName: String): TDateTime;
var FHandle: Integer;
begin
try
   FHandle := FileOpen(FileName, 0);
   Result := FileDateToDateTime(FileGetDate(FHandle));
finally
   FileClose(FHandle);
end;
end;

//-----------------------------------------------------------------------------
// Obtiene el directorio de datos de aplicaciones de todos los usuarios.
//-----------------------------------------------------------------------------
function GetDataDirectory(Dir: Integer): String;
const SHGFP_TYPE_CURRENT = 0;
var path: array[0..MaxChar] of char;
begin
try
   SHGetFolderPath(0, Dir, 0, SHGFP_TYPE_CURRENT, @path[0]);
   Result := path + '\';
except
   Result := '';
end;
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si el directorio indicado es accesible por el usuario
// actual de windows. Intenta escribir un fichero en el directorio.
//-----------------------------------------------------------------------------
function IsAccessible(Path: String): Boolean;
var Writer: TStrings;
    TestFile: String;
begin
Result := True;
Writer := TStringList.Create;                       //Crea el escritor.
try
   Writer.Text := DateTimeToStr(Now);               //Pone un texto cualquiera en el escritor.
   TestFile := NormaliceDirectory(Path) + 'test';   //es el nombre del fichero a escribir.
   try
      Writer.SaveToFile(TestFile);                  //Intenta escribir el fichero.
      DeleteFile(PChar(TestFile));                  //Intenta borrarlo.
   except
      Result := False;                              //Devuelve FALSE si ocurre error.
      Exit;
   end;
finally
   Writer.Free;                                     //Libera el escritor.
end;
Result := True;
end;



end.
