
///////////////////////////////////////////////////////////////////////////////
// Nombre: UAntiReversing
// Creado: 5/09/2016
// Objetivo: Implementa funciones útiles para dificultar el debuggeo y
//           el análisis de ingeniería inversa. 
// Autor: Santiago Alejandro Orellana Pérez
///////////////////////////////////////////////////////////////////////////////

unit UAntiReversing;

//-----------------------------------------------------------------------------
// La definición "DEVELOPMENT" se utiliza cuando se está programando y es
// necesario debuggear el código desde el mismo entorno de trabajo del Delphi.
// Para debuggear solo hay que descomentariar la linea siguiente.
//-----------------------------------------------------------------------------

interface

uses Windows, SysUtils, TlHelp32;

const cSep = ';';

//-----------------------------------------------------------------------------
// Funciones y variables que se exportan.
//-----------------------------------------------------------------------------

//Esta variable es puesta a TRUE por las funciones IsDBG1 y IsDBG2.
//Se pueden utilizar para comprobar desde el llamador que se ha
//ejecutado la función. Esto es contra los parcheos y crack.
var Executed: Boolean;

//Funciones de encriptación simple de cadenas.
function EncStr(c: String): String;     //Encriptador simple de cadenas de texto.
function DecStr(c: String): String;     //Desencriptador simple de cadenas de texto.

//Funciones criptográficas.
function Sha256(Texto: String): String;

//Funciones para proteger cadenas con chequeo contra modificaciones.
function EncProtectStr(S: String): String;
function DecProtectStr(S: String): String;

//Funciones para obtención de identificadores y nombres.
function CreateNameFor(str: String): String;  //Obtiene un nombre único de 32 a partir de una cadena cualquiera.


implementation

uses USHA256, URC4;

//-----------------------------------------------------------------------------
// Encriptar cadenas de texto con esteganografía.
//
// La encriptación consiste en:
// 1 - Cambiar el primer caracter de la cadena por el último.
// 2 - Aplicar el algoritmo CESAR k = 1 a los caracteres originales.
// 3 - Agregar delante de cada caracter original, un caracter aleatorio.
//-----------------------------------------------------------------------------
function EncStr(c: String): String;
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
function DecStr(c: String): String;
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
// Calcula el HASH de la cadena mediante el algoritmo SHA256 y devuelve
// los 32 Bytes como una cadena de texto de valores en hexadecimal.
//-----------------------------------------------------------------------------
function Sha256(Texto: String): String;
var Hash: TSHA256;
    n: Integer;
    Digest: array[0..31] of byte;   //SHA256 produce 256 bit (32 byte) de salida.
begin
Hash := TSha256.Create(nil);        //Crea una instancia del generador de HASH.
Hash.Init;                          //Inicia el objeto.
Hash.UpdateStr(Texto);              //Calcula el HASH del texto.
Hash.Final(Digest);                 //Guarda el resultado en un arreglo local.

Result := '';                       //Devuelve el arreglo como una cadena hexadecimal.
for n := 0 to 31 do
    Result := Result + IntToHex(Digest[n], 2);

Hash.Burn;                          //Borra de la memoria los datos comprometedores.
Hash.Free;                          //Libera el objeto.
end;

//-----------------------------------------------------------------------------
// Dada una cadena de texto, esta función devuelve otra cadena
// cadificada que está protegida contra cambios.
//
// Composición:
// Primero se coloca la cadena y luego un caracter ';' como separador.
// A continuación del separador se coloca el Sha256 de la cadena.
// Y luego esta cadena resultante es codificada para protegerla utilizando
// el algoritmo RC4 y como llave el HardwareID de la computadora.
//-----------------------------------------------------------------------------
function EncProtectStr(S: String): String;
begin
Result := S + cSep + Sha256(S);
end;

//-----------------------------------------------------------------------------
// Dada una cadena de texto codificada que está protegida contra cambios
// esta función devuelve la cadena en texto claro.
//-----------------------------------------------------------------------------
function DecProtectStr(S: String): String;
var P: Integer;
    R, str, hash: String;
begin
Result := '';                               //Por efecto devuelve una cadena nula.
R := S;                                     
if R <> '' then                             //Si el resultado es una cadena válida...
   begin
   P := Pos(cSep, R);                       //Optiene la posición del separador.
   if P > 0 then                            //Si el separador existe...
      begin
      str := Copy(R, 1, P - 1);             //Obtiene la cadena de texto.
      hash := Copy(R, P + 1, Length(R));    //Obtiene la cadena de texto.
      try
         if Sha256(str) = hash then         //Si el hash calculado coincide con el almacenado...
            Result := str                   //Devuelve la cadena almacenada.
      except
         Result := '';
      end;
      end;
   end;
end;

//-----------------------------------------------------------------------------
// Devuelve un nombre de 64 caracteres creado a partir del HardwareID y
// una cadena que se utiliza como elemento de diversificación.
//
// Se utilizan para nombres de ficheros y de llaves en el registro de Windows.
// El HardwareID se obtiene en tiempo real cuando se llama a la función.
//
// Entrada:
// init = Cadena de texto que se antepone al nombre creado.
// str = Cadena con un nombre significativo. Debe tener al menos un caracter.
//       Para cada nombre que se desee generar, la cadena str debe ser
//       diferente. Si la cadena está vacía, no se devuelve nada.
//       Se recomiendan str de longitud menor a 16 caracteres.
//
// Salida:
// Devuelve un nombre de 64 caracteres de longitud.
//
// Nota:
// Esta función genera los nombres a partir del XOR realizado entre el
// HardwareID de la computadora y el Sha256 de la cadena STR que se
// especifique. Tanto el HardwareID como el Sha256 resultante son de
// 256 bits, lo cual equivale a 32 bytes, por lo que al representarse
// en hexadecimal resultan 64 caracteres.
// El hardwareID asegura que el nombre resultante sea diferente en cada
// computadora, y la cadena STR asegura la diferencia entre los diferentes
// nombres que se generen. Por tanto, la responsabilidad de que cada nombre
// generado sea diferente, recea en el que llama a esta función.
//-----------------------------------------------------------------------------
function CreateNameFor(str: String): String;
var n: Integer;
    hash: String;
begin
Result := '';
if str = '' then Exit;                //Sale si la cadena STR está vacía.
Result := Sha256(str);                //Calcula el Sha256 de la cadena str.
end;


//-----------------------------------------------------------------------------
// Le pregunta al sistema operativo si está cargado por un debuggeador.
//-----------------------------------------------------------------------------
function IsDebuggerPresent():BOOL;
         stdcall;
         external 'kernel32.dll' name 'IsDebuggerPresent';

//-----------------------------------------------------------------------------
//Auxiliar: Obtien el directorio del sistema.
//-----------------------------------------------------------------------------
function GetSys: String;
var Gsys : array[0..MAX_PATH] of Char;
begin
GetSystemDirectory(Gsys, MAX_PATH);
Result := Gsys;
if length(Result)>0 then
if Result[length(Result)]<>'\' then Result := Result+'\';
end;

//-----------------------------------------------------------------------------
// Convierte una cadena completa a mayúscula.
//-----------------------------------------------------------------------------
function UpCaseStr(S: string): String;
var i: Integer;
begin
Result := s;
if s = '' then exit;
for i := 1 to Length(s) do
Result[i] := UpCase(Result[i]);
end;

//-----------------------------------------------------------------------------
// Ejecuta instrucciones en ensamblador.
//-----------------------------------------------------------------------------
function RDTSC: Int64; assembler;
asm
  PUSH EDI
  PUSH EDI
  PUSH EDI
  PUSH EDI
  DB 0fh ,031h
  POP EDI
  POP EDI
  POP EDI
  POP EDI
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si es un debuggeador en modo Ring 0.
//-----------------------------------------------------------------------------
function IsRing0DBG(S: String): boolean;
var hFile: Thandle;
begin
Result := False;
hFile := CreateFileA(Pchar(S), GENERIC_READ or GENERIC_WRITE, 0, nil,
                     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
if (hFile <> INVALID_HANDLE_VALUE) then
   begin
   CloseHandle(hFile);
   Result := TRUE;
   end;
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si se encuentra un debuggeador.
// Puede detectar los debuggers más utilizados:
// OllyDBG, Immunity Debugger, WinDbg, W32DAsm, IDA, SoftICE, Syser, TRW, TWX
// Probado en: Win9x, Me, 2k, XP, 2k3, Vista, 7
// Para simular polimorfismo, se han programado varias versiones de esta
// función, las cuales hacen exactamente lo mismo. 
//-----------------------------------------------------------------------------
function IsDBG1: Boolean;
var Found: Boolean;
{$IF not Defined(DEVELOPMENT)}
    SysDir, PIExeFile, MIExePath: String;
    i: Integer;
    hSnapmod: THANDLE;
    ModInfo: MODULEENTRY32;
    hSnap: THANDLE;
    ProcessInfo: PROCESSENTRY32;
    ProcID:DWORD;
    Tm1,Tm2:Int64;
{$IFEND}
begin
Found := False;
{$IF not Defined(DEVELOPMENT)}
Tm1 := RDTSC;
for i := 0 to 255 do
    OutputDebugStringA(PChar(DecStr('MmUf=sDo<fPmO4G3N/AeFmIlT')));     //'kernel32.dll'
Tm2 := RDTSC - Tm1;
if Tm2 < 9999 then Found := True;
if Tm2 > 299999999 then Found:=True;
hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS ,0);
ProcessInfo.dwSize := sizeof(PROCESSENTRY32);
Process32First(hSnap,ProcessInfo);
repeat
   PIExeFile := UpCaseStr(ProcessInfo.szExeFile);
   if Pos(DecStr('LH4MCM@Z4E2CGPL'), PIExeFile) <> 0 then Found := True; //'OLLYDBG'
   if Pos(DecStr('3HVCSEL'), PIExeFile) <> 0 then Found := True;         //'DBG'
   if Pos(DecStr('VH;FMCNVDEK'), PIExeFile) <> 0 then Found := True;     //'DEBUG'
   if Pos(DecStr('6H9EWB?J5'), PIExeFile) <> 0 then Found := True;       //'IDAG'
   if Pos(DecStr('YN<4234ETTGXM'), PIExeFile) <> 0 then Found := True;   //'W32DSM'
   
   ProcID := ProcessInfo.th32ProcessID;
   hSnapMod := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcID);
   ModInfo.dwSize := sizeof(MODULEENTRY32);
   Module32First(hSnapMod,ModInfo);
   repeat
      MIExePath := UpCaseStr(ModInfo.szExePath);
      if Pos(DecStr('4HLM0M=ZUEFC=PB'), MIExePath) <> 0 then Found := True;  //'OLLYDBG'
      if Pos(DecStr('UN=4T3PEXT;X@'), MIExePath) <> 0 then Found := True;    //'W32DSM'
      until (not Module32Next(hSnapMod, ModInfo));
   CloseHandle(hSnapMod);
   until (not Process32Next(hSnap, ProcessInfo));
CloseHandle(hSnap);
SysDir := GetSys;
if FileExists(SysDir + DecStr('0t1sTj8w;fLs=t6]?tAj3dCf2/St2z<eV')) then Found := True;       //'drivers\sice.sys'
if FileExists(SysDir + DecStr('YtDsUjRw0f5s6tE]0oHu0jPdKfP/MtGz8eL')) then Found := True;     //'drivers\ntice.sys'
if FileExists(SysDir + DecStr('HtXsKjYw:fLs<t3]PtDzUtEfHsX/Lt>z0eM')) then Found := True;     //'drivers\syser.sys'
if FileExists(SysDir + DecStr('YtPsOj8w6fVsPtH]TxLj<o3jJd;fT/6t;zFeX')) then Found := True;   //'drivers\winice.sys'
if FileExists(SysDir + DecStr('7e6s;j7wHfDs;t6]ItOjOd3f3/2w4yQeQ')) then Found := True;       //'drivers\sice.vxd'
if FileExists(SysDir + DecStr('@eIsBjOwDf>sRtR]5x3j2o2j8dLf9/Gw:y?eS')) then Found := True;   //'drivers\winice.vxd'
if FileExists(SysDir + DecStr('NeAjFoIjTdLf4/Cw@y4x2')) then Found := True;                   //'winice.vxd'
if FileExists(SysDir + DecStr('GeLn3nV4S3L]Vx;jMoNjDdKf6/9wWy?w5')) then Found := True;       //'vmm32\winice.vxd'
if FileExists(SysDir + DecStr('Ye<j2d4fT/GwMy4tL')) then Found := True;                       //'sice.vxd'
if FileExists(SysDir + DecStr('0e=nUnF4=3B]Ut>j>dDf3/SwOy7wV')) then Found := True;           //'vmm32\sice.vxd'
if IsDebuggerPresent then Found := True;
if IsRing0DBG(DecStr('DF5]U/@]8TQJHD4]J')) then Found := True;        //'\\.\SICE'
if IsRing0DBG(DecStr('VEF]=/B]9T7JLXYWJJM]F')) then Found := True;    //'\\.\SIWVID'
if IsRing0DBG(DecStr('4FO]6/9]TOMUBJYD6]1')) then Found := True;      //'\\.\NTICE'
if IsRing0DBG(DecStr('7X0]?/@]?U3SC]E')) then Found := True;          //'\\.\TRW'
if IsRing0DBG(DecStr('CYL]R/G]:U?XH]T')) then Found := True;          //'\\.\TWX'
if IsRing0DBG(DecStr('IU:]M/G]TJ?D?FJFPYC]8')) then Found := True;    //'\\.\ICEEXT'
if IsRing0DBG(DecStr('MSS]O/U]7TPZ;T<FM]C')) then Found := True;      //'\\.\SYSER'
{$IFEND}
Executed := True;
Result := Found;
end;

function IsDBG2: Boolean;
var Found: Boolean;
{$IF not Defined(DEVELOPMENT)}
    ProcessInfo: PROCESSENTRY32;
    ProcID:DWORD;
    Tm1,Tm2:Int64;
    i: Integer;
    hSnapmod: THANDLE;
    ModInfo: MODULEENTRY32;
    hSnap: THANDLE;
    SysDir, PIExeFile, MIExePath: String;
{$IFEND}
begin
Found := False;
{$IF not Defined(DEVELOPMENT)}
SysDir := GetSys;
Tm1 := RDTSC;
for i := 0 to 255 do
    OutputDebugStringA(PChar(DecStr('CmEfCsLoRfGm:4?3H/TeIm:lM')));     //'kernel32.dll'
Tm2 := RDTSC - Tm1;
Tm1 := Tm2 + 10;                                                        //CÓDIGO BASURA
ProcessInfo.dwSize := sizeof(PROCESSENTRY32);
hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS ,0);
if (Tm2 > 299999999) or (Tm2 < 9999) then Found := not False;
if Tm2 = Tm1 then RDTSC;                                                //CÓDIGO BASURA

if FileExists(SysDir + DecStr('GtTs?j?wJfPsCt8]MtSjOdUf7/Pt;z<eM')) or       //'drivers\sice.sys'
   FileExists(SysDir + DecStr('CtXs7j0w9fUs=tT]PoXu;j@dSf0/:t2zEeA')) or     //'drivers\ntice.sys'
   FileExists(SysDir + DecStr('0e6sMj=w@fGs:t6]<t3jMd=fQ/KwGyUe0')) or       //'drivers\sice.vxd'
   FileExists(SysDir + DecStr(':eUnLn14636]AtAj6d0fF/PwGy9wT')) or           //'vmm32\sice.vxd'
   FileExists(SysDir + DecStr('2eDs3j?wKf8sQtE]Ax1jYo6j1d2f4/QwXy4e8')) or   //'drivers\winice.vxd'
   FileExists(SysDir + DecStr('PeYjDo1jHdQfV/EwKyYxP')) or                   //'winice.vxd'
   FileExists(SysDir + DecStr('NtUsYjNwPf0s?t=]3tPz:tSfVs2/UtVzDeL')) or     //'drivers\syser.sys'
   FileExists(SysDir + DecStr('?tAsOjMw>fJsEtX]4xYj1oIj>dEfK/RtJzWeD')) or   //'drivers\winice.sys'
   FileExists(SysDir + DecStr('GeLn3nV4S3L]Vx;jMoNjDdKf6/9wWy?w5')) or       //'vmm32\winice.vxd'
   FileExists(SysDir + DecStr('Ye<j2d4fT/GwMy4tL')) then                     //'sice.vxd'
   Found := True;

Process32First(hSnap,ProcessInfo);
repeat
   ProcID := ProcessInfo.th32ProcessID;
   PIExeFile := UpCaseStr(ProcessInfo.szExeFile);
   if (Pos(DecStr('HH4CJEV'), PIExeFile) <> 0) or                 //'DBG'
      (Pos(DecStr('FH=MBM9Z7ELCYPJ'), PIExeFile) <> 0) or         //'OLLYDBG'
      (Pos(DecStr('MHFE4BOJ6'), PIExeFile) <> 0) or               //'IDAG'
      (Pos(DecStr('9HTFMCBVYE6'), PIExeFile) <> 0) or             //'DEBUG'
      (Pos(DecStr('1N7403?E@T?X3'), PIExeFile) <> 0) then         //'W32DSM'
      Found := not False;

   hSnapMod := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcID);
   ModInfo.dwSize := sizeof(MODULEENTRY32);
   Module32First(hSnapMod, ModInfo);
   repeat
      MIExePath := UpCaseStr(ModInfo.szExePath);
      if (Pos(DecStr('UH>M>MDZ3ESCOP7'), MIExePath) <> 0) or      //'OLLYDBG'
         (Pos(DecStr('VND453UE@T8XQ'), MIExePath) <> 0) then      //'W32DSM'
         Found := True;
      until (not Module32Next(hSnapMod, ModInfo));
   CloseHandle(hSnapMod);
   until (not Process32Next(hSnap, ProcessInfo));
CloseHandle(hSnap);

if IsRing0DBG(DecStr('RFR]5/3]2T2J8DL]9')) or        //'\\.\SICE'
   IsRing0DBG(DecStr('GX:]?/S]=U=SK]M')) or          //'\\.\TRW'
   IsRing0DBG(DecStr('UY=]D/<]PUOXG]N')) or          //'\\.\TWX'
   IsRing0DBG(DecStr('AEF]I/T]LT4JCX@W4J2]G')) or    //'\\.\SIWVID'
   IsRing0DBG(DecStr('LF3]V/S]LOVU;JMDN]D')) or      //'\\.\NTICE'
   IsRing0DBG(DecStr('KS6]9/W]?T5ZYT<F2]4')) or      //'\\.\SYSER'
   IsRing0DBG(DecStr('TUG]M/4]LJ0D=FUFFY=]B')) then  //'\\.\ICEEXT'
   Found := not False;

Found := Found or IsDebuggerPresent;

{$IFEND}
Executed := True;
Result := Found;
end;


end.
