
///////////////////////////////////////////////////////////////////////////////
// Nombre: CryptBase
// Objetivo: Implementa las clases bases para los algoritmos criptogr�ficos.
// Autor: Santiago Alejandro Orellana P�rez
// Nota: El c�digo fue adaptado de la librer�a DCPcrypt v2.0 escrita
//       por David Barton (crypto@cityinthesky.co.uk).
///////////////////////////////////////////////////////////////////////////////

unit UCryptBase;

interface
uses
  Classes, Sysutils, UBase64, UAntiReversing;

//-----------------------------------------------------------------------------
// Redefinici�n de tipos de datos.
//-----------------------------------------------------------------------------
type
  Pbyte= ^byte;
  Pword= ^word;
  Pdword= ^dword;
  Pint64= ^int64;
  dword= longword;
  Pwordarray= ^Twordarray;
  Twordarray= array[0..19383] of word;
  Pdwordarray= ^Tdwordarray;
  Tdwordarray= array[0..8191] of dword;

//-----------------------------------------------------------------------------
// Esta es la clase base de la que derivan todos los algoritmos de HASH.
//-----------------------------------------------------------------------------
type
  EDCP_hash= class(Exception);
  TDCP_hash= class(TComponent)
  protected
    fInitialized: boolean;
    procedure DeadInt(Value: integer);
    procedure DeadStr(Value: string);
  private
    function _GetId: integer;
    function _GetAlgorithm: string;
    function _GetHashSize: integer;
  public
    property Initialized: boolean
      read fInitialized;
    class function GetId: integer; virtual;
    class function GetAlgorithm: string; virtual;
    class function GetHashSize: integer; virtual;
    class function SelfTest: boolean; virtual;
    procedure Init; virtual;
    procedure Final(var Digest); virtual;
    procedure Burn; virtual;
    procedure Update(const Buffer; Size: longword); virtual;
    procedure UpdateStream(Stream: TStream; Size: longword);
    procedure UpdateStr(const Str: AnsiString); {$IFDEF UNICODE}overload; {$ENDIF}
{$IFDEF UNICODE}
    procedure UpdateStr(const Str: UnicodeString); overload;
{$ENDIF}
    destructor Destroy; override;
  published
    property Id: integer read _GetId write DeadInt;
    property Algorithm: string read _GetAlgorithm write DeadStr;
    property HashSize: integer read _GetHashSize write DeadInt;
  end;
  TDCP_hashclass= class of TDCP_hash;


//-----------------------------------------------------------------------------
// Esta es la clase base de la que se derivan todas las clases criptogr�ficas.
//-----------------------------------------------------------------------------
type
  EDCP_cipher= class(Exception);
  TDCP_cipher= class(TComponent)
  protected
    fInitialized: boolean;
    procedure DeadInt(Value: integer);
    procedure DeadStr(Value: string);
  private
    function _GetId: integer;
    function _GetAlgorithm: string;
    function _GetMaxKeySize: integer;
  public
    property Initialized: boolean read fInitialized;
    class function GetId: integer; virtual;
    class function GetAlgorithm: string; virtual;
    class function GetMaxKeySize: integer; virtual;
    class function SelfTest: boolean; virtual;
    procedure Init(const Key; Size: longword; InitVector: pointer); virtual;
    procedure InitStr(const Key: AnsiString; HashType: TDCP_hashclass); {$IFDEF UNICODE}overload; {$ENDIF}
{$IFDEF UNICODE}
    procedure InitStr(const Key: UnicodeString; HashType: TDCP_hashclass); overload;
{$ENDIF}
    procedure Burn; virtual;
    procedure Reset; virtual;
    procedure Encrypt(const Indata; var Outdata; Size: longword); virtual;
    procedure Decrypt(const Indata; var Outdata; Size: longword); virtual;
    function EncryptStream(InStream, OutStream: TStream; Size: longword): longword;
    function DecryptStream(InStream, OutStream: TStream; Size: longword): longword;
    function EncryptString(const Str: AnsiString): AnsiString; {$IFDEF UNICODE}overload; {$ENDIF}virtual;
    function DecryptString(const Str: AnsiString): AnsiString; {$IFDEF UNICODE}overload; {$ENDIF}virtual;
{$IFDEF UNICODE}
    function EncryptString(const Str: UnicodeString): UnicodeString; overload; virtual;
    function DecryptString(const Str: UnicodeString): UnicodeString; overload; virtual;
{$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Id: integer read _GetId write DeadInt;
    property Algorithm: string read _GetAlgorithm write DeadStr;
    property MaxKeySize: integer read _GetMaxKeySize write DeadInt;
  end;
  TDCP_cipherclass= class of TDCP_cipher;

//-----------------------------------------------------------------------------
// De esta clase se derivan los cifradores de bloque como RC4.
//-----------------------------------------------------------------------------
type
  TDCP_ciphermode= (cmCBC, cmCFB8bit, cmCFBblock, cmOFB, cmCTR);
  EDCP_blockcipher= class(EDCP_cipher);
  TDCP_blockcipher= class(TDCP_cipher)
  protected
    fCipherMode: TDCP_ciphermode;
    procedure InitKey(const Key; Size: longword); virtual;
  private
    function _GetBlockSize: integer;
  public
    class function GetBlockSize: integer; virtual;
    procedure SetIV(const Value); virtual;
    procedure GetIV(var Value); virtual;
    procedure Encrypt(const Indata; var Outdata; Size: longword); override;
    procedure Decrypt(const Indata; var Outdata; Size: longword); override;
    function EncryptString(const Str: AnsiString): AnsiString; overload; override;
    function DecryptString(const Str: AnsiString): AnsiString; overload; override;
{$IFDEF UNICODE}
    function EncryptString(const Str: UnicodeString): UnicodeString; overload; override;
    function DecryptString(const Str: UnicodeString): UnicodeString; overload; override;
{$ENDIF}
    procedure EncryptECB(const Indata; var Outdata); virtual;
    procedure DecryptECB(const Indata; var Outdata); virtual;
    procedure EncryptCBC(const Indata; var Outdata; Size: longword); virtual;
    procedure DecryptCBC(const Indata; var Outdata; Size: longword); virtual;
    procedure EncryptCFB8bit(const Indata; var Outdata; Size: longword); virtual;
    procedure DecryptCFB8bit(const Indata; var Outdata; Size: longword); virtual;
    procedure EncryptCFBblock(const Indata; var Outdata; Size: longword); virtual;
    procedure DecryptCFBblock(const Indata; var Outdata; Size: longword); virtual;
    procedure EncryptOFB(const Indata; var Outdata; Size: longword); virtual;
    procedure DecryptOFB(const Indata; var Outdata; Size: longword); virtual;
    procedure EncryptCTR(const Indata; var Outdata; Size: longword); virtual;
    procedure DecryptCTR(const Indata; var Outdata; Size: longword); virtual;
    constructor Create(AOwner: TComponent); override;
  published
    property BlockSize: integer read _GetBlockSize write DeadInt;
    property CipherMode: TDCP_ciphermode read fCipherMode write fCipherMode default cmCBC;
  end;
  TDCP_blockcipherclass= class of TDCP_blockcipher;

//-----------------------------------------------------------------------------
// Otras funciones �tiles.
//-----------------------------------------------------------------------------
procedure XorBlock(var InData1, InData2; Size: longword);


implementation
{$Q-}{$R-}

//-----------------------------------------------------------------------------
// TDCP_hash 
//-----------------------------------------------------------------------------

procedure TDCP_hash.DeadInt(Value: integer);
begin
end;

procedure TDCP_hash.DeadStr(Value: string);
begin
end;

function TDCP_hash._GetId: integer;
begin
Result:= GetId;
end;

function TDCP_hash._GetAlgorithm: string;
begin
Result:= GetAlgorithm;
end;

function TDCP_hash._GetHashSize: integer;
begin
Result:= GetHashSize;
end;

class function TDCP_hash.GetId: integer;
begin
Result:= -1;
end;

class function TDCP_hash.GetAlgorithm: string;
begin
Result:= '';
end;

class function TDCP_hash.GetHashSize: integer;
begin
Result:= -1;
end;

class function TDCP_hash.SelfTest: boolean;
begin
Result:= false;
end;

procedure TDCP_hash.Init;
begin
end;

procedure TDCP_hash.Final(var Digest);
begin
end;

procedure TDCP_hash.Burn;
begin
end;

procedure TDCP_hash.Update(const Buffer; Size: longword);
begin
end;

procedure TDCP_hash.UpdateStream(Stream: TStream; Size: longword);
var Buffer: array[0..8191] of byte;
    i, read: integer;
begin
for i:= 1 to (Size div Sizeof(Buffer)) do
    begin
    read:= Stream.Read(Buffer,Sizeof(Buffer));
    Update(Buffer,read);
    end;
if (Size mod Sizeof(Buffer))<> 0 then
   begin
   read:= Stream.Read(Buffer,Size mod Sizeof(Buffer));
   Update(Buffer,read);
   end;
end;

procedure TDCP_hash.UpdateStr(const Str: AnsiString);
begin
Update(Str[1],Length(Str));
end;

{$IFDEF UNICODE}
procedure TDCP_hash.UpdateStr(const Str: UnicodeString);
begin
Update(Str[1],Length(Str)*Sizeof(Str[1]));
end; { DecryptString }
{$ENDIF}

destructor TDCP_hash.Destroy;
begin
if fInitialized then Burn;
inherited Destroy;
end;


//-----------------------------------------------------------------------------
// TDCP_cipher 
//-----------------------------------------------------------------------------

procedure TDCP_cipher.DeadInt(Value: integer);
begin
end;

procedure TDCP_cipher.DeadStr(Value: string);
begin
end;

function TDCP_cipher._GetId: integer;
begin
Result:= GetId;
end;

function TDCP_cipher._GetAlgorithm: string;
begin
Result:= GetAlgorithm;
end;

function TDCP_cipher._GetMaxKeySize: integer;
begin
Result:= GetMaxKeySize;
end;

class function TDCP_cipher.GetId: integer;
begin
Result:= -1;
end;

class function TDCP_cipher.GetAlgorithm: string;
begin
Result:= '';
end;

class function TDCP_cipher.GetMaxKeySize: integer;
begin
Result:= -1;
end;

class function TDCP_cipher.SelfTest: boolean;
begin
Result:= false;
end;

procedure TDCP_cipher.Init(const Key; Size: longword; InitVector: pointer);
begin
if fInitialized then Burn;
if (Size <= 0) or ((Size and 3)<> 0) or (Size> longword(GetMaxKeySize)) then
   raise EDCP_cipher.Create('Tama�o de llave no v�lido.')
else
   fInitialized:= true;
end;

procedure TDCP_cipher.InitStr(const Key: AnsiString; HashType: TDCP_hashclass);
var Hash: TDCP_hash;
    Digest: pointer;
begin
if fInitialized then Burn;
try
   GetMem(Digest,HashType.GetHashSize div 8);
   Hash:= HashType.Create(Self);
   Hash.Init;
   Hash.UpdateStr(Key);
   Hash.Final(Digest^);
   Hash.Free;
   if MaxKeySize< HashType.GetHashSize then
      Init(Digest^,MaxKeySize,nil)
   else
      Init(Digest^,HashType.GetHashSize,nil);
   FillChar(Digest^,HashType.GetHashSize div 8,$FF);
   FreeMem(Digest);
except
   raise EDCP_cipher.Create('No se puede alojar suficiente memoria.');
end;
end;

{$IFDEF UNICODE}
procedure TDCP_cipher.InitStr(const Key: UnicodeString; HashType: TDCP_hashclass);
var Hash: TDCP_hash;
    Digest: pointer;
begin
if fInitialized then Burn;
try
   GetMem(Digest,HashType.GetHashSize div 8);
   Hash:= HashType.Create(Self);
   Hash.Init;
   Hash.UpdateStr(Key);
   Hash.Final(Digest^);
   Hash.Free;
   if MaxKeySize< HashType.GetHashSize then
      Init(Digest^,MaxKeySize,nil)
   else
      Init(Digest^,HashType.GetHashSize,nil);
   FillChar(Digest^,HashType.GetHashSize div 8,$FF);
   FreeMem(Digest);
except
   raise EDCP_cipher.Create(DecStr('D/4n?q<p;t2j<cSm:f?!Hb2m;p@k3bPsA!1tMvEgBjNd0j4f:oFuQfJ!;nSfDnCp1sLjLbA!9qHb>s3bS!HfSm1!LiLbSt?iR!QeVj3h;fEtOuPJB'));    //'Imposible alojar suficiente memoria para el hash digest.'
end;
end;
{$ENDIF}

procedure TDCP_cipher.Burn;
begin
fInitialized:= false;
end;

procedure TDCP_cipher.Reset;
begin
end;

procedure TDCP_cipher.Encrypt(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_cipher.Decrypt(const Indata; var Outdata; Size: longword);
begin
end;

function TDCP_cipher.EncryptStream(InStream, OutStream: TStream; Size: longword): longword;
var Buffer: array[0..8191] of byte;
    i, Read: longword;
begin
Result:= 0;
for i:= 1 to (Size div Sizeof(Buffer)) do
    begin
    Read:= InStream.Read(Buffer,Sizeof(Buffer));
    Inc(Result,Read);
    Encrypt(Buffer,Buffer,Read);
    OutStream.Write(Buffer,Read);
    end;
if (Size mod Sizeof(Buffer))<> 0 then
   begin
   Read:= InStream.Read(Buffer,Size mod Sizeof(Buffer));
   Inc(Result,Read);
   Encrypt(Buffer,Buffer,Read);
   OutStream.Write(Buffer,Read);
   end;
end;

function TDCP_cipher.DecryptStream(InStream, OutStream: TStream; Size: longword): longword;
var Buffer: array[0..8191] of byte;
    i, Read: longword;
begin
Result:= 0;
for i:= 1 to (Size div Sizeof(Buffer)) do
    begin
    Read:= InStream.Read(Buffer,Sizeof(Buffer));
    Inc(Result,Read);
    Decrypt(Buffer,Buffer,Read);
    OutStream.Write(Buffer,Read);
    end;
if (Size mod Sizeof(Buffer))<> 0 then
   begin
   Read:= InStream.Read(Buffer,Size mod Sizeof(Buffer));
   Inc(Result,Read);
   Decrypt(Buffer,Buffer,Read);
   OutStream.Write(Buffer,Read);
   end;
end;

function TDCP_cipher.EncryptString(const Str: AnsiString): AnsiString;
begin
SetLength(Result,Length(Str));
Encrypt(Str[1],Result[1],Length(Str));
Result:= Base64EncodeStr(Result);
end;

function TDCP_cipher.DecryptString(const Str: AnsiString): AnsiString;
begin
Result:= Base64DecodeStr(Str);
Decrypt(Result[1],Result[1],Length(Result));
end;

{$IFDEF UNICODE}
function TDCP_cipher.EncryptString(const Str: UnicodeString): UnicodeString;
begin
SetLength(Result,Length(Str));
Encrypt(Str[1],Result[1],Length(Str)*SizeOf(Str[1]));
Result:= Base64EncodeStr(Result);
end;

function TDCP_cipher.DecryptString(const Str: UnicodeString): UnicodeString;
begin
Result:= Base64DecodeStr(Str);
Decrypt(Result[1],Result[1],Length(Result)*SizeOf(Result[1]));
end;
{$ENDIF}

constructor TDCP_cipher.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
Burn;
end;

destructor TDCP_cipher.Destroy;
begin
if fInitialized then Burn;
inherited Destroy;
end;


//-----------------------------------------------------------------------------
// TDCP_blockcipher
//-----------------------------------------------------------------------------

procedure TDCP_blockcipher.InitKey(const Key; Size: longword);
begin
end;

function TDCP_blockcipher._GetBlockSize: integer;
begin
Result:= GetBlockSize;
end;

class function TDCP_blockcipher.GetBlockSize: integer;
begin
Result:= -1;
end;

procedure TDCP_blockcipher.SetIV(const Value);
begin
end;

procedure TDCP_blockcipher.GetIV(var Value);
begin
end;

procedure TDCP_blockcipher.Encrypt(const Indata; var Outdata; Size: longword);
begin
case fCipherMode of
     cmCBC: EncryptCBC(Indata,Outdata,Size);
     cmCFB8bit: EncryptCFB8bit(Indata,Outdata,Size);
     cmCFBblock: EncryptCFBblock(Indata,Outdata,Size);
     cmOFB: EncryptOFB(Indata,Outdata,Size);
     cmCTR: EncryptCTR(Indata,Outdata,Size);
     end;
end;

function TDCP_blockcipher.EncryptString(const Str: AnsiString): AnsiString;
begin
SetLength(Result,Length(Str));
EncryptCFB8bit(Str[1],Result[1],Length(Str));
Result:= Base64EncodeStr(Result);
end;

function TDCP_blockcipher.DecryptString(const Str: AnsiString): AnsiString;
begin
Result:= Base64DecodeStr(Str);
DecryptCFB8bit(Result[1],Result[1],Length(Result));
end;

{$IFDEF UNICODE}
function TDCP_blockcipher.EncryptString(const Str: UnicodeString): UnicodeString;
begin
SetLength(Result,Length(Str));
EncryptCFB8bit(Str[1],Result[1],Length(Str)*SizeOf(Str[1]));
Result:= Base64EncodeStr(Result);
end;

function TDCP_blockcipher.DecryptString(const Str: UnicodeString): UnicodeString;
begin
Result:= Base64DecodeStr(Str);
DecryptCFB8bit(Result[1],Result[1],Length(Result)*SizeOf(Result[1]));
end;
{$ENDIF}

procedure TDCP_blockcipher.Decrypt(const Indata; var Outdata; Size: longword);
begin
case fCipherMode of
     cmCBC: DecryptCBC(Indata,Outdata,Size);
     cmCFB8bit: DecryptCFB8bit(Indata,Outdata,Size);
     cmCFBblock: DecryptCFBblock(Indata,Outdata,Size);
     cmOFB: DecryptOFB(Indata,Outdata,Size);
     cmCTR: DecryptCTR(Indata,Outdata,Size);
     end;
end;

procedure TDCP_blockcipher.EncryptECB(const Indata; var Outdata);
begin
end;

procedure TDCP_blockcipher.DecryptECB(const Indata; var Outdata);
begin
end;

procedure TDCP_blockcipher.EncryptCBC(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.DecryptCBC(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.EncryptCFB8bit(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.DecryptCFB8bit(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.EncryptCFBblock(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.DecryptCFBblock(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.EncryptOFB(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.DecryptOFB(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.EncryptCTR(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.DecryptCTR(const Indata; var Outdata; Size: longword);
begin
end;

constructor TDCP_blockcipher.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
fCipherMode:= cmCBC;
end;


//-----------------------------------------------------------------------------
// Otras funciones �tiles.
//-----------------------------------------------------------------------------
procedure XorBlock(var InData1, InData2; Size: longword);
var i: longword;
begin
for i:= 1 to Size do
    Pbyte(longword(@InData1)+i-1)^:= Pbyte(longword(@InData1)+i-1)^ xor Pbyte(longword(@InData2)+i-1)^;
end;

end.


