
///////////////////////////////////////////////////////////////////////////////
// Nombre: SHA256
// Creado: 28/7/2016
// Objetivo: Implementa el algoritmo de hash SHA256.
// Autor: Santiago Alejandro Orellana Pérez
// Nota: El código fue adaptado de la librería DCPcrypt v2.0 escrita
//       por David Barton (crypto@cityinthesky.co.uk).
///////////////////////////////////////////////////////////////////////////////

unit USHA256;

interface
uses
  Classes, Sysutils, UCryptBase, UAntiReversing;    //UCryptBase

type
  TSha256= class(TDCP_hash)
  protected
    LenHi, LenLo: longword;
    Index: DWord;
    CurrentHash: array[0..7] of DWord;
    HashBuffer: array[0..63] of byte;
    K: array[0..63] of DWORD;
    procedure Compress;
  public
    class function GetAlgorithm: string; override;
    class function GetHashSize: integer; override;
    procedure Init; override;
    procedure Final(var Digest); override;
    procedure Burn; override;
    procedure Update(const Buffer; Size: longword); override;
  end;

implementation
{$R-}{$Q-}

//-----------------------------------------------------------------------------
class function TSha256.GetAlgorithm: string;
begin
Result:= 'SHA256';
end;

//-----------------------------------------------------------------------------
class function TSha256.GetHashSize: integer;
begin
Result:= 256;
end;

//-----------------------------------------------------------------------------
function SwapDWord(a: dword): dword;
begin
Result:= ((a and $FF) shl 24) or ((a and $FF00) shl 8) or ((a and $FF0000) shr 8) or ((a and $FF000000) shr 24);
end;

//-----------------------------------------------------------------------------
procedure TSha256.Compress;
var a, b, c, d, e, f, g, h, t1, t2: DWord;
    W: array[0..63] of DWord;
    i: longword;
begin
Index:= 0;
a := CurrentHash[0]; b:= CurrentHash[1]; c:= CurrentHash[2]; d:= CurrentHash[3];
e := CurrentHash[4]; f:= CurrentHash[5]; g:= CurrentHash[6]; h:= CurrentHash[7];
Move(HashBuffer,W,Sizeof(HashBuffer));
for i := 0 to 15 do W[i]:= SwapDWord(W[i]);
for i := 16 to 63 do
    W[i]:= (((W[i-2] shr 17) or (W[i-2] shl 15)) xor ((W[i-2] shr 19) or (W[i-2] shl 13)) xor
    (W[i-2] shr 10)) + W[i-7] + (((W[i-15] shr 7) or (W[i-15] shl 25)) xor
    ((W[i-15] shr 18) or (W[i-15] shl 14)) xor (W[i-15] shr 3)) + W[i-16];

for i := 0 to 63 do
    begin
    t1 := h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) +
          ((e and f) xor (not e and g)) + K[i] + W[i];
    t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) +
          ((a and b) xor (a and c) xor (b and c));
    h := g; g:= f; f:= e; e:= d + t1; d:= c; c:= b; b:= a; a:= t1 + t2;
    end;

CurrentHash[0]:= CurrentHash[0] + a;
CurrentHash[1]:= CurrentHash[1] + b;
CurrentHash[2]:= CurrentHash[2] + c;
CurrentHash[3]:= CurrentHash[3] + d;
CurrentHash[4]:= CurrentHash[4] + e;
CurrentHash[5]:= CurrentHash[5] + f;
CurrentHash[6]:= CurrentHash[6] + g;
CurrentHash[7]:= CurrentHash[7] + h;
FillChar(W, Sizeof(W), 0);
FillChar(HashBuffer, Sizeof(HashBuffer), 0);
end;

//-----------------------------------------------------------------------------
procedure TSha256.Init;
begin
Burn;
CurrentHash[0]:= $6a09e667;
CurrentHash[1]:= $bb67ae85;
CurrentHash[2]:= $3c6ef372;
CurrentHash[3]:= $a54ff53a;
CurrentHash[4]:= $510e527f;
CurrentHash[5]:= $9b05688c;
CurrentHash[6]:= $1f83d9ab;
CurrentHash[7]:= $5be0cd19;

K[00] := $428a2f98;
K[01] := $71374491;
K[02] := $b5c0fbcf;
K[03] := $e9b5dba5;
K[04] := $3956c25b;
K[05] := $59f111f1;
K[06] := $923f82a4;
K[07] := $ab1c5ed5;
K[08] := $d807aa98;
K[09] := $12835b01;
K[10] := $243185be;
K[11] := $550c7dc3;
K[12] := $72be5d74;
K[13] := $80deb1fe;
K[14] := $9bdc06a7;
K[15] := $c19bf174;
K[16] := $e49b69c1;
K[17] := $efbe4786;
K[18] := $0fc19dc6;
K[19] := $240ca1cc;
K[20] := $2de92c6f;
K[21] := $4a7484aa;
K[22] := $5cb0a9dc;
K[23] := $76f988da;
K[24] := $983e5152;
K[25] := $a831c66d;
K[26] := $b00327c8;
K[27] := $bf597fc7;
K[28] := $c6e00bf3;
K[29] := $d5a79147;
K[30] := $06ca6351;
K[31] := $14292967;
K[32] := $27b70a85;
K[33] := $2e1b2138;
K[34] := $4d2c6dfc;
K[35] := $53380d13;
K[36] := $650a7354;
K[37] := $766a0abb;
K[38] := $81c2c92e;
K[39] := $92722c85;
K[40] := $a2bfe8a1;
K[41] := $a81a664b;
K[42] := $c24b8b70;
K[43] := $c76c51a3;
K[44] := $d192e819;
K[45] := $d6990624;
K[46] := $f40e3585;
K[47] := $106aa070;
K[48] := $19a4c116;
K[49] := $1e376c08;
K[50] := $2748774c;
K[51] := $34b0bcb5;
K[52] := $391c0cb3;
K[53] := $4ed8aa4a;
K[54] := $5b9cca4f;
K[55] := $682e6ff3;
K[56] := $748f82ee;
K[57] := $78a5636f;
K[58] := $84c87814;
K[59] := $8cc70208;
K[60] := $90befffa;
K[61] := $a4506ceb;
K[62] := $bef9a3f7;
K[63] := $c67178f2;

fInitialized := true;
end;

//-----------------------------------------------------------------------------
procedure TSha256.Burn;
begin
LenHi := 0; LenLo:= 0;
Index := 0;
FillChar(HashBuffer, Sizeof(HashBuffer), 0);
FillChar(CurrentHash, Sizeof(CurrentHash), 0);
FillChar(K, Sizeof(K), 0);
fInitialized := false;
end;

//-----------------------------------------------------------------------------
procedure TSha256.Update(const Buffer; Size: longword);
var PBuf: ^byte;
begin
if not fInitialized then
   raise EDCP_hash.Create('No se ha inicializado el HASH.');

Inc(LenHi, Size shr 29);
Inc(LenLo, Size * 8);
if LenLo < (Size * 8) then Inc(LenHi);

PBuf := @Buffer;
while Size > 0 do
      begin
      if (Sizeof(HashBuffer) - Index) <= DWord(Size) then
         begin
         Move(PBuf^, HashBuffer[Index], Sizeof(HashBuffer) - Index);
         Dec(Size, Sizeof(HashBuffer) - Index);
         Inc(PBuf, Sizeof(HashBuffer) - Index);
         Compress;
         end
      else
         begin
         Move(PBuf^, HashBuffer[Index], Size);
         Inc(Index, Size);
         Size := 0;
         end;
      end;
end;

//-----------------------------------------------------------------------------
procedure TSha256.Final(var Digest);
begin
if not fInitialized then
   raise EDCP_hash.Create('No se ha inicializado el HASH.');
HashBuffer[Index]:= $80;
if Index>= 56 then Compress;
PDWord(@HashBuffer[56])^:= SwapDWord(LenHi);
PDWord(@HashBuffer[60])^:= SwapDWord(LenLo);
Compress;
CurrentHash[0]:= SwapDWord(CurrentHash[0]);
CurrentHash[1]:= SwapDWord(CurrentHash[1]);
CurrentHash[2]:= SwapDWord(CurrentHash[2]);
CurrentHash[3]:= SwapDWord(CurrentHash[3]);
CurrentHash[4]:= SwapDWord(CurrentHash[4]);
CurrentHash[5]:= SwapDWord(CurrentHash[5]);
CurrentHash[6]:= SwapDWord(CurrentHash[6]);
CurrentHash[7]:= SwapDWord(CurrentHash[7]);
Move(CurrentHash,Digest,Sizeof(CurrentHash));
Burn;
end;


end.

