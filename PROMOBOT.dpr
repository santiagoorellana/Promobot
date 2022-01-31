
///////////////////////////////////////////////////////////////////////////////
// Nombre: PROMOBOT
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 25/02/2018
///////////////////////////////////////////////////////////////////////////////

program PROMOBOT;

uses
  Forms,
  Windows,
  UMainForm in 'UMainForm.pas' {MainForm},
  UStoreMonitor in 'UStoreMonitor.pas',
  UAntiReversing in 'UAntireversing.pas',
  UBase64 in 'UBase64.pas',
  UCommon in 'UCommon.pas',
  UCryptBase in 'UCryptBase.pas',
  URC4 in 'URC4.pas',
  USHA256 in 'USHA256.pas',
  UFormOrigin in 'UFormOrigin.pas' {FormOrigin},
  UFormSelectDir in 'UFormSelectDir.pas' {FormSelectDir},
  UBackGroundCopyFile in 'UBackGroundCopyFile.pas',
  UBackGroundCopyImage in 'UBackGroundCopyImage.pas';

{$R *.res}

var Semaphore: THandle;

begin
//Antes de iniciar se comprueba si ya existen instancias de este
//programa corriendo. Si ya está corriendo, termina esta instancia.
try
   Semaphore := CreateSemaphore(nil, 0, 1, PChar(CreateNameFor(cSemaphorePromobot)));
except
   Semaphore := 0;
end;   
if ((Semaphore <> 0) and (GetLastError = ERROR_ALREADY_EXISTS)) then
   begin
   CloseHandle(Semaphore);
   Halt;
   end;

//SI no hay instancias corriendo, continúa abriendo esta instancia.
  Application.Initialize;                         //Inicia la aplicación.
  Application.Title := 'PROMOBOT';                //Establece el título.
  Application.CreateForm(TMainForm, MainForm);
  //Abre el formulario principal.
  Application.Run;                                //Ejecuta la aplicación.
end.
