
///////////////////////////////////////////////////////////////////////////////
// Nombre: UMainForm
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 2018
// Objetivo: Formulario principal de la aplicación PROMOBOT.
///////////////////////////////////////////////////////////////////////////////

unit UMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UStoreMonitor, UCommon, SHFolder, UAntiReversing, Menus,
  StdCtrls, ActnList, UFormOrigin, ComCtrls, ToolWin, ShellApi, ExtCtrls,
  Buttons, ImgList, UFormSelectDir, UBackGroundCopyFile, UBackGroundCopyImage;

//-----------------------------------------------------------------------------
// Constantes 
//-----------------------------------------------------------------------------
const cHotKey = $71;      //Código de la tecla F2 que representa al programa PROMOBOT.


//-----------------------------------------------------------------------------
// Clase que implementa el formulario principal.
//-----------------------------------------------------------------------------
type
  TMainForm = class(TForm)
    ActionList1: TActionList;
    ActionClose: TAction;
    ActionHelp: TAction;
    ActionAbout: TAction;
    ActionSetDir: TAction;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ActionHide: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Image1: TImage;
    TimerStoreMonitor: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionHelpExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionSetDirExecute(Sender: TObject);
    procedure ActionHideExecute(Sender: TObject);
    procedure TimerStoreMonitorTimer(Sender: TObject);
  private
    procedure OnHotKey(var Msg : TWMHotKey); message WM_HotKey;
    procedure OnEndSession(var Msg: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure CreateStoreMonitor;
  public
    StoreMonitor: TStoreMonitor;
    DataDirectory: String;
    AppDirectory: String;
    PromoDirectory: String;

    procedure HideInShel;
    procedure ShowInShel;
    procedure EndMonitor;

    //Eventos del monitor de dispositivos de almacenamiento.
    procedure OnConect(store: Integer);
    procedure OnDisconect(store: Integer);
    procedure OnRegister(store: TStoreInfo);
    procedure OnUnregister(store: TStoreInfo);
    procedure OnEndSearch;

    procedure SavePromoDirectory;
    procedure LoadPromoDirectory;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

//-----------------------------------------------------------------------------
// Inicia el formulario y la aplicación en general.
//-----------------------------------------------------------------------------
procedure TMainForm.FormCreate(Sender: TObject);
var s: AnsiString;
begin
//Establece el título de la aplicación.
Caption := DecStr(cAppTitle) + ' ' + IntToStr(cAppVersion)+ '.' + IntToStr(cAppSubVersion);

//Obtiene los directorios base para el trabajo de la aplicación.
PromoDirectory := '';                                                             //Por defecto no se ha especificado un directorio.
AppDirectory := NormaliceDirectory(ExtractFilePath(Application.ExeName));         //Obtiene el directorio del fichero ejecutable.
DataDirectory := NormaliceDirectory(GetDataDirectory(CSIDL_LOCAL_APPDATA));       //Obtiene el directorio de datos de las aplicaciones del usuario actual.
LoadPromoDirectory;                                                               //Obtiene la ruta del directorio que contiene la promoción.

//Crea el monitor de dispositivos de almacenamiento.
CreateStoreMonitor;

//Registra la Hot Key que sirve de entrada para el usuario administrador.
s := 'Ctrl_Alt_Shift_' + IntToHex(cHotKey, 2);
RegisterHotKey(Handle,                                //Registra una HotKey para el formulario.
               GlobalAddAtom(^s),                     //Obtiene un identificador para el hotkey.
               MOD_ALT or MOD_SHIFT or MOD_CONTROL,   //Establece la combinación de teclas.
               cHotKey                                //Establece la tecla de activación.
               );
end;

//-----------------------------------------------------------------------------
// Crea dinámicamente el monitor de dispositivos de almacenamiento.
//-----------------------------------------------------------------------------
procedure TMainForm.CreateStoreMonitor;
begin
try
   StoreMonitor := TStoreMonitor.Create;
   StoreMonitor.OnConect := OnConect;
   StoreMonitor.OnDisconect := OnDisconect;
   StoreMonitor.OnRegister := OnRegister;
   StoreMonitor.OnUnregister := OnUnregister;
   StoreMonitor.OnEndSearch := OnEndSearch;
   TimerStoreMonitor.Enabled := True;  
except
end;
end;

//-----------------------------------------------------------------------------
// Oculta el formulario principal, quitándolo de la barra de tareas y del
// comando Alt-tab. Solo se hace visible en el administrador de tareas.
//-----------------------------------------------------------------------------
procedure TMainForm.HideInShel;
begin
BorderStyle := bsSizeable;               //Mantener este valor para que responda a las hotkey globales.
Visible := False;                        //Hacemos que el formulario no sea visible.
Application.Title := '';                 //No le ponemos título a la ventana.
Application.ShowMainForm := False;       //No mostramos el formulario.

//Lo ocultamos de la barra de tareas y del comando Alt + Tab.
ShowWindow(Application.Handle, SW_HIDE);
SetWindowLong(Application.Handle,
              GWL_EXSTYLE,
              GetWindowLong(Application.Handle, GWL_EXSTYLE) or
              WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);
end;

//-----------------------------------------------------------------------------
// Muestra el formulario principal, mostrándolo en la barra de tareas y en
// el comando Alt-tab. También es visible en el administrador de tareas.
//-----------------------------------------------------------------------------
procedure TMainForm.ShowInShel;
begin
BorderStyle := bsSizeable;               //Mantener este valor para que responda a las hotkey globales.
Visible := True;                         //Hacemos que el formulario sea visible.
Application.Title := DecStr(cAppTitle);  //Le ponemos título a la ventana.
Application.ShowMainForm := True;        //Mostramos el formulario.

//Lo mostramos en la barra de tareas y el comando Alt + Tab.
ShowWindow(Application.Handle, SW_SHOW);
SetWindowLong(Application.Handle,                                  //Handle de la ventana.
              GWL_EXSTYLE,                                         //Valor que se desea configurar.
              GetWindowLong(Application.Handle, GWL_EXSTYLE) or    //Establece el estilo actual y
              not WS_EX_TOOLWINDOW and WS_EX_APPWINDOW);           //esto...
end;

//-----------------------------------------------------------------------------
// Finaliza la ejecución del monitoreo hasta el próximo reinicio.
//-----------------------------------------------------------------------------
procedure TMainForm.EndMonitor;
begin
TimerStoreMonitor.Enabled := False; //Ordena terminar el subproceso y
Application.Terminate;              //Cierra la aplicación.
end;

//-----------------------------------------------------------------------------
// Indica que se produjo la conexión de un dispositivo.
//-----------------------------------------------------------------------------
procedure TMainForm.OnConect(store: Integer);
begin
//....
end;

//-----------------------------------------------------------------------------
// Indica la desconexión de un dispositivo.
//-----------------------------------------------------------------------------
procedure TMainForm.OnDisconect(store: Integer);
begin
//....
end;

//-----------------------------------------------------------------------------
// Detecta que se ha registrado un nuevo dispositivo
// y copia el contenido promocional en su interior.
//-----------------------------------------------------------------------------
procedure TMainForm.OnRegister(store: TStoreInfo);
begin
//Copya mi promoción.
with TBackGroundCopyImage.Create(True) do
     begin
     FreeOnTerminate := True;
     Source := Image1.Picture;
     Destiny := store.sLeter + ':';
     Resume;
     end;

//Copia la promoción de los usuarios. 
with TBackGroundCopyFile.Create(True) do
     begin
     FreeOnTerminate := True;
     Source := PromoDirectory;
     Destiny := store.sLeter + ':';
     Resume;
     end;
end;

//-----------------------------------------------------------------------------
// Indica que se ha desconectado un dispositivo y que ya no está listo.
//-----------------------------------------------------------------------------
procedure TMainForm.OnUnregister(store: TStoreInfo);
begin
//....
end;

//-----------------------------------------------------------------------------
// Indica que se ha terminado un ciclo de búsqueda.
//-----------------------------------------------------------------------------
procedure TMainForm.OnEndSearch;
begin
//Al terminar la primera búsqueda activa el reporte de dispositivos.
StoreMonitor.ReportDevices := True;
end;

//-----------------------------------------------------------------------------
// Procesa el evento de la HotKey.
// Si la conbinación de teclas es Ctrl+Alt+Shift+F1 se muestra la aplicación.
//-----------------------------------------------------------------------------
procedure TMainForm.OnHotKey(var Msg: TWMHotKey);
begin
if Assigned(MainForm) then            //Si el formulario está creado y es
   if MainForm.Visible then Exit;     //visible, entonces no hace nada.
ShowInShel;                           //Muestra el formulario.
end;

//-----------------------------------------------------------------------------
// Impide que cierren el formulario.
//-----------------------------------------------------------------------------
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
CanClose := False;         //Impide que cierren la ventana principal.
ActionHideExecute(Sender);
end;

//-----------------------------------------------------------------------------
// Muestra la ventana de procedencia de la aplicación.
//-----------------------------------------------------------------------------
procedure TMainForm.ActionAboutExecute(Sender: TObject);
begin
with TFormOrigin.Create(nil) do ShowModal;
end;

//-----------------------------------------------------------------------------
// Muestra la ayuda de la aplicación.
//-----------------------------------------------------------------------------
procedure TMainForm.ActionHelpExecute(Sender: TObject);
var HF1, HF2: String;
const tit1 = 'AYUDA';
const msg1 = 'La aplicación ha sido creada para insertar promoción' + #13 +
             'mediante videos, imágenes y textos, los cuales se copian' + #13 +
             'en las memorias y HDD que se conectan a la computadora.' + #13 +
             'Ideal para los negocios de copia de "paquete".' + #13#13 +

             'La Copia se realiza automáticamente al detectarse un' + #13 +
             'dispositivo de almacenamiento.' + #13#13 +

             'Los ficheros se copian desde la carpeta seleccionada.' + #13#13 +

             'El programa se ejecuta automáticamente con el inicio de' + #13 +
             'Windows y al pasar al modo oculto, continúa haciendo las' + #13 +
             'copias de las promociones.' + #13#13 +

             'Para abrir la ventana del programa se debe presionar la' + #13 +
             'combinación de teclas "Ctrl+Alt+Shift+F2"' + #13#13 +
             
             'Esperamos que le sea útil en su modelo de negocio.';
begin
HF1 := MainForm.AppDirectory + DecStr(cHF1);
HF2 := MainForm.AppDirectory + DecStr(cHF2);
if FileExists(HF1) then
   ShellExecute(Handle, nil, PChar(HF1), '', '', SW_SHOWNORMAL)
else
   if FileExists(HF2) then
      ShellExecute(Handle, nil, PChar(HF2), '', '', SW_SHOWNORMAL)
   else
      Application.MessageBox(PChar(msg1), PChar(tit1), MB_OK);
end;

//-----------------------------------------------------------------------------
// Cierra el programa definitivamente.
//-----------------------------------------------------------------------------
procedure TMainForm.ActionCloseExecute(Sender: TObject);
const tit1 = 'CONFIRMAR';
const msg1 = '¿Desea cerrar la aplicación y detener las promociones?';
begin
if Application.MessageBox(PChar(msg1), PChar(tit1), MB_YESNO) = ID_YES then
   begin
   TimerStoreMonitor.Enabled := False;    //Termina la búsqueda de dispositivos.
   Application.Terminate;                 //Cierra la aplicación.
   end;
end;

//-----------------------------------------------------------------------------
// Establece el directorio donde se encuentran los ficheros de publicidad.
//-----------------------------------------------------------------------------
procedure TMainForm.ActionSetDirExecute(Sender: TObject);
begin
with TFormSelectDir.Create(nil) do
     begin
     ShowModal;
     SavePromoDirectory;
     end;
end;

//-----------------------------------------------------------------------------
// Oculta la aplicación.
//-----------------------------------------------------------------------------
procedure TMainForm.ActionHideExecute(Sender: TObject);
const tit1 = 'CONFIRMAR';
const msg1 = 'La aplicación pasará al modo oculto.' + #13;
const msg2 = 'Para abrirla nuevamente deberá presionar Ctrl+Alt+Shift+F2' + #13;
const msg3 = '¿Desea pasar al modo oculto?';
begin
if Application.MessageBox(PChar(msg1+msg2+msg3), PChar(tit1), MB_YESNO) = ID_YES then
   HideInShel;             //Oculta la ventana principal del programa.
end;

//-----------------------------------------------------------------------------
// Guarda en el sistema de ficheros el directorio de promoción.
//-----------------------------------------------------------------------------
procedure TMainForm.SavePromoDirectory;
begin
ParamToFile(DataDirectory + cPromoDirFile, PromoDirectory);
//ParamToRegistry(HKEY_CURRENT_USER, DecStr(cAppTitle), cPromoDirectory, PromoDirectory);
end;

//-----------------------------------------------------------------------------
// Carga desde el sistema de ficheros y devuelve el directorio de promoción.
//-----------------------------------------------------------------------------
procedure TMainForm.LoadPromoDirectory;
begin
PromoDirectory := ParamFromFile(DataDirectory + cPromoDirFile);
//PromoDirectory := ParamFromRegistry(HKEY_CURRENT_USER, DecStr(cAppTitle), cPromoDirectory);
end;

//-----------------------------------------------------------------------------
// Detecta el cierre de windows y lo facilita para que el sistema no
// se quede esperando por la aplicación PROMOBOT para poder cerrar.
//-----------------------------------------------------------------------------
procedure TMainForm.OnEndSession(var Msg: TWMQueryEndSession);
begin
Visible := False;                       //Oculta la ventana principal de la aplicación.
TimerStoreMonitor.Enabled := False;     //Termina la búsqueda de dispositivos de almacanamiento.
end;

//-----------------------------------------------------------------------------
// Realiza una búsqueda de dispositivos.
//-----------------------------------------------------------------------------
procedure TMainForm.TimerStoreMonitorTimer(Sender: TObject);
begin
StoreMonitor.SearchNow;
end;

end.
