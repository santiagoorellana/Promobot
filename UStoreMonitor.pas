
///////////////////////////////////////////////////////////////////////////////
// Nombre: StoreMonitor
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 2016-2018
// Objetivo: Monitorear la conexión y desconexión de dispositivos
//           de almacenamiento en la computadora (HDD, USB, CD-ROM, DVD, etc).
//-----------------------------------------------------------------------------
// Se ha probado con los siguientes dispositivos:
// 1 - Memorias FLASH.            (OK)
// 2 - Discos duros internos.     (OK)
// 3 - Lectores de CD y DVD.      (OK)
// 4 - Quemadores de CD.          (OK)
// 5 - Discos duros externos      (OK)
// 6 - Quemadores externos        (NO PROBADO)
// 7 - Unidades de red            (NO PROBADO)
// 8 - Dispositivos móviles       (OK)
//-----------------------------------------------------------------------------
// Modo de uso:
//
// var SM: TStoreMonitor;             //Crea una variable para el objeto monitor.
// ...
// SM := TStoreMonitor.Create;        //Crea una instancia del objeto monitor.
// SM.Interval := 200;                //Intervalo de búsquedas en milisegundos.
// SM.OnConect := OnConect;           //Asigna el evento que informa Inserción.
// SM.OnDisconect := OnDisconect;     /Asigna el evento que informa Desconexión
// SM.OnRegister := OnRegister;       //Asigna el evento que informa Inserción.
// SM.OnUnregister := OnUnregister;   //Asigna el evento que informa Desconexión
// SM.OnEndSearch := OnEndSearch;     //Asigna el evento que informa Desconexión
// SM.OnError := OnError;             //Asigna el evento de que informa ERROR.
// SM.Resume;                         //Inicia el monitoreo.
// SM.Active := True;                 //Activa la detección de dispositivos.
// ...
// SM.Terminate;                      //Termina el monitoreo de dispositivos.
///////////////////////////////////////////////////////////////////////////////

unit UStoreMonitor;

interface

uses Windows, SysUtils, Classes, UCommon, UAntiReversing;

//-----------------------------------------------------------------------------
// Guarda la información que se recupera de los dispositivos de almacenamiento.
//-----------------------------------------------------------------------------
Type
  TStoreInfo = record
    sRegister: Boolean;           //Indica si el dispositivo está registrado.
    sIndex: Byte;                 //Índice asignado por el sistema operativo.
    sLeter: Char;                 //Letra asignada por el sistema operativo.
    sName: String;                //Nombre del dispositivo.
    sType: Byte;                  //Tipo de dispositivo.
    sSystem: Boolean;             //Indica si es la unidad del sistema.
    sSerial: DWORD;               //Número de serie del dispositivo.
    sDateTimeConect: TDateTime;   //Fecha y hora de la conexión.
    sTimeConectionMin: Integer;   //Duración de la conexión en minutos.
    sCapacity: Int64;             //Capacidad del dispositivo en GB.
    sInfoInitial: Int64;          //Cantidad de información inicial contenida en el dispositivo.
    sInfoMinimal: Int64;          //Cantidad de información mínima que hubo en el dispositivo.
    sInfoFinal: Int64;            //Cantidad de información que finalmente obtiene el dispositivo.
  end;

//-----------------------------------------------------------------------------
// Lista de dispositivos de almacenamientos.
//-----------------------------------------------------------------------------

const cLastDevice = 31;                                 //Índice del último en la lista.
type TStores = array [0..cLastDevice] of TStoreInfo;    //Lista de dispositivos.

//-----------------------------------------------------------------------------
// Tipos de eventos que genera este objeto.
//-----------------------------------------------------------------------------
type TSMNotifyEventD = procedure(StoreData: TStoreInfo) of object;     //Eventos que reciben una estructura como parámetro.
type TSMNotifyEventI = procedure(StoreIndex: Integer) of object;       //Eventos que reciben un entero como parámetro.
type TSMNotifyEventE = procedure(pUnit: String; Format: Integer) of object;      
type TSMNotifySearch = procedure of object;                //NOtifica la finalización de un ciclo de búsqueda.

//-----------------------------------------------------------------------------
// Detecta los dispositivos de almacenamiento conectados a la PC.
//-----------------------------------------------------------------------------
type
  TStoreMonitor = class 
  private
    FStoresMask: DWORD;                               //Máscara de bists que representa dispositivos conectados.
    FStores: TStores;                                 //Lista con los datos de los dispositivos.

    FOnConect: TSMNotifyEventI;                       //Evento que se llama cuando se detecta la conexión de un dispositivo.
    FOnDisconect: TSMNotifyEventI;                    //Evento que se llama cuando el dispositivo se desconecta.
    FOnRegister: TSMNotifyEventD;                     //Evento que se llama el registrar un dispositivo.
    FOnUnregister: TSMNotifyEventD;                   //Evento lamado cuando se borra el registro de un dispositivo.
    FOnEndSearch: TSMNotifySearch;

    FIntervalMS: Integer;                             //Intervalo en milisegundos entre las búsqueda.
    FReportDevices: Boolean;                          //Debe ser TRUE para que se reporten los dispositivos.

    function IsStoreReady(index: Byte): Boolean;          //Devuelve TRUE si el dispositivo está listo para ser registrado.
    function IsStoreRegister(index: Byte): Boolean;       //Devuelve TRUE si el dispositivo está registrado.
    function RegisterStore(index: Byte): Boolean;         //Registra los datos del dispositivo especificado, si existe.
    procedure UpgradeStoreRegister(index: Byte);          //Actualiza datos del dispositivo como la cantidad de información contenida.
    function UnregisterStore(index: Byte): Boolean;       //Borra los datos del dispositivo indicado.
    function GetStorePath(index: Byte): String;           //Obtiene una cadena con la ruta del dispositivo indicado.
    procedure OneSearch;                                  //Efectua una búsqueda de dispositivos de almacanamiento.

    procedure OnEventConect(index: Byte);             //Lanza un evento OnConect.
    procedure OnEventDisconect(index: Byte);          //Lanza un evento OnDisconect.
    procedure OnEventRegister(Store: TStoreInfo);     //Lanza un evento OnRegister.
    procedure OnEventUnregister(Store: TStoreInfo);   //Lanza un evento OnUnregister.
    procedure OnEventEndSearch;                       //Lanza un evento OnEndSearch.
  protected
  public
    constructor Create;                                                             //Inicia el objeto monitor.
    destructor Destroy; override;                                                   //Prepara el objeto para eliminarlo.
    property ReportDevices: Boolean read FReportDevices write FReportDevices;
    procedure SearchNow;                                                            //Realiza una búsqueda de dispositivos.
    property OnConect: TSMNotifyEventI read FOnConect write FOnConect;              //Se dispara cuando se inserta un dispositivo de almacenamiento.
    property OnDisconect: TSMNotifyEventI read FOnDisconect write FOnDisconect;     //Se dispara cuando se desconecta un dispositivo de almacenamiento.
    property OnRegister: TSMNotifyEventD read FOnRegister write FOnRegister;        //Se dispara cuando se inserta un dispositivo de almacenamiento.
    property OnUnregister: TSMNotifyEventD read FOnUnregister write FOnUnregister;  //Se dispara cuando se desconecta un dispositivo de almacenamiento.
    property OnEndSearch: TSMNotifySearch read FOnEndSearch write FOnEndSearch;     //Se dispara cuando se desconecta un dispositivo de almacenamiento.

    function GetStore(index: Byte; var Data: TStoreInfo): Boolean;                  //Devuelve los datos del dispositivo indicado.
    function TypeToString(pType: Byte): String;                                     //Devuelve una cadena con el tipo de dispositivo.
  end;


implementation

uses DateUtils;

//-----------------------------------------------------------------------------
// Inicia el detector de dispositivos.
//-----------------------------------------------------------------------------
constructor TStoreMonitor.Create;
begin
inherited Create;             //Llama al constructor de la clase paterna.
FStoresMask := 0;             //Inicialmente ningún dispositivo está conectado.
FIntervalMS := 1000;          //Se buscan cambios cada 1 segundo.

//Inicialmente los eventos no están asignados.
FOnConect := nil;
FOnDisconect := nil;
FOnRegister := nil;
FOnUnregister := nil;

//La detección de dispositivos está desactivada inicialmente.
FReportDevices := False;
end;

//-----------------------------------------------------------------------------
// Aquí se implementa la búsqueda continua de dispositivos de almacenamiento.
//-----------------------------------------------------------------------------
procedure TStoreMonitor.SearchNow; 
begin
OneSearch;              //Ejecuta una búsqueda de dispositivos de almacenamiento.
OnEventEndSearch;       //Notifica que se ha termina un ciclo de búsqueda.
end;

//-----------------------------------------------------------------------------
// Destruye todo lo que se ha utilizado en la instancia.
//-----------------------------------------------------------------------------
destructor TStoreMonitor.Destroy;
begin
FOnConect := nil;
FOnDisconect := nil;
FOnRegister := nil;
FOnUnregister := nil;
inherited Destroy
end;

//-----------------------------------------------------------------------------
// Esta función debe ser llamada cada vez que se desee buscar dispositivos.
//
// Se detectan todos los dispositivos de almacenamiento, pero se ignoran los
// lectores de disquete A y B, porque los controladores de disquete son muy
// lentos en dar respuesta y entorpecen el funcionamiento del programa.
// Además, ya esos dispositivos no se utilizan y la inclusión de estos
// en el chequeo no le dará mayor efectividad al programa.
//-----------------------------------------------------------------------------
procedure TStoreMonitor.OneSearch;
var n: Integer;
    NewStoresMask: DWORD;
    XorMask: DWORD;
    Store: TStoreInfo;
begin
try
   NewStoresMask := GetLogicalDrives;                  //Obtiene una máscara de bits indicando dispositivos instalados.
   NewStoresMask := NewStoresMask and $FFFFFFFC;       //Ignora los lectores de disquete. Unidades A y B.
   if NewStoresMask <> FStoresMask then                //Si han cambiado los dispositivos:
      begin
      XorMask := NewStoresMask xor FStoresMask;        //Obtiene los bits que han cambiado en la máscara.
      for n := 2 to cLastDevice do                     //Actualiza la lista de dispositivos.
          begin
          if XorMask and (1 shl n) > 0 then            //Si ha cambiado el estado del dispositivo n:
             begin
             if NewStoresMask and (1 shl n) > 0 then   //Se ha CONECTADO el dispositivo n...
                begin
                OnEventConect(n);                      //Reporta la conexión del dispositivo n.
                if IsStoreReady(n) then                //Si el dispositivo está listo para ser encuestado...
                   if RegisterStore(n) then            //Si obtiene los datos del dispositivo:
                      OnEventRegister(FStores[n]);     //Reporta el registro del dispositivo.
                end
             else                                      //Se ha DESCONECTADO el dispositivo n...
                begin
                Store := FStores[n];                   //Copia los datos del dispositivo antes de borraros de la lista.
                if UnregisterStore(n) then             //Borra los datos de registro del dispositivo.
                   OnEventUnregister(Store);           //Reporta el borrado de los datos de registro del dispositivo.
                OnEventDisconect(n);                   //Reporta la desconexión del dispositivo n.
                end;
             end;
          end;
      FStoresMask := NewStoresMask;                    //Actualiza la salva de la máscara de bits.
      end
   else                                                //Si no han cambiado los dispositivos...
      for n := 2 to cLastDevice do                     //Recorre la lista de dispositivos.
          if FStoresMask and (1 shl n) > 0 then        //Si está CONECTADO el dispositivo n...
             UpgradeStoreRegister(n);                  //Actualiza el registro del dispositivo.

except
end;
end;


//-----------------------------------------------------------------------------
// Devuelve los datos del dispositivo indicado.
//
// Entradas:
// index = Índice del dispositivo.
// Data = Aquí se colocan los datos que se obtienen del dispositivo indicado.
//
// Salida: Devuelve TRUE si el dispositivo indicado existe y está registrado.
//-----------------------------------------------------------------------------
function TStoreMonitor.GetStore(index: Byte; var Data: TStoreInfo): Boolean;
begin
Result := False;                                      //Por defecto devuelve FALSE.
if (index >= 2) and (index <= cLastDevice) then       //Si el índice está en rango...
   if IsStoreRegister(index) then                     //Y si el dispositivo está registrado...
      begin
      Data := FStores[index];                         //Devuelve los datos del dispositivo.
      Result := True;                                 //Devuelve TRUE.
      end;
end;

//-----------------------------------------------------------------------------
// Devuelve la dirección del dispositivo correspondiente al índice especificado.
// Son 32 dispositivos, por lo que el índice debe estar en un rango de 0 a 31.
// El primer índice es 0 y corresponde al dispositivo asignado a la unidad A.
//-----------------------------------------------------------------------------
function TStoreMonitor.GetStorePath(index: Byte): String;
begin
Result := Chr(65 + index) + ':\';
end;

//-----------------------------------------------------------------------------
// Dispara un evento OnConect.
//-----------------------------------------------------------------------------
procedure TStoreMonitor.OnEventConect(index: Byte);
begin
if FReportDevices then               //Si el detector está activado...
   if Assigned(FOnConect) then       //Si el evento está asignado...
      FOnConect(index);              //Avisa que se ha conectado un dispositivo.
end;

//-----------------------------------------------------------------------------
// Dispara un evento OnDisconect.
//-----------------------------------------------------------------------------
procedure TStoreMonitor.OnEventDisconect(index: Byte);
begin
if FReportDevices then               //Si el detector está activado...
   if Assigned(OnDisconect) then     //Si el evento está asignado...
      OnDisconect(index);            //Avisa de la desconexión de un dispositivo.
end;

//-----------------------------------------------------------------------------
// Dispara un evento OnRegister.
// Si el dispositivo es AutoExport, no se registra en la base de datos.
//-----------------------------------------------------------------------------
procedure TStoreMonitor.OnEventRegister(Store: TStoreInfo);
begin
if FReportDevices then                   //Si el detector está activado...
   if Assigned(FOnRegister) then         //Si el evento está asignado...
      FOnRegister(Store)                 //Avisa que se ha registrado un dispositivo.
end;

//-----------------------------------------------------------------------------
// Dispara un evento OnUnregister.
//-----------------------------------------------------------------------------
procedure TStoreMonitor.OnEventUnregister(Store: TStoreInfo);
begin
if FReportDevices then                   //Si el detector está activado...
   if Assigned(FOnUnregister) then       //Si el evento está asignado...
      FOnUnregister(Store)               //Avisa que se ha borrado un registro de dispositivo.
end;

//-----------------------------------------------------------------------------
// Dispara un evento OnEndSearch.
//-----------------------------------------------------------------------------
procedure TStoreMonitor.OnEventEndSearch;
begin
if Assigned(FOnEndSearch) then       //Si el evento está asignado...
   FOnEndSearch;                     //Avisa que se ha terminado una búsqueda.
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si el dispositivo de almacenamiento está listo.
//
// Los lectores de CD-ROM, DVD y otros similares, cuando se conectan o
// instalan en la computadora empiezan a ser reconocidos como dispositivos
// de almacenaiento, pero no responden a la función "GetVolumeInformation"
// o "GetDiskFreeSpaceEx" del API de Windows. Esto se debe a que los datos
// que devuelven estas funciones solo pueden ser obtenidos del CD o DVD que
// se inserte en el dispositivo de almacenamiento.
// Por esta razón, se creó esta función que comprueba si el dispositivo
// de almacenamiento está listo para ser encuestado.
//-----------------------------------------------------------------------------
function TStoreMonitor.IsStoreReady(index: Byte): Boolean;
var MFNL, VF, SN: DWORD;
    T: array[0..Max_Path]of char;
begin
try
   Result := GetVolumeInformation(PChar(GetStorePath(index)), T, Max_Path, @SN, MFNL, VF, nil, 0);
except
   Result := False;
end;
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si el dispositivo de almacenamiento está registrado.
//-----------------------------------------------------------------------------
function TStoreMonitor.IsStoreRegister(index: Byte): Boolean;
begin
Result := FStores[index].sRegister;
end;

//-----------------------------------------------------------------------------
// Obtiene datos del dispositivo de almacenamiento y los registra en la lista.
//-----------------------------------------------------------------------------
function TStoreMonitor.RegisterStore(index: Byte): Boolean;
var MaxFileNameLength, VolFlags, SerNum: DWORD;
    Temp: array[0..Max_Path]of char;
    BytesAvailable, TotalOfBytes, FreeBytes: Int64;
    pcSystemDirectory: PChar;
    dwSDSize: DWORD;
    RGVI: Boolean;
begin
Result := False;
try
   FStores[index].sRegister := False;

   //Intenta obtener el número de serie y nombre del dispositivo de almacenamiento.
   try
      RGVI := GetVolumeInformation(PChar(GetStorePath(index)), Temp, Max_Path,
                                   @SerNum, MaxFileNameLength, VolFlags, nil, 0);
   except
      RGVI := False;
   end;

   //Si se pudo obtiene el número de serie y nombre del dispositivo de almacenamiento...
   if RGVI then
      begin
      FStores[index].sDateTimeConect := Now;         //Obtiene la fecha y hora de conexión.
      FStores[index].sTimeConectionMin := 0;
      FStores[index].sRegister := True;
      FStores[index].sIndex := index;
      FStores[index].sLeter := Chr(65 + index);
      FStores[index].sName := String(Temp);
      FStores[index].sType := GetDriveType(PChar(LowerCase(GetStorePath(index))));
      FStores[index].sSerial := SerNum;

      //Obtiene la capacidad del dispositivo y su contenido en GB.
      try
         if GetDiskFreeSpaceEx(PChar(GetStorePath(index)), BytesAvailable, TotalOfBytes, @FreeBytes) then
            begin
            FStores[index].sCapacity := TotalOfBytes;
            FStores[index].sInfoInitial := (TotalOfBytes - FreeBytes);
            FStores[index].sInfoFinal := FStores[index].sInfoInitial;            //AL principio estos valores deben ser
            FStores[index].sInfoMinimal := FStores[index].sInfoInitial;          //iguales a la capacidad inicial.
            end;
      except
      end;

      //Determina si el dispositivo es la unidad de sistema.
      dwSDSize := MAX_PATH + 1;
      try
         GetMem(pcSystemDirectory, dwSDSize );
         try
            if Windows.GetSystemDirectory(pcSystemDirectory, dwSDSize ) <> 0 then
               FStores[index].sSystem := FStores[index].sLeter = UpperCase(pcSystemDirectory[0]);
         finally
            FreeMem(pcSystemDirectory );
         end;
      except
      end;
      Result := True;
      end;
except
   Result := False;
end;
end;

//-----------------------------------------------------------------------------
// Actualiza el registro de los dispositivos de almacenamiento.
//-----------------------------------------------------------------------------
procedure TStoreMonitor.UpgradeStoreRegister(index: Byte);
var BytesAvailable, TotalOfBytes, FreeBytes: Int64;
    Store: TStoreInfo;
    Temp: array[0..Max_Path]of char;
    MaxFileNameLength, VolFlags, SerNum: DWORD;
begin
if IsStoreReady(index) then                   //Si el dispositivo está listo...
   begin
   if not IsStoreRegister(index) then         //Si el dispositivo no está registrado...
      begin
      if RegisterStore(index) then            //Registra los datos del dispositivo.
         OnEventRegister(Fstores[index]);     //Reporta el registro del dispositivo.
      end
   else                                       //Si el dispositivo ya está registrado...
      begin                                   //Actualiza los datos registrados.
      try
         //Actualiza el nombre del dispositivo.
         try
            if GetVolumeInformation(PChar(GetStorePath(index)), Temp, Max_Path,
                                    @SerNum, MaxFileNameLength, VolFlags, nil, 0) then
               FStores[index].sName := String(Temp);
         except
         end;

         //Obtiene el espacio utilizado en el dispositivo en GB.
         try
            if GetDiskFreeSpaceEx(PChar(GetStorePath(index)), BytesAvailable, TotalOfBytes, @FreeBytes) then
               FStores[index].sInfoFinal := (TotalOfBytes - FreeBytes);
         except
         end;

         //Si el espacio utilizado actual es menor que el último registrado, actualiza el dato.
         if FStores[index].sInfoMinimal > FStores[index].sInfoFinal then
            FStores[index].sInfoMinimal := FStores[index].sInfoFinal;

         //Cuenta la duración de la conexión del dispositivo en minutos.
         FStores[index].sTimeConectionMin := MinutesBetween(Now, FStores[index].sDateTimeConect);
      except
      end;
      end;
   end
else                                   //Si no está listo el dispositivo...
   begin
   Store := FStores[index];            //Copia los datos del dispositivo antes de borraros de la lista.
   if UnregisterStore(index) then      //Borra los datos del registro del dispositivo.
      OnEventUnregister(Store);        //Reporta el borrado del registro del dispositivo.
   end;
end;

//-----------------------------------------------------------------------------
// Borra los datos del dispositivo y lo marca como no registrado.
//-----------------------------------------------------------------------------
function TStoreMonitor.UnregisterStore(index: Byte): Boolean;
begin
Result := False;
if IsStoreRegister(index) then
   begin
   FStores[index].sRegister := False;
   FStores[index].sIndex := 0;
   FStores[index].sLeter := Chr(0);
   FStores[index].sName := '';
   FStores[index].sType := 0;
   FStores[index].sSystem := False;
   FStores[index].sSerial := 0;
   FStores[index].sDateTimeConect := 0;
   FStores[index].sTimeConectionMin := 0;
   FStores[index].sCapacity := 0;
   FStores[index].sInfoInitial := 0;
   FStores[index].sInfoMinimal := 0;
   FStores[index].sInfoFinal := 0;
   Result := True;
   end;
end;

//-----------------------------------------------------------------------------
// Devuelve una cadena con el tipo de dispositivo, según WinAPI.
//-----------------------------------------------------------------------------
function TStoreMonitor.TypeToString(pType: Byte): String;
begin
case pType of
     0:               Result := 'Desconocido';
     1:               Result := 'No existe';
     DRIVE_REMOVABLE: Result := 'Extraible';
     DRIVE_FIXED:     Result := 'Fijo';
     DRIVE_REMOTE:    Result := 'Unidad de red';
     DRIVE_CDROM:     Result := 'CD-ROM';
     DRIVE_RAMDISK:   Result := 'Disco-RAM';
     else Result := '';
     end;
end;



end.
