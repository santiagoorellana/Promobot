
///////////////////////////////////////////////////////////////////////////////
// Nombre: UFormSelectDir
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 2018
// Objetivo: Formulario que permite seleccionar un directorio.
///////////////////////////////////////////////////////////////////////////////

unit UFormSelectDir;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ComCtrls,
  ActnList, Buttons, FileCtrl, ToolWin;

type
  TFormSelectDir = class(TForm)
    DirectoryListBox1: TDirectoryListBox;
    ToolBar1: TToolBar;
    DriveComboBox1: TDriveComboBox;
    ToolBar2: TToolBar;
    ActionList1: TActionList;
    ActionAcept: TAction;
    ActionCancel: TAction;
    ToolButton2: TToolButton;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ToolButton1: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure ActionAceptExecute(Sender: TObject);
    procedure ActionCancelExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses UMainForm;

var FormSelectDir: TFormSelectDir;

{$R *.dfm}

//-----------------------------------------------------------------------------
// Inicia el formulario.
//-----------------------------------------------------------------------------
procedure TFormSelectDir.FormCreate(Sender: TObject);
begin
DirectoryListBox1.Directory := MainForm.PromoDirectory;
end;

//-----------------------------------------------------------------------------
// Selecciona el directorio especificado por el usuario.
//-----------------------------------------------------------------------------
procedure TFormSelectDir.ActionAceptExecute(Sender: TObject);
begin
MainForm.PromoDirectory := DirectoryListBox1.Directory;
Close;
end;

//-----------------------------------------------------------------------------
// No cambia de directorio.
//-----------------------------------------------------------------------------
procedure TFormSelectDir.ActionCancelExecute(Sender: TObject);
begin
Close;
end;

end.
