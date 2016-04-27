
{******************************************}
{                                          }
{             FastReport v3.0              }
{         mFX enduser components           }
{                                          }
{         Copyright (c) 2004               }
{         by Carlos de Cumont              }
{                                          }
{******************************************}

unit frxmFXEditor;

interface

{$I mFX.Inc}

implementation

uses Classes, SysUtils, Variants,
  {$IFDEF MSWINDOWS}Windows,{$ENDIF MSWINDOWS}
  {$IFDEF VCL}Vcl.Forms, Vcl.Dialogs,{$ENDIF VCL}
  frxMFXComponents, frxCustomDB, frxDsgnIntf, frxRes, mFX.Base, mFX.ClientDataSet;

type
  TfrxDatabaseNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    function Edit: Boolean; override;
  end;

  TfrxDatabaseProperty = class(TfrxComponentProperty)
  public
    function GetValue: String; override;
  end;

  TfrxIndexNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure GetValues; override;
  end;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TfrxDatabaseNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paDialog];
end;
{______________________________________________________________________________}
function TfrxDatabaseNameProperty.Edit: Boolean;
var SaveConnected: Boolean;
    db:TFXCustomDatabase;
begin
  with TOpenDialog.Create(nil) do begin
    InitialDir := GetCurrentDir;
    Filter := frxResources.Get('ftDB') + ' (*.gdb)|*.gdb|' + frxResources.Get('ftAllFiles') + ' (*.*)|*.*';
    Result := Execute;
    if Result then begin
      db:=TfrxmFXDatabase(Component).Database;
      SaveConnected   := db.Connected;
      db.Connected    := False;
      db.DatabaseName := FileName;
      db.Connected    := SaveConnected;
      end;
    Free;
    end;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TfrxDatabaseProperty.GetValue: String;
var frxdb:TfrxmFXDatabase;
    db:TFXCustomDatabase;
begin
  frxdb:=TfrxmFXDatabase(GetOrdValue);
  if frxdb=nil then begin
    db:=frxMFXComponents.MFXMasterDB;
    if db<>nil then
      Result := db.Name else
      Result := frxResources.Get('prNotAssigned');
  end else
    Result := inherited GetValue;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TfrxIndexNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;
{______________________________________________________________________________}
procedure TfrxIndexNameProperty.GetValues;
var i: Integer;
begin
  inherited;
  try with TfrxmFXClientDataSet(Component).ClientDataSet do Begin
        if (IndexDefs <> nil) then begin
          IndexDefs.Update;
          for i := 0 to Pred(IndexDefs.Count) do Begin
            if IndexDefs[i].Name <> '' then
              Values.Add(IndexDefs[i].Name);
        end end end 
  except
  end;
end;

initialization

  frxPropertyEditors.Register(
    TypeInfo(String),
    TfrxmFXDatabase,
    'DatabaseName',
    TfrxDataBaseNameProperty
    );

  frxPropertyEditors.Register(
    TypeInfo(TfrxmFXDatabase),
    TfrxmFXClientDataSet,
    'Database',
    TfrxDatabaseProperty
    );

  frxPropertyEditors.Register(
    TypeInfo(String),
    TfrxmFXClientDataSet,
    'IndexName',
    TfrxIndexNameProperty
    );

end.
