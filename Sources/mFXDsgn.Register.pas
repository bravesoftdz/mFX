unit mFXDsgn.Register;

interface

{$I mFX.Inc}

Uses System.Classes, Data.DB,
  DesignIntf, DesignEditors,
  {$IFDEF LINUX}Signals, QDialogs, QForms,{$ENDIF}
  {$IFDEF MSWINDOWS}Dialogs, Forms,{$ENDIF}
  SysUtils;

procedure Register;

implementation

Uses DesignConst, TypInfo,
  mFX.Header, mFX.Consts, mFX.Utils, mFX.Classes, mFX.Database, mFX.SQL, mFX.SQLScript, mFX.DDLTool, mFX.DatabaseInfo, mFX.Services,
  mFX.InternalDataSet, mFX.ClientDataSet, mFX.SQLField;

{$R mFXIcons.res}

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure Register;
Begin

  RegisterFields([
    TFXStringField,
    TFXMemoField,
    TFXBCDField
    ]);

  RegisterComponents('Firebird', [
    TFXDatabase
   ,TFXTransaction
   ,TFXSQL
   ,TFXSQLScript
   ,TFXBackupService
   ,TFXRestoreService
    ]);

end;

end.

