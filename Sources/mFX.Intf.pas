unit mFX.Intf;

interface

{$I mFX.Inc}

uses System.Classes, System.SysUtils,
  mFX.Header, mFX.Classes, mFX.Consts;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  TFXClientLibraryVersion = (
    fxsv_Undefined,  //'n/a';
    fxsv_NotFound,   //'n/a';
    fxsv_ibPre6,
    fxsv_fb,         //Firebird
    fxsv_fbEmbeded   //Firebird 1.5';
    );

  TFXEventIdx = 0..Pred(FX_MAX_EVENT_BLOCK);
  TFXEventsCount = Array [TFXEventIdx] of ISC_STATUS;

  IFXClientLib = interface
  ['{793A4F88-687F-4533-86FA-32EFE5D1DD36}']

    /// <summary>Clear LastError</summary>
    procedure ClearLastErrorMsg;
    /// <summary>Pop Firebird Error</summary>
    procedure ReadFirebirdError(Const Sender:TObject);overload;
    /// <summary>Pop Firebird Error</summary>
    procedure ReadFirebirdError(Const Sender:TObject;Const aContext:String);overload;
    /// <summary>Raise Firebird Error</summary>
    procedure RaiseLastFirebirdError(Const Sender:TObject);
    /// <summary>Raise Firebird Error</summary>
    procedure ReadAndRaiseFirebirdError(Const Sender:TObject);overload;
    /// <summary>Raise Firebird Error</summary>
    procedure ReadAndRaiseFirebirdError(Const Sender:TObject;Const aContext:String);overload;
    /// <summary>Raise Firebird Error</summary>
    function LastFirebirdError(Const Sender:TObject):EFXError;
    /// <summary>Raise Firebird Error</summary>
    function LastFirebirdErrorMsg(Const Sender:TObject):String;
    /// <summary>CheckStatusVector</summary>
    function CheckStatusVector(Const ErrorCodes: array of ISC_STATUS): Boolean;


    function Call_attach_database(Const db_name:String;Const db_handle:PISC_DB_HANDLE;Const parm_buffer_length:FXShort;Const parm_buffer:PFXByte): ISC_STATUS;
    function Call_database_info(Const db_handle:PISC_DB_HANDLE;Const item_list_buffer_length:FXShort;Const item_list_buffer:PFXByte;Const result_buffer_length:FXShort;Const result_buffer:PFXByte): ISC_STATUS;
    procedure Check_database_info(Const Sender:TObject;Const db_handle:PISC_DB_HANDLE;Const item_list_buffer_length:FXShort;Const item_list_buffer:PFXByte;Const result_buffer_length:FXShort;Const result_buffer:PFXByte);
    function Call_detach_database(Const db_handle:PISC_DB_HANDLE):ISC_STATUS;
    procedure Check_drop_database(Const db_handle:PISC_DB_HANDLE);


    function Call_start_multiple(Const tran_handle:PISC_TR_HANDLE;Const db_handle_count:FXShort;Const teb_vector_address:PISC_TEB):ISC_STATUS;
    procedure Check_commit_retaining(Const tran_handle:PISC_TR_HANDLE);
    function Call_commit_retaining(Const tran_handle:PISC_TR_HANDLE):ISC_STATUS;
    function Call_commit_transaction(Const tran_handle:PISC_TR_HANDLE):ISC_STATUS;
    procedure Check_rollback_retaining(Const tran_handle:PISC_TR_HANDLE);
    function Call_rollback_retaining(Const tran_handle:PISC_TR_HANDLE):ISC_STATUS;
    function Call_rollback_transaction(Const tran_handle:PISC_TR_HANDLE):ISC_STATUS;


    procedure Check_dsql_alloc_statement2(Const db_handle:PISC_DB_HANDLE;Const stmt_handle:PISC_STMT_HANDLE);
    function Call_dsql_alloc_statement2(Const db_handle:PISC_DB_HANDLE;Const stmt_handle:PISC_STMT_HANDLE):ISC_STATUS;
    function Call_dsql_prepare(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const statement:TBytes):ISC_STATUS;
    procedure Check_dsql_prepare(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const statement:TBytes);
    procedure Check_dsql_sql_info(Const stmt_handle:PISC_STMT_HANDLE;Const item_length:FXShort;Const items:PFXByte;Const buffer_length:FXShort;Const buffer:PFXByte);
    function Call_dsql_sql_info(Const stmt_handle:PISC_STMT_HANDLE;Const item_length:FXShort;Const items:PFXByte;Const buffer_length:FXShort;Const buffer:PFXByte):ISC_STATUS;
    procedure Check_dsql_describe(Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA);
    function Call_dsql_describe(Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA):ISC_STATUS;
    procedure Check_dsql_describe_bind(Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA);
    function Call_dsql_describe_bind(Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA):ISC_STATUS;
    procedure Check_dsql_execute(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA);
    function Call_dsql_execute(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA):ISC_STATUS;
    Procedure Check_dsql_execute2(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const in_xsqlda:PXSQLDA);overload;
    Procedure Check_dsql_execute2(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const in_xsqlda,out_xsqlda:PXSQLDA);overload;
    function Call_dsql_execute2(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const in_xsqlda:PXSQLDA):ISC_STATUS;overload;
    function Call_dsql_execute2(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const in_xsqlda,out_xsqlda:PXSQLDA):ISC_STATUS;overload;
    Procedure Check_dsql_set_cursor_name(Const stmt_handle:PISC_STMT_HANDLE;Const cursor_name:PAnsiChar);
    function Call_dsql_free_statement(stmt_handle:PISC_STMT_HANDLE;options:FXUShort):ISC_STATUS;
    function Call_dsql_fetch(Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA):ISC_STATUS;
    procedure Check_dsql_execute_immediate(Const db_handle:PISC_DB_HANDLE;Const tran_handle:PISC_TR_HANDLE;Const length:FXUShort;Const statement:PAnsiChar);


    Procedure Check_close_blob(Const hBlob_Handle : PISC_BLOB_HANDLE);
    function Call_close_blob(Const hBlob_Handle : PISC_BLOB_HANDLE):ISC_STATUS;
    Procedure Check_create_blob2(Const db_handle: PISC_DB_HANDLE;Const tr_handle:PISC_TR_HANDLE;Const hBlob_Handle : PISC_BLOB_HANDLE;Const blob_id : PISC_QUAD);
    Procedure Check_open_blob2(Const db_handle: PISC_DB_HANDLE;Const tr_handle:PISC_TR_HANDLE;Const hBlob_Handle : PISC_BLOB_HANDLE;Const blob_id : PISC_QUAD);
    procedure Check_ReadBlob(Const hBlobHandle: PISC_BLOB_HANDLE;Const aBuffer: PFXByte;Const aNumSegments,aMaxSegmentSize,aBlobSize: FXLong);
    function Call_ReadBlob(Const hBlobHandle: PISC_BLOB_HANDLE;Const aBuffer: PFXByte;Const aNumSegments,aMaxSegmentSize,aBlobSize: FXLong):Boolean;
    procedure WriteBlob(Const hBlobHandle: PISC_BLOB_HANDLE;Const aBuffer: PFXByte;Const aBlobSize: FXLong);
    procedure Check_BlobInfo(Const hBlobHandle: PISC_BLOB_HANDLE; var NumSegments,MaxSegmentSize,BlobSize:FXLong;var BlobType: FXShort);
    function Call_BlobInfo(Const hBlobHandle: PISC_BLOB_HANDLE; var NumSegments,MaxSegmentSize,BlobSize:FXLong;var BlobType: FXShort):ISC_STATUS;



    // Service manager functions
    function Call_service_attach(Const isc_arg2:FXUShort;Const isc_arg3:PAnsiChar;Const service_handle: PISC_SVC_HANDLE;Const isc_arg5:FXUShort;Const isc_arg6:PFXByte): ISC_STATUS;
    function Call_service_detach(Const service_handle:PISC_SVC_HANDLE): ISC_STATUS;
    function Call_service_start(Const service_handle: PISC_SVC_HANDLE;Const recv_handle:PISC_SVC_HANDLE;Const isc_arg4:FXUShort;Const isc_arg5:PFXByte):ISC_STATUS;
    function Call_service_query(Const service_handle: PISC_SVC_HANDLE;Const recv_handle:PISC_SVC_HANDLE;Const isc_arg4:FXUShort;Const isc_arg5:PFXByte;Const isc_arg6:FXUShort;Const isc_arg7:PFXByte;Const isc_arg8:FXUShort;Const isc_arg9:PFXByte): ISC_STATUS;
  end;

/// <summary>LoadLib</summary>
function LoadLib:IFXClientLib;
/// <summary>LoadLib</summary>
function LoadClientLib:Boolean;
/// <summary>LoadLib</summary>
procedure FreeLoadClientLib;

implementation

Uses
  {$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF}
  mFX.ClientLib, mFX.ClientStub;

{$IFDEF MSWINDOWS}
Const
  FIREBIRD_DLL   = 'fbclient.dll';
  {$IFDEF WIN32}FIREBIRD32_DLL = 'fbclient32.dll';{$ENDIF}
  {$IFDEF WIN64}FIREBIRD64_DLL = 'fbclient64.dll';{$ENDIF}
{$ENDIF}

{$IFDEF LINUX}
Const
    'libgds.so';
{$ENDIF}

Var
  ClientLibHandle  : THandle = 0;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function LoadLib:IFXClientLib;
Begin
  if LoadClientLib then
    Result:=TFXClientLibrary.Create(ClientLibHandle) else
    Result:=TFXClientStub.Create
End;
{______________________________________________________________________________}
function LoadClientLib:Boolean;
Var h:THandle;
begin
  // try fbclient.dll then fbclient[32|64].dll
  if ClientLibHandle=0 then Begin
    h:=GetModuleHandle(FIREBIRD_DLL);
    {$IFDEF WIN32}if h=0 then h:=GetModuleHandle(FIREBIRD32_DLL);{$ENDIF}
    {$IFDEF WIN64}if h=0 then h:=GetModuleHandle(FIREBIRD64_DLL);{$ENDIF}
    if h=0 then Begin
      h:=System.SysUtils.SafeLoadLibrary(FIREBIRD_DLL);
      {$IFDEF WIN32}if h=0 then h:=System.SysUtils.SafeLoadLibrary(FIREBIRD32_DLL);{$ENDIF}
      {$IFDEF WIN64}if h=0 then h:=System.SysUtils.SafeLoadLibrary(FIREBIRD64_DLL);{$ENDIF}
      if h=0 then Begin
        // No way to load lib
        Assert(ClientLibHandle=0);
      end else Begin
        // OK Loaded
        ClientLibHandle:=h
        end;
    end else Begin
      // OK Loaded
      ClientLibHandle:=h
    end end;
  Result:=ClientLibHandle<>0;
end;
{______________________________________________________________________________}
procedure FreeLoadClientLib;
Var TempHandle:THandle;
begin
  if (ClientLibHandle<>0) then begin
    TempHandle:=ClientLibHandle;
    ClientLibHandle:=0;
    if not FreeLibrary(TempHandle) then
      Raise Exception.Create('Unload Error');
    end
end;


end.

