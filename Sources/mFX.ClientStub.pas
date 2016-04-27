unit mFX.ClientStub;

interface

{$I mFX.Inc}

uses System.Classes, System.SysUtils,
  mFX.Header, mFX.Classes, mFX.Consts, mFX.Intf;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  TFXClientStub = class(TInterfacedObject,IFXClientLib)
  private
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

implementation

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXClientStub.Call_attach_database(const db_name: String;const db_handle: PISC_DB_HANDLE; const parm_buffer_length: FXShort;const parm_buffer: PFXByte): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(nil,fxceFirebirdMissing,'attach_database');
  Result := 0;
end;
{______________________________________________________________________________}
function TFXClientStub.Call_BlobInfo(const hBlobHandle: PISC_BLOB_HANDLE;var NumSegments, MaxSegmentSize, BlobSize: FXLong;var BlobType: FXShort): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'BlobInfo');
  Result := 0;
end;
{______________________________________________________________________________}
function TFXClientStub.Call_close_blob(const hBlob_Handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'close_blob');
  Result := 0;
end;
{______________________________________________________________________________}
function TFXClientStub.Call_commit_retaining(const tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'commit_retaining');
  Result := 0;
end;
{______________________________________________________________________________}
function TFXClientStub.Call_commit_transaction(const tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'commit_transaction');
  Result := 0;
end;
{______________________________________________________________________________}
function TFXClientStub.Call_database_info(const db_handle: PISC_DB_HANDLE;const item_list_buffer_length: FXShort; const item_list_buffer: PFXByte;const result_buffer_length: FXShort;  const result_buffer: PFXByte): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'database_info');
  Result := 0;
end;
{______________________________________________________________________________}
function TFXClientStub.Call_detach_database(const db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'detach_database');
  Result := 0;
end;
{______________________________________________________________________________}
function TFXClientStub.Call_dsql_alloc_statement2(const db_handle: PISC_DB_HANDLE;const stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'dsql_alloc_statement2');
  Result := 0;
end;

function TFXClientStub.Call_dsql_describe(const stmt_handle: PISC_STMT_HANDLE;const xsqlda: PXSQLDA): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'dsql_describe');
  Result := 0;
end;

function TFXClientStub.Call_dsql_describe_bind(const stmt_handle: PISC_STMT_HANDLE; const xsqlda: PXSQLDA): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'dsql_describe_bind');
  Result := 0;
end;

function TFXClientStub.Call_dsql_execute(const tran_handle: PISC_TR_HANDLE;const stmt_handle: PISC_STMT_HANDLE; const xsqlda: PXSQLDA): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'dsql_execute');
  Result := 0;
end;

function TFXClientStub.Call_dsql_execute2(const tran_handle: PISC_TR_HANDLE;const stmt_handle: PISC_STMT_HANDLE; const in_xsqlda: PXSQLDA): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'dsql_execute2');
  Result := 0;
end;

function TFXClientStub.Call_dsql_execute2(const tran_handle: PISC_TR_HANDLE;const stmt_handle: PISC_STMT_HANDLE; const in_xsqlda,out_xsqlda: PXSQLDA): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'dsql_execute2');
  Result := 0;
end;

function TFXClientStub.Call_dsql_fetch(const stmt_handle: PISC_STMT_HANDLE;const xsqlda: PXSQLDA): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'dsql_fetch');
  Result := 0;
end;

function TFXClientStub.Call_dsql_free_statement(stmt_handle: PISC_STMT_HANDLE;options: FXUShort): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'dsql_free_statement');
  Result := 0;
end;

function TFXClientStub.Call_dsql_prepare(const tran_handle: PISC_TR_HANDLE;const stmt_handle: PISC_STMT_HANDLE; const statement: TBytes): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'dsql_prepare');
  Result := 0;
end;

function TFXClientStub.Call_dsql_sql_info(const stmt_handle: PISC_STMT_HANDLE;const item_length: FXShort; const items: PFXByte;const buffer_length: FXShort; const buffer: PFXByte): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'dsql_sql_info');
  Result := 0;
end;

function TFXClientStub.Call_ReadBlob(const hBlobHandle: PISC_BLOB_HANDLE;const aBuffer: PFXByte; const aNumSegments, aMaxSegmentSize,aBlobSize: FXLong): Boolean;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'ReadBlob');
  Result := False;
end;

function TFXClientStub.Call_rollback_retaining(const tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'rollback_retaining');
  Result := 0;
end;

function TFXClientStub.Call_rollback_transaction(const tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'rollback_transaction');
  Result := 0;
end;

function TFXClientStub.Call_service_attach(const isc_arg2: FXUShort;const isc_arg3: PAnsiChar; const service_handle: PISC_SVC_HANDLE;const isc_arg5: FXUShort; const isc_arg6: PFXByte): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'service_attach');
  Result := 0;
end;

function TFXClientStub.Call_service_detach(const service_handle: PISC_SVC_HANDLE): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'service_detach');
  Result := 0;
end;

function TFXClientStub.Call_service_query(const service_handle,recv_handle: PISC_SVC_HANDLE; const isc_arg4: FXUShort;const isc_arg5: PFXByte; const isc_arg6: FXUShort; const isc_arg7: PFXByte;const isc_arg8: FXUShort; const isc_arg9: PFXByte): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'service_query');
  Result := 0;
end;

function TFXClientStub.Call_service_start(const service_handle,recv_handle: PISC_SVC_HANDLE; const isc_arg4: FXUShort;const isc_arg5: PFXByte): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'service_start');
  Result := 0;
end;

function TFXClientStub.Call_start_multiple(const tran_handle: PISC_TR_HANDLE;const db_handle_count: FXShort;const teb_vector_address: PISC_TEB): ISC_STATUS;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'start_multiple');
  Result := 0;
end;

function TFXClientStub.CheckStatusVector(const ErrorCodes: array of ISC_STATUS): Boolean;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'StatusVector');
  Result := False;
end;

procedure TFXClientStub.Check_BlobInfo(const hBlobHandle: PISC_BLOB_HANDLE;var NumSegments, MaxSegmentSize, BlobSize: FXLong; var BlobType: FXShort);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_BlobInfo');
end;

procedure TFXClientStub.Check_close_blob(const hBlob_Handle: PISC_BLOB_HANDLE);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_close_blob');
end;

procedure TFXClientStub.Check_commit_retaining(const tran_handle: PISC_TR_HANDLE);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_commit_retaining');
end;

procedure TFXClientStub.Check_create_blob2(const db_handle: PISC_DB_HANDLE;const tr_handle: PISC_TR_HANDLE; const hBlob_Handle: PISC_BLOB_HANDLE;const blob_id: PISC_QUAD);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_create_blob2');
end;

procedure TFXClientStub.Check_database_info(const Sender: TObject;const db_handle: PISC_DB_HANDLE; const item_list_buffer_length: FXShort;const item_list_buffer: PFXByte; const result_buffer_length: FXShort;const result_buffer: PFXByte);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_database_info');
end;

procedure TFXClientStub.Check_drop_database(const db_handle: PISC_DB_HANDLE);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_drop_database');
end;

procedure TFXClientStub.Check_dsql_alloc_statement2(const db_handle: PISC_DB_HANDLE; const stmt_handle: PISC_STMT_HANDLE);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_dsql_alloc_statement2');
end;

procedure TFXClientStub.Check_dsql_describe(const stmt_handle: PISC_STMT_HANDLE;const xsqlda: PXSQLDA);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_dsql_describe');
end;

procedure TFXClientStub.Check_dsql_describe_bind(const stmt_handle: PISC_STMT_HANDLE; const xsqlda: PXSQLDA);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_dsql_describe_bind');
end;

procedure TFXClientStub.Check_dsql_execute(const tran_handle: PISC_TR_HANDLE;const stmt_handle: PISC_STMT_HANDLE; const xsqlda: PXSQLDA);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_dsql_execute');
end;

procedure TFXClientStub.Check_dsql_execute2(const tran_handle: PISC_TR_HANDLE;const stmt_handle: PISC_STMT_HANDLE; const in_xsqlda: PXSQLDA);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_dsql_execute2');
end;

procedure TFXClientStub.Check_dsql_execute2(const tran_handle: PISC_TR_HANDLE;const stmt_handle: PISC_STMT_HANDLE; const in_xsqlda, out_xsqlda: PXSQLDA);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_dsql_execute2');
end;

procedure TFXClientStub.Check_dsql_execute_immediate(const db_handle: PISC_DB_HANDLE; const tran_handle: PISC_TR_HANDLE;const length: FXUShort; const statement: PAnsiChar);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_dsql_execute_immediate');
end;

procedure TFXClientStub.Check_dsql_prepare(const tran_handle: PISC_TR_HANDLE;const stmt_handle: PISC_STMT_HANDLE; const statement: TBytes);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_dsql_prepare');
end;

procedure TFXClientStub.Check_dsql_set_cursor_name(const stmt_handle: PISC_STMT_HANDLE; const cursor_name: PAnsiChar);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_dsql_set_cursor_name');
end;

procedure TFXClientStub.Check_dsql_sql_info(const stmt_handle: PISC_STMT_HANDLE;const item_length: FXShort; const items: PFXByte;const buffer_length: FXShort; const buffer: PFXByte);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_dsql_sql_info');
end;

procedure TFXClientStub.Check_open_blob2(const db_handle: PISC_DB_HANDLE;const tr_handle: PISC_TR_HANDLE; const hBlob_Handle: PISC_BLOB_HANDLE;const blob_id: PISC_QUAD);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_open_blob2');
end;

procedure TFXClientStub.Check_ReadBlob(const hBlobHandle: PISC_BLOB_HANDLE;const aBuffer: PFXByte; const aNumSegments, aMaxSegmentSize,aBlobSize: FXLong);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_ReadBlob');
end;

procedure TFXClientStub.Check_rollback_retaining(const tran_handle: PISC_TR_HANDLE);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'_rollback_retaining');
end;

procedure TFXClientStub.ClearLastErrorMsg;
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'LastErrorMsg');
end;

function TFXClientStub.LastFirebirdError(const Sender: TObject): EFXError;
begin
  Result := EFXClientError.CreateMsg(Sender,fxceFirebirdMissing,'FirebirdError');
end;

function TFXClientStub.LastFirebirdErrorMsg(const Sender: TObject): String;
begin
  Result := FXClientErrorMsg(fxceFirebirdMissing,['FirebirdError']);
end;

procedure TFXClientStub.RaiseLastFirebirdError(const Sender: TObject);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'LastFirebirdError');
end;

procedure TFXClientStub.ReadAndRaiseFirebirdError(const Sender: TObject;const aContext: String);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'ndRaiseFirebirdError');
end;

procedure TFXClientStub.ReadAndRaiseFirebirdError(const Sender: TObject);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'ndRaiseFirebirdError');
end;

procedure TFXClientStub.ReadFirebirdError(const Sender: TObject);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'irebirdError');
end;

procedure TFXClientStub.ReadFirebirdError(const Sender: TObject;const aContext: String);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'irebirdError');
end;

procedure TFXClientStub.WriteBlob(const hBlobHandle: PISC_BLOB_HANDLE;const aBuffer: PFXByte; const aBlobSize: FXLong);
begin
  FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'Blob');
end;

end.
