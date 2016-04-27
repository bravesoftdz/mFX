unit mFX.ClientLib;

interface

{$I mFX.Inc}

uses System.Classes, System.SysUtils,
  {$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF}
  mFX.Header, mFX.Classes, mFX.Consts, mFX.Intf;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  TFXClientLibrary = class(TInterfacedObject,IFXClientLib)
  private
    fClientLibVersion            : TFXClientLibraryVersion;
    fStatusVector                : TStatusVector;
    fLastErrorContext            : String;
    fLastSQLErrorCode            : ISC_LONG;
    fLastSQLErrorMessage         : String;
    fLastErrorCode               : ISC_LONG;
    fLastErrorMessage            : String;
    fisc_blob_get                : Tisc_blob_get;
    fisc_blob_put                : Tisc_blob_put;
    fisc_sqlcode                 : Tisc_sqlcode;
    fisc_sql_interprete          : Tisc_sql_interprete;
    fisc_interprete              : Tisc_interprete;
    fisc_blob_info               : Tisc_blob_info;
    fisc_open_blob2              : Tisc_open_blob2;
    fisc_close_blob              : Tisc_close_blob;
    fisc_get_segment             : Tisc_get_segment;
    fisc_put_segment             : Tisc_put_segment;
    fisc_create_blob2            : Tisc_create_blob2;
    fisc_service_attach          : Tisc_service_attach;
    fisc_service_detach          : Tisc_service_detach;
    fisc_service_query           : Tisc_service_query;
    fisc_service_start           : Tisc_service_start;
    fisc_dsql_free_statement     : Tisc_dsql_free_statement;
    fisc_dsql_execute2           : Tisc_dsql_execute2;
    fisc_dsql_execute            : Tisc_dsql_execute;
    fisc_dsql_set_cursor_name    : Tisc_dsql_set_cursor_name;
    fisc_dsql_fetch              : Tisc_dsql_fetch;
    fisc_dsql_sql_info           : Tisc_dsql_sql_info;
    fisc_dsql_alloc_statement2   : Tisc_dsql_alloc_statement2;
    fisc_dsql_prepare            : Tisc_dsql_prepare;
    fisc_dsql_describe_bind      : Tisc_dsql_describe_bind;
    fisc_dsql_describe           : Tisc_dsql_describe;
    fisc_dsql_execute_immediate  : Tisc_dsql_execute_immediate;
    fisc_drop_database           : Tisc_drop_database;
    fisc_detach_database         : Tisc_detach_database;
    fisc_attach_database         : Tisc_attach_database;
    fisc_database_info           : Tisc_database_info;
    fisc_start_multiple          : Tisc_start_multiple;
    fisc_commit_transaction      : Tisc_commit_transaction;
    fisc_commit_retaining        : Tisc_commit_retaining;
    fisc_rollback_transaction    : Tisc_rollback_transaction;
    fisc_rollback_retaining      : Tisc_rollback_retaining;
    fisc_cancel_events           : Tisc_cancel_events;
    fisc_que_events              : Tisc_que_events;
    fisc_event_counts            : Tisc_event_counts;
    fisc_event_block             : Tisc_event_block;
    fisc_free                    : Tisc_free;
    fisc_add_user                : Tisc_add_user;
    fisc_delete_user             : Tisc_delete_user;
    fisc_modify_user             : Tisc_modify_user;
    fisc_prepare_transaction     : Tisc_prepare_transaction;
    fisc_prepare_transaction2    : Tisc_prepare_transaction2;

    fisc_get_client_version      : Tisc_get_client_version;
    fisc_get_client_major_version: Tisc_get_client_major_version;
    fisc_get_client_minor_version: Tisc_get_client_minor_version;

    /// <SUMMARY>LoadAPIs</SUMMARY>
    procedure LoadAPIs(Const ClientLibHandle:THandle);

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

  public
    /// <SUMMARY>constructor</SUMMARY>
    constructor Create(Const ClientLibHandle:THandle);reintroduce;virtual;
    /// <summary>destructor</summary>
    class destructor Destroy;

  end;

implementation

uses mFX.ErrorCodes;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXClientLibrary.Create(Const ClientLibHandle:THandle);
Begin
  inherited Create;
  Self.LoadAPIs(ClientLibHandle);
end;
{______________________________________________________________________________}
class destructor TFXClientLibrary.Destroy;
begin
  FreeLoadClientLib;
  inherited;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXClientLibrary.LoadAPIs(Const ClientLibHandle:THandle);
  {____________________________________________________________________________}
  function GetProcAddr(Const ProcName: PAnsiChar): Pointer;
  begin
    {$IFDEF MSWINDOWS}
    Result:=GetProcAddress(ClientLibHandle, ProcName);
    {$ENDIF}
    {$IFDEF LINUX}
    Result := dlsym(fLibrary, ProcName);
    {$ENDIF}
  end;
Begin
  fisc_blob_get                 := GetProcAddr('BLOB_get'                         );
  fisc_blob_put                 := GetProcAddr('BLOB_put'                         );
  fisc_sqlcode                  := GetProcAddr('isc_sqlcode'                      );
  fisc_sql_interprete           := GetProcAddr('isc_sql_interprete'               );
  fisc_interprete               := GetProcAddr('fb_interpret'                     );
  fisc_blob_info                := GetProcAddr('isc_blob_info'                    );
  fisc_open_blob2               := GetProcAddr('isc_open_blob2'                   );
  fisc_close_blob               := GetProcAddr('isc_close_blob'                   );
  fisc_get_segment              := GetProcAddr('isc_get_segment'                  );
  fisc_put_segment              := GetProcAddr('isc_put_segment'                  );
  fisc_create_blob2             := GetProcAddr('isc_create_blob2'                 );
  fisc_dsql_free_statement      := GetProcAddr('isc_dsql_free_statement'          );
  fisc_dsql_execute2            := GetProcAddr('isc_dsql_execute2'                );
  fisc_dsql_execute             := GetProcAddr('isc_dsql_execute'                 );
  fisc_dsql_set_cursor_name     := GetProcAddr('isc_dsql_set_cursor_name'         );
  fisc_dsql_fetch               := GetProcAddr('isc_dsql_fetch'                   );
  fisc_dsql_sql_info            := GetProcAddr('isc_dsql_sql_info'                );
  fisc_dsql_alloc_statement2    := GetProcAddr('isc_dsql_alloc_statement2'        );
  fisc_dsql_prepare             := GetProcAddr('isc_dsql_prepare'                 );
  fisc_dsql_describe_bind       := GetProcAddr('isc_dsql_describe_bind'           );
  fisc_dsql_describe            := GetProcAddr('isc_dsql_describe'                );
  fisc_dsql_execute_immediate   := GetProcAddr('isc_dsql_execute_immediate'       );
  fisc_drop_database            := GetProcAddr('isc_drop_database'                );
  fisc_detach_database          := GetProcAddr('isc_detach_database'              );
  fisc_attach_database          := GetProcAddr('isc_attach_database'              );
  fisc_database_info            := GetProcAddr('isc_database_info'                );
  fisc_start_multiple           := GetProcAddr('isc_start_multiple'               );
  fisc_commit_transaction       := GetProcAddr('isc_commit_transaction'           );
  fisc_commit_retaining         := GetProcAddr('isc_commit_retaining'             );
  fisc_rollback_transaction     := GetProcAddr('isc_rollback_transaction'         );
  fisc_cancel_events            := GetProcAddr('isc_cancel_events'                );
  fisc_que_events               := GetProcAddr('isc_que_events'                   );
  fisc_event_counts             := GetProcAddr('isc_event_counts'                 );
  fisc_event_block              := GetProcAddr('isc_event_block'                  );
  fisc_free                     := GetProcAddr('isc_free'                         );
  fisc_add_user                 := GetProcAddr('isc_add_user'                     );
  fisc_delete_user              := GetProcAddr('isc_delete_user'                  );
  fisc_modify_user              := GetProcAddr('isc_modify_user'                  );
  fisc_prepare_transaction      := GetProcAddr('isc_prepare_transaction'          );
  fisc_prepare_transaction2     := GetProcAddr('isc_prepare_transaction2'         );

  fisc_rollback_retaining       := GetProcAddr('isc_rollback_retaining'           );
    fisc_service_attach         := GetProcAddr('isc_service_attach'               );
    fisc_service_detach         := GetProcAddr('isc_service_detach'               );
    fisc_service_query          := GetProcAddr('isc_service_query'                );
    fisc_service_start          := GetProcAddr('isc_service_start'                );

  fisc_get_client_version       := nil;
  fisc_get_client_major_version := nil;
  fisc_get_client_minor_version := nil;

  if Assigned(fisc_rollback_retaining) then
    fClientLibVersion          := fxsv_fb else
    fClientLibVersion          := fxsv_ibPre6;
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.ClearLastErrorMsg;
Begin
  fLastErrorContext   :='';
  fLastSQLErrorCode   := 0;
  fLastSQLErrorMessage:='';
  fLastErrorCode      := 0;
  fLastErrorMessage   :='';
End;
{______________________________________________________________________________}
procedure TFXClientLibrary.ReadFirebirdError(Const Sender:TObject;Const aContext:String);
var local_buffer: array[0..2048] of AnsiChar;
    SQLErrorCodeMsg,uMsg:String;
    lpszSrc: PAnsiChar;
    Msg:AnsiString;
    s:PStatusVector;
    MsgLen:Integer;
Begin
  ClearLastErrorMsg;
  fLastErrorContext:=aContext;
  fLastErrorCode:=fStatusVector[1];
  lpszSrc:=@(local_buffer[0]);

  if Assigned(fisc_sqlcode) then
    fLastSQLErrorCode:=fisc_sqlcode(@fStatusVector);

  if (fLastSQLErrorCode<>0) then Begin
    if Assigned(fisc_sql_interprete) Then Begin
      MsgLen:=0;
      fisc_sql_interprete(fLastSQLErrorCode, local_buffer, SizeOf(local_buffer));
      While (MsgLen<SizeOf(local_buffer))and(not CharInSet(local_buffer[MsgLen],[#0,#10,#13,'.'])) do
        Inc(MsgLen);
      if MsgLen>0 then Begin
        SetLength(Msg, MsgLen);
        AnsiToOemBuff(lpszSrc, PAnsiChar(Msg), MsgLen);
        uMsg:=String(Msg);
        fLastSQLErrorMessage:=uMsg
      End else
        fLastSQLErrorMessage:=EmptyStr;
    end end;

  if Assigned(fisc_interprete) then Begin
    s:=@fStatusVector;
    SQLErrorCodeMsg:='SQL error code = '+IntToStr(fLastSQLErrorCode);
    MsgLen:=fisc_interprete(local_buffer,SizeOf(local_buffer),@s);
    While MsgLen > 0 do begin
      SetLength(Msg, MsgLen);
      AnsiToOemBuff(Pointer(@local_buffer), PAnsiChar(Msg), MsgLen);
      uMsg:=String(Msg);
      if Not SameText(SQLErrorCodeMsg,uMsg) then Begin
        if fLastErrorMessage<>'' then
          fLastErrorMessage:=fLastErrorMessage+sLinebreak+uMsg else
          fLastErrorMessage:=uMsg;
        end;
      MsgLen:=fisc_interprete(local_buffer,SizeOf(local_buffer),@s);
      end;
  end else
    fLastErrorMessage:='Can not interpret Firebird Error !';

  MsgLen:=Length(fLastErrorMessage);
  While (MsgLen>0)and(CharInSet(fLastErrorMessage[MsgLen],[#10,#13,'.'])) do
    Dec(MsgLen);
  SetLength(fLastErrorMessage,MsgLen);
End;
{______________________________________________________________________________}
procedure TFXClientLibrary.ReadFirebirdError(Const Sender:TObject);
Begin
  ReadFirebirdError(Sender,EmptyStr);
End;
{______________________________________________________________________________}
function TFXClientLibrary.LastFirebirdErrorMsg(Const Sender:TObject):String;
Var c:TComponent;
Begin
  if Sender<>nil then Begin
    if Sender is TComponent then Begin
      c:=TComponent(Sender);
      if c.Name<>EmptyStr then
        Result:=c.ClassName+':'+c.Name else
        Result:=c.ClassName
    End else
      Result:=Sender.ClassName;
    if fLastErrorContext<>EmptyStr Then
      Result:=Result+sLinebreak+fLastErrorContext+sLinebreak+sLinebreak+fLastErrorMessage else
      Result:=Result+sLinebreak+fLastErrorMessage;
  end else
  if fLastErrorContext<>EmptyStr Then
    Result:=fLastErrorContext+sLinebreak+sLinebreak+fLastErrorMessage else
    Result:=fLastErrorMessage;

  Case fLastSQLErrorCode of
    0:Begin
      // No SQL ErrorCode
      end;
   -902:Begin
      case fLastErrorCode of
        isc_network_error,
        isc_io_error:Begin
          // Useless SQL Error Code Msg
          end;
        else Begin
          Result:=Result+sLinebreak+sLinebreak+'SQL error code = '+IntToStr(fLastSQLErrorCode)+sLinebreak+fLastSQLErrorMessage;
      end end end;
    else Begin
      Result:=Result+sLinebreak+sLinebreak+'SQL error code = '+IntToStr(fLastSQLErrorCode)+sLinebreak+fLastSQLErrorMessage;
    end end;

  if fLastErrorCode<>0 then Begin
    Result:=Result+sLinebreak+sLinebreak+'Error code = '+IntToStr(fLastErrorCode);
    end;

  ClearLastErrorMsg;
End;
{______________________________________________________________________________}
function TFXClientLibrary.LastFirebirdError(Const Sender:TObject):EFXError;
Begin
  Case fLastSQLErrorCode of
       0:Result:=EFXFirebirdError    .Create(Sender,fLastErrorContext,fLastErrorCode,fLastErrorMessage);
    -551:Result:=EFXFirebirdRoleError.Create(Sender,fLastErrorContext,fLastSQLErrorCode,fLastSQLErrorMessage,fLastErrorCode,fLastErrorMessage);
    else Result:=EFXFirebirdError    .Create(Sender,fLastErrorContext,fLastSQLErrorCode,fLastSQLErrorMessage,fLastErrorCode,fLastErrorMessage);
    end;
  ClearLastErrorMsg;
End;
{______________________________________________________________________________}
procedure TFXClientLibrary.RaiseLastFirebirdError(Const Sender:TObject);
Begin
  raise LastFirebirdError(Sender);
End;
{______________________________________________________________________________}
procedure TFXClientLibrary.ReadAndRaiseFirebirdError(Const Sender:TObject);
Begin
  ReadFirebirdError(Sender);
  raise LastFirebirdError(Sender);
End;
{______________________________________________________________________________}
procedure TFXClientLibrary.ReadAndRaiseFirebirdError(Const Sender:TObject;Const aContext:String);
Begin
  ReadFirebirdError(Sender,aContext);
  raise LastFirebirdError(Sender);
End;
{______________________________________________________________________________}
function TFXClientLibrary.CheckStatusVector(Const ErrorCodes: array of ISC_STATUS): Boolean;
var p: PISC_STATUS;
    i: Integer;
  procedure NextP(i: Integer);
  begin
  p := PISC_STATUS(PAnsiChar(p) + (i * SizeOf(ISC_STATUS)));
  end;
begin
  result := False;
  p := @FStatusVector;
  while (p^ <> 0) and (not result) do
    case p^ of
      3: NextP(3);
      1, 4:begin
        NextP(1);
        i := 0;
        while (i <= High(ErrorCodes)) and (not result) do begin
          result := p^ = ErrorCodes[i];
          Inc(i);
          end;
        NextP(1);
        end;
      else
        NextP(2);
    end;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXClientLibrary.Call_attach_database(Const db_name:String;Const db_handle:PISC_DB_HANDLE;Const parm_buffer_length:FXShort;Const parm_buffer:PFXByte): ISC_STATUS;
Var sDBN:AnsiString;
    lDBN:FXShort;
Begin
  sDBN:=AnsiString(db_name);
  lDBN:=length(sDBN);

  Result:=fisc_attach_database( @fStatusVector,
                                   lDBN,
                                   PAnsiChar(sDBN),
                                   db_handle,
                                   parm_buffer_length,
                                   parm_buffer
                                   )
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_database_info(Const db_handle:PISC_DB_HANDLE;Const item_list_buffer_length:FXShort;Const item_list_buffer:PFXByte;Const result_buffer_length:FXShort;Const result_buffer:PFXByte): ISC_STATUS;
Begin
  Result:=fisc_database_info(   @fStatusVector,
                                   db_handle,
                                   item_list_buffer_length,
                                   item_list_buffer,
                                   result_buffer_length,
                                   result_buffer
                                   )
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_database_info(Const Sender:TObject;Const db_handle:PISC_DB_HANDLE;Const item_list_buffer_length:FXShort;Const item_list_buffer:PFXByte;Const result_buffer_length:FXShort;Const result_buffer:PFXByte);
Var Err:ISC_STATUS;
Begin
  Err:=fisc_database_info(   @fStatusVector,
                               db_handle,
                               item_list_buffer_length,
                               item_list_buffer,
                               result_buffer_length,
                               result_buffer
                               );

  if Err>0 Then
    ReadAndRaiseFirebirdError(Sender);

end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_detach_database(Const db_handle:PISC_DB_HANDLE):ISC_STATUS;
Begin
  Result:=fisc_detach_database( @fStatusVector,
                                   db_handle
                              )
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_drop_database(Const db_handle:PISC_DB_HANDLE);
Var Err:ISC_STATUS;
Begin
  Err:=fisc_drop_database(  @fStatusVector,
                               db_handle
                               );

  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXClientLibrary.Call_start_multiple(Const tran_handle:PISC_TR_HANDLE;Const db_handle_count:FXShort;Const teb_vector_address:PISC_TEB):ISC_STATUS;
Begin
  Result:=fisc_start_multiple(  @fStatusVector,
                                       tran_handle,
                                       db_handle_count,
                                       teb_vector_address
                                       )
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_commit_retaining(Const tran_handle:PISC_TR_HANDLE);
Var Err:ISC_STATUS;
Begin
  Err:=fisc_commit_retaining(  @fStatusVector,
                               tran_handle
                            );

  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_commit_retaining(Const tran_handle:PISC_TR_HANDLE):ISC_STATUS;
Begin
  Result:=fisc_commit_retaining(  @fStatusVector,
                                   tran_handle
                               )
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_commit_transaction(Const tran_handle:PISC_TR_HANDLE):ISC_STATUS;
Begin
  Result:=fisc_commit_transaction(  @fStatusVector,
                                     tran_handle
                                 )
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_rollback_retaining(Const tran_handle:PISC_TR_HANDLE);
Var Err:ISC_STATUS;
Begin
  Err:=fisc_rollback_retaining(  @fStatusVector,
                               tran_handle
                              );

  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_rollback_retaining(Const tran_handle:PISC_TR_HANDLE):ISC_STATUS;
Begin
  Result:=fisc_rollback_retaining(  @fStatusVector,
                                   tran_handle
                                   )
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_rollback_transaction(Const tran_handle:PISC_TR_HANDLE):ISC_STATUS;
Begin
  Result:=fisc_rollback_transaction(  @fStatusVector,
                                   tran_handle
                                   )
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_dsql_alloc_statement2(Const db_handle:PISC_DB_HANDLE;Const stmt_handle:PISC_STMT_HANDLE);
Var err:ISC_STATUS;
Begin
  Err:=fisc_dsql_alloc_statement2(  @fStatusVector,
                                     db_handle,
                                     stmt_handle
                                 );
  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_dsql_alloc_statement2(Const db_handle:PISC_DB_HANDLE;Const stmt_handle:PISC_STMT_HANDLE):ISC_STATUS;
Begin
  Result:=fisc_dsql_alloc_statement2(  @fStatusVector,
                                        db_handle,
                                        stmt_handle
                                    );
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_dsql_prepare(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const statement:TBytes);
Var err:ISC_STATUS;
    len:FXUShort;
    i:NativeInt;
Begin
  i:=length(statement);
  if i>=High(len) then
    len:=0 else
    len:=i;

  Err:=fisc_dsql_prepare(  @fStatusVector,
                            tran_handle,
                            stmt_handle,
                            len,
                            PFXByte(statement),
                            mFX_Dialect,
                            nil
                        );

  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_dsql_prepare(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const statement:TBytes):ISC_STATUS;
Var len:FXUShort;
    i:NativeInt;
Begin
  i:=length(statement);
  if i>=High(len) then
    len:=0 else
    len:=i;

  Result:=fisc_dsql_prepare(    @fStatusVector,
                                 tran_handle,
                                 stmt_handle,
                                 len,
                                 PFXByte(statement),
                                 mFX_Dialect,
                                 nil
                           );
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_dsql_sql_info(Const stmt_handle:PISC_STMT_HANDLE;Const item_length:FXShort;Const items:PFXByte;Const buffer_length:FXShort;Const buffer:PFXByte);
Var err:ISC_STATUS;
Begin
  Err:=fisc_dsql_sql_info(  @fStatusVector,
                               stmt_handle,
                               item_length,
                               items,
                               buffer_length,
                               buffer
                               );

  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_dsql_sql_info(Const stmt_handle:PISC_STMT_HANDLE;Const item_length:FXShort;Const items:PFXByte;Const buffer_length:FXShort;Const buffer:PFXByte):ISC_STATUS;
Begin
  result:=fisc_dsql_sql_info(  @fStatusVector,
                               stmt_handle,
                               item_length,
                               items,
                               buffer_length,
                               buffer
                               );
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_dsql_describe(Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA);
Var err:ISC_STATUS;
Begin
  Err:=fisc_dsql_describe(  @fStatusVector,
                             stmt_handle,
                             mFX_Dialect,
                             xsqlda
                         );

  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_dsql_describe(Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA):ISC_STATUS;
Begin
  Result:=fisc_dsql_describe(  @fStatusVector,
                               stmt_handle,
                               mFX_Dialect,
                               xsqlda
                               );
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_dsql_describe_bind(Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA);
Var err:ISC_STATUS;
Begin
  Err:=fisc_dsql_describe_bind(  @fStatusVector,
                                  stmt_handle,
                                  mFX_Dialect,
                                  xsqlda
                               );
  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_dsql_describe_bind(Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA):ISC_STATUS;
Begin
  Result:=fisc_dsql_describe_bind(  @fStatusVector,
                                     stmt_handle,
                                     mFX_Dialect,
                                     xsqlda
                                  );
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_dsql_execute(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA);
Var err:ISC_STATUS;
Begin
  Err:=fisc_dsql_execute(  @fStatusVector,
                            tran_handle,
                            stmt_handle,
                            mFX_Dialect,
                            xsqlda
                        );

  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_dsql_execute(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA):ISC_STATUS;
Begin
  Result:=fisc_dsql_execute(  @fStatusVector,
                            tran_handle,
                            stmt_handle,
                            mFX_Dialect,
                            xsqlda
                        );
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_dsql_execute2(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const in_xsqlda:PXSQLDA);
Var err:ISC_STATUS;
Begin
  Err:=fisc_dsql_execute2(  @fStatusVector,
                             tran_handle,
                             stmt_handle,
                             mFX_Dialect,
                             in_xsqlda,
                             nil
                         );

  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_dsql_execute2(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const in_xsqlda,out_xsqlda:PXSQLDA);
Var err:ISC_STATUS;
Begin
  Err:=fisc_dsql_execute2(  @fStatusVector,
                               tran_handle,
                               stmt_handle,
                               mFX_Dialect,
                               in_xsqlda,
                               out_xsqlda
                               );

  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_dsql_execute2(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const in_xsqlda:PXSQLDA):ISC_STATUS;
Begin
        Result:=fisc_dsql_execute2(   @fStatusVector,
                                       tran_handle,
                                       stmt_handle,
                                       mFX_Dialect,
                                       in_xsqlda,
                                       nil
                                       )
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_dsql_execute2(Const tran_handle:PISC_TR_HANDLE;Const stmt_handle:PISC_STMT_HANDLE;Const in_xsqlda,out_xsqlda:PXSQLDA):ISC_STATUS;
Begin
  Result:=fisc_dsql_execute2(   @fStatusVector,
                                       tran_handle,
                                       stmt_handle,
                                       mFX_Dialect,
                                       in_xsqlda,
                                       out_xsqlda
                                       )
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_dsql_set_cursor_name(Const stmt_handle:PISC_STMT_HANDLE;Const cursor_name:PAnsiChar);
Var err:ISC_STATUS;
Begin
  Err:=fisc_dsql_set_cursor_name(  @fStatusVector,
                               stmt_handle,
                               cursor_name,
                               0
                               );

  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_dsql_free_statement(stmt_handle:PISC_STMT_HANDLE;options:FXUShort):ISC_STATUS;
Begin
  Result:=fisc_dsql_free_statement(   @fStatusVector,
                                   stmt_handle,
                                   options
                                   )
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_dsql_fetch(Const stmt_handle:PISC_STMT_HANDLE;Const xsqlda:PXSQLDA):ISC_STATUS;
Begin
  Result:=fisc_dsql_fetch(   @fStatusVector,
                                   stmt_handle,
                                   mFX_Dialect,
                                   xsqlda
                                   )
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_dsql_execute_immediate(Const db_handle:PISC_DB_HANDLE;Const tran_handle:PISC_TR_HANDLE;Const length:FXUShort;Const statement:PAnsiChar);
Var Err:ISC_STATUS;
Begin
  Err:=fisc_dsql_execute_immediate(   @fStatusVector,
                               db_handle,
                               tran_handle,
                               length,
                               statement,
                               mFX_Dialect,
                               nil
                               );

  if Err>0 Then
    ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_BlobInfo(Const hBlobHandle: PISC_BLOB_HANDLE; var NumSegments,MaxSegmentSize,BlobSize:FXLong;var BlobType: FXShort);
var results: array[0..99] of Byte;
    items: array[0..3] of Byte;
    item_length:FXShort;
    item: Byte;
    i:Integer;
begin
  BlobType:=0;
  BlobSize:=0;
  NumSegments:=0;
  MaxSegmentSize:=FX_DefaultBlobSegmentSize;

  Assert(hBlobHandle^<>nil);
  Assert(Assigned(fisc_blob_info));

  items[0] := isc_info_blob_num_segments;
  items[1] := isc_info_blob_max_segment;
  items[2] := isc_info_blob_total_length;
  items[3] := isc_info_blob_type;

  if fisc_blob_info(@fStatusVector, hBlobHandle, SizeOf(items), @items[0], SizeOf(results),@results[0]) > 0 then
    ReadAndRaiseFirebirdError(Self);

  i := 0;
  while (i < SizeOf(results)) and (results[i] <> isc_info_end) do begin
    item := results[i];
    Inc(i);
    item_length:=vax_short(@results,i,2);
    Inc(i, 2);
    case item of
      isc_info_blob_num_segments:
        NumSegments := vax_integer(@results,i,item_length);
      isc_info_blob_max_segment:
        MaxSegmentSize := vax_integer(@results,i,item_length);
      isc_info_blob_total_length:
        BlobSize:= vax_integer(@results,i,item_length);
      isc_info_blob_type:
        BlobType:= vax_integer(@results,i,item_length);
      end;
    Inc(i, item_length);
  end;
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_BlobInfo(Const hBlobHandle: PISC_BLOB_HANDLE; var NumSegments,MaxSegmentSize,BlobSize:FXLong;var BlobType: FXShort):ISC_STATUS;
var results: array[0..99] of Byte;
    items: array[0..3] of Byte;
    item_length:FXShort;
    item: Byte;
    i:Integer;
begin
  BlobType:=0;
  BlobSize:=0;
  NumSegments:=0;
  MaxSegmentSize:=FX_DefaultBlobSegmentSize;

  Assert(hBlobHandle^<>nil);
  Assert(Assigned(fisc_blob_info));

  items[0] := isc_info_blob_num_segments;
  items[1] := isc_info_blob_max_segment;
  items[2] := isc_info_blob_total_length;
  items[3] := isc_info_blob_type;

  Result:=fisc_blob_info(@fStatusVector, hBlobHandle, SizeOf(items), @items[0], SizeOf(results),@results[0]);
  if Result<=0 then Begin
    i := 0;
    while (i < SizeOf(results)) and (results[i] <> isc_info_end) do begin
      item := results[i];
      Inc(i);
      item_length:=vax_short(@results,i,2);
      Inc(i, 2);
      case item of
        isc_info_blob_num_segments:
          NumSegments := vax_integer(@results,i,item_length);
        isc_info_blob_max_segment:
          MaxSegmentSize := vax_integer(@results,i,item_length);
        isc_info_blob_total_length:
          BlobSize:= vax_integer(@results,i,item_length);
        isc_info_blob_type:
          BlobType:= vax_integer(@results,i,item_length);
        end;
      Inc(i, item_length);
    end end;
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_ReadBlob(Const hBlobHandle: PISC_BLOB_HANDLE;Const aBuffer: PFXByte;Const aNumSegments,aMaxSegmentSize,aBlobSize: FXLong);
var StatusVectorArray:PStatusVector;
    BytesRead, SegLen: FXUShort;
    Err:ISC_STATUS;
    CurPos:FXLong;
    pb:PFXByte;
begin
  CurPos:=0;
  pb:=aBuffer;
  SegLen:=aMaxSegmentSize;
  Assert(hBlobHandle^<>nil);
  Assert(aMaxSegmentSize<>0);
  StatusVectorArray:=@fStatusVector;
  while (CurPos < aBlobSize) do begin
    if (CurPos + SegLen > aBlobSize) then
      SegLen := aBlobSize - CurPos;
    Err:=fisc_get_segment(@fStatusVector,hBlobHandle,@BytesRead,SegLen,pb);
    if (Err<>0)or(StatusVectorArray[1]=isc_segment) Then
      ReadAndRaiseFirebirdError(Self);
    Inc(CurPos,BytesRead);
    Inc(pb,BytesRead);
    end
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_ReadBlob(Const hBlobHandle: PISC_BLOB_HANDLE;Const aBuffer: PFXByte;Const aNumSegments,aMaxSegmentSize,aBlobSize: FXLong):Boolean;
var StatusVectorArray:PStatusVector;
    BytesRead, SegLen: FXUShort;
    Err:ISC_STATUS;
    CurPos:FXLong;
    pb:PFXByte;
begin
  CurPos:=0;
  pb:=aBuffer;
  SegLen:=aMaxSegmentSize;
  Assert(hBlobHandle^<>nil);
  Assert(aMaxSegmentSize<>0);
  StatusVectorArray:=@fStatusVector;
  while (CurPos < aBlobSize) do begin
    if (CurPos + SegLen > aBlobSize) then
      SegLen := aBlobSize - CurPos;
    Err:=fisc_get_segment(@fStatusVector,hBlobHandle,@BytesRead,SegLen,pb);
    if (Err>0)or(StatusVectorArray[1]=isc_segment) Then Begin
      Result:=False;
      exit;
      end;
    Inc(CurPos,BytesRead);
    Inc(pb,BytesRead);
    end;
  Result:=True;
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.WriteBlob(Const hBlobHandle: PISC_BLOB_HANDLE;Const aBuffer: PFXByte;Const aBlobSize:FXLong);
var SegLen:FXUShort;
    pb:PFXByte;
    CurPos:FXLong;
begin
  CurPos := 0;
  pb:=aBuffer;
  Assert(hBlobHandle^<>nil);
  SegLen := FX_DefaultBlobSegmentSize;
  while (CurPos < aBlobSize) do begin
    if (CurPos + SegLen > aBlobSize) then
      SegLen := aBlobSize - CurPos;
    if fisc_put_segment(@fStatusVector,hBlobHandle,SegLen,pb) > 0 then
      ReadAndRaiseFirebirdError(Self);
    Inc(CurPos, SegLen);
    Inc(pb,SegLen);
    end
end;
{______________________________________________________________________________}
procedure TFXClientLibrary.Check_close_blob(Const hBlob_Handle : PISC_BLOB_HANDLE);
Var err:ISC_STATUS;
Begin
  Assert(hBlob_Handle^<>nil);
  err:=fisc_close_blob(@fStatusVector,hBlob_Handle);

  if (err>0) then
    ReadAndRaiseFirebirdError(Self);
  Assert(hBlob_Handle^=nil);
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_close_blob(Const hBlob_Handle : PISC_BLOB_HANDLE): ISC_STATUS;
Begin
  Result:=fisc_close_blob(@fStatusVector,hBlob_Handle);

  Assert(hBlob_Handle^=nil);
end;
{______________________________________________________________________________}
Procedure TFXClientLibrary.Check_create_blob2(Const db_handle: PISC_DB_HANDLE;Const tr_handle:PISC_TR_HANDLE;Const hBlob_Handle : PISC_BLOB_HANDLE;Const blob_id : PISC_QUAD);
Var err:ISC_STATUS;
Begin
  Assert(hBlob_Handle^=nil);

  err:=fisc_create_blob2(       @fStatusVector,
                                   db_handle,
                                   tr_handle,
                                   hBlob_Handle,
                                   blob_id,
                                   0,
                                   nil
                                   );

  if err> 0 then
    ReadAndRaiseFirebirdError(Self);
  Assert(hBlob_Handle^<>nil);
end;
{______________________________________________________________________________}
Procedure TFXClientLibrary.Check_open_blob2(Const db_handle: PISC_DB_HANDLE;Const tr_handle:PISC_TR_HANDLE;Const hBlob_Handle : PISC_BLOB_HANDLE;Const blob_id : PISC_QUAD);
Var err:ISC_STATUS;
Begin
  Assert(hBlob_Handle^=nil);

  err:=fisc_open_blob2( @fStatusVector,
                               db_handle,
                               tr_handle,
                               hBlob_Handle,
                               blob_id,
                               0,
                               nil
                               );

  if err> 0 then
    ReadAndRaiseFirebirdError(Self);

  Assert(hBlob_Handle^<>nil);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXClientLibrary.Call_service_detach(Const service_handle:PISC_SVC_HANDLE): ISC_STATUS;
Begin
  result:=fisc_service_detach(@fStatusVector,
                               service_handle
                               );
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_service_attach(Const isc_arg2:FXUShort;Const isc_arg3:PAnsiChar;Const service_handle: PISC_SVC_HANDLE;Const isc_arg5:FXUShort;Const isc_arg6:PFXByte): ISC_STATUS;
Begin
  result:=fisc_service_attach(@fStatusVector,
                               isc_arg2,
                               isc_arg3,
                               service_handle,
                               isc_arg5,
                               isc_arg6
                               );
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_service_start(Const service_handle: PISC_SVC_HANDLE;Const recv_handle:PISC_SVC_HANDLE;Const isc_arg4:FXUShort;Const isc_arg5:PFXByte):ISC_STATUS;
Begin
  result:=fisc_service_start(@fStatusVector,
                               service_handle,
                               recv_handle,
                               isc_arg4,
                               isc_arg5
                               );
end;
{______________________________________________________________________________}
function TFXClientLibrary.Call_service_query(Const service_handle: PISC_SVC_HANDLE;Const recv_handle:PISC_SVC_HANDLE;Const isc_arg4:FXUShort;Const isc_arg5:PFXByte;Const isc_arg6:FXUShort;Const isc_arg7:PFXByte;Const isc_arg8:FXUShort;Const isc_arg9:PFXByte): ISC_STATUS;
Begin
  result:=fisc_service_query(@fStatusVector,
                               service_handle,
                               recv_handle,
                               isc_arg4,
                               isc_arg5,
                               isc_arg6,
                               isc_arg7,
                               isc_arg8,
                               isc_arg9
                               );
end;

end.

