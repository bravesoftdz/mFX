unit mFX.Classes;

interface

{$I mFX.Inc}

uses System.SysUtils, System.Classes, System.StrUtils, System.Variants,
  mFX.ErrorCodes, mFX.Header, mFX.Consts;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  TStatusVector = array[0..19] of ISC_STATUS;
  PStatusVector = ^TStatusVector;

  TFXConnectResult = (
    fxConnectError,
    fxNotConnected,
    fxConnectEstablished,
    fxConnectionTestOK,
    fxConnectionLost
    );

  TFXTransactionAction = (
    fxTARollback,
    fxTACommit,
    fxTARollbackRetaining,
    fxTACommitRetaining
    );

  /// <summary>TFXSQL do not change Order !!!!</summary>
  TFXQueryType = (
    fxSQLUnknown,

    fxSQLSelectSelect,
    fxSQLInsert,
    fxSQLUpdate,
    fxSQLDelete,
    fxSQLDDLStatement,
    fxSQLGetSegment,
    fxSQLPutSegment,
    fxSQLExecProcedure,
    fxSQLStartTransaction,
    fxSQLCommit,
    fxSQLRollback,
    fxSQLSelectForUpdate,
    fxSQLSetGenerator,

    fxSQLInsertSelect, fxSQLUpdateSelect, fxSQLDeleteSelect,
    fxSQLDDLComment
    );

  TFXClientErrorCode = (
    fxceUnknownException,
    fxceFirebirdMissing,
    fxceAliasNotLocal,
    fxceIB60feature,
    fxceNotSupported,
    fxceNotPermitted,
    fxceDPBConstantUnknown,
    fxceDPBConstantInvalidValue,
    fxceTPBConstantUnknown,
    fxceTPBConstantInvalidValue,
    fxceDatabaseNotAssigned,
    fxceDatabaseClosed,
    fxceDatabaseOpen,
    fxceDatabaseNameMissing,
    fxceTransactionNotAssigned,
    fxceNotInTransaction,
    fxceInTransaction,
    fxceDBNotInTransactionDBs,
    fxceNoDatabasesInTransaction,
    fxceXSQLDAIndexOutOfRange,
    fxceInvalidStatementHandle,
    fxceSQLOpen,
    fxceSQLClosed,
    fxceDatasetOpen,
    fxceUnsupportedQueryType,
    fxceUnknownSQLDataType,
    fxceInvalidDataConversion,
    fxceBlobCannotBeRead,
    fxceBlobCannotBeWritten,
    fxceBlobNotIntialized,
    fxceEmptyQuery,
    fxceFieldNotFound,
    fxceFieldSizeMismatch,
    fxceStringTooLarge,
    fxceSQLParseError,
    fxceUserAbort,
    fxceCantEndSharedTransaction,
    fxceEmptySQLStatement,
    fxceIsASelectStatement,
    fxceNoTableName,
    fxceInvalidEvents,
    fxceEventAlreadyRegistered,
    fxceNoEventsRegistered,
    fxceSQLDialectInvalid,
    fxceSPBConstantNotSupported,
    fxceSPBConstantUnknown,
    fxceServiceActive,
    fxceServiceInActive,
    fxceQueryParamsError,
    fxceStartParamsError,
    fxceOutputParsingError,
    fxceUseSpecificProcedures,
    fxceEOFInComment,
    fxceEOFInString,
    fxceParamNameExpected,
    fxceUnknownPlan
    );

  EFXError = class(Exception)
  private
    fContext      : String;
    fSQLErrorCode : FXLong;
    fSQLErrorMsg  : String;
    fErrorCode    : FXLong;
    fErrorMsg     : String;
    fClientError  : TFXClientErrorCode;
  public
    property Context       : String             read fContext;
    property ClientError   : TFXClientErrorCode read fClientError;
    property SQLErrorCode  : FXLong             read fSQLErrorCode;
    property SQLErrorMsg   : String             read fSQLErrorMsg;
    property ErrorCode     : FXLong             read fErrorCode;
    property ErrorMsg      : String             read fErrorMsg;
  end;

  /// <summary>EFXFirebirdError</summary>
  EFXClientError = class(EFXError)
  public
    /// <summary>constructor </summary>
    constructor Create(Const Sender:TObject;Const aClientError:TFXClientErrorCode); overload;
    /// <summary>constructor </summary>
    constructor CreateMsg(Const Sender:TObject;Const aClientError:TFXClientErrorCode;Const Msg:String); overload;
    /// <summary>constructor </summary>
    constructor CreateFmt(Const Sender:TObject;Const aClientError:TFXClientErrorCode;const Args:array of const); overload;
  end;

  /// <summary>EFXFirebirdError</summary>
  EFXFirebirdError = class(EFXError)
  private
    /// <summary>Build Error Message</summary>
    procedure BuildErrorMessage;
  public
    /// <summary>constructor </summary>
    constructor Create(Const Sender:TObject;Const aErrorCode:FXLong;Const aErrorMsg:String); overload;
    /// <summary>constructor </summary>
    constructor Create(Const Sender:TObject;Const aContext:String;Const aErrorCode:FXLong;Const aErrorMsg:String); overload;
    /// <summary>constructor </summary>
    constructor Create(Const Sender:TObject;Const aSQLCode:FXLong;Const aSQLErrorMsg:String;Const aErrorCode:FXLong;Const aErrorMsg:String); overload;
    /// <summary>constructor </summary>
    constructor Create(Const Sender:TObject;Const aContext:String;Const aSQLCode:FXLong;Const aSQLErrorMsg:String;Const aErrorCode:FXLong;Const aErrorMsg:String); overload;
  end;

  /// <summary>EFXFirebirdRoleError</summary>
  EFXFirebirdRoleError = class(EFXFirebirdError)
  end;

/// <summary>Convert QueryType Descr</summary>
function FXQueryType2Descr(Const Value:TFXQueryType):String;overload;

/// <summary>FX Raise ClientError</summary>
function FXClientErrorMsg(Const aClientError:TFXClientErrorCode):String;overload;
/// <summary>FX Raise ClientError</summary>
function FXClientErrorMsg(Const aClientError:TFXClientErrorCode;const Args:array of const):String;overload;

/// <summary>FX Raise ClientError</summary>
procedure FXRaiseClientError(Const Sender:TObject;Const aClientError:TFXClientErrorCode);
/// <summary>FX Raise ClientError</summary>
procedure FXRaiseClientErrorMsg(Const Sender:TObject;Const aClientError:TFXClientErrorCode;Const aMsg:String);
/// <summary>FX Raise ClientError</summary>
procedure FXRaiseClientErrorFmt(Const Sender:TObject;Const aClientError:TFXClientErrorCode;const Args:array of const);

/// <summary>End TR Action Desc</summary>
function EndTRActionDesc(Const Value:TFXTransactionAction):String;

Const
  _FXLocalBufferLength_    =    512;
  _FXBigLocalBufferLength_ =   2048;
  _FXHugeLocalBufferLength_=   8096;

implementation

Const
  _FXErrorMessages_ : array[TFXClientErrorCode] of string = (
    SUnknownException,
    SFirebirdMissing,
    SAliasNotLocal,
    SIB60feature,
    SNotSupported,
    SNotPermitted,
    SDPBConstantUnknown,
    SDPBConstantInvalidValue,
    STPBConstantUnknown,
    STPBConstantInvalidValue,
    SDatabaseNotAssigned,
    SDatabaseClosed,
    SDatabaseOpen,
    SDatabaseNameMissing,
    STransactionNotAssigned,
    SNotInTransaction,
    SInTransaction,
    SDatabaseNotInTransaction,
    SNoDatabasesInTransaction,
    SXSQLDAIndexOutOfRange,
    SInvalidStatementHandle,
    SSQLOpen,
    SSQLClosed,
    SDatasetOpen,
    SUnsupportedQueryType,
    SUnknownSQLDataType,
    SInvalidDataConversion,
    SBlobCannotBeRead,
    SBlobCannotBeWritten,
    SBlobNotInitialized,
    SEmptyQuery,
    SFieldNotFound,
    SFieldSizeMismatch,
    SStringTooLarge,
    SSQLParseError,
    SUserAbort,
    SCantEndSharedTransaction,
    SEmptySQLStatement,
    SIsASelectStatement,
    SNoTableName,
    SInvalidEvents,
    SEventAlreadyRegistered,
    SNoEventsRegistered,
    SSQLDialectInvalid,
    SSPBConstantNotSupported,
    SSPBConstantUnknown,
    SServiceActive,
    SServiceInActive,
    SQueryParamsError,
    SStartParamsError,
    SOutputParsingError,
    SUseSpecificProcedures,
    SEOFInComment,
    SEOFInString,
    SParamNameExpected,
    SUnknownPlan
    );

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function FXQueryType2Descr(Const Value:TFXQueryType):String;
Begin
  case Value of
    fxSQLSelectSelect    :Result:='SQL Select';
    fxSQLSelectForUpdate :Result:='SQL Select For Update';
    fxSQLInsert          :Result:='SQL Insert';
    fxSQLInsertSelect    :Result:='SQL Insert Returning';
    fxSQLUpdate          :Result:='SQL Update';
    fxSQLUpdateSelect    :Result:='SQL Update Returning';
    fxSQLDelete          :Result:='SQL Delete';
    fxSQLDeleteSelect    :Result:='SQL Delete Returning';
    fxSQLSetGenerator    :Result:='SQL SetGenerator';
    fxSQLDDLStatement    :Result:='SQL DDL Statement';
    fxSQLDDLComment      :Result:='SQL DDL Comment';
    fxSQLGetSegment      :Result:='SQL GetSegment';
    fxSQLPutSegment      :Result:='SQL PutSegment';
    fxSQLExecProcedure   :Result:='SQL Exec Stored Procedure';
    fxSQLStartTransaction:Result:='TR Start';
    fxSQLCommit          :Result:='TR Commit';
    fxSQLRollback        :Result:='TR Rollback';
    else                  Result:='Unknown SQL Query';
    end;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function EndTRActionDesc(Const Value:TFXTransactionAction):String;
Begin
  case Value of
    fxTARollbackRetaining:Result:='Rollback Retain';
    fxTARollback         :Result:='Rollback';
    fxTACommitRetaining  :Result:='Commit Retain';
    else                  Result:='Commit';
    end;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor EFXFirebirdError.Create(Const Sender:TObject;Const aErrorCode:FXLong;Const aErrorMsg:String);
Var c:TComponent;
begin
  if Sender<>nil then Begin
    if Sender is TComponent then Begin
      c:=TComponent(Sender);
      if c.Name<>EmptyStr then
        fContext:=c.ClassName+':'+c.Name else
        fContext:=c.ClassName
    End else
      fContext:=Sender.ClassName
    end;
  fErrorCode    := aErrorCode;
  fErrorMsg     := aErrorMsg;
  BuildErrorMessage;
end;
{______________________________________________________________________________}
constructor EFXFirebirdError.Create(Const Sender:TObject;Const aContext:String;Const aErrorCode:FXLong;Const aErrorMsg:String);
Var c:TComponent;
begin
  if Sender<>nil then Begin
    if Sender is TComponent then Begin
      c:=TComponent(Sender);
      if c.Name<>EmptyStr then
        fContext:=c.ClassName+':'+c.Name else
        fContext:=c.ClassName
    End else
      fContext:=Sender.ClassName
    end;
  if aContext<>'' then
    if fContext<>'' then
      fContext:=fContext+sLinebreak+aContext else
      fContext:=aContext;
  fErrorCode    := aErrorCode;
  fErrorMsg     := aErrorMsg;
  BuildErrorMessage;
end;
{______________________________________________________________________________}
constructor EFXFirebirdError.Create(Const Sender:TObject;Const aSQLCode:FXLong;Const aSQLErrorMsg:String;Const aErrorCode:FXLong;Const aErrorMsg:String);
Var c:TComponent;
begin
  if Sender<>nil then Begin
    if Sender is TComponent then Begin
      c:=TComponent(Sender);
      if c.Name<>EmptyStr then
        fContext:=c.ClassName+':'+c.Name else
        fContext:=c.ClassName
    End else
      fContext:=Sender.ClassName
    end;
  fSQLErrorCode := aSQLCode;
  fSQLErrorMsg  := aSQLErrorMsg;
  fErrorCode    := aErrorCode;
  fErrorMsg     := aErrorMsg;
  BuildErrorMessage;
end;
{______________________________________________________________________________}
constructor EFXFirebirdError.Create(Const Sender:TObject;Const aContext:String;Const aSQLCode:FXLong;Const aSQLErrorMsg:String;Const aErrorCode:FXLong;Const aErrorMsg:String);
Var c:TComponent;
begin
  if Sender<>nil then Begin
    if Sender is TComponent then Begin
      c:=TComponent(Sender);
      if c.Name<>EmptyStr then
        fContext:=c.ClassName+':'+c.Name else
        fContext:=c.ClassName
    End else
      fContext:=Sender.ClassName
    end;
  if aContext<>'' then
    if fContext<>'' then
      fContext:=fContext+sLinebreak+aContext else
      fContext:=aContext;
  fSQLErrorCode := aSQLCode;
  fSQLErrorMsg  := aSQLErrorMsg;
  fErrorCode    := aErrorCode;
  fErrorMsg     := aErrorMsg;
  BuildErrorMessage;
end;
{______________________________________________________________________________}
procedure EFXFirebirdError.BuildErrorMessage;
Var Msg:String;
Begin
  if fContext<>EmptyStr Then
    Msg:=fContext+sLinebreak+sLinebreak+fErrorMsg else
    Msg:=fErrorMsg;

  Case fSQLErrorCode of
    0:Begin
      // No SQL ErrorCode
      end;
   -902:Begin
      case fErrorCode of
        isc_network_error,
        isc_io_error:Begin
          // Useless SQL Error Code Msg
          end;
        else Begin
          Msg:=Msg+sLinebreak+sLinebreak+'SQL error code = '+IntToStr(fSQLErrorCode)+sLinebreak+fSQLErrorMsg;
      end end end;
    else Begin
      Msg:=Msg+sLinebreak+sLinebreak+'SQL error code = '+IntToStr(fSQLErrorCode)+sLinebreak+fSQLErrorMsg;
    end end;

  if fErrorCode<>0 then Begin
    Msg:=Msg+sLinebreak+sLinebreak+'Error code = '+IntToStr(fErrorCode);
    end;

  Message:=Msg;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor EFXClientError.Create(Const Sender:TObject;Const aClientError:TFXClientErrorCode);
begin
  fClientError:=aClientError;
  Self.Message:=_FXErrorMessages_[aClientError];
end;
{______________________________________________________________________________}
constructor EFXClientError.CreateMsg(Const Sender:TObject;Const aClientError:TFXClientErrorCode;Const Msg:String);
begin
  fClientError:=aClientError;
  if Msg<>'' then
    Self.Message:=_FXErrorMessages_[aClientError]+sLinebreak+Msg else
    Self.Message:=_FXErrorMessages_[aClientError]
end;
{______________________________________________________________________________}
constructor EFXClientError.CreateFmt(Const Sender:TObject;Const aClientError:TFXClientErrorCode;const Args:array of const);
begin
  fClientError:=aClientError;
  Self.Message:=format(_FXErrorMessages_[aClientError],Args);
end;
{______________________________________________________________________________}
function FXClientErrorMsg(Const aClientError:TFXClientErrorCode):String;
begin
  result:=_FXErrorMessages_[aClientError]
end;
{______________________________________________________________________________}
function FXClientErrorMsg(Const aClientError:TFXClientErrorCode;const Args:array of const):String;
Begin
  result:=format(_FXErrorMessages_[aClientError],Args)
End;
{______________________________________________________________________________}
procedure FXRaiseClientError(Const Sender:TObject;Const aClientError:TFXClientErrorCode);
begin
  raise EFXClientError.Create(Sender,aClientError);
end;
{______________________________________________________________________________}
procedure FXRaiseClientErrorMsg(Const Sender:TObject;Const aClientError:TFXClientErrorCode;Const aMsg:String);
begin
  raise EFXClientError.CreateMsg(Sender,aClientError,aMsg);
end;
{______________________________________________________________________________}
procedure FXRaiseClientErrorFmt(Const Sender:TObject;Const aClientError:TFXClientErrorCode;const Args:array of const);
begin
  raise EFXClientError.CreateFmt(Sender,aClientError,Args);
end;

end.

