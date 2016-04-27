unit mFX.SQLScript;

interface

{$I mFX.Inc}
{$M-}

Uses Types, Classes, SysUtils, DB,
  mFX.Classes, mFX.Header, mFX.Base, mFX.Parser, mFX.SQLScriptParser, mFX.SQL, mFX.Utils, mFX.MetaData;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  TFXCustomScript = Class;
  
  TFXScriptTrace = procedure(Const Sender:TFXCustomScript;Const Msg:String) of object;
  TFXScriptError = procedure(Const Sender:TFXCustomScript;Const e:Exception;Var Ignore:Boolean) of object;
  TFXScriptDependencyError = procedure(Const Sender:TFXCustomScript;Const aMetaObj:TFXCustomMetaNode) of object;

  /// <summary>TFXInternalParser</summary>
  TFXInternalParser = Class(TFXSQLScriptParser)
  protected
    fOwner : TFXCustomScript;
    /// <SUMMARY>ExecCommand</SUMMARY>
    procedure ExecCommentBloc;override;
    /// <SUMMARY>Exec Query</SUMMARY>
    procedure ExecQueryBloc;override;
  End;

  /// <summary>TFXInternalParser</summary>
  TFXInternalPreprocessParser = Class(TFXSQLScriptParser)
  protected
    fOwner : TFXCustomScript;
    /// <SUMMARY>ExecCommand</SUMMARY>
    procedure ExecCommentBloc;override;
    /// <SUMMARY>Exec Query</SUMMARY>
    procedure ExecQueryBloc;override;
  End;

  /// <summary>SQL Query</summary>
  TFXCustomScript = class(TFXCustomSQL)
  protected
    fTerminator      : Char;
    fActualParamCheck: Boolean;
    fAutoCommit      : Boolean;
    fScriptParser    : TFXSQLScriptParser;
    fPreprocessParser: TFXInternalPreprocessParser;
    fDependencyErrors: Integer;
    fErrorCount      : Integer;
    fStatementCount  : Integer;
    fDDLCount        : Integer;
    fDDLError        : Integer;
    fCommitCount     : Integer;
    fSelectCount     : Integer;
    fSelectedRows    : Integer;
    fInsertCount     : Integer;
    fInsertedRows    : Integer;
    fUpdateCount     : Integer;
    fUpdatedRows     : Integer;
    fDeleteCount     : Integer;
    fStoredProcCount : Integer;
    fDeletedRows     : Integer;

    fDoEcho          : Boolean;
    fOnlyEcho        : Boolean;
    fDoTrace         : Boolean;
    fDoTraceResult   : Boolean;
    fStopOnError     : Boolean;

    /// <summary>DDL Ok</summary>
    function GetDDLOk:Integer;
    /// <summary>Init Lines</summary>
    procedure SetLines(Const Value:String);inline;
    /// <summary>Init Lines</summary>
    procedure SetCommand(Const Value:String);inline;
    /// <SUMMARY>Exec Command</SUMMARY>
    procedure ExecCommentBloc;
    /// <SUMMARY>Exec Query</SUMMARY>
    function ExecQueryBloc:Boolean;
  protected
    /// <summary>Echo</summary>
    procedure Echo(Const aMsg:String);virtual;abstract;
    /// <summary>Trace</summary>
    procedure Trace(Const aMsg:String);virtual;abstract;
    /// <summary>Trace Result</summary>
    procedure TraceResult(const Msg:String);overload;virtual;abstract;
    /// <summary>Trace Error</summary>
    function TraceError(Const e:Exception):Boolean;virtual;abstract;

    /// <SUMMARY>Exec Query</SUMMARY>
    procedure ProcessComment;virtual;
    /// <SUMMARY>Exec Query</SUMMARY>
    procedure ProcessSelect;virtual;
    /// <SUMMARY>Exec Query</SUMMARY>
    procedure ProcessNonSelect;virtual;
    /// <SUMMARY>Exec Query</SUMMARY>
    procedure ProcessDDLComment;virtual;
    /// <SUMMARY>Exec Query</SUMMARY>
    procedure ProcessDDL;virtual;
  public
    /// <summary>constructor</summary>
    constructor Create(AOwner: TComponent); override;
    ///<summary>destructor</summary>
    destructor Destroy; override;

    /// <summary>RAZ Counter</summary>
    procedure RAZCounter;inline;
    /// <summary>Run SQL</summary>
    procedure Preprocess(Const Value:String);
    /// <summary>Run SQL</summary>
    procedure RunSQL(Const Value:String;Const DoRAZCounters:Boolean=True);
    /// <summary>RunSQLFile</summary>
    procedure RunSQLStream(Const aStream:TStream;Const aEncoding: TEncoding;Const DoDetectBOM: Boolean = False;Const aBufferSize: Integer = 1024;Const DoRAZCounters:Boolean=True);

    /// <summary>RunSQLFile</summary>
    procedure RunSQLFile(Const aFileName:String;Const DoRAZCounters:Boolean=True);
    /// <summary>RunSQLRes</summary>
    procedure RunSQLRes(Const aInstance:Cardinal;Const aResName:String;Const DoRAZCounters:Boolean=True);

    /// <SUMMARY>Exec SQL</SUMMARY>
    function ExecSQL(Const Value:String;Const DoRAZCounters,DoRestartTr,DoCommit:Boolean):Boolean;

    property StatementCount  : Integer         read fStatementCount;
    property CommitCount     : Integer         read fCommitCount;
    property DDLCount        : Integer         read fDDLCount;
    property DDLOk           : Integer         read GetDDLOk;
    property DDLError        : Integer         read fDDLError;
    property ErrorCount      : Integer         read fErrorCount;
    property DependencyErrors: Integer         read fDependencyErrors;
    property SelectCount     : Integer         read fSelectCount;
    property SelectedRows    : Integer         read fSelectedRows;
    property InsertCount     : Integer         read fInsertCount;
    property InsertedRows    : Integer         read fInsertedRows;
    property UpdateCount     : Integer         read fUpdateCount;
    property UpdatedRows     : Integer         read fUpdatedRows;
    property DeleteCount     : Integer         read fDeleteCount;
    property StoredProcCount : Integer         read fStoredProcCount;
    property DeletedRows     : Integer         read fDeletedRows;

    property Terminator      : Char            read fTerminator    write fTerminator     default ';';
    property AutoCommit      : Boolean         read fAutoCommit    write fAutoCommit;
    property DoEcho          : Boolean         read fDoEcho        write fDoEcho;
    property OnlyEcho        : Boolean         read fOnlyEcho      write fOnlyEcho;
    property DoTrace         : Boolean         read fDoTrace       write fDoTrace;
    property DoTraceResult   : Boolean         read fDoTraceResult write fDoTraceResult;
    property StopOnError     : Boolean         read fStopOnError   write fStopOnError    default True;

  end;

  /// <summary>SQL Query</summary>
  TFXSQLScript = class(TFXCustomScript)
  private
    fOnEcho          : TFXScriptTrace;
    fOnTrace         : TFXScriptTrace;
    fOnTraceResult   : TFXScriptTrace;
    fOnError         : TFXScriptError;

  protected
    /// <summary>Echo</summary>
    procedure Echo(Const aMsg:String);override;
    /// <summary>Trace</summary>
    procedure Trace(Const aMsg:String);override;
    /// <summary>Trace Error</summary>
    function TraceError(Const e:Exception):Boolean;override;
    /// <summary>Trace Result</summary>
    procedure TraceResult(const Msg:String);override;

  published
    property Database;
    property Transaction;
    property SQL;
    property DefaultFieldsCount;
    property ParamCheck;
    property StopOnError;
    property OnSQLChanging;
    property OnSQLChanged;

    property OnEcho          : TFXScriptTrace  read fOnEcho        write fOnEcho;
    property OnTrace         : TFXScriptTrace  read fOnTrace       write fOnTrace;
    property OnTraceResult   : TFXScriptTrace  read fOnTraceResult write fOnTraceResult;
    property OnError         : TFXScriptError  read fOnError       write fOnError;
  end;

implementation

Uses mFX.Consts;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXCustomScript.Create(AOwner: TComponent);
Begin
  inherited;
  fStopOnError:=True;
  fTerminator:=';';
end;
{______________________________________________________________________________}
destructor TFXCustomScript.Destroy;
begin
  fPreprocessParser.Free;
  fScriptParser.Free;
  inherited Destroy;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXCustomScript.RAZCounter;
Begin
  fStatementCount  := 0;
  fCommitCount     := 0;
  fDDLCount        := 0;
  fDDLError        := 0;
  fErrorCount      := 0;
  fDependencyErrors:= 0;
  fSelectCount     := 0;
  fSelectedRows    := 0;
  fInsertCount     := 0;
  fInsertedRows    := 0;
  fUpdateCount     := 0;
  fUpdatedRows     := 0;
  fDeleteCount     := 0;
  fStoredProcCount := 0;
  fDeletedRows     := 0;
end;
{______________________________________________________________________________}
function TFXCustomScript.GetDDLOk:Integer;
Begin
  Result:=fDDLCount-fDDLError
End;
{______________________________________________________________________________}
procedure TFXCustomScript.SetLines(Const Value:String);
Begin
  if fScriptParser=nil then Begin
    fScriptParser:=TFXInternalParser.Create;
    TFXInternalParser(fScriptParser).fOwner:=Self
    End;
  fScriptParser.Init(Value,False,fTerminator);
  FreeAndNil(fPreprocessParser);
End;
{______________________________________________________________________________}
procedure TFXCustomScript.SetCommand(Const Value:String);
Begin
  if fScriptParser=nil then Begin
    fScriptParser:=TFXInternalParser.Create;
    TFXInternalParser(fScriptParser).fOwner:=Self
    End;
  fScriptParser.Init(Value,True,fTerminator);
  FreeAndNil(fPreprocessParser);
End;
{______________________________________________________________________________}
procedure TFXInternalPreprocessParser.ExecCommentBloc;
Begin
  // Ignore
End;
{______________________________________________________________________________}
procedure TFXInternalPreprocessParser.ExecQueryBloc;
Begin
  Inc(fOwner.fStatementCount);
End;
{______________________________________________________________________________}
procedure TFXInternalParser.ExecCommentBloc;
Begin
  fOwner.ExecCommentBloc
End;
{______________________________________________________________________________}
procedure TFXCustomScript.ExecCommentBloc;
Begin
  Self.ProcessComment;
End;
{______________________________________________________________________________}
procedure TFXInternalParser.ExecQueryBloc;
Begin
  if fHasCodeBlock then
    fOwner.fActualParamCheck:=False;
  fOwner.ExecQueryBloc;
End;
{______________________________________________________________________________}
procedure TFXCustomScript.ProcessComment;
Begin
  if Self.fDoEcho then
    Self.Echo(fScriptParser.Command);
End;
{______________________________________________________________________________}
procedure TFXCustomScript.ProcessDDL;
Begin
  Self.ExecQuery;
  if Self.DoTraceResult then
    Self.Trace('-- DDL Done');
End;
{______________________________________________________________________________}
procedure TFXCustomScript.ProcessDDLComment;
Begin
  Self.ExecQuery;
  if Self.DoTraceResult then
    Self.Trace('-- DDL Comment Done');
End;
{______________________________________________________________________________}
procedure TFXCustomScript.ProcessNonSelect;
Begin
  Case Self.QueryType of
    fxSQLInsert:Begin
      Self.ExecQuery;
      if Self.DoTraceResult then
        Self.TraceResult(format('-- %d Record(s) inserted',[Self.RowsAffected]));
      Inc(fInsertedRows,Self.RowsInserted);
      Inc(fUpdatedRows ,Self.RowsUpdated );
      Inc(fDeletedRows ,Self.RowsDeleted );
      end;
    fxSQLUpdate:Begin
      Self.ExecQuery;
      if Self.DoTraceResult then
        Self.TraceResult(format('-- %d Record(s) updated',[Self.RowsAffected]));
      Inc(fInsertedRows,Self.RowsInserted);
      Inc(fUpdatedRows ,Self.RowsUpdated );
      Inc(fDeletedRows ,Self.RowsDeleted );
	  /// <remark> MantisBT Ref:0000332 </remark>
      //Inc(Self.fUpdateCount);
      end;
    fxSQLDelete:Begin
      Self.ExecQuery;
      if Self.DoTraceResult then
        Self.TraceResult(format('-- %d Record(s) deleted',[Self.RowsAffected]));
      Inc(fInsertedRows,Self.RowsInserted);
      Inc(fUpdatedRows ,Self.RowsUpdated );
      Inc(fDeletedRows ,Self.RowsDeleted );
      end;
    end;
End;
{______________________________________________________________________________}
procedure TFXCustomScript.ProcessSelect;
Var j:Integer;
    s:String;
Begin
  Self.ExecQuery;

  if Self.DoTraceResult then Begin
    s:='->';
    for j:=0 to Pred(Self.Current.Count) do
      s:=s+Fields[j].Name+#15' | ';
    Self.TraceResult(s);
    end;

  While Not Self.EOF do Begin
    if Self.DoTraceResult then Begin
      s:='->';
      for j:=0 to Pred(Self.Current.Count) do
        s:=s+Fields[j].AsTrimString+#15' | ';
      Self.TraceResult(s);
      end;
    Inc(fSelectedRows);
    Self.Next;
    end;

  if Self.DoTraceResult then
    Self.TraceResult(format('-- %d Record(s) fetched',[Self.RowsAffected]));
End;
{______________________________________________________________________________}
function TFXCustomScript.ExecQueryBloc:Boolean;
Var SavParamCheck,DidStart:Boolean;
Begin
  Inc(fStatementCount);
  if Self.fDoEcho then
    Self.Echo(fScriptParser.Command);
  Self.SQL.Text:=fScriptParser.Command;
  DidStart:=Self.Start_TR;
  SavParamCheck:=Self.ParamCheck;
  Self.ParamCheck:=Self.fActualParamCheck;
  Self.fActualParamCheck:=SavParamCheck;
  try Self.Prepare;
      Case Self.QueryType of
        fxSQLCommit:Begin
          // Just Commit
          Dec(fStatementCount);
          Inc(fCommitCount);
          Self.CloseQuery;
          Self.Commit_TR;
          end;
        fxSQLInsert:Begin
          Self.ProcessNonSelect;
          Inc(Self.fInsertCount);
          end;
        fxSQLUpdate:Begin
          Self.ProcessNonSelect;
          Inc(Self.fUpdateCount);
          end;
        fxSQLDelete:Begin
          Self.ProcessNonSelect;
          Inc(fDeleteCount);
          end;
        fxSQLExecProcedure:Begin
          Self.ProcessNonSelect;
          Inc(fStoredProcCount);
          end;
        fxSQLInsertSelect, fxSQLUpdateSelect, fxSQLDeleteSelect,
        fxSQLSelectForUpdate,
        fxSQLSelectSelect:begin
          Self.ProcessSelect;
          Inc(fSelectCount);
          end;
        fxSQLDDLComment:Begin
          Self.ProcessDDLComment;
          end;
        fxSQLDDLStatement:begin
          Inc(fDDLCount);
          Inc(fDDLError);
          Self.ProcessDDL;
          Dec(fDDLError);
          end;
        fxSQLSetGenerator:Begin
          Self.ExecQuery;
          if Self.DoTraceResult then
            Self.Trace('-- Generator Set');
          end;
        else Begin
          Self.CloseQuery;
          FXRaiseClientError(Self,fxceNotPermitted);
        end end;
      Self.CloseQuery;
      if fAutoCommit Then
        Self.Commit_TR;
      Self.ParamCheck:=fActualParamCheck;
      Result:=True;
  except on e:Exception do Begin
      Self.ParamCheck:=fActualParamCheck;
      Self.CloseQuery(False);
      Inc(fErrorCount);
      if DidStart Then
        Self.Rollback_TR;
      if TraceError(e) Then
        Raise;
      Result:=False;
  end end
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXCustomScript.RunSQLRes(Const aInstance:Cardinal;Const aResName:String;Const DoRAZCounters:Boolean=True);
Var StrStream:TStringStream;
    Stream:TResourceStream;
Begin
  if DoRAZCounters then
    RAZCounter;

  Stream:=nil;
  StrStream:=nil;
  try Stream:=TResourceStream.Create(aInstance,aResName,RT_RCDATA);
      StrStream:=TStringStream.Create('');
      StrStream.LoadFromStream(Stream);
      SetLines(StrStream.DataString);
  finally
      StrStream.Free;
      Stream.Free;
  end;

  try while not fScriptParser.EOF do
        fScriptParser.Read;
  finally
      fScriptParser.Clear
  end;
end;
{______________________________________________________________________________}
procedure TFXCustomScript.RunSQLStream(Const aStream:TStream;Const aEncoding: TEncoding;Const DoDetectBOM: Boolean = False;Const aBufferSize: Integer = 1024;Const DoRAZCounters:Boolean=True);
Var StrStream:TStreamReader;
Begin
  if DoRAZCounters then
    RAZCounter;

  StrStream:=TStreamReader.Create(aStream,aEncoding,DoDetectBOM,aBufferSize);
  try SetLines(StrStream.ReadToEnd);
  finally
      StrStream.Free;
  end;

  try while not fScriptParser.EOF do
        fScriptParser.Read;
  finally
      fScriptParser.Clear
  end;
end;
{______________________________________________________________________________}
procedure TFXCustomScript.RunSQLFile(Const aFileName:String;Const DoRAZCounters:Boolean=True);
Var StrStream:TStreamReader;
Begin
  if DoRAZCounters then
    RAZCounter;

  StrStream:=TStreamReader.Create(aFileName,True);
  try SetLines(StrStream.ReadToEnd);
  finally
      StrStream.Free;
  end;

  try while not fScriptParser.EOF do
        fScriptParser.Read;
  finally
      fScriptParser.Clear
  end;
end;
{______________________________________________________________________________}
procedure TFXCustomScript.Preprocess(Const Value:String);
Begin
  RAZCounter;
  FreeAndNil(fPreprocessParser);
  fPreprocessParser:=TFXInternalPreprocessParser.Create;
  TFXInternalPreprocessParser(fPreprocessParser).fOwner:=Self;
  fPreprocessParser.Init(Value,False,fTerminator);
  try while not fPreprocessParser.EOF do
        fPreprocessParser.Read;
  finally
      FreeAndNil(fPreprocessParser);
  end;
End;
{______________________________________________________________________________}
procedure TFXCustomScript.RunSQL(Const Value:String;Const DoRAZCounters:Boolean=True);
Begin
  if DoRAZCounters then
    RAZCounter;

  SetLines(Value);
  try while not fScriptParser.EOF do
        fScriptParser.Read;
  finally
      fScriptParser.Clear
  end;
End;
{______________________________________________________________________________}
function TFXCustomScript.ExecSQL(Const Value:String;Const DoRAZCounters,DoRestartTr,DoCommit:Boolean):Boolean;
Begin
  if DoRAZCounters then
    Self.RAZCounter;
    
  if DoRestartTr then
    Self.ReStart_TR else
    Self.Start_TR;

  SetCommand(Value);
  try fActualParamCheck:=Self.ParamCheck;
      result:=Self.ExecQueryBloc;
  finally
      fScriptParser.Clear;
      if DoCommit then
        Self.Commit_TR;
  end
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSQLScript.Echo(Const aMsg:String);
Begin
  if Assigned(fOnEcho) Then
    fOnEcho(Self,aMsg)
end;
{______________________________________________________________________________}
procedure TFXSQLScript.Trace(Const aMsg:String);
Begin
  if Assigned(fOnTrace) Then
    fOnTrace(Self,aMsg)
end;
{______________________________________________________________________________}
procedure TFXSQLScript.TraceResult(const Msg:String);
Begin
  if Assigned(fOnTraceResult) then
    fOnTraceResult(Self,Msg);
end;
{______________________________________________________________________________}
function TFXSQLScript.TraceError(Const e:Exception):Boolean;
Begin
  Result:=fStopOnError;
  if Assigned(fOnError) Then
    fOnError(Self,e,Result)
end;

end.

