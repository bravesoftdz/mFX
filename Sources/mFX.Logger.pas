unit mFX.Logger;

interface

{$I mFX.Inc}

uses System.Types, System.SysUtils;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  /// <summary>Log Action</summary>
  TFXLogAction = (
    fxtError
   ,fxtDBConnect
   ,fxtDBConnecting
     ,fxtDBConnected
     ,fxtDBDisConnect
   ,fxtDBDisConnected
   ,fxtTRStart
     ,fxtTRParams
     ,fxtTRCommit
     ,fxtTRCommitRetain
     ,fxtTRRollback
     ,fxtTRRollbackRetain
     ,fxtTRStartError
   ,fxtSQLPrepare
     ,fxtSQLText
     ,fxtSQLPreProcess
     ,fxtSQLParams
     ,fxtSQLParamsDumpBlob
     ,fxtSQLPlan
     ,fxtSQLExecQuery
     ,fxtSQLRowsAffected
     ,fxtSQLCloseCursor
   ,fxtSQLPrepareError
   ,fxtSQLCloseQuery
   ,fxtOpsProgress
   ,fxtOpsError
   ,fxtInfoMsg
   ,fxtMiscInfo
   ,fxtFetchInfo
   ,fxtBlobInfo
    );
  TFXLogActions = Set of TFXLogAction;

  IFXLogger = interface
  ['{B93FD9A4-2A99-4A3D-AF48-2DCC82037D8F}']

    /// <summary>Get Enabled</summary>
    function GetEnabled:Boolean;
    /// <summary>Get Enabled</summary>
    procedure SetEnabled(Const Value:Boolean);
    /// <summary>Do Log Action</summary>
    function GetDoLogAction(Const Action:TFXLogAction):Boolean;
    /// <summary>Get Enabled</summary>
    procedure SetLogActions(Const Actions:TFXLogActions);

    /// <summary>Log Action</summary>
    Procedure Log(Const Sender:TObject;Const Action:TFXLogAction);overload;
    /// <summary>Log Action</summary>
    Procedure Log(Const Sender:TObject;Const Action:TFXLogAction;Const aMsg: String);overload;
    /// <summary>Log Error</summary>
    Procedure Log(Const Sender:TObject;Const Action:TFXLogAction;Const aContext:String;Const e:Exception);overload;

    property Enabled    : Boolean read GetEnabled write SetEnabled;
    property LogActions : TFXLogActions write SetLogActions;
    property DoLogAction[Const Action:TFXLogAction]: Boolean read GetDoLogAction;
  end;

/// <summary>FX Log Action Description</summary>
function FXLogDesc(Const Value:TFXLogAction):String;

implementation

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function FXLogDesc(Const Value:TFXLogAction):String;
Begin
  case Value of
    fxtDBConnect           :Result:='DB Connect';
      fxtDBConnecting      :Result:='DB Connecting';
      fxtDBConnected       :Result:='DB Connected';
      fxtDBDisConnect      :Result:='DB DisConnect';
    fxtDBDisConnected      :Result:='DB DisConnected';

    fxtTRStart             :Result:='TR Start';
      fxtTRParams          :Result:='TR Params';
      fxtTRCommit          :Result:='TR Commit';
      fxtTRCommitRetain    :Result:='TR CommitRetain';
      fxtTRRollback        :Result:='TR Rollback';
      fxtTRRollbackRetain  :Result:='TR RollbackRetain';
      fxtTRStartError      :Result:='TR StartErrror';

    fxtSQLPrepare          :Result:='SQL Prepare';
      fxtSQLText           :Result:='SQL Text';
      fxtSQLPreProcess     :Result:='SQL PreProcess';
      fxtSQLParams         :Result:='SQL Params';
      fxtSQLParamsDumpBlob :Result:='SQL ParamsDumpBlob';
      fxtSQLExecQuery      :Result:='ExecQuery';
      fxtSQLPlan           :Result:='SQL Plan';
      fxtSQLRowsAffected   :Result:='SQL RowsAffected';
      fxtSQLCloseCursor    :Result:='CloseCursor';
    fxtSQLCloseQuery       :Result:='CloseQuery';

    fxtOpsProgress         :Result:='Ops Progress';
    fxtOpsError            :Result:='Ops Error';
    fxtInfoMsg             :Result:='Info';
    fxtMiscInfo            :Result:='MiscInfo';
    fxtFetchInfo           :Result:='FetchInfo';
    fxtBlobInfo            :Result:='BlobInfo';
    else                    Result:='Error';
    end;
end;

end.

