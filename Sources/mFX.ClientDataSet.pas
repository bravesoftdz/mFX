unit mFX.ClientDataSet;

interface

{$R-,T-,H+,X+}
{$I mFX.Inc}

uses System.SysUtils, System.Classes, System.Types, System.Variants,
  Data.Db, Data.DBCommon, DataSnap.DBClient, DataSnap.Provider,
  mFX.Intf, mFX.Header, mFX.Classes, mFX.Base, mFX.Blob, mFX.SQL, mFX.SQLField, mFX.List,
  {$IFDEF mFXTRACE}mFX.Logger,{$ENDIF mFXTRACE}
  mFX.Consts, mFX.Utils,
  mFX.InternalDataSet;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  TFXCDSMode = (
    fxClearIndexBeforeOpen,
    fxClearClonedSourceAfterClose,
    fxApplyAfterPost,
    fxApplyAfterDelete,
    fxApplyBeforeClose
    );
  TFXCDSModes = Set Of TFXCDSMode;

  ///<summary>TFXClientDataSet a la TSimpleDataSet</summary>
  TFXClientDataSet = class(TCustomClientDataSet)
  protected
    fDataSet      : TFXInternalDataSet;
    fProvider     : TDataSetProvider;
    fRecordValues : TFXRecordValues;

    ///<summary>Get Database</summary>
    function GetDatabase: TFXCustomDatabase;
    ///<summary>Set Database</summary>
    procedure SetDatabase(Const Value: TFXCustomDatabase);
    ///<summary>Get Transaction</summary>
    function GetTransaction: TFXCustomTransaction;
    ///<summary>Set Transaction</summary>
    procedure SetTransaction(Const Value: TFXCustomTransaction);
    ///<summary>SetCommand</summary>
    procedure SetCommand(Const Value: String);
    ///<summary>GetCommand</summary>
    function GetCommand: String;
    ///<summary>GetParams</summary>
    function GetParams: TParams;
    ///<summary>SetParams</summary>
    procedure SetParams(Const Values: TParams);

    ///<summary>Push RecordValues</summary>
    procedure PushRecordValues;inline;
    ///<summary>Pop RecordValues</summary>
    procedure PopRecordValues;inline;
    ///<summary>Clear RecordValues</summary>
    procedure ClearRecordValues;inline;

  protected
    ///<summary>Set Database</summary>
    procedure SetActive(Value: Boolean); override;
    ///<summary>Set Database</summary>
    procedure SetName(const Value: TComponentName);override;
    ///<summary>OpenCursor</summary>
    procedure OpenCursor(InfoQuery: Boolean); override;
    ///<summary>DoBeforeGetParams</summary>
    procedure DoBeforeGetParams(var OwnerData: OleVariant); override;
    ///<summary>IProviderSupport : PSGetCommandText</summary>
    function PSGetCommandText: string; override;

  public
    ///<summary>constructor</summary>
    constructor Create(AOwner: TComponent); override;
    ///<summary>destructor</summary>
    destructor Destroy; override;

    ///<summary>Refresh CommandText</summary>
    procedure RefreshCommandText(const Value:String;Const DoOpen:Boolean=True);
    ///<summary>AssignParams</summary>
    procedure AssignParams(Const aSQLParams:TFXSQLDAParams);

    ///<summary>Push Query ResultSet to memory buffer</summary>
    procedure PushQuery(Const aSQL:TFXCustomSQL;Const ReStart:Boolean);

    {$IFDEF mFXTRACE}
    /// <summary>Trace</summary>
    procedure LogProgress(Const aMsg:String);overload;inline;
    /// <summary>Log Action</summary>
    Procedure Log(Const Action:TFXLogAction;Const aMsg: string);overload;inline;
    /// <summary>Trace</summary>
    procedure LogError(Const aContext:String;Const e:Exception);overload;inline;
    /// <summary>Trace</summary>
    procedure LogMiscInfo(Const aMsg: string);overload;inline;
    /// <summary>Trace</summary>
    procedure LogFetchInfo(Const aMsg: string);overload;inline;
    {$ENDIF mFXTRACE}

    property Database                 : TFXCustomDatabase        read GetDatabase           write SetDatabase;
    property Transaction              : TFXCustomTransaction     read GetTransaction        write SetTransaction;
    property CommandText              : String                   read GetCommand            write SetCommand;
    property InternalDataSet          : TFXInternalDataSet       read fDataSet;

  published
    property Active;
    property Aggregates;
    property AggregatesActive;
    property AutoCalcFields;
    property DBConnection             : TFXCustomDatabase        read GetDatabase           write SetDatabase;
    property DBTransaction            : TFXCustomTransaction     read GetTransaction        write SetTransaction;
    property Provider                 : TDataSetProvider         read fProvider;
    property Constraints;
    property DisableStringTrim;
    property FileName;
    property Filter;
    property Filtered;
    property FilterOptions;
    property FieldDefs;
    property IndexDefs;
    property IndexFieldNames;
    property IndexName;
    property FetchOnDemand;
    property MasterFields;
    property MasterSource;
    property ObjectView;
    property PacketRecords;
    property Params                   : TParams                  read GetParams             write SetParams;
    property ReadOnly;
    property StoreDefs;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property OnReconcileError;
    property BeforeApplyUpdates;
    property AfterApplyUpdates;
    property BeforeGetRecords;
    property AfterGetRecords;
    property BeforeRowRequest;
    property AfterRowRequest;
    property BeforeExecute;
    property AfterExecute;
    property BeforeGetParams;
    property AfterGetParams;
  end;

  TFXClientDataSetEx = Class(TFXClientDataSet)
  strict private
    fFXCDSModes  : TFXCDSModes;
    fUndoChange  : Boolean;
    fNbErrors    : Integer;
    fLastErr     : String;
    fLastUpdate  : TUpdateKind;
    fApplying    : Integer;
    fHint        : String;
    fRelation    : String;
    ///<summary>Get TableName</summary>
    function GetTableName: String;inline;
    ///<summary>GetDescription:</summary>
    function GetDescription:String;
    ///<summary>GetStatusDescription:</summary>
    function GetStatusDescription:String;
    ///<summary>DefaultReconcileError(</summary>
    procedure DefaultReconcileError(DataSet: TCustomClientDataSet;E: EReconcileError; UpdateKind: TUpdateKind;var Action: TReconcileAction);
  protected
    ///<summary>DoBeforeOpen;</summary>
    procedure DoBeforeOpen;override;
    ///<summary>DoAfterPost;</summary>
    procedure DoAfterPost;override;
    ///<summary>DoAfterCancel;</summary>
    procedure DoAfterCancel;override;
    ///<summary>DoAfterDelete;</summary>
    procedure DoAfterDelete;override;
    ///<summary>DoBeforeClose;</summary>
    procedure DoBeforeClose;override;
    ///<summary>DoAfterClose;</summary>
    procedure DoAfterClose;override;
    ///<summary>DoOnNewRecord; </summary>
    procedure DoOnNewRecord; override;
    ///<summary>DoBeforeEdit; </summary>
    procedure DoBeforeEdit; override;
    ///<summary>DoBeforeInsert; </summary>
    procedure DoBeforeInsert; override;

  public
    ///<summary>Create(</summary>
    constructor Create(AOwner: TComponent); override;

    ///<summary>Apply On Insert/EditE or delete</summary>
    function ApplyEdit:Boolean;overload;

    ///<summary>CheckApply(</summary>
    function CheckApply(Const ForceUndoChange,ResetInEditMode,Boum:Boolean):Integer;overload;

    property Desc      : String          read GetDescription;
    property StatusDesc: String          read GetStatusDescription;
    property LastErr   : String          read fLastErr;
    property TableName : String         read GetTableName;

  published
    property Hint       : String         read fHint       write fHint;
    property Relation   : String         read fRelation   write fRelation;
    property FXCDSModes : TFXCDSModes    read fFXCDSModes write fFXCDSModes default [fxClearIndexBeforeOpen,fxClearClonedSourceAfterClose,fxApplyAfterPost,fxApplyAfterDelete,fxApplyBeforeClose];
  end;

implementation

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXClientDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fProvider:=TDataSetProvider.Create(Self);
  fProvider.UpdateMode:=upWhereAll;
  fProvider.SetSubComponent(True);
  fProvider.Exported:=False;
  fDataSet:=TFXInternalDataSet.Create(Self);
  fProvider.DataSet:=Self.fDataSet;
  fDataSet.SetSubComponent(True);
  SetProvider(fProvider);
end;
{______________________________________________________________________________}
destructor TFXClientDataSet.Destroy;
begin
  fRecordValues.Free;
  inherited;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
{$IFDEF mFXTRACE}
procedure TFXClientDataSet.LogProgress(Const aMsg:String);
Begin
  Assert(fDataSet<>nil);
  if (fDataSet.Database<>nil) then
    fDataSet.Database.Log(Self,fxtOpsProgress,aMsg);
end;
{______________________________________________________________________________}
procedure TFXClientDataSet.Log(Const Action:TFXLogAction;Const aMsg: string);
Begin
  Assert(fDataSet<>nil);
  if (fDataSet.Database<>nil) then
    fDataSet.Database.Log(Self,Action,aMsg);
end;
{______________________________________________________________________________}
procedure TFXClientDataSet.LogError(Const aContext:String;Const e:Exception);
Begin
  Assert(fDataSet<>nil);
  if (fDataSet.Database<>nil) then
    fDataSet.Database.LogError(Self,aContext,e);
end;
{______________________________________________________________________________}
procedure TFXClientDataSet.LogMiscInfo(Const aMsg: string);
Begin
  Assert(fDataSet<>nil);
  if (fDataSet.Database<>nil) then
    fDataSet.Database.Log(Self,fxtMiscInfo,aMsg);
end;
{______________________________________________________________________________}
procedure TFXClientDataSet.LogFetchInfo(Const aMsg: string);
Begin
  Assert(fDataSet<>nil);
  if (fDataSet.Database<>nil) then
    fDataSet.Database.Log(Self,fxtFetchInfo,aMsg);
end;
{$ENDIF mFXTRACE}
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXClientDataSet.SetName(const Value: TComponentName);
Begin
  inherited SetName(Value);
  fProvider.Name:=Self.Name+'_IProvider';
  fDataSet.Name:=Self.Name+'_IDataSet';
End;
{______________________________________________________________________________}
function TFXClientDataSet.GetDatabase: TFXCustomDatabase;
begin
  Assert(fDataSet<>nil);
  result := fDataSet.Database;
end;
{______________________________________________________________________________}
procedure TFXClientDataSet.SetDatabase(Const Value: TFXCustomDatabase);
begin
  Assert(fDataSet<>nil);
  fDataSet.Database := Value;
end;
{______________________________________________________________________________}
function TFXClientDataSet.GetTransaction: TFXCustomTransaction;
begin
  Assert(fDataSet<>nil);
  result := fDataSet.Transaction;
end;
{______________________________________________________________________________}
procedure TFXClientDataSet.SetTransaction(Const Value: TFXCustomTransaction);
begin
  Assert(fDataSet<>nil);
  fDataSet.Transaction := Value;
end;
{______________________________________________________________________________}
procedure TFXClientDataSet.SetCommand(Const Value: String);
Begin
  Assert(fDataSet<>nil);
  fDataSet.CommandText:=Value;
End;
{______________________________________________________________________________}
procedure TFXClientDataSet.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) then begin
    // Ignore when Streaming out of DFM
  end else
    inherited SetActive(Value);
End;
{______________________________________________________________________________}
function TFXClientDataSet.GetCommand:String;
Begin
  Assert(fDataSet<>nil);
  Result:=fDataSet.CommandText;
End;
{______________________________________________________________________________}
procedure TFXClientDataSet.RefreshCommandText(const Value:String;Const DoOpen:Boolean=True);
Begin
  Assert(fDataSet<>nil);
  Self.DisableControls;
  try if Self.Active then
        Self.Close;
      fDataSet.RefreshCommandText(Value);
      if DoOpen Then
        Self.Open
  finally
      Self.EnableControls
  end;
End;
{______________________________________________________________________________}
function TFXClientDataSet.GetParams: TParams;
Begin
  Assert(fDataSet<>nil);
  Result:=fDataSet.Params;
End;
{______________________________________________________________________________}
procedure TFXClientDataSet.SetParams(Const Values: TParams);
Begin
  Assert(fDataSet<>nil);
  fDataSet.Params:=Values
End;
{______________________________________________________________________________}
procedure TFXClientDataSet.AssignParams(Const aSQLParams:TFXSQLDAParams);
Var sp:TFXSQLVAR;
    i:Integer;
    p:TParam;
Begin
  Assert(fDataSet<>nil);
  fDataSet.Params.Clear;
  for i:=0 to Pred(aSQLParams.Count) do Begin
    p:=TParam(fDataSet.Params.Add);
    sp:=aSQLParams[i];
    p.Name:=sp.Name;
    sp.AssignTo(p);
    End;
End;
{______________________________________________________________________________}
procedure TFXClientDataSet.PushQuery(Const aSQL:TFXCustomSQL;Const ReStart:Boolean);
Var SavDBTransaction:TFXCustomTransaction;
    SavDBConnection:TFXCustomDatabase;
    PushQueryDone:Boolean;
    i:Integer;
    f:TField;
Begin
  Assert(fDataSet<>nil);
  PushQueryDone:=False;
  Self.DisableControls;
  try //ReStart -> Close and clear memory buffer
      if (ReStart) then Begin
        if Self.Active then
          Self.Close;
        end;
      //Create Empty Buffer
      if (not Self.Active) then Begin
        //Create FieldDefs iff needed
        if (Self.FieldDefs.Count = 0)and(Self.Fields.Count=0) then Begin
          SavDBConnection:=Self.DBConnection;
          SavDBTransaction:=Self.DBTransaction;
          try Self.DBConnection:=aSQL.Database;
              Self.DBTransaction:=aSQL.Transaction;
              Self.CommandText:=EmptyStr;
              Self.CommandText:=aSQL.SQL.Text;
              Self.AssignParams(aSQL.Params);
              PushQueryDone:=True;
              Open;
          finally
              fDataSet.Transaction:=SavDBTransaction;
              fDataSet.Database:=SavDBConnection;
          end
        end else
          Self.CreateDataSet;
        end;
      //Push Query ResultSet
      if not PushQueryDone then Begin
        aSQL.ExecQuery;
        While Not aSQL.Eof do Begin
          Self.Insert;
          for i:=0 to Pred(Self.Fields.Count) do Begin
            f:=Self.Fields[i];
            Case f.DataType of
              ftSmallint,
              ftInteger,
              ftWord:
                f.AsInteger:=aSQL.Fields[i].AsInteger;
              ftLargeint:
               (f as TLargeintField).AsLargeInt:=aSQL.Fields[i].AsInt64;
              ftFloat,
              ftCurrency,
              ftBCD:
                f.AsFloat:=aSQL.Fields[i].AsDouble;
              ftDate:
                f.AsDateTime:=aSQL.Fields[i].AsDate;
              ftTime:
                f.AsDateTime:=aSQL.Fields[i].AsTime;
              ftTimeStamp,
              ftDateTime:
                f.AsDateTime:=aSQL.Fields[i].AsDateTime;
              ftWideMemo, ftWideString, ftFixedWideChar,
              ftFmtMemo, ftMemo, ftString, ftFixedChar:Begin
                f.AsString:=aSQL.Fields[i].AsString;
                End;
              else Begin
                f.Value:=aSQL.Fields[i].Value;
            end end end;
          Self.Post;
          aSQL.Next;
          end;
        aSQL.CloseQuery;
        End;
  finally
      Self.EnableControls;
      aSQL.CloseQuery;
  end;
End;
{______________________________________________________________________________}
procedure TFXClientDataSet.OpenCursor(InfoQuery: Boolean);
begin
  Assert(fDataSet<>nil);
  if Assigned(fProvider) then
    SetProvider(fProvider);
  inherited
end;
{______________________________________________________________________________}
procedure TFXClientDataSet.DoBeforeGetParams(var OwnerData: OleVariant);
begin
  Assert(fDataSet<>nil);
  if Assigned(fProvider) then
    SetProvider(fProvider);
  inherited;
end;
{______________________________________________________________________________}
function TFXClientDataSet.PSGetCommandText: string;
var IP: IProviderSupportNG;
begin
  if Supports(fDataSet,IProviderSupportNG,IP) then
    Result := IP.PSGetCommandText else
    Result := Self.CommandText;
end;
{______________________________________________________________________________}
procedure TFXClientDataSet.PushRecordValues;
Begin
  if fRecordValues=nil then
    fRecordValues:=TFXRecordValues.Create;
  fRecordValues.Push(Self)
End;
{______________________________________________________________________________}
procedure TFXClientDataSet.PopRecordValues;
Begin
  if fRecordValues<>nil then
    fRecordValues.Pop(Self)
End;
{______________________________________________________________________________}
procedure TFXClientDataSet.ClearRecordValues;
Begin
  if fRecordValues<>nil then
    fRecordValues.Clear;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXClientDataSetEx.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  fFXCDSModes:=[fxClearIndexBeforeOpen,fxClearClonedSourceAfterClose,fxApplyAfterPost,fxApplyAfterDelete,fxApplyBeforeClose];
end;
{______________________________________________________________________________}
function TFXClientDataSetEx.GetDescription:String;
Begin
  if Self.Hint<>EmptyStr then
    Result:=Self.Hint else
    result:=format('%s_%s',[Self.ClassName,Self.Name])
end;
{______________________________________________________________________________}
function TFXClientDataSetEx.GetTableName: String;
Var ws:String;
Begin
  if fRelation<>EmptyStr then Begin
    Result:=fRelation;
    exit;
    end;

  if Assigned(fProvider.OnGetTableName) then Begin
    ws:='';
    fProvider.OnGetTableName(Self,nil,ws);
    if ws<>'' then Begin
      fRelation:=ws;
      Result:=fRelation;
      exit;
    end end;

  if fHint<>EmptyStr then Begin
    fRelation:=fHint;
    Result:=fRelation;
    exit;
    end;

  fRelation:=Self.Name;
  Result:=fRelation;
End;
{______________________________________________________________________________}
function TFXClientDataSetEx.GetStatusDescription:String;
Var f:TField;
    fc:Integer;
Begin
  result:=EmptyStr;

  Case Self.State of
    dsEdit,
    dsInsert,
    dsBrowse:Begin
      if Self.IsEmpty then Begin
        result:=' Empty DataSet';
      end else Begin
        fc:=Self.FieldCount;
        if fc>=1 then Begin
          f:=Self.Fields[0];
          result:=Trim(f.AsString)
          end;
        if fc>=2 then Begin
          f:=Self.Fields[1];
          if result<>EmptyStr then
            result:=result+' : '+Trim(f.AsString) else
            result:=Trim(f.AsString)
          end;
        Case Self.State of
          dsEdit  :
            result:=result+' : Edit';
          dsInsert:
            result:=result+' : Insert';
          dsBrowse:
            result:=result+' : Browse';
      end end end;
    else Begin
      result:=format('%s %s %s : Invalid State ',[Self.ClassName,Self.Name,Self.Hint])
    end end;
end;
{______________________________________________________________________________}
procedure TFXClientDataSetEx.DefaultReconcileError(DataSet: TCustomClientDataSet;E: EReconcileError; UpdateKind: TUpdateKind;var Action: TReconcileAction);
Var s:String;
Begin
  s:=E.Message;
  Inc(fNbErrors);
  fLastUpdate:=UpdateKind;
  if fLastErr=EmptyStr then
    fLastErr:=s else
    fLastErr:=fLastErr+sLinebreak+s;
end;
{______________________________________________________________________________}
function TFXClientDataSetEx.ApplyEdit:Boolean;
Var TableName:String;
    ErrCount:Integer;
Begin
  Assert(fApplying=0);
  Assert(not Assigned(Self.OnReconcileError));
  Assert(Self.State in [dsEdit,dsInsert,dsBrowse]);

  if Self.State in [dsEdit,dsInsert] then
    Self.Post;

  If Self.ChangeCount<=0 then Begin
    Result:=True;
    exit;
    end;

  If Self.ChangeCount<>1 then
    Raise Exception.Create('Ops Not Supported !');

  fNbErrors:=0;
  Inc(fApplying);
  fLastErr:=Format('Apply %s:',[TableName]);
  Self.OnReconcileError:=Self.DefaultReconcileError;
  try fDataSet.ReStart_RWTR;
      TableName:=Self.GetTableName;
      ErrCount:=Self.ApplyUpdates(-1);
      If ErrCount<=0 then Begin
        Self.MergeChangeLog;
        fDataSet.Commit_TR;
        fLastErr:=EmptyStr;
        fNbErrors:=0;
        Result:=True;
      end else Begin
        While Self.ChangeCount>0 do
          Self.UndoLastChange(False);
        Self.MergeChangeLog;
        case fLastUpdate of
          ukModify:Begin
            PushRecordValues;
            Self.Edit;
            PopRecordValues;
            ClearRecordValues;
            end;
          ukInsert:Begin
            PushRecordValues;
            Self.Insert;
            PopRecordValues;
            ClearRecordValues;
            end;
          ukDelete:Begin
            //
          end end;
        fDataSet.RollBack_TR;
        Result:=False
        end;
  finally
      Self.OnReconcileError:=nil;
      fDataSet.RollBack_TR;
      Dec(fApplying)
  end
end;
{______________________________________________________________________________}
function TFXClientDataSetEx.CheckApply(Const ForceUndoChange,ResetInEditMode,Boum:Boolean):Integer;
Var TableName:String;
Begin
  Result:=0;
  Assert(Self.State in [dsEdit,dsInsert,dsBrowse]);

  if fApplying<>0 Then Begin
    // Recurse
    Exit;
    end;

  if Self.State in [dsEdit,dsInsert] then
    Self.Post;

  If Self.ChangeCount<=0 then
    exit;

  if Assigned(Self.OnReconcileError) then Begin
    Self.ApplyUpdates(-1);
    exit;
    end;

  if ResetInEditMode then Begin
    If Self.ChangeCount<>1 then Begin
      Raise Exception.Create('Ops Not Supported !');
    end end;

  fNbErrors:=0;
  Inc(fApplying);
  fUndoChange:=ForceUndoChange;
  fLastErr:=Format('Apply %s:',[TableName]);
  Self.OnReconcileError:=Self.DefaultReconcileError;
  try fDataSet.ReStart_RWTR;
      TableName:=Self.GetTableName;
      Result:=Self.ApplyUpdates(-1);
      If Result<=0 then Begin
        Self.MergeChangeLog;
        fLastErr:=EmptyStr;
        fNbErrors:=0;
      end else Begin
        if fUndoChange then Begin
          While Self.ChangeCount>0 do
            Self.UndoLastChange(False);
          Self.MergeChangeLog;
        end else
        if ResetInEditMode then Begin
          Self.Edit;
          Self.SetModified(True)
          end;
        if Boum then
          Raise Exception.Create(fLastErr);
        end;
      fDataSet.Commit_TR;
  finally
      Self.OnReconcileError:=nil;
      fDataSet.RollBack_TR;
      Dec(fApplying)
  end
end;
{______________________________________________________________________________}
procedure TFXClientDataSetEx.DoBeforeOpen;
Begin
  inherited;
  fNbErrors:=0;
  fLastErr:=EmptyStr;
  if fxClearIndexBeforeOpen in fFXCDSModes then
    Self.IndexName:='';
end;
{______________________________________________________________________________}
procedure TFXClientDataSetEx.DoOnNewRecord;
Begin
  if fApplying=0 then Begin
    inherited;
    end;
End;
{______________________________________________________________________________}
procedure TFXClientDataSetEx.DoBeforeInsert;
Begin
  if fApplying=0 then Begin
    inherited;
    end;
End;
{______________________________________________________________________________}
procedure TFXClientDataSetEx.DoBeforeEdit;
Begin
  if fApplying=0 then Begin
    inherited;
    end;
End;
{______________________________________________________________________________}
procedure TFXClientDataSetEx.DoAfterPost;
Begin
  inherited;
  if fxApplyAfterPost in fFXCDSModes then
    Self.CheckApply(False,True,True);
end;
{______________________________________________________________________________}
procedure TFXClientDataSetEx.DoAfterCancel;
Begin
  inherited;
  fNbErrors:=0;
  fLastErr:=EmptyStr;
End;
{______________________________________________________________________________}
procedure TFXClientDataSetEx.DoAfterDelete;
Begin
  inherited;
  if fxApplyAfterDelete in fFXCDSModes then
    Self.CheckApply(True,True,True);
end;
{______________________________________________________________________________}
procedure TFXClientDataSetEx.DoBeforeClose;
Begin
  inherited;
  if fxApplyBeforeClose in fFXCDSModes then
    Self.CheckApply(False,False,True);
end;
{______________________________________________________________________________}
procedure TFXClientDataSetEx.DoAfterClose;
Begin
  if (fxClearClonedSourceAfterClose in fFXCDSModes)and(Self.CloneSource<>nil) then
    Self.Notification(Self.CloneSource,opRemove);
  inherited;
end;

end.


