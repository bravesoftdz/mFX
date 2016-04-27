unit mFX.DDLTool;

interface

{$I mFX.Inc}

uses System.Classes, System.SysUtils,
  mFX.Header, mFX.Classes, mFX.Utils, mFX.List, mFX.Base, mFX.MetaData, mFX.SQL,
  mFX.SQLScript;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  TFXDDLTool = Class;
  TFXCustomDDLTool = Class;

  /// <summary>SQL Query</summary>
  TFXCustomDDLTool = class(TFXCustomScript)
  private
    fSchema           : TFXCustomMetaSchema;

    /// <SUMMARY>Check Schema</SUMMARY>
    procedure CheckSchema;

  protected
    /// <SUMMARY>Check Schema</SUMMARY>
    procedure DependencyError(Const aMetaObj:TFXCustomMetaNode);virtual;

  public
    /// <summary>destructor</summary>
    destructor Destroy; override;

    /// <SUMMARY>AddRole</SUMMARY>
    procedure AddRole(Const aRole:String);
    /// <SUMMARY>Drop Role</SUMMARY>
    function DropRole(Const aRole:String):Boolean;overload;
    /// <SUMMARY>Drop All Roles</SUMMARY>
    function DropRoles:Integer;overload;
    /// <SUMMARY>Grant Read To</SUMMARY>
    procedure GrantReadTo(Const aRole:String);inline;
    /// <SUMMARY>Grant All To</SUMMARY>
    procedure GrantAllTo(Const aRole:String);overload;inline;
    /// <SUMMARY>Grant To</SUMMARY>
    procedure GrantAllTo(Const MetaObjects:TFXMetaObjects;Const FullAccess:Boolean;Const aRole:String);overload;

    ///<SUMMARY>Drop Procedures</SUMMARY>
    function DropProcedures:Integer;
    ///<SUMMARY>Drop Procedures</SUMMARY>
    function AlterProceduresAsDummy:Integer;
    ///<SUMMARY>Drop Procedures</SUMMARY>
    function DropDummyProcedures:Integer;
    /// <summary>Drop Exceptions</summary>
    function DropExceptions:Integer;
    /// <summary>DropUDFs</summary>
    function DropUDFs:Integer;overload;

    /// <summary>Drop Views</summary>
    function DropViews:Integer;
    /// <summary>Drop Tiggers</summary>
    function DropTiggers:Integer;
    /// <summary>Drop ForeignKeys</summary>
    function DropForeignKeys:Integer;
    /// <summary>Drop Indexes</summary>
    function DropIndexes:Integer;
    /// <summary>Drop UnqConstraints</summary>
    function DropUnqConstraints:Integer;
    /// <summary>Drop Check Constraints</summary>
    function DropCheckConstraints:Integer;
    /// <summary>Drop PrimaryKeys</summary>
    function DropPrimaryKeys:Integer;
    /// <summary>Drop ComputedFields</summary>
    function DropComputedFields:Integer;
  end;

  /// <summary>SQL Query</summary>
  TFXDDLTool = class(TFXCustomDDLTool)
  private
    fOnEcho           : TFXScriptTrace;
    fOnTrace          : TFXScriptTrace;
    fOnTraceResult    : TFXScriptTrace;
    fOnError          : TFXScriptError;
    fOnDependencyError: TFXScriptDependencyError;

  protected
    /// <summary>Echo</summary>
    procedure Echo(Const aMsg:String);override;
    /// <summary>Trace</summary>
    procedure Trace(Const aMsg:String);override;
    /// <summary>Trace Error</summary>
    function TraceError(Const e:Exception):Boolean;override;
    /// <SUMMARY>Check Schema</SUMMARY>
    procedure DependencyError(Const aMetaObj:TFXCustomMetaNode);override;
    /// <summary>Trace Result</summary>
    procedure TraceResult(const aMsg:String);override;

  published
    property Database;
    property Transaction;
    property SQL;
    property ParamCheck;
    property OnSQLChanging;
    property OnSQLChanged;

    property OnEcho           : TFXScriptTrace           read fOnEcho            write fOnEcho;
    property OnTrace          : TFXScriptTrace           read fOnTrace           write fOnTrace;
    property OnTraceResult    : TFXScriptTrace           read fOnTraceResult     write fOnTraceResult;
    property OnError          : TFXScriptError           read fOnError           write fOnError;
    property OnDependencyError: TFXScriptDependencyError read fOnDependencyError write fOnDependencyError;
  end;

implementation

Uses mFX.Consts, mFX.ErrorCodes, mFX.SystemTable;

Type
  ///<SUMMARY>Database Def</SUMMARY>
  TFXDDLToolSchema = class(TFXCustomMetaSchema)
  private
    fDDLTool       : TFXCustomDDLTool;

    ///<SUMMARY>Find Role</SUMMARY>
    function FindRole(Const aName:String):Boolean;overload;
    ///<SUMMARY>Download All Roles</SUMMARY>
    procedure DownloadRoles;

    ///<SUMMARY>Download All Tables</SUMMARY>
    procedure DownloadTables;
    ///<SUMMARY>Download All Views</SUMMARY>
    procedure DownloadViews;
    ///<SUMMARY>Download All Views</SUMMARY>
    procedure DownloadViewsDependencies;
    ///<SUMMARY>Download All Procedures</SUMMARY>
    procedure DownloadComputedFieldsDependencies;

    ///<SUMMARY>Download All Triggers</SUMMARY>
    procedure DownloadCheckConstraints;
    ///<SUMMARY>Download All Triggers</SUMMARY>
    procedure DownloadTriggers;
    ///<SUMMARY>Download All Indexes</SUMMARY>
    procedure DownloadIndexes;
    ///<SUMMARY>Download All ForeignKeys</SUMMARY>
    procedure DownloadIndexesDependencies;
    ///<SUMMARY>Download All Primary Indexes</SUMMARY>
    procedure DownloadPrimaryKeys;
    ///<SUMMARY>Download All ForeignKeys</SUMMARY>
    procedure DownloadForeignKeys;
    ///<SUMMARY>Download All ForeignKeys</SUMMARY>
    procedure DownloadForeignKeysDependencies;

    ///<SUMMARY>Download All Exceptions</SUMMARY>
    procedure DownloadExceptions;
    ///<SUMMARY>Download All UDFs</SUMMARY>
    procedure DownloadUDFs(Const DownloadParams:Boolean);
    ///<SUMMARY>Download All Procedures</SUMMARY>
    procedure DownloadUDFsDependencies;
    ///<SUMMARY>Download All Procedures</SUMMARY>
    procedure DownloadProcedures(Const DownloadParams:Boolean);
    ///<SUMMARY>Download All Procedures</SUMMARY>
    procedure DownloadProcsDependencies;

    ///<SUMMARY>Constructor</SUMMARY>
    constructor Create(Const aDDLTool: TFXCustomDDLTool);reintroduce;
  end;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXDDLToolSchema.Create(Const aDDLTool: TFXCustomDDLTool);
begin
  fTerminator    := ';';
  fDDLTool       := aDDLTool;
  fDB            := aDDLTool.Database;
  fTRx           := aDDLTool.Transaction;

  fNodes         := TFXList.Create;
end;
{______________________________________________________________________________}
function TFXDDLToolSchema.FindRole(Const aName:String):Boolean;
Begin
  TFXMetaRole.PrepareQuery(aName,fDDLTool);
  fDDLTool.ParamCheck:=False;
  fDDLTool.ExecQuery;
  result:=not fDDLTool.EOF;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadRoles;
Begin
  Self.Clear;
  TFXMetaRole.PrepareQuery(fDDLTool);
  fDDLTool.ParamCheck:=False;
  fDDLTool.ExecQuery;
  While Not fDDLTool.Eof do Begin
    TFXMetaRole.CreateFromSQL(Self,fDDLTool);
    fDDLTool.Next;
    end;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadTables;
Begin
  Self.Clear;
  TFXMetaTable.PrepareQuery(fDDLTool);
  fDDLTool.ParamCheck:=False;
  fDDLTool.ExecQuery;
  While Not fDDLTool.Eof do Begin
    TFXMetaTable.CreateFromSQL(Self,fDDLTool);
    fDDLTool.Next;
    end;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadViews;
Begin
  Self.Clear;
  TFXMetaView.PrepareQuery(fDDLTool);
  fDDLTool.ParamCheck:=False;
  fDDLTool.ExecQuery;
  While Not fDDLTool.Eof do Begin
    TFXMetaView.CreateFromSQL(Self,fDDLTool);
    fDDLTool.Next;
    end;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadViewsDependencies;
Var MetaObj:TFXMetaView;
    i:Integer;
Begin
  fDDLTool.SQL.Text:=_QRY_Dependencies_;
  fDDLTool.SQL.Add('Where (ot.RDB$FIELD_NAME = ''RDB$OBJECT_TYPE'')');
  fDDLTool.SQL.Add('  and (d.RDB$DEPENDED_ON_NAME=:name)');
  fDDLTool.SQL.Add('  and (d.RDB$DEPENDED_ON_TYPE='+MetaObject2Str(moView)+')');
  fDDLTool.ParamCheck:=True;
  for i:=0 to Pred(fViews.Count) do Begin
    MetaObj:=TFXMetaView(fViews[i]);
    MetaObj.DownloadDependencies(fDDLTool);
    end;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadComputedFieldsDependencies;
Var MetaTableField:TFXMetaTableField;
    MetaTable:TFXCustomMetaRelation;
    i,j:Integer;
Begin
  fDDLTool.SQL.Text:=_QRY_Dependencies_;
  fDDLTool.SQL.Add('Where (d.RDB$FIELD_NAME=:name)and(d.RDB$DEPENDED_ON_NAME=:parent)');
  fDDLTool.ParamCheck:=True;
  for i:=0 to Pred(fTables.Count) do Begin
    Assert((TObject(fTables[i]) as TFXCustomMetaRelation).Schema=Self);
    MetaTable:=TFXCustomMetaRelation(fTables[i]);
    for j:=0 to Pred(MetaTable.Fields.Count) do Begin
      Assert((TObject(MetaTable.Fields[j]) as TFXMetaTableField).Schema=Self);
      MetaTableField:=TFXMetaTableField(MetaTable.Fields[j]);
      if MetaTableField.IsComputed then Begin
        MetaTableField.DownloadDependencies(fDDLTool);
    end end end;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadCheckConstraints;
Begin
  Self.Clear;
  TFXMetaCheckConstraint.PrepareQuery(fDDLTool);
  fDDLTool.ParamCheck:=False;
  fDDLTool.ExecQuery;
  While Not fDDLTool.Eof do Begin
    TFXMetaCheckConstraint.CreateFromSQL(Self,nil,fDDLTool);
    fDDLTool.Next;
    end;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadTriggers;
Begin
  TFXMetaTrigger.PrepareQuery(fDDLTool);
  fDDLTool.ParamCheck:=False;
  fDDLTool.ExecQuery;
  While Not fDDLTool.Eof do Begin
    TFXMetaTrigger.CreateFromSQL(Self,nil,fDDLTool);
    fDDLTool.Next;
    end;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadIndexes;
Begin
  Self.Clear;
  TFXMetaIndex.PrepareQuery(fDDLTool);
  fDDLTool.ParamCheck:=False;
  fDDLTool.ExecQuery;
  While Not fDDLTool.Eof do Begin
    TFXMetaIndex.CreateFromSQL(Self,nil,fDDLTool);
    fDDLTool.Next;
    end;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadIndexesDependencies;
Var MetaObj:TFXMetaIndex;
    i:Integer;
Begin
  fDDLTool.SQL.Text:=_QRY_Dependencies_;
  fDDLTool.SQL.Add('Where (ot.RDB$FIELD_NAME = ''RDB$OBJECT_TYPE'')');
  fDDLTool.SQL.Add('  and (d.RDB$DEPENDED_ON_NAME=:name)');
  fDDLTool.SQL.Add('  and (d.RDB$DEPENDED_ON_TYPE='+MetaObject2Str(moIndex)+')');
  fDDLTool.ParamCheck:=True;
  for i:=0 to Pred(fIndexes.Count) do Begin
    MetaObj:=TFXMetaIndex(fIndexes[i]);
    MetaObj.DownloadDependencies(fDDLTool);
    end;
  fDDLTool.CloseQuery;

  /// <remark>
  ///  It fails all the time "fSchema.FindRelation(MetaObj.RelationName,TableObj)"
  ///  because the tables are not loaded for the RelationName
  ///  TOBE Check
  /// </remark>
  for i:=0 to Pred(fIndexes.Count) do Begin
    MetaObj:=TFXMetaIndex(fIndexes[i]);
    Self.DownloadRelation(fDDLTool.Transaction,MetaObj.RelationName);
    end;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadPrimaryKeys;
Begin
  Self.Clear;
  TFXMetaPrimaryKey.PrepareQuery(False,fDDLTool);
  fDDLTool.ParamCheck:=False;
  fDDLTool.ExecQuery;
  While Not fDDLTool.Eof do Begin
    TFXMetaPrimaryKey.CreateFromSQL(Self,nil,fDDLTool);
    fDDLTool.Next;
    end;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadForeignKeys;
Begin
  TFXMetaForeignKey.PrepareQuery(fDDLTool);
  fDDLTool.ParamCheck:=False;
  fDDLTool.ExecQuery;
  While Not fDDLTool.Eof do Begin
    TFXMetaForeignKey.CreateFromSQL(Self,nil,fDDLTool);
    fDDLTool.Next;
    end;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadForeignKeysDependencies;
Var MetaObj:TFXMetaForeignKey;
    i:Integer;
Begin
  fDDLTool.SQL.Text:=_QRY_Dependencies_;
  fDDLTool.SQL.Add('Where (ot.RDB$FIELD_NAME = ''RDB$OBJECT_TYPE'')');
  fDDLTool.SQL.Add('  and (d.RDB$DEPENDED_ON_NAME=:name)');
  fDDLTool.SQL.Add('  and (d.RDB$DEPENDED_ON_TYPE='+MetaObject2Str(moForeignKey)+')');
  fDDLTool.ParamCheck:=True;
  for i:=0 to Pred(fForeignKeys.Count) do Begin
    MetaObj:=TFXMetaForeignKey(fForeignKeys[i]);
    Assert(MetaObj.IndexName<>EmptyStr);
    MetaObj.DownloadDependencies(fDDLTool);
    end;
  fDDLTool.CloseQuery;

  /// <remark>
  ///  It fails all the time "FindRelation(MetaObj.RelationName,TableObj)" and  "fSchema.FindRelation(MetaObj.FKName,FKTableObj)"
  ///  because the tables are not loaded for the RelationName and FKName
  ///  TOBE Check
  /// </remark>
  for i:=0 to Pred(fForeignKeys.Count) do Begin
    MetaObj:=TFXMetaForeignKey(fForeignKeys[i]);
    Self.DownloadRelation(fDDLTool.Transaction,MetaObj.RelationName);
    Self.DownloadRelation(fDDLTool.Transaction,MetaObj.FKName);
    end;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadExceptions;
Begin
  Self.Clear;
  TFXMetaException.PrepareQuery(fDDLTool);
  fDDLTool.ParamCheck:=False;
  fDDLTool.ExecQuery;
  While Not fDDLTool.Eof do Begin
    TFXMetaException.CreateFromSQL(Self,fDDLTool);
    fDDLTool.Next;
    end;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadUDFs(Const DownloadParams:Boolean);
Var MetaUDF:TFXMetaUDF;
    i:Integer;
Begin
  Self.Clear;
  TFXMetaUDF.PrepareQuery(fDDLTool);
  fDDLTool.ParamCheck:=False;
  fDDLTool.ExecQuery;
  While Not fDDLTool.Eof do Begin
    TFXMetaUDF.CreateFromSQL(Self,fDDLTool);
    fDDLTool.Next;
    end;
  fDDLTool.CloseQuery;
  if (DownloadParams)and(fUDFs<>nil)and(fUDFs.Count>0) then Begin
    fDDLTool.SQL.Text:='TODO _QRY_Proc_Fields_';
    fDDLTool.ParamCheck:=True;
    for i:=0 to Pred(fUDFs.Count) do Begin
      MetaUDF:=TFXMetaUDF(fUDFs[i]);
      MetaUDF.DownloadParams(fDDLTool);
      end;
    fDDLTool.CloseQuery;
    end;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadUDFsDependencies;
Var MetaUDF:TFXMetaUDF;
    i:Integer;
Begin
  fDDLTool.SQL.Text:=_QRY_Dependencies_;
  fDDLTool.SQL.Add('Where (ot.RDB$FIELD_NAME = ''RDB$OBJECT_TYPE'')');
  fDDLTool.SQL.Add('  and (d.RDB$DEPENDED_ON_NAME=:name)');
  fDDLTool.SQL.Add('  and (d.RDB$DEPENDED_ON_TYPE='+MetaObject2Str(moUDF)+')');
  fDDLTool.ParamCheck:=True;
  for i:=0 to Pred(fUDFs.Count) do Begin
    MetaUDF:=TFXMetaUDF(fUDFs[i]);
    MetaUDF.DownloadDependencies(fDDLTool);
    end;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadProcedures(Const DownloadParams:Boolean);
Var MetaProc:TFXMetaProcedure;
    i:Integer;
Begin
  Assert(fDB<>nil);
  Assert(fTRx<>nil);
  TFXMetaProcedure.PrepareQuery(fDDLTool);
  fDDLTool.ParamCheck:=False;
  fDDLTool.ExecQuery;
  While Not fDDLTool.Eof do Begin
    TFXMetaProcedure.CreateFromSQL(Self,fDDLTool);
    fDDLTool.Next;
    end;
  fDDLTool.CloseQuery;

  if (DownloadParams)and(fProcs<>nil)and(fProcs.Count>0) then Begin
    fDDLTool.SQL.Text:=_QRY_Proc_Fields_;
    fDDLTool.ParamCheck:=True;
    for i:=0 to Pred(fProcs.Count) do Begin
      MetaProc:=TFXMetaProcedure(fProcs[i]);
      MetaProc.DownloadParams(fDDLTool);
      end;
    fDDLTool.CloseQuery;
    end;
End;
{______________________________________________________________________________}
procedure TFXDDLToolSchema.DownloadProcsDependencies;
Var MetaProc:TFXMetaProcedure;
    i:Integer;
Begin
  fDDLTool.SQL.Text:=_QRY_Dependencies_;
  fDDLTool.SQL.Add('Where (ot.RDB$FIELD_NAME = ''RDB$OBJECT_TYPE'')');
  fDDLTool.SQL.Add('  and (d.RDB$DEPENDED_ON_NAME=:name)');
  fDDLTool.SQL.Add('  and (d.RDB$DEPENDED_ON_TYPE='+MetaObject2Str(moProcedure)+')');
  fDDLTool.ParamCheck:=True;
  for i:=0 to Pred(fProcs.Count) do Begin
    MetaProc:=TFXMetaProcedure(fProcs[i]);
    MetaProc.DownloadDependencies(fDDLTool);
    end;
  fDDLTool.CloseQuery;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
destructor TFXCustomDDLTool.Destroy;
Begin
  FreeAndNil(fSchema);
  inherited;
end;
{______________________________________________________________________________}
procedure TFXCustomDDLTool.CheckSchema;
Begin
  if fSchema=nil then Begin
    fSchema:=TFXDDLToolSchema.Create(Self);
    End;
End;
{______________________________________________________________________________}
procedure TFXCustomDDLTool.AddRole(Const aRole:String);
Begin
  ReStart_TR;
  CheckSchema;
  if not (fSchema as TFXDDLToolSchema).FindRole(aRole) then Begin
    Self.ReStart_TR;
    Self.SQL.Text:=Format('Create Role %s;',[UpperCase(aRole)]);
    Self.ParamCheck:=False;
    Self.ExecQuery;
    Self.CloseQuery;
    end;
  Commit_TR;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropRole(Const aRole:String):Boolean;
Begin
  ReStart_TR;
  CheckSchema;
  Result:=False;
  if (fSchema as TFXDDLToolSchema).FindRole(UpperCase(aRole)) then Begin
    Self.ReStart_TR;
    Self.SQL.Text:=Format('Drop Role %s;',[UpperCase(aRole)]);
    Self.ParamCheck:=False;
    Self.ExecQuery;
    Self.CloseQuery;
    Result:=True;
    end;
  Commit_TR;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropRoles:Integer;
Var MetaRole:TFXMetaRole;
    i:Integer;
Begin
  ReStart_TR;
  CheckSchema;
 (fSchema as TFXDDLToolSchema).DownloadRoles;
  if fSchema.Roles<>nil then Begin
    Result:=fSchema.Roles.Count;
    for i:=0 to Pred(Result) do Begin
      MetaRole:=TFXMetaRole(fSchema.Roles[i]);
      if not MetaRole.IsADMIN then Begin
        Self.SQL.Text:=MetaRole.AsDropDDL;
        Self.ParamCheck:=False;
        Self.ExecQuery;
        Self.CloseQuery;
      end else Begin
        if Self.DoTrace then
          Self.Trace('.. Keep '+MetaRole.Name);
        Dec(Result)
      end end;
    if Self.DoTrace then
      Self.Trace(format('%d Roles dropped',[Result]));
    Self.Commit_TR;
  end else
    Result:=0;
  fSchema.Clear;
End;
{______________________________________________________________________________}
procedure TFXCustomDDLTool.GrantAllTo(Const MetaObjects:TFXMetaObjects;Const FullAccess:Boolean;Const aRole:String);
Var MetaObject:TFXCustomMetaNode;
    i:Integer;
Begin
  ReStart_TR;
  CheckSchema;
  if moTable in MetaObjects then Begin
   (fSchema as TFXDDLToolSchema).DownloadTables;
    if fSchema.Tables<>nil then Begin
      for i:=0 to Pred(fSchema.Tables.Count) do Begin
        MetaObject:=TFXCustomMetaNode(fSchema.Tables[i]);
        if FullAccess then
          Self.SQL.Text:=Format('GRANT ALL on %s to %s;',[MetaObject.QuotedName,aRole]) else
          Self.SQL.Text:=Format('GRANT SELECT on %s to %s;',[MetaObject.QuotedName,aRole]);
        Self.ParamCheck:=False;
        Self.ExecQuery;
        Self.CloseQuery;
    end end end;

  if moView in MetaObjects then Begin
   (fSchema as TFXDDLToolSchema).DownloadViews;
    if fSchema.Views<>nil then Begin
      for i:=0 to Pred(fSchema.Views.Count) do Begin
        MetaObject:=TFXCustomMetaNode(fSchema.Views[i]);
        if FullAccess then
          Self.SQL.Text:=Format('GRANT ALL on %s to %s;',[MetaObject.QuotedName,aRole]) else
          Self.SQL.Text:=Format('GRANT SELECT on %s to %s;',[MetaObject.QuotedName,aRole]);
        Self.ParamCheck:=False;
        Self.ExecQuery;
        Self.CloseQuery;
    end end end;

  if moProcedure in MetaObjects then Begin
    fSchema.Clear;
   (fSchema as TFXDDLToolSchema).DownloadProcedures(False);
    if fSchema.Procs<>nil then Begin
      for i:=0 to Pred(fSchema.Procs.Count) do Begin
        MetaObject:=TFXCustomMetaNode(fSchema.Procs[i]);
        Self.SQL.Text:=Format('GRANT EXECUTE on Procedure %s to %s;',[MetaObject.QuotedName,aRole]);
        Self.ParamCheck:=False;
        Self.ExecQuery;
        Self.CloseQuery;
    end end end;

  Self.Commit_TR;
  fSchema.Clear;
end;
{______________________________________________________________________________}
procedure TFXCustomDDLTool.GrantAllTo(Const aRole:String);
Begin
  GrantAllTo([moTable,moView,moProcedure],True,aRole);
end;
{______________________________________________________________________________}
procedure TFXCustomDDLTool.GrantReadTo(Const aRole:String);
Begin
  GrantAllTo([moTable,moView,moProcedure],False,aRole);
end;
{______________________________________________________________________________}
function TFXCustomDDLTool.AlterProceduresAsDummy:Integer;
Var MetaProc:TFXMetaProcedure;
    i:Integer;
Begin
  ReStart_TR;
  CheckSchema;
  fSchema.Clear;
 (fSchema as TFXDDLToolSchema).DownloadProcedures(True);
  if fSchema.Procs<>nil then Begin
    Result:=fSchema.Procs.Count;
    for i:=0 to Pred(Result) do Begin
      MetaProc:=TFXMetaProcedure(fSchema.Procs[i]);
      Self.SQL.Text:=MetaProc.AsDummyAlterDDL;
      Self.ParamCheck:=False;
      Self.ExecQuery;
      Self.CloseQuery;
      end;
    Self.Commit_TR;
  end else
    Result:=0;
  fSchema.Clear;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropDummyProcedures:Integer;
Var MetaProc:TFXMetaProcedure;
    i:Integer;
Begin
  ReStart_TR;
  CheckSchema;
  fSchema.Clear;
 (fSchema as TFXDDLToolSchema).DownloadProcedures(False);
  if fSchema.Procs<>nil then Begin
   (fSchema as TFXDDLToolSchema).DownloadProcsDependencies;
    Self.Commit_TR;
    Result:=fSchema.Procs.Count;
    for i:=0 to Pred(Result) do Begin
      Inc(fDDLCount);
      Inc(fStatementCount);
      MetaProc:=TFXMetaProcedure(fSchema.Procs[i]);
      if MetaProc.HasDependencies then Begin
        Assert(not MetaProc.HasDependedOn[moProcedure]);
        DependencyError(MetaProc);
        Inc(fDependencyErrors);
        Inc(fErrorCount);
        Inc(fDDLError);
      end else Begin
        Self.SQL.Text:=MetaProc.AsDropDDL;
        Self.ParamCheck:=False;
        Self.Start_TR;
        try Self.ExecQuery;
            Self.CloseQuery;
            Self.Commit_TR;
        except on e:Exception do Begin
            Self.CloseQuery(False);
            Self.Rollback_TR;
            Inc(fErrorCount);
            Inc(fDDLError);
            if TraceError(e) Then
              Raise;
      end end end end;
  end else
    Result:=0;
  Self.Commit_TR;
  fSchema.Clear;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropProcedures:Integer;
Var MetaProc:TFXMetaProcedure;
    i:Integer;
Begin
  ReStart_TR;
  CheckSchema;
  fSchema.Clear;
 (fSchema as TFXDDLToolSchema).DownloadProcedures(True);
  if fSchema.Procs<>nil then Begin
    Result:=fSchema.Procs.Count;
    for i:=0 to Pred(Result) do Begin
      MetaProc:=TFXMetaProcedure(fSchema.Procs[i]);
      Self.SQL.Text:=MetaProc.AsDummyAlterDDL;
      Self.ParamCheck:=False;
      Self.ExecQuery;
      Self.CloseQuery;
      end;
    Self.ReStart_TR;
    for i:=0 to Pred(Result) do Begin
      MetaProc:=TFXMetaProcedure(fSchema.Procs[i]);
      Self.SQL.Text:=MetaProc.AsDropDDL;
      Self.ParamCheck:=False;
      Self.ExecQuery;
      Self.CloseQuery;
      end;
    Self.Commit_TR;
  end else
    Result:=0;
  fSchema.Clear;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropExceptions:Integer;
Var MetaException:TFXMetaException;
    i:Integer;
Begin
  ReStart_TR;
  CheckSchema;
 (fSchema as TFXDDLToolSchema).DownloadExceptions;
  if fSchema.Exceptions<>nil then Begin
    Result:=fSchema.Exceptions.Count;
    for i:=0 to Pred(Result) do Begin
      MetaException:=TFXMetaException(fSchema.Exceptions[i]);
      Self.SQL.Text:=MetaException.AsDropDDL;
      Self.ParamCheck:=False;
      Self.ExecQuery;
      Self.CloseQuery;
      end;
    Self.Commit_TR;
  end else
    Result:=0;
  fSchema.Clear;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropUDFs:Integer;
Var MetaObj:TFXMetaUDF;
    i:Integer;
Begin
  ReStart_TR;
  CheckSchema;
 (fSchema as TFXDDLToolSchema).DownloadUDFs(False);
  if fSchema.UDFs<>nil then Begin
   (fSchema as TFXDDLToolSchema).DownloadUDFsDependencies;
    Self.Commit_TR;
    Result:=fSchema.UDFs.Count;
    for i:=0 to Pred(Result) do Begin
      Inc(fDDLCount);
      Inc(fStatementCount);
      MetaObj:=TFXMetaUDF(fSchema.UDFs[i]);
      if MetaObj.HasDependencies then Begin
        Assert(not MetaObj.HasDependedOn[moProcedure]);
        DependencyError(MetaObj);
        Inc(fDependencyErrors);
        Inc(fErrorCount);
        Inc(fDDLError);
      end else Begin
        Self.SQL.Text:=MetaObj.AsDropDDL;
        Self.ParamCheck:=False;
        Self.Start_TR;
        try Self.ExecQuery;
            Self.CloseQuery;
            Self.Commit_TR;
        except on e:Exception do Begin
            Self.CloseQuery(False);
            Self.Rollback_TR;
            Inc(fErrorCount);
            Inc(fDDLError);
            if TraceError(e) Then
              Raise;
      end end end end;
  end else
    Result:=0;
  Self.Commit_TR;
  fSchema.Clear;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropViews:Integer;
Var MetaObj:TFXMetaView;
    i:Integer;
Begin
  ReStart_TR;
  CheckSchema;
 (fSchema as TFXDDLToolSchema).DownloadViews;
  if fSchema.Views<>nil then Begin
   (fSchema as TFXDDLToolSchema).DownloadTriggers;
   (fSchema as TFXDDLToolSchema).DownloadProcedures(False);
   (fSchema as TFXDDLToolSchema).DownloadViewsDependencies;
    Self.Commit_TR;
    Result:=fSchema.Views.Count;
    for i:=0 to Pred(Result) do Begin
      Inc(fDDLCount);
      Inc(fStatementCount);
      MetaObj:=TFXMetaView(fSchema.Views[i]);
      if MetaObj.HasDependencies then Begin
        Assert(not MetaObj.HasDependedOn[moView]);
        DependencyError(MetaObj);
        Inc(fDependencyErrors);
        Inc(fErrorCount);
        Inc(fDDLError);
      end else Begin
        Self.SQL.Text:=MetaObj.AsDropDDL;
        Self.ParamCheck:=False;
        Self.Start_TR;
        try Self.ExecQuery;
            Self.CloseQuery;
            Self.Commit_TR;
        except on e:Exception do Begin
            Self.CloseQuery(False);
            Self.Rollback_TR;
            Inc(fErrorCount);
            Inc(fDDLError);
            if TraceError(e) Then
              Raise;
      end end end end;
    Self.Commit_TR;
  end else
    Result:=0;
  fSchema.Clear;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropTiggers:Integer;
Var MetaObj:TFXMetaTrigger;
    i:Integer;
Begin
  ReStart_TR;
  CheckSchema;
  fSchema.Clear;
 (fSchema as TFXDDLToolSchema).DownloadTriggers;
  if fSchema.Triggers<>nil then Begin
    Self.Commit_TR;
    Result:=fSchema.Triggers.Count;
    for i:=0 to Pred(Result) do Begin
      Inc(fDDLCount);
      Inc(fStatementCount);
      MetaObj:=TFXMetaTrigger(fSchema.Triggers[i]);
      Self.SQL.Text:=MetaObj.AsDropDDL;
      Self.ParamCheck:=False;
      Self.Start_TR;
      try Self.ExecQuery;
          Self.CloseQuery;
          Self.Commit_TR;
      except on e:Exception do Begin
          Self.CloseQuery(False);
          Self.Rollback_TR;
          Inc(fErrorCount);
          Inc(fDDLError);
          if TraceError(e) Then
            Raise;
    end end end;
  end else
    Result:=0;
  Self.Commit_TR;
  fSchema.Clear;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropForeignKeys:Integer;
Var TableObj,FKTableObj:TFXCustomMetaRelation;
    MetaObj:TFXMetaForeignKey;
    i:Integer;
Begin
  ReStart_TR;
  CheckSchema;
  fSchema.Clear;
  /// <remark>
  /// May be a full fSchema Download is safer ?
  /// </remark>
 (fSchema as TFXDDLToolSchema).DownloadForeignKeys;
  if fSchema.ForeignKeys<>nil then Begin
   (fSchema as TFXDDLToolSchema).DownloadProcedures(False);
   (fSchema as TFXDDLToolSchema).DownloadForeignKeysDependencies;
    /// <remark>
    /// DownloadForeignKeysDependencies did download linked Obj
    /// </remark>
    Result:=fSchema.ForeignKeys.Count;
    for i:=0 to Pred(Result) do Begin
      Inc(fDDLCount);
      Inc(fStatementCount);
      MetaObj:=TFXMetaForeignKey(fSchema.ForeignKeys[i]);
      if MetaObj.HasDependencies then Begin
        Assert(not MetaObj.HasDependedOn[moForeignKey]);
        DependencyError(MetaObj);
        Inc(fDependencyErrors);
        Inc(fErrorCount);
        Inc(fDDLError);
      end else
      if (MetaObj.Relation=nil)and(not fSchema.FindRelation(MetaObj.RelationName,TableObj)) then Begin
        Echo('**Can Drop '+MetaObj.Name+' Missing Tbl:'+MetaObj.RelationName);
        Inc(fErrorCount);
        Inc(fDDLError);
      End else
      if (MetaObj.FKRelation=nil)and(not fSchema.FindRelation(MetaObj.FKName,FKTableObj)) then Begin
        Echo('**Can Drop '+MetaObj.Name+' Missing FK Tbl:'+MetaObj.FKName);
        Inc(fErrorCount);
        Inc(fDDLError);
      End else Begin
        // MetaObj.Relation:=TableObj;
        Self.SQL.Text:=MetaObj.AsDropDDL;
        Self.ParamCheck:=False;
        Self.Start_TR;
        try Self.ExecQuery;
            Self.CloseQuery;
            Self.Commit_TR;
        except on e:Exception do Begin
            Echo(Self.SQL.Text);
            Self.CloseQuery(False);
            Self.Rollback_TR;
            Inc(fErrorCount);
            Inc(fDDLError);
            if TraceError(e) Then
              Raise;
      end end end end;
  end else
    Result:=0;
  Self.Commit_TR;
  fSchema.Clear;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropIndexes:Integer;
Var TableObj:TFXCustomMetaRelation;
    MetaObj:TFXMetaIndex;
    i:Integer;
Begin
  ReStart_TR;
  CheckSchema;
  fSchema.Clear;
  /// <remark>
  /// May be a full fSchema Download is safer ?
  /// </remark>
 (fSchema as TFXDDLToolSchema).DownloadIndexes;
  if fSchema.Indexes<>nil then Begin
   (fSchema as TFXDDLToolSchema).DownloadProcedures(False);
   (fSchema as TFXDDLToolSchema).DownloadIndexesDependencies;
    /// <remark>
    /// DownloadIndexesDependencies did download linked Obj
    /// </remark>
    Result:=fSchema.Indexes.Count;
    for i:=0 to Pred(Result) do Begin
      Inc(fDDLCount);
      Inc(fStatementCount);
      MetaObj:=TFXMetaIndex(fSchema.Indexes[i]);
      if MetaObj.HasDependencies then Begin
        DependencyError(MetaObj);
        Inc(fDependencyErrors);
        Inc(fErrorCount);
        Inc(fDDLError);
      End else
      if (MetaObj.Relation=nil)and(not fSchema.FindRelation(MetaObj.RelationName,TableObj)) then Begin
        Echo('**Can Drop '+MetaObj.Name+' Missing Tbl:'+MetaObj.RelationName);
        Inc(fErrorCount);
        Inc(fDDLError);
      end else Begin
        Self.SQL.Text:=MetaObj.AsDropDDL;
        Self.ParamCheck:=False;
        Self.Start_TR;
        try Self.ExecQuery;
            Self.CloseQuery;
            Self.Commit_TR;
        except on e:Exception do Begin
            Self.CloseQuery(False);
            Self.Rollback_TR;
            Inc(fErrorCount);
            Inc(fDDLError);
            if TraceError(e) Then
              Raise;
      end end end end;
  end else
    Result:=0;
  Self.Commit_TR;
  fSchema.Clear;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropUnqConstraints:Integer;
Var PrimaryKey:TFXMetaPrimaryKey;
    i:Integer;
Begin
  Result:=0;
  ReStart_TR;
  CheckSchema;
 (fSchema as TFXDDLToolSchema).DownloadPrimaryKeys;
  if fSchema.PrimaryKeys<>nil then Begin
    Result:=fSchema.PrimaryKeys.Count;
    for i:=0 to Pred(fSchema.PrimaryKeys.Count) do Begin
      PrimaryKey:=TFXMetaPrimaryKey(fSchema.PrimaryKeys[i]);
      if not PrimaryKey.IsPrimary then Begin
        Self.SQL.Text:=PrimaryKey.AsDropDDL;
        Self.ParamCheck:=False;
        Self.ExecQuery;
        Self.CloseQuery;
        Inc(Result)
      end end;
    Self.Commit_TR;
    end;
  fSchema.Clear;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropCheckConstraints:Integer;
Var CheckConstraint:TFXMetaCheckConstraint;
    i:Integer;
Begin
  Result:=0;
  ReStart_TR;
  CheckSchema;
 (fSchema as TFXDDLToolSchema).DownloadCheckConstraints;;
  if fSchema.CheckConstraints<>nil then Begin
    Result:=fSchema.CheckConstraints.Count;
    for i:=0 to Pred(fSchema.CheckConstraints.Count) do Begin
      CheckConstraint:=TFXMetaCheckConstraint(fSchema.CheckConstraints[i]);
      Inc(fDDLCount);
      Inc(fStatementCount);
      Self.SQL.Text:=CheckConstraint.AsDropDDL;
      Self.ParamCheck:=False;
      Self.ReStart_TR;
      try Self.ExecQuery;
          Self.CloseQuery;
          Self.Commit_TR;
          Inc(Result);
      except on e:Exception do Begin
          //DependencyList.Add(MetaTable.Name+'.'+MetaTableField.Name);
          Self.CloseQuery(False);
          Self.Rollback_TR;
          Inc(fErrorCount);
          Inc(fDDLError);
          if TraceError(e) Then
            Raise;
    end end end end;
  fSchema.Clear;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropPrimaryKeys:Integer;
Var PrimaryKey:TFXMetaPrimaryKey;
    i:Integer;
Begin
  Result:=0;
  ReStart_TR;
  CheckSchema;
 (fSchema as TFXDDLToolSchema).DownloadPrimaryKeys;
  if fSchema.PrimaryKeys<>nil then Begin
    Result:=fSchema.PrimaryKeys.Count;
    for i:=0 to Pred(fSchema.PrimaryKeys.Count) do Begin
      PrimaryKey:=TFXMetaPrimaryKey(fSchema.PrimaryKeys[i]);
      if PrimaryKey.IsPrimary then Begin
        Self.SQL.Text:=PrimaryKey.AsDropDDL;
        Self.ParamCheck:=False;
        Self.ExecQuery;
        Self.CloseQuery;
        Inc(Result)
      end end;
    Self.Commit_TR;
    end;
  fSchema.Clear;
End;
{______________________________________________________________________________}
function TFXCustomDDLTool.DropComputedFields:Integer;
Var MetaTableField:TFXMetaTableField;
    MetaTable:TFXMetaTable;
    i,j:Integer;
Begin
  Result:=0;
  ReStart_TR;
  CheckSchema;
  fSchema.Clear;
  fSchema.Download(Self.Transaction,True);
  if fSchema.Tables<>nil then Begin
   (fSchema as TFXDDLToolSchema).DownloadTriggers;
   (fSchema as TFXDDLToolSchema).DownloadProcedures(False);
   (fSchema as TFXDDLToolSchema).DownloadComputedFieldsDependencies;
    for i:=0 to Pred(fSchema.Tables.Count) do Begin
      MetaTable:=TFXMetaTable(fSchema.Tables[i]);
      for j:=0 to Pred(MetaTable.Fields.Count) do Begin
        MetaTableField:=TFXMetaTableField(MetaTable.Fields[j]);
        if MetaTableField.IsComputed then Begin
          Inc(Result);
          Inc(fDDLCount);
          Inc(fStatementCount);
          if MetaTableField.HasDependencies then Begin
            DependencyError(MetaTableField);
            Inc(fDependencyErrors);
            Inc(fErrorCount);
            Inc(fDDLError);
          end else Begin
            Self.SQL.Text:=MetaTableField.AsDropDDL;
            Self.ParamCheck:=False;
            Self.Start_TR;
            try Self.ExecQuery;
                Self.CloseQuery;
                Self.Commit_TR;
            except on e:Exception do Begin
                Self.CloseQuery(False);
                Self.Rollback_TR;
                Inc(fErrorCount);
                Inc(fDDLError);
                if TraceError(e) Then
                  Raise;
      end end end end end end;
    Self.Commit_TR;
    end;
  fSchema.Clear;
End;
{______________________________________________________________________________}
procedure TFXCustomDDLTool.DependencyError(Const aMetaObj:TFXCustomMetaNode);
Begin
  Assert(aMetaObj<>nil);
  Assert(aMetaObj.HasDependencies);
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXDDLTool.Echo(Const aMsg:String);
Begin
  if Assigned(fOnEcho) Then
    fOnEcho(Self,aMsg)
end;
{______________________________________________________________________________}
procedure TFXDDLTool.Trace(Const aMsg:String);
Begin
  if Assigned(fOnTrace) Then
    fOnTrace(Self,aMsg)
end;
{______________________________________________________________________________}
procedure TFXDDLTool.TraceResult(const aMsg:String);
Begin
  if Assigned(fOnTraceResult) then
    fOnTraceResult(Self,aMsg);
end;
{______________________________________________________________________________}
function TFXDDLTool.TraceError(Const e:Exception):Boolean;
Begin
  Result:=Self.StopOnError;
  if Assigned(fOnError) Then
    fOnError(Self,e,Result)
end;
{______________________________________________________________________________}
procedure TFXDDLTool.DependencyError(Const aMetaObj:TFXCustomMetaNode);
Begin
  Assert(aMetaObj<>nil);
  Assert(aMetaObj.HasDependencies);
  if Assigned(fOnDependencyError) Then
    fOnDependencyError(Self,aMetaObj)
End;


end.

