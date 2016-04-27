unit frxmFXRTTI;

interface

{$I mFX.Inc}

implementation

uses System.Types, System.Classes, System.SysUtils, System.Variants, Data.DB, Datasnap.DBClient,
  {$IFDEF MSWINDOWS}Winapi.Windows,Winapi.Messages,{$ENDIF}
  mFX.Header, mFX.Classes, mFX.Base, mFX.Database, mFX.SQL, mFX.ClientDataSet,
  frxClass, frxCustomDB, fs_iinterpreter, frxMFXComponents;

type
  TFXFR4Functions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;const MethodName: String; var Params: Variant): Variant;

    function CallDBMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetDataBaseProp(Instance: TObject; ClassType: TClass;const PropName: String): Variant;
    procedure SetDataBaseProp(Instance: TObject; ClassType: TClass;const PropName: String; Value: Variant);

    function CallTRMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;
    function CallTransactionMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetTransactionProp(Instance: TObject; ClassType: TClass;const PropName: String): Variant;
    procedure SetTransactionProp(Instance: TObject; ClassType: TClass;const PropName: String; Value: Variant);

    function CallFXClientDataSetMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;

    function GetSQLBaseProp(Instance: TObject; ClassType: TClass;const PropName: String): Variant;
    procedure SetSQLBaseProp(Instance: TObject; ClassType: TClass;const PropName: String; Value: Variant);

    function CallSQLMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetSQLProp(Instance: TObject; ClassType: TClass;const PropName: String): Variant;
    procedure SetSQLProp(Instance: TObject; ClassType: TClass;const PropName: String; Value: Variant);

    function CallSQLVARMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetSQLVARProp(Instance: TObject; ClassType: TClass;const PropName: String): Variant;
    procedure SetSQLVARProp(Instance: TObject; ClassType: TClass;const PropName: String; Value: Variant);

    function GetFRXmFXDatabase(Instance: TObject; ClassType: TClass;const PropName: String): Variant;

    function GetFRXDataSetProp(Instance: TObject; ClassType: TClass;const PropName: String): Variant;

    function CallFRXDataSetMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;

  protected
    Function RegisterDataset(Const DataSet:TfrxCustomQuery):Boolean;

  public
    constructor Create(AScript: TfsScript); override;
  end;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXFR4Functions.Create;
begin
  inherited Create(AScript);

  with AScript do begin

    with AddClass(TFXCustomDatabase ,'TComponent') do Begin
      AddConstructor('constructor CreateEx', CallDBMethod);
      AddConstructor('constructor Create(AOwner: TComponent)', CallDBMethod);
      AddProperty('Params'         , 'TStrings'             , GetDataBaseProp   , SetDataBaseProp);
      End;

    with AddClass(TFXCustomTransaction,'TComponent') do Begin
      AddConstructor('constructor CreateEx', CallTRMethod);
      AddConstructor('constructor Create(AOwner: TComponent)', CallTRMethod);
      AddMethod('procedure Commit', CallTransactionMethod);
      AddMethod('procedure RollBack', CallTransactionMethod);
      AddProperty('DefaultDatabase', 'TFXCustomDatabase'    , GetTransactionProp, SetTransactionProp);
      End;

    with AddClass(TFXSQLVAR,'TObject') do Begin
      AddMethod('function GetAsString:String', CallSQLVARMethod);
      AddMethod('procedure SetAsString(Const Value:String)', CallSQLVARMethod);
      AddMethod('function GetAsInteger:Integer', CallSQLVARMethod);
      AddMethod('procedure SetAsInteger(Const Value:Integer)', CallSQLVARMethod);
      AddMethod('function GetAsInt64:Int64', CallSQLVARMethod);
      AddMethod('procedure SetAsInt64(Const Value:Int64)', CallSQLVARMethod);
      AddMethod('function GetAsDateTime:TDateTime', CallSQLVARMethod);
      AddMethod('procedure SetAsDateTime(Const Value:TDateTime)', CallSQLVARMethod);
      AddMethod('function GetAsTime:TDateTime', CallSQLVARMethod);
      AddMethod('procedure SetAsTime(Const Value:TDateTime)', CallSQLVARMethod);
      AddMethod('function GetAsDate:TDateTime', CallSQLVARMethod);
      AddMethod('procedure SetAsDate(Const Value:TDateTime)', CallSQLVARMethod);
      AddMethod('function GetAsVariant:Variant', CallSQLVARMethod);
      AddMethod('procedure SetAsVariant(Const Value:Variant)', CallSQLVARMethod);
      AddMethod('function GetAsFloat:Double', CallSQLVARMethod);
      AddMethod('procedure SetAsFloat(Const Value:Double)', CallSQLVARMethod);
      AddMethod('function IsNull:Boolean', CallSQLVARMethod);
      AddProperty('AsString'       , 'string'               , GetSQLVARProp     , SetSQLVARProp  );
      AddProperty('AsInteger'      , 'Integer'              , GetSQLVARProp     , SetSQLVARProp  );
      AddProperty('AsInt64'        , 'Int64'                , GetSQLVARProp     , SetSQLVARProp  );
      AddProperty('AsDateTime'     , 'TDateTime'            , GetSQLVARProp     , SetSQLVARProp  );
      AddProperty('AsDate'         , 'TDateTime'            , GetSQLVARProp     , SetSQLVARProp  );
      AddProperty('AsTime'         , 'TDateTime'            , GetSQLVARProp     , SetSQLVARProp  );
      AddProperty('AsVariant'      , 'Variant'              , GetSQLVARProp     , SetSQLVARProp  );
      AddProperty('AsFloat'        , 'Double'               , GetSQLVARProp     , SetSQLVARProp  );
      end;

    with AddClass(TFXSQLBase,'TComponent') do Begin
      AddProperty('Database'       , 'TFXCustomDatabase'    , GetSQLBaseProp    , SetSQLBaseProp );
      AddProperty('Transaction'    , 'TFXCustomTransaction' , GetSQLBaseProp    , SetSQLBaseProp );
      end;

    with AddClass(TFXCustomSQL,'TFXSQLBase') do Begin
      AddConstructor('constructor CreateEx', CallSQLMethod);
      AddConstructor('constructor Create(AOwner: TComponent)', CallSQLMethod);
      AddMethod('procedure Start_TR', CallSQLMethod);
      AddMethod('procedure Commit_TR', CallSQLMethod);
      AddMethod('procedure ReStart_TR', CallSQLMethod);
      AddMethod('procedure Prepare', CallSQLMethod);
      AddMethod('procedure ExecQuery', CallSQLMethod);
      AddMethod('function Next: TFXSQLDA', CallSQLMethod);
      AddMethod('function Eof: Boolean', CallSQLMethod);
      AddMethod('procedure CloseQuery', CallSQLMethod);
      AddMethod('procedure Close', CallSQLMethod);
      //FindField may return NIL iff field not found
      AddMethod('function FindField(FieldName: String): TFXSQLVAR', CallSQLMethod);
      //FindField may boom iff field not found
      AddMethod('function FieldByName(FieldName: String): TFXSQLVAR', CallSQLMethod);
      //Fields may boom iff field not found
      AddDefaultProperty('Fields', 'Integer', 'TFXSQLVAR', CallSQLMethod, True);
      //Params may boom iff Param not found
      AddIndexProperty('Params', 'Integer', 'TFXSQLVAR', CallSQLMethod, True);

      AddProperty('SQL'            , 'TStrings'             , GetSQLProp        , SetSQLProp);
      AddProperty('SQLText'        , 'String'               , GetSQLProp        , SetSQLProp);
      end;

    with AddClass(TFXSQL,'TFXCustomSQL') do Begin
      end;


    With AddClass(TFXClientDataSet,'TDataSet') do Begin
      AddMethod('procedure CloneCursor(Source: TCustomClientDataSet)', CallFXClientDataSetMethod);
      end;

    with AddClass(TfrxmFXDatabase,'TfrxCustomDatabase') do Begin
      AddProperty('Database'       , 'TFXCustomDatabase'    ,GetFRXmFXDatabase  , nil               );
      end;

    with AddClass(TfrxmFXClientDataSet, 'TfrxCustomQuery') do Begin
      AddMethod('procedure Export(Const aDir,aFileName:String)', CallFRXDataSetMethod);
      AddMethod('procedure PushQuery(Const aSQL:TFXCustomSQL;Const ReStart:Boolean)', CallFRXDataSetMethod);
      AddMethod('procedure CloneCursor(Source: TfrxmFXClientDataSet)', CallFRXDataSetMethod);
      AddMethod('procedure Post', CallFRXDataSetMethod);
      AddMethod('procedure Edit', CallFRXDataSetMethod);
      AddMethod('procedure Insert', CallFRXDataSetMethod);
      AddProperty('ClientDataSet'  ,'TFXClientDataSet'      , GetFRXDataSetProp , nil               );
      end;

    AddMethod(
      'function RegisterDataset(Const DataSet:TfrxCustomQuery):Boolean;',
      CallMethod,
      'Register Dynamic Dataset into the Engine',
      'Return True if added , False if allready register'
    );
    end;
end;
{______________________________________________________________________________}
function TFXFR4Functions.CallMethod(Instance: TObject; ClassType: TClass;const MethodName: String; var Params: Variant): Variant;
Var obj:TObject;
Begin
  if SameText(MethodName,'RegisterDataset') then Begin
    obj:=TObject(frxInteger(Params[0]));
    Result := RegisterDataset(obj as TfrxCustomQuery)
  end else
    Result := Null;
end;
{______________________________________________________________________________}
function TFXFR4Functions.CallDBMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;
Var i,db:TFXCustomDatabase;
    owner:TComponent;
    obj:TObject;
begin
  if SameText(MethodName,'CREATE') or SameText(MethodName,'CREATEEX') then Begin
    if Caller.Count=1 then Begin
      obj:=TObject(frxInteger(Caller.Params[0]));
      owner:=Obj as TComponent
    end else
      owner:=nil;
    i:=TFXCustomDatabase(Instance);
    db:=i.Create(owner);
    Result:=frxInteger(db);
    exit;
    end;

  if Instance=nil then Begin
    Result := 0;
    exit;
    End;

  Result := 'Not Supported';
end;
{______________________________________________________________________________}
function TFXFR4Functions.GetDataBaseProp(Instance: TObject; ClassType: TClass;const PropName: String): Variant;
Var db:TFXCustomDatabase;
Begin
  Result:=0;
  db:=TFXCustomDatabase(Instance);
  if SameText(PropName,'Params') then
    Result:=frxInteger(db.Params)
End;
{______________________________________________________________________________}
procedure TFXFR4Functions.SetDataBaseProp(Instance: TObject; ClassType: TClass;const PropName: String; Value: Variant);
Var db:TFXCustomDatabase;
Begin
  db:=TFXCustomDatabase(Instance);
  if SameText(PropName,'Params') then
    db.Params:=TStrings(Integer(Value))
End;
{______________________________________________________________________________}
function TFXFR4Functions.CallTRMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;
Var tr,i:TFXCustomTransaction;
    owner:TComponent;
    obj:TObject;
begin
  if SameText(MethodName,'CREATE') or SameText(MethodName,'CREATEEX') then Begin
    if Caller.Count=1 then Begin
      obj:=TObject(frxInteger(Caller.Params[0]));
      owner:=obj as TComponent
    end else
      owner:=nil;
    i:=TFXCustomTransaction(Instance);
    tr:=i.Create(owner);
    Result:=frxInteger(tr);
    exit;
    end;

  if Instance=nil then Begin
    Result := 0;
    exit;
    End;

  Result := 'Not Supported';
end;
{______________________________________________________________________________}
function TFXFR4Functions.GetTransactionProp(Instance: TObject; ClassType: TClass;const PropName: String): Variant;
Var TR:TFXCustomTransaction;
Begin
  Result:=0;
  tr:=TFXCustomTransaction(Instance);
  if SameText(PropName,'DefaultDatabase') then
    Result:=frxInteger(tr.DefaultDatabase)
End;
{______________________________________________________________________________}
procedure TFXFR4Functions.SetTransactionProp(Instance: TObject; ClassType: TClass;const PropName: String; Value: Variant);
Var TR:TFXCustomTransaction;
    obj:TObject;
Begin
  tr:=TFXCustomTransaction(Instance);
  if SameText(PropName,'DefaultDatabase') then Begin
    obj:=TObject(frxInteger(Value));
    tr.DefaultDatabase:=obj as TFXCustomDatabase
    end;
End;
{______________________________________________________________________________}
function TFXFR4Functions.CallTransactionMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;
Var TR:TFXCustomTransaction;
Begin
  tr:=TFXCustomTransaction(Instance);
  if SameText(MethodName,'Commit') then Begin
    tr.Commit;
    exit;
    End;
  if SameText(MethodName,'RollBack') then Begin
    tr.RollBack;
    exit;
    End;
End;
{______________________________________________________________________________}
function TFXFR4Functions.GetFRXmFXDatabase(Instance: TObject; ClassType: TClass;const PropName: String): Variant;
Var MFXDb:TfrxmFXDatabase;
begin
  if Instance=nil then Begin
    Result := 0;
    exit;
    End;

  Assert(Instance is TfrxmFXDatabase);
  MFXDb:=TfrxmFXDatabase(Instance);
  if SameText(PropName,'DATABASE') then Begin
    Result:=frxInteger(MFXDb.Database);
    exit;
    End;

  Result:='Not Supported';
end;
{______________________________________________________________________________}
function TFXFR4Functions.GetFRXDataSetProp(Instance: TObject; ClassType: TClass;const PropName: String): Variant;
Var MFXCds:TfrxmFXClientDataSet;
begin
  if Instance=nil then Begin
    Result := 0;
    exit;
    End;

  Assert(Instance is TfrxmFXClientDataSet);
  MFXCds:=TfrxmFXClientDataSet(Instance);
  if SameText(PropName,'CLIENTDATASET') then Begin
    Result:=frxInteger(MFXCds.ClientDataSet);
    exit;
    End;

  Result:='Not Supported';
end;
{______________________________________________________________________________}
function TFXFR4Functions.CallFXClientDataSetMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;
Var SourceDataSet:TCustomClientDataSet;
    cds:TFXClientDataSet;
    obj:TObject;
Begin
  Result:=0;
  cds:=TFXClientDataSet(Instance);
  if SameText(MethodName,'CloneCursor') then Begin
    obj:=TObject(frxInteger(Caller.Params[0]));
    SourceDataSet:=obj as TCustomClientDataSet;
    cds.CloneCursor(SourceDataSet,True,True);
    exit;
    End;
End;
{______________________________________________________________________________}
function TFXFR4Functions.CallFRXDataSetMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;
Var SourceDataSet:TfrxmFXClientDataSet;
    ds:TfrxmFXClientDataSet;
    SQL:TFXCustomSQL;
    ReStart:Boolean;
    fd,fn,ff:String;
    obj:TObject;
begin
  Result:=0;

  ds:=TfrxmFXClientDataSet(Instance);
  if SameText(MethodName,'Edit') then Begin
    ds.ClientDataSet.Edit
  end else
  if SameText(MethodName,'Post') then Begin
    ds.ClientDataSet.Post
  end else
  if SameText(MethodName,'Insert') then Begin
    ds.ClientDataSet.Insert
  end else
  if SameText(MethodName,'PushQuery') then Begin
    obj:=TObject(frxInteger(Caller.Params[0]));
    SQL:=obj as TFXCustomSQL;
    ReStart:=Caller.Params[1];
    ds.LockParams;
    try ds.ClientDataSet.PushQuery(SQL,ReStart);
    finally
        ds.UnLockParams;
    end;
  end else
  if SameText(MethodName,'CloneCursor') then Begin
    obj:=TObject(frxInteger(Caller.Params[0]));
    SourceDataSet:=obj as TfrxmFXClientDataSet;
    ds.CloneCursor(SourceDataSet.ClientDataSet);
  end else
  if SameText(MethodName,'Export') then Begin
    fd:=Caller.Params[0];
    fn:=Caller.Params[1];
    ff:=IncludeTrailingPathDelimiter(fd)+fn;
//    mFX.Export.TFXExport.DoExport(ds.ClientDataSet,ff,fn,EmptyStr,xfXLS,[xpRemoveFilter,xpExportFieldName]);
  end else
end;
{______________________________________________________________________________}
function TFXFR4Functions.GetSQLBaseProp(Instance: TObject; ClassType: TClass;const PropName: String): Variant;
Var sql:TFXSQLBase;
begin
  if Instance=nil then Begin
    Result := 0;
    exit;
    End;

  Assert(Instance is TFXSQLBase);
  sql:=TFXSQLBase(Instance);
  if SameText(PropName,'Database') then Begin
    Result:=frxInteger(SQL.Database);
    exit;
    end;
  if SameText(PropName,'Transaction') then Begin
    Result:=frxInteger(SQL.Transaction);
    exit;
    end;
  Result := 0;
end;
{______________________________________________________________________________}
procedure TFXFR4Functions.SetSQLBaseProp(Instance: TObject; ClassType: TClass;const PropName: String; Value: Variant);
Var sql:TFXSQLBase;
    obj:TObject;
begin
  if Instance=nil then Begin
    exit;
    End;

  Assert(Instance is TFXSQLBase,Instance.ClassName);
  sql:=TFXSQLBase(Instance);
  if SameText(PropName,'Database') then Begin
    obj:=TObject(frxInteger(Value));
    SQL.Database:=obj as TFXCustomDatabase;
    exit;
    end;

  if SameText(PropName,'Transaction') then Begin
    obj:=TObject(frxInteger(Value));
    SQL.Transaction:=obj as TFXCustomTransaction;
    exit;
    end;
end;
{______________________________________________________________________________}
function TFXFR4Functions.CallSQLMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;
Var i,sql:TFXCustomSQL;
    owner:TComponent;
    obj:TObject;
    j:Integer;
    s:String;
begin
  if SameText(MethodName,'CREATE') or SameText(MethodName,'CREATEEX') then Begin
    if Caller.Count=1 then Begin
      obj:=TObject(frxInteger(Caller.Params[0]));
      owner:=obj as TComponent;
    end else
      owner:=nil;
    i:=TFXCustomSQL(Instance);
    sql:=i.Create(owner);
    Result:=frxInteger(sql);
    exit;
    end;

  if Instance=nil then Begin
    Result := 0;
    exit;
    End;

  Assert(Instance is TFXCustomSQL);
  sql:=TFXCustomSQL(Instance);
  if SameText(MethodName,'RESTART_TR') then Begin
    sql.ReStart_TR;
    Result := 0;
    exit;
    end;
  if SameText(MethodName,'START_TR') then Begin
    sql.Start_TR;
    Result := 0;
    exit;
    end;
  if SameText(MethodName,'COMMIT_TR') then Begin
    sql.Commit_TR;
    Result := 0;
    exit;
    end;

  if SameText(MethodName,'PREPARE') then Begin
    sql.Start_TR;
    sql.Prepare;
    Result := 0;
    exit;
    end;

  if SameText(MethodName,'EXECQUERY') then Begin
    sql.Start_TR;
    sql.ExecQuery;
    Result := 0;
    exit;
    end;

  if SameText(MethodName,'NEXT') then Begin
    Result:=frxInteger(sql.Next);
    exit;
    end;

  if SameText(MethodName,'EOF') then Begin
    Result:=sql.Eof;
    exit;
    end;

  if SameText(MethodName,'CLOSE') or SameText(MethodName,'CLOSEQUERY') then Begin
    sql.CloseQuery;
    Result := 0;
    exit;
    end;

  if SameText(MethodName,'FIELDS.GET') then Begin
    j:=Caller.Params[0];
    Result:=frxInteger(sql.Fields[j]);
    exit;
    end;

  if SameText(MethodName,'PARAMS.GET') then Begin
    sql.Start_TR;
    j:=Caller.Params[0];
    Result:=frxInteger(sql.Params[j]);
    exit;
    end;

  if SameText(MethodName,'FINDFIELD') then Begin
    s:=Caller.Params[0];
    Result:=frxInteger(sql.FindField(s));
    exit;
    end;

  if SameText(MethodName,'FIELDBYNAME') then Begin
    s:=Caller.Params[0];
    Result:=frxInteger(sql.FieldByName(s));
    exit;
    end;

  Result := 'Not Supported';
end;
{______________________________________________________________________________}
function TFXFR4Functions.GetSQLProp(Instance: TObject; ClassType: TClass;const PropName: String): Variant;
Var sql:TFXCustomSQL;
begin
  if Instance=nil then Begin
    Result := 0;
    exit;
    End;
  Assert(Instance is TFXCustomSQL);
  sql:=TFXCustomSQL(Instance);
  if SameText(PropName,'SQL') then Begin
    Result:=frxInteger(SQL.SQL);
    exit;
    end;
  if SameText(PropName,'SQLText') then Begin
    Result:=SQL.SQL.Text;
    exit;
    end;
  Result := 0;
end;
{______________________________________________________________________________}
procedure TFXFR4Functions.SetSQLProp(Instance: TObject; ClassType: TClass;const PropName: String; Value: Variant);
Var sql:TFXCustomSQL;
    obj:TObject;
begin
  if Instance=nil then Begin
    exit;
    End;

  Assert(Instance is TFXCustomSQL);
  sql:=TFXCustomSQL(Instance);
  if SameText(PropName,'SQL') then Begin
    obj:=TObject(frxInteger(Value));
    SQL.SQL.Assign(obj as TPersistent);
    exit;
    end;
  if SameText(PropName,'SQLText') then Begin
    SQL.SQL.Text:=Value;
    exit;
    end;
end;
{______________________________________________________________________________}
function TFXFR4Functions.GetSQLVARProp(Instance: TObject; ClassType: TClass;const PropName: String): Variant;
Var sqlvar:TFXSQLVAR;
begin
  Result := 0;
  if Instance=nil then Begin
    exit;
    End;
  Assert(Instance is TFXSQLVAR);
  sqlvar:=TFXSQLVAR(Instance);
  if SameText(PropName,'ASSTRING') then Begin
    Result:=sqlvar.AsString;
    exit;
    end;
  if SameText(PropName,'ASINTEGER') then Begin
    Result:=sqlvar.AsInteger;
    exit;
    end;
  if SameText(PropName,'ASINT64') then Begin
    Result:=sqlvar.AsInt64;
    exit;
    end;
  if SameText(PropName,'ASFLOAT') then Begin
    Result:=sqlvar.AsDouble;
    exit;
    end;
  if SameText(PropName,'ASDATE') then Begin
    Result:=sqlvar.AsDate;
    exit;
    end;
  if SameText(PropName,'ASTIME') then Begin
    Result:=sqlvar.AsTime;
    exit;
    end;
  if SameText(PropName,'ASDATETIME') then Begin
    Result:=sqlvar.AsDateTime;
    exit;
    end;
  if SameText(PropName,'ASVARIANT') then Begin
    Result:=sqlvar.AsVariant;
    exit;
    end;
End;
{______________________________________________________________________________}
procedure TFXFR4Functions.SetSQLVARProp(Instance: TObject; ClassType: TClass;const PropName: String; Value: Variant);
Var sqlvar:TFXSQLVAR;
    d:TDateTime;
    i:Integer;
    i64:Int64;
    e:Double;
    s:String;
begin
  if Instance=nil then Begin
    exit;
    End;
  Assert(Instance is TFXSQLVAR);
  sqlvar:=TFXSQLVAR(Instance);
  if SameText(PropName,'ASSTRING') then Begin
    s:=Value;
    sqlvar.AsString:=s;
    exit;
    end;
  if SameText(PropName,'ASINTEGER') then Begin
    i:=Value;
    sqlvar.AsInteger:=i;
    exit;
    end;
  if SameText(PropName,'ASINT64') then Begin
    i64:=Value;
    sqlvar.AsInt64:=i64;
    exit;
    end;
  if SameText(PropName,'ASFLOAT') then Begin
    e:=Value;
    sqlvar.AsDouble:=e;
    exit;
    end;
  if SameText(PropName,'ASDATE') then Begin
    d:=Value;
    sqlvar.AsDate:=d;
    exit;
    end;
  if SameText(PropName,'ASTIME') then Begin
    d:=Value;
    sqlvar.AsTime:=d;
    exit;
    end;
  if SameText(PropName,'ASDATETIME') then Begin
    d:=Value;
    sqlvar.AsDateTime:=d;
    exit;
    end;
  if SameText(PropName,'ASVARIANT') then Begin
    sqlvar.Value:=Value;
    exit;
    end;
End;
{______________________________________________________________________________}
function TFXFR4Functions.CallSQLVARMethod(Instance: TObject; ClassType: TClass;const MethodName: String; Caller: TfsMethodHelper): Variant;
Var sqlvar:TFXSQLVAR;
    d:TDateTime;
    i:Integer;
    i64:Int64;
    e:Double;
    s:String;
begin
  Result := 0;
  if Instance=nil then Begin
    exit;
    End;

  Assert(Instance is TFXSQLVAR);
  sqlvar:=TFXSQLVAR(Instance);
  if SameText(MethodName,'GETASSTRING')then Begin
    Result:=sqlvar.AsString;
    exit;
    end;
  if SameText(MethodName,'SETASSTRING')then Begin
    s:=Caller.Params[0];
    sqlvar.AsString:=s;
    exit;
    end;

  if SameText(MethodName,'GETASINTEGER') then Begin
    Result:=sqlvar.AsInteger;
    exit;
    end;
  if SameText(MethodName,'SETASINTEGER') then Begin
    i:=Caller.Params[0];
    sqlvar.AsInteger:=i;
    exit;
    end;

  if SameText(MethodName,'GETASINT64') then Begin
    Result:=sqlvar.AsInt64;
    exit;
    end;
  if SameText(MethodName,'SETASINT64') then Begin
    i64:=Caller.Params[0];
    sqlvar.AsInt64:=i64;
    exit;
    end;

  if SameText(MethodName,'GETASFLOAT') then Begin
    Result:=sqlvar.AsDouble;
    exit;
    end;
  if SameText(MethodName,'SETASFLOAT') then Begin
    e:=Caller.Params[0];
    sqlvar.AsDouble:=e;
    exit;
    end;

  if SameText(MethodName,'GETASDATE') then Begin
    Result:=sqlvar.AsDate;
    exit;
    end;
  if SameText(MethodName,'SETASDATE') then Begin
    d:=Caller.Params[0];
    sqlvar.AsDate:=d;
    exit;
    end;

  if SameText(MethodName,'GETASTIME') then Begin
    Result:=sqlvar.AsTime;
    exit;
    end;
  if SameText(MethodName,'SETASTIME') then Begin
    d:=Caller.Params[0];
    sqlvar.AsTime:=d;
    exit;
    end;

  if SameText(MethodName,'GETASDATETIME') then Begin
    Result:=sqlvar.AsDateTime;
    exit;
    end;
  if SameText(MethodName,'SETASDATETIME') then Begin
    d:=Caller.Params[0];
    sqlvar.AsDateTime:=d;
    exit;
    end;

  if SameText(MethodName,'GETASVARIANT') then Begin
    Result:=sqlvar.AsVariant;
    exit;
    end;
  if SameText(MethodName,'SETASVARIANT') then Begin
    sqlvar.Value:=Caller.Params[0];
    exit;
    end;

  if SameText(MethodName,'ISNULL') then Begin
    Result:=sqlvar.IsNull;
    exit;
    end;
end;
{______________________________________________________________________________}
Function TFXFR4Functions.RegisterDataset(Const DataSet:TfrxCustomQuery):Boolean;
Var varReport:TfsCustomVariable;
    frxReport:TfrxReport;
    obj:TObject;
begin
  varReport:=Self.Script.Find('Report');
  obj:=TObject(frxInteger(varReport.Value));
  frxReport:=obj as TfrxReport;
  if frxReport.DataSets.Find(DataSet)=nil then Begin
    frxReport.DataSets.Add(DataSet);
    Result:=True;
  End else
    Result:=False;
end;

initialization

  fsRTTIModules.Add(TFXFR4Functions);

finalization

  if fsRTTIModules<>nil then
    fsRTTIModules.Remove(TFXFR4Functions);

end.
