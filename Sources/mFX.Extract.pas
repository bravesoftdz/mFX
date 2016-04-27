unit mFX.Extract;

interface

{$I mFX.Inc}

uses
  SysUtils, Classes,
  mFX.Intf, mFX.Utils, mFX.Header, mFX.Classes, mFX.Base, mFX.SQL;

type
  TExtractObjectTypes = (
    eoDatabase,
    eoDomain,
    eoTable, eoView,
    eoProcedure, eoFunction,
    eoGenerator, eoException,
    eoBLOBFilter,
    eoRole,
    eoTrigger,
    eoForeign,
    eoIndexes,
    eoChecks,
    eoData
    );

  TExtractType = (
    etDomain,
    etTable,
    etRole,
    etTrigger,
    etForeign,
    etIndex,
    etData,
    etGrant,
    etCheck,
    etAlterProc
    );
  TExtractTypes = Set of TExtractType;

  TFXCustomExtract = class(TFXCustomSQL)
  private
    FShowSystem     : Boolean;
    fTerminator     : Char;
    FMetaData       : TStrings;
    fODSMajorVersion: FXLong;
    fPageSize       : FXLong;
    fDBSQLDialect   : FXLong;
    {:Add AddBlankLine }
    procedure AddBlankLine;
    {:Add AddCmd }
    procedure AddCmd(Value:String);overload;
    {:Add AddCmd }
    procedure AddCmd(Values:TStrings);overload;
    {:Add AddComment }
    procedure AddComment(Value:String);overload;
    {:Add AddComment }
    procedure AddComment(Values:TStrings);overload;
    {:Add the set term iff needed }
    procedure SetTerminator(Const Value:Char);
    {:Add the set Dialect iff needed }
    procedure CheckSetDialect;
    {:Add the Commit works }
    procedure AddCommitWorks;
    {:QuoteString}
    function QuoteString(Value: String): String;
    {:QuoteIdentifier}
    function QuoteIdentifier(Value: String): String;
    {:returns the list of columns in an index. }
    function GetIndexSegments ( indexname : String) : String;
    {:Check if Name is a system Name like RDB$xxx}
    function IsUserDomain(Const Value:String):Boolean;
    {:FieldType To SQLType }
    function FieldType2SQLType(Const Value:Integer;Out SQLType:FXShort):Boolean;
    {:GetArrayField : Retrieves the dimensions of arrays and prints them.}
    function GetArrayField(Const FieldName:String):String;
    {:Process Field without User Domain  }
    function ProcessColumn(Const ArrayColumn:Boolean=False):String;
    {:Process Computed Field }
    function ProcessComputedColumn:String;
    {:Process User Domain Field }
    function ProcessUserDomainColumn:String;
    {:Process Array Field }
    function ProcessArrayColumn:String;
    {:Process Caracter Set }
    function ProcessCaracterSet(Const f:TFXSQLVAR):String;
    {:Process Collation }
    function ProcessCollation:String;
    {:Process Default value for a field }
    function ProcessDefault:String;
    {:Process Null Flag for a field}
    function ProcessNullFlag:String;
    {:Process Stored Proc Params}
    function ProcessStoredProcParams(Const qryHeader:TFXCustomSQL):String;
    {:Process UDF Params}
    procedure ProcessUDFParams(Const FctName:String);
    {:Process UDF Results}
    procedure ProcessUDFResults(Const FctName:String);
    {:CreateSQL  }
    function CreateSQL : TFXCustomSQL;
    {:ShowGrants}
    procedure ShowGrants(Const MetaObject: String);
    {:ShowGrantRoles}
    procedure ShowGrantRoles;
    {:GetProcedureArgs:This function extract the procedure parameters and adds it to the extract file}
    procedure GetProcedureArgs(Proc : String);
    {:GetFieldLength}
    function GetFieldLength(sql : TFXCustomSQL) : Integer;
    {:GetLongDatabaseInfo  }
    function GetLongDatabaseInfo(Const Value : Byte):FXLong;

    {:LoadDBInfo ODS Version, ....}
    procedure LoadDBInfo;
    {:ExtractListTable = Shows columns, types, info for a given table name and text of views.}
    function ExtractListTable(Const RelationName,NewName:String;Const DomainFlag:Boolean):Boolean;
    {:ExtractListView = Show text of the specified view.}
    procedure ExtractListView(Const ViewName,NewName:String);
    {:ListData}
    procedure ListData(ObjectName:String);
    {:ListRoles}
    procedure ListRoles(ObjectName:String = '');
    {:Print the permissions on all user tables.Get separate permissions on table/views and then procedures }
    procedure ListGrants;
    {:ListProcs:Shows text of a stored procedure given a name. or lists procedures if no argument.}
    procedure ListProcs(Const ProcedureName:String='';Const AlterOnly:Boolean = false);
    {:ListAllTables: Extract the names of all user tables from rdb$relations.}
    procedure ListAllTables(Const flag:Boolean);
    {:ListTriggers: Lists triggers in general on non-system tables with sql source only}
    procedure ListTriggers(ObjectName:String = ''; ExtractType:TExtractType = etTrigger);
    {:ListCheck:List check constraints for all objects to allow forward references }
    procedure ListCheck(ObjectName:String = ''; ExtractType:TExtractType = etCheck);
    {:PrintSet}
    function PrintSet(var Used:Boolean):String;
    {:ListCreateDb:Print the create database command if requested.  At least put the page size in a comment with the extracted db name}
    procedure ListCreateDb(Const NewDataBase:String = '');
    {:ListDomains}
    procedure ListDomains(Const ObjectName:String = ''; ExtractType:TExtractType = etDomain);
    {:ListException}
    procedure ListException(ExceptionName:String = '');
    {:ListFilters:List all blob filters}
    procedure ListFilters(FilterName:String = '');
    {:ListForeign}
    procedure ListForeign(ObjectName:String = ''; ExtractType:TExtractType = etForeign);
    {:ListFunctions:List all external functions}
    procedure ListFunctions(Const FunctionName:String = '');
    {:ListGenerators}
    procedure ListGenerators(GeneratorName:String = '');
    {:ListIndex}
    procedure ListIndex(ObjectName:String = ''; ExtractType:TExtractType = etIndex);
    {:ListViews}
    procedure ListViews(ViewName:String = '');

  protected
    {:GetFieldType}
    function GetFieldType(FieldType, FieldSubType, FieldScale, FieldSize,FieldPrec, FieldLen:Integer):String;
    {:GetCharacterSets}
    function GetCharacterSets(Const CharSetId, Collation:FXShort;Const CollateOnly:Boolean):String;
    {:ExtractObject}
    procedure ExtractObject(ObjectType:TExtractObjectTypes; ObjectName:String = '';ExtractTypes:TExtractTypes = []);

    property Items     :TStrings      read fMetaData;
    property ShowSystem: Boolean      read FShowSystem write FShowSystem;
    property Terminator: Char         read fTerminator;

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    {:ExtractDDL}
    function ExtractDDL(Const Flag : Boolean;TableName:String):Boolean;

  end;

  TFXExtract = Class(TFXCustomExtract)
  public
    property Items;    
    property Terminator;
  published
    property ShowSystem;

  end;

implementation

Uses Math, mFX.Consts, mFX.ErrorCodes, mFX.SystemTable;

Const
  TabWidth = 8;
  
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXCustomExtract.Create(AOwner: TComponent);
begin
  inherited;
  fTerminator:=';';
  fMetaData:=TStringList.Create;
end;
{______________________________________________________________________________}
destructor TFXCustomExtract.Destroy;
begin
  FMetaData.Free;
  inherited;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXCustomExtract.GetLongDatabaseInfo(Const Value : Byte):FXLong;
var local_buffer: array[0.._FXLocalBufferLength_] of Byte;
    DatabaseInfoCommand: Byte;
    cl:TFXCustomLibrary;
    len:FXLong;
begin
  Result:=0;
  Self.CheckDatabase;
  if Self.Database.Connected Then Begin
    cl:=Self.Database.ClientLibrary;
    DatabaseInfoCommand := Value;
    cl.Check_database_info(Self,@Self.Database.Handle, 1, @DatabaseInfoCommand,SizeOf(local_buffer),@local_buffer[0]);
    len:=mFX.Header.vax_integer(@local_buffer,1,2);
    Result:=mFX.Header.vax_integer(@local_buffer,3,len);
  end else
    FXRaiseClientError(fxceDatabaseClosed);
end;
{______________________________________________________________________________}
function TFXCustomExtract.CreateSQL: TFXCustomSQL;
begin
  Self.CheckDatabase;
  Self.CheckTransaction;
  Result := TFXCustomSQL.Create(Self.Database);
  Result.Database    := Self.Database;
  Result.Transaction := Self.Transaction;
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.LoadDBInfo;
Begin
  fODSMajorVersion:=GetLongDatabaseInfo(isc_info_ods_version      );
  fPageSize       :=GetLongDatabaseInfo(isc_info_page_size        );
  fDBSQLDialect   :=GetLongDatabaseInfo(isc_info_db_SQL_Dialect   );
  if Self.fODSMajorVersion < ODS_VERSION10 then
    FXRaiseClientErrorFmt(fxceUnknownException,['Extract Need ODS >= 10']);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXCustomExtract.AddBlankLine;
Begin
  fMetaData.Add('');
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.AddCmd(Value:String);
Var Values:TStrings;
    i,c:Integer;
Begin
  Values:=TStringList.Create;
  try Values.Text:=Value;
      While (Values.Count>0)and(Trim(Values[0])=EmptyStr) do
        Values.Delete(0);
      While (Values.Count>0)and(Trim(Values[Pred(Values.Count)])=EmptyStr) do
        Values.Delete(Pred(Values.Count));
      c:=Values.Count;
      if c>1 Then Begin
        for i:=0 to c-2 do
          fMetaData.Add(Values[i]);
        for i:=Pred(c) to Pred(c) do
          fMetaData.Add(Values[i]+' '+fTerminator)
      end else
      if c=1 Then
        fMetaData.Add(Values[0]+' '+fTerminator)
  finally
      Values.Free
  end
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.AddCmd(Values:TStrings);
Var i,c:Integer;
Begin
  While (Values.Count>0)and(Trim(Values[0])=EmptyStr) do
    Values.Delete(0);
  While (Values.Count>0)and(Trim(Values[Pred(Values.Count)])=EmptyStr) do
    Values.Delete(Pred(Values.Count));
  c:=Values.Count;
  if c>1 Then Begin
    for i:=0 to c-2 do
      fMetaData.Add(Values[i]);
    for i:=Pred(c) to Pred(c) do
      fMetaData.Add(Values[i]+' '+fTerminator)
  end else
  if c=1 Then
    fMetaData.Add(Values[0]+' '+fTerminator)
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.AddComment(Value:String);
Var Values:TStrings;
    i:Integer;
    s:String;
Begin
  Values:=TStringList.Create;
  try Values.Text:=Value;
      for i:=0 to Pred(Values.Count) do Begin
        s:=Trim(Values[i]);
        if s<>EmptyStr Then
          fMetaData.Add('/* '+Values[i]+' */') else
          fMetaData.Add('')
        end;
  finally
      Values.Free
  end
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.AddComment(Values:TStrings);
Var i:Integer;
    s:String;
begin
  for i:=0 to Pred(Values.Count) do Begin
    s:=Trim(Values[i]);
    if s<>EmptyStr Then
      fMetaData.Add('/* '+Values[i]+' */') else
      fMetaData.Add('')
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.SetTerminator(Const Value:Char);
Begin
  if (fTerminator<>Value) then Begin
    Self.AddCmd('SET TERM '+Value);
    fTerminator:=Value 
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.CheckSetDialect;
Begin
  AddBlankLine;
  Assert(Self.Database<>nil);
  AddCmd(Format('SET SQL DIALECT %d',[mFX_Dialect]));
  AddBlankLine;
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.AddCommitWorks;
Begin
  AddBlankLine;
  AddCmd('COMMIT');
  AddBlankLine;
end;
{______________________________________________________________________________}
function TFXCustomExtract.QuoteString(Value: String): String;
Begin
  Result:=AnsiQuotedStr(Value,'''');
end;
{______________________________________________________________________________}
function TFXCustomExtract.QuoteIdentifier(Value: String): String;
Begin
  Result:=Database.QuoteIdentifier(Value)
end;
{______________________________________________________________________________}
function TFXCustomExtract.IsUserDomain(Const Value:String):Boolean;
Var Prefix:String;
Begin
  Result:=False;
  Prefix:=Copy(Value, 1, 4);
  if Prefix<>'RDB$' Then
    exit;
  Assert(Length(Value)>4);
  Result:=(Value[5] in ['0'..'9'])
end;
{______________________________________________________________________________}
function TFXCustomExtract.FieldType2SQLType(Const Value:Integer;Out SQLType:FXShort):Boolean;
Var t:FXShort;
Begin
  Result:=False;
  //Look through types array
  SQLType:=blr_text;
  for t:=Low(Columntypes) to High(ColumnTypes) do begin
    if Value=ColumnTypes[t].SQLType then begin
      Result:=True;
      SQLType:=t;
      break
    end end;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXCustomExtract.ExtractDDL(Const Flag: Boolean; TableName: String):Boolean;
begin
  Result := true;
  fMetaData.Clear;

  Self.CheckDatabase;
  Self.CheckTransaction;

  SetTerminator(';');
  CheckSetDialect;
  AddBlankLine;

  Self.LoadDBInfo;
  Self.Start_TR;

  if TableName<>EmptyStr then begin
    Result := ExtractListTable(TableName, '', true);
    AddCommitWorks;
  end else Begin
    ListCreateDb;
    ListFilters;
    ListFunctions;
    ListDomains;
    ListAllTables(flag);
    ListIndex;
    ListForeign;
    ListGenerators;
    ListViews;
    ListCheck;
    ListException;
    ListProcs;
//    ListTriggers;
    ListGrants;
    AddCommitWorks;
    end
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXCustomExtract.GetFieldLength(sql: TFXCustomSQL): Integer;
Var f:TFXSQLVAR;
begin
  f:=sql.FieldByName('RDB$CHARACTER_LENGTH');
  if f.IsNull then 
    f:=sql.FieldByName('RDB$FIELD_LENGTH');
  Result := f.AsInteger;
end;
{______________________________________________________________________________}
function TFXCustomExtract.GetCharacterSets(Const CharSetId, Collation:FXShort;Const CollateOnly:Boolean):String;
var CharSetSQL:TFXCustomSQL;
    s,ss,c:String;
begin
  Result:=EmptyStr;
  Assert(Transaction<>nil);
  Assert(Transaction.InTransaction);

  CharSetSQL := CreateSQL;
  try if Collation <> 0 then begin
        With CharSetSQL do Begin
          SQL.Text:=_Collation_SQL_;
          Prepare;
          With Params do Begin
            ByName('Char_Set_Id').AsInteger := CharSetId;
            ByName('Collation').AsInteger := Collation;
            end;
          ExecQuery;
          if Not EOF then Begin
            //Is specified collation the default collation for character set?
            ss:=Trim(Fields[_Collation_SQL_DEFAULT_COLLATE_NAME].AsTrimString);
            s :=Trim(Fields[_Collation_SQL_COLLATION_NAME      ].AsTrimString);
            c :=Trim(Fields[_Collation_SQL_CHARACTER_SET_NAME  ].AsTrimString);
            if SameText(ss,s) then begin
              if not CollateOnly then
                Result := ' CHARACTER SET ' + s;
            end else
            if CollateOnly then
              Result := ' COLLATE ' + s
            else
              Result := ' CHARACTER SET ' +c+' COLLATE ' +s;
          end end;
      end else
      if CharSetId <> 0 then begin
        // Is specified the default ?
        With CharSetSQL do Begin
          SQL.Text:=_Non_Collation_SQL_;
          Prepare;
          With Params do Begin
            ByName('CharSetId').AsInt16 := CharSetId;
            end;
          ExecQuery;
          Assert(Not EOF);
          ss:=CharSet2Str(Self.DataBase.DefaultCharSet);
          s:=Trim(Fields[_Non_Collation_SQL_CHARACTER_SET_NAME].AsTrimString);
          if SameText(s,ss) then
            Result := ' CHARACTER SET ' +s
        end end;
  finally
      CharSetSQL.Free;
  end
end;
{______________________________________________________________________________}
function TFXCustomExtract.GetArrayField(Const FieldName: String): String;
var qryArray:TFXCustomSQL;
    f:TFXSQLVAR;
begin
  Result := '[';
  qryArray := CreateSQL;
  //Format is [lower:upper, lower:upper,..]
  try With qryArray do Begin
        SQL.Text:=_Array_SQL_;
        With Params do Begin
          ByName('FIELDNAME').AsTrimString:= FieldName;
          end;
        ExecQuery;
        Assert(Not Eof);
        while not Eof do begin
          f:=Fields[_Array_SQL_DIMENSION];
          if (f.AsInteger > 0) then
            Result := Result + ', ';
          Result := Result + Fields[_Array_SQL_LOWER_BOUND].AsTrimString + ':' +Fields[_Array_SQL_UPPER_BOUND].AsTrimString;
          Next;
          end;
        Close;
        end;
      Result := Result + '] ';
  finally
      qryArray.Free;
  end
end;
{______________________________________________________________________________}
function TFXCustomExtract.ProcessDefault:String;
Var f:TFXSQLVAR;
Begin
  f:=Self.Fields[_FIELD_DEF_DEFAULT_SOURCE_];
  if not f.IsNull then
    Result:=' ' + Trim(f.AsTrimString) else
    Result:=''
end;
{______________________________________________________________________________}
function TFXCustomExtract.ProcessCaracterSet(Const f:TFXSQLVAR):String;
Begin
  If (not f.IsNull)and(f.AsInteger<>0) then
    Result:= GetCharacterSets(f.AsInteger, 0, false) else
    Result:= ''
end;
{______________________________________________________________________________}
function TFXCustomExtract.ProcessCollation:String;
Var Collation:Integer;
    g,gg,f:TFXSQLVAR;
Begin

  f:=Self.Fields[_FIELD_DEF_CHARACTER_SET_ID_];
  Assert((not f.IsNull)and(f.AsInteger<>0));

  g:=Self.Fields[_FIELD_DEF_COLLATION_ID_];
  gg:=Self.Fields[_RELATION_FIELD_DEF_COLLATION_ID];

  { Override rdb$fields id with relation_fields if present }
  If (not gg.IsNull)and(gg.AsInteger<>0) then
    Collation:=gg.AsInteger
  else
  If (not g.IsNull)and(g.AsInteger<>0) then
    Collation:=g.AsInteger
  else
    Collation:=0;

  if (Collation <> 0) then
    Result:=GetCharacterSets(f.AsInteger, Collation, True) else
    Result:=''

end;
{______________________________________________________________________________}
function TFXCustomExtract.ProcessNullFlag:String;
Var FieldName, RelationName, FieldConstraint: String;
    qryConstraints :TFXCustomSQL;
Begin
  result:=EmptyStr;
  // The null flag is either 1 or null (for nullable) .
  // if there is a constraint name, print that too.
  // Domains cannot have named constraints.
  // The column name is in rdb$trigger_name in rdb$check_constraints.
  // We hope we get at most one row back.
  FieldName   :=Trim(Self.Fields[_RELATION_FIELD_DEF_FIELD_NAME].AsTrimString);
  RelationName:=Trim(Self.Fields[_RELATION_DEF_RELATION_NAME].AsTrimString);
  
  if Self.Fields[_RELATION_FIELD_DEF_NULL_FLAG].AsInteger = 1 then begin
    qryConstraints := CreateSQL;
    try With qryConstraints do Begin
          SQL.Text:=_Field_Constraints_;
          Prepare;
          With Params do Begin
            ByName('FIELDNAME'   ).AsTrimString := FieldName;
            ByName('RELATIONNAME').AsTrimString := RelationName;
            end;
          ExecQuery;
          while not Eof do begin
            FieldConstraint:=Trim(Fields[_Field_Constraints_CONSTRAINT_NAME].AsTrimString);
            if Pos('INTEG',FieldConstraint)<>1 then
              result := result + ' CONSTRAINT '+Self.QuoteIdentifier(FieldConstraint);
            Next;
            end;
          Close;
          end;
        result := result + ' NOT NULL';
    finally
        qryConstraints.Free
    end end
end;
{______________________________________________________________________________}
function TFXCustomExtract.ProcessColumn(Const ArrayColumn:Boolean=False):String;
Var FieldName, FieldSource, CaracterSet : String;
    SQLTypesFound,CheckCollate:Boolean;
    FieldType, FieldScale : Integer;
    SQLType, FieldSubType: FXShort;
    qryPrecision : TFXCustomSQL;
    f:TFXSQLVAR;
Begin
  CheckCollate:=False;

  FieldName   :=Trim(Self.Fields[_RELATION_FIELD_DEF_FIELD_NAME].AsTrimString);
  FieldSource :=Trim(Self.Fields[_FIELD_DEF_FIELD_NAME_].AsTrimString);
  FieldType   :=Self.Fields[_FIELD_DEF_FIELD_TYPE_    ].AsInteger;
  FieldScale  :=Self.Fields[_FIELD_DEF_FIELD_SCALE_   ].AsInteger;
  FieldSubType:=Self.Fields[_FIELD_DEF_FIELD_SUB_TYPE_].AsInteger;

  if not FieldType2SQLType(FieldType,SQLType) then
    Raise Exception.CreateFmt('Invalid FieldType:<%d> for Field:<%s> Source:<%s>',[FieldType,FieldName,FieldSource]);

  SQLTypesFound:=False;
  if Self.fODSMajorVersion >= ODS_VERSION10 then begin
    //Handle Integral subtypes NUMERIC and DECIMAL
    if FieldType in [blr_short, blr_long, blr_int64] then begin
      //FIELD_NAME>>FIELD_SOURCE
      qryPrecision := CreateSQL;
      try With qryPrecision do Begin
            SQL.Text:=_SELECT_FIELD_PRECISION_;
            Prepare;
            With Params do Begin
              Vars[0].AsTrimString:=FieldSource;
              end;
            ExecQuery;
            Assert(not qryPrecision.EOF);
            //We are ODS >= 10 and could be any Dialect
            f:=qryPrecision.Fields[_SELECT_FIELD_PRECISION_PRECISION_];
            if not f.IsNull then begin
              //We are Dialect >=3 since FIELD_PRECISION is non-NULL
              if (FieldSubType>0)and(FieldSubType<=MAX_INTSUBTYPES) then begin
                Result := Format('%s(%d, %d)',[IntegralSubtypes[FieldSubType],f.AsInteger,-FieldScale]);
              SQLTypesFound := TRUE;
            end end;
            Close;
            end;
      finally
          qryPrecision.Free
    end end end;

  if Not SQLTypesFound then Begin
    //Take a stab at numerics and decimals
    if (FieldType = blr_short) and (FieldScale < 0) then
      Result := Format('NUMERIC(4, %d)', [-FieldScale]) else
    if (FieldType = blr_long) and (FieldScale < 0) then
      Result := Format('NUMERIC(9, %d)', [-FieldScale]) else
    if (FieldType = blr_double) and (FieldScale < 0) then
      Result := Format('NUMERIC(15, %d)', [-FieldScale]) else
    if FieldType in [blr_text, blr_varying] then Begin
      Result := Format('%s(%d)', [ColumnTypes[SQLType].TypeName,GetFieldLength(Self)]);
      if not ArrayColumn then Begin
        CaracterSet:=ProcessCaracterSet(Self.FieldByName('RDB$CHARACTER_SET_ID'));
        if CaracterSet<>EmptyStr then Begin
          Result:=Result+CaracterSet;
          CheckCollate:=True
        end end
    end else
    if FieldType = blr_blob then begin
      Result := ColumnTypes[SQLType].TypeName + ' SUB_TYPE ';
      if (FieldSubType>0)and(FieldSubType<=MAXSUBTYPES) then
        Result := Result + SubTypes[FieldSubType] else
        Result := Result + IntToStr(FieldSubType);
      Result := Result + Format(' SEGMENT SIZE %d',[Self.Fields[_FIELD_DEF_SEGMENT_LENGTH_].AsInteger]);
      if not ArrayColumn then Begin
        CaracterSet:=ProcessCaracterSet(Self.FieldByName('RDB$CHARACTER_SET_ID'));
        if CaracterSet<>EmptyStr then Begin
          Result:=Result+CaracterSet;
          CheckCollate:=True
        end end
    end else Begin
      Result := ColumnTypes[SQLType].TypeName;
    end end;

  if not ArrayColumn then Begin
    Result:=Result +
      ProcessDefault +
      ProcessNullFlag;
    if CheckCollate then
      Result:=Result +
        ProcessCollation;
    end

end;
{______________________________________________________________________________}
function TFXCustomExtract.ProcessArrayColumn:String;
Var FieldSource,CaracterSet:String;
    CheckCollate:Boolean;
    FieldType:Integer;
Begin
  CheckCollate:=False;
  FieldSource :=Trim(Self.Fields[_FIELD_DEF_FIELD_NAME_].AsTrimString);
  FieldType   :=Self.Fields[_FIELD_DEF_FIELD_TYPE_    ].AsInteger;

  //1-Process as Standart Column
  Result:=ProcessColumn(True);

  //2-Catch arrays after printing the type  }
  Result:=Result+GetArrayField(FieldSource);

  //3-Caracter Set
  Case FieldType Of
    blr_text,blr_varying,blr_blob:Begin
    CaracterSet:=ProcessCaracterSet(Self.FieldByName('RDB$CHARACTER_SET_ID'));
    if CaracterSet<>EmptyStr then Begin
      Result:=Result+CaracterSet;
      CheckCollate:=True
    end end end;

  Result:=Result +
    ProcessDefault +
    ProcessNullFlag;

  if CheckCollate then
    Result:=Result +
      ProcessCollation;

end;
{______________________________________________________________________________}
function TFXCustomExtract.ProcessUserDomainColumn:String;
Var CheckCollate:Boolean;
    CaracterSet:String;
    FieldType:Integer;
Begin

  CheckCollate:=False;
  FieldType   :=Self.Fields[_FIELD_DEF_FIELD_TYPE_    ].AsInteger;
  Result:=Self.QuoteIdentifier(Trim(Self.Fields[_FIELD_DEF_FIELD_NAME_].AsTrimString));

  //3-Caracter Set
  Case FieldType of
    blr_text,blr_varying,blr_blob:Begin
      CaracterSet:=ProcessCaracterSet(Self.FieldByName('RDB$CHARACTER_SET_ID'));
      if CaracterSet<>EmptyStr then Begin
        Result:=Result+CaracterSet;
        CheckCollate:=True
    end end end;

  Result:=Result +
    ProcessDefault +
    ProcessNullFlag;

  if CheckCollate then
    Result:=Result +
      ProcessCollation;
    
end;
{______________________________________________________________________________}
function TFXCustomExtract.ProcessComputedColumn:String;
Var CheckCollate:Boolean;
    f:TFXSQLVAR;
    s:String;
Begin
  CheckCollate:=False;
  Result:='COMPUTED BY ';
  f:=Self.Fields[_FIELD_DEF_COMPUTED_SOURCE_];
  if not f.IsNull then Begin
    s:=Trim(f.AsTrimString);
    Assert(Length(s)>=1);
    if (s[1]<>'(') then
      s:= '/* ' + s + ' */';
    Result := Result + s
    end;

  if CheckCollate then
    Result:=Result +
      ProcessCollation;
end;
{______________________________________________________________________________}
function TFXCustomExtract.GetIndexSegments(IndexName: String): String;
var qryColNames:TFXCustomSQL;
    f:String;
begin
  Result := '';
  qryColNames := CreateSQL;
  try With qryColNames do Begin
        SQL.Text:=_SELECT_Index_Def_;
        Prepare;
        With Params do Begin
          ByName('IndexName').AsTrimString := IndexName;
          end;
        ExecQuery;
        while not Eof do begin
          //Place a comma and a blank between each segment column name
          f:=Trim(Fields[_SELECT_Index_Def_FIELD_NAME].AsTrimString);
          Result := Result + Self.QuoteIdentifier(f);
          Next;
          if not Eof then
            Result := Result + ', ';
          end;
        Close;
        end;
  finally
      qryColNames.Free;
  end;
end;
{______________________________________________________________________________}
function TFXCustomExtract.ProcessStoredProcParams(Const qryHeader:TFXCustomSQL):String;
var ParamName,ProcName,ParamDef,CaracterSet:String;
    ParamType,FieldType,FieldScale:Integer;
    t, SQLType, FieldSubType: FXShort;
    qryPrecision:TFXCustomSQL;
    SQLTypesFound:Boolean;
    f:TFXSQLVAR;
begin

  ProcName    :=Trim(qryHeader.Fields[_SELECT_Procedure_Param_Proc     ].AsTrimString);
  ParamName   :=Trim(qryHeader.Fields[_SELECT_Procedure_Param_Param    ].AsTrimString);
  ParamType   :=qryHeader.Fields[_SELECT_Procedure_Param_Param_Type    ].AsInteger;
  FieldType   :=qryHeader.Fields[_SELECT_Procedure_Param_FIELD_TYPE    ].AsInteger;
  FieldScale  :=qryHeader.Fields[_SELECT_Procedure_Param_FIELD_SCALE   ].AsInteger;
  FieldSubType:=qryHeader.Fields[_SELECT_Procedure_Param_FIELD_SUB_TYPE].AsInteger;

  //Look through types array
  SQLType:=blr_text;
  SQLTypesFound:=False;
  for t:=Low(Columntypes) to High(ColumnTypes) do begin
    if FieldType=ColumnTypes[t].SQLType then begin
      SQLTypesFound:=True;
      SQLType:=t;
      break
    end end;
  if not SQLTypesFound then
    Raise Exception.CreateFmt('Invalid FieldType:<%d> for Params:<%s>',[FieldType,ParamName]);

  SQLTypesFound:=False;
  if Self.fODSMajorVersion >= ODS_VERSION10 then begin
    //Handle Integral subtypes NUMERIC and DECIMAL
    if FieldType in [blr_short, blr_long, blr_int64] then begin
      //FIELD_NAME>>FIELD_SOURCE
      qryPrecision := CreateSQL;
      try With qryPrecision do Begin
            SQL.Text:=_SELECT_Procedure_Params_Precision;
            Prepare;
            With Params do Begin
              ByName('procname').AsTrimString := ProcName;
              ByName('PARAM'   ).AsTrimString := ParamName;
              ByName('Input'   ).AsInteger    := ParamType;
              end;
            ExecQuery;
            Assert(not qryPrecision.EOF);
            //We are ODS >= 10 and could be any Dialect
            f:=qryPrecision.Fields[0];
            if not f.IsNull then begin
              //We are Dialect >=3 since FIELD_PRECISION is non-NULL
              if (FieldSubType>0)and(FieldSubType<=MAX_INTSUBTYPES) then begin
                ParamDef := Format('%s(%d, %d)',[IntegralSubtypes[FieldSubType],f.AsInteger,-FieldScale]);
              SQLTypesFound := TRUE;
            end end;
            Close;
            end;
      finally
          qryPrecision.Free
    end end end;

  if Not SQLTypesFound then Begin
    //Take a stab at numerics and decimals
    if (FieldType = blr_short) and (FieldScale < 0) then
      ParamDef:= Format('NUMERIC(4, %d)', [-FieldScale]) else
    if (FieldType = blr_long) and (FieldScale < 0) then
      ParamDef:= Format('NUMERIC(9, %d)', [-FieldScale]) else
    if (FieldType = blr_double) and (FieldScale < 0) then
      ParamDef:= Format('NUMERIC(15, %d)', [-FieldScale]) else
    if FieldType in [blr_text, blr_varying] then Begin
      ParamDef:= Format('%s(%d)', [ColumnTypes[SQLType].TypeName,GetFieldLength(qryHeader)]);
      CaracterSet:=ProcessCaracterSet(qryHeader.FieldByName('RDB$CHARACTER_SET_ID'));
      if CaracterSet<>EmptyStr then
        ParamDef:=ParamDef+CaracterSet;
    end else
    if FieldType=blr_blob then Begin
      ParamDef := ColumnTypes[SQLType].TypeName + ' SUB_TYPE ';
      if (FieldSubType>0)and(FieldSubType<=MAXSUBTYPES) then
        ParamDef := ParamDef + SubTypes[FieldSubType] else
        ParamDef := ParamDef + IntToStr(FieldSubType);
      ParamDef := ParamDef + Format(' SEGMENT SIZE %d',[qryHeader.Fields[_SELECT_Procedure_Param_SEGMENT_LENGTH].AsInteger]);
      CaracterSet:=ProcessCaracterSet(qryHeader.FieldByName('RDB$CHARACTER_SET_ID'));
      if CaracterSet<>EmptyStr then
        ParamDef:=ParamDef+CaracterSet;
    end else Begin
      ParamDef:= ColumnTypes[SQLType].TypeName;
    end end;

  Result := Format('  %-50.50s %s', [Self.QuoteIdentifier(ParamName),ParamDef]);

{
    if qryHeader.FieldByName('RDB$FIELD_TYPE').AsInteger = blr_blob then
    begin
      subtype := qryHeader.FieldByName('RDB$FIELD_SUB_TYPE').AsShort;
      Result := Result + ' SUB_TYPE ';
      if (subtype > 0) and (subtype <= MAXSUBTYPES) then
        Result := Result + SubTypes[subtype]
      else
        Result := Result + IntToStr(subtype);
      Result := Result + Format(' SEGMENT SIZE %d',
          [qryHeader.FieldByName('RDB$SEGMENT_LENGTH').AsInteger]);
    end;
    }
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.GetProcedureArgs(Proc: String);
var qryHeader:TFXCustomSQL;
    FirstTime:Boolean;
    Line:String;
begin
  FirstTime := true;
  qryHeader := CreateSQL;
  try
      qryHeader.SQL.Text := _SELECT_Procedure_Params_;
      With qryHeader.Params do Begin
        ByName('procname').AsTrimString := Proc;
        ByName('Input'   ).AsInteger := 0;
        end;
      qryHeader.ExecQuery;
      while not qryHeader.Eof do begin
        if FirstTime then begin
          FirstTime := false;
          FMetaData.Add('(');
          end;
        Line := ProcessStoredProcParams(qryHeader);
        qryHeader.Next;
        if not qryHeader.Eof then
          Line := Line + ',';
        FMetaData.Add(Line);
        end;
      //If there was at least one param, close parens
      if not FirstTime then begin
        FMetaData.Add( ')');
        FirstTime := true;
        end;
      qryHeader.Close;

      qryHeader.Params.ByName('Input').AsInteger := 1;
      qryHeader.ExecQuery;
      while not qryHeader.Eof do begin
        if FirstTime then begin
          FirstTime := false;
          FMetaData.Add('RETURNS' +CRLF+ '(');
          end;
        Line := ProcessStoredProcParams(qryHeader);
        qryHeader.Next;
        if not qryHeader.Eof then
          Line := Line + ',';
        FMetaData.Add(Line);
        end;
      //If there was at least one param, close parens
      if not FirstTime then begin
        FMetaData.Add( ')');
       end;
      FMetaData.Add('AS'); 
  finally
    qryHeader.Free;
  end;
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.ProcessUDFParams(Const FctName:String);
Var //First, DidCharset, PrecisionKnown:Boolean;
    //aFieldType,FirstArg:Integer;
    qryFuncArgs:TFXCustomSQL;
    //FctParam,CaracterSet:String;
    //aSQLType:Short;
Begin
  qryFuncArgs := CreateSQL;
  try
     {
      FirstArg := 0;
      With qryFuncArgs do Begin
        SQL.Text:=_SELECT_UDFS_ARGUMENTS_;
        Params[0].AsTrimString :=FctName;
        ExecQuery;
        while not Eof do begin
          //Find parameter type
          aFieldType:=qryFuncArgs.FieldByName('RDB$FIELD_TYPE').AsInteger;
          if Not FieldType2SQLType(aFieldType,aSQLType) Then
            Raise Exception.CreateFmt('Invalid FieldType:<%d> for UDF Param:<%s>',[aFieldType,FctParam]);
          Case aFieldType of
            blr_text, blr_varying, blr_cstring:begin
              CaracterSet:='';//ProcessCaracterSet(qryHeader.FieldByName('RDB$CHARACTER_SET_ID'));
              DidCharset := false;
              qryCharSets.Params.ByName('CHARACTER_SET_ID').AsInteger :=
                 qryFuncArgs.FieldByName('RDB$CHARACTER_SET_ID').AsInteger;
              qryCharSets.ExecQuery;
              while not qryCharSets.Eof do begin
              DidCharset := true;
              TypeBuffer := Format('%s(%d) CHARACTER SET %s',
                [ColumnTypes[i].TypeName,
                 qryFuncArgs.FieldByName('RDB$FIELD_LENGTH').AsInteger div
                 Math.Max(1,qryCharSets.FieldByName('RDB$BYTES_PER_CHARACTER').AsInteger),
                 qryCharSets.FieldByName('RDB$CHARACTER_SET_NAME').AsTrimString]); 
              qryCharSets.Next;
            end;
            qryCharSets.Close;
            if not DidCharset then
              TypeBuffer := Format('%s(%d)', [ColumnTypes[i].TypeName,   
                qryFuncArgs.FieldByName('RDB$FIELD_LENGTH').AsInteger]);  
          end //end_if
          else
          begin
            PrecisionKnown := false;
            if (fODSMajorVersion >= ODS_VERSION10) and
                (FieldType in [blr_short, blr_long, blr_int64]) then
            begin
              qryFuncPos.Params.ByName('RDB$FUNCTION_NAME').AsTrimString :=   
                qryFuncArgs.FieldByName('RDB$FUNCTION_NAME').AsTrimString;
              qryFuncPos.Params.ByName('RDB$ARGUMENT_POSITION').AsInteger :=
                qryFuncArgs.FieldByName('RDB$ARGUMENT_POSITION').AsInteger;   

              qryFuncPos.ExecQuery;
              while not qryFuncPos.Eof do
              begin
                // We are ODS >= 10 and could be any Dialect
                if not qryFuncPos.FieldByName('RDB$FIELD_PRECISION').IsNull then
                begin
                  // We are Dialect >=3 since FIELD_PRECISION is non-NULL
                  if (qryFuncPos.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger > 0) and
                      (qryFuncPos.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger <= MAX_INTSUBTYPES) then
                  begin
                    TypeBuffer := Format('%s(%d, %d)',
                      [IntegralSubtypes[qryFuncPos.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger],
                       qryFuncPos.FieldByName('RDB$FIELD_PRECISION').AsInteger,
                       -qryFuncPos.FieldByName('RDB$FIELD_SCALE').AsInteger] );
                    PrecisionKnown := true;
                  end; //end_if
                end; // if field_precision is not null
                qryFuncPos.Next;
              end;
              qryFuncPos.Close;
            end; //  if major_ods >= ods_version10 &&
            if not PrecisionKnown then
            begin
              // Take a stab at numerics and decimals
              if (FieldType = blr_short) and
                  (qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger < 0) then
                TypeBuffer := Format('NUMERIC(4, %d)',
                  [-qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger])
              else
                if (FieldType = blr_long) and
                    (qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger < 0) then
                  TypeBuffer := Format('NUMERIC(9, %d)',
                    [-qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger])
                else
                  if (FieldType = blr_double) and
                      (qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger < 0) then
                    TypeBuffer := Format('NUMERIC(15, %d)',
                        [-qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger])
                  else
                    TypeBuffer := ColumnTypes[i].TypeName;
            end; // if  not PrecisionKnown
          end; // if FCHAR or VARCHAR or CSTRING ... else

          if qryFunctions.FieldByName('RDB$RETURN_ARGUMENT').AsInteger =
                 qryFuncArgs.FieldByName('RDB$ARGUMENT_POSITION').AsInteger then
          begin
            ReturnBuffer := 'RETURNS ' + TypeBuffer;
            if qryFuncArgs.FieldByName('RDB$MECHANISM').AsInteger = 0 then
              ReturnBuffer := ReturnBuffer + ' BY VALUE ';
            if qryFuncArgs.FieldByName('RDB$MECHANISM').AsInteger < 0 then
              ReturnBuffer := ReturnBuffer + ' FREE_IT';
          end
          else
          begin
            // First arg needs no comma
            if FirstArg then
            begin
              Line := Line + TypeBuffer;
              FirstArg := false;
            end
            else
              Line := Line + ', ' + TypeBuffer;
          end; //end_else
          qryFuncArgs.Next;
        end;
        qryFuncArgs.Close;
      }
  finally
    qryFuncArgs.Free;
  end;
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.ProcessUDFResults(Const FctName:String);
Begin
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXCustomExtract.ListCreateDb(Const NewDataBase:String = '');
var SHADOW_NUMBER, FileFlags, FileLength, FileSequence, FileStart:Integer;
    NoDb, First, FirstFile, HasWal, SetUsed:Boolean;
    Buffer:TStringList;
    s,FileName:String;
begin
  NoDb := FALSE;
  HasWal := FALSE;
  SetUsed := FALSE;

  Buffer:=TStringList.Create;
  try
      if NewDataBase = EmptyStr Then 
        Buffer.Add('CREATE DATABASE '+Self.QuoteIdentifier(Database.DatabaseName)) else
        Buffer.Add('CREATE DATABASE '+Self.QuoteIdentifier(NewDataBase));
      Buffer.Add(StringOfChar(' ',TabWidth)+'PAGE_SIZE '+IntToStr(fPageSize));

      Self.SQL.Text:=_SELECT_DATABASE_CHARACTER_SET;
      Self.ExecQuery;
      If Not Self.Eof Then Begin
        s:=Trim(Self.Fields[_SELECT_DATABASE_CHARACTER_SET_NAME].AsTrimString);
        Buffer.Add(StringOfChar(' ',TabWidth)+'DEFAULT CHARACTER SET '+s);
        end;
      Self.Close;
      Self.AddCmd(Buffer);

      //List secondary files and shadows as alter db and create shadow in comment
      Self.SQL.Text := _SELECT_DATABASE_Files;
      Self.ExecQuery;
      First:=True;
      Buffer.Clear;
      while not Self.Eof do begin
        if First then begin
          Self.AddComment('Add secondary files in comments');
          First := false;
          end;
        FileFlags    :=Self.Fields[_SELECT_DATABASE_Files_FLags        ].AsInteger;
        FileLength   :=Self.Fields[_SELECT_DATABASE_Files_Length       ].AsInteger;
        FileSequence :=Self.Fields[_SELECT_DATABASE_Files_Sequence     ].AsInteger;
        FileStart    :=Self.Fields[_SELECT_DATABASE_Files_Start        ].AsInteger;
        FileName     :=Trim(Self.Fields[_SELECT_DATABASE_Files_Name    ].AsString);
        SHADOW_NUMBER:=Self.Fields[_SELECT_DATABASE_Files_SHADOW_NUMBER].AsInteger;

        if FileFlags = 0 then Begin
          if Buffer.Count>0 then Begin
            Self.AddCmd(Buffer);
            Buffer.Clear;
            end;
          Buffer.Add('ALTER DATABASE ADD FILE '+Self.QuoteString(FileName));
          if FileStart <> 0 then
            Buffer.Add(StringOfChar(' ',TabWidth)+'STARTING '+IntToStr(FileStart));
          if FileLength <> 0 then
            Buffer.Add(StringOfChar(' ',TabWidth)+'LENGTH   '+IntToStr(FileLength));
          Self.AddCmd(Buffer);
          Buffer.Clear;
          end;

        if (FileFlags and FILE_cache) <> 0 then Begin
          if Buffer.Count>0 then Begin
            Self.AddCmd(Buffer);
            Buffer.Clear;
            end;
          Buffer.Add('ALTER DATABASE ADD CACHE '+Self.QuoteString(FileName)+' LENGTH '+IntToStr(FileLength));
          Self.AddCmd(Buffer);
          Buffer.Clear;
          end;

        if (FileFlags and FILE_shadow) <> 0 then Begin
          if (FileSequence <> 0) Then Begin
            Buffer.Add(StringOfChar(' ',TabWidth)+'FILE   '+Self.QuoteString(FileName));
            if FileLength <> 0 then
              Buffer.Add(StringOfChar(' ',TabWidth)+'LENGTH   '+IntToStr(FileLength));
            if FileStart <> 0 then
              Buffer.Add(StringOfChar(' ',TabWidth)+'STARTING '+IntToStr(FileStart));
          end else Begin
            if Buffer.Count>0 then Begin
              Self.AddCmd(Buffer);
              Buffer.Clear;
              end;
            Buffer.Add('CREATE SHADOW '+IntToStr(SHADOW_NUMBER)+' '+Self.QuoteString(FileName));
            if (FileFlags and FILE_inactive) <> 0 then
              Buffer.Add(StringOfChar(' ',TabWidth)+'INACTIVE');
            if (FileFlags and FILE_manual) <> 0 then
              Buffer.Add(StringOfChar(' ',TabWidth)+'MANUAL') else
              Buffer.Add(StringOfChar(' ',TabWidth)+'AUTO');
            if (FileFlags and FILE_conditional) <> 0 then
              Buffer.Add(StringOfChar(' ',TabWidth)+'CONDITIONAL');
            if FileLength <> 0 then
              Buffer.Add(StringOfChar(' ',TabWidth)+'LENGTH   '+IntToStr(FileLength));
            if FileStart <> 0 then
              Buffer.Add(StringOfChar(' ',TabWidth)+'STARTING '+IntToStr(FileStart));
          end end;
        Self.Next;
        end;
      Self.Close;
      if Buffer.Count>0 then Begin
        Self.AddCmd(Buffer);
        Buffer.Clear;
        end;

      Self.SQL.Text := _SELECT_DATABASE_Logs_Files;
      Self.ExecQuery;
      First:=True;
      while not Self.Eof do begin
        HasWal := true;
        if First then Begin
          Buffer.Add('ALTER DATABASE ADD LOGFILE');
          First:=False;
          end;
        FileFlags    :=Self.Fields[_SELECT_DATABASE_Logs_Files_FLags    ].AsInteger;
        FileLength   :=Self.Fields[_SELECT_DATABASE_Logs_Files_LENGTH   ].AsInteger;
        FileName     :=Trim(Self.Fields[_SELECT_DATABASE_Logs_Files_Name].AsString);
        if (FileFlags and LOG_default) = 0 then begin
          if (FileFlags and LOG_overflow) <> 0 then Begin
            Buffer.Add(StringOfChar(' ',TabWidth)+'OVERFLOW '+Self.QuoteString(FileName));
          end else
          if (FileFlags and LOG_serial) <> 0 then Begin
            Buffer.Add(StringOfChar(' ',TabWidth)+'BASE_NAME '+Self.QuoteString(FileName));
          end else begin
            Buffer.Add(StringOfChar(' ',TabWidth)+Self.QuoteString(FileName));
          end end;
        if FileLength <> 0 then
          Buffer.Add(StringOfChar(' ',TabWidth)+'SIZE   '+IntToStr(FileLength));
        Self.AddCmd(Buffer);
        Buffer.Clear;
        Self.Next;
        end;
      Self.Close;

      if HasWal then begin
        Self.AddCmd(PrintSet(SetUsed));
        Self.AddCmd(Format('NUM_LOG_BUFFERS = %d',[GetLongDatabaseInfo(isc_info_num_wal_buffers)]));
        Self.AddCmd(PrintSet(SetUsed));
        Self.AddCmd(Format('LOG_BUFFER_SIZE = %d',[GetLongDatabaseInfo(isc_info_wal_buffer_size)]));
        Self.AddCmd(PrintSet(SetUsed));
        Self.AddCmd(Format('GROUP_COMMIT_WAIT_TIME = %d',[GetLongDatabaseInfo(isc_info_wal_grpc_wait_usecs)]));
        Self.AddCmd(PrintSet(SetUsed));
        Self.AddCmd(Format('CHECK_POINT_LENGTH = %d',[GetLongDatabaseInfo(isc_info_wal_ckpt_length)]));
        end;

  finally
      Buffer.Free
  end
end;
{______________________________________________________________________________}
// Result are in form :
//  DECLARE FILTER <fname> INPUT_TYPE <blob_sub_type> OUTPUT_TYPE <blob_subtype>
//      ENTRY_POINT <string> MODULE_NAME <string>
procedure TFXCustomExtract.ListFilters(FilterName:String = '');
const
  FiltersSQL =
    'SELECT * FROM RDB$FILTERS ' +
    'ORDER BY RDB$FUNCTION_NAME';
  FilterNameSQL =
    'SELECT * FROM RDB$FILTERS ' +
    'WHERE RDB$FUNCTION_NAME = :FunctionName ' +
    'ORDER BY RDB$FUNCTION_NAME';
var
  First:Boolean;
  qryFilters:TFXCustomSQL;
begin
  First := true;
  qryFilters := CreateSQL;
  try
    if FilterName = '' then   
      qryFilters.SQL.Text := FiltersSQL
    else
    begin
      qryFilters.SQL.Text := FilterNameSQL;
      qryFilters.Params.ByName('FunctionName').AsTrimString := FilterName;  
    end;
    qryFilters.ExecQuery;
    while not qryFilters.Eof do
    begin
      if First then
      begin
        FMetaData.Add('');
        FMetaData.Add('/*  BLOB Filter declarations */');
        FMetaData.Add('');    
        First := false;
      end; //end_if

      FMetaData.Add(Format('DECLARE FILTER %s INPUT_TYPE %d OUTPUT_TYPE %d', 
        [qryFilters.FieldByName('RDB$FUNCTION_NAME').AsTrimString,     
         qryFilters.FieldByName('RDB$INPUT_SUB_TYPE').AsInteger,       
         qryFilters.FieldByName('RDB$OUTPUT_SUB_TYPE').AsInteger]));     
      FMetaData.Add(Format('%sENTRY_POINT ''%s'' MODULE_NAME ''%s''%s%',   
        [TAB, qryFilters.FieldByName('RDB$ENTRYPOINT').AsTrimString,      
         qryFilters.FieldByName('RDB$MODULE_NAME').AsTrimString, fTerminator])); 
      FMetaData.Add('');                 

      qryFilters.Next;
    end;

  finally
    qryFilters.Free;
  end;
end;
{______________________________________________________________________________}
function TFXCustomExtract.ExtractListTable(Const RelationName, NewName: String;Const DomainFlag: Boolean):Boolean;
var NewRelationName,Column,FieldName,FieldSource,s:String;
    IndexName,ConstraintType,ConstraintName:String;
    HasConstraint:Boolean;
    f:TFXSQLVAR;
begin
  Start_TR;
  Result:=False;
  Assert(not Self.Open);
  Assert(RelationName=Trim(RelationName));

  // If a new_name is passed, substitute it for relation_name
  //       relation_name -- Name of table to investigate
  //       new_name -- Name of a new name to clone the table
  //       domain_flag -- extract needed domains before the table


  if DomainFlag then
    ListDomains(RelationName);

  //Main loop on field Def
  Self.SQL.Text:=_SELECT_RELATION_FIELDS_DEF_;
  Self.Params[0].AsTrimString := RelationName;
  Self.ExecQuery;

  if Self.Eof then
    Raise Exception.CreateFmt('Invalid RelationName <%s>',[RelationName]);

  if NewName=EmptyStr then
    NewRelationName:=RelationName else
    NewRelationName:=NewName;

  s:=Trim(Self.Fields[_RELATION_DEF_OWNER_NAME].AsTrimString);
  if (s<>Emptystr) then
    AddComment(Format('Table: %s, Owner: %s',[NewRelationName,s]));

  FMetaData.Add('CREATE TABLE '+Self.QuoteIdentifier(NewRelationName));
  s:=Trim(Self.Fields[_RELATION_DEF_EXTERNAL_FILE].AsTrimString);
  if (s<>Emptystr) then
    FMetaData.Add(Format('EXTERNAL FILE %s ',[QuotedStr(s)]));
  FMetaData.Add('(');

  while not Self.Eof do begin
    FieldName   :=Trim(Self.Fields[_RELATION_FIELD_DEF_FIELD_NAME].AsTrimString);
    FieldSource :=Trim(Self.Fields[_FIELD_DEF_FIELD_NAME_].AsTrimString);
    Column := Format('%s%-35.35s ',[StringOfChar(' ',TabWidth),Self.QuoteIdentifier(FieldName)]);
    f:=Self.Fields[_FIELD_DEF_computed_blr_];
    if not f.IsNull then
      Column:=Column + ProcessComputedColumn
    else begin
      f:=Self.Fields[_FIELD_DEF_SYSTEM_FLAG_];
      if (not IsUserDomain(FieldSource))and(f.AsInteger <> 1) then
        Column:=Column + ProcessUserDomainColumn
      else Begin
        f:=Self.Fields[_FIELD_DEF_DIMENSIONS_];
        if not f.IsNull then
          Column:=Column + ProcessArrayColumn else
          Column:=Column + ProcessColumn;
      end end;
    Self.Next;
    if not Self.Eof then
      Column := Column + ',';
    FMetaData.Add(Column);
    end;
  Self.Close;

  //Do primary and unique keys only. references come later
  Self.SQL.Text:=_SELECT_RELATION_Indexes;
  Self.Params[0].AsTrimString := RelationName;
  Self.ExecQuery;
  HasConstraint:=False;
  while not Self.Eof do begin
    if not HasConstraint then Begin
      //Add , to the Last Field Column
      Column:=FMetaData.Strings[Pred(FMetaData.Count)]+ ',';
      FMetaData.Strings[Pred(FMetaData.Count)]:=Column;
      HasConstraint:=True;
      end;
    IndexName     :=Trim(Self.Fields[_SELECT_RELATION_Idx_INDEX_NAME     ].AsTrimString);
    ConstraintType:=Trim(Self.Fields[_SELECT_RELATION_Idx_CONSTRAINT_TYPE].AsTrimString);
    ConstraintName:=Trim(Self.Fields[_SELECT_RELATION_Idx_CONSTRAINT_NAME].AsTrimString);
    if Pos('PRIMARY', ConstraintType) = 1 then
      Column := Format('PRIMARY KEY (%s)',[GetIndexSegments(IndexName)])
    else
    if Pos('UNIQUE', ConstraintType ) = 1 then
      Column := Format('UNIQUE (%s)',[GetIndexSegments(IndexName)])
    else
      Column := EmptyStr;
    Self.Next;
    If Column <>EmptyStr then Begin
      //If the name of the constraint is not INTEG..., print it
      if Pos('INTEG',ConstraintName) <> 1 then
        Column := StringOfChar(' ',TabWidth)+'CONSTRAINT '+Self.QuoteIdentifier(ConstraintName)+' '+Column;
      if not Self.Eof then
        Column := Column + ',';
      FMetaData.Add(Column);
    end end;
  Self.Close;
  AddCmd(')')

end;
{______________________________________________________________________________}
procedure TFXCustomExtract.ExtractListView(Const ViewName,NewName:String);
var NewRelationName,FieldName,ViewSource,ColList,s:String;
begin
  Start_TR;
  Assert(not Self.Open);
  Assert(ViewName=Trim(ViewName));

  // Use a SQL query to get the info and print it.
  //    Note: This should also contain check option }


  Self.SQL.Text:=_Select_Views_;
  Self.Params[0].AsTrimString := ViewName;
  Self.ExecQuery;

  if Self.Eof then
    Raise Exception.CreateFmt('Invalid RelationName <%s>',[ViewName]);

  if NewName=EmptyStr then
    NewRelationName:=ViewName else
    NewRelationName:=NewName;

  s:=Trim(Self.Fields[_Select_Views_OWNER_NAME].AsTrimString);
  if (s<>Emptystr) then
    AddComment(Format('View: %s, Owner: %s',[NewRelationName,s]));

  FMetaData.Add('CREATE VIEW '+Self.QuoteIdentifier(NewRelationName));

  while not Self.Eof do begin
    FieldName:=Trim(Self.Fields[_Select_Views_FIELD_NAME].AsTrimString);
    ColList:=ColList+Self.QuoteIdentifier(FieldName);
    Next;
    if not Self.Eof then
      ColList := ColList + ', ';
    end;
  FMetaData.Add(ColList + ') AS');
  ViewSource:=Trim(Self.Fields[_Select_Views_VIEW_SOURCE].AsTrimString);
  Self.Close;
  AddCmd(ViewSource)
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.ListRoles(ObjectName: String);
var PrevOwner, RoleName, OwnerName:String;
    qryRoles:TFXCustomSQL;
begin
  if fODSMajorVersion >= ODS_VERSION9 Then Begin
    qryRoles := CreateSQL;
    try PrevOwner := '';
        FMetaData.Add('');
        FMetaData.Add('/* Grant Roles for this database */');
        FMetaData.Add('');
        With qryRoles do Begin
          SQL.Text := _SELECT_Roles_;
          if ObjectName<>EmptyStr then Begin
            SQL.Add('WHERE (RDB$ROLE_NAME = :ROLE_NAME)');
            SQL.Add('ORDER BY 1');
            Prepare;
            Params[0].AsTrimString:=ObjectName;
          end else
            SQL.Add('ORDER BY 1');
          ExecQuery;
          while not Eof do begin
            RoleName := Self.QuoteIdentifier(Trim(Fields[_SELECT_Roles_ROLE_NAME].AsTrimString));
            OwnerName:= Trim(Fields[_SELECT_Roles_OWNER_NAME].AsString);
            if not SameText(PrevOwner,OwnerName) then begin
              FMetaData.Add('');
              FMetaData.Add(Format('/* Role: %s, Owner: %s */', [RoleName, OwnerName]));
              FMetaData.Add('');
              PrevOwner := OwnerName
              end;
            Self.AddCmd('CREATE ROLE ' + RoleName);
            Next
            end;
          Close
          end
    finally
        qryRoles.Free
    end end
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.ListGrants;
var RelationName,ProcedureName:String;
    qry:TFXCustomSQL;
begin
  Self.ListRoles;

  // This version of cursor gets only sql tables identified by security class
  // and misses views, getting only null view_source

  Self.AddBlankLine;
  Self.AddComment('Grant permissions for this database');
  Self.AddBlankLine;

  qry:=CreateSQL;
  try With qry do Begin
        SQL.Text := SELECT_RELATIONS_SECURITY_CLASS_;
        ExecQuery;
        while not Eof do begin
          RelationName := Trim(Fields[SELECT_RELATIONS_SECURITY_CLASS_RELATION_NAME].AsTrimString);
          ShowGrants(RelationName);
          Next;
          end;
        Close;
        end;
  finally
      qry.Free
  end;

  ShowGrantRoles;

  qry:=CreateSQL;
  try With qry do Begin
        SQL.Text := _QRY_Procs_;
        ExecQuery;
        while not Eof do begin
          ProcedureName:=Trim(Fields[_QRY_Proc_NAME].AsTrimString);
          ShowGrants(ProcedureName);
          Next;
          end;
        Close;
        end;
  finally
      qry.Free
  end;

end;
{______________________________________________________________________________}
procedure TFXCustomExtract.ListProcs(Const ProcedureName:String;Const AlterOnly:Boolean);
var qryProcedures:TFXCustomSQL;
    HeaderNeeded:Boolean;
    ProcName:String;
    SList:TStrings;
    SavTerm:Char;
    f:TFXSQLVAR;
begin

  //Since procedures may reference each other,
  // we will create all dummy procedures of the correct name,
  // then alter these to their correct form.
  // Add the parameter names when these procedures are created.

  SavTerm:=fTerminator;
  HeaderNeeded:= true;
  qryProcedures := CreateSQL;
  SList := TStringList.Create;
  try
      // First the dummy procedures create the procedures with their parameters
      if not AlterOnly then begin
        if ProcedureName=EmptyStr then Begin
          qryProcedures.SQL.Text := _QRY_Procs_
        end else begin
          qryProcedures.SQL.Text := _QRY_Proc_;
          qryProcedures.Params[0].AsTrimString := ProcedureName;
          end;
        qryProcedures.ExecQuery;
        while not qryProcedures.Eof do begin
          if HeaderNeeded then begin
            Self.AddCmd('COMMIT WORK');
            if fTerminator<>'^' then Begin
             Self.AddCmd('SET TERM ^');
             fTerminator:='^';
             end;
            Self.AddComment('Stored procedures');
            HeaderNeeded := false;
            end;
          ProcName := Trim(qryProcedures.Fields[_QRY_Proc_NAME].AsTrimString);
          FMetaData.Add(Format('CREATE PROCEDURE %s', [Self.QuoteIdentifier(ProcName)]));
          GetProcedureArgs(ProcName);
          Self.AddCmd('BEGIN'+CRLF+'  EXIT;'+CRLF+'END');
          qryProcedures.Next;
          end;
        qryProcedures.Close;
        end;

      // Then Alter procedure(s)
      if ProcedureName=EmptyStr then Begin
        qryProcedures.SQL.Text := _SELECT_Procedures_Source_
      end else begin
        qryProcedures.SQL.Text := _SELECT_Procedure_Source_;
        qryProcedures.Params[0].AsTrimString := ProcedureName;
        end;
      qryProcedures.ExecQuery;
      while not qryProcedures.Eof do begin
        SList.Clear;
        ProcName := Trim(qryProcedures.Fields[_QRY_Proc_NAME].AsTrimString);
        FMetaData.Add(Format('ALTER PROCEDURE %s', [Self.QuoteIdentifier(ProcName)]));
        GetProcedureArgs(ProcName);
        f:=qryProcedures.Fields[_SELECT_Procedures_Source];
        if not f.IsNull then begin
          SList.Text := f.AsTrimString;
          while (Slist.Count>0)and(Trim(SList[0])=EmptyStr) do
            SList.Delete(0);
          while (Slist.Count>0)and(Trim(SList[Pred(Slist.Count)])=EmptyStr) do
            SList.Delete(Pred(Slist.Count));
        end else Begin
          SList.Text := 'BEGIN'+CRLF+'  EXIT;'+CRLF+'END';
          end;
        Self.AddCmd(SList);
        qryProcedures.Next;
        end;
      qryProcedures.Close;

      if not HeaderNeeded then begin
        if SavTerm<>fTerminator Then
          Self.AddCmd('SET TERM '+SavTerm);
        Self.AddCmd('COMMIT WORK');
        end;
        
  finally
      qryProcedures.Free;
      SList.Free;
  end;
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.ListAllTables(Const flag: Boolean);
var qryTables:TFXCustomSQL;
    f:TFXSQLVAR;
begin

  //Parameters:  flag -- 0, get all tables }
  qryTables := CreateSQL;
   try
     qryTables.SQL.Text := _SELECT_TABLES_;
     qryTables.ExecQuery;
     while not qryTables.Eof do begin
       f:=qryTables.Fields[_SELECT_TABLES_FLAGS];
       if (not Flag)and(f.AsInteger<>1) then Begin
         qryTables.Next;
         continue;
         end;
       {
       f:=qryTables.Fields[_SELECT_TABLES_SECURITY_CLASS];
       if (not flag)and(Pos('SQL$',f.AsTrimString)=1) then Begin
         qryTables.Next;
         Continue;
         end;
       }
       f:=qryTables.Fields[_SELECT_TABLES_RELATION_NAME];
       ExtractListTable(Trim(f.AsTrimString),'', false);
       qryTables.Next
       end;
     qryTables.Close
  finally
     qryTables.Free
  end
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.ListTriggers(ObjectName:String; ExtractType:TExtractType);
var TriggerName, RelationName, InActive: String;
    HeaderNeeded,CommentOut:Boolean;
    qryTriggers:TFXCustomSQL;
    SList:TStrings;
    SavTerm:Char;
    f:TFXSQLVAR;
begin
  HeaderNeeded:=true;
  SavTerm:=fTerminator;
  SList:=TStringList.Create;
  qryTriggers:=CreateSQL;
  try
      if ObjectName = EmptyStr then
        Build_Select_Triggers(qryTriggers,EmptyStr  ,EmptyStr  ,False,False) else
      if ExtractType = etTable then
        Build_Select_Triggers(qryTriggers,EmptyStr  ,ObjectName,False,False) else
        Build_Select_Triggers(qryTriggers,ObjectName,EmptyStr  ,False,False);
      qryTriggers.ExecQuery;
      while not qryTriggers.Eof do begin
        SList.Clear;
        if HeaderNeeded then begin
          if fTerminator<>'^' Then
            Self.AddCmd('SET TERM '+'^');
          Self.AddBlankLine;
          Self.AddComment('Triggers only will work for SQL triggers');
          Self.AddBlankLine;
          HeaderNeeded := false;
          end;
        TriggerName  := Trim(qryTriggers.Fields[_SELECT_TRIGGERS_Name    ].AsTrimString);
        RelationName := Trim(qryTriggers.Fields[_SELECT_TRIGGERS_Relation].AsTrimString);
        f:=qryTriggers.Fields[_SELECT_TRIGGERS_INACTIVE];
        if (f.IsNull)or(f.AsInteger = 1)then
          InActive := 'INACTIVE' else
          InActive := 'ACTIVE';   
        f:=qryTriggers.Fields[_SELECT_TRIGGERS_FLAGS];
        CommentOut:=(f.AsInteger <> 1);
        if CommentOut then
          FMetaData.Add('/* ');
        SList.Add(Format('CREATE TRIGGER %s FOR %s '+CRLF+' %s %s POSITION %d',[
           Self.QuoteIdentifier(TriggerName),Self.QuoteIdentifier(RelationName),
           InActive,
           TriggerTypes[qryTriggers.Fields[_SELECT_TRIGGERS_TYPE].AsInteger],
           qryTriggers.Fields[_SELECT_TRIGGERS_SEQUENCE].AsInteger
           ]));
        f:=qryTriggers.Fields[Succ(_SELECT_TRIGGERS_LAST)];
        if not f.IsNull then
          SList.Add(f.AsTrimString) else
          SList.Add(' Dummy Trigger !!!');
        Self.AddCmd(SList);
        if CommentOut then
          FMetaData.Add('*/');
        qryTriggers.Next;
        end;
      qryTriggers.Close;

      if not HeaderNeeded then begin
        if SavTerm<>fTerminator Then
          Self.AddCmd('SET TERM '+SavTerm);
        Self.AddCmd('COMMIT WORK');
        end;

  finally
    qryTriggers.Free;
    SList.Free;
  end;
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.ListCheck(ObjectName:String; ExtractType:TExtractType);
const
  // Query gets the check clauses for triggers stored for check constraints
  CheckSQL =
    'SELECT * FROM RDB$TRIGGERS TRG JOIN RDB$CHECK_CONSTRAINTS CHK ON ' +   
    '  TRG.RDB$TRIGGER_NAME = CHK.RDB$TRIGGER_NAME ' +   
    'WHERE ' +   
    '  TRG.RDB$TRIGGER_TYPE = 1 AND ' +   
    '  EXISTS (SELECT RDB$CONSTRAINT_NAME FROM RDB$RELATION_CONSTRAINTS RELC WHERE ' +    
    '    CHK.RDB$CONSTRAINT_NAME = RELC.RDB$CONSTRAINT_NAME) ' +    
    'ORDER BY CHK.RDB$CONSTRAINT_NAME';  

  CheckNameSQL =
    'SELECT * FROM RDB$TRIGGERS TRG JOIN RDB$CHECK_CONSTRAINTS CHK ON ' +  
    '  TRG.RDB$TRIGGER_NAME = CHK.RDB$TRIGGER_NAME ' +
    'WHERE ' +      
    '  TRG.RDB$RELATION_NAME = :TableName AND ' +   
    '  TRG.RDB$TRIGGER_TYPE = 1 AND ' +    
    '  EXISTS (SELECT RDB$CONSTRAINT_NAME FROM RDB$RELATION_CONSTRAINTS RELC WHERE ' +  
    '    CHK.RDB$CONSTRAINT_NAME = RELC.RDB$CONSTRAINT_NAME) ' +  
    'ORDER BY CHK.RDB$CONSTRAINT_NAME';

  CheckByNameSQL =
    'SELECT * FROM RDB$TRIGGERS TRG JOIN RDB$CHECK_CONSTRAINTS CHK ON ' +  
    '  TRG.RDB$TRIGGER_NAME = CHK.RDB$TRIGGER_NAME ' +   
    'WHERE ' +     
    '  TRG.RDB$TRIGGER_NAME = :TriggerName AND ' +   
    '  TRG.RDB$TRIGGER_TYPE = 1 AND ' +   
    '  EXISTS (SELECT RDB$CONSTRAINT_NAME FROM RDB$RELATION_CONSTRAINTS RELC WHERE ' +  
    '    CHK.RDB$CONSTRAINT_NAME = RELC.RDB$CONSTRAINT_NAME) ' +    
    'ORDER BY CHK.RDB$CONSTRAINT_NAME';    

var
  qryChecks:TFXCustomSQL;
  SList:TStrings;
  RelationName:String;
begin
  qryChecks := CreateSQL;
  SList := TStringList.Create;
  try
    if ObjectName = '' then     
      qryChecks.SQL.Text := CheckSQL
    else
      if ExtractType = etTable then
      begin
        qryChecks.SQL.Text := CheckNameSQL;
        qryChecks.Params.ByName('TableName').AsTrimString := ObjectName;   
      end
      else
      begin
        qryChecks.SQL.Text := CheckByNameSQL;
        qryChecks.Params.ByName('TriggerName').AsTrimString := ObjectName; 
      end;
    qryChecks.ExecQuery;
    while not qryChecks.Eof do
    begin
      SList.Clear;
      RelationName := qryChecks.FieldByName('RDB$RELATION_NAME').AsTrimString;  
      SList.Add(Format('ALTER TABLE %s ADD',   
		    [Database.QuoteIdentifier(RelationName)]));
      if Pos('INTEG', qryChecks.FieldByName('RDB$CONSTRAINT_NAME').AsTrimString) <> 1 then    
        SList.Add(Format('%sCONSTRAINT %s ', [TAB,        
          Database.QuoteIdentifier(qryChecks.FieldByName('RDB$CONSTRAINT_NAME').AsTrimString)]));

      if not qryChecks.FieldByName('RDB$TRIGGER_SOURCE').IsNull then
        SList.Text := SList.Text + qryChecks.FieldByName('RDB$TRIGGER_SOURCE').AsTrimString;

      SList.Strings[SList.Count - 1] := SList.Strings[SList.Count - 1] + fTerminator +CRLF;
      FMetaData.AddStrings(SList);
      qryChecks.Next;
    end;
  finally
    qryChecks.Free;
    SList.Free;
  end;
end;

{	             ListDomainTable
  Functional description
  	List domains as identified by fields with any constraints on them
  	for the named table

  	Parameters:  table_name == only extract domains for this table }

procedure TFXCustomExtract.ListDomains(Const ObjectName: String; ExtractType:TExtractType);
const
  DomainSQL =
    'SELECT distinct fld.* FROM RDB$FIELDS FLD JOIN RDB$RELATION_FIELDS RFR ON ' +  
    '  RFR.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME ' +   
    'WHERE RFR.RDB$RELATION_NAME = :TABLE_NAME ' +     
    'ORDER BY FLD.RDB$FIELD_NAME';

  DomainByNameSQL =
    'SELECT * FROM RDB$FIELDS FLD ' +   
    'WHERE FLD.RDB$FIELD_NAME = :DomainName ' +   
    'ORDER BY FLD.RDB$FIELD_NAME';  

  AllDomainSQL =
    'select * from RDB$FIELDS ' +   
    'where RDB$SYSTEM_FLAG <> 1 ' +   
    'order BY RDB$FIELD_NAME';    

var
  First:Boolean;
  qryDomains:TFXCustomSQL;
  FieldName, Line:String;

  function FormatDomainStr:String;
  var
    i, SubType:Integer;
    PrecisionKnown:Boolean;
  begin
    Result := '';  
    for i := Low(ColumnTypes) to High(ColumnTypes) do
      if qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger = ColumnTypes[i].SQLType then    
      begin
        PrecisionKnown := FALSE;
        if fODSMajorVersion >= ODS_VERSION10 then
        begin
          if qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger in [blr_short, blr_long, blr_int64] then  
          begin
            { We are ODS >= 10 and could be any Dialect }
            if (fDBSQLDialect >= 3) and
               (not qryDomains.FieldByName('RDB$FIELD_PRECISION').IsNull) and  
               (qryDomains.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger > 0) and  
               (qryDomains.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger <= MAX_INTSUBTYPES) then  
            begin
              Result := Result + Format('%s(%d, %d)', [  
                IntegralSubtypes [qryDomains.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger],  
                qryDomains.FieldByName('RDB$FIELD_PRECISION').AsInteger, 
                -1 * qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger]);
              PrecisionKnown := true;
            end;
          end;
        end;
        if PrecisionKnown = false then
        begin
          { Take a stab at numerics and decimals }
          if (qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger = blr_short) and  
              (qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger < 0) then 
            Result := Result + Format('NUMERIC(4, %d)',     
              [-qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger] )  
          else
            if (qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger = blr_long) and 
                (qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger < 0) then  
              Result := Result + Format('NUMERIC(9, %d)',                   
                [-qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger] )    
            else
              if (qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger = blr_double) and    
                  (qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger  < 0) then  
                Result := Result + Format('NUMERIC(15, %d)',
                  [-qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger] ) 
              else
                Result := Result + ColumnTypes[i].TypeName;
        end;
        break;
      end;

    if qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger = blr_blob then 
    begin
      subtype := qryDomains.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;  
      Result := Result + ' SUB_TYPE ';   
      if (subtype > 0) and (subtype <= MAXSUBTYPES) then
        Result := Result + SubTypes[subtype]
      else
        Result := Result + Format('%d', [subtype]);  
      Result := Result + Format(' SEGMENT SIZE %d', [qryDomains.FieldByName('RDB$SEGMENT_LENGTH').AsInteger]);  
    end //end_if
    else
      if qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger in [blr_text, blr_varying] then   
        Result := Result + Format('(%d)', [GetFieldLength(qryDomains)]);

    { since the character set is part of the field type, display that
     information now. }
    if not qryDomains.FieldByName('RDB$CHARACTER_SET_ID').IsNull then  
      Result := Result + GetCharacterSets(qryDomains.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,
         0, FALSE);
    if not qryDomains.FieldByName('RDB$DIMENSIONS').IsNull then 
      Result := Result + GetArrayField(FieldName);

    if not qryDomains.FieldByName('RDB$DEFAULT_SOURCE').IsNull then   
      Result := Result + Format('%s%s %s', [CRLF, TAB,           
         qryDomains.FieldByName('RDB$DEFAULT_SOURCE').AsTrimString]);  

    if not qryDomains.FieldByName('RDB$VALIDATION_SOURCE').IsNull then   
      if Pos('CHECK', AnsiUpperCase(qryDomains.FieldByName('RDB$VALIDATION_SOURCE').AsTrimString)) = 1 then 
        Result := Result + Format('%s%s %s', [CRLF, TAB,   
           qryDomains.FieldByName('RDB$VALIDATION_SOURCE').AsTrimString])  
      else
        Result := Result + Format('%s%s /* %s */', [CRLF, TAB,   
           qryDomains.FieldByName('RDB$VALIDATION_SOURCE').AsTrimString]);

    if qryDomains.FieldByName('RDB$NULL_FLAG').AsInteger = 1 then   
      Result := Result + ' NOT NULL';       

    { Show the collation order if one has been specified.  If the collation
       order is the default for the character set being used, then no collation
       order will be shown ( because it isn't needed ).

       If the collation id is 0, then the default for the character set is
       being used so there is no need to retrieve the collation information.}

    if (not qryDomains.FieldByName('RDB$COLLATION_ID').IsNull) and  
       (qryDomains.FieldByName('RDB$COLLATION_ID').AsInteger <> 0) then   
      Result := Result + GetCharacterSets(qryDomains.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,  
        qryDomains.FieldByName('RDB$COLLATION_ID').AsInteger, true);  
  end;

begin
  First := true;
  qryDomains := CreateSQL;
  try
    if ObjectName <> '' then    
    begin
      if ExtractType = etTable then
      begin
        qryDomains.SQL.Text := DomainSQL;
        qryDomains.Params.ByName('table_name').AsTrimString := ObjectName;  
      end
      else
      begin
        qryDomains.SQL.Text := DomainByNameSQL;
        qryDomains.Params.ByName('DomainName').AsTrimString := ObjectName;  
      end;
    end
    else
      qryDomains.SQL.Text := AllDomainSQL;

    qryDomains.ExecQuery;
    while not qryDomains.Eof do
    begin
      FieldName := qryDomains.FieldByName('RDB$FIELD_NAME').AsTrimString; 
      { Skip over artifical domains }
      if (Pos('RDB$',FieldName) = 1) and   
         (FieldName[5] in ['0'..'9']) and  
         (qryDomains.FieldByName('RDB$SYSTEM_FLAG').AsInteger <> 1) then  
      begin
        qryDomains.Next;
        continue;
      end;

      if First then
      begin
        FMetaData.Add('/* Domain definitions */');  
        First := false;
      end;

      Line := Format('CREATE DOMAIN %s AS ', [Database.QuoteIdentifier(FieldName)]);  
      Line := Line + FormatDomainStr + fTerminator;
      FMetaData.Add(Line);
      qryDomains.Next;
    end;
  finally
    qryDomains.Free;
  end;
end;

{          ListException
 Functional description
   List all exceptions defined in the database

   Parameters:  none }

procedure TFXCustomExtract.ListException(ExceptionName:String = '');
const
  ExceptionSQL =
    'select * from RDB$EXCEPTIONS ' +  
    'ORDER BY RDB$EXCEPTION_NAME';   

  ExceptionNameSQL =
    'select * from RDB$EXCEPTIONS ' +
    'WHERE RDB$EXCEPTION_NAME = :ExceptionName ' + 
    'ORDER BY RDB$EXCEPTION_NAME'; 

var
  First:Boolean;
  qryException:TFXCustomSQL;
begin
  First := true;
  qryException := CreateSQL;
  try
    if ExceptionName = '' then
      qryException.SQL.Text := ExceptionSQL
    else
    begin
      qryException.SQL.Text := ExceptionNameSQL;
      qryException.Params.ByName('ExceptionName').AsTrimString := ExceptionName; 
    end;

    qryException.ExecQuery;
    while not qryException.Eof do
    begin
      if First then
      begin
        FMetaData.Add('');     
        FMetaData.Add('/*  Exceptions */');    
        FMetaData.Add('');    
        First := false;
      end; //end_if
      
      FMetaData.Add(Format('CREATE EXCEPTION %s %s%s',  
        [Database.QuoteIdentifier(qryException.FieldByName('RDB$EXCEPTION_NAME').AsTrimString), 
        QuotedStr(qryException.FieldByName('RDB$MESSAGE').AsTrimString), fTerminator]));
      qryException.Next;
    end;
  finally
    qryException.Free;
  end;
end;

{            ListForeign
  Functional description
   List all foreign key constraints and alter the tables }

procedure TFXCustomExtract.ListForeign(ObjectName:String; ExtractType:TExtractType);
const
  { Static queries for obtaining foreign constraints, where RELC1 is the
    foreign key constraints, RELC2 is the primary key lookup and REFC
    is the join table }
  ForeignSQL =
    'SELECT REFC.RDB$UPDATE_RULE REFC_UPDATE_RULE, REFC.RDB$DELETE_RULE REFC_DELETE_RULE, ' +   
    '  RELC1.RDB$RELATION_NAME RELC1_RELATION_NAME, RELC2.RDB$RELATION_NAME RELC2_RELATION_NAME, ' + 
    '  RELC1.RDB$INDEX_NAME RELC1_INDEX_NAME, RELC1.RDB$CONSTRAINT_NAME RELC1_CONSTRAINT_NAME, ' +  
    '  RELC2.RDB$INDEX_NAME RELC2_INDEX_NAME ' +
    'FROM RDB$REF_CONSTRAINTS REFC, RDB$RELATION_CONSTRAINTS RELC1, ' +
    '     RDB$RELATION_CONSTRAINTS RELC2 ' + 
    'WHERE ' +  
    '  RELC1.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' AND ' +  
    '  REFC.RDB$CONST_NAME_UQ = RELC2.RDB$CONSTRAINT_NAME AND ' +
    '  REFC.RDB$CONSTRAINT_NAME = RELC1.RDB$CONSTRAINT_NAME AND ' +  
    '  (RELC2.RDB$CONSTRAINT_TYPE = ''UNIQUE'' OR ' +  
    '    RELC2.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'') ' +   
    'ORDER BY RELC1.RDB$RELATION_NAME, RELC1.RDB$CONSTRAINT_NAME';   

  ForeignNameSQL =
    'SELECT REFC.RDB$UPDATE_RULE REFC_UPDATE_RULE, REFC.RDB$DELETE_RULE REFC_DELETE_RULE, ' +  
    '  RELC1.RDB$RELATION_NAME RELC1_RELATION_NAME, RELC2.RDB$RELATION_NAME RELC2_RELATION_NAME, ' +   
    '  RELC1.RDB$INDEX_NAME RELC1_INDEX_NAME, RELC1.RDB$CONSTRAINT_NAME RELC1_CONSTRAINT_NAME, ' +  
    '  RELC2.RDB$INDEX_NAME RELC2_INDEX_NAME ' + 
    'FROM RDB$REF_CONSTRAINTS REFC, RDB$RELATION_CONSTRAINTS RELC1, ' +  
    '     RDB$RELATION_CONSTRAINTS RELC2 ' +
    'WHERE ' +  
    '  RELC1.RDB$RELATION_NAME = :TableName AND ' + 
    '  RELC1.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' AND ' +   
    '  REFC.RDB$CONST_NAME_UQ = RELC2.RDB$CONSTRAINT_NAME AND ' +
    '  REFC.RDB$CONSTRAINT_NAME = RELC1.RDB$CONSTRAINT_NAME AND ' +
    '  (RELC2.RDB$CONSTRAINT_TYPE = ''UNIQUE'' OR ' +  
    '    RELC2.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'') ' +   
    'ORDER BY RELC1.RDB$RELATION_NAME, RELC1.RDB$CONSTRAINT_NAME';

  ForeignByNameSQL =
    'SELECT REFC.RDB$UPDATE_RULE REFC_UPDATE_RULE, REFC.RDB$DELETE_RULE REFC_DELETE_RULE, ' +   
    '  RELC1.RDB$RELATION_NAME RELC1_RELATION_NAME, RELC2.RDB$RELATION_NAME RELC2_RELATION_NAME, ' +  
    '  RELC1.RDB$INDEX_NAME RELC1_INDEX_NAME, RELC1.RDB$CONSTRAINT_NAME RELC1_CONSTRAINT_NAME, ' +   
    '  RELC2.RDB$INDEX_NAME RELC2_INDEX_NAME ' +   
    'FROM RDB$REF_CONSTRAINTS REFC, RDB$RELATION_CONSTRAINTS RELC1, ' +  
    '     RDB$RELATION_CONSTRAINTS RELC2 ' +  
    'WHERE ' +   
    '  RELC1.RDB$CONSTRAINT_NAME = :ConstraintName AND ' +   
    '  RELC1.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' AND ' +   
    '  REFC.RDB$CONST_NAME_UQ = RELC2.RDB$CONSTRAINT_NAME AND ' + 
    '  REFC.RDB$CONSTRAINT_NAME = RELC1.RDB$CONSTRAINT_NAME AND ' +   
    '  (RELC2.RDB$CONSTRAINT_TYPE = ''UNIQUE'' OR ' +
    '    RELC2.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'') ' +   
    'ORDER BY RELC1.RDB$RELATION_NAME, RELC1.RDB$CONSTRAINT_NAME';  

var
  qryForeign:TFXCustomSQL;
  Line:String;

begin
  qryForeign := CreateSQL;
  try
    if ObjectName = '' then   
      qryForeign.SQL.Text := ForeignSQL
    else
    begin
      if ExtractType = etTable then
      begin
        qryForeign.SQL.Text := ForeignNameSQL;
        qryForeign.Params.ByName('TableName').AsTrimString := ObjectName;   
      end
      else
      begin
        qryForeign.SQL.Text := ForeignByNameSQL;
        qryForeign.Params.ByName('ConstraintName').AsTrimString := ObjectName;  
      end;
    end;
    qryForeign.ExecQuery;
    while not qryForeign.Eof do
    begin
      Line := Format('ALTER TABLE %s ADD ', [Database.QuoteIdentifier(
        qryForeign.FieldByName('RELC1_RELATION_NAME').AsTrimString)]);  

      { If the name of the constraint is not INTEG..., print it.
         INTEG... are internally generated names. }
      if (not qryForeign.FieldByName('RELC1_CONSTRAINT_NAME').IsNull) and   
         ( Pos('INTEG', qryForeign.FieldByName('RELC1_CONSTRAINT_NAME').AsTrimString) <> 1) then   
        Line := Line + Format('CONSTRAINT %s ', [Database.QuoteIdentifier(
          Trim(qryForeign.FieldByName('RELC1_CONSTRAINT_NAME').AsTrimString))]);  

      Line := Line + Format('FOREIGN KEY (%s) REFERENCES %s ', [    
        GetIndexSegments(qryForeign.FieldByName('RELC1_INDEX_NAME').AsTrimString),   
        Trim(qryForeign.FieldByName('RELC2_RELATION_NAME').AsTrimString)]);  

      Line := Line + Format('(%s)',   
        [GetIndexSegments(qryForeign.FieldByName('RELC2_INDEX_NAME').AsTrimString)]);

      { Add the referential actions, if any }
      if (not qryForeign.FieldByName('REFC_UPDATE_RULE').IsNull) and  
         (Trim(qryForeign.FieldByName('REFC_UPDATE_RULE').AsTrimString) <> 'RESTRICT') then   
        Line := Line + Format(' ON UPDATE %s',   
           [Trim(qryForeign.FieldByName('REFC_UPDATE_RULE').AsTrimString)]);  

      if (not qryForeign.FieldByName('REFC_DELETE_RULE').IsNull) and    
         (Trim(qryForeign.FieldByName('REFC_DELETE_RULE').AsTrimString) <> 'RESTRICT') then  
        Line := Line + Format(' ON DELETE %s',    
           [Trim(qryForeign.FieldByName('REFC_DELETE_RULE').AsTrimString)]);  

      Line := Line + fTerminator;
      FMetaData.Add(Line);
      qryForeign.Next;
    end;
  finally
    qryForeign.Free;
  end;
end;
{______________________________________________________________________________}
procedure TFXCustomExtract.ListFunctions(Const FunctionName:String = '');
var ReturnBuffer, TypeBuffer, Line, FctName, EntryPoint, ModuleName:String;
    qryFunctions, qryCharSets, qryFuncPos:TFXCustomSQL;
    First, FirstArg, DidCharset, PrecisionKnown:Boolean;
    i, FieldType:Integer;
begin
  First := true;
  qryFunctions:= nil;
  qryFuncPos  := nil;
  qryCharSets := nil;
  try
      //qryFuncArgs := CreateSQL;
      //qryFuncArgs.SQL.Text := FunctionArgsSQL;

      qryFuncPos  := CreateSQL;
      qryFuncPos.SQL.Text := FuncArgsPosSQL;

      qryCharSets := CreateSQL;
      qryCharSets.SQL.Text := CharSetSQL;

      qryFunctions:= CreateSQL;
      Build_Select_UDFs(qryFunctions,FunctionName,False);
      qryFunctions.ExecQuery;
      while not qryFunctions.Eof do begin
        if First then begin
          Self.AddBlankLine;
          Self.AddComment('External Function declarations');
          Self.AddBlankLine;
          First := false;
          end;
        //Start new function declaration
        FctName   :=Trim(qryFunctions.Fields[_SELECT_UDFS_FUNCTION_NAME].AsTrimString);
        EntryPoint:=Trim(qryFunctions.Fields[_SELECT_UDFS_ENTRYPOINT   ].AsTrimString);
        ModuleName:=Trim(qryFunctions.Fields[_SELECT_UDFS_MODULE_NAME  ].AsTrimString);
        {
        Line:='DECLARE EXTERNAL FUNCTION '+FctName+CRLF+
              ProcessUDFParams(FctName)+CRLF+
              ProcessUDFResults(FctName)+CRLF+
              'ENTRY_POINT '''+EntryPoint+''' MODULE_NAME '''+ModuleName+''';
        }
        Self.AddCmd(Line);
        qryFunctions.Next;
        end;
  finally
      qryFunctions.Free;
      //qryFuncArgs.Free;
      qryCharSets.Free;
      qryFuncPos.Free;
  end;
end;

{  ListGenerators
 Functional description
   Re create all non-system generators }

procedure TFXCustomExtract.ListGenerators(GeneratorName:String = '');   
const
  GeneratorSQL =
    'SELECT RDB$GENERATOR_NAME ' +    
    'FROM RDB$GENERATORS ' +     
    'WHERE ' +                 
    '  (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG <> 1) ' +    
    'ORDER BY RDB$GENERATOR_NAME';   

  GeneratorNameSQL =
    'SELECT RDB$GENERATOR_NAME ' +   
    'FROM RDB$GENERATORS ' +     
    'WHERE RDB$GENERATOR_NAME = :GeneratorName AND ' +    
    '  (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG <> 1) ' +   
    'ORDER BY RDB$GENERATOR_NAME';  

var
  qryGenerator:TFXCustomSQL;
  GenName:String;
begin
  qryGenerator := CreateSQL;
  try
    if GeneratorName = '' then     
      qryGenerator.SQL.Text := GeneratorSQL
    else
    begin
      qryGenerator.SQL.Text := GeneratorNameSQL;
      qryGenerator.Params.ByName('GeneratorName').AsTrimString := GeneratorName;  
    end;
    qryGenerator.ExecQuery;
    FMetaData.Add('');  
    while not qryGenerator.Eof do
    begin
      GenName := qryGenerator.FieldByName('RDB$GENERATOR_NAME').AsTrimString; 
      if ((Pos('RDB$',GenName) = 1) and    
         (GenName[5] in ['0'..'9'])) or    
         ((Pos('SQL$',GenName) = 1) and    
         (GenName[5] in ['0'..'9'])) then  
      begin
        qryGenerator.Next;
        continue;
      end;
      FMetaData.Add(Format('CREATE GENERATOR %s%s',   
        [Database.QuoteIdentifier(GenName),
         fTerminator]));
      qryGenerator.Next;
    end;
  finally
    qryGenerator.Free;
  end;
end;

{       ListIndex
 Functional description
   Define all non-constraint indices
   Use a static SQL query to get the info and print it.

   Uses get_index_segment to provide a key list for each index }

procedure TFXCustomExtract.ListIndex(ObjectName:String; ExtractType:TExtractType);
const
  IndexSQL =
    'SELECT IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME, IDX.RDB$UNIQUE_FLAG, ' +  
    '       IDX.RDB$INDEX_TYPE ' +     
    'FROM RDB$INDICES IDX JOIN RDB$RELATIONS RELC ON ' +   
    '  IDX.RDB$RELATION_NAME = RELC.RDB$RELATION_NAME ' +  
    'WHERE (RELC.RDB$SYSTEM_FLAG <> 1 OR RELC.RDB$SYSTEM_FLAG IS NULL) AND ' + 
    '  NOT EXISTS (SELECT * FROM RDB$RELATION_CONSTRAINTS RC ' +   
    '    WHERE RC.RDB$INDEX_NAME = IDX.RDB$INDEX_NAME) ' +  
    'ORDER BY IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME';

  IndexNameSQL =
    'SELECT IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME, IDX.RDB$UNIQUE_FLAG, ' + 
    '       IDX.RDB$INDEX_TYPE ' +   
    'FROM RDB$INDICES IDX JOIN RDB$RELATIONS RELC ON ' +  
    '  IDX.RDB$RELATION_NAME = RELC.RDB$RELATION_NAME ' +   
    'WHERE (RELC.RDB$SYSTEM_FLAG <> 1 OR RELC.RDB$SYSTEM_FLAG IS NULL) AND ' +  
    '  RELC.RDB$RELATION_NAME = :RelationName AND ' +  
    '  NOT EXISTS (SELECT * FROM RDB$RELATION_CONSTRAINTS RC ' +   
    '    WHERE RC.RDB$INDEX_NAME = IDX.RDB$INDEX_NAME) ' + 
    'ORDER BY IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME'; 

  IndexByNameSQL =
    'SELECT IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME, IDX.RDB$UNIQUE_FLAG, ' +  
    '       IDX.RDB$INDEX_TYPE ' +  
    'FROM RDB$INDICES IDX JOIN RDB$RELATIONS RELC ON ' +   
    '  IDX.RDB$RELATION_NAME = RELC.RDB$RELATION_NAME ' +   
    'WHERE (RELC.RDB$SYSTEM_FLAG <> 1 OR RELC.RDB$SYSTEM_FLAG IS NULL) AND ' +   
    '  IDX.RDB$INDEX_NAME = :IndexName AND ' +   
    '  NOT EXISTS (SELECT * FROM RDB$RELATION_CONSTRAINTS RC ' +
    '    WHERE RC.RDB$INDEX_NAME = IDX.RDB$INDEX_NAME) ' +  
    'ORDER BY IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME';  

var
  qryIndex:TFXCustomSQL;
  First:Boolean;
  Unique, IdxType, Line:String;
begin
  First := true;
  qryIndex := CreateSQL;
  try
    if ObjectName = '' then
      qryIndex.SQL.Text := IndexSQL
    else
    begin
      if ExtractType = etTable then
      begin
        qryIndex.SQL.Text := IndexNameSQL;
        qryIndex.Params.ByName('RelationName').AsTrimString := ObjectName;
      end
      else
      begin
        qryIndex.SQL.Text := IndexByNameSQL;
        qryIndex.Params.ByName('IndexName').AsTrimString := ObjectName;  
      end;
    end;
    qryIndex.ExecQuery;
    while not qryIndex.Eof do
    begin
      if First then
      begin
        if ObjectName = '' then     
          FMetaData.Add(CRLF + '/*  Index definitions for all user tables */' + CRLF)   
        else
          FMetaData.Add(CRLF + '/*  Index definitions for ' + ObjectName + ' */' + CRLF);  
        First := false;
      end; //end_if

      if qryIndex.FieldByName('RDB$UNIQUE_FLAG').AsInteger = 1 then  
        Unique := ' UNIQUE'  
      else
        Unique := '';    

      if qryIndex.FieldByName('RDB$INDEX_TYPE').AsInteger = 1 then   
        IdxType := ' DESCENDING'   
      else
        IdxType := '';   

      Line := Format('CREATE%s%s INDEX %s ON %s(', [Unique, IdxType, 
        Database.QuoteIdentifier(
            qryIndex.FieldByName('RDB$INDEX_NAME').AsTrimString),   
        Database.QuoteIdentifier(
            qryIndex.FieldByName('RDB$RELATION_NAME').AsTrimString)]);   

      Line := Line + GetIndexSegments(qryIndex.FieldByName('RDB$INDEX_NAME').AsTrimString) +   
          ')' + fTerminator;

      FMetaData.Add(Line);
      qryIndex.Next;
    end;
  finally
    qryIndex.Free;
  end;
end;

{    ListViews
 Functional description
   Show text of views.
   Use a SQL query to get the info and print it.
   Note: This should also contain check option }

procedure TFXCustomExtract.ListViews(ViewName:String);
const
  ViewSQL =
    'SELECT RDB$RELATION_NAME, RDB$OWNER_NAME, RDB$VIEW_SOURCE ' +  
    'FROM RDB$RELATIONS ' +   
    'WHERE ' +       
    '  (RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL) AND ' +  
    '  NOT RDB$VIEW_BLR IS NULL AND ' +   
    '  RDB$FLAGS = 1 ' +    
    'ORDER BY RDB$RELATION_ID';  

  ViewNameSQL =
    'SELECT RDB$RELATION_NAME, RDB$OWNER_NAME, RDB$VIEW_SOURCE ' + 
    'FROM RDB$RELATIONS ' +   
    'WHERE ' +  
    '  (RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL) AND ' +   
    '  NOT RDB$VIEW_BLR IS NULL AND ' +  
    '  RDB$FLAGS = 1 AND ' +   
    '  RDB$RELATION_NAME = :ViewName ' +   
    'ORDER BY RDB$RELATION_ID';   

  ColumnSQL =
    'SELECT RDB$FIELD_NAME FROM RDB$RELATION_FIELDS ' +    
    'WHERE ' +  
    '  RDB$RELATION_NAME = :RELATION_NAME ' +    
    'ORDER BY RDB$FIELD_POSITION'; 

var
  qryView, qryColumns:TFXCustomSQL;
  SList:TStrings;
begin
  qryView := CreateSQL;
  qryColumns := CreateSQL;
  SList := TStringList.Create;
  try
    if ViewName = '' then  
      qryView.SQL.Text := ViewSQL
    else
    begin
      qryView.SQL.Text := ViewNameSQL;
      qryView.Params.ByName('ViewName').AsTrimString := ViewName;  
    end;
    qryColumns.SQL.Text := ColumnSQL;
    qryView.ExecQuery;
    while not qryView.Eof do
    begin
      SList.Add(Format('%s/* View: %s, Owner: %s */%s',  
         [CRLF, qryView.FieldByName('RDB$RELATION_NAME').AsTrimString,   
          qryView.FieldByName('RDB$OWNER_NAME').AsTrimString, CRLF])); 

      SList.Add(Format('CREATE VIEW %s (', [Database.QuoteIdentifier(
        qryView.FieldByName('RDB$RELATION_NAME').AsTrimString)])); 

      qryColumns.Params.ByName('RELATION_NAME').AsTrimString :=  
          qryView.FieldByName('RDB$RELATION_NAME').AsTrimString;  
      qryColumns.ExecQuery;
      while not qryColumns.Eof do
      begin
        SList.Add('  ' + Database.QuoteIdentifier(
           qryColumns.FieldByName('RDB$FIELD_NAME').AsTrimString));  
        qryColumns.Next;
        if not qryColumns.Eof then
          SList.Strings[SList.Count - 1] := SList.Strings[SList.Count - 1] + ', ';  
      end;
      qryColumns.Close;
      SList.Text := SList.Text + Format(') AS%s', [CRLF]);  
      if not qryView.FieldByName('RDB$VIEW_SOURCE').IsNull then    
        SList.Text := SList.Text + qryView.FieldByName('RDB$VIEW_SOURCE').AsTrimString;   
      SList.Text := SList.Text + Format('%s%s', [fTerminator, CRLF]);
      FMetaData.AddStrings(SList);
      SList.Clear;
      qryView.Next;
    end;
  finally
    qryView.Free;
    qryColumns.Free;
    SList.Free;
  end;
end;

{    PrintSet
  Functional description
     print (using ISQL_printf) the word "SET"
     if the first line of the ALTER DATABASE
     settings options. Also, add trailing
     comma for end of prior line if needed.

  uses Print_buffer, a global }

function TFXCustomExtract.PrintSet(var Used: Boolean):String;
begin
  if not Used then
  begin
    Result := '  SET ';  
    Used := true;
  end
  else
    Result := Format(', %s      ', [CRLF]);
end;


procedure TFXCustomExtract.ExtractObject(ObjectType:TExtractObjectTypes;ObjectName:String = ''; ExtractTypes:TExtractTypes = []);
var DidActivate:Boolean;
begin

  Start_TR;
  
  DidActivate := false;
  if not Transaction.Active then
  begin
    Transaction.StartTransaction;
    DidActivate := true;
  end;
  FMetaData.Clear;
  case ObjectType of
    eoDatabase:ExtractDDL(true, '');
    eoDomain :
      if etTable in ExtractTypes then
        ListDomains(ObjectName, etTable)
      else
        ListDomains(ObjectName);
    eoTable :
    begin
      if ObjectName <> '' then
      begin
        if etDomain in ExtractTypes then
          ListDomains(ObjectName, etTable);
        ExtractListTable(ObjectName, '', false);
        if etIndex in ExtractTypes then
          ListIndex(ObjectName, etTable);
        if etForeign in ExtractTypes then
          ListForeign(ObjectName, etTable);
        if etCheck in ExtractTypes then
          ListCheck(ObjectName, etTable);
        if etTrigger in ExtractTypes then
          ListTriggers(ObjectName, etTable);
        if etGrant in ExtractTypes then
          ShowGrants(ObjectName);
        if etData in ExtractTypes then
          ListData(ObjectName);
      end
      else
        ListAllTables(true);
    end;
    eoView :
    begin
      ListViews(ObjectName);
      if etTrigger in ExtractTypes then
        ListTriggers(ObjectName, etTable);
    end;
    eoProcedure:ListProcs(ObjectName, (etAlterProc in ExtractTypes));
    eoFunction:ListFunctions(ObjectName);
    eoGenerator:ListGenerators(ObjectName);
    eoException:ListException(ObjectName);
    eoBLOBFilter:ListFilters(ObjectName);
    eoRole:ListRoles(ObjectName);
    eoTrigger:
      if etTable in ExtractTypes then
        ListTriggers(ObjectName, etTable)
      else
        ListTriggers(ObjectName);
    eoForeign :
      if etTable in ExtractTypes then
        ListForeign(ObjectName, etTable)
      else
        ListForeign(ObjectName);
    eoIndexes :
      if etTable in ExtractTypes then
        ListIndex(ObjectName, etTable)
      else
        ListIndex(ObjectName);
    eoChecks :
      if etTable in ExtractTypes then
        ListCheck(ObjectName, etTable)
      else
        ListCheck(ObjectName);
    eoData:ListData(ObjectName);
  end;
  if DidActivate then
    Transaction.Commit;
end;

function TFXCustomExtract.GetFieldType(FieldType, FieldSubType, FieldScale,
  FieldSize, FieldPrec, FieldLen: Integer): String;
var
  i:Integer;
  PrecisionKnown:Boolean;
begin
  Result := '';     
  for i := Low(ColumnTypes) to High(ColumnTypes) do
    if FieldType = ColumnTypes[i].SQLType then
    begin
      PrecisionKnown := FALSE;
      if fODSMajorVersion >= ODS_VERSION10 then
      begin
        if FieldType in [blr_short, blr_long, blr_int64] then
        begin
          { We are ODS >= 10 and could be any Dialect }
          if (FieldPrec <> 0) and
             (FieldSubType > 0) and
             (FieldSubType <= MAX_INTSUBTYPES) then
          begin
            Result := Result + Format('%s(%d, %d)', [   
              IntegralSubtypes [FieldSubType],
              FieldPrec,
              -1 * FieldScale]);
            PrecisionKnown := true;
          end;
        end;
      end;
      if PrecisionKnown = false then
      begin
        { Take a stab at numerics and decimals }
        if (FieldType = blr_short) and
            (FieldScale < 0) then
          Result := Result + Format('NUMERIC(4, %d)',  
            [-FieldScale] )
        else
          if (FieldType = blr_long) and
              (FieldScale < 0) then
            Result := Result + Format('NUMERIC(9, %d)',  
              [-FieldScale] )
          else
            if (FieldType = blr_double) and
                (FieldScale  < 0) then
              Result := Result + Format('NUMERIC(15, %d)',  
                [-FieldScale] )
            else
              Result := Result + ColumnTypes[i].TypeName;
      end;
      break;
    end;
  if (FieldType in [blr_text, blr_varying]) and
     (FieldSize <> 0) then
    Result := Result + Format('(%d)', [FieldSize]);  
end;

{  S H O W _ g r a n t s
 Functional description
   Show grants for given object name
   This function is also called by extract for privileges.
     It must extract granted privileges on tables/views to users,
     - these may be compound, so put them on the same line.
   Grant execute privilege on procedures to users
   Grant various privilegs to procedures.
   All privileges may have the with_grant option set. }

procedure TFXCustomExtract.ShowGrants(Const MetaObject: String);
const
  { This query only finds tables, eliminating owner privileges }
  OwnerPrivSQL =
    'SELECT PRV.RDB$USER, PRV.RDB$GRANT_OPTION, PRV.RDB$FIELD_NAME, ' +
    '       PRV.RDB$USER_TYPE, PRV.RDB$PRIVILEGE ' +
    'FROM RDB$USER_PRIVILEGES PRV, RDB$RELATIONS REL ' +
    'WHERE ' +
    '  PRV.RDB$RELATION_NAME = :METAOBJECT AND ' +
    '  REL.RDB$RELATION_NAME = :METAOBJECT AND ' +
    '  PRV.RDB$PRIVILEGE <> ''M'' AND ' +
    '  REL.RDB$OWNER_NAME <> PRV.RDB$USER ' +
    'ORDER BY  PRV.RDB$USER, PRV.RDB$FIELD_NAME, PRV.RDB$GRANT_OPTION';

  ProcPrivSQL =
    'SELECT PRV.RDB$USER, PRV.RDB$GRANT_OPTION, PRV.RDB$FIELD_NAME, ' +
    '       PRV.RDB$USER_TYPE, PRV.RDB$PRIVILEGE, PRV.RDB$RELATION_NAME ' +
    'FROM RDB$USER_PRIVILEGES PRV, RDB$PROCEDURES PRC ' +
    'where ' +
    '  PRV.RDB$OBJECT_TYPE = 5 AND ' +
    '  PRV.RDB$RELATION_NAME = :METAOBJECT AND ' +
    '  PRC.RDB$PROCEDURE_NAME = :METAOBJECT AND ' +
    '  PRV.RDB$PRIVILEGE = ''X'' AND ' +
    '  PRC.RDB$OWNER_NAME <> PRV.RDB$USER ' +
    'ORDER BY PRV.RDB$USER, PRV.RDB$FIELD_NAME, PRV.RDB$GRANT_OPTION';

  RolePrivSQL =
    'SELECT RDB$RELATION_NAME,RDB$GRANT_OPTION FROM RDB$USER_PRIVILEGES ' +
    'WHERE ' +   
    '  RDB$OBJECT_TYPE = 13 AND ' +   
    '  RDB$USER_TYPE = 8  AND ' +  
    '  RDB$RELATION_NAME = :METAOBJECT AND ' +   
    '  RDB$PRIVILEGE = ''M'' ' +   
    'ORDER BY RDB$USER';    

var
  PrevUser, PrevField,  WithOption,
  PrivString, ColString, UserString,
  FieldName, User:String;
  c:Char;
  PrevOption, PrivFlags, GrantOption:Integer;
  First, PrevFieldNull:Boolean;
  qryOwnerPriv:TFXCustomSQL;

    {  Given a bit-vector of privileges, turn it into a
       string list. }
  function MakePrivString(cflags:Integer):String;
  var
    i:Integer;
  begin
    for i := Low(PrivTypes) to High(PrivTypes) do
    begin
      if (cflags and PrivTypes[i].PrivFlag) <> 0 then
      begin
        if Result <> '' then      
          Result := Result + ', ';    
        Result := Result + PrivTypes[i].PrivString;
      end; //end_if
    end; //end_for
  end; //end_fcn MakePrivDtring

begin
  if MetaObject = '' then
    exit;

  First := true;
  PrevOption := -1;
  PrevUser := '';
  PrivString := '';
  ColString := '';
  WithOption := '';
  PrivFlags := 0;
  PrevFieldNull := false;
  PrevField := '';

  qryOwnerPriv := CreateSQL;
  try
    qryOwnerPriv.SQL.Text := OwnerPrivSQL;
    qryOwnerPriv.Params.ByName('metaobject').AsTrimString := MetaObject;   
    qryOwnerPriv.ExecQuery;
    while not qryOwnerPriv.Eof do
    begin
      { Sometimes grant options are null, sometimes 0.  Both same }
      if qryOwnerPriv.FieldByName('RDB$GRANT_OPTION').IsNull then   
        GrantOption := 0
      else
        GrantOption := qryOwnerPriv.FieldByName('RDB$GRANT_OPTION').AsInteger; 

      if qryOwnerPriv.FieldByName('RDB$FIELD_NAME').IsNull then  
        FieldName := ''
      else
        FieldName := qryOwnerPriv.FieldByName('RDB$FIELD_NAME').AsTrimString; 

      User := Trim(qryOwnerPriv.FieldByName('RDB$USER').AsTrimString);  
      { Print a new grant statement for each new user or change of option }

      if ((PrevUser <> '') and (PrevUser <> User)) or
          ((Not First) and
            (PrevFieldNull <> qryOwnerPriv.FieldByName('RDB$FIELD_NAME').IsNull)) or  
          ((not PrevFieldNull) and (PrevField <> FieldName)) or
          ((PrevOption <> -1) and (PrevOption <> GrantOption)) then
      begin
        PrivString := MakePrivString(PrivFlags);

        First := false;
        FMetaData.Add(Format('GRANT %s%s ON %s TO %s%s%s', [PrivString, 
          ColString, Database.QuoteIdentifier(MetaObject),
          UserString, WithOption, Terminator]));
        { re-initialize strings }

        PrivString := '';
        WithOption := '';
        ColString := '';
        PrivFlags := 0;
      end; //end_if

      PrevUser := User;
      PrevOption := GrantOption;
      PrevFieldNull := qryOwnerPriv.FieldByName('RDB$FIELD_NAME').IsNull;  
      PrevField := FieldName;

      case qryOwnerPriv.FieldByName('RDB$USER_TYPE').AsInteger of  
        obj_relation,
        obj_view,
        obj_trigger,
        obj_procedure,
        obj_sql_role:
          UserString := Database.QuoteIdentifier(User);
        else
          UserString := User;
      end; //end_case

      case qryOwnerPriv.FieldByName('RDB$USER_TYPE').AsInteger of  
        obj_view :
          UserString := 'VIEW ' + UserString; 
        obj_trigger :
          UserString := 'TRIGGER '+ UserString;  
        obj_procedure :
          UserString := 'PROCEDURE ' + UserString;  
      end; //end_case

      c := qryOwnerPriv.FieldByName('RDB$PRIVILEGE').AsTrimString[1]; 

      case c of
        'S':PrivFlags := PrivFlags or priv_SELECT;   
        'I':PrivFlags := PrivFlags or priv_INSERT;   
        'U':PrivFlags := PrivFlags or priv_UPDATE;    
        'D':PrivFlags := PrivFlags or priv_DELETE;     
        'R':PrivFlags := PrivFlags or priv_REFERENCES;  
        'X':;                                                 
          { Execute should not be here -- special handling below }
        else
          PrivFlags := PrivFlags or priv_UNKNOWN;
      end; //end_switch

      { Column level privileges for update only }

      if FieldName = '' then
        ColString := ''
      else
        ColString := Format(' (%s)', [Database.QuoteIdentifier(FieldName)]); 

      if GrantOption <> 0 then
        WithOption := ' WITH GRANT OPTION';       

      qryOwnerPriv.Next;
    end;
    { Print last case if there was anything to print }
    if PrevOption <> -1 then
    begin
      PrivString := MakePrivString(PrivFlags);
      First := false;
      FMetaData.Add(Format('GRANT %s%s ON %s TO %s%s%s', [PrivString,  
        ColString, Database.QuoteIdentifier(MetaObject),
        UserString, WithOption, Terminator]));
      { re-initialize strings }
    end; //end_if
    qryOwnerPriv.Close;

    if First then
    begin
     { Part two is for stored procedures only }
      qryOwnerPriv.SQL.Text := ProcPrivSQL;
      qryOwnerPriv.Params.ByName('metaobject').AsTrimString := MetaObject;   
      qryOwnerPriv.ExecQuery;
      while not qryOwnerPriv.Eof do
      begin
        First := false;
        User := Trim(qryOwnerPriv.FieldByName('RDB$USER').AsTrimString); 

        case qryOwnerPriv.FieldByName('RDB$USER_TYPE').AsInteger of  
          obj_relation,
          obj_view,
          obj_trigger,
          obj_procedure,
          obj_sql_role:
            UserString := Database.QuoteIdentifier(User);
          else
            UserString := User;
        end; //end_case
        case qryOwnerPriv.FieldByName('RDB$USER_TYPE').AsInteger of   
          obj_view :
            UserString := 'VIEW ' + UserString;   
          obj_trigger :
            UserString := 'TRIGGER '+ UserString;   
          obj_procedure :
            UserString := 'PROCEDURE ' + UserString;    
        end; //end_case

        if qryOwnerPriv.FieldByName('RDB$GRANT_OPTION').AsInteger = 1 then   
          WithOption := ' WITH GRANT OPTION'       
        else
          WithOption := '';

        FMetaData.Add(Format('GRANT EXECUTE ON PROCEDURE %s TO %s%s%s',  
          [Database.QuoteIdentifier(MetaObject), UserString,
           WithOption, terminator]));

        qryOwnerPriv.Next;
      end;
      qryOwnerPriv.Close;
    end;

    if First then begin
      qryOwnerPriv.SQL.Text := RolePrivSQL;
      qryOwnerPriv.Params.ByName('metaobject').AsTrimString := MetaObject;
      qryOwnerPriv.ExecQuery;
      while not qryOwnerPriv.Eof do begin
        if qryOwnerPriv.FieldByName('RDB$GRANT_OPTION').AsInteger = 1 then
          WithOption := ' WITH ADMIN OPTION' else
          WithOption := '';
        FMetaData.Add(Format('GRANT %s TO %s%s%s',[
           Database.QuoteIdentifier(qryOwnerPriv.FieldByName('RDB$RELATION_NAME').AsTrimString),
           qryOwnerPriv.FieldByName('RDB$USER_NAME').AsTrimString,
           WithOption,
           terminator
           ]));
        qryOwnerPriv.Next;
      end end;

    qryOwnerPriv.Close;
  finally
    qryOwnerPriv.Free;
  end;
end;

{	  ShowGrantRoles
  Functional description
   	Show grants for given role name
   	This function is also called by extract for privileges.
   	All membership privilege may have the with_admin option set. }

procedure TFXCustomExtract.ShowGrantRoles;
const
  RoleSQL =
    'SELECT RDB$USER, RDB$GRANT_OPTION, RDB$RELATION_NAME ' +  
    'FROM RDB$USER_PRIVILEGES ' +    
    'WHERE ' +   
    '  RDB$OBJECT_TYPE = %d AND ' +   
    '  RDB$USER_TYPE = %d AND ' +  
    '  RDB$PRIVILEGE = ''M'' ' +   
    'ORDER BY  RDB$RELATION_NAME, RDB$USER'; 

var
  WithOption, UserString:String;
  qryRole:TFXCustomSQL;

begin
  qryRole := CreateSQL;
  try
    qryRole.SQL.Text := Format(RoleSQL, [obj_sql_role, obj_user]);
    qryRole.ExecQuery;
    while not qryRole.Eof do
    begin
      UserString := Trim(qryRole.FieldByName('RDB$USER').AsTrimString); 

      if (not qryRole.FieldByName('RDB$GRANT_OPTION').IsNull) and    
         (qryRole.FieldByName('RDB$GRANT_OPTION').AsInteger = 1) then  
        WithOption := ' WITH ADMIN OPTION'    
      else
        WithOption := '';
      FMetaData.Add(Format('GRANT %s TO %s%s%s%s',    
        [ Database.QuoteIdentifier(qryRole.FieldByName('RDB$RELATION_NAME').AsTrimString),  
         UserString, WithOption, Terminator,CRLF]));

      qryRole.Next;
    end;
  finally
    qryRole.Free;
  end;
end;

procedure TFXCustomExtract.ListData(ObjectName: String);
const
  SelectSQL = 'SELECT * FROM %s';  
var
  qrySelect:TFXCustomSQL;
  Line, FieldName:String;
  c:TFXSQLVAR;
  d:PXSQLVAR;
  i:Integer;
begin
  qrySelect := CreateSQL;
  try
    qrySelect.SQL.Text := Format(SelectSQL,
      [Database.QuoteIdentifier(ObjectName)]);
    qrySelect.ExecQuery;
    while not qrySelect.Eof do
    begin
      Line := 'INSERT INTO ' + Database.QuoteIdentifier(ObjectName) + ' (';
      for i := 0 to qrySelect.Current.Count - 1 do Begin
        c:=qrySelect.Fields[i];
        if (not (c.SQLType in [SQL_ARRAY,SQL_BLOB])) then begin
          d:=qrySelect.Fields[i].AsXSQLVAR;
          FieldName:=DecodeQuery(d.sqlname, d.sqlname_length);
          Line := Line + Database.QuoteIdentifier(FieldName);
          if i <> (qrySelect.Current.Count - 1) then
            Line := Line + ', ';
        end end;
      Line := Line + ') VALUES (';
      for i := 0 to qrySelect.Current.Count - 1 do begin
        c:=qrySelect.Fields[i];
        if (c.IsNull) and (not (c.SQLType in [SQL_ARRAY,SQL_BLOB])) then begin
          Line := Line + 'NULL';
          if i <> (qrySelect.Current.Count - 1) then
            Line := Line + ', ';  
        end else
        case qrySelect.Fields[i].SQLType of
          SQL_TEXT, SQL_VARYING, SQL_DATE, SQL_TIME, SQL_TIMESTAMP :begin
            Line := Line + QuotedStr(qrySelect.Fields[i].AsTrimString);
            if i <> (qrySelect.Current.Count - 1) then
              Line := Line + ', ';
            end;
          SQL_SHORT, SQL_LONG, SQL_BIGINT,
          SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:begin
            Line := Line + qrySelect.Fields[i].AsTrimString;
            if i <> (qrySelect.Current.Count - 1) then
              Line := Line + ', ';   
            end;
          SQL_ARRAY, SQL_BLOB:Begin
            end;
          else Begin
            FXRaiseClientError(fxceInvalidDataConversion);
          end end;
      end;
      Line := Line + ')' + fTerminator;   
      FMetaData.Add(Line);
      qrySelect.Next;
    end;
  finally
    qrySelect.Free;
  end;
end;


end.

