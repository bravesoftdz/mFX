unit mFX.InternalDataSet;

interface

{$I mFX.Inc}
{$R-,T-,H+,X+,M-}

uses System.Classes, System.SysUtils, System.Types, System.Variants, Data.Db, DataSnap.DBClient,
  mFX.Intf, mFX.Header, mFX.MetaData, mFX.Classes, mFX.Base, mFX.SQL, mFX.Blob,
  {$IFDEF mFXTRACE}mFX.Logger,{$ENDIF mFXTRACE}
  mFX.SQLField;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  TFXDataLink = Class;
  TFXInternalDataSet = Class;

  TFXDataLink = class(TDetailDataLink)
  private
    FDataSet : TFXInternalDataSet;
  protected
    procedure ActiveChanged; override;
    procedure CheckBrowseMode; override;
    function GetDetailDataSet: TDataSet; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(Const ADataSet:TFXInternalDataSet);
  end;

  TFXInternalDataSet = class(TDataSet)
  private
    fClientDataSet    : TCustomClientDataSet;
    fCommandText      : String;
    fTableName        : String;
    fSelectSQL        : TFXCustomSQL;
    fExecSQL          : TFXCustomSQL;
    fInternalPrepared : Boolean;
    fFieldsDefPrepared: Boolean;
    fParamsPrepared   : Boolean;
    fParams           : TParams;
    fDataLink         : TDataLink;
    fKeyFields        : string;
    ///<SUMMARY>Set CommandText</SUMMARY>
    procedure SetCommandText(Const Value: String);
    ///<SUMMARY>Get CommandText</SUMMARY>
    function GetCommandText: String;
    ///<SUMMARY>Get Database</SUMMARY>
    function GetDatabase: TFXCustomDatabase;
    ///<SUMMARY>Set Database</SUMMARY>
    procedure SetDatabase(Const Value: TFXCustomDatabase);
    ///<SUMMARY>Get Transaction</SUMMARY>
    function GetTransaction: TFXCustomTransaction;
    ///<SUMMARY>Set Transaction</SUMMARY>
    procedure SetTransaction(Const Value: TFXCustomTransaction);
    ///<SUMMARY>GetParams</SUMMARY>
    function GetParams: TParams;
    ///<SUMMARY>SetParams</SUMMARY>
    procedure SetParams(Const Values: TParams);
    ///<SUMMARY>GetExecSQL</SUMMARY>
    function GetExecSQL: TFXCustomSQL;

    ///<SUMMARY>Init Fields from SQL</SUMMARY>
    procedure BuildFieldsDef;
    ///<SUMMARY>Init Params from SQL</SUMMARY>
    procedure BuildParamsFromSQL;
    ///<SUMMARY>Init Params from Query</SUMMARY>
    procedure BuildParamsFromQuery;
    ///<SUMMARY>Internal Prepare</SUMMARY>
    procedure InternalPrepare;
    ///<SUMMARY>Internal Un Prepare</SUMMARY>
    procedure InternalUnPrepare;
    ///<SUMMARY>SetPrepared</SUMMARY>
    procedure SetPrepared(Const Value: Boolean);
    ///<SUMMARY>RefreshParams</SUMMARY>
    procedure RefreshParams;

  public
    ///<summary>Free Handle</summary>
    function EncodeQuery(Const Value:String):RawByteString;
    ///<summary>Free Handle</summary>
    function DecodeQuery(Const Value:RawByteString):String;overload;
    ///<summary>Free Handle</summary>
    function DecodeQuery(Const Value:PAnsiChar;Const Len:SmallInt):String;overload;

  protected
    ///<SUMMARY>Set Database</SUMMARY>
    procedure SetName(const Value: TComponentName);override;
    ///<SUMMARY>InternalOpen</SUMMARY>
    procedure InternalOpen; override;
    ///<SUMMARY>IsCursorOpen</SUMMARY>
    function IsCursorOpen: Boolean; override;
    ///<SUMMARY>InternalClose</SUMMARY>
    procedure InternalClose; override;
    ///<SUMMARY>InternalRefresh</SUMMARY>
    procedure InternalRefresh; override;

    ///<SUMMARY>Override InternalInitFieldDefs to use metadata returned by prepared query</SUMMARY>
    procedure InternalInitFieldDefs; override;
    ///<SUMMARY>Override GetFieldClass to use ClientDataSet persitent field ProviderFlags,Required,...</SUMMARY>
    procedure CreateFields;override;
    ///<SUMMARY>Override GetFieldClass to use TFXStringField and TFXBCDField</SUMMARY>
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;

    ///<SUMMARY>GetRecordCount</SUMMARY>
    function GetRecordCount: Integer; override;
    ///<SUMMARY>Get Record</SUMMARY>
    function GetRecord(Buffer:TRecordBuffer;GetMode: TGetMode; DoCheck: Boolean): TGetResult;override;

    procedure InternalHandleException; override;

  protected
    procedure PSEndTransaction(Commit: Boolean);override;
    procedure PSExecute;override;

    function PSExecuteStatement(const ASQL: string; AParams: TParams): Integer; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;Out cds:TFXInternalDataSet): Integer; overload;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;var ResultSet: TDataSet): Integer; override;

    function PSGetDefaultOrder: TIndexDef;override;
    function PSGetParams: TParams;override;
    function PSGetQuoteChar: string;override;
    function PSGetTableName: string;override;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;override;
    function PSInTransaction: Boolean;override;
    function PSIsSQLBased: Boolean;override;
    function PSIsSQLSupported: Boolean;override;
    procedure PSReset;override;
    procedure PSSetParams(AParams: TParams);override;
    procedure PSSetCommandText(const CommandText: string);override;
    procedure PSStartTransaction;override;

  public
    ///<SUMMARY>constructor</SUMMARY>
    constructor Create(AOwner:TComponent);override;
    ///<SUMMARY>constructor</SUMMARY>
    constructor CreateResolver(Const AOwner:TFXInternalDataSet);
    ///<SUMMARY>destructor</SUMMARY>
    destructor Destroy; override;

    function Start_DB:Boolean;
    procedure ReStart_ROTR;
    function Start_ROTR:Boolean;
    function Commit_TR: Boolean;
    function RollBack_TR: Boolean;

    procedure ReStart_RWTR;
    function Start_RWTR:Boolean;

    function RefreshCommandText(const Value:String):Boolean;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetFieldData(Field:TField;Var Buffer:TValueBuffer):Boolean;override;
    function GetFieldData(FieldNo:Integer;Var Buffer:TValueBuffer):Boolean;override;
    function GetFieldData(Field:TField;Var Buffer:TValueBuffer;NativeFormat:Boolean):Boolean; override;

    property Database                 : TFXCustomDatabase    read GetDatabase         write SetDatabase;
    property Transaction              : TFXCustomTransaction read GetTransaction      write SetTransaction;
    property TableName                : String               read fTableName          write fTableName;
    property ExecSQL                  : TFXCustomSQL         read GetExecSQL;

  published
    property CommandText              : String               read GetCommandText      write SetCommandText;
    property Params                   : TParams              read GetParams           write SetParams;

  end;


implementation

uses AnsiStrings, FmtBcd;

const
  mFXDefaultFieldClasses: array[TFieldType] of TFieldClass = (
     nil                                                                        // ftUnknown
    ,TFXStringField                                                             // ftString
    ,TSmallintField                                                             // ftSmallint
    ,TIntegerField                                                              // ftInteger
    ,TWordField                                                                 // ftWord
    ,TBooleanField                                                              // ftBoolean
    ,TFloatField                                                                // ftFloat
    ,TCurrencyField                                                             // ftCurrency
    ,TFXBCDField                                                                // ftBCD
    ,TDateField                                                                 // ftDate
    ,TTimeField                                                                 // ftTime
    ,TDateTimeField                                                             // ftDateTime
    ,TBytesField                                                                // ftBytes
    ,TVarBytesField                                                             // ftVarBytes
    ,TAutoIncField                                                              // ftAutoInc
    ,TBlobField                                                                 // ftBlob
    ,TFXMemoField                                                               // ftMemo }
    ,TGraphicField                                                              // ftGraphic
    ,TFXMemoField                                                               // ftFmtMem
    ,TBlobField                                                                 // ftParadoxOle
    ,TBlobField                                                                 // ftDBaseOle
    ,TBlobField                                                                 // ftTypedBinary
    ,nil                                                                        // ftCursor
    ,TFXStringField                                                             // ftFixedChar
    ,TFXStringField                                                             // ftWideString
    ,TLargeIntField                                                             // ftLargeInt
    ,TADTField                                                                  // ftADT
    ,TArrayField                                                                // ftArray
    ,TReferenceField                                                            // ftReference
    ,TDataSetField                                                              // ftDataSet
    ,TBlobField                                                                 // ftOraBlob
    ,TFXMemoField                                                               // ftOraClob
    ,TVariantField                                                              // ftVariant
    ,TInterfaceField                                                            // ftInterface
    ,TIDispatchField                                                            // ftIDispatch
    ,TGuidField                                                                 // ftGuid
    ,TSQLTimeStampField                                                         // ftTimeStamp
    ,TFMTBcdField                                                               // ftFMTBcd
    ,TFXStringField                                                             // ftFixedWideChar
    ,TFXMemoField                                                               // ftWideMem
    ,nil                                                                        // ftOraTimeStamp
    ,nil                                                                        // ftOraInterva
    {$IFDEF RTL200_UP}
    ,TIntegerField                                                              // ftLongWord,
    ,TIntegerField                                                              // ftShortint,
    ,TIntegerField                                                              // ftByte,
    ,TFloatField                                                                // ftExtended,
    ,nil                                                                        // ftConnection,
    ,nil                                                                        // ftParams,
    ,nil                                                                        // ftStream
    {$ENDIF RTL200_UP}
    {$IFDEF RTL210_UP}
    ,nil                                                                        // ftTimeStampOffset
    ,nil                                                                        // ftObject
    ,nil                                                                        // ftSingle
    {$ENDIF RTL210_UP}
    );

Type
  /// <summary>TFXDSBlobStream</summary>
  TFXDSBlobStream = class(TFXBlobStream)
  private
    /// <summary>construct</summary>
    constructor CreateFromBlobField(Const aSQL:TFXCustomSQL;Const AField: TField);overload;
  end;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXDSBlobStream.CreateFromBlobField(Const aSQL:TFXCustomSQL;Const AField: TField);
Var LChars:TCharArray;
    bs:TBytesStream;
    Len:Integer;
    p:TFXSQLVAR;
begin
  inherited Create(aSQL);
  p:=aSQL.Current[Pred(AField.FieldNo)];
  Self.AsQuad:=PISC_QUAD(p.Data^.sqlData)^;
  Self.OpenBlob;
  if Self.BlobSize>0 then Begin
    case p.SQLBlobType of
      isc_blob_text:Begin
        bs:=TBytesStream.Create;
        try Self.SaveToStream(bs);
            LChars:=aSQL.Encoding.GetChars(bs.bytes,0,bs.Size);
            Len:=Length(LChars)*SizeOf(Char);
            Self.SetSize(Len);
            Self.Seek(0, soFromBeginning);
            Move(LChars[0],Self.BlobBuffer^,Len);
        finally
            bs.Free;
    end end end end;
  Self.Seek(0, soFromBeginning);
  Assert(p.SQLType=SQL_BLOB);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXDataLink.Create(Const ADataSet:TFXInternalDataSet);
begin
  inherited Create;
  fDataSet:=ADataSet;
end;
{______________________________________________________________________________}
procedure TFXDataLink.ActiveChanged;
begin
  if fDataSet.Active then
    fDataSet.RefreshParams;
end;
{______________________________________________________________________________}
function TFXDataLink.GetDetailDataSet: TDataSet;
begin
  Result:=fDataSet;
end;
{______________________________________________________________________________}
procedure TFXDataLink.RecordChanged(Field: TField);
begin
  if Field=nil then
    if fDataSet.Active then
      fDataSet.RefreshParams;
end;
{______________________________________________________________________________}
procedure TFXDataLink.CheckBrowseMode;
begin
  if fDataSet.Active then
    fDataSet.CheckBrowseMode;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXInternalDataSet.Create(AOwner:TComponent);
Begin
  inherited Create(AOwner);
  Assert(AOwner is TCustomClientDataSet);
  fClientDataSet:=TCustomClientDataSet(AOwner);
  fSelectSQL:=TFXCustomSQL.Create(Self);
  fParams:=TParams.Create(Self);
  FDataLink:=TFXDataLink.Create(Self);
  SetUniDirectional(True);
  DisableControls;
  ObjectView:=True
End;
{______________________________________________________________________________}
constructor TFXInternalDataSet.CreateResolver(Const AOwner:TFXInternalDataSet);
Begin
  inherited Create(nil);
  fClientDataSet:=AOwner.fClientDataSet;
  fSelectSQL:=TFXCustomSQL.Create(Self);
  fParams:=TParams.Create(Self);
  FDataLink:=TFXDataLink.Create(Self);
  SetUniDirectional(True);
  DisableControls;
  ObjectView:=True
End;
{______________________________________________________________________________}
destructor TFXInternalDataSet.Destroy;
begin
  Self.Close;
  FreeAndNil(fExecSQL);
  FreeAndNil(fSelectSQL);
  FreeAndNil(fParams);
  inherited Destroy;
  FDataLink.Free;
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.SetName(const Value: TComponentName);
Begin
  FreeAndNil(fExecSQL);
  inherited SetName(Value);
  fSelectSQL.Name:=Self.Name+'_ISQL';
End;
{______________________________________________________________________________}
procedure TFXInternalDataSet.SetCommandText(Const Value: String);
Var s:String;
Begin
  s:=Trim(Value);
  if s<>fCommandText then Begin
    SetPrepared(False);
    fCommandText:=s;
    DataEvent(dePropertyChange,0);
    end
End;
{______________________________________________________________________________}
function TFXInternalDataSet.GetCommandText: String;
Begin
  Result:=fCommandText
End;
{______________________________________________________________________________}
function TFXInternalDataSet.RefreshCommandText(const Value:String):Boolean;
Var s:String;
Begin
  s:=Trim(Value);
  if Not SameText(s,fCommandText) then Begin
    fInternalPrepared:=False;
    fCommandText:=s;
    Result:=True
  End else
    Result:=False
End;
{______________________________________________________________________________}
function TFXInternalDataSet.GetDatabase: TFXCustomDatabase;
begin
  result:=fSelectSQL.Database;
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.SetDatabase(Const Value: TFXCustomDatabase);
begin
  FreeAndNil(fExecSQL);
  fSelectSQL.Database:=Value;
end;
{______________________________________________________________________________}
function TFXInternalDataSet.GetTransaction: TFXCustomTransaction;
begin
  result:=fSelectSQL.Transaction;
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.SetTransaction(Const Value: TFXCustomTransaction);
begin
  FreeAndNil(fExecSQL);
  fSelectSQL.Transaction:=Value;
end;
{______________________________________________________________________________}
function TFXInternalDataSet.GetParams: TParams;
Begin
  if not fParamsPrepared then
    BuildParamsFromQuery; 
  Result:=fParams;
End;
{______________________________________________________________________________}
procedure TFXInternalDataSet.SetParams(Const Values: TParams);
Begin
  fParams.Assign(Values);
End;
{______________________________________________________________________________}
function TFXInternalDataSet.GetExecSQL:TFXCustomSQL;
Begin
  if fExecSQL=nil then
    fExecSQL:=TFXCustomSQL.Create(Self);
  fExecSQL.Database:=fSelectSQL.Database;
  fExecSQL.Transaction:=fSelectSQL.Transaction;
  Result:=fExecSQL
End;
{______________________________________________________________________________}
procedure TFXInternalDataSet.InternalOpen;
Var pp:TFXSQLVAR;
    i:integer;
    p:TParam;
begin
  Start_DB;
  Start_ROTR;
  SetPrepared(True);
  FreeAndNil(fExecSQL);
  //Assign fSelectSQL Params from fParams
  Assert(fParamsPrepared);
  Assert(fParams.Count=fSelectSQL.Params.Count);
  for I := 0 to Pred(fParams.Count) do begin
    pp:=fSelectSQL.Params[i];
    p:=fParams[i];
    pp.Assign(p)
    end;
  // OK Now execute the query
  fSelectSQL.OpenCursor;
  FieldDefs.Update;
  Self.CreateFields;
  BindFields(True);
end;
{______________________________________________________________________________}
function TFXInternalDataSet.IsCursorOpen: Boolean;
begin
  Result:=fSelectSQL.CursorOpen
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.InternalClose;
begin
  fSelectSQL.CloseQuery;
  FreeAndNil(fExecSQL);
  BindFields(False);
  DestroyFields;
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.InternalRefresh;
begin
  SetState(dsInactive);
  CloseCursor;
  OpenCursor(False);
  SetState(dsBrowse);
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.BuildParamsFromSQL;
Var SavValues:TParams;
    s:TFXSQLVAR;
    p,pp:TParam;
    i:integer;
Begin
  SavValues:=nil;
  try //Save actual Values iff any !!!!
      if fParams.Count>0 then Begin 
        SavValues:=TParams.Create(nil);
        SavValues.Assign(fParams);
        fParams.Clear;
        End;
      Assert(fSelectSQL.Prepared);
      //Get params from prepared query ...
      for I := 0 to Pred(fSelectSQL.Params.Count) do begin
        s:=fSelectSQL.Params[i];
        p:=TParam(fParams.Add);
        p.Name:=s.Name;
        End;
      //Assign Back Values saved if any !!!!
      if SavValues<>nil then Begin 
        for I := 0 to Pred(fParams.Count) do begin
          p:=fParams[i];
          pp:=SavValues.FindParam(p.Name);
          if pp<>nil then
            p.Value:=pp.Value;
        end end;
      fParamsPrepared:=True;
  finally
      SavValues.Free
  end;
End;
{______________________________________________________________________________}
procedure TFXInternalDataSet.BuildParamsFromQuery;
Var SavValues:TParams;
    p,pp:TParam;
    i:Integer;
Begin
  SavValues:=nil;
  try //Save actual Values iff any !!!!
      if fParams.Count>0 then Begin 
        SavValues:=TParams.Create(nil);
        SavValues.Assign(fParams);
        fParams.Clear;
        End;
      //Get params from command text ...
      if fCommandText<>'' then 
        fParams.ParseSQL(fCommandText,True);
      //Assign Back Values saved if any !!!!
      if SavValues<>nil then Begin 
        for I := 0 to Pred(fParams.Count) do begin
          p:=fParams[i];
          pp:=SavValues.FindParam(p.Name);
          if pp<>nil then
            p.Value:=pp.Value;
        end end;
      fParamsPrepared:=True;
  finally
      SavValues.Free
  end;
End;
{______________________________________________________________________________}
procedure TFXInternalDataSet.RefreshParams;
var DataSet: TDataSet;
begin
  if FDataLink.DataSource <> nil then begin
    DisableControls;
    try DataSet := FDataLink.DataSource.DataSet;
        if DataSet <> nil then Begin
          if (DataSet.Active)and(DataSet.State<>dsSetKey) then begin
            Close;
            Open
          end end
    finally
        EnableControls
    end end
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXInternalDataSet.EncodeQuery(Const Value:String):RawByteString;
Var Bytes: TBytes;
Begin
  Bytes:=Self.Database.Encoding.GetBytes(Value);
  SetLength(Result,Length(Bytes));
  move(Bytes[0], Result[1], Length(Bytes));
End;
{______________________________________________________________________________}
function TFXInternalDataSet.DecodeQuery(Const Value:RawByteString):String;
Var Bytes: TBytes;
Begin
  SetLength(Bytes,Length(Value));
  move(Value[1], Bytes[0], Length(Value));
  Result:=Self.Database.Encoding.GetString(Bytes);
End;
{______________________________________________________________________________}
function TFXInternalDataSet.DecodeQuery(Const Value:PAnsiChar;Const Len:SmallInt):String;
Var Bytes: TBytes;
Begin
  SetLength(Bytes,Len);
  move(Value[0], Bytes[0], Len);
  Result:=Self.Database.Encoding.GetString(Bytes);
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXInternalDataSet.Start_DB:Boolean;
Var DB:TFXCustomDatabase;
begin
  DB:=fSelectSQL.Database;
  if DB=nil then
    FXRaiseClientError(fClientDataSet,fxceDatabaseNotAssigned);

  if not DB.Connected then Begin
    fInternalPrepared:=False;
    Result:=True;
    DB.Open;
  End else
    Result:=False
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXInternalDataSet.Commit_TR: Boolean;
Var TR:TFXCustomTransaction;
begin
  Result := False;
  TR:=fSelectSQL.Transaction;
  if TR<>nil then Begin
    With TR do Begin
      If Intransaction then Begin
        Result := True;
        Commit
    end end end;
  fSelectSQL.Transaction:=nil;
  if fExecSQL<>nil then
    fExecSQL.Transaction:=nil;
end;
{______________________________________________________________________________}
function TFXInternalDataSet.RollBack_TR: Boolean;
Var TR:TFXCustomTransaction;
begin
  Result := False;
  TR:=fSelectSQL.Transaction;
  if TR<>nil then Begin
    With TR do Begin
      If Intransaction then Begin
        Result := True;
        Rollback
    end end end;
  fSelectSQL.Transaction:=nil;
  if fExecSQL<>nil then
    fExecSQL.Transaction:=nil;
end;
{______________________________________________________________________________}
function TFXInternalDataSet.Start_ROTR: Boolean;
Var TR:TFXCustomTransaction;
begin
  Result := False;
  TR:=fSelectSQL.Database.ROAutoTR;
  fSelectSQL.Transaction:=TR;
  if fExecSQL<>nil then
    fExecSQL.Transaction:=TR;
  With TR do Begin
    if not InTransaction then begin
      StartTransaction;
      Result := True
    end end
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.ReStart_ROTR;
Begin
  Self.Commit_TR;
  Self.Start_ROTR;
end;
{______________________________________________________________________________}
function TFXInternalDataSet.Start_RWTR: Boolean;
Var TR:TFXCustomTransaction;
begin
  Result := False;
  TR:=fSelectSQL.Database.RWAutoTR;
  fSelectSQL.Transaction:=TR;
  if fExecSQL<>nil then
    fExecSQL.Transaction:=TR;
  With TR do Begin
    if not InTransaction then begin
      StartTransaction;
      Result := True
    end end
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.ReStart_RWTR;
Begin
  Self.Commit_TR;
  Self.Start_RWTR;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXInternalDataSet.SetPrepared(Const Value: Boolean);
begin
  if Value then Begin
    if not fInternalPrepared then
      InternalPrepare
  end else
    InternalUnPrepare
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.InternalPrepare;
var DidStart: Boolean;
begin
  if fCommandText='' then
    FXRaiseClientError(fClientDataSet,fxceEmptyQuery);

  DidStart:=False;
  try //Start_DB;
      FreeAndNil(fExecSQL);
      DidStart:=Start_ROTR;
      With fSelectSQL do Begin
        SQL.Text:=fCommandText;
        Prepare;
        end;
      fTableName:=fSelectSQL.UniqueRelationName;
      FInternalPrepared:=True;
      if not fFieldsDefPrepared then
        Self.BuildFieldsDef;
      Assert(FieldDefs.Count=fSelectSQL.Current.Count);
      if (not fParamsPrepared)or(fParams.Count<>fSelectSQL.Params.Count) then
        Self.BuildParamsFromSQL;
      Assert(fParams.Count=fSelectSQL.Params.Count);
  finally
      if DidStart then
        Commit_TR
  end
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.InternalUnPrepare;
begin
  fFieldsDefPrepared:=False;
  FInternalPrepared:=False;
  fParamsPrepared:=False;
  fTableName:='';
  FreeAndNil(fExecSQL);
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.BuildFieldsDef;
var i,SQLFieldType,FieldPrecision: Integer;
    RelationName,FieldName:string;
    TR:TFXCustomTransaction;
    Schema:TFXCustomSchema;
    FieldNullable:Boolean;
    FieldAliasName:string;
    FieldType:TFieldType;
    sf:TFXMetaTableField;
    FieldDataSize:Word;
    f:TFieldDef;
    c:TFXSQLVAR;
    d:PXSQLVAR;
begin
  FieldDefs.BeginUpdate;
  try FieldDefs.Clear;
      // Prepare Schema
      TR:=fSelectSQL.Transaction;
      Schema:=fSelectSQL.Schema;
      // Loop on Output Fields
      for i := 0 to Pred(fSelectSQL.Current.Count) do Begin
        c:=fSelectSQL.Current[i];
        d:=c.Data;
        // RAZ And Get the field name
        FieldDataSize := 0;
        FieldPrecision:= 0;
        FieldType     := ftUnknown;
        FieldNullable := c.IsNullable;
        SQLFieldType  := d^.sqltype and not 1;
        FieldAliasName:= DecodeQuery(d^.aliasname, d^.aliasname_length);
        RelationName  := DecodeQuery(d^.relname  , d^.relname_length  );
        FieldName     := DecodeQuery(d^.sqlname  , d^.sqlname_length  );
        // Map SQLFieldType to FieldType
        case SQLFieldType of
          // All VARCHAR's must be converted to strings before recording their values
          SQL_TEXT:Begin
            FieldPrecision   := d^.sqllen;
            if d^.SQLSubtype>1 then
              FieldDataSize  := FieldPrecision div d^.SQLSubtype else
              FieldDataSize  := FieldPrecision;
            FieldType        := ftFixedWideChar;
            end;
          SQL_VARYING:Begin
            FieldPrecision   := d^.sqllen;
            if d^.SQLSubtype>1 then
              FieldDataSize  := FieldPrecision div d^.SQLSubtype else
              FieldDataSize  := FieldPrecision;
            FieldType        := ftWideString;
            end;
          // All Doubles/Floats should be cast to doubles  .....................
          SQL_FLOAT, SQL_DOUBLE, SQL_D_FLOAT:Begin
            FieldType        := ftFloat;
            end;
          //Short Can be Integer or Float  .....................................
          SQL_SHORT:begin
            if (d^.sqlscale >= 0) then Begin
              FieldType      := ftSmallInt
            end else begin
              FieldType      := ftBCD;
              FieldPrecision := 4;
              FieldDataSize  :=-d^.sqlscale;
            end end;
          // Long Can be Integer or Float ......................................
          SQL_LONG:begin
            if (d^.sqlscale>= 0) then Begin
              FieldType      := ftInteger
            end else
            if (d^.sqlscale>=-4) then begin
              FieldType      := ftBCD;
              FieldPrecision := 9;
              FieldDataSize  :=-d^.sqlscale;
            end else Begin
              FieldType := ftFloat;
            end end;
          // BigInt Can be Integer or Float. ...................................
          SQL_BIGINT:begin
            if (d^.sqlscale = 0) then Begin
              FieldType      := ftLargeInt
            end else
            if (d^.sqlscale >= (-4)) then begin
              FieldType      := ftBCD;
              FieldPrecision := 18;
              FieldDataSize  :=-d^.sqlscale;
            end else Begin
              FieldType      := ftFloat;
            end end;
          // Date/Time .........................................................
          SQL_TIMESTAMP:Begin
            FieldType        := ftDateTime;
            end;
          SQL_TIME:Begin
            FieldType        := ftTime;
            end;
          SQL_DATE:Begin
            FieldType        := ftDate;
            end;
          // Blob ..............................................................
          SQL_BLOB: begin
            FieldDataSize    := sizeof (TISC_QUAD);
            Case c.SQLBlobType of
              isc_blob_text:FieldType :=ftWideMemo;
              else FieldType := ftBlob;
          end end end;
        if (FieldType <> ftUnknown) then begin
          f:=FieldDefs.AddFieldDef;
          f.Name     := String(FieldAliasName);
          f.FieldNo  := Succ(i);
          f.DataType := FieldType;
          f.Size     := FieldDataSize;
          f.Precision:= FieldPrecision;
          f.Required := not FieldNullable;
          f.InternalCalcField := False;
          if (FieldName<>'')and(RelationName<>'')and(Schema<>nil) then begin
            sf:=(Schema as TFXCustomMetaSchema).FindRelationField(TR,RelationName,FieldName);
            if sf<>nil then Begin
              if sf.IsComputed then Begin
                f.Attributes := [faReadOnly];
                f.InternalCalcField := True;
                end;
              if sf.HasDefault then Begin
                if not FieldNullable then
                  f.Attributes := [faRequired];
        end end end end end;
      fFieldsDefPrepared:=True;
  finally
      FieldDefs.EndUpdate
  end
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.CreateFields;
var TR:TFXCustomTransaction;
    Schema:TFXCustomSchema;
    sf:TFXMetaTableField;
    Data:PXSQLVAR;
    fd:TFieldDef;
    fn,rn:String;
    f,ff:TField;
    i:Integer;
begin
  if FieldDefs.Count>0 then Begin
    // Prepare Schema
    TR:=fSelectSQL.Transaction;
    Schema:=fSelectSQL.Database.Schema;
    //Create Fields from FieldDefs
    for i:=0 to Pred(FieldDefs.Count) do Begin
      fd:=FieldDefs[i];
      if (fd.DataType=ftUnknown) then
        Continue;
      if (faHiddenCol in fd.Attributes)and(not Self.FieldDefs.HiddenFields) then
        Continue;
      f:=fd.CreateField(Self);
      case fd.DataType of
        ftFixedWideChar,ftFixedChar,
        ftWideString,ftString:Begin
           With (f as TFXStringField) do Begin
             FixedChar:= False;
             Bytes    := fd.Precision
        end end end;
      fn:=f.FieldName;
      ff:=fClientDataSet.FindField(fn);
      if ff<>nil then Begin
        // We have persistent fields
        if ff is TFXStringField then
          (ff as TFXStringField).Bytes:=fd.Precision;
        f.ProviderFlags:=ff.ProviderFlags;
        f.Required:=ff.Required;
      end else Begin
        Data:=fSelectSQL.Current[Pred(fd.FieldNo)].Data;
        rn:=DecodeQuery(Data^.relname, Data^.relname_length);
        if (rn<>'')and(fn<>'')and(Schema<>nil) then begin
          sf:=(Schema as TFXCustomMetaSchema).FindRelationField(TR,rn,fn);
          if sf<>nil then Begin
            if sf.IsComputed then
              f.ReadOnly := True;
            if sf.FieldInKey then
              f.ProviderFlags:=f.ProviderFlags+[pfInKey];
    end end end end end;
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.InternalInitFieldDefs;
begin
  if not fInternalPrepared then
    InternalPrepare;
  if not fFieldsDefPrepared then
    BuildFieldsDef;
end;
{______________________________________________________________________________}
function TFXInternalDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := mFXDefaultFieldClasses[FieldType];
end;
{______________________________________________________________________________}
function TFXInternalDataSet.GetRecord(Buffer:TRecordBuffer;GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Case GetMode of
    gmCurrent:Begin
      Result:=grOK
      end;
    gmNext   :Begin
      fSelectSQL.Next;
      If Not fSelectSQL.Eof then Begin
        Result:=grOK;
      End else
        Result:=grEOF
      end;
    else Begin
      Result:=grError;
    end end;
end;
{______________________________________________________________________________}
function TFXInternalDataSet.GetRecordCount: Integer;
begin
  if fSelectSQL.CursorOpen then
    Result:=fSelectSQL.RecordCount else
    Result:=-1
end;
{______________________________________________________________________________}
function TFXInternalDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
Begin
  Assert(Mode=bmRead);
  result:=TFXDSBlobStream.CreateFromBlobField(fSelectSQL,Field);
end;
{______________________________________________________________________________}
function TFXInternalDataSet.GetFieldData(FieldNo:Integer;Var Buffer:TValueBuffer):Boolean;
Begin
  Result := GetFieldData(FieldByNumber(FieldNo), Buffer, False);
End;
{______________________________________________________________________________}
function TFXInternalDataSet.GetFieldData(Field:TField;Var Buffer:TValueBuffer):Boolean;
Begin
  Result := GetFieldData(Field, Buffer, False);
End;
{______________________________________________________________________________}
function TFXInternalDataSet.GetFieldData(Field:TField;Var Buffer:TValueBuffer;NativeFormat:Boolean):Boolean;
var LocalCurrency:Currency;
    LocalInt32: Integer;
    LocalInt64:FXInt64;
    LocalDouble:FXDouble;
    LocalFloat:FXFloat;
    LocalBCD: TBcd;
    ptimestamp:PISC_TIMESTAMP;
    pdate:PISC_DATE;
    ptime:PISC_TIME;
    t:TTimeStamp;
    LChars: TCharArray;
    str_len:Integer;
    len:Integer;
    p:TFXSQLVAR;
    d:PXSQLVAR;
    b:TBytes;
begin
  Assert(Field.FieldNo>0);
  Assert(fSelectSQL.CursorOpen);
  Assert(Field.FieldNo<=fSelectSQL.Current.Count);

  p:=fSelectSQL.Current[Pred(Field.FieldNo)];
  If (Not p.IsNull)and(Buffer<>nil) then Begin
    d:=p.Data;
    Case p.SQLType of
      // All VARCHAR's must be converted to strings before recording their values
      SQL_TEXT:Begin
        str_len:=d^.sqllen;
        if d^.SQLSubtype>1 then
          len:=str_len div d^.SQLSubtype else
          len:=str_len;
        if (len*2+2)>Field.DataSize then
          FXRaiseClientErrorFmt(fClientDataSet,fxceFieldSizeMismatch,[Field.FieldName]);
        SetLength(b,str_len);
        move(d^.sqldata^, b[0], str_len);
        LChars:=fSelectSQL.Encoding.GetChars(b);
        str_len:=Length(LChars);
        if not (Field as TStringField).FixedChar then
          len:=0;
        while (str_len>len)and(LChars[Pred(str_len)]<=#32) do
          Dec(str_len);
        Move(LChars[0],Buffer[0],str_len*2);
        PWordArray(Buffer)[str_len]:=0;
        end;
      SQL_VARYING:Begin
        str_len:=d^.sqllen;
        if d^.SQLSubtype>1 then
          len:=str_len div d^.SQLSubtype else
          len:=str_len;
        Assert(len=Field.Size);
        if (len*2+2)>Field.DataSize then
          FXRaiseClientErrorFmt(fClientDataSet,fxceFieldSizeMismatch,[Field.FieldName]);
        len:=mFX.Header.vax_integer(d^.sqldata, 2);
        SetLength(b,len);
        move(PAnsiChar(d^.sqldata)[2], b[0], len);
        LChars:=fSelectSQL.Encoding.GetChars(b);
        len:=Length(LChars);
        Move(LChars[0],Buffer[0],len*2);
        PWordArray(Buffer)[len]:=0;
        end;
      // All Doubles/Floats should be cast to doubles  .....................
      SQL_FLOAT:begin
        Assert(Field is TFloatField);
        Assert(d^.sqllen=SizeOf(FXFloat));
        Assert(Field.DataSize=SizeOf(Double));
        LocalFloat:=PFXFloat(d^.sqldata)^;
        Move(LocalFloat,Buffer[0],SizeOf(LocalFloat))
        end;
      SQL_DOUBLE, SQL_D_FLOAT:Begin
        Assert(Field is TFloatField);
        Assert(d^.sqllen=SizeOf(FXDouble));
        Assert(Field.DataSize=SizeOf(Double));
        Assert(SizeOf(FXDouble)=SizeOf(FXDouble));
        Move(d^.sqldata^,Buffer[0],SizeOf(Double))
        end;
      //Short Can be Integer or Float  .....................................
      SQL_LONG,
      SQL_BIGINT,
      SQL_SHORT:Begin
        Assert(d^.sqlscale<=0);
        if (d^.sqlscale= 0) then Begin
          Case p.SQLType of
            SQL_SHORT:Begin
              Assert(Field is TIntegerField);
              Assert(SizeOf(FXShort)=d^.sqllen);
              Assert(Field.DataSize=SizeOf(FXShort));
              Move(d^.sqlData^,Buffer[0],SizeOf(FXShort))
              end;
            SQL_LONG :Begin
              Assert(Field is TIntegerField);
              Assert(SizeOf(FXLong)=d^.sqllen);
              Assert(Field.DataSize=SizeOf(FXLong));
              Move(d^.sqlData^,Buffer[0],SizeOf(PFXLong))
              end;
            else Begin
              Assert(Field is TLargeintField);
              Assert(SizeOf(FXInt64)=d^.sqllen);
              Assert(Field.DataSize=SizeOf(FXInt64));
              Move(d^.sqlData^,Buffer[0],SizeOf(FXInt64))
            end end;
        end else
        if (d^.sqlscale>=-4) then begin
          Case p.SQLType of
            SQL_SHORT :LocalInt64:=Int64(PFXShort(d^.sqldata)^);
            SQL_LONG  :LocalInt64:=Int64(PFXLong (d^.sqldata)^);
            else       LocalInt64:=      PFXInt64(d^.sqldata)^ ;
            end;
          LocalCurrency:=p.AdjustScale2Currency(LocalInt64,d^.sqlscale);
          if (Field.DataType = ftBCD) then begin
            Assert(Field is TBCDField);
            Move(d^.sqlData^,LocalBCD,SizeOf(LocalBCD));
            CurrToBCD(LocalCurrency, LocalBCD, 32, Field.Size);
            Move(LocalBCD,Buffer[0],SizeOf(LocalBCD))
          end else Begin
            Assert(Field is TCurrencyField);
            Move(LocalCurrency,Buffer[0],SizeOf(LocalCurrency))
            end
        end else begin
          Assert(Field is TFloatField);
          Case p.SQLType of
            SQL_SHORT:LocalInt64:=Int64(PFXShort(d^.sqldata)^);
            SQL_LONG :LocalInt64:=Int64(PFXLong (d^.sqldata)^);
            else      LocalInt64:=     (PFXInt64(d^.sqldata)^);
            end;
          LocalDouble:=p.AdjustScale2Double(LocalInt64,d^.sqlscale);
          Move(LocalDouble,Buffer[0],SizeOf(LocalDouble))
        end end;
      // Date/Time .........................................................
      SQL_DATE:begin
        Assert(Field is TDateField);
        Assert(Field.DataSize=SizeOf(LocalInt32));
        pdate:=PISC_DATE(d^.sqldata);
        DecodeSQLDate(pdate,t);
        LocalInt32:=t.Date;
        Move(LocalInt32,Buffer[0],SizeOf(LocalInt32));
        end;
      SQL_TIME:Begin
        Assert(Field is TTimeField);
        Assert(Field.DataSize=SizeOf(LocalInt32));
        ptime:=PISC_TIME(d^.sqldata);
        DecodeSQLTime(ptime,t);
        LocalInt32:=t.Time;
        Move(LocalInt32,Buffer[0],SizeOf(LocalInt32));
        end;
      SQL_TIMESTAMP:begin
        Assert(Field is TDateTimeField);
        Assert(Field.DataSize=SizeOf(LocalDouble));
        ptimestamp:=PISC_TIMESTAMP(d^.sqldata);
        DecodeSQLTimeStamp(ptimestamp,t);
        LocalDouble:=TimeStampToMSecs(t);
        Move(LocalDouble,Buffer[0],SizeOf(LocalDouble));
        end;
      // Blob ,  ............................................................
      else Begin
        FXRaiseClientError(fClientDataSet,fxceInvalidDataConversion);
      end end;
    Result := True
  end else
    Result := False
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.InternalHandleException;
begin
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXInternalDataSet.PSIsSQLBased: Boolean;
begin
  Result:=True
end;
{______________________________________________________________________________}
function TFXInternalDataSet.PSIsSQLSupported: Boolean;
begin
  Result:=True
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.PSStartTransaction;
begin
  fSelectSQL.Transaction:=fSelectSQL.Database.RWAutoTR;
  if fExecSQL<>nil then
    fExecSQL.Transaction:=fSelectSQL.Database.RWAutoTR;
  fSelectSQL.Transaction.StartTransaction;
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.PSEndTransaction(Commit: Boolean);
begin
  if Commit then
    fSelectSQL.Transaction.Commit else
    fSelectSQL.Transaction.Rollback;
  fSelectSQL.Transaction:=nil;
end;
{______________________________________________________________________________}
function TFXInternalDataSet.PSInTransaction: Boolean;
begin
  if fSelectSQL.Transaction<>nil then
    Result:=fSelectSQL.Transaction.InTransaction else
    Result:=False;
end;
{______________________________________________________________________________}
function TFXInternalDataSet.PSGetQuoteChar: String;
Begin
  Result:='';
end;
{______________________________________________________________________________}
function TFXInternalDataSet.PSGetTableName:String;
begin
  Result:=fTableName
end;
{______________________________________________________________________________}
function TFXInternalDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams): Integer;
var i:Integer;
begin
  fSelectSQL.CheckDatabaseConnected;
  fSelectSQL.CheckInTransaction;

  if fExecSQL=nil then
    fExecSQL:=TFXCustomSQL.Create(Self);
  try fExecSQL.SQL.Text:=ASQL;
      fExecSQL.Database:=fSelectSQL.Database;
      fExecSQL.Transaction:=fSelectSQL.Transaction;
      fExecSQL.ParamCheck:=(AParams.Count>0);
      fExecSQL.Prepare;
      Assert(fExecSQL.Params.Count=AParams.Count);
      for i := 0 to Pred(AParams.Count) do
        fExecSQL.Params[i].Assign(AParams[i]);
      fExecSQL.ExecQuery;
      Case fExecSQL.QueryType of
        fxSQLInsertSelect, fxSQLUpdateSelect, fxSQLDeleteSelect,
        fxSQLSelectForUpdate,
        fxSQLSelectSelect:begin
          Result:=0;
          While not fExecSQL.Eof do Begin
            fExecSQL.Next;
            inc(Result)
          end end;
        fxSQLInsert:Begin
          Result:=fExecSQL.RowsInserted
          end;
        fxSQLUpdate:Begin
          Result:=fExecSQL.RowsUpdated
          end;
        fxSQLDelete:Begin
          Result:=fExecSQL.RowsDeleted
          end;
        else Begin
          Result:=fExecSQL.RowsAffected;
        end end;
  finally
      fExecSQL.CloseQuery;
  end;
end;
{______________________________________________________________________________}
function TFXInternalDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;Out cds:TFXInternalDataSet): Integer;
begin
  fSelectSQL.CheckDatabaseConnected;
  fSelectSQL.CheckInTransaction;

  cds:=TFXInternalDataSet.CreateResolver(Self);
  cds.Name:=Self.Name+'_ResultSet';
  cds.fSelectSQL.Database:=fSelectSQL.Database;
  cds.fSelectSQL.Transaction:=fSelectSQL.Transaction;
  cds.CommandText:=ASQL;
  cds.fParams.Assign(AParams);
  cds.fParamsPrepared:=True;
  cds.Open;
  Result:=cds.RecordCount;
end;
{______________________________________________________________________________}
function TFXInternalDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;var ResultSet: TDataSet): Integer;
var cds:TFXInternalDataSet;
begin
  Result:=PSExecuteStatement(ASQL,AParams,cds);
  ResultSet:=cds;
end;
{______________________________________________________________________________}
function TFXInternalDataSet.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
var fbe:EFXFirebirdError;
    PrevErr:Integer;
begin
  if Prev <> nil then
    PrevErr := Prev.ErrorCode else
    PrevErr := 0;
  if E is EFXFirebirdError then Begin
    fbe:=EFXFirebirdError(E);
    Result := EUpdateError.Create(fbe.Message, fbe.Context, fbe.ErrorCode, PrevErr, fbe)
  end else
    Result := inherited PSGetUpdateException(E, Prev);
end;
{______________________________________________________________________________}
Procedure TFXInternalDataSet.PSReset;
begin
  if Active then begin
    Close;
    Open;
    end
end;
{______________________________________________________________________________}
function TFXInternalDataSet.PSGetParams: TParams;
begin
  if not fParamsPrepared then
    Self.BuildParamsFromQuery;
  Result := Self.fParams;
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.PSSetParams(AParams: TParams);
begin
  Assert(AParams<>nil);
  Assert(AParams<>fParams);
  if not fParamsPrepared then
    Self.BuildParamsFromQuery;
  AParams.AssignValues(fParams);
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.PSExecute;
begin
  if fExecSQL=nil then
    fExecSQL:=TFXCustomSQL.Create(Self);
  fExecSQL.SQL.Text:=Self.CommandText;
  fExecSQL.Database:=fSelectSQL.Database;
  fExecSQL.Transaction:=fSelectSQL.Transaction;
  try fExecSQL.Prepare;
      Case fExecSQL.QueryType of
        fxSQLInsertSelect, fxSQLUpdateSelect, fxSQLDeleteSelect,
        fxSQLSelectForUpdate,
        fxSQLSelectSelect:begin
          FXRaiseClientError(fClientDataSet,fxceIsASelectStatement);
          end;
        else Begin
          fExecSQL.ExecQuery;
        end end;
  finally
      fExecSQL.CloseQuery;
  end;
end;
{______________________________________________________________________________}
procedure TFXInternalDataSet.PSSetCommandText(const CommandText: String);
begin
  fCommandText:=Trim(CommandText);
end;
{______________________________________________________________________________}
function TFXInternalDataSet.PSGetDefaultOrder: TIndexDef;
begin
  if FKeyFields='' then Begin
    Result := inherited PSGetDefaultOrder
  end else begin
    // detail table default order
    Result := TIndexDef.Create(nil);
    Result.Options := [ixUnique];      // keyfield is unique
    Result.Name := StringReplace(FKeyFields, ';', '_', [rfReplaceAll]);
    Result.Fields := FKeyFields;
  end;
end;

end.

