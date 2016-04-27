unit mFX.SQL;

interface

{$I mFX.Inc}
{$A8}
{$R-}
{$M-}

uses System.Types, System.Classes, System.SysUtils, Data.DB,
  {$IFDEF mFXTRACE}mFX.Logger,{$ENDIF mFXTRACE}
  mFX.Classes, mFX.Header, mFX.Intf, mFX.Base,
  mFX.SQLParamsParser;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  TFXSQL = class;
  TFXCustomSQL = class;
  TFXCustomSQLDA = class;
  TFXSQLDAFields = class;
  TFXSQLDAParams = class;

  /// <summary>Params/Fiels Data </summary>
  TFXSQLVAR = class(TObject)
  private
    fSQL       : TFXCustomSQL;
    fParent    : TFXCustomSQLDA;
    fIndex     : Integer;
    fDuplicated: Boolean;
    fModified  : Boolean;
    fName      : String;
    // Point to the PXSQLVAR in the owner object
    FXSQLVAR   : PXSQLVAR;
    // length of data area *
    fMaxChars  : Integer;
    fMaxBytes  : Integer;
    fDSQLType  : Integer;
    /// <summary>GetAlias</summary>
    function GetAlias: String;
    /// <summary>GetRelation</summary>
    function GetRelation: String;
    /// <summary>GetSQLType</summary>
    function GetSQLType: Integer;inline;
    /// <summary>GetDataType</summary>
    function GetDataType: TFieldType;
    /// <summary>GetSQLScal</summary>
    function GetSQLScale:SmallInt;inline;
    /// <summary>GetSQLBlobType</summary>
    function GetSQLBlobType: Integer;inline;
    /// <summary>GetDatabase</summary>
    function GetDatabase: TFXCustomDatabase;inline;
    /// <summary>GetTransaction</summary>
    function GetTransaction: TFXCustomTransaction;inline;

    /// <summary>GetIsNull</summary>
    function GetIsNull: Boolean;inline;
    /// <summary>SetIsNul</summary>
    procedure SetIsNull(Const Value: Boolean);
    /// <summary>GetIsNullable</summary>
    function GetIsNullable: Boolean;inline;

    /// <summary>GetAsCurrency</summary>
    function GetAsCurrency: Currency;
    /// <summary>SetAsCurrenc</summary>
    procedure SetAsCurrency(Const Value: Currency);
    /// <summary>GetAsCardinal</summary>
    function GetAsCardinal: Cardinal;
    /// <summary>SetAsCardina</summary>
    procedure SetAsCardinal(Const Value: Cardinal);
    /// <summary>GetAsDate</summary>
    function GetAsDate: TDateTime;
    /// <summary>SetAsDat</summary>
    procedure SetAsDate(Const Value: TDateTime);
    /// <summary>GetAsTime</summary>
    function GetAsTime: TDateTime;
    /// <summary>SetAsTim</summary>
    procedure SetAsTime(Const Value: TDateTime);
    /// <summary>GetAsDateTime</summary>
    function GetAsDateTime: TDateTime;
    /// <summary>GetAsTimeStamp</summary>
    function GetAsTimeStamp: TTimeStamp;
    /// <summary>SetAsDateTim</summary>
    procedure SetAsDateTime(Const Value: TDateTime);
    /// <summary>SetAsTimeStam</summary>
    procedure SetAsTimeStamp(Const Value: TTimeStamp);
    /// <summary>GetAsInt16</summary>
    function GetAsInt16: Smallint;
    /// <summary>SetAsInt1</summary>
    procedure SetAsInt16(Const Value: Smallint);
    /// <summary>GetAsInt32</summary>
    function GetAsInt32: Integer;
    /// <summary>SetAsInt3</summary>
    procedure SetAsInt32(Const Value: Integer);
    /// <summary>GetAsInt64</summary>
    function GetAsInt64: Int64;
    /// <summary>SetAsInt6</summary>
    procedure SetAsInt64(Const Value: Int64);
    /// <summary>GetAsInt64</summary>
    function GetAsUInt64: UInt64;
    /// <summary>SetAsInt6</summary>
    procedure SetAsUInt64(Const Value: UInt64);
    /// <summary>GetAsSingle</summary>
    function GetAsSingle: Single;
    /// <summary>SetAsSingl</summary>
    procedure SetAsSingle(Const Value: Single);
    /// <summary>GetAsDouble</summary>
    function GetAsDouble: Double;
    /// <summary>SetAsDoubl</summary>
    procedure SetAsDouble(Const Value: Double);
    /// <summary>GetAsExtended</summary>
    function GetAsExtended: Extended;
    /// <summary>SetAsExtende</summary>
    procedure SetAsExtended(Const Value: Extended);
    /// <summary>GetAsQuad</summary>
    function GetAsQuad: TISC_QUAD;
    /// <summary>SetAsQua</summary>
    procedure SetAsQuad(Const Value: TISC_QUAD);
    /// <summary>GetAsStrin</summary>
    function GetAsString:String;
    /// <summary>SetAsStrin</summary>
    procedure SetAsString(Const Value: String);
    /// <summary>GetAsTrimString</summary>
    function GetAsTrimString: String;
    /// <summary>SetAsTrimStrin</summary>
    procedure SetAsTrimString(const Value: String);
    /// <summary>GetAsAnsiString</summary>
    function GetAsAnsiString: AnsiString;
    /// <summary>SetAsAnsiStrin</summary>
    procedure SetAsAnsiString(Const Value: AnsiString);
    /// <summary>GetAsAnsiTrimString</summary>
    function GetAsAnsiTrimString: AnsiString;
    /// <summary>SetAsAnsiTrimStrin</summary>
    procedure SetAsAnsiTrimString(Const Value: AnsiString);
    /// <summary>GetAsVariant</summary>
    function GetAsVariant: Variant;
    /// <summary>SetAsVarian</summary>
    procedure SetAsVariant(Const Value: Variant);
    /// <summary>GetAsXSQLVAR</summary>
    function GetAsXSQLVAR: PXSQLVAR;inline;
    /// <summary>SetAsXSQLVA</summary>
    procedure SetAsXSQLVAR(Const Value: PXSQLVAR);
    /// <summary>GetAsXSQLVAR</summary>
    function GetAsGUID: TGUID;
    /// <summary>GetAsXSQLVAR</summary>
    procedure SetAsAsGUID(const Value: TGUID);

    /// <summary>GetAsBool</summary>
    function GetAsBool: Boolean;
    /// <summary>GetAsBool</summary>
    function GetAsBoolDef(Const aDef:Boolean): Boolean;

    ///<summary>Clone Source Data</summary>
    procedure AssignSQLVAR(Const Source: PXSQLVAR);
    ///<summary>Assign Bytes to SQL VarChar</summary>
    procedure AssignBytes(Const Value: TGUID);overload;
    ///<summary>Assign Bytes to SQL VarChar</summary>
    procedure AssignBytes(Const Value: TBytes);overload;
    ///<summary>Assign AnsiString to SQL VarChar</summary>
    procedure AssignAnsiString(Const Value: AnsiString);
    ///<summary>Read Source.BlobData and Write to new BlobData, Return the New Blob.ISC_QUAD  </summary>
    function CloneBlob(Const Source: TFXSQLVAR;Out QUAD:TISC_QUAD):Boolean;

  public
    /// <summary>Creat</summary>
    constructor Create(Const aParent: TFXCustomSQLDA;Const aQuery: TFXCustomSQL);

    ///<summary>Clear</summary>
    procedure Clear;inline;
    ///<summary>Assign a TFXSQLVAR</summary>
    procedure Assign(Const Source: TFXSQLVAR);overload;
    ///<summary>Assign a TParam</summary>
    procedure Assign(Const Source: TParam);overload;
    ///<summary>AssignTo a TParam</summary>
    procedure AssignTo(Const Target: TParam);overload;
    ///<summary>AssignTo a TParam</summary>
    procedure AssignTo(Const Target: TField);overload;
    ///<summary>AssignTo a TParam</summary>
    class procedure AssignTo(Const aSource:TFXSQLVAR;Const Target: TField);overload;

    ///<summary>Load FromFile</summary>
    procedure LoadFromFile(const FileName: String);
    ///<summary>Load FromStream</summary>
    procedure LoadFromStream(Const Stream: TStream);
    ///<summary>Load ByteArray</summary>
    procedure LoadFromByteArray(Const Value: Variant);
    ///<summary>SaveTo File</summary>
    procedure SaveToFile(const FileName: String);
    ///<summary>SaveTo Stream</summary>
    procedure BlobToStream(Const Stream: TStream);

    /// <summary>functio</summary>
    procedure ForceMaxChars(Const Value: integer);
    /// <summary>functio</summary>
    Class function AdjustScale2Int16(Const Value: Int64; Const Scale: FXShort): SmallInt;
    /// <summary>functio</summary>
    Class function AdjustScale2Int32(Const Value: Int64; Const Scale: FXShort): Integer;
    /// <summary>functio</summary>
    Class function AdjustScale2Int64(Const Value: Int64; Const Scale: FXShort): Int64;
    /// <summary>functio</summary>
    Class function AdjustScale2Single(Const Value: Int64; Const Scale: FXShort): Single;
    /// <summary>functio</summary>
    Class function AdjustScale2Double(Const Value: Int64; Const Scale: FXShort): Double;
    /// <summary>functio</summary>
    Class function AdjustScale2Extended(Const Value: Int64; Const Scale: FXShort): Extended;
    /// <summary>functio</summary>
    Class function AdjustScale2Currency(Const Value: Int64; Const Scale: FXShort): Currency;

    property Name        : String               read fName;
    property Alias       : String               read GetAlias;
    property Relation    : String               read GetRelation;
    property DataType    : TFieldType           read GetDataType;
    property MaxChars    : Integer              read fMaxChars;
    property MaxBytes    : Integer              read fMaxBytes;
    property DSQLType    : Integer              read fDSQLType;
    property SQLType     : Integer              read GetSQLType;
    property SQLBlobType : Integer              read GetSQLBlobType;
    property SQLScale    : SmallInt             read GetSQLScale;
    property SQL         : TFXCustomSQL         read fSQL;
    property Index       : Integer              read fIndex;
    property Modified    : Boolean              read fModified;
    property Database    : TFXCustomDatabase    read GetDatabase;
    property Transaction : TFXCustomTransaction read GetTransaction;

    property Data            : PXSQLVAR             read FXSQLVAR            write FXSQLVAR;
    property Value           : Variant              read GetAsVariant        write SetAsVariant;
    property AsDate          : TDateTime            read GetAsDate           write SetAsDate;
    property AsTime          : TDateTime            read GetAsTime           write SetAsTime;
    property AsDateTime      : TDateTime            read GetAsDateTime       write SetAsDateTime;
    property AsTimeStamp     : TTimeStamp           read GetAsTimeStamp      write SetAsTimeStamp;
    property AsSingle        : Single               read GetAsSingle         write SetAsSingle;
    property AsDouble        : Double               read GetAsDouble         write SetAsDouble;
    property AsExtended      : Extended             read GetAsExtended       write SetAsExtended;
    property AsCurrency      : Currency             read GetAsCurrency       write SetAsCurrency;
    property AsInteger       : Integer              read GetAsInt32          write SetAsInt32;
    property AsInt16         : Smallint             read GetAsInt16          write SetAsInt16;
    property AsInt32         : Integer              read GetAsInt32          write SetAsInt32;
    property AsInt64         : Int64                read GetAsInt64          write SetAsInt64;
    property AsUInt64        : UInt64               read GetAsUInt64         write SetAsUInt64;
    property AsCardinal      : Cardinal             read GetAsCardinal       write SetAsCardinal;
    property AsQuad          : TISC_QUAD            read GetAsQuad           write SetAsQuad;
    property AsGUID          : TGUID                read GetAsGUID           write SetAsAsGUID;
    property AsBool          : Boolean              read GetAsBool;
    property AsBoolDef[Const aDef:Boolean]: Boolean read GetAsBoolDef;
    property AsString        : String               read GetAsString         write SetAsString;
    property AsTrimString    : String               read GetAsTrimString     write SetAsTrimString;
    property AsAnsiString    : AnsiString           read GetAsAnsiString     write SetAsAnsiString;
    property AsAnsiTrimString: AnsiString           read GetAsAnsiTrimString write SetAsAnsiTrimString;
    property AsVariant       : Variant              read GetAsVariant        write SetAsVariant;
    property AsXSQLVAR       : PXSQLVAR             read GetAsXSQLVAR        write SetAsXSQLVAR;
    property IsNull          : Boolean              read GetIsNull           write SetIsNull;
    property IsNullable      : Boolean              read GetIsNullable;
  end;

  /// <summary>Params/Fiels Dyn Array </summary>
  TFXSQLVARArray = Array of TFXSQLVAR;

  /// <summary>Parent Class for InputParams / OutputFields</summary>
  TFXCustomSQLDA = Class
  private
    fSQL         : TFXCustomSQL;
    FCount       : Integer;
    fNames       : TStrings;
    FSize        : Integer;
    fSQLDA       : PXSQLDA;
    /// Dyn Array of IBXQLVARs
    fSQLVARs     : TFXSQLVARArray;
    /// <summary>GetModified</summary>
    function GetModified: Boolean;
    /// <summary>GetNames</summary>
    function GetNames: String;inline;
    /// <summary>GetXSQLDA</summary>
    function GetXSQLDA: PXSQLDA;inline;
    /// <summary>GetById</summary>
    function GetByIdx(Const Idx: Integer): TFXSQLVAR;inline;
    /// <summary>GetByNam</summary>
    function GetByName(Const aIdx: String): TFXSQLVAR;
    /// <summary>IsDuplicat</summary>
    function IsDuplicate(Const aIdx: String): Boolean;
    /// <summary>GetDatabase</summary>
    function GetDatabase: TFXCustomDatabase;inline;
    /// <summary>GetTransaction</summary>
    function GetTransaction: TFXCustomTransaction;inline;
    /// <summary>Allocate SQLVARs/summary>
    procedure SetCount(Const Value: Integer);
  public
    /// <summary>constructor/summary>
    constructor Create(Const aQuery: TFXCustomSQL);
    /// <summary>destructor/summary>
    destructor Destroy; override;

    /// <summary>ByNam</summary>
    function ByName(Const aIdx: String): TFXSQLVAR;
    /// <summary>FindByNam</summary>
    function FindByName(Const aIdx: String): TFXSQLVAR;inline;

    property Database    : TFXCustomDatabase    read GetDatabase;
    property Transaction : TFXCustomTransaction read GetTransaction;
    property AsXSQLDA    : PXSQLDA              read GetXSQLDA;
    property Count       : Integer              read FCount write SetCount;
    property Modified    : Boolean              read GetModified;
    property Names       : String               read GetNames;

    property Vars[Const Idx: Integer]: TFXSQLVAR read GetByIdx; default;
  end;

  /// <summary>Parent Class for InputParams / OutputFields</summary>
  TFXSQLDAParams = class(TFXCustomSQLDA)
  private
    /// <summary>DSQL Initialize Params</summary>
    procedure DSQLPreprocess(Const aParams:TStrings);
    /// <summary>DSQL Finalize Params</summary>
    procedure DSQLDescribe;
  end;

  /// <summary>Parent Class for InputParams / OutputFields</summary>
  TFXSQLDAFields = class(TFXCustomSQLDA)
  private
    fRelationName: String;
    /// <summary>DSQL Initialize Params</summary>
    procedure DSQLPreprocess(Const aFields:TStrings);
    /// <summary>DSQL Finalize Params</summary>
    procedure DSQLDescribe;
  public
    property RelationName: String read fRelationName;
  end;

  /// <summary>Custom SQL Query</summary>
  TFXCustomSQL = class(TFXSQLBase)
  private
    { Has the query been prepared? }
    fPrepared          : Boolean;
    { Once prepared, this accesses the SQL Query }
    fSQLHandle         : TISC_STMT_HANDLE;
    { Select, update, delete, insert, create, alter, etc...}
    fQueryType         : TFXQueryType;
    { Is a cursor open? }
    fCursorOpen        : Boolean;
    { Any parameters to the query }
    fSQLParams         : TFXSQLDAParams;
    { The current record }
    fSQLFields         : TFXSQLDAFields;
    { At EOF? }
    fEOF               : Boolean;
    { How many records have been read so far? }
    fRecordCount       : Integer;
    { SQL Query (pre-processed for param labels) }
    fSQLParser         : TFXSQLParamsParser;
    { Cursor name...}
    FCursor            : AnsiString;
    { SQL Query (by user) }
    fSQLText           : TStrings;
    FProcessedSQL      : TStrings;
    { Call this when the SQL is changing }
    FOnSQLChanging     : TNotifyEvent;
    { Call this when the SQL is changed }
    FOnSQLChanged      : TNotifyEvent;
    { Parse SQL Query to extract INPUT PARAMS and OUTPUT FIELDS }
    FParamCheck        : Boolean;
    { Default Fields Count }
    fDefaultFieldsCount: Integer;

    ///<summary>RAZ</summary>
    procedure UnPrepare;inline;

    /// <summary>Set SQLText</summary>
    procedure SetSQLText(Const Value: TStrings);
    /// <summary>SQLText Changing</summary>
    procedure SQLChanging(Sender: TObject);
    /// <summary>SQLText Changed</summary>
    procedure SQLChanged(Sender: TObject);
    ///<summary>Set Prepared</summary>
    procedure SetPrepared(Const Value:Boolean);
    /// <summary>Prepare Input Params</summary>
    procedure PrepareInputParams;inline;
    /// <summary>Prepare Output Fields</summary>
    procedure PrepareOutputFields;inline;

    /// <summary>Get EOF</summary>
    function GetEOF: Boolean;
    /// <summary>Get BOF</summary>
    function GetBOF: Boolean;
    /// <summary>Get Output Fields</summary>
    function GetFields(const Idx: Integer): TFXSQLVAR;
    /// <summary>Get Output Fields</summary>
    function GetFieldIndex(Const FieldName: String): Integer;
    /// <summary>Get Plan</summary>
    function GetPlan:String;overload;
    /// <summary>Get RecordCount</summary>
    function GetRecordCount: Integer;
    /// <summary>Get RowsAffected</summary>
    function GetRowsAffected: Integer;
    /// <summary>Get Rows Inserted</summary>
    function GetRowsInserted: Integer;
    /// <summary>Get Rows Updated</summary>
    function GetRowsUpdated: Integer;
    /// <summary>Get Rows Deleted</summary>
    function GetRowsDeleted: Integer;
    /// <summary>Get SQL Input Params</summary>
    function GetSQLParams: TFXSQLDAParams;
    /// <summary>Get UniqueR elationName</summary>
    function GetUniqueRelationName: String;
    /// <summary>GetSchema:</summary>
    function GetSchema: TFXCustomSchema;inline;
    /// <summary>Get Single Line Query:</summary>
    function GetSingleLineQuery:String;inline;
    /// <summary>Get Single Line Query:</summary>
    procedure SetSingleLineQuery(Const Value:String);

    {$IFDEF mFXTrace}
    /// <summary>Trace</summary>
    procedure LogPrepare;inline;
   /// <summary>Trace</summary>
    procedure LogQuery(Const aQuery: string);inline;
    /// <summary>Trace</summary>
    procedure LogParams;inline;
    /// <summary>Trace</summary>
    procedure LogPlan;inline;
    /// <summary>Trace</summary>
    procedure LogRowsAffected;inline;
    {$ENDIF mFXTrace}

  public
    ///<summary>return the Connection Encoding Helper</summary>
    function Encoding : TEncoding;inline;
    ///<summary>Free Handle</summary>
    function EncodeQuery(Const Value:String):RawByteString;
    ///<summary>Free Handle</summary>
    procedure EncodeQueryEx(Const Value:String;Var Bytes: TBytes);
    ///<summary>Free Handle</summary>
    function DecodeQuery(Const Value:RawByteString):String;overload;
    ///<summary>Free Handle</summary>
    function DecodeQuery(Const Value:PAnsiChar;Const Len:SmallInt):String;overload;
    ///<summary>Free Handle</summary>
    procedure RecodeQuery(Const Value:String;Const Dest:PAnsiChar;Var Len:SmallInt);

  protected
    ///<summary>Free Handle</summary>
    function InternalClose:Boolean;override;
    ///<summary>Close will free the Statement</summary>
    function InternalCloseCursor:Boolean;
    ///<summary>fBase Event</summary>
    procedure BeforeTransactionEndEvent(Const aTR: TFXCustomTransaction;Const Action:TFXTransactionAction;Const Silent:Boolean);override;

  public
    ///<summary>constructor</summary>
    constructor Create(AOwner: TComponent); override;
    ///<summary>destructor</summary>
    destructor Destroy; override;

    ///<summary>raise error if open</summary>
    procedure CheckClosed;override;
    ///<summary>raise error if query is not closed.</summary>
    procedure CheckCursorClosed;inline;
    ///<summary>raise error if query is not open.</summary>
    procedure CheckCursorOpen;inline;
    ///<summary>raise error if Query Text is Empty.</summary>
    procedure CheckSQLQuery;inline;
    ///<summary>raise error if query is not open.</summary>
    procedure CheckSQLHandle;inline;

    /// <summary>Load From Resource</summary>
    procedure SQLLoadFromRes(Const aResName:String);
    /// <summary>Load From Resource</summary>
    procedure SQLReplaceString(const OldValue: string; const NewValue: string);overload;

    /// <summary>Prepare</summary>
    procedure Prepare;
    ///<summary>Execute the query</summary>
    procedure ExecQuery;
    ///<summary>Execute the query</summary>
    function TryExecQuery:Boolean;overload;
    ///<summary>Execute the query</summary>
    function TryExecQuery(Const aQuery:String):Boolean;overload;
    ///<summary>Execute the query</summary>
    function TryExecQuery(Const aQuery:String;Const DoRestart:Boolean):Boolean;overload;
    ///<summary>Execute the query</summary>
    procedure OpenCursor;
    ///<summary>return the current record data</summary>
    function Current: TFXSQLDAFields;inline;
    /// <summary>Go to the next record... </summary>
    function Next: TFXSQLDAFields;
    ///<summary>Close will free the Statement</summary>
    procedure CloseCursor;
    ///<summary>Free Handle</summary>
    function CloseQuery(Const RaiseOnError:Boolean=True):Boolean;

    /// <summary>ParamBy</summry>
    function ParamBy(Idx: String): TFXSQLVAR;
    /// <summary>ParamByName</summary>
    function ParamByName(Idx: String): TFXSQLVAR;

    ///<summary>FindField return nil if </summary>
    function FindField(Const FieldName: String): TFXSQLVAR;
    ///<summary>FindField raise execption if </summary>
    function FieldByName(Const FieldName: String): TFXSQLVAR;
    ///<summary>AssignTo a TParam</summary>
    class procedure AssignFieldTo(Const aSource:TFXSQLVAR;Const Target: TField);

    /// <summary>GetServeurDate</summary>
    function GetServeurDate:TDateTime;deprecated;
    /// <summary>GetServeurDateTime</summary>
    function GetServeurDateTime:TDateTime;deprecated;

    property SQLHandle         : TISC_STMT_HANDLE     read fSQLHandle;
    property SQL               : TStrings             read fSQLText            write SetSQLText;
    property SingleLineQuery   : String               read GetSingleLineQuery  write SetSingleLineQuery;
    property ParamCheck        : Boolean              read FParamCheck         write FParamCheck         default True;
    property DefaultFieldsCount: Integer              read fDefaultFieldsCount write fDefaultFieldsCount default 10;
    property Params            : TFXSQLDAParams       read GetSQLParams;
    property Prepared          : Boolean              read fPrepared           write SetPrepared;
    property QueryType         : TFXQueryType         read fQueryType;
    property CursorOpen        : Boolean              read fCursorOpen;
    property Eof               : Boolean              read GetEOF;
    property Bof               : Boolean              read GetBOF;
    property Plan              : String               read GetPlan;
    property RecordCount       : Integer              read GetRecordCount;
    property RowsAffected      : Integer              read GetRowsAffected;
    property RowsUpdated       : Integer              read GetRowsUpdated;
    property RowsInserted      : Integer              read GetRowsInserted;
    property RowsDeleted       : Integer              read GetRowsDeleted;
    property UniqueRelationName: String               read GetUniqueRelationName;
    property Schema            : TFXCustomSchema      read GetSchema;

    property FieldIndex[Const FieldName: String ]: Integer   read GetFieldIndex;
    property Fields    [const Idx: Integer]: TFXSQLVAR read GetFields;default;

    property OnSQLChanging     : TNotifyEvent         read FOnSQLChanging      write FOnSQLChanging;
    property OnSQLChanged      : TNotifyEvent         read FOnSQLChanged       write FOnSQLChanged;
  end;

  /// <summary>SQL Query</summary>
  TFXSQL = class(TFXCustomSQL)
  published
    property Database;
    property Transaction;
    property SQL;
    property ParamCheck;
    property DefaultFieldsCount;
    property OnSQLChanging;
    property OnSQLChanged;
  end;

implementation

uses System.Variants, System.Character, System.AnsiStrings,
  {$IFDEF MSWINDOWS}Winapi.Windows, {$ENDIF}
  mFX.ErrorCodes, mFX.Blob, mFX.Utils;

Type
  /// <summary>TMemoryStream Access </summary>
  ///  Trick to access Memory Pointer
  TMemoryStreamAccess = class(TCustomMemoryStream)
  public
    /// <summary>Writ</summary>
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TMemoryStreamAccess.Write(const Buffer; Count: Longint): Longint;
Begin
  result:=0
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXSQLVAR.Create(Const aParent: TFXCustomSQLDA;Const aQuery: TFXCustomSQL);
begin
  inherited Create;
  fParent := aParent;
  fSQL    := aQuery;
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetDatabase: TFXCustomDatabase;
Begin
  Result:=fSQL.Database
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetTransaction: TFXCustomTransaction;
Begin
  Result:=fSQL.Transaction
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAlias:String;
Var i:Integer;
Begin
  i:=FXSQLVAR^.aliasname_length;
  if i>0 then
    Result:=fSQL.DecodeQuery(FXSQLVAR^.aliasname, i) else
    Result:=Self.Name
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetRelation:String;
Var i:Integer;
Begin
  i:=FXSQLVAR^.relname_length;
  if i>0 then
    Result:=fSQL.DecodeQuery(FXSQLVAR^.relname, i) else
    Result:=''
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.ForceMaxChars(Const Value: integer);
Begin
  fMaxChars:=Value;
End;
{______________________________________________________________________________}
function TFXSQLVAR.GetSQLType: Integer;
begin
  result := FXSQLVAR^.sqltype and (not 1);
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetSQLBlobType: Integer;
Begin
  Assert(Self.SQLType=SQL_BLOB);
  result := FXSQLVAR^.sqlsubtype
End;
{______________________________________________________________________________}
function TFXSQLVAR.GetSQLScale: SmallInt;
begin
  result := FXSQLVAR^.sqlscale
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetIsNullable: Boolean;
begin
  result := (FXSQLVAR^.sqltype and 1 = 1);
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetIsNull: Boolean;
begin
  if (FXSQLVAR^.sqltype and 1 = 1) then Begin
    if FXSQLVAR^.sqlind<>nil then
      result := (FXSQLVAR^.sqlind^ = -1) else
      result :=  False
  end else
    result :=  False
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetIsNull(Const Value: Boolean);
var xvar: TFXSQLVAR;
    i: Integer;
begin
  // Assign
  fModified := True;
  if Value then begin
    FXSQLVAR^.sqltype := FXSQLVAR^.sqltype or 1;
    if FXSQLVAR^.sqlind=nil then
      FXAlloc(Pointer(FXSQLVAR^.sqlind), SizeOf(FXSQLVAR^.sqlind^),False);
    FXSQLVAR^.sqlind^ := -1;
  end else Begin
    if FXSQLVAR^.sqlind<>nil then
      FXSQLVAR^.sqlind^ := 0;
    end;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetDataType: TFieldType;
begin
  Case Self.SQLType of
    SQL_SHORT,SQL_LONG:
      if FXSQLVAR^.sqlscale = 0 then
        Result:=ftInteger else
      if FXSQLVAR^.sqlscale >= -4 then
        Result:=ftCurrency else
        Result:=ftExtended;
    SQL_BIGINT:
      if FXSQLVAR^.sqlscale = 0 then
        Result:=ftLargeint else
      if FXSQLVAR^.sqlscale >= -4 then
        Result:=ftCurrency else
        Result:=ftExtended;
    SQL_DOUBLE,SQL_FLOAT,SQL_D_FLOAT:
      Result:=ftExtended;
    SQL_DATE:
      Result:=ftDate;
    SQL_TIME :
      Result:=ftTime;
    SQL_TIMESTAMP:
      Result:=ftDateTime;
    else
      Result:=ftWideString;
    end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.Clear;
begin
  SetIsNull(True)
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.Assign(Const Source: TFXSQLVAR);
var QUAD:TISC_QUAD;
begin
  if (Source=nil)or(Source.IsNull) then begin
    SetIsNull(True)
  end else Begin
    Case Self.SQLType of
      SQL_ARRAY:Begin
        // arrays not supported
        FXRaiseClientError(Self,fxceNotSupported);
        end;
      SQL_BLOB:Begin
        Case Source.SQLType of
          SQL_ARRAY:Begin
            // arrays not supported
            FXRaiseClientError(Self,fxceNotSupported);
            end;
          SQL_BLOB:Begin
            // Blob 2 Blob
            if Self.CloneBlob(Source,QUAD) then
              AsQuad:=QUAD else
              Self.Clear;
            end;
          else Begin
            AsXSQLVAR := Source.AsXSQLVAR;
        end end end;
      SQL_TEXT,SQL_VARYING:Begin
        Case Source.SQLType of
          SQL_TEXT,SQL_VARYING:Begin
            if Self.fSQL.ClientCharSet<>Source.fSQL.ClientCharSet then Begin
              // Translitarate !!!
              AsTrimString := Source.AsTrimString;
            end else
              AsXSQLVAR := Source.AsXSQLVAR;
            end;
          else Begin
            AsXSQLVAR := Source.AsXSQLVAR;
        end end end;
      else Begin
        Case Source.SQLType of
          SQL_ARRAY:Begin
            // arrays not supported
            FXRaiseClientError(Self,fxceNotSupported);
            end;
          SQL_BLOB:Begin
            // Blob 2 string !!!
            FXRaiseClientError(Self,fxceNotSupported);
            end;
          else Begin
            AsXSQLVAR := Source.AsXSQLVAR;
    end end end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.Assign(Const Source: TParam);
Begin
  if (Source=nil)or(Source.IsNull) then Begin
    Self.IsNull:=True;
  End else Begin
    case Source.DataType of
      ftWideMemo, ftWideString, ftFixedWideChar:
        Self.AsString:=Source.AsString;
      ftFmtMemo, ftMemo, ftString, ftFixedChar:
        Self.AsAnsiString:=Source.AsAnsiString;
      ftBoolean, ftSmallint, ftWord:
        Self.AsInt16:=Source.AsSmallInt;
      ftInteger:
        Self.AsInt32:=Source.AsInteger;
      ftLargeInt:
        Self.AsInt64:=Source.Value;
      ftFloat:
        Self.AsExtended:=Source.AsFloat;
      ftBCD,ftCurrency:
        Self.AsCurrency:=Source.AsCurrency;
      ftDate:
        Self.AsDate:=Source.AsDateTime;
      ftTime:
        Self.AsTime:=Source.AsDateTime;
      ftDateTime:
        Self.AsDateTime:=Source.AsDateTime;
      ftBlob:
        Self.Value:=Source.Value;
      else
        FXRaiseClientError(Self,fxceNotSupported)
    end end
End;
{______________________________________________________________________________}
procedure TFXSQLVAR.AssignTo(Const Target: TParam);
Begin
  if Target<>nil then Begin
    if Self.IsNull then Begin
      Target.Clear;
    End else Begin
      Case Self.SQLType of
        SQL_TEXT, SQL_VARYING:
          Target.AsString:=Self.AsString;
        SQL_SHORT,SQL_LONG:
          if FXSQLVAR^.sqlscale = 0 then
            Target.AsInteger:=Self.AsInteger else
          if FXSQLVAR^.sqlscale >= -4 then
            Target.AsCurrency:=Self.AsCurrency else
            Target.AsFloat:=Self.AsExtended;
        SQL_BIGINT:
          if FXSQLVAR^.sqlscale = 0 then
            Target.Value:=Self.AsInt64 else
          if FXSQLVAR^.sqlscale >= -4 then
            Target.AsCurrency:=Self.AsCurrency else
            Target.AsFloat:=Self.AsExtended;
        SQL_DOUBLE,SQL_FLOAT,SQL_D_FLOAT:
          Target.AsFloat:=Self.AsExtended;
        SQL_DATE:
          Target.AsDate:=Self.AsDate;
        SQL_TIME :
          Target.AsTime:=Self.AsTime;
        SQL_TIMESTAMP:
          Target.AsDateTime:=Self.AsDateTime;
        else
          Target.Value:=Self.Value
    end end end
End;
{______________________________________________________________________________}
procedure TFXSQLVAR.AssignTo(Const Target: TField);
Begin
  if Target<>nil then Begin
    if Self.IsNull then Begin
      Target.Clear;
    End else Begin
      Case Self.SQLType of
        SQL_TEXT, SQL_VARYING:
          Target.AsString:=Self.AsString;
        SQL_SHORT,SQL_LONG:
          if FXSQLVAR^.sqlscale = 0 then
            Target.AsInteger:=Self.AsInteger else
          if FXSQLVAR^.sqlscale >= -4 then
            Target.AsCurrency:=Self.AsCurrency else
            Target.AsFloat:=Self.AsExtended;
        SQL_BIGINT:
          if FXSQLVAR^.sqlscale = 0 then
            Target.Value:=Self.AsInt64 else
          if FXSQLVAR^.sqlscale >= -4 then
            Target.AsCurrency:=Self.AsCurrency else
            Target.AsFloat:=Self.AsExtended;
        SQL_DOUBLE,SQL_FLOAT,SQL_D_FLOAT:
          Target.AsFloat:=Self.AsExtended;
        SQL_DATE:
          Target.AsDateTime:=Self.AsDate;
        SQL_TIME :
          Target.AsDateTime:=Self.AsTime;
        SQL_TIMESTAMP:
          Target.AsDateTime:=Self.AsDateTime;
        else
          Target.Value:=Self.Value
    end end end
End;
{______________________________________________________________________________}
class procedure TFXSQLVAR.AssignTo(Const aSource:TFXSQLVAR;Const Target: TField);
Begin
  if Target<>nil then Begin
    if (aSource=nil)or(aSource.IsNull) then Begin
      Target.Clear;
    End else Begin
      Case aSource.SQLType of
        SQL_TEXT, SQL_VARYING:
          Target.AsString:=aSource.AsString;
        SQL_SHORT,SQL_LONG:
          if aSource.FXSQLVAR^.sqlscale = 0 then
            Target.AsInteger:=aSource.AsInteger else
          if aSource.FXSQLVAR^.sqlscale >= -4 then
            Target.AsCurrency:=aSource.AsCurrency else
            Target.AsFloat:=aSource.AsExtended;
        SQL_BIGINT:
          if aSource.FXSQLVAR^.sqlscale = 0 then
            Target.Value:=aSource.AsInt64 else
          if aSource.FXSQLVAR^.sqlscale >= -4 then
            Target.AsCurrency:=aSource.AsCurrency else
            Target.AsFloat:=aSource.AsExtended;
        SQL_DOUBLE,SQL_FLOAT,SQL_D_FLOAT:
          Target.AsFloat:=aSource.AsExtended;
        SQL_DATE:
          Target.AsDateTime:=aSource.AsDate;
        SQL_TIME :
          Target.AsDateTime:=aSource.AsTime;
        SQL_TIMESTAMP:
          Target.AsDateTime:=aSource.AsDateTime;
        else
          Target.Value:=aSource.Value
    end end end
End;
{______________________________________________________________________________}
class procedure TFXCustomSQL.AssignFieldTo(Const aSource:TFXSQLVAR;Const Target: TField);
Begin
  if Target<>nil then Begin
    if (aSource=nil)or(aSource.IsNull) then Begin
      Target.Clear;
    End else Begin
      Case aSource.SQLType of
        SQL_TEXT, SQL_VARYING:
          Target.AsString:=aSource.AsString;
        SQL_SHORT,SQL_LONG:
          if aSource.FXSQLVAR^.sqlscale = 0 then
            Target.AsInteger:=aSource.AsInteger else
          if aSource.FXSQLVAR^.sqlscale >= -4 then
            Target.AsCurrency:=aSource.AsCurrency else
            Target.AsFloat:=aSource.AsExtended;
        SQL_BIGINT:
          if aSource.FXSQLVAR^.sqlscale = 0 then
            Target.Value:=aSource.AsInt64 else
          if aSource.FXSQLVAR^.sqlscale >= -4 then
            Target.AsCurrency:=aSource.AsCurrency else
            Target.AsFloat:=aSource.AsExtended;
        SQL_DOUBLE,SQL_FLOAT,SQL_D_FLOAT:
          Target.AsFloat:=aSource.AsExtended;
        SQL_DATE:
          Target.AsDateTime:=aSource.AsDate;
        SQL_TIME :
          Target.AsDateTime:=aSource.AsTime;
        SQL_TIMESTAMP:
          Target.AsDateTime:=aSource.AsDateTime;
        else
          Target.Value:=aSource.Value
    end end end
End;
{______________________________________________________________________________}
procedure TFXSQLVAR.LoadFromFile(const FileName: String);
var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try LoadFromStream(fs);
  finally
      fs.Free
  end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.LoadFromByteArray(Const Value: Variant);
var bs:TFXBlobStream;
    vt:TVarType;
begin
  vt:=System.Variants.VarType(Value);
  if (vt and varArray)<>varArray then
    FXRaiseClientError(Self,fxceNotPermitted);
  if (vt and VarTypeMask)<>varByte Then
    FXRaiseClientError(Self,fxceNotPermitted);
  if VarArrayDimCount(Value)<>1 Then
    FXRaiseClientError(Self,fxceNotPermitted);

  bs:=TFXBlobStream.Create(fSQL);
  try bs.Mode := bmWrite;
      bs.LoadFromByteArray(Value);
      bs.Finalize;
      Self.AsQuad := bs.AsQuad
  finally
      bs.Free
  end;
End;
{______________________________________________________________________________}
procedure TFXSQLVAR.LoadFromStream(Const Stream: TStream);
var bs: TFXBlobStream;
begin
  if (Stream=nil)or(Stream.Size=0) then begin
    Self.IsNull:=True
  end else Begin
    bs:=TFXBlobStream.Create(fSQL);
    try bs.Mode := bmWrite;
        Assert(bs.Database = fSQL.Database);
        Assert(bs.Transaction = fSQL.Transaction);
        Stream.Seek(0, soFromBeginning);
        bs.LoadFromStream(Stream);
        bs.Finalize;
        Self.AsQuad := bs.AsQuad
    finally
        bs.Free
    end end;
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SaveToFile(const FileName: String);
var fs: TFileStream;
    bs:TFXBlobStream;
begin
  if not Self.IsNull then Begin
    case Self.SQLType of
      SQL_BLOB:Begin
        fs:=nil;
        bs:=TFXBlobStream.Create(fSQL);
        try bs.AsQuad := PISC_QUAD(FXSQLVAR^.sqldata)^;
            fs:=TFileStream.Create(FileName, fmCreate or fmShareExclusive);
            bs.SaveToStream(fs);
        finally
            bs.Free;
            fs.Free;
        end end;
      else Begin
        FXRaiseClientError(Self,fxceInvalidDataConversion);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.BlobToStream(Const Stream: TStream);
var bs:TFXBlobStream;
    QUAD:TISC_QUAD;
begin
  if not Self.IsNull then Begin
    case Self.SQLType of
      SQL_BLOB:Begin
        bs:=TFXBlobStream.Create(fSQL);
        try QUAD:=PISC_QUAD(FXSQLVAR^.sqldata)^;
            bs.AsQuad := QUAD;
            bs.SaveToStream(Stream);
        finally
            bs.Free
        end end;
      else Begin
        FXRaiseClientError(Self,fxceInvalidDataConversion);
    end end end
end;
{______________________________________________________________________________}
Class function TFXSQLVAR.AdjustScale2Int16(Const Value: Int64; Const Scale: FXShort):SmallInt;
var Scaling:Int64;
    i:Integer;
begin
  Scaling := 1;
  Assert(Scale<0);
  for i := -1 downto Scale do
    Scaling := Scaling * 10;
  result := Value div Scaling;
end;
{______________________________________________________________________________}
Class function TFXSQLVAR.AdjustScale2Int32(Const Value: Int64; Const Scale: FXShort):Integer;
var Scaling:Int64;
    i:Integer;
begin
  Scaling := 1;
  Assert(Scale<0);
  for i := -1 downto Scale do
    Scaling := Scaling * 10;
  result := Value div Scaling;
end;
{______________________________________________________________________________}
Class function TFXSQLVAR.AdjustScale2Int64(Const Value: Int64; Const Scale: FXShort): Int64;
var Scaling:Int64;
    i:Integer;
begin
  Scaling := 1;
  Assert(Scale<0);
  for i := -1 downto Scale do
    Scaling := Scaling * 10;
  result := Value div Scaling;
end;
{______________________________________________________________________________}
Class function TFXSQLVAR.AdjustScale2Currency(Const Value: Int64;Const Scale: FXShort): Currency;
var Scaling:Int64;
    i:Integer;
begin
  Scaling := 1;
  Assert(Scale<0);
  for i := -1 downto Scale do
    Scaling := Scaling * 10;
  result := Value / Scaling;
end;
{______________________________________________________________________________}
Class function TFXSQLVAR.AdjustScale2Double(Const Value: Int64; Const Scale: FXShort): Double;
var Scaling:Int64;
    i:Integer;
begin
  Scaling := 1;
  Assert(Scale<0);
  for i := -1 downto Scale do
    Scaling := Scaling * 10;
  result := Value / Scaling;
end;
{______________________________________________________________________________}
Class function TFXSQLVAR.AdjustScale2Extended(Const Value: Int64; Const Scale: FXShort): Extended;
var Scaling:Int64;
    i:Integer;
begin
  Scaling := 1;
  Assert(Scale<0);
  for i := -1 downto Scale do
    Scaling := Scaling * 10;
  result := Value / Scaling;
end;
{______________________________________________________________________________}
Class function TFXSQLVAR.AdjustScale2Single(Const Value: Int64; Const Scale: FXShort): Single;
var Scaling:Int64;
    i:Integer;
begin
  Scaling := 1;
  Assert(Scale<0);
  for i := -1 downto Scale do
    Scaling := Scaling * 10;
  result := Value / Scaling;
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.AssignSQLVAR(Const Source: PXSQLVAR);
begin
  FXSQLVAR^.sqltype   := Source^.sqltype;
  FXSQLVAR^.sqlscale  := Source^.sqlscale;
  FXSQLVAR^.sqlsubtype:= Source^.sqlsubtype;

  FXSQLVAR^.sqllen    := Source^.sqllen;
  FXAlloc(FXSQLVAR^.sqldata,FXSQLVAR^.sqllen,False);
  Move(Source^.sqldata^, FXSQLVAR^.sqldata^, Source^.sqllen);

  if Source^.sqlind<>nil then Begin
    if FXSQLVAR^.sqlind=nil then
      FXAlloc(Pointer(FXSQLVAR^.sqlind), SizeOf(FXSQLVAR^.sqlind^),False);
    FXSQLVAR^.sqlind^ := Source^.sqlind^;
  end else
    FXFree(Pointer(Source^.sqlind));

  // Update State
  fModified := True;
End;
{______________________________________________________________________________}
procedure TFXSQLVAR.AssignBytes(Const Value: TGUID);
Var xvar:TFXSQLVAR;
    a:PByteArray;
    i:Integer;
Begin
  if (fMaxBytes>0)and(SizeOf(Value)>fMaxBytes) then
    FXRaiseClientErrorFmt(Self,fxceStringTooLarge,[SizeOf(Value),fMaxBytes]);

  // Set sqltype
  FXSQLVAR^.sqltype := SQL_TEXT or (FXSQLVAR^.sqltype and 1);
  FXSQLVAR^.sqllen  := SizeOf(Value);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  a := PByteArray(@Value);
  FXAlloc(FXSQLVAR^.sqldata,SizeOf(Value),False);
  Move(a[0],FXSQLVAR^.sqldata^,SizeOf(Value));

  // Update State
  fModified := True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end;
End;
{______________________________________________________________________________}
procedure TFXSQLVAR.AssignBytes(Const Value: TBytes);
Var i,NbBytes:Integer;
    xvar:TFXSQLVAR;
Begin
  NbBytes:=Length(Value);
  if (fMaxBytes>0)and(NbBytes>fMaxBytes) then
    FXRaiseClientErrorFmt(Self,fxceStringTooLarge, [NbBytes,fMaxBytes]);

  // Set sqltype
  FXSQLVAR^.sqltype := SQL_TEXT or (FXSQLVAR^.sqltype and 1);
  FXSQLVAR^.sqllen  := NbBytes;

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  if NbBytes>0 then Begin
    FXAlloc(FXSQLVAR^.sqldata,NbBytes,False);
    Move(Value[0],FXSQLVAR^.sqldata^,NbBytes);
    end;

  // Update State
  fModified := True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end;
End;
{______________________________________________________________________________}
procedure TFXSQLVAR.AssignAnsiString(Const Value: AnsiString);
Var i,NbBytes:Integer;
    xvar:TFXSQLVAR;
Begin
  NbBytes:=Length(Value);
  if (fMaxBytes>0)and(NbBytes>fMaxBytes) then
    FXRaiseClientErrorFmt(Self,fxceStringTooLarge, [NbBytes,fMaxBytes]);

  // Set sqltype
  FXSQLVAR^.sqltype := SQL_TEXT or (FXSQLVAR^.sqltype and 1);
  FXSQLVAR^.sqllen  := NbBytes;

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  if NbBytes>0 then Begin
    FXAlloc(FXSQLVAR^.sqldata,NbBytes,False);
    Move(Value[1],FXSQLVAR^.sqldata^,NbBytes);
    end;

  // Update State
  fModified := True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end;
End;
{______________________________________________________________________________}
function TFXSQLVAR.CloneBlob(Const Source: TFXSQLVAR;Out QUAD:TISC_QUAD):Boolean;
Var Bytes,SourceBytes:TBytes;
    iSegs, iMaxSeg, iSize: FXLong;
    bhandle: TISC_BLOB_HANDLE;
    cl:IFXClientLib;
    iBlobType: FXShort;
    sQuad:TISC_QUAD;
    szBuff:PFXByte;
Begin
  szBuff:=nil;
  Assert(Self.SQLType=SQL_BLOB);
  Assert(Source.SQLType=SQL_BLOB);
  Assert(fParent is TFXSQLDAParams);
  Assert(Source.fParent is TFXSQLDAFields);
  Assert(Self.fSQL.ClientLibrary=Source.fSQL.ClientLibrary);
  try // Read Blob
      bhandle:=nil;iSize:=0;
      sQuad:=Source.AsQuad;
      cl:=Source.fSQL.ClientLibrary;
      cl.Check_open_blob2(Source.fSQL.DBHandle,Source.fSQL.TRHandle, @bhandle, @sQuad);
      try cl.Check_BlobInfo(@bhandle, iSegs, iMaxSeg, iSize, iBlobType);
          if iSize>0 Then Begin
            FXAlloc(szBuff,iSize,False);
            cl.Check_ReadBlob(@bhandle,szBuff,iSegs,iMaxSeg,iSize);
            end;
      finally
          cl.Check_close_blob(@bhandle);
      end;
      // Write Blob
      if iSize>0 Then Begin
        cl:=fSQL.ClientLibrary;
        cl.Check_create_blob2(fSQL.DBHandle,fSQL.TRHandle, @bhandle, @QUAD);
        try // Translitarate !!!
            if (Self.SQLBlobType=isc_blob_text)and(Source.SQLBlobType=isc_blob_text)and(fSQL.Database.ClientCharSet<>Source.Database.ClientCharSet) then Begin
              SetLength(SourceBytes,iSize);
              move(szBuff^, SourceBytes[0], iSize);
              Bytes:=TEncoding.Convert(Source.fSQL.Encoding,fSQL.Encoding,SourceBytes);
              cl.WriteBlob(@bhandle, @Bytes[0], Length(Bytes));
              SetLength(SourceBytes,0);
              SetLength(Bytes,0);
            end else
              cl.WriteBlob(@bhandle, szBuff, iSize);
            Result:=True;
        finally
            cl.Check_close_blob(@bhandle);
        end
      End else
        Result:=False;
  finally
      FXFree(szBuff);
  end;
End;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsGUID: TGUID;
Var str_len:FXShort;
    sz:PAnsiChar;
Begin
  if Not Self.IsNull then Begin
    case Self.SQLType of
      SQL_TEXT:Begin
        str_len := FXSQLVar^.sqllen;
        if str_len=Sizeof(TGUID) Then Begin
          result:=PGUID(FXSQLVAR^.sqldata)^
        End else
          FXRaiseClientError(Self,fxceInvalidDataConversion);
        end;
      SQL_VARYING:Begin
        str_len:=mFX.Header.vax_Integer(FXSQLVAR^.sqldata, 2);
        sz:=PAnsiChar(FXSQLVAR^.sqldata);Inc(sz,2);
        if str_len=Sizeof(TGUID) Then Begin
          result:=PGUID(sz)^
        End else
          FXRaiseClientError(Self,fxceInvalidDataConversion);
        end;
      else Begin
        FXRaiseClientError(Self,fxceInvalidDataConversion);
      end end
  end else
    result:=TGUID.Empty;
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsAsGUID(const Value: TGUID);
begin
  if Value<>TGUID.Empty then Begin
    Self.AssignBytes(Value);
  End else
    Self.SetIsNull(True);
end;
{______________________________________________________________________________}
Function TFXSQLVAR.GetAsBool:Boolean;
Var s:String;
Begin
  if Self.IsNull then Begin
    Result:=False;
  end else Begin
    s:=UpperCase(Self.AsTrimString);
    if s<>EmptyStr then Begin
      Result:=(CharInSet(s[1],['T','Y','O','J','1']))
    end else
      Result:= False
    end;
End;
{______________________________________________________________________________}
Function TFXSQLVAR.GetAsBoolDef(Const aDef:Boolean): Boolean;
Var s:String;
Begin
  if Self.IsNull then Begin
    Result:=aDef;
  end else Begin
    s:=UpperCase(Self.AsTrimString);
    if s<>EmptyStr then Begin
      Result:=(CharInSet(s[1],['T','Y','O','J','1']))
    end else
      Result:=aDef;
    end;
End;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsXSQLVAR: PXSQLVAR;
begin
  result := FXSQLVAR;
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsXSQLVAR(Const Value: PXSQLVAR);
var xvar: TFXSQLVAR;
    i: Integer;
begin
  // Assign
  Self.AssignSQLVAR(Value);

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsInt16: SmallInt;
Var s:String;
begin
  result :=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_SHORT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXShort));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXShort(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Int16(Int64(PFXShort(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_LONG:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXLong));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXLong(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Int16(Int64(PFXLong(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_BIGINT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXInt64));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXInt64(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Int16(PFXInt64(FXSQLVAR^.sqldata)^,FXSQLVAR^.sqlscale);
        end;
      SQL_FLOAT:Begin
        Result := Round(PFXFloat(FXSQLVAR^.sqldata)^);
        end;
      SQL_DOUBLE,SQL_D_FLOAT:Begin
        Result := Round(PFXDouble(FXSQLVAR^.sqldata)^);
        end;
      else Begin
        s:=Self.AsString;
        if s<>'' then
          Result:=StrToInt(s);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsInt16(Const Value: SmallInt);
var xvar: TFXSQLVAR;
    i: Integer;
begin
  // Set sqltype
  Self.FXSQLVAR^.sqltype := SQL_SHORT or (Self.FXSQLVAR^.sqltype and 1);
  Self.FXSQLVAR^.sqlscale:= 0;
  Self.FXSQLVAR^.sqllen  := SizeOf(FXShort);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(FXShort),False);
  PFXShort(Self.FXSQLVAR^.sqldata)^:=Value;

  // Update State
  fModified:=True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsInt32:Integer;
Var s:String;
begin
  result :=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_SHORT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXShort));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXShort(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Int32(Int64(PFXShort(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_LONG:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXLong));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXLong(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Int32(Int64(PFXLong(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_BIGINT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXInt64));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXInt64(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Int32(PFXInt64(FXSQLVAR^.sqldata)^,FXSQLVAR^.sqlscale);
        end;
      SQL_FLOAT:Begin
        Result := Round(PFXFloat(FXSQLVAR^.sqldata)^);
        end;
      SQL_DOUBLE,SQL_D_FLOAT:Begin
        Result := Round(PFXDouble(FXSQLVAR^.sqldata)^);
        end;
      else Begin
        s:=Self.AsString;
        if s<>'' then
          Result:=StrToInt(s);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsInt32(Const Value: Integer);
var xvar: TFXSQLVAR;
    i: Integer;
begin
  // Set sqltype
  Self.FXSQLVAR^.sqltype := SQL_LONG or (Self.FXSQLVAR^.sqltype and 1);
  Self.FXSQLVAR^.sqlscale:= 0;
  Self.FXSQLVAR^.sqllen  := SizeOf(FXLong);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(FXLong),False);
  PFXLong(Self.FXSQLVAR^.sqldata)^:=Value;

  // Update State
  fModified:=True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsInt64: Int64;
Var s:String;
begin
  result :=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_SHORT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXShort));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXShort(FXSQLVAR^.sqldata)^ else
          result:= AdjustScale2Int64(Int64(PFXShort(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_LONG:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXLong));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXLong(FXSQLVAR^.sqldata)^ else
          result:= AdjustScale2Int64(Int64(PFXLong(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_BIGINT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXInt64));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXInt64(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Int64(PFXInt64(FXSQLVAR^.sqldata)^,FXSQLVAR^.sqlscale);
        end;
      SQL_FLOAT:Begin
        result := Round(PFXFloat(FXSQLVAR^.sqldata)^);
        end;
      SQL_DOUBLE,SQL_D_FLOAT:Begin
        result := Round(PFXDouble(FXSQLVAR^.sqldata)^);
        end;
      else Begin
        s:=Self.AsString;
        if s<>'' then
          result:=StrToInt64(s);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsInt64(Const Value: Int64);
var xvar: TFXSQLVAR;
    i: Integer;
begin
  // Set sqltype
  Self.FXSQLVAR^.sqltype := SQL_BIGINT or (Self.FXSQLVAR^.sqltype and 1);
  Self.FXSQLVAR^.sqlscale:= 0;
  Self.FXSQLVAR^.sqllen  := SizeOf(FXInt64);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(FXInt64),False);
  PFXInt64(Self.FXSQLVAR^.sqldata)^:=Value;

  // Update State
  fModified:=True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsUInt64: UInt64;
Var s:String;
begin
  result :=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_SHORT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXShort));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXShort(FXSQLVAR^.sqldata)^ else
          result:= AdjustScale2Int64(Int64(PFXShort(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_LONG:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXLong));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXLong(FXSQLVAR^.sqldata)^ else
          result:= AdjustScale2Int64(Int64(PFXLong(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_BIGINT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXInt64));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXInt64(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Int64(PFXInt64(FXSQLVAR^.sqldata)^,FXSQLVAR^.sqlscale);
        end;
      SQL_FLOAT:Begin
        result := Round(PFXFloat(FXSQLVAR^.sqldata)^);
        end;
      SQL_DOUBLE,SQL_D_FLOAT:Begin
        result := Round(PFXDouble(FXSQLVAR^.sqldata)^);
        end;
      else Begin
        s:=Self.AsString;
        if s<>'' then
          result:=StrToInt64(s);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsUInt64(Const Value: UInt64);
var xvar: TFXSQLVAR;
    i: Integer;
begin
  Assert(Value<=High(Int64));

  // Set sqltype
  Self.FXSQLVAR^.sqltype := SQL_BIGINT or (Self.FXSQLVAR^.sqltype and 1);
  Self.FXSQLVAR^.sqlscale:= 0;
  Self.FXSQLVAR^.sqllen  := SizeOf(FXInt64);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(FXInt64),False);
  PFXInt64(Self.FXSQLVAR^.sqldata)^:=Value;

  // Update State
  fModified:=True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsCardinal:Cardinal;
Var s:String;
    i64:Int64;
begin
  result :=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_SHORT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXShort));
        if FXSQLVAR^.sqlscale=0 then
          i64 := PFXShort(FXSQLVAR^.sqldata)^ else
          i64 := AdjustScale2Int64(Int64(PFXShort(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        Result:=i64
        end;
      SQL_LONG:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXLong));
        if FXSQLVAR^.sqlscale=0 then
          i64 := PFXLong(FXSQLVAR^.sqldata)^ else
          i64 := AdjustScale2Int64(Int64(PFXLong(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        Result:=i64
        end;
      SQL_BIGINT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXInt64));
        if FXSQLVAR^.sqlscale=0 then
          i64 := PFXInt64(FXSQLVAR^.sqldata)^ else
          i64 := AdjustScale2Int64(PFXInt64(FXSQLVAR^.sqldata)^,FXSQLVAR^.sqlscale);
        Result:=i64
        end;
      SQL_FLOAT:Begin
        i64 := Round(PFXFloat(FXSQLVAR^.sqldata)^);
        Result:=i64
        end;
      SQL_DOUBLE,SQL_D_FLOAT:Begin
        i64 := Round(PFXDouble(FXSQLVAR^.sqldata)^);
        Result:=i64
        end;
      else Begin
        s:=Self.AsString;
        if s<>'' then Begin
          i64:=StrToInt64(s);
          Result:=i64
    end end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsCardinal(Const Value:Cardinal);
var b:Cardinal;
begin
  b:=High(Integer);
  if Value>b then Begin
    SetAsInt64(Value)
  end else Begin
    b:=High(SmallInt);
    if Value>b then
      SetAsInt32(Value) else
      SetAsInt16(Value)
    end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsSingle: Single;
Var s:String;
begin
  result :=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_SHORT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXShort));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXShort(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Single(Int64(PFXShort(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_LONG:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXLong));
        if FXSQLVAR^.sqlscale=0 then
          Result := PFXLong(FXSQLVAR^.sqldata)^ else
          Result := AdjustScale2Single(Int64(PFXLong(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_BIGINT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXInt64));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXInt64(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Single(PFXInt64(FXSQLVAR^.sqldata)^,FXSQLVAR^.sqlscale);
        end;
      SQL_FLOAT:Begin
        Result := PFXFloat(FXSQLVAR^.sqldata)^;
        end;
      SQL_DOUBLE,SQL_D_FLOAT:Begin
        Result := PFXDouble(FXSQLVAR^.sqldata)^;
        end;
      else Begin
        s:=Self.AsString;
        if s<>'' then
          Result:=StrToFloat(s);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsSingle(Const Value: Single);
var xvar: TFXSQLVAR;
    i: Integer;
begin
  // Set sqltype
  Self.FXSQLVAR^.sqltype := SQL_FLOAT or (Self.FXSQLVAR^.sqltype and 1);
  Self.FXSQLVAR^.sqlscale:= 0;
  Self.FXSQLVAR^.sqllen  := SizeOf(FXFloat);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(FXFloat),False);
  PFXFloat(Self.FXSQLVAR^.sqldata)^:=Value;

  // Update State
  fModified:=True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsDouble:Double;
Var s:String;
begin
  result :=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_SHORT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXShort));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXShort(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Double(Int64(PFXShort(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_LONG:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXLong));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXLong(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Double(Int64(PFXLong(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_BIGINT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXInt64));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXInt64(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Double(PFXInt64(FXSQLVAR^.sqldata)^,FXSQLVAR^.sqlscale);
        end;
      SQL_FLOAT:Begin
        Result := PFXFloat(FXSQLVAR^.sqldata)^;
        end;
      SQL_DOUBLE,SQL_D_FLOAT:Begin
        Result := PFXDouble(FXSQLVAR^.sqldata)^;
        end;
      else Begin
        s:=Self.AsString;
        if s<>'' then
          Result:=StrToFloat(s);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsDouble(Const Value: Double);
var xvar: TFXSQLVAR;
    i: Integer;
begin
  // Set sqltype
  Self.FXSQLVAR^.sqltype := SQL_DOUBLE or (Self.FXSQLVAR^.sqltype and 1);
  Self.FXSQLVAR^.sqlscale:= 0;
  Self.FXSQLVAR^.sqllen  := SizeOf(FXDouble);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(FXDouble),False);
  PFXDouble(Self.FXSQLVAR^.sqldata)^:=Value;

  // Update State
  fModified:=True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsExtended:Extended;
Var s:String;
begin
  result :=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_SHORT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXShort));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXShort(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Extended(Int64(PFXShort(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_LONG:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXLong));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXLong(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Extended(Int64(PFXLong(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_BIGINT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXInt64));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXInt64(FXSQLVAR^.sqldata)^ else
          Result:= AdjustScale2Extended(PFXInt64(FXSQLVAR^.sqldata)^,FXSQLVAR^.sqlscale);
        end;
      SQL_FLOAT:Begin
        Result := PFXFloat(FXSQLVAR^.sqldata)^;
        end;
      SQL_DOUBLE,SQL_D_FLOAT:Begin
        Result := PFXDouble(FXSQLVAR^.sqldata)^;
        end;
      else Begin
        s:=Self.AsString;
        if s<>'' then
          Result:=StrToFloat(s);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsExtended(Const Value: Extended);
var xvar: TFXSQLVAR;
    i: Integer;
begin
  // Set sqltype
  Self.FXSQLVAR^.sqltype := SQL_DOUBLE or (Self.FXSQLVAR^.sqltype and 1);
  Self.FXSQLVAR^.sqlscale:= 0;
  Self.FXSQLVAR^.sqllen  := SizeOf(FXDouble);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(FXDouble),False);
  PFXDouble(Self.FXSQLVAR^.sqldata)^:=Value;

  // Update State
  fModified:=True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsCurrency: Currency;
Var s:String;
begin
  result :=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_SHORT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXShort));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXShort(FXSQLVAR^.sqldata)^ else
          result:= AdjustScale2Currency(Int64(PFXShort(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_LONG:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXLong));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXLong(FXSQLVAR^.sqldata)^ else
          result:= AdjustScale2Currency(Int64(PFXLong(FXSQLVAR^.sqldata)^),FXSQLVAR^.sqlscale);
        end;
      SQL_BIGINT:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(FXInt64));
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXInt64(FXSQLVAR^.sqldata)^ else
          result:= AdjustScale2Currency(PFXInt64(FXSQLVAR^.sqldata)^,FXSQLVAR^.sqlscale);
        end;
      SQL_FLOAT:Begin
        result := PFXFloat(FXSQLVAR^.sqldata)^;
        end;
      SQL_DOUBLE,SQL_D_FLOAT:Begin
        result := PFXDouble(FXSQLVAR^.sqldata)^;
        end;
      else Begin
        s:=Self.AsString;
        if s<>'' then
          result:=StrToCurr(s);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsCurrency(Const Value: Currency);
var xvar:TFXSQLVAR;
    i:Integer;
begin
  // Set sqltype
  Self.FXSQLVAR^.sqltype := SQL_BIGINT or (Self.FXSQLVAR^.sqltype and 1);
  Self.FXSQLVAR^.sqlscale:=-4;
  Self.FXSQLVAR^.sqllen  := SizeOf(FXInt64);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(FXInt64),False);
  PCurrency(Self.FXSQLVAR^.sqldata)^:=Value;
  Assert(SizeOf(FXInt64)=SizeOf(Currency));

  // Update State
  fModified:=True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsDate: TDateTime;
var ptimestamp:PISC_TIMESTAMP;
    pdate:PISC_DATE;
    ptime:PISC_TIME;
    s:String;
begin
  result :=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_DATE: begin
        pdate:=PISC_DATE(FXSQLVAR^.sqldata);
        DecodeSQLDate(pdate,result);
        result:=Trunc(result);
        end;
      SQL_TIME: begin
        ptime:=PISC_TIME(FXSQLVAR^.sqldata);
        DecodeSQLTime(ptime,result);
        result:=Trunc(result);
        end;
      SQL_TIMESTAMP: begin
        ptimestamp:=PISC_TIMESTAMP(PISC_QUAD(FXSQLVAR^.sqldata));
        DecodeSQLTimeStamp(ptimestamp,result);
        result:=Trunc(result);
        end;
      else begin
        s:=Self.AsString;
        if s<>'' then
          result:=StrToDate(s);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsDate(Const Value: TDateTime);
var xvar:TFXSQLVAR;
    i:Integer;
begin
  // Set sqltype
  Self.FXSQLVAR^.sqltype := SQL_DATE or (Self.FXSQLVAR^.sqltype and 1);
  Self.FXSQLVAR^.sqlscale:= 0;
  Self.FXSQLVAR^.sqllen  := SizeOf(ISC_DATE);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(ISC_DATE),False);
  PISC_DATE(Self.FXSQLVAR^.sqldata)^:=EncodeDateTime2SQLDate(Value);

  // Update State
  fModified:=True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsTime: TDateTime;
var ptimestamp:PISC_TIMESTAMP;
    pdate:PISC_DATE;
    ptime:PISC_TIME;
    s:String;
begin
  result :=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_DATE: begin
        pdate:=PISC_DATE(FXSQLVAR^.sqldata);
        DecodeSQLDate(pdate,result);
        Result:=Frac(result);
        end;
      SQL_TIME: begin
        ptime:=PISC_TIME(FXSQLVAR^.sqldata);
        DecodeSQLTime(ptime,result);
        Result:=Frac(result);
        end;
      SQL_TIMESTAMP: begin
        ptimestamp:=PISC_TIMESTAMP(PISC_QUAD(FXSQLVAR^.sqldata));
        DecodeSQLTimeStamp(ptimestamp,result);
        Result:=Frac(result);
        end;
      else begin
        s:=Self.AsString;
        if s<>'' then
          result:=StrToTime(s);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsTime(Const Value: TDateTime);
var xvar:TFXSQLVAR;
    i:Integer;
begin
  // Set sqltype
  Self.FXSQLVAR^.sqltype := SQL_TIME or (Self.FXSQLVAR^.sqltype and 1);
  Self.FXSQLVAR^.sqlscale:= 0;
  Self.FXSQLVAR^.sqllen  := SizeOf(ISC_TIME);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(ISC_TIME),False);
  PISC_TIME(Self.FXSQLVAR^.sqldata)^:=EncodeDateTime2SQLTime(Value);

  // Update State
  fModified:=True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsDateTime:TDateTime;
var ptimestamp:PISC_TIMESTAMP;
    pdate:PISC_DATE;
    ptime:PISC_TIME;
    s:String;
begin
  result :=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_DATE: begin
        pdate:=PISC_DATE(FXSQLVAR^.sqldata);
        DecodeSQLDate(pdate,result);
        end;
      SQL_TIME: begin
        ptime:=PISC_TIME(FXSQLVAR^.sqldata);
        DecodeSQLTime(ptime,result);
        end;
      SQL_TIMESTAMP: begin
        ptimestamp:=PISC_TIMESTAMP(PISC_QUAD(FXSQLVAR^.sqldata));
        DecodeSQLTimeStamp(ptimestamp,result);
        end;
      else begin
        s:=Self.AsString;
        if s<>'' then
          result:=StrToDateTime(s);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsDateTime(Const Value:TDateTime);
var xvar: TFXSQLVAR;
    i:Integer;
begin
  // Set sqltype
  Self.FXSQLVAR^.sqltype := SQL_TIMESTAMP or (Self.FXSQLVAR^.sqltype and 1);
  Self.FXSQLVAR^.sqlscale:= 0;
  Self.FXSQLVAR^.sqllen  := SizeOf(ISC_TIMESTAMP);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(ISC_TIMESTAMP),False);
  PISC_TIMESTAMP(Self.FXSQLVAR^.sqldata)^:=EncodeDateTime2SQLTimeStamp(Value);

  // Update State
  fModified:=True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsTimeStamp:TTimeStamp;
var ptimestamp:PISC_TIMESTAMP;
    pdate:PISC_DATE;
    ptime:PISC_TIME;
    dt:TDateTime;
    s:String;
begin
  result.Date:=0;
  result.Time:=0;
  if Not Self.IsNull then Begin
    Case Self.SQLType of
      SQL_DATE: begin
        pdate:=PISC_DATE(FXSQLVAR^.sqldata);
        DecodeSQLDate(pdate,dt);
        result:=DateTimeToTimeStamp(dt)
        end;
      SQL_TIME: begin
        ptime:=PISC_TIME(FXSQLVAR^.sqldata);
        DecodeSQLTime(ptime,dt);
        result:=DateTimeToTimeStamp(dt)
        end;
      SQL_TIMESTAMP: begin
        ptimestamp:=PISC_TIMESTAMP(PISC_QUAD(FXSQLVAR^.sqldata));
        DecodeSQLTimeStamp(ptimestamp,dt);
        result:=DateTimeToTimeStamp(dt)
        end;
      else begin
        s:=Self.AsString;
        if s<>'' then Begin
          dt:=StrToDateTime(s);
          result:=DateTimeToTimeStamp(dt)
    end end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsTimeStamp(Const Value: TTimeStamp);
var xvar: TFXSQLVAR;
    i:Integer;
begin
  // Set sqltype
  Self.FXSQLVAR^.sqltype := SQL_TIMESTAMP or (Self.FXSQLVAR^.sqltype and 1);
  Self.FXSQLVAR^.sqlscale:= 0;
  Self.FXSQLVAR^.sqllen  := SizeOf(ISC_TIMESTAMP);

  // Set Is Not Null
  if Self.FXSQLVAR^.sqlind<>nil then
    FXSQLVAR^.sqlind^ := 0;

  // Alloc and set sqldata
  FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(ISC_TIMESTAMP),False);
  PISC_TIMESTAMP(Self.FXSQLVAR^.sqldata)^:=EncodeTimeStamp2SQLTimeStamp(Value);

  // Update State
  fModified:=True;

  // Assign to Duplicate
  if fDuplicated then Begin
    for i := 0 to Pred(fParent.FCount) do begin
      xvar := fParent[i];
      if xvar<>Self then Begin
        if xvar.fName=Self.fName then Begin
          xvar.AssignSQLVAR(fXSQLVAR);
    end end end end
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsQuad: TISC_QUAD;
begin
  result.gds_quad_high := 0;
  result.gds_quad_low  := 0;
  if not Self.IsNull then Begin
    case Self.SQLType of
      SQL_BLOB, SQL_ARRAY, SQL_QUAD:Begin
        Assert(FXSQLVAR^.sqllen=SizeOf(TISC_QUAD));
        result := PISC_QUAD(FXSQLVAR^.sqldata)^;
        end;
      else Begin
        FXRaiseClientError(Self,fxceInvalidDataConversion);
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsQuad(Const Value: TISC_QUAD);
var xvar: TFXSQLVAR;
    i: Integer;
begin
  // Check we have a Blob
  Case Self.SQLType of
    SQL_BLOB,SQL_ARRAY:Begin
      // Set Is Not Null
      if Self.FXSQLVAR^.sqlind<>nil then
        FXSQLVAR^.sqlind^ := 0;
      // Alloc and set sqldata
      Self.FXSQLVAR^.sqllen  := SizeOf(TISC_QUAD);
      FXAlloc(Self.FXSQLVAR^.sqldata,SizeOf(TISC_QUAD),False);
      PISC_QUAD(Self.FXSQLVAR^.sqldata)^:=Value;
      // Update State
      fModified:=True;
      // Assign to Duplicate
      if fDuplicated then Begin
        for i := 0 to Pred(fParent.FCount) do begin
          xvar := fParent[i];
          if xvar<>Self then Begin
            if xvar.fName=Self.fName then Begin
              xvar.AssignSQLVAR(fXSQLVAR);
      end end end end end
    else Begin
      FXRaiseClientError(Self,fxceInvalidDataConversion);
    end end;
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsAnsiString:AnsiString;
Var ms:TMemoryStream;
    str_len:FXShort;
    sz:PAnsiChar;
    d:TDateTime;
    e:Extended;
    i64:Int64;
Begin
  result:='';
  if Not Self.IsNull then Begin
    case Self.SQLType of
      SQL_BLOB:begin
        ms:=TMemoryStream.Create;
        try Self.BlobToStream(ms);
            sz:=ms.Memory;
            SetString(result, sz, ms.Size);
        finally
            ms.Free;
        end end;
      SQL_TEXT:Begin
        str_len := FXSQLVar^.sqllen;
        sz:=PAnsiChar(FXSQLVAR^.sqldata);
        SetString(result, sz, str_len);
        end;
      SQL_VARYING:Begin
        str_len:=mFX.Header.vax_Integer(FXSQLVAR^.sqldata, 2);
        sz:=PAnsiChar(FXSQLVAR^.sqldata);Inc(sz,2);
        SetString(result, sz, str_len);
        end;
      SQL_DATE:Begin
        d:=Self.AsDate;
        result:=AnsiString(DateToStr(d));
        end;
      SQL_TIME:Begin
        d:=Self.AsTime;
        result:=AnsiString(TimeToStr(d));
        end;
      SQL_TIMESTAMP:Begin
        d:=Self.AsDateTime;
        result:=AnsiString(DateTimeToStr(d));
        end;
      SQL_SHORT,SQL_LONG,SQL_BIGINT:Begin
        if FXSQLVAR^.sqlscale = 0 then Begin
          i64:=Self.AsInt64;
          Result:= AnsiString(IntToStr(i64));
        end else Begin
          e:=Self.AsExtended;
          result := AnsiString(FloatToStr(e));
        end end;
      SQL_DOUBLE,SQL_FLOAT,SQL_D_FLOAT:Begin
        e:=Self.AsExtended;
        result := AnsiString(FloatToStr(e));
        end;
    else Begin
      FXRaiseClientError(Self,fxceInvalidDataConversion);
    end end end;
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsAnsiTrimString:AnsiString;
Var s:AnsiString;
    i:Integer;
begin
  s:=GetAsAnsiString;
  i:=Length(s);
  while (I > 0) and (s[I] <= ' ') do
    Dec(I);
  Result := Copy(s, 1, i);
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsAnsiString(Const Value: AnsiString);
var ms:TMemoryStreamAccess;
    Len,i32:Integer;
    curr:Currency;
    d:TDateTime;
    e:extended;
    i64:Int64;
begin
  case fDSQLType of
    SQL_BLOB:begin
      Len:=Length(Value);
      ms:=TMemoryStreamAccess.Create;
      try ms.SetPointer(@Value[1],Len);
          Self.LoadFromStream(ms);
      finally
          ms.Free;
      end end;
    SQL_TEXT,SQL_VARYING:Begin
      // Assign
      AssignAnsiString(Value);
      end;
    SQL_DATE:Begin
      d:=StrToDate(String(Value));
      SetAsDate(d);
      end;
    SQL_TIME:Begin
      d:=StrToTime(String(Value));
      SetAsTime(d);
      end;
    SQL_TIMESTAMP:Begin
      d:=StrToDateTime(String(Value));
      SetAsDateTime(d);
      end;
    SQL_SHORT,SQL_LONG:Begin
      if FXSQLVAR^.sqlscale = 0 then Begin
        i32:=StrToInt(String(Value));
        Self.SetAsInt32(i32)
      end else
      if FXSQLVAR^.sqlscale >=-4 then Begin
        curr:=StrToCurr(String(Value));
        Self.SetAsCurrency(curr);
      end else Begin
        e:=StrToFloat(String(Value));
        SetAsExtended(e);
      end end;
    SQL_BIGINT:Begin
      if FXSQLVAR^.sqlscale = 0 then Begin
        i64:=StrToInt64(String(Value));
        SetAsInt64(i64);
      end else
      if FXSQLVAR^.sqlscale >=-4 then Begin
        curr:=StrToCurr(String(Value));
        Self.SetAsCurrency(curr);
      end else Begin
        e:=StrToFloat(String(Value));
        SetAsExtended(e);
      end end;
    SQL_DOUBLE,SQL_FLOAT,SQL_D_FLOAT:Begin
      e:=StrToFloat(String(Value));
      SetAsExtended(e);
      end;
    else Begin
      FXRaiseClientError(Self,fxceInvalidDataConversion);
    end end;
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsAnsiTrimString(Const Value: AnsiString);
Var s:AnsiString;
    i:Integer;
begin
  i:=Length(Value);
  while (I > 0) and (Value[I] <= ' ') do
    Dec(I);
  s:=Copy(Value, 1, i);
  SetAsAnsiString(s);
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsString: String;
Var bs:TBytesStream;
    len:FXShort;
    b:TBytes;
    ms:TMemoryStream;
    str_len:FXShort;
    sz:PAnsiChar;
    d:TDateTime;
    e:Extended;
    i64:Int64;
Begin
  result:='';
  if Not Self.IsNull then Begin
    case Self.SQLType of
      SQL_BLOB:begin
        // Translitarate !!!
        Case Self.SQLBlobType of
          isc_blob_text:Begin
            bs:=TBytesStream.Create;
            try BlobToStream(bs);
                result:=fSQL.Encoding.GetString(bs.bytes,0,bs.Size);
            finally
                bs.Free;
            end end;
          else Begin
            ms:=TMemoryStream.Create;
            try Self.BlobToStream(ms);
                sz:=ms.Memory;
                SetString(result, sz, ms.Size);
            finally
                ms.Free;
          end end end;
        end;
      SQL_TEXT:Begin
        str_len := FXSQLVar^.sqllen;
        if FXSQLVAR^.SQLSubtype>1 then
          len:=str_len div FXSQLVAR^.SQLSubtype else
          len:=str_len;
        SetLength(b,str_len);
        move(FXSQLVAR^.sqldata^, b[0], str_len);
        result:=fSQL.Encoding.GetString(b);
        str_len:=Length(Result);
        while (str_len>len)and(Result[str_len]<=#32) do
          Dec(str_len);
        SetLength(Result,str_len);
        end;
      SQL_VARYING:Begin
        len:=mFX.Header.vax_integer(FXSQLVAR^.sqldata, 2);
        SetLength(b,len);
        move(PAnsiChar(FXSQLVAR^.sqldata)[2], b[0], len);
        result:=fSQL.Encoding.GetString(b);
        end;
      SQL_DATE:Begin
        d:=Self.AsDate;
        result:=DateToStr(d);
        end;
      SQL_TIME:Begin
        d:=Self.AsTime;
        result:=TimeToStr(d);
        end;
      SQL_TIMESTAMP:Begin
        d:=Self.AsDateTime;
        result:=DateTimeToStr(d);
        end;
      SQL_SHORT,SQL_LONG,SQL_BIGINT:Begin
        if FXSQLVAR^.sqlscale = 0 then Begin
          i64:=Self.AsInt64;
          Result:= IntToStr(i64);
        end else Begin
          e:=Self.AsExtended;
          result := FloatToStr(e);
        end end;
      SQL_DOUBLE,SQL_FLOAT,SQL_D_FLOAT:Begin
        e:=Self.AsExtended;
        result := FloatToStr(e);
        end;
    else Begin
      FXRaiseClientError(Self,fxceInvalidDataConversion);
    end end end;
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsTrimString: String;
Var s:String;
begin
  s:=GetAsString;
  Result:=TrimRight(s);
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsString(Const Value: String);
Var bs:TBytesStream;
    b:TBytes;
    curr:Currency;
    i32:Integer;
    d:TDateTime;
    e:Extended;
    i64:Int64;
begin
  case fDSQLType of
    SQL_BLOB:begin
      bs:=TBytesStream.Create(fSQL.Encoding.GetBytes(Value));
      try LoadFromStream(bs);
      finally
          bs.Free;
      end end;
    SQL_TEXT,SQL_VARYING:Begin
      // Assign
      b:=fSQL.Encoding.GetBytes(Value);
      AssignBytes(b);
      end;
    SQL_DATE:Begin
      d:=StrToDate(Value);
      SetAsDate(d);
      end;
    SQL_TIME:Begin
      d:=StrToTime(Value);
      SetAsTime(d);
      end;
    SQL_TIMESTAMP:Begin
      d:=StrToDateTime(Value);
      SetAsDateTime(d);
      end;
    SQL_SHORT,SQL_LONG:Begin
      if FXSQLVAR^.sqlscale = 0 then Begin
        i32:=StrToInt(Value);
        Self.SetAsInt32(i32)
      end else
      if FXSQLVAR^.sqlscale >=-4 then Begin
        curr:=StrToCurr(Value);
        Self.SetAsCurrency(curr);
      end else Begin
        e:=StrToFloat(Value);
        SetAsExtended(e);
      end end;
    SQL_BIGINT:Begin
      if FXSQLVAR^.sqlscale = 0 then Begin
        i64:=StrToInt64(Value);
        SetAsInt64(i64);
      end else
      if FXSQLVAR^.sqlscale >=-4 then Begin
        curr:=StrToCurr(Value);
        Self.SetAsCurrency(curr);
      end else Begin
        e:=StrToFloat(Value);
        SetAsExtended(e);
      end end;
    SQL_DOUBLE,SQL_FLOAT,SQL_D_FLOAT:Begin
      e:=StrToFloat(Value);
      SetAsExtended(e);
      end;
    else Begin
      FXRaiseClientError(Self,fxceInvalidDataConversion);
    end end;
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsTrimString(const Value: String);
Var s:String;
begin
  s:=TrimRight(Value);
  SetAsString(s);
end;
{______________________________________________________________________________}
function TFXSQLVAR.GetAsVariant: Variant;
begin
  if not Self.IsNull then Begin
    case Self.SQLType of
      SQL_TEXT, SQL_VARYING:
        result := Self.AsString;
      SQL_DATE:
        result := Self.AsDate;
      SQL_TIME:
        result := Self.AsTime;
      SQL_TIMESTAMP:
        result := Self.AsDateTime;
      SQL_SHORT:Begin
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXShort(FXSQLVAR^.sqldata)^ else
        if FXSQLVAR^.sqlscale >= -4 then
          result:= Self.AsCurrency else
          result:= Self.AsDouble;
        end;
      SQL_LONG:Begin
        if FXSQLVAR^.sqlscale = 0 then
          Result:= PFXLong(FXSQLVAR^.sqldata)^ else
        if FXSQLVAR^.sqlscale >= -4 then
          result:= Self.AsCurrency else
          result:= Self.AsDouble;
        end;
      SQL_BIGINT:Begin
        if FXSQLVAR^.sqlscale=0 then
          Result:= PFXInt64(FXSQLVAR^.sqldata)^ else
          Result:= Self.AsDouble;
        end;
      SQL_FLOAT:Begin
        Result := PFXFloat(FXSQLVAR^.sqldata)^;
        end;
      SQL_DOUBLE,SQL_D_FLOAT:Begin
        Result := PFXDouble(FXSQLVAR^.sqldata)^;
        end;
      SQL_BLOB:begin
        Case FXSQLVAR^.sqlsubtype  of
          isc_blob_text:Begin
            // TODO Check If Fetch On Demand
            Result:=Self.AsString;
            end;
         else Begin
           // TODO Check better FXRaiseClientError(Self,fxceInvalidDataConversion)
           Result:='(BLOB)';
         end end;
      end else Begin
        FXRaiseClientError(Self,fxceInvalidDataConversion)
    end end
  end else
    result := System.Variants.NULL;
end;
{______________________________________________________________________________}
procedure TFXSQLVAR.SetAsVariant(Const Value: Variant);
Var d:TDateTime;
    vt:TVarType;
begin
  vt:=System.Variants.VarType(Value);
  Case vt of
    varEmpty,
    varNull:
      Self.SetIsNull(True);
    varSmallint,
    varByte:
      Self.SetAsInt16(Value);
    varInteger:
      Self.SetAsInt32(Value);
    varInt64:
      Self.SetAsInt64(Value);
    varSingle:
      Self.SetAsSingle(Value);
    varDouble:
      Self.SetAsDouble(Value);
    varCurrency:
      Self.SetAsCurrency(Value);
    varBoolean:if Value then
      Self.SetAsInt32(1) else
      Self.SetAsInt32(0);
    varDate:Begin
      d:=Value;
      Case Self.SQLType of
        SQL_DATE:Begin
          Self.SetAsDate(d);
          end;
        SQL_TIME:Begin
          Self.SetAsTime(d);
          end;
        SQL_TIMESTAMP:Begin
          Self.SetAsDateTime(d);
          end;
        else Begin
          if Frac(d)=0 then
            Self.SetAsTime(d) else
          if Int(d)=0 then
            Self.SetAsDate(d) else
            Self.SetAsDateTime(d);
      end end end;
    varUString,varOleStr,varString:
      Self.SetAsString(Value);
    else Begin
      if (vt and varArray)=varArray then Begin
        LoadFromByteArray(Value);
      end else
        FXRaiseClientError(Self,fxceNotPermitted);
    end end
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXCustomSQLDA.Create(Const aQuery: TFXCustomSQL);
begin
  inherited Create;
  fNames := TStringList.Create;
  fSQL   := aQuery;
end;
{______________________________________________________________________________}
destructor TFXCustomSQLDA.Destroy;
var i: Integer;
begin
  if fSQLDA <> nil then begin
    // Access violation !!! after fCount
    for i := 0 to FSize - 1 do begin
      FXFree(Pointer(fSQLVARs[i].FXSQLVAR^.sqlind));
      FXFree(fSQLVARs[i].FXSQLVAR^.sqldata);
      fSQLVARs[i].Free; // <<<< Access violation !!! after fCount
      end;
    FreeMem(fSQLDA);
    fSQLVARs:= nil;///SetLength(fSQLVARs,0);//
    fSQLDA  := nil;
    end;
  fNames.Free;
  inherited
end;
{______________________________________________________________________________}
function TFXCustomSQLDA.GetModified: Boolean;
var i: Integer;
begin
  result := False;
  for i := 0 to Pred(fCount) do Begin
    if fSQLVARs[i].Modified then begin
      result:=True;
      Break;
    end end
end;
{______________________________________________________________________________}
function TFXCustomSQLDA.GetDatabase: TFXCustomDatabase;
Begin
  Result:=fSQL.Database
end;
{______________________________________________________________________________}
function TFXCustomSQLDA.GetTransaction: TFXCustomTransaction;
Begin
  Result:=fSQL.Transaction
end;
{______________________________________________________________________________}
function TFXCustomSQLDA.GetNames:String;
begin
  result := fNames.Text;
end;
{______________________________________________________________________________}
function TFXCustomSQLDA.IsDuplicate(Const aIdx: String): Boolean;
var i, Cnt, Nb: Integer;
    ffn: String;
begin
  Cnt := Pred(fNames.Count);
  ffn := FXFormatIdentifier(aIdx);
  for i:=0 to Cnt do Begin
    if (fNames[i] = ffn) then Begin
      Nb:=Integer(fNames.Objects[i]);
      if Nb>0 then Begin
        Result:=True;
        exit;
    end end end;
  Result:=False
end;
{______________________________________________________________________________}
function TFXCustomSQLDA.GetXSQLDA: PXSQLDA;
begin
  result := fSQLDA;
end;
{______________________________________________________________________________}
function TFXCustomSQLDA.GetByName(Const aIdx: String): TFXSQLVAR;
var i, Cnt : Integer;
    ffn: String;
begin
  Cnt := Pred(fNames.Count);
  ffn := FXFormatIdentifier(aIdx);
  for i:=0 to Cnt do Begin
    if (fNames[i] = ffn) then Begin
      Result:=fSQLVARs[i];
      exit;
    end end;
  Result:=nil
end;
{______________________________________________________________________________}
function TFXCustomSQLDA.GetByIdx(Const Idx: Integer): TFXSQLVAR;
begin
  if (Idx < 0) or (Idx >= FCount) then
    FXRaiseClientError(Self,fxceXSQLDAIndexOutOfRange);
  result := fSQLVARs[Idx]
end;
{______________________________________________________________________________}
function TFXCustomSQLDA.FindByName(Const aIdx: String): TFXSQLVAR;
Begin
  result := Self.GetByName(aIdx);
end;
{______________________________________________________________________________}
function TFXCustomSQLDA.ByName(Const aIdx: String): TFXSQLVAR;
begin
  result := GetByName(aIdx);
  if result = nil then
    FXRaiseClientErrorFmt(Self,fxceFieldNotFound, [aIdx]);
end;
{______________________________________________________________________________}
procedure TFXCustomSQLDA.SetCount(Const Value: Integer);
var i, OldSize, NewSize: Integer;
    p : PXSQLVAR;
begin
  fCount:=Value;
  if fCount>0 then Begin
    if fSize > 0 then
      OldSize := XSQLDA_LENGTH(fSize) else
      OldSize := 0;
    if fCount > fSize then begin
      NewSize:=XSQLDA_LENGTH(fCount);
      FXReAlloc(fSQLDA,OldSize,NewSize,True);
//      l:=Length(fSQLVARs);
      SetLength(fSQLVARs, FCount);
//      for i := l to Pred(FCount) do
//        Assert(fSQLVARs[i]=nil);
      fSQLDA^.version:=SQLDA_VERSION1;
      p := @fSQLDA^.sqlvar[0];
      for i := 0 to Pred(FCount) do begin
        if i >= FSize then
          fSQLVARs[i] := TFXSQLVAR.Create(self, fSQL);
        fSQLVARs[i].FXSQLVAR := p;
        p := Pointer(PByte(p) + sizeof(fSQLDA^.sqlvar));
        end;
      fSize := fCount;
      end;
    if fSize > 0 then begin
      fSQLDA^.sqln := Value;
      fSQLDA^.sqld := Value;
    end end;

  if fCount>0 then Begin
    While fNames.Count>fCount do
      fNames.Delete(fCount);
  End else
    fNames.Clear;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSQLDAParams.DSQLPreprocess(Const aParams:TStrings);
Var fpn,pn:String;
    i:Integer;
Begin
  fNames.Clear;
  if aParams<>nil then Begin
    Self.Count:=aParams.Count;
    for i := 0 to Pred(aParams.Count) do Begin
      pn:=aParams[i];
      fpn:=FXFormatIdentifier(pn);
      fSQLVARs[i].fName := fpn;
      fSQLVARs[i].fIndex:= i;
      fNames.Add(fpn);
      end;
  End else
    Self.Count:=0;
End;
{______________________________________________________________________________}
procedure TFXSQLDAParams.DSQLDescribe;
var i,j,n:Integer;
    c:TFXSQLVAR;
    d:PXSQLVAR;
    s:String;
begin
  Assert(fSQLDA<>nil);
  Assert(fNames.Count=fCount);
  for i := 0 to Pred(fCount) do begin
    c:=fSQLVARs[i];
    c.fModified:=False;
    d:=c.Data;
    // Get SQLType
    c.fDSQLType:=d^.sqltype and (not 1);
    // Find Duplicated
    n:=0;
    for j:=0 to Pred(fNames.Count) do begin
      s:=fNames[j];
      if s=c.fName then
        Inc(n);
      end;
    Assert(n>=1);
    Assert(c.fIndex=i);
    c.fDuplicated:=(n>1);
    Assert(c.fName=fNames[i]);
    // Get Text MaxLen
    Case c.fDSQLType of
      SQL_TEXT,SQL_VARYING:Begin
        c.fMaxBytes:=d^.sqllen;
        c.fMaxChars:=d^.MaxChars;
        end;
      else Begin
        c.fMaxBytes:=0;
        c.fMaxChars:=0;
      end end;
    // Alloc sqldata
    Case c.fDSQLType of
      SQL_TEXT, SQL_DATE, SQL_TIME, SQL_TIMESTAMP,
      SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT,
      SQL_LONG, SQL_BIGINT, SQL_DOUBLE, SQL_FLOAT,
      SQL_D_FLOAT:begin
        if (d^.sqllen = 0) then Begin
          // Make sure you get a valid pointer anyway select '' from foo
          FXAlloc(d^.sqldata,1,True)
        end else
          FXAlloc(d^.sqldata,d^.sqllen,True)
        end;
      SQL_VARYING:begin
        FXAlloc(d^.sqldata,d^.sqllen+2,True);
        end;
      else begin
        FXRaiseClientErrorFmt(Self,fxceUnknownSQLDataType,[c.fDSQLType])
      end end;
    // Init Null
    if (d^.sqltype and 1 = 1) then
      FXAlloc(Pointer(d^.sqlind), SizeOf(d^.sqlind^),True) else
    if (d^.sqlind <> nil) then
      FXFree(Pointer(d^.sqlind));
    end
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSQLDAFields.DSQLPreprocess(Const aFields:TStrings);
Var pn:String;
    i:Integer;
Begin
  fNames.Clear;
  fRelationName:='';
  if aFields<>nil then Begin
    Self.Count:=aFields.Count;
    for i := 0 to Pred(aFields.Count) do Begin
      pn:=aFields[i];
      fSQLVARs[i].fName := pn;
      fSQLVARs[i].fIndex:= i;
      fNames.Add(pn);
      end;
  End else
    Self.Count:=0;
End;
{______________________________________________________________________________}
procedure TFXSQLDAFields.DSQLDescribe;
var RelationNameIsUnique:Boolean;
    fn,ffn,sp,st:String;
    i,j,len:Integer;
    c:TFXSQLVAR;
    d:PXSQLVAR;
Begin
  Assert(fSQLDA<>nil);
  Assert(fRelationName='');
  RelationNameIsUnique:=True;

  for i := 0 to Pred(FCount) do begin
    c:=fSQLVARs[i];
    c.fModified:=False;
    c.fDuplicated:=False;
    d:=c.Data;
    // Get SQLType
    c.fDSQLType:=d^.sqltype and (not 1);
    // Find Unique RelationName
    if (RelationNameIsUnique)and(d^.relname_length>0) then Begin
      st:=fSQL.DecodeQuery(d^.relname,d^.relname_length);
      if fRelationName=EmptyStr then Begin
        //First One
        fRelationName:=st
      end else
      if st<>fRelationName then begin
        //RelationName Not Unique !!
        RelationNameIsUnique:=False;
        fRelationName:=EmptyStr;
      end else Begin
        //OK Still Same
      end end;
    // Get Unique Field Name
    if d^.aliasname_length=0 then begin
      j:=1;
      fn:='F_1';
      while IsDuplicate(fn) do begin
        Inc(j);
        fn:='F_'+IntToStr(j);
        end;
      fSQL.RecodeQuery(fn,d^.aliasname,d^.aliasname_length);
    end else Begin
      fn:=fSQL.DecodeQuery(d^.aliasname, d^.aliasname_length);
      if IsDuplicate(fn) Then begin
        sp:=fn+'_';
        fn:=sp+'1';
        len:=Length(fn);
        While len>31 do Begin
          Delete(sp,len-2,2);
          fn:=sp+'_1';
          len:=Length(fn);
          End;
        j:=1;
        while IsDuplicate(fn) do begin
          Inc(j);
          fn:=sp+IntToStr(j);
          len:=Length(fn);
          While len>31 do Begin
            Delete(sp,len-2,2);
            fn:=sp+'_'+IntToStr(j);
            len:=Length(fn);
          end end;
        fSQL.RecodeQuery(fn,d^.aliasname,d^.aliasname_length);
      end end;
    ffn:=FXFormatIdentifier(fn);
    fSQLVARs[i].fName := ffn;
    fSQLVARs[i].fIndex:= i;
    if i<fNames.Count then Begin
      Assert(Integer(fNames.Objects[i])=0);
      fNames.Objects[i]:=Pointer(1);
      fNames[i]:=ffn
    end else Begin
      fNames.AddObject(ffn,Pointer(1));
      end;
    // Get Text MaxLen
    Case c.fDSQLType of
      SQL_TEXT,SQL_VARYING:Begin
        c.fMaxBytes:=d^.sqllen;
        c.fMaxChars:=d^.MaxChars;
        end;
      else Begin
        c.fMaxBytes:=0;
        c.fMaxChars:=0;
      end end;
    // Alloc sqldata
    Case c.fDSQLType of
      SQL_TEXT, SQL_DATE, SQL_TIME, SQL_TIMESTAMP,
      SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT,
      SQL_LONG, SQL_BIGINT, SQL_DOUBLE, SQL_FLOAT,
      SQL_D_FLOAT:begin
        if (d^.sqllen = 0) then Begin
          // Make sure you get a valid pointer anyway select '' from foo
          FXAlloc(d^.sqldata,1,True)
        end else
          FXAlloc(d^.sqldata,d^.sqllen,True)
        end;
      SQL_VARYING:begin
        FXAlloc(d^.sqldata,d^.sqllen+2,True);
        end;
      else begin
        FXRaiseClientErrorFmt(Self,fxceUnknownSQLDataType,[c.fDSQLType])
      end end;
    // Init Null
    if (d^.sqltype and 1 = 1) then
      FXAlloc(Pointer(d^.sqlind), SizeOf(d^.sqlind^),True) else
    if (d^.sqlind <> nil) then
      FXFree(Pointer(d^.sqlind));
    end
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXCustomSQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fSQLText := TStringList.Create;
  fProcessedSQL := TStringList.Create;
  TStringList(fSQLText).OnChanging := SQLChanging;
  TStringList(fSQLText).OnChange   := SQLChanged;

  fSQLParams := TFXSQLDAParams.Create(self);
  fSQLFields := TFXSQLDAFields.Create(self);
  fCursor    := AnsiString(Self.Name+mFX.Utils.FXRandomString(8));

  fDefaultFieldsCount := 10;
  fParamCheck         := True;
end;
{______________________________________________________________________________}
destructor TFXCustomSQL.Destroy;
begin
  inherited Destroy;
  fSQLFields.Free;
  fSQLParams.Free;
  fProcessedSQL.Free;
  fSQLParser.Free;
  fSQLText.Free;
end;
{______________________________________________________________________________}
Procedure TFXCustomSQL.UnPrepare;
Begin
  fEOF:=False;
  fRecordCount:=0;
  fCursorOpen:=False;
  if fSQLParser<>nil then
    fSQLParser.Clear;

  fSQLHandle:=nil;
  fPrepared:=False;
  fQueryType:=fxSQLUnknown;
  if fSQLParams<>nil then
    fSQLParams.DSQLPreprocess(nil);
  if fSQLFields<>nil then
    fSQLFields.DSQLPreprocess(nil);
End;
{______________________________________________________________________________}
function TFXCustomSQL.GetSingleLineQuery:String;
Var i,l:Integer;
    Last:Char;
    s:String;
Begin
  Result:=EmptyStr;
  for i:=0 to Pred(fSQLText.Count) do Begin
    s:=Trim(fSQLText[i]);
    if s<>EmptyStr then
      Result:=Result+' '+s;
    end;

  Last:=';';
  l:=Length(result);
  while l>0 do Begin
    if result[l].IsWhiteSpace then Begin
      Dec(l);
      Continue;
      end;
    Last:=result[l];
    Break;
    End;

  if Last<>';' then
    result:=result+';'
End;
{______________________________________________________________________________}
Procedure TFXCustomSQL.SetSingleLineQuery(Const Value:String);
Begin
  Self.SQL.Text:=Value;
End;
{______________________________________________________________________________}
{$IFDEF mFXTrace}
procedure TFXCustomSQL.LogPrepare;
Var msg:String;
Begin
  if Self.DoLogAction(fxtSQLPrepare) then Begin
    if Self.Database.Name=EmptyStr then
      msg:='DB:'+Self.Database.DatabaseName else
      msg:='DB:'+Self.Database.Name+' ('+Self.Database.DatabaseName+')';
    Self.Log(fxtSQLPrepare,msg);
    end
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.LogQuery(Const aQuery: string);
Var msg,sql:String;
    l:Integer;
    Last:Char;
Begin
  if Self.DoLogAction(fxtSQLText) then Begin
    Last:=';';
    sql:=Trim(aQuery);
    l:=Length(sql);
    while l>0 do Begin
      if sql[l].IsWhiteSpace then Begin
        Dec(l);
        Continue;
        end;
      Last:=sql[l];
      Break;
      end;
    if Last<>';' then
      msg:=sql+';' else
      msg:=sql;
    Self.Log(fxtSQLText,sql);
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.LogParams;
var uFieldName:String;
    msg,pVal:String;
    p:TFXSQLVAR;
    guid:TGUID;
    i:Integer;
Begin
  if Self.DoLogAction(fxtSQLParams) then Begin
    if (fSQLParams<>nil)and(fSQLParams.Count>0) then Begin
      Self.Log(fxtSQLParams,'PARAMS');
      try for i := 0 to Pred(fSQLParams.Count) do Begin
            p:=fSQLParams[i];
            if p.IsNull then Begin
              pVal:='<null>'
            end else Begin
              Case p.SQLType of
                SQL_ARRAY:Begin
                  // arrays not supported
                  pVal:='(ARRAY)'
                  end;
                SQL_BLOB:Begin
                  pVal:='(BLOB)'
                  end;
                SQL_TEXT:Begin
                  if (p.Data^.sqllen=Sizeof(TGUID)) Then Begin
                    uFieldName:=UpperCase(p.Name);
                    if (uFieldName.Contains('UUID'))or(uFieldName.Contains('GUID')) Then Begin
                      guid:=PGUID(p.Data^.sqldata)^;
                      pVal:=GUIDToString(guid)
                    End else
                      pVal:=p.AsString;
                  end else
                    pVal:=p.AsString;
                  end;
                else Begin
                  pVal:=p.AsString;
              end end end;
            msg:=format('%20.20s = %s',[p.Name,pVal]);
            Self.Log(fxtSQLParams,msg);
            end;
      except on e:exception do Begin
          //
    end end end end;
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.LogPlan;
Var msg:String;
Begin
  if Self.DoLogAction(fxtSQLPlan) then Begin
    Msg:=Self.GetPlan;
    if Msg<>'' then
      Self.Log(fxtSQLPlan,msg);
    end
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.LogRowsAffected;
Var RowsInserted,RowsUpdated,RowsDeleted:Integer;
Begin
  if Self.DoLogAction(fxtSQLRowsAffected) then Begin
    RowsInserted:=Self.RowsInserted;
    RowsUpdated :=Self.RowsUpdated ;
    RowsDeleted :=Self.RowsDeleted ;
    if (RowsInserted>0)and(RowsUpdated>0)and(RowsDeleted>0) then
      Self.Log(fxtSQLRowsAffected,format('Insert: %d ; Update: %d ; Delete: %d',[RowsInserted,RowsUpdated,RowsDeleted])) else
    if (RowsInserted>0)and(RowsUpdated>0) then
      Self.Log(fxtSQLRowsAffected,format('Insert: %d ; Update: %d',[RowsInserted,RowsUpdated,RowsDeleted])) else
    if (RowsInserted>0)and(RowsDeleted>0) then
      Self.Log(fxtSQLRowsAffected,format('Insert: %d ; Delete: %d',[RowsInserted,RowsUpdated,RowsDeleted])) else
    if (RowsUpdated>0)and(RowsDeleted>0) then
      Self.Log(fxtSQLRowsAffected,format('Update: %d ; Delete: %d',[RowsInserted,RowsUpdated,RowsDeleted])) else
    if (RowsInserted>0) then
      Self.Log(fxtSQLRowsAffected,format('Insert: %d',[RowsInserted])) else
    if (RowsUpdated>0) then
      Self.Log(fxtSQLRowsAffected,format('Update: %d',[RowsUpdated])) else
    if (RowsDeleted>0) then
      Self.Log(fxtSQLRowsAffected,format('Delete: %d',[RowsDeleted])) else
      Self.Log(fxtSQLRowsAffected,format('Insert: %d ; Update: %d ; Delete: %d',[RowsInserted,RowsUpdated,RowsDeleted]))
    end
end;
{$ENDIF mFXTrace}
{______________________________________________________________________________}
procedure TFXCustomSQL.CheckClosed;
begin
  if fSQLHandle<>nil then
    FXRaiseClientError(Self,fxceSQLOpen);
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.CheckCursorClosed;
begin
  if fCursorOpen then
    FXRaiseClientError(Self,fxceSQLOpen);
  Assert(fRecordCount=0);
  Assert(Not fEOF);
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.CheckCursorOpen;
begin
  if not fCursorOpen then
    FXRaiseClientError(Self,fxceSQLClosed);
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.CheckSQLHandle;
Begin
  if (fSQLHandle=nil) then
    FXRaiseClientError(Self,fxceInvalidStatementHandle);
End;
{______________________________________________________________________________}
procedure TFXCustomSQL.CheckSQLQuery;
begin
  if (fSQLText.Count=0) then
    FXRaiseClientError(Self,fxceEmptyQuery);
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.SetSQLText(Const Value:TStrings);
begin
  fSQLText.Assign(Value);
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.SQLChanging(Sender: TObject);
begin
  if Assigned(fOnSQLChanging) then
    fOnSQLChanging(Self);
  if not Self.InternalClose then
    Self.RaiseLastError;
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.SQLChanged(Sender: TObject);
begin
  if Assigned(fOnSQLChanged) then
    fOnSQLChanged(Self)
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetBOF: Boolean;
begin
  result := (not fCursorOpen)or(fRecordCount=1);
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetEOF: Boolean;
begin
  result := (not fCursorOpen)or(fEOF);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXCustomSQL.Encoding:TEncoding;
Begin
  Result:=Self.Database.Encoding
End;
{______________________________________________________________________________}
function TFXCustomSQL.EncodeQuery(Const Value:String):RawByteString;
Var Bytes: TBytes;
    i:NativeInt;
Begin
  Bytes:=Self.Encoding.GetBytes(Value);
  i:=length(Bytes);
  SetLength(Result,i+1);
  move(Bytes[0],Result[1],i);
  Bytes[i]:=0;
End;
{______________________________________________________________________________}
Procedure TFXCustomSQL.EncodeQueryEx(Const Value:String;Var Bytes: TBytes);
Var i:NativeInt;
Begin
  Bytes:=Self.Encoding.GetBytes(Value);
  i:=length(Bytes);
  SetLength(Bytes,i+1);
  Bytes[i]:=0;
End;
{______________________________________________________________________________}
function TFXCustomSQL.DecodeQuery(Const Value:RawByteString):String;
Var Bytes: TBytes;
Begin
  SetLength(Bytes,Length(Value));
  move(Value[1], Bytes[0], Length(Value));
  Result:=Self.Encoding.GetString(Bytes);
End;
{______________________________________________________________________________}
function TFXCustomSQL.DecodeQuery(Const Value:PAnsiChar;Const Len:SmallInt):String;
Var Bytes: TBytes;
Begin
  SetLength(Bytes,Len);
  move(Value[0], Bytes[0], Len);
  Result:=Self.Encoding.GetString(Bytes);
End;
{______________________________________________________________________________}
procedure TFXCustomSQL.RecodeQuery(Const Value:String;Const Dest:PAnsiChar;Var Len:SmallInt);
Var fn:RawByteString;
Begin
  fn:=EncodeQuery(Value);
  Len:=Length(fn);
  System.AnsiStrings.StrPCopy(Dest,fn);
  {$IFDEF DEBUG}
//raise Exception.Create('Badly writen Query , duplicate fieldName, no Alias, ...');
  {$ENDIF DEBUG}
End;
{______________________________________________________________________________}
procedure TFXCustomSQL.SQLLoadFromRes(Const aResName:String);
Var Stream:TCustomMemoryStream;
Begin
  Stream:=nil;
  CheckClosed;
  fSQLText.BeginUpdate;
  try fSQLText.Clear;
      Stream:=TResourceStream.Create(HInstance,aResName,RT_RCDATA);
      fSQLText.LoadFromStream(Stream);// let delphi find Encoding :) TEncoding.ANSI
      FreeAndNil(Stream)
  finally
      Stream.Free;
      fSQLText.EndUpdate;
  end;
End;
{______________________________________________________________________________}
procedure TFXCustomSQL.SQLReplaceString(const OldValue: string; const NewValue: string);
Var i:Integer;
    s:String;
Begin
  CheckClosed;
  fSQLText.BeginUpdate;
  try
      for i:=0 to Pred(fSQLText.Count) do Begin
        s:=fSQLText[i].Replace(OldValue,NewValue);
        fSQLText[i]:=s
        end;

  finally
      fSQLText.EndUpdate;
  end;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXCustomSQL.FindField(Const FieldName: String): TFXSQLVAR;
Begin
  Result:=fSQLFields.GetByName(FieldName);
end;
{______________________________________________________________________________}
function TFXCustomSQL.FieldByName(Const FieldName: String): TFXSQLVAR;
var i: Integer;
begin
  i := GetFieldIndex(FieldName);
  if (i < 0) then
    FXRaiseClientErrorFmt(Self,fxceFieldNotFound, [FieldName]);
  result := GetFields(i);
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetFields(const Idx: Integer): TFXSQLVAR;
begin
  if (Idx < 0) or (Idx >= fSQLFields.Count) then
    FXRaiseClientErrorFmt(Self,fxceFieldNotFound, [IntToStr(Idx)]);
  result := fSQLFields[Idx];
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetFieldIndex(Const FieldName: String): Integer;
Var f:TFXSQLVAR;
begin
  f:=fSQLFields.GetByName(FieldName);
  if f=nil then
    result := -1 else
    result := f.Index;
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetSQLParams: TFXSQLDAParams;
begin
  if not Self.Prepared then
    Self.Prepare;
  result := fSQLParams;
end;
{______________________________________________________________________________}
function TFXCustomSQL.ParamByName(Idx: String): TFXSQLVAR;
begin
  if not Prepared then
    Prepare;
  result := fSQLParams.ByName(Idx);
end;
{______________________________________________________________________________}
function TFXCustomSQL.ParamBy(Idx: String): TFXSQLVAR;
begin
  if not Prepared then
    Prepare;
  result := fSQLParams.GetByName(Idx)
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetUniqueRelationName: String;
begin
  if Self.Prepared then
    result := fSQLFields.RelationName else
    result := '';
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetSchema: TFXCustomSchema;
Begin
  if Self.Database<>nil then Begin
    Result:=Self.Database.Schema
  end else Begin
    FXRaiseClientError(Self,fxceNotSupported);
    Result:=nil;
    End;
End;
{______________________________________________________________________________}
function TFXCustomSQL.Current: TFXSQLDAFields;
begin
  result := fSQLFields;
end;
{______________________________________________________________________________}
function TFXCustomSQL.Next: TFXSQLDAFields;
var fetch_res: ISC_STATUS;
    cl:IFXClientLib;
begin
  CheckCursorOpen;

  Case Self.QueryType of
    fxSQLInsertSelect,fxSQLUpdateSelect,fxSQLDeleteSelect:Begin
      if fRecordCount=0 then Begin
        cl:=Self.ClientLibrary;
        cl.Call_dsql_fetch(@fSQLHandle,fSQLFields.AsXSQLDA);
        result:=fSQLFields;
        Self.fEOF:=False;
        Inc(fRecordCount);
      end else Begin
        Self.fEOF:=True;
        Result:=nil;
      end end;
    else Begin
      if not fEOF then begin
        cl:=Self.ClientLibrary;
        fetch_res := cl.Call_dsql_fetch(@fSQLHandle,fSQLFields.AsXSQLDA);
        if (fetch_res = 100) or (cl.CheckStatusVector([isc_dsql_cursor_err])) then begin
          {$IFDEF mFXTRACE}Self.LogFetchInfo('SQL Fetch EOF : RecordCount : '+IntToStr(fRecordCount));{$ENDIF}
          fEOF:=True;
          Result:=nil;
        end else
        if (fetch_res > 0) then begin
          ReadLastErrorCloseAndRaise('SQL Fetch');
          result:=nil;
        end else begin
          Inc(fRecordCount);
          result:=fSQLFields;
          end
      end else
        result:=nil;
    end end;
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.SetPrepared(Const Value:Boolean);
Begin
  if Value then
    Self.Prepare else
    Self.CloseQuery
End;
{______________________________________________________________________________}
procedure TFXCustomSQL.PrepareInputParams;
Var cl:IFXClientLib;
    Err:ISC_STATUS;
Begin
  // We already know how many inputs there are, so...
  if (fSQLParams.fSQLDA <> nil) Then Begin
    cl:=Self.ClientLibrary;
    Err:=cl.Call_dsql_describe_bind(@fSQLHandle,fSQLParams.fSQLDA);
    if Err>0 Then
      ReadLastErrorCloseAndRaise('Bind Params');
    fSQLParams.DSQLDescribe;
    end;
End;
{______________________________________________________________________________}
procedure TFXCustomSQL.PrepareOutputFields;
Var cl:IFXClientLib;
    Err:ISC_STATUS;
Begin
  // Allocate an initial output descriptor.
  // Using isc_dsql_describe, get the right size for the columns...
  if fSQLFields.Count=0 then
    fSQLFields.Count := fDefaultFieldsCount;
  cl:=Self.ClientLibrary;
  Err:=cl.Call_dsql_describe(@fSQLHandle,fSQLFields.fSQLDA);
  if Err>0 Then
    ReadLastErrorCloseAndRaise('Describe Fields');
  if fSQLFields.fSQLDA^.sqld > fSQLFields.fSQLDA^.sqln then begin
    fSQLFields.Count := fSQLFields.fSQLDA^.sqld;
    Err:=cl.Call_dsql_describe(@fSQLHandle,fSQLFields.fSQLDA);
    if Err>0 Then
      ReadLastErrorCloseAndRaise('Describe Fields');
  end else
    fSQLFields.Count := fSQLFields.fSQLDA^.sqld;
  fSQLFields.DSQLDescribe;
End;
{______________________________________________________________________________}
procedure TFXCustomSQL.Prepare;
var res_buffer: array[0..7] of Byte;
    cl:IFXClientLib;
    statement:TBytes;
    stmt_len:FXShort;
    err:ISC_STATUS;
    type_item:Byte;
    Query:String;
    p:Integer;
begin
  if Not fPrepared then Begin
    CheckDatabaseConnected;
    Self.CheckInTransaction;
    Assert(fSQLHandle=nil);
    Assert(not fCursorOpen);
    Assert(fSQLFields.Count=0);
    Assert(fSQLParams.Count=0);
    Assert(fQueryType=fxSQLUnknown);

    Query:=fSQLText.Text;
    {$IFDEF mFXTrace}Self.LogPrepare;{$ENDIF}
    {$IFDEF mFXTrace}Self.LogQuery(Query);{$ENDIF}
    if Self.ParamCheck then Begin
      if fSQLParser=nil then
        fSQLParser:=TFXSQLParamsParser.Create;
      fSQLParser.ExtractParams(Query);
      fSQLParams.DSQLPreprocess(fSQLParser.ParamsNames);
      Query:=fSQLParser.Query;
      fSQLParser.Clear;
      end;

    cl:=Self.ClientLibrary;
    err:=cl.Call_dsql_alloc_statement2(DBHandle,@fSQLHandle);
    if Err>0 Then Begin
      {$IFDEF mFXTrace}Self.Log(fxtSQLPrepareError,'Alloc Statement');{$ENDIF}
      ReadLastErrorCloseAndRaise('Alloc Statement');
      end;
    Self.EncodeQueryEx(Query,statement);
    err:=cl.Call_dsql_prepare(@Transaction.Handle,@fSQLHandle,statement);
    if Err>0 Then Begin
      {$IFDEF mFXTrace}Self.Log(fxtSQLPrepareError,'Prepare Query');{$ENDIF}
      ReadLastErrorCloseAndRaise('Prepare Query');
      end;
    type_item:=isc_info_sql_stmt_type;
    err:=cl.Call_dsql_sql_info(@fSQLHandle, 1, @type_item, SizeOf(res_buffer),@res_buffer);
    if Err>0 Then Begin
      {$IFDEF mFXTrace}Self.Log(fxtSQLPrepareError,'Query Info');{$ENDIF}
      ReadLastErrorCloseAndRaise('Query Info');
      end;
    if (res_buffer[0] <> isc_info_sql_stmt_type) then Begin
      {$IFDEF mFXTrace}Self.Log(fxtSQLPrepareError,'SQL_INFO Error');{$ENDIF}
      ReadLastErrorCloseAndRaise(fxceUnknownException,['SQL_INFO Error']);
      end;
    stmt_len:=mFX.Header.vax_Integer(@res_buffer,1,2);
    fQueryType:=TFXQueryType(mFX.Header.vax_Integer(@res_buffer,3,stmt_len));
    // Ask Input/Output getting the type
    case fQueryType of
      fxSQLInsertSelect,fxSQLUpdateSelect,fxSQLDeleteSelect,
      fxSQLExecProcedure:Begin
        Query:=UpperCase(Query);
        p:=Pos('RETURNING',Query);
        if p>0 then Begin
          if Pos('DELETE',Query)>0 then fQueryType:=fxSQLDeleteSelect else
          if Pos('UPDATE',Query)>0 then fQueryType:=fxSQLUpdateSelect else
          if Pos('INSERT',Query)>0 then fQueryType:=fxSQLInsertSelect
        end end end;
    // Ask Input/Output getting the type
    case fQueryType of
      fxSQLInsertSelect,fxSQLUpdateSelect,fxSQLDeleteSelect,
      fxSQLSelectForUpdate,fxSQLExecProcedure,
      fxSQLSelectSelect:Begin
        PrepareInputParams;
        PrepareOutputFields;
        fPrepared:=True;
        end;
      fxSQLInsert, fxSQLUpdate, fxSQLDelete,
      fxSQLDDLComment, fxSQLDDLStatement,
      fxSQLSetGenerator:begin
        // NO output
        Assert(fSQLFields.Count=0);
        PrepareInputParams;
        fPrepared:=True;
        end;
      fxSQLCommit:Begin
        // Just Commit
        Assert(fSQLFields.Count=0);
        Assert(fSQLParams.Count=0);
        fPrepared:=True;
        end;
      else begin
        {$IFDEF mFXTrace}Self.Log(fxtSQLPrepareError,'Unsupported QueryType');{$ENDIF}
        CloseAndRaiseClientErrorFmt(fxceUnsupportedQueryType,[Integer(fQueryType)]);
      end end;
    // Just Trace
    {$IFDEF mFXTrace}Self.LogPlan;{$ENDIF}
    end
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.OpenCursor;
var cl:IFXClientLib;
begin
  CheckCursorClosed;
  CheckDatabaseConnected;
  Self.CheckInTransaction;
  if not Self.Prepared then
    Self.Prepare;

  {$IFDEF mFXTrace}Self.LogParams;{$ENDIF}
  {$IFDEF mFXTrace}Self.LogMiscInfo('Open Cursor');{$ENDIF}

  cl:=Self.ClientLibrary;
  if fSQLFields.Count=0 Then Begin
    // SQLCommit, SQLRollback,....
    FXRaiseClientErrorMsg(Self,fxceUnknownException,'Not a Cursor !');
    end;

  cl.Check_dsql_execute2(Self.TRHandle,@fSQLHandle,fSQLParams.AsXSQLDA);
  cl.Check_dsql_set_cursor_name(@fSQLHandle, PAnsiChar(fCursor));
  Self.fCursorOpen:= True;
end;
{______________________________________________________________________________}
function TFXCustomSQL.TryExecQuery(Const aQuery:String):Boolean;
Begin
  Result:=TryExecQuery(aQuery,False);
End;
{______________________________________________________________________________}
function TFXCustomSQL.TryExecQuery(Const aQuery:String;Const DoRestart:Boolean):Boolean;
Begin
  if DoRestart then
    Self.ReStart_TR;
  Self.SQL.Text:=aQuery;
  if Self.TryExecQuery then Begin
    if fCursorOpen then Begin
      While not Self.Eof do Begin
        Self.Next;
        end;
      Self.CloseCursor;
      end;
    Result:=True;
  End else
    Result:=False;
  Self.CloseQuery;
End;
{______________________________________________________________________________}
function TFXCustomSQL.TryExecQuery:Boolean;
var cl:IFXClientLib;
    err:ISC_STATUS;
begin
  CheckCursorClosed;
  CheckDatabaseConnected;
  Self.CheckInTransaction;
  if not Self.Prepared then
    Self.Prepare;

  cl:=Self.ClientLibrary;
  if fSQLFields.Count>0 Then Begin
    {$IFDEF mFXTrace}Self.LogParams;{$ENDIF}
    {$IFDEF mFXTrace}Self.LogMiscInfo('Open Cursor');{$ENDIF}
    Err:=cl.Call_dsql_execute2(Self.TRHandle,@fSQLHandle,fSQLParams.AsXSQLDA);
    if Err<=0 Then Begin
      cl.Check_dsql_set_cursor_name(@fSQLHandle, PAnsiChar(fCursor));
      Self.fCursorOpen:= True;
      Case Self.QueryType of
        fxSQLInsertSelect,fxSQLUpdateSelect,fxSQLDeleteSelect:Begin
          cl.Call_dsql_fetch(@fSQLHandle,fSQLFields.AsXSQLDA);
          Self.fEOF:=False;
          Inc(fRecordCount);
          end;
        else Begin
          Self.Next;
        end end;
      Result:=True;
    End else Begin
      Self.PushError;
      Result:=False;
      end;
  end else Begin
    // SQLCommit, SQLRollback,....
    {$IFDEF mFXTrace}Self.LogParams;{$ENDIF}
    {$IFDEF mFXTrace}Self.LogMiscInfo('Execute Query');{$ENDIF}
    Err:=cl.Call_dsql_execute(TRHandle,@fSQLHandle,fSQLParams.AsXSQLDA);
    if Err<=0 Then Begin
      {$IFDEF mFXTrace}Self.LogRowsAffected;{$ENDIF}
      Result:=True;
    End else Begin
      Self.PushError;
      Result:=False;
    end end;
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.ExecQuery;
var cl:IFXClientLib;
begin
  CheckCursorClosed;
  CheckDatabaseConnected;
  Self.CheckInTransaction;
  if not Self.Prepared then
    Self.Prepare;

  cl:=Self.ClientLibrary;
  if fSQLFields.Count>0 Then Begin
    {$IFDEF mFXTrace}Self.LogParams;{$ENDIF}
    {$IFDEF mFXTrace}Self.Log(fxtSQLExecQuery,'Open Cursor');{$ENDIF}
    cl.Check_dsql_execute2(Self.TRHandle,@fSQLHandle,fSQLParams.AsXSQLDA);
    cl.Check_dsql_set_cursor_name(@fSQLHandle, PAnsiChar(fCursor));
    Self.fCursorOpen:= True;
    Case Self.QueryType of
      fxSQLInsertSelect,fxSQLUpdateSelect,fxSQLDeleteSelect:Begin
        cl.Call_dsql_fetch(@fSQLHandle,fSQLFields.AsXSQLDA);
        Self.fEOF:=False;
        Inc(fRecordCount);
        end;
      else Begin
        Self.Next;
      end end;
  end else Begin
    // SQLCommit, SQLRollback,....
    {$IFDEF mFXTrace}Self.LogParams;{$ENDIF}
    {$IFDEF mFXTrace}Self.Log(fxtSQLExecQuery,'Execute Query');{$ENDIF}
    cl.Check_dsql_execute(TRHandle,@fSQLHandle,fSQLParams.AsXSQLDA);
    {$IFDEF mFXTrace}Self.LogRowsAffected;{$ENDIF}
    end;
end;
{______________________________________________________________________________}
function TFXCustomSQL.InternalCloseCursor:Boolean;
var isc_res:ISC_STATUS;
begin
  if fSQLHandle<>nil then Begin
    if fCursorOpen then begin
      {$IFDEF mFXTrace}Self.Log(fxtSQLCloseCursor,'Close Cursor');{$ENDIF}
      isc_res:=Self.ClientLibrary.Call_dsql_free_statement(@fSQLHandle,DSQL_close);
      if isc_res>0 then Begin
        Self.PushError;
        Result:=False
      end else
        Result:=True;
      fEOF:=False;
      fRecordCount:=0;
      fCursorOpen:=False;
      if fSQLParser<>nil then
        fSQLParser.Clear;
    end else
      Result:=True
  end else
    Result:=True
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.CloseCursor;
Begin
  if not InternalCloseCursor then
    Self.RaiseLastError;
End;
{______________________________________________________________________________}
function TFXCustomSQL.InternalClose:Boolean;
var isc_res: ISC_STATUS;
Begin
  if fSQLHandle<>nil then Begin
    try // Close Cursor ................
        Result:=True;
        if fCursorOpen then begin
          {$IFDEF mFXTrace}Self.Log(fxtSQLCloseCursor,'Close Cursor');{$ENDIF}
          isc_res := Self.ClientLibrary.Call_dsql_free_statement(@fSQLHandle,DSQL_close);
          if isc_res>0 then Begin
            Self.PushError;
            Result:=False;
          end end;
        // Free Handle ................
        {$IFDEF mFXTrace}Self.Log(fxtSQLCloseQuery,'Close Statement');{$ENDIF}
        isc_res := Self.ClientLibrary.Call_dsql_free_statement(@fSQLHandle,DSQL_drop);
        if isc_res>0 then Begin
          Self.PushError;
          Result:=False;
          end;
        Self.UnPrepare;
    except on e:exception do Begin
        {$IFDEF mFXTrace}Self.LogError('Internal Close',e);{$ENDIF}
        PushError('Internal Close',e);
        Self.UnPrepare;
        Result:=False;
    end end
  end else
    Result:=True;
End;
{______________________________________________________________________________}
function TFXCustomSQL.CloseQuery(Const RaiseOnError:Boolean=True):Boolean;
begin
  Result:=InternalClose;
  if not Result then
    if RaiseOnError then
      RaiseLastError;
end;
{______________________________________________________________________________}
procedure TFXCustomSQL.BeforeTransactionEndEvent(Const aTR: TFXCustomTransaction;Const Action:TFXTransactionAction;Const Silent:Boolean);
begin
  case Action of
    fxTARollbackRetaining,
    fxTACommitRetaining:Begin
      // Do not Close
      end;
    else Begin
      // Close Cursor but keep prepared !!
      if not Self.InternalCloseCursor then
        if not Silent then
          Self.RaiseLastError;
    end end;
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetPlan: String;
var result_buffer: array[0..16384] of Byte;
    result_length, i: Integer;
    info_request: AnsiChar;
    cl:IFXClientLib;
    err:ISC_STATUS;
begin
  if (Self.Prepared) then Begin
    cl:=Self.ClientLibrary;
    info_request := Char(isc_info_sql_get_plan);
    Err:=cl.Call_dsql_sql_info(@fSQLHandle, 2, @info_request, SizeOf(result_buffer),@result_buffer);
    if (Err<=0)and(result_buffer[0]=isc_info_sql_get_plan) then Begin
      result_length := mFX.Header.vax_Integer(@result_buffer[1], 2);
      SetLength(Result,result_length);
      for i := 1 to result_length do
        Result[i]:=Char(AnsiChar(result_buffer[i + 2]));
      Result:=Trim(Result);
    end else
      Result:='';
  end else
    Result:='';
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetRecordCount: Integer;
begin
  result := fRecordCount;
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetRowsAffected: Integer;
var result_buffer: array[0..1048] of Byte;
    info_request: AnsiChar;
    cl:IFXClientLib;
begin
  if Self.Prepared then Begin
    cl:=Self.ClientLibrary;
    info_request := AnsiChar(isc_info_sql_records);
    cl.Check_dsql_sql_info(@fSQLHandle, 1, @info_request, SizeOf(result_buffer),@result_buffer);
    if (result_buffer[0]=isc_info_sql_records) then Begin
      Result:=   mFX.Header.vax_Integer(@result_buffer, 6, 4) ;
      Inc(Result,mFX.Header.vax_Integer(@result_buffer,13, 4));
      Inc(Result,mFX.Header.vax_Integer(@result_buffer,27, 4));
    end else
      result := -1;
  end else
    result := -1;
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetRowsUpdated: Integer;
var result_buffer: array[0..1048] of Byte;
    info_request:AnsiChar;
    cl:IFXClientLib;
begin
  if Self.Prepared then Begin
    cl:=Self.ClientLibrary;
    info_request := AnsiChar(isc_info_sql_records);
    cl.Check_dsql_sql_info(@fSQLHandle, 1, @info_request, SizeOf(result_buffer),@result_buffer);
    if (result_buffer[0]=isc_info_sql_records) then Begin
      Result:=mFX.Header.vax_Integer(@result_buffer, 6, 4);
    end else
      result := -1;
  end else
    result := -1;
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetRowsDeleted: Integer;
var result_buffer: array[0..1048] of Byte;
    info_request:AnsiChar;
    cl:IFXClientLib;
begin
  if Self.Prepared then Begin
    cl:=Self.ClientLibrary;
    info_request := AnsiChar(isc_info_sql_records);
    cl.Check_dsql_sql_info(@fSQLHandle, 1, @info_request, SizeOf(result_buffer),@result_buffer);
    if (result_buffer[0]=isc_info_sql_records) then Begin
      Result:=mFX.Header.vax_Integer(@result_buffer,13, 4);
    end else
      result := -1;
  end else
    result := -1;
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetRowsInserted: Integer;
var result_buffer: array[0..1048] of Byte;
    info_request:AnsiChar;
    cl:IFXClientLib;
begin
  if Self.Prepared then Begin
    cl:=Self.ClientLibrary;
    info_request := AnsiChar(isc_info_sql_records);
    cl.Check_dsql_sql_info(@fSQLHandle, 1, @info_request, SizeOf(result_buffer),@result_buffer);
    if (result_buffer[0]=isc_info_sql_records) then Begin
      Result:=mFX.Header.vax_Integer(@result_buffer,27, 4);
    end else
      result := -1;
  end else
    result := -1;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXCustomSQL.GetServeurDate:TDateTime;
Var DidStart:Boolean;
Begin
  Assert(Not CursorOpen);
  DidStart:=Self.Start_TR;
  Self.SQL.Text:='Select current_date From RDB$DATABASE';
  Self.DefaultFieldsCount:=1;
  Self.ParamCheck:=False;
  Self.ExecQuery;
  Result:=Self.Fields[0].AsDate;
  Self.CloseQuery;
  //Commit iff
  if DidStart then
    Self.Commit_TR;
end;
{______________________________________________________________________________}
function TFXCustomSQL.GetServeurDateTime:TDateTime;
Var DidStart:Boolean;
Begin
  Assert(Not CursorOpen);
  DidStart:=Self.Start_TR;
  Self.SQL.Text:='Select current_timestamp From RDB$DATABASE';
  Self.DefaultFieldsCount:=1;
  Self.ParamCheck:=False;
  Self.ExecQuery;
  Result:=Self.Fields[0].AsDateTime;
  Self.CloseQuery;
  //Commit iff
  if DidStart then
    Self.Commit_TR;
end;

end.

