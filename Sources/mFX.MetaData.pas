unit mFX.MetaData;

interface

{$I mFX.Inc}
{$M-}

uses System.Types, System.Classes, System.SysUtils,
  mFX.Header, mFX.Intf, mFX.Consts, mFX.Classes, mFX.List,
  mFX.Base, mFX.SQL;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  TFXMetaTrace = procedure(Const Msg:String) of object;

  TFXCustomMetaNode = Class;
  TFXCustomMetaField = Class;
  TFXCustomMetaRelation = Class;

  TFXCustomMetaSchema = Class;
    TFXMetaRole = Class;
    TFXMetaCharSet = Class;
    TFXMetaException = Class;
    TFXMetaDomain = Class;
    TFXMetaProcedure = Class;
    TFXMetaUDF = Class;

    TFXMetaView = Class;
    TFXMetaTable = Class;
      TFXMetaTableField = Class;
      TFXMetaCheckConstraint = Class;
      TFXMetaTrigger = Class;
      TFXMetaPrimaryKey = Class;
      TFXMetaForeignKey = Class;
      TFXMetaIndex = Class;

  ///	<summary>
  ///	  Meta Object Type
  ///	</summary>
  ///	<remarks>
  ///	  Object types as used in RDB$DEPENDENCIES and RDB$USER_PRIVILEGES
  ///	</remarks>
  TFXMetaObject = (
    moTable,

    moView,

    moTrigger,

    moComputed,

    moValidation,

    moProcedure,

    moExprIndex,

    moException,

    moUser,

    moField,

    moIndex,

    moPrimaryKey,

    moUnqConstraint,

    moForeignKey,

    moGenerator,

    moUDF,

    moCheckConstraint,

    moCharSet,

    moDomain,

    moProcInParam,

    moProcOutParam,

    moUDFInParam,

    moRole,

    moSchema,

    moUndefined

  );
  TFXMetaObjects = Set Of TFXMetaObject;

  TFXFieldType = (
    uftUnKnown,
    uftNumeric,
    uftChar,
    uftVarchar,
    uftCString,
    uftSmallint,
    uftInteger,
    uftQuad,
    uftFloat,
    uftDoublePrecision,
    uftTimestamp,
    uftBlobData,
    uftBlobArray,
    uftBlobId,
    uftDate,
    uftTime,
    uftInt64,
    uftArray
    );

  TFXFieldScale = 1..15;

  TFXFieldFlag = (
    fiPrimary,
    fiForeign,
    fiIndice,
    fiUnique
    );
  TFXFieldFlags = set of TFXFieldFlag;

  TFXIndexOrder = (
    ioDescending,
    ioAscending
    );

  TFXForeignKeyRule = (
    urRestrict,
    urCascade,
    urSetNull,
    urNoAction,
    urSetDefault
    );

  TFXTriggerPrefix = (
    tpBefore,
    tpAfter
    );

  TFXTriggerSuffix = (
    tsInsert,
    tsUpdate,
    tsDelete
    );
  TFXTriggerSuffixes = set of TFXTriggerSuffix;

  ///<summary>Meta Data Node</summary>
  TFXCustomMetaNode = Class(TFXCustomSchemaNode)
  protected
    fSchema       : TFXCustomMetaSchema;
    fOwner        : TFXCustomMetaNode;
    fMetaObject   : TFXMetaObject;
    fName         : String;
    fDescr        : String;
    fUsing        : TList;
    fDependenciesX: TList;
    /// <summary>FullName</summary>
    function GetFullName:String;virtual;
    /// <summary>QuoteIdentifier</summary>
    function GetQuotedName:String;overload;
    /// <summary>QuoteIdentifier</summary>
    function GetQuotedName(Out Value:String):Boolean;overload;
    /// <summary>QuoteIdentifier</summary>
    class function DoQuoteName(Const Value:String):String;overload;

    /// <summary>Get Has Dependencies</summary>
    function GetHasDependencies:Boolean;
    /// <summary>Get Has Dependencies</summary>
    function GetHasDependedOn(Const aMetaObject:TFXMetaObject):Boolean;
    ///<summary>Download In/Out Params, ...</summary>
    procedure Assign2DependenciesQuery(Const aSQL:TFXCustomSQL);virtual;
    /// <summary>AddDependency:Add a MetaNode who depend from this table</summary>
    procedure AddDependency(Const aMetaNode:TFXCustomMetaNode);overload;
    /// <summary>AddDependency:Add a MetaNode who depend from this table</summary>
    procedure AddDependency(Const aMetaObject:TFXMetaObject;Const aName:String);overload;

    /// <summary>WriteDependencies</summary>
    procedure WriteTerminator(Const Stream: TStringStream);
    /// <summary>WriteDependencies</summary>
    procedure WriteDependencies(Const Stream: TStringStream);
    /// <summary>WriteComment</summary>
    procedure WriteComment(Const Stream: TStringStream;Const aComment:String);
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);virtual;abstract;
  public
    /// <summary>destructor</summary>
    destructor Destroy; override;

    /// <summary>Create DDL</summary>
    function AsDropDDL:String;virtual;
    ///<summary>Download In/Out Params, ...</summary>
    procedure DownloadDependencies(Const aSQL:TFXCustomSQL);virtual;

    property Schema         : TFXCustomMetaSchema read fSchema;
    property Parent         : TFXCustomMetaNode   read FOwner;
    property MetaObject     : TFXMetaObject       read fMetaObject;
    property Name           : String              read fName;
    property QuotedName     : String              read GetQuotedName;
    property FullName       : String              read GetFullName;
    property Descr          : String              read fDescr;

    property Dependencies   : TList               read fDependenciesX;
    property HasDependencies: Boolean             read GetHasDependencies;
    property HasDependedOn [Const aMetaObject:TFXMetaObject] : Boolean read GetHasDependedOn;
  end;

  /// <summary>TFXArrayBound</summary>
  TFXArrayBound = Class(TObject)
  protected
    fLowerBound : Integer;
    fHigherBound: Integer;
  end;

  /// <summary>TFXArrayBounds</summary>
  TFXArrayBounds = class(TFXList)
  protected
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aOwner:TFXCustomMetaField;Const aSQL:TFXCustomSQL);
  end;

  ///<summary>Base Class for TableField, Domain, ...</summary>
  TFXCustomMetaField = class(TFXCustomMetaNode)
  protected
    fFieldType     : TFXFieldType;
    fSubType       : Smallint;
    fScale         : Word;
    fLength        : Smallint;
    fPrecision     : Smallint;
    fFieldCharSet  : String;
    fSegmentLen    : Smallint;
    fBytesPerChar  : Integer;
    fIsNotNull     : Boolean;
    fHasDefault    : Boolean;
    fDefaultSrc    : String;
    fHasCheck      : Boolean;
    fCheckSrc      : String;
    fArrayBounds   : TFXArrayBounds;
    /// <summary>Get FieldType Name</summary>
    function GetFieldTypeDLL:String;
    /// <summary>Assign FieldType,... FromSQL</summary>
    procedure AssignFromSQL(Const aFieldTypeIdx:Integer;Const aSQL:TFXCustomSQL);
    /// <summary>Assign NullFlag FromSQL</summary>
    procedure AssignNullFlagFromSQL(Const aNullFlagIdx:Integer;Const aSQL:TFXCustomSQL);
    /// <summary>Assign Array Dimensions FromSQL</summary>
    procedure AssignArrayDimensionsFromSQL(Const aDimensionsIdx:Integer;Const aSQL1,aSQL2:TFXCustomSQL);
    /// <summary>Assign Blob Segment FromSQL</summary>
    procedure AssignBlobSegmentFromSQL(Const aSegLenIdx:Integer;Const aSQL:TFXCustomSQL);
    /// <summary>Assign Validation Check FromSQL</summary>
    procedure AssignCheckFromSQL(Const aValidationSourceIdx:Integer;Const aSQL:TFXCustomSQL);
    /// <summary>Assign Default Source FromSQL</summary>
    procedure AssignDefaultFromSQL(Const aDefaultSourceIdx:Integer;Const aSQL:TFXCustomSQL);
  public
    ///<summary>destructor</summary>
    destructor Destroy; override;

    property FieldName    : String         read fName;
    property IsNotNull    : Boolean        read fIsNotNull;
    property HasDefault   : Boolean        read fHasDefault;
    property DefaultSrc   : String         read fDefaultSrc;
    property HasCheck     : Boolean        read fHasCheck;
    property CheckSrc     : String         read fCheckSrc;
    property ArrayBounds  : TFXArrayBounds read fArrayBounds;
    property FieldTypeDLL : String         read GetFieldTypeDLL;
  end;

  /// <summary>TFXMetaDomain:</summary>
  TFXMetaDomain = class(TFXCustomMetaField)
  protected
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL1,aSQL2:TFXCustomSQL);
  public
    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;

  end;

  ///<summary>Field Def</summary>
  TFXMetaTableField = class(TFXCustomMetaField)
  protected
    fFlags         : TFXFieldFlags;
    fDomain        : String;
    fPrimaryKeys   : TList;
    fIsComputed    : Boolean;
    fComputedSrc   : String;
    ///<summary>Download Fields, Primary Keys, ...</summary>
    procedure Download;
    /// <summary>Is Field part of Primary or Unique Index</summary>
    function GetFieldInKey:Boolean;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aOwner:TFXCustomMetaRelation;Const aSQL1,aSQL2:TFXCustomSQL);
    ///<summary>destructor</summary>
    destructor Destroy; override;

    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aSQL:TFXCustomSQL);overload;inline;
    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;

    property Flags        : TFXFieldFlags  read fFlags;
    property Domain       : String         read fDomain;
    property FieldInKey   : Boolean        read GetFieldInKey;
    property IsComputed   : Boolean        read fIsComputed;
    property ComputedSrc  : String         read fComputedSrc;
  end;

  /// <summary>TFXCustomTableMetaNode:</summary>
  TFXCustomRelationMetaNode = class(TFXCustomMetaNode)
  protected
    fRelation: String;
    /// <summary>Get RelationName</summary>
    function GetRelationName:String;inline;
    /// <summary>Get Quoted RelationName</summary>
    function GetQuotedRelationName:String;inline;
  public
    property RelationName      : String    read GetRelationName;
    property QuotedRelationName: String    read GetQuotedRelationName;
  end;

  ///<summary>Primary or Unique Index</summary>
  TFXMetaCustomIndex = class(TFXCustomRelationMetaNode)
  protected
    fIndexName : String;
    fFields    : TList;
    ///<summary>Getter...</summary>
    function GetQuotedIndexName: String;
    ///<summary>Add Fields...</summary>
    procedure AddField(Const aField:TFXMetaTableField;Const aSQL:TFXCustomSQL);virtual;abstract;
  public
    ///<summary>Download Fields, Primary Keys, ...</summary>
    procedure Download(Const aSQL:TFXCustomSQL);

    /// <summary>Create DDL</summary>
    /// SET STATISTICS INDEX IDX_XDRS_01_CALLNR
    function AsRecomputeDDL:String;

    property Relation       : TFXCustomMetaNode read fOwner;
    property IndexName      : String            read fIndexName;
    property QuotedIndexName: String            read GetQuotedIndexName;
  end;

  /// <summary>TFXMetaForeignKey:</summary>
  TFXMetaForeignKey = class(TFXMetaCustomIndex)
  protected
    fFKName    : String;
    fFKRelation: TFXMetaTable;
    fFKOnDelete: TFXForeignKeyRule;
    fFKOnUpdate: TFXForeignKeyRule;
    fFKFields  : TList;
    ///<summary>Add Fields...</summary>
    procedure AddField(Const aField:TFXMetaTableField;Const aSQL:TFXCustomSQL);override;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
    ///<summary>Download In/Out Params, ...</summary>
    procedure Assign2DependenciesQuery(Const aSQL:TFXCustomSQL);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aOwner:TFXMetaTable;Const aSQL:TFXCustomSQL);
    /// <summary>destructor</summary>
    destructor Destroy; override;

    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aSQL:TFXCustomSQL);overload;inline;
    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aName:String;Const aSQL:TFXCustomSQL);overload;inline;

    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;

    property FKName     : String            read fFKName;
    property FKRelation : TFXMetaTable      read fFKRelation;
    property FKOnDelete : TFXForeignKeyRule read fFKOnDelete;
    property FKOnUpdate : TFXForeignKeyRule read fFKOnUpdate;
  end;

  ///<summary>Primary or Unique Index</summary>
  TFXMetaPrimaryKey = class(TFXMetaCustomIndex)
  protected
    fIsPrimary     : Boolean;
    ///<summary>Add Fields...</summary>
    procedure AddField(Const aField:TFXMetaTableField;Const aSQL:TFXCustomSQL);override;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aOwner:TFXMetaTable;Const aSQL:TFXCustomSQL);
    ///<summary>destructor</summary>
    destructor Destroy; override;

    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const IncludeSysTable:Boolean;Const aSQL:TFXCustomSQL);overload;inline;
    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aName:String;Const aSQL:TFXCustomSQL);overload;inline;
    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;

    property IsPrimary  : Boolean           read fIsPrimary;
  end;

  /// <summary>TFXMetaIndex:</summary>
  TFXMetaIndex = class(TFXMetaCustomIndex)
  protected
    fActive    : Boolean;
    fUnique    : Boolean;
    fOrder     : TFXIndexOrder;
    ///<summary>Add Fields...</summary>
    procedure AddField(Const aField:TFXMetaTableField;Const aSQL:TFXCustomSQL);override;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aOwner:TFXMetaTable;Const aSQL:TFXCustomSQL);
    /// <summary>destructor</summary>
    destructor Destroy; override;

    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aSQL:TFXCustomSQL);overload;inline;
    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aName:String;Const aSQL:TFXCustomSQL);overload;inline;

    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;
    /// <summary>Create DDL</summary>
    /// SET STATISTICS INDEX IDX_XDRS_01_CALLNR
    function AsOff:String;
    /// <summary>Create DDL</summary>
    /// SET STATISTICS INDEX IDX_XDRS_01_CALLNR
    function AsOn:String;
  end;

  /// <summary>TFXMetaTrigger:</summary>
  TFXMetaCheckConstraint = class(TFXCustomRelationMetaNode)
  protected
    fSource  : String;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aOwner:TFXCustomMetaRelation;Const aSQL:TFXCustomSQL);

    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aSQL:TFXCustomSQL);overload;inline;

    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;
  end;

  /// <summary>TFXMetaTrigger:</summary>
  TFXMetaTrigger = class(TFXCustomRelationMetaNode)
  protected
    fPrefix  : TFXTriggerPrefix;
    fSuffix  : TFXTriggerSuffixes;
    fPosition: Smallint;
    fActive  : Boolean;
    fSource  : String;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aOwner:TFXCustomMetaRelation;Const aSQL:TFXCustomSQL);

    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aSQL:TFXCustomSQL);overload;inline;

    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;
    /// <summary>AsSetActive</summary>
    function SetActiveDDL:String;
    /// <summary>AsSetInactive</summary>
    function SetInactiveDDL:String;

    property Active   : Boolean     read fActive;
  end;

  ///<summary>Relation Def</summary>
  TFXCustomMetaRelation = class(TFXCustomMetaNode)
  protected
    fFields          : TFXList;
    fTriggers        : TFXList;
    fCheckConstraints: TFXList;
  public
    ///<summary>destructor</summary>
    destructor Destroy; override;

    ///<summary>Find Relation.Field Schema</summary>
    function FindField(Const aName : String) : TFXMetaTableField;
    ///<summary>Build Select All True Fields from Table</summary>
    function SelectStart(Const aAlias:String;Const aFilter:TFunc<TFXMetaTableField,Boolean>):String;

    property Fields       : TFXList  read fFields;
  end;

  ///<summary>Relation Def</summary>
  TFXMetaMissingRelation = class(TFXCustomMetaRelation)
  protected
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    ///<summary>Find Relation.Field Schema</summary>
    constructor CreateMissingRelation(Const aSchema:TFXCustomMetaSchema;Const aName:String);
  end;

  ///<summary>Relation Def</summary>
  TFXMetaTable = class(TFXCustomMetaRelation)
  protected
    fPrimaryKeys   : TFXList;
    fForeignKeys   : TFXList;
    fIndexes       : TFXList;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);
    ///<summary>destructor</summary>
    destructor Destroy; override;

    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aSQL:TFXCustomSQL);overload;inline;
    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aName:String;Const aSQL:TFXCustomSQL);overload;
    ///<summary>Download Fields, Primary Keys, ...</summary>
    procedure Download(Const aSQL1,aSQL2:TFXCustomSQL);
    /// <summary>Order ByPrimaryKeys</summary>
    function HasPrimaryKeys:Boolean;
    /// <summary>Order ByPrimaryKeys</summary>
    function OrderByPrimaryKeys:String;

    ///<summary>Download Fields, Primary Keys, ...</summary>
    procedure ClearForeignKeys;
    ///<summary>Download Fields, Primary Keys, ...</summary>
    procedure DownloadForeignKeys(Const aSQL1,aSQL2:TFXCustomSQL);

    ///<summary>Download Fields, Primary Keys, ...</summary>
    procedure ClearIndexes;
    ///<summary>Download Fields, Primary Keys, ...</summary>
    procedure DownloadIndexes(Const aSQL1,aSQL2:TFXCustomSQL);

    ///<summary>Download In/Out Params, ...</summary>
    procedure IndexesOff(Const aSQL:TFXCustomSQL;Const OnTrace:TFXMetaTrace);
    ///<summary>Download In/Out Params, ...</summary>
    procedure IndexesOn(Const aSQL:TFXCustomSQL;Const OnTrace:TFXMetaTrace);
    ///<summary>Download In/Out Params, ...</summary>
    procedure RecomputeIndexes(Const aSQL:TFXCustomSQL;Const OnTrace:TFXMetaTrace);

    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;

    property RelationName : String   read fName;
  end;

  /// <summary>TFXMetaView:</summary>
  TFXMetaView = class(TFXCustomMetaRelation)
  protected
    fSource        : String;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);

    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aSQL:TFXCustomSQL);overload;inline;
    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aName:String;Const aSQL:TFXCustomSQL);overload;
    ///<summary>Download Fields, ...</summary>
    procedure Download(Const aSQL1,aSQL2:TFXCustomSQL);
    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;
  end;

  /// <summary>TFXMetaProcInField:</summary>
  TFXMetaProcInField = class(TFXCustomMetaField)
  protected
    fDomain        : String;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aOwner:TFXMetaProcedure;Const aSQL:TFXCustomSQL);
  end;

  /// <summary>TFXMetaProcOutField:</summary>
  TFXMetaProcOutField = class(TFXCustomMetaField)
  protected
    fDomain        : String;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aOwner:TFXMetaProcedure;Const aSQL:TFXCustomSQL);
  end;

  /// <summary>TFXMetaProcedure:</summary>
  TFXMetaProcedure = class(TFXCustomMetaNode)
  protected
    FSource       : String;
    fInputParams  : TFXList;
    fOutputParams : TFXList;
    /// <summary>HeaderToDDL</summary>
    procedure WriteHeaderDDL(Const Stream: TStringStream);
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);
    /// <summary>destructor</summary>
    destructor Destroy; override;

    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aSQL:TFXCustomSQL);overload;inline;
    ///<summary>Download In/Out Params, ...</summary>
    procedure DownloadParams(Const aSQL:TFXCustomSQL);

    /// <summary>AsDummyAlter</summary>
    function AsDummyAlterDDL:String;
    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;
  end;

  /// <summary>Meta UDF InField</summary>
  TFXMetaUDFInField = class(TFXCustomMetaField)
  protected
    fPosition      : Integer;
    fMechanism     : Integer;
    fNull          : Boolean;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aUDF:TFXMetaUDF;Const aSQL:TFXCustomSQL);
  end;

  ///<summary>UDF Def</summary>
  TFXMetaUDF = class(TFXCustomMetaNode)
  protected
    fModule        : String;
    fEntry         : String;
    fReturn        : Integer;
    fInputParams   : TFXList;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);
    ///<summary>destructor</summary>
    destructor Destroy; override;

    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aSQL:TFXCustomSQL);overload;inline;

    ///<summary>Download In/Out Params, ...</summary>
    procedure DownloadParams(Const aSQL:TFXCustomSQL);

    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;
  end;

  /// <summary>TFXMetaException</summary>
  TFXMetaException = class(TFXCustomMetaNode)
  protected
    fMessage     : String;
    fNumber      : Integer;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);

    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aSQL:TFXCustomSQL);overload;inline;

    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;
  end;

  /// <summary>TFXMetaRole:</summary>
  TFXMetaGenerator = class(TFXCustomMetaNode)
  protected
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);
    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aSQL:TFXCustomSQL);overload;inline;
  end;

  /// <summary>TFXMetaRole:</summary>
  TFXMetaRole = class(TFXCustomMetaNode)
  protected
    fOwnerName : String;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);

    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aSQL:TFXCustomSQL);overload;inline;
    /// <summary>Create DDL</summary>
    Class procedure PrepareQuery(Const aName : String;Const aSQL:TFXCustomSQL);overload;inline;

    /// <summary>IsSYSDBA</summary>
    function IsADMIN:Boolean;
    /// <summary>Create DDL</summary>
    function AsDropDDL:String;override;
  end;

  ///<summary>CharSet Def</summary>
  TFXMetaCharSet = class(TFXCustomMetaNode)
  protected
    fID            : Integer;
    fBytesPerChar  : Integer;
    /// <summary>Create DDL</summary>
    procedure WriteCreateDDL(Const Stream: TStringStream);override;
    /// <summary>constructor</summary>
    constructor CreateNone(Const aSchema:TFXCustomMetaSchema);
  public
    /// <summary>constructor</summary>
    constructor CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);

    property CharSet : String read fName;
  end;

  ///<summary>Database Def</summary>
  TFXCustomMetaSchema = class (TFXCustomSchema)
  protected
    fLoaded         : Boolean;
    fSequencesLoaded: Boolean;
    fSQL1           : TFXCustomSQL;
    fSQL2           : TFXCustomSQL;
    fSQL3           : TFXCustomSQL;
    fSQLCharSet     : TFXCustomSQL;
    ///<summary>Prepare Query</summary>
    function GetSQL1:TFXCustomSQL;
    ///<summary>Prepare Query</summary>
    function GetSQL2:TFXCustomSQL;
    ///<summary>Prepare Query</summary>
    function GetSQL3:TFXCustomSQL;
    /// <summary>GetCharSet</summary>
    function GetCharSet(Const aID:Integer):TFXMetaCharSet;

    ///<summary>Set Tr</summary>
    procedure SetTr(Const aTRx:TFXCustomTransaction);
  public
    ///<summary>destructor</summary>
    destructor Destroy; override;

    ///<summary>Clear all Relation.Field Schema</summary>
    procedure Clear;override;
    ///<summary>Clear all Relation.Field Schema</summary>
    procedure Download(Const aTR:TFXCustomTransaction;Const ForceReload:Boolean);

    /// <summary>GetTableNames</summary>
    procedure TableNames(Const aList: TStrings);
    /// <summary>FieldNames</summary>
    procedure FieldNames(Const aList: TStrings);

    ///<summary>Find Node</summary>
    function FindNode(Const aMetaObject:TFXMetaObject;Const aName:String):TFXCustomMetaNode;
    ///<summary>Find Relation.Field Schema</summary>
    function FindCharset(Const aID:Integer):TFXMetaCharSet;overload;
    ///<summary>Find Relation.Field Schema</summary>
    function FindCharset(Const aID:Integer;Out CharSet:TFXMetaCharSet):Boolean;overload;

    ///<summary>Find Relation.Field Schema</summary>
    function CreateMissingRelation(Const aName:String):TFXCustomMetaRelation;
    ///<summary>Find Relation.Field Schema</summary>
    function FindRelation(Const aName:String) : TFXCustomMetaRelation;overload;
    ///<SUMMARY>Find Relation.Field Schema</SUMMARY>
    function FindRelation(Const aName:String;Out aNode:TFXCustomMetaRelation):Boolean;overload;
    ///<summary>Find Relation.Field Schema</summary>
    function FindRelation(Const aTR:TFXCustomTransaction;Const aName:String):TFXCustomMetaRelation;overload;virtual;
    ///<summary>Find Relation.Field Schema</summary>
    function DownloadRelation(Const aTR:TFXCustomTransaction;Const aName:String):TFXCustomMetaRelation;overload;virtual;
    ///<SUMMARY>Find Relation.Field Schema</SUMMARY>
    function DownloadRelation(Const aName:String;Out aNode:TFXCustomMetaRelation):Boolean;overload;

    ///<summary>Find Relation.Field Schema</summary>
    function FindRelationField(Const aTR:TFXCustomTransaction;Const aRelation,aField:String):TFXMetaTableField;overload;

    /// <summary>Find Exception</summary>
    function FindException(Const aName:String):TFXMetaException;
    ///<summary>Find UDF</summary>
    function FindUDF(Const aName:String):TFXMetaUDF;overload;
    ///<summary>Find Role</summary>
    function FindRole(Const aName:String):TFXMetaRole;overload;

    ///<summary>Clear Sequences</summary>
    procedure ClearSequences;
    ///<summary>Download Sequences</summary>
    procedure DownloadSequences(Const aTR:TFXCustomTransaction;Const ForceReload:Boolean);

    /// <summary>Find Procedure</summary>
    function FindProc(Const aName:String;Out FoundProc:TFXCustomMetaNode):Boolean;

    /// <summary>Find Trigger</summary>
    function FindTrigger(Const aName:String;Out FoundProc:TFXCustomMetaNode):Boolean;

  end;

/// <summary>IntToMetaObject</summary>
function Int2MetaObject(Const Value:Integer):TFXMetaObject;
/// <summary>IntToMetaObject</summary>
function MetaObject2Int(Const Value:TFXMetaObject):Integer;
/// <summary>IntToMetaObject</summary>
function MetaObject2Str(Const Value:TFXMetaObject):String;

/// <summary>Str2ForeignKeyRule</summary>
function Str2ForeignKeyRule(Const Value:String):TFXForeignKeyRule;

/// <summary>DecodeTriggerPrefix</summary>
function DecodeTriggerPrefix(Const Value: Integer):TFXTriggerPrefix;
/// <summary>DecodeSuffixes</summary>
function DecodeSuffixes(Const Value: Integer):TFXTriggerSuffixes;

implementation

Uses StrUtils,
  mFX.SystemTable;

Const
  _TriggerPrefixTypes_ : array [TFXTriggerPrefix] of String = (
    'BEFORE',
    'AFTER'
    );

  _TriggerSuffixTypes_ : array [TFXTriggerSuffix] of String = (
    'INSERT',
    'UPDATE',
    'DELETE'
    );

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function MetaObject2Int(Const Value:TFXMetaObject):Integer;
Begin
  Case Value of
    moTable     :Result:=0;
    moView      :Result:=1;
    moTrigger   :Result:=2;
    moComputed  :Result:=3;
    moValidation:Result:=4;
    moProcedure :Result:=5;
    moExprIndex :Result:=6;
    moException :Result:=7;
    moUser      :Result:=8;
    moField     :Result:=9;
    moForeignKey,
    moIndex     :Result:=10;
    //otUserGroup
    moRole      :Result:=12;
    moGenerator :Result:=14;
    moUDF       :Result:=15;
    //otBlobFilter;
    else         Result:=-1
    end
end;
{______________________________________________________________________________}
function MetaObject2Str(Const Value:TFXMetaObject):String;
Begin
  Result:=IntToStr(MetaObject2Int(Value))
End;
{______________________________________________________________________________}
function Int2MetaObject(Const Value:Integer):TFXMetaObject;
Begin
  Case Value of
     0:Result:=moTable;
     1:Result:=moView;
     2:Result:=moTrigger;
     3:Result:=moComputed;
     4:Result:=moValidation;
     5:Result:=moProcedure;
     6:Result:=moExprIndex;
     7:Result:=moException;
     8:Result:=moUser;
     9:Result:=moField;
    10:Result:=moUndefined;//otDependentCount;
    11:Result:=moUndefined;//otUserGroup;
    12:Result:=moRole;
    13:Result:=moGenerator;
    14:Result:=moUndefined;//otBlobFilter;
    15:Result:=moUDF;
    else Result:=moUndefined;
    end
end;
{______________________________________________________________________________}
function Str2ForeignKeyRule(Const Value:String):TFXForeignKeyRule;
Begin
  if SameText('RESTRICT',Value) then
    Result:=urRestrict else
  if SameText('CASCADE',Value) then
    Result:=urCascade else
  if SameText('SET NULL',Value) then
    Result:=urSetNull else
  if SameText('NO ACTION',Value) then
    Result:=urNoAction else
    Result:=urSetDefault
end;
{______________________________________________________________________________}
function DecodeTriggerPrefix(Const Value: Integer):TFXTriggerPrefix;
Var i:Integer;
Begin
  i:=(Value + 1) and 1;
  Result:=TFXTriggerPrefix(i);
end;
{______________________________________________________________________________}
function DecodeSuffixes(Const Value: Integer):TFXTriggerSuffixes;
var s:TFXTriggerSuffix;
    V,Slot:Integer;
begin
  Result := [];
  for Slot := 1 to 3 do begin
    V := ((Value + 1) shr (Slot * 2 - 1)) and 3;
    if V > 0 then Begin
      s:=TFXTriggerSuffix(V - 1);
      Include(Result,s);
    end end;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
destructor TFXCustomMetaNode.Destroy;
Begin
  fDependenciesX.Free;
  fUsing.Free;
  inherited;
end;
{______________________________________________________________________________}
function TFXCustomMetaNode.AsDropDDL:String;
Begin
  Result:='Internal Error';
End;
{______________________________________________________________________________}
function TFXCustomMetaNode.GetFullName:String;
Begin
  if (fOwner<>nil)and(fOwner<>Self) Then
    Result:=Self.ClassName+':'+fOwner.FullName+'.'+Self.fName else
    Result:=Self.ClassName+':'+Self.fName
end;
{______________________________________________________________________________}
class function TFXCustomMetaNode.DoQuoteName(Const Value:String):String;
Var s:String;
Begin
  s:=UpperCase(Value);
  if s<>Value Then Begin
    //do Quote !
    Result:=AnsiQuotedStr(Value,'"');
  end else Begin
    //do not Quote !
    Result:=s;
    end;
end;
{______________________________________________________________________________}
function TFXCustomMetaNode.GetQuotedName:String;
Var s:String;
Begin
  s:=UpperCase(fName);
  if s<>fName Then Begin
    //do Quote !
    Result:=AnsiQuotedStr(fName,'"');
  end else Begin
    //do not Quote !
    Result:=s
    end;
end;
{______________________________________________________________________________}
function TFXCustomMetaNode.GetQuotedName(Out Value:String):Boolean;
Var s:String;
Begin
  s:=UpperCase(fName);
  if s<>fName Then Begin
    //do Quote !
    Value:=AnsiQuotedStr(Self.fName,'"');
    result:=True
  end else Begin
    //do not Quote !
    Value:=s;
    result:=False
    end
end;
{______________________________________________________________________________}
function TFXCustomMetaNode.GetHasDependencies:Boolean;
Begin
  if fDependenciesX<>nil then Begin
    Result:=(fDependenciesX.Count>0)
  End else
    Result:=False;
End;
{______________________________________________________________________________}
function TFXCustomMetaNode.GetHasDependedOn(Const aMetaObject:TFXMetaObject):Boolean;
Var MetaNode:TFXCustomMetaNode;
    i:Integer;
Begin
  if fDependenciesX<>nil then Begin
    for i:=0 to Pred(fDependenciesX.Count) do Begin
      MetaNode:=TFXCustomMetaNode(fDependenciesX[i]);
      if MetaNode.fMetaObject=aMetaObject then Begin
        Result:=True;
        exit;
    end end end;

  Result:=False;
End;
{______________________________________________________________________________}
procedure TFXCustomMetaNode.AddDependency(Const aMetaNode:TFXCustomMetaNode);
Begin
  //This node is depend from aMetaNode
  if fDependenciesX=nil then
    fDependenciesX:=TList.Create;
  if fDependenciesX.IndexOf(aMetaNode)<0 then
    fDependenciesX.Add(aMetaNode);

  //aMetaNode use this node
  if aMetaNode.fUsing=nil then
    aMetaNode.fUsing:=TList.Create;
  if aMetaNode.fUsing.IndexOf(Self)<0 then
    aMetaNode.fUsing.Add(Self);
end;
{______________________________________________________________________________}
procedure TFXCustomMetaNode.AddDependency(Const aMetaObject:TFXMetaObject;Const aName:String);
Var MetaNode:TFXCustomMetaNode;
begin
  MetaNode:=fSchema.FindNode(aMetaObject,aName);
  if MetaNode<>nil Then
    Self.AddDependency(MetaNode)
end;
{______________________________________________________________________________}
procedure TFXCustomMetaNode.Assign2DependenciesQuery(Const aSQL:TFXCustomSQL);
Var v:TFXSQLVAR;
Begin
  aSQL.Params[0].AsTrimString:=fName;
  v:=aSQL.Params.FindByName('parent');
  if v<>nil then begin
    Assert(fOwner<>nil);
    v.AsTrimString:=fOwner.fName;
    end;
End;
{______________________________________________________________________________}
procedure TFXCustomMetaNode.DownloadDependencies(Const aSQL:TFXCustomSQL);
Var DepName,RelationName,FieldName:String;
    Relation:TFXCustomMetaRelation;
    Node:TFXCustomMetaNode;
    DepType:TFXMetaObject;
    DidActivate:Boolean;
Begin
  Assert(fSchema.fDB<>nil);
  Assert(fSchema.fTRx<>nil);
  Assert(fDependenciesX=nil);

  DidActivate:=aSQL.Start_TR;
  Assign2DependenciesQuery(aSQL);
  aSQL.ExecQuery;
  While Not aSQL.Eof do Begin
    DepName:= aSQL.Fields[_QRY_Dependency_DependentObj].AsTrimString;
    DepType:= Int2MetaObject(aSQL.Fields[_QRY_Dependency_DependentType].AsInteger);
    case DepType of
      moComputed:Begin
        RelationName:= aSQL.Fields[_QRY_Dependency_DependentRelation].AsTrimString;
        FieldName   := aSQL.Fields[_QRY_Dependency_DependentField   ].AsTrimString;
        if not fSchema.FindRelation(RelationName,Relation) then
          Relation:=fSchema.DownloadRelation(aSQL.Transaction,RelationName);
        if Relation<>nil then Begin
          Node:=Relation.FindField(FieldName);
          if Node<>nil then Begin
            Self.AddDependency(Node);
          end else
            raise Exception.CreateFmt('Missing Dep.Field DepType : %d (DepName:%s %s.%s)',[MetaObject2Int(DepType),DepName,RelationName,FieldName]);
        end else
          raise Exception.CreateFmt('Missing Dep.Relation DepType : %d (DepName:%s %s.%s)',[MetaObject2Int(DepType),DepName,RelationName,FieldName]);
        end;
      moView:Begin
        if not fSchema.FindRelation(DepName,Relation) then
          Relation:=fSchema.DownloadRelation(aSQL.Transaction,DepName);
        if Relation<>nil then Begin
          Self.AddDependency(Relation);
        end else
          raise Exception.CreateFmt('Missing Dep.View DepType : %d (DepName:%s)',[MetaObject2Int(DepType),DepName]);
        end;
      moProcedure:Begin
        if fSchema.FindProc(DepName,Node) then Begin
          Self.AddDependency(Node);
        End else
          raise Exception.CreateFmt('Missing Dep.StoredProc DepType : %d (DepName:%s)',[MetaObject2Int(DepType),DepName]);
        end;
      moTrigger:Begin
        if fSchema.FindTrigger(DepName,Node) then Begin
          Self.AddDependency(Node);
        End else
          raise Exception.CreateFmt('Missing Dep.Trigger DepType : %d (DepName:%s)',[MetaObject2Int(DepType),DepName]);
        end;
      else Begin
        raise Exception.CreateFmt('Unexcepted DepType : %d (DepName:%s)',[MetaObject2Int(DepType),DepName]);
      end end;
    aSQL.Next;
    end;
  aSQL.CloseQuery;

  if DidActivate then
    aSQL.Commit_TR;
end;
{______________________________________________________________________________}
procedure TFXCustomMetaNode.WriteTerminator(Const Stream: TStringStream);
Begin
  Stream.WriteString(fSchema.fTerminator+sLinebreak);
end;
{______________________________________________________________________________}
procedure TFXCustomMetaNode.WriteDependencies(Const Stream: TStringStream);
Var MetaNode:TFXCustomMetaNode;
    i:Integer;
Begin
  if fDependenciesX<>nil then Begin
    for i:=0 to Pred(fDependenciesX.Count) do Begin
      MetaNode:=TFXCustomMetaNode(fDependenciesX[i]);
      Stream.WriteString(format('-- %s Used By : %s',[Self.FullName,MetaNode.FullName]));
      Stream.WriteString(sLinebreak);
    end end;

  if fUsing<>nil then Begin
    for i:=0 to Pred(fUsing.Count) do Begin
      MetaNode:=TFXCustomMetaNode(fUsing[i]);
      Stream.WriteString(format('-- %s Using : %s',[Self.FullName,MetaNode.FullName]));
      Stream.WriteString(sLinebreak);
    end end;
end;
{______________________________________________________________________________}
procedure TFXCustomMetaNode.WriteComment(Const Stream: TStringStream;Const aComment:String);
Begin
  Stream.WriteString('-- '+aComment+sLinebreak);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXArrayBounds.CreateFromSQL(Const aOwner:TFXCustomMetaField;Const aSQL:TFXCustomSQL);
var DidActivate:Boolean;
    b:TFXArrayBound;
Begin
  DidActivate:=aSQL.Start_TR;
  aSQL.SQL.Text:=format(_QRY_ArrayDim_,[aOwner.QuotedName]);
  aSQL.ParamCheck:=False;
  aSQL.ExecQuery;
  While Not aSQL.Eof do Begin
    b:=TFXArrayBound.Create;
    b.fLowerBound :=aSQL.Fields[_QRY_ArrayDim_LOWER_BOUND].AsInteger;
    b.fHigherBound:=aSQL.Fields[_QRY_ArrayDim_UPPER_BOUND].AsInteger;
    Self.Add(b);
    aSQL.Next;
    end;
  aSQL.CloseQuery;
  if DidActivate then
    aSQL.Commit_TR;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
destructor TFXCustomMetaField.Destroy;
Begin
  fArrayBounds.Free;
  inherited;
end;
{______________________________________________________________________________}
procedure TFXCustomMetaField.AssignFromSQL(Const aFieldTypeIdx:Integer;Const aSQL:TFXCustomSQL);
Var charset:TFXMetaCharSet;
    f:TFXSQLVAR;
Begin
  fSubType       := aSQL.Fields[aFieldTypeIdx+ 1].AsInteger;
  fScale         := Abs(aSQL.Fields[aFieldTypeIdx+ 2].AsInteger);
  fPrecision     := aSQL.Fields[aFieldTypeIdx+ 3].AsInteger;
  fLength        := aSQL.Fields[aFieldTypeIdx+ 4].AsInteger;

  if fScale<>0 Then Begin
    fFieldType   := uftNumeric;
    if fPrecision=0 Then Begin
      Case aSQL.Fields[aFieldTypeIdx+ 0].AsInteger of
        blr_short   :fPrecision := 4;
        blr_long    :fPrecision := 7;
        blr_int64,
        blr_quad,
        blr_double  :fPrecision := 15;
        else         Raise Exception.CreateFmt('<%s> Unknow FieldType %d',[fName,aSQL.Fields[aFieldTypeIdx+ 0].AsInteger]);
      end end;
  end else Begin
    Case aSQL.Fields[aFieldTypeIdx+ 0].AsInteger of
      blr_text,
      blr_text2     :fFieldType := uftChar;
      blr_varying,
      blr_varying2  :fFieldType := uftVarchar;
      blr_cString,
      blr_cString2  :fFieldType := uftCString;
      blr_short     :fFieldType := uftSmallint;
      blr_long      :fFieldType := uftInteger;
      blr_quad      :fFieldType := uftQuad;
      blr_float,
      blr_d_float   :fFieldType := uftFloat;
      blr_double    :fFieldType := uftDoublePrecision;
      blr_timestamp :fFieldType := uftTimestamp;
      blr_blob      :fFieldType := uftBlobData;
      blr_blob_id   :fFieldType := uftBlobId;
      blr_sql_date  :fFieldType := uftDate;
      blr_sql_time  :fFieldType := uftTime;
      blr_int64     :fFieldType := uftInt64;
      else           Raise Exception.CreateFmt('<%s> Unknow FieldType %d',[fName,aSQL.Fields[aFieldTypeIdx+ 0].AsInteger]);
    end end;

  Case fFieldType of
    uftCString,
    uftVarchar,
    uftChar:Begin
      fBytesPerChar := 1;
      fFieldCharSet := EmptyStr;
      f:=aSQL.Fields[aFieldTypeIdx+ 5];
      if not f.IsNull then Begin
        if fSchema.FindCharset(f.AsInteger,charset) then Begin
          fFieldCharSet:=charset.CharSet;
          fBytesPerChar:=charset.fBytesPerChar;
        End else
          Raise Exception.CreateFmt('Invalid CharSet <%d> in Field/Param <%s>',[f.AsInteger,fName]);
      end end;
    else Begin
      fBytesPerChar := 1;
      fFieldCharSet := EmptyStr;
    end end;
End;
{______________________________________________________________________________}
procedure TFXCustomMetaField.AssignNullFlagFromSQL(Const aNullFlagIdx:Integer;Const aSQL:TFXCustomSQL);
Begin
  fIsNotNull :=(aSQL.Fields[aNullFlagIdx].AsInteger=1);
End;
{______________________________________________________________________________}
procedure TFXCustomMetaField.AssignArrayDimensionsFromSQL(Const aDimensionsIdx:Integer;Const aSQL1,aSQL2:TFXCustomSQL);
Var f:TFXSQLVAR;
Begin
  Case fFieldType of
    uftBlobData,
    uftBlobArray:Begin
      f:=aSQL1.Fields[aDimensionsIdx];
      if f.AsInteger>0 then Begin
        fFieldType:=uftBlobArray;
        fArrayBounds:=TFXArrayBounds.CreateFromSQL(Self,aSQL2);
        Assert(fArrayBounds.Count=f.AsInteger)
      end else
        fFieldType:=uftBlobData;
      end;
    else Begin
      Assert(aSQL1.Fields[aDimensionsIdx].AsInteger=0);
    end end
End;
{______________________________________________________________________________}
procedure TFXCustomMetaField.AssignBlobSegmentFromSQL(Const aSegLenIdx:Integer;Const aSQL:TFXCustomSQL);
Begin
  Case fFieldType of
    uftBlobData:Begin
      fSegmentLen:=aSQL.Fields[aSegLenIdx].AsInteger;
      end;
    else Begin
      Assert(aSQL.Fields[aSegLenIdx].AsInteger=0);
    end end
End;
{______________________________________________________________________________}
procedure TFXCustomMetaField.AssignCheckFromSQL(Const aValidationSourceIdx:Integer;Const aSQL:TFXCustomSQL);
Var f:TFXSQLVAR;
Begin
  f:=aSQL.Fields[aValidationSourceIdx];
  fHasCheck  := not f.IsNull;
  if fHasCheck then
    fCheckSrc:= f.AsTrimString;
End;
{______________________________________________________________________________}
procedure TFXCustomMetaField.AssignDefaultFromSQL(Const aDefaultSourceIdx:Integer;Const aSQL:TFXCustomSQL);
Var f:TFXSQLVAR;
Begin
  f:=aSQL.Fields[aDefaultSourceIdx];
  fHasDefault  := not f.IsNull;
  if fHasDefault then
    fDefaultSrc:= f.AsTrimString;
End;
{______________________________________________________________________________}
function TFXCustomMetaField.GetFieldTypeDLL:String;
Const
  _FXNumericFieldType_ : array[1..2] of String = (
    'NUMERIC',
    'DECIMAL'
    );
  _FXFieldTypes_ : array [TFXFieldType] of String = (
    '',
    'NUMERIC', 'CHAR', 'VARCHAR', 'CString', 'SMALLINT',
    'INTEGER', 'QUAD','FLOAT', 'DOUBLE PRECISION', 'TIMESTAMP', 'BLOB', 'ARRAY', 'BLOBID',
    'DATE', 'TIME', 'BIGINT' , 'ARRAY'
    );
Var b:TFXArrayBound;
    i:Integer;
begin
  case fFieldType of
    uftNumeric:Begin
      Assert(fArrayBounds=nil);
      Result := Format('%s(%d,%d)',[_FXNumericFieldType_[fSubType],fPrecision,fScale]);
      end;
    uftChar..
    uftCString:Begin
      Assert(fArrayBounds=nil);
      Result := Format('%s(%d)', [_FXFieldTypes_[fFieldType],fLength div fBytesPerChar]);
      end;
    uftBlobArray:Begin
      Assert(fArrayBounds<>nil);
      Assert(fArrayBounds.Count>0);
      b:=TFXArrayBound(fArrayBounds[0]);
      Result:=Result+format(' [%d:%d',[b.fLowerBound,b.fHigherBound]);
      for i := 1 to Pred(fArrayBounds.Count) do begin
        b:=TFXArrayBound(fArrayBounds[i]);
        Result:=Result+format(',%d:%d',[b.fLowerBound,b.fHigherBound]);
        end;
      Result:=Result+']';
      end;
    else Begin
      Assert(fArrayBounds=nil);
      Result := Format('%s', [_FXFieldTypes_[fFieldType]]);
    end end;
{
function TFXMetaTableField.GetShortFieldType:String;
Var b:TFXArrayBound;
    i:Integer;
Begin
  Result:=inherited GetShortFieldType;
  if (fArrayBounds<>nil) Then Begin
    Assert(fArrayBounds.Count>0);
    b:=TFXArrayBound(fArrayBounds[0]);
    Result:=Result+format(' [%d:%d',[b.fLowerBound,b.fHigherBound]);
    for i := 1 to Pred(fArrayBounds.Count) do begin
      b:=TFXArrayBound(fArrayBounds[i]);
      Result:=Result+format(',%d:%d',[b.fLowerBound,b.fHigherBound]);
      end;
    Result:=Result+']';
    end;
end;

  //TODO
  case fFieldType of
    uftChar..
    uftCString:begin
      if (fFieldCharSet<>fxNone) then Begin
        if (fFieldCharSet<>fExtractor.Database.DefaultCharSet) then
          Result:=' CHARACTER SET '+CharSet2Str(fFieldCharSet);
      end end;
    uftBlob:
      Result:=Format(' SUB_TYPE %d SEGMENT SIZE %d',[fSubType,fSegmentLen]);
    else
      Result:=EmptyStr;
    end;
}
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaDomain.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL1,aSQL2:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fMetaObject    := moDomain;

  AssignFromSQL(_QRY_Domain_FieldType,aSQL1);
  fName        := aSQL1.Fields[_QRY_Domain_Name  ].AsTrimString;
  fDescr       := aSQL1.Fields[_QRY_Domain_DESCR ].AsTrimString;

  AssignNullFlagFromSQL(_QRY_Domain_NullFlag,aSQL1);
  AssignArrayDimensionsFromSQL(_QRY_Domain_Dimensions,aSQL1,aSQL2);
  AssignBlobSegmentFromSQL(_QRY_Domain_SegLen,aSQL1);
  AssignCheckFromSQL(_QRY_Domain_ValidationSource,aSQL1);
  AssignDefaultFromSQL(_QRY_Domain_DefaultSource,aSQL1);
end;
{______________________________________________________________________________}
procedure TFXMetaDomain.WriteCreateDDL(Const Stream: TStringStream);
Var tab:String;
begin
  tab:=StringOfChar(' ',14);
  Stream.WriteString(
    'CREATE DOMAIN '+Self.QuotedName+' AS'+sLinebreak+
    tab+Self.FieldTypeDLL
    );

  if fHasDefault then
    Stream.WriteString(sLinebreak+tab+'DEFAULT '+fDefaultSrc);

  if fIsNotNull then
    Stream.WriteString(sLinebreak+tab+'NOT NULL');

  if fHasCheck then
    Stream.WriteString(sLinebreak+tab+fCheckSrc);
  WriteTerminator(Stream);

  if fDescr<>'' Then Begin
    Stream.WriteString(Format('COMMENT ON DOMAIN %s IS'+sLinebreak+'%s',[Self.QuotedName,AnsiQuotedStr(fDescr,'''')]));
    WriteTerminator(Stream);
    end;

  WriteDependencies(Stream);
  Stream.WriteString(sLinebreak);
end;
{______________________________________________________________________________}
function TFXMetaDomain.AsDropDDL:String;
Begin
  Result:=Format('DROP DOMAIN %s',[Self.QuotedName]);
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaTableField.CreateFromSQL(Const aOwner:TFXCustomMetaRelation;Const aSQL1,aSQL2:TFXCustomSQL);
Begin
  fSchema        := aOwner.fSchema;
  fOwner         := aOwner;
  fMetaObject    := moField;

  AssignFromSQL(_QRY_Table_Field_FieldType,aSQL1);
  fName          := aSQL1.Fields[_QRY_Table_Field_Name             ].AsTrimString;
  fDescr         := aSQL1.Fields[_QRY_Table_Field_Descr            ].AsTrimString;
  fDomain        := aSQL1.Fields[_QRY_Table_Field_Domain           ].AsTrimString;
  if Pos('RDB$',fDomain)=1 Then 
    fDomain      :='';

  fComputedSrc   := aSQL1.Fields[_QRY_Table_Field_ComputedSource   ].AsTrimString;
  if fComputedSrc='' then Begin
    AssignNullFlagFromSQL(_QRY_Table_Field_NullFlag,aSQL1);
    AssignArrayDimensionsFromSQL(_QRY_Table_Field_Dimensions,aSQL1,aSQL2);
    AssignBlobSegmentFromSQL(_QRY_Table_Field_SegLen,aSQL1);
    AssignCheckFromSQL(_QRY_Table_Field_ValidationSource,aSQL1);
    AssignDefaultFromSQL(_QRY_Table_Field_DefaultSource,aSQL1);
  end else Begin
    fIsComputed  := True;
    end;
    
  if fSchema.fFields=nil then
    fSchema.fFields:=TList.Create;
  fSchema.fFields.Add(Self);

  if aOwner.fFields=nil then
    aOwner.fFields:=TFXList.Create;
  aOwner.fFields.Add(Self);
End;
{______________________________________________________________________________}
destructor TFXMetaTableField.Destroy;
Begin
  fPrimaryKeys.Free;
  inherited;
end;
{______________________________________________________________________________}
Class procedure TFXMetaTableField.PrepareQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Table_Fields_+'WHERE (RFR.RDB$SYSTEM_FLAG <> 1 OR RFR.RDB$SYSTEM_FLAG IS NULL) ORDER BY RFR.RDB$RELATION_NAME, RFR.RDB$FIELD_POSITION, RFR.RDB$FIELD_NAME';
End;
{______________________________________________________________________________}
procedure TFXMetaTableField.Download;
Begin
End;
{______________________________________________________________________________}
function TFXMetaTableField.GetFieldInKey:Boolean;
Begin
  Result:=fFlags*[fiPrimary,fiUnique]<>[];
End;
{______________________________________________________________________________}
procedure TFXMetaTableField.WriteCreateDDL(Const Stream: TStringStream);
Var l:Integer;
    s:String;
Begin
  if GetQuotedName(s) then
    s:=' '+s else
    s:='  '+s;
  l:=System.Length(s);
  Stream.WriteString(s);
  Assert(System.Length(s)<35);
  Stream.WriteString(StringOfChar(' ',35-l));
  if fIsComputed Then Begin
    Stream.WriteString(' COMPUTED BY '+fComputedSrc);
  end else
  if fDomain<>'' Then Begin
    Stream.WriteString(fDomain);
    if fHasDefault then
      Stream.WriteString(' DEFAULT '+fDefaultSrc);
    if fIsNotNull then
      Stream.WriteString(' NOT NULL');
  end else Begin
    Stream.WriteString(Self.FieldTypeDLL);
    if fHasDefault then
      Stream.WriteString(' DEFAULT '+fDefaultSrc);
    if fIsNotNull then
      Stream.WriteString(' NOT NULL');
    if fHasCheck then
      Stream.WriteString(' '+fCheckSrc);
    end;
  WriteTerminator(Stream);
  
  if fDescr<>'' Then Begin
    Stream.WriteString(Format('COMMENT ON DOMAIN %s IS'+sLinebreak+'%s',[Self.QuotedName,AnsiQuotedStr(fDescr,'''')]));
    WriteTerminator(Stream);
    end;

  WriteDependencies(Stream);
  Stream.WriteString(sLinebreak);
End;
{______________________________________________________________________________}
function TFXMetaTableField.AsDropDDL:String;
Begin
  Assert(fOwner<>nil);
  Result:=Format('ALTER TABLE %s DROP %s',[fOwner.QuotedName,Self.QuotedName]);
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXCustomRelationMetaNode.GetRelationName:String;
begin
  if fOwner is TFXCustomMetaRelation then
    Result:=fOwner.fName else
    Result:=fRelation
end;
{______________________________________________________________________________}
function TFXCustomRelationMetaNode.GetQuotedRelationName:String;
begin
  if fOwner is TFXCustomMetaRelation then
    Result:=fOwner.GetQuotedName else
    Result:=fSchema.QuoteIdentifier(fRelation)
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXMetaCustomIndex.GetQuotedIndexName: String;
Var s:String;
Begin
  s:=UpperCase(fIndexName);
  if s<>fIndexName Then Begin
    //do Quote !
    Result:=AnsiQuotedStr(fIndexName,'"');
  end else Begin
    //do not Quote !
    Result:=s
    end;
end;
{______________________________________________________________________________}
procedure TFXMetaCustomIndex.Download(Const aSQL:TFXCustomSQL);
var DidActivate:Boolean;
    f:TFXMetaTableField;
    t:TFXMetaTable;
    fn:String;
Begin
  if (fOwner<>nil)and(fOwner is TFXMetaTable) then
    t:=fOwner as TFXMetaTable else
    t:=nil;
  DidActivate:=aSQL.Start_TR;
  With aSQL do Begin
    SQL.Text   :=_QRY_Index_Fields_;
    SQL.Add(format('WHERE (ISG.RDB$INDEX_NAME=%s)',[AnsiQuotedStr(Self.QuotedIndexName,'''')]));
    SQL.Add(       'ORDER BY 1 ASC');
    end;
  aSQL.ParamCheck:=False;
  aSQL.ExecQuery;
  While Not aSQL.Eof do Begin
    fn:=aSQL.Fields[_QRY_Index_Field_Name].AsTrimString;
    if t<>nil then Begin
      f:=t.FindField(fn);
      if f=nil then
        Raise Exception.Create('Unexpected Error in Primary Key Fields:'+fName);
      Self.AddField(f,aSQL);
      end;
    aSQL.Next;
    end;
  aSQL.CloseQuery;
  if DidActivate then
    aSQL.Commit_TR;
End;
{______________________________________________________________________________}
function TFXMetaCustomIndex.AsRecomputeDDL:String;
Begin
  Result:=Format('SET STATISTICS INDEX %s',[Self.QuotedName]);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaPrimaryKey.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aOwner:TFXMetaTable;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fOwner         := aOwner;
  fFields        := TList.Create;

  fRelation      := aSQL.Fields[_QRY_PrimaryKey_RelationName_  ].AsTrimString;
  fName          := aSQL.Fields[_QRY_PrimaryKey_ConstraintName_].AsTrimString;
  fIndexName     := aSQL.Fields[_QRY_PrimaryKey_IndexName_     ].AsTrimString;
  fDescr         := aSQL.Fields[_QRY_PrimaryKey_Description_   ].AsTrimString;
  fIsPrimary     := SameText(aSQL.Fields[_QRY_PrimaryKey_ConstraintType_].AsTrimString,'PRIMARY KEY');
  if fIsPrimary then
    fMetaObject  := moPrimaryKey else
    fMetaObject  := moUnqConstraint;

  if fSchema.fPrimaryKeys=nil then
    fSchema.fPrimaryKeys:=TList.Create;
  fSchema.fPrimaryKeys.Add(Self);

  if aOwner<>nil then Begin
    if aOwner.fPrimaryKeys=nil then
      aOwner.fPrimaryKeys:=TFXList.Create;
    aOwner.fPrimaryKeys.Add(Self);
  End else
    fSchema.fNodes.Add(Self);
End;
{______________________________________________________________________________}
destructor TFXMetaPrimaryKey.Destroy;
Begin
  fFields.Free;
  inherited;
end;
{______________________________________________________________________________}
Class procedure TFXMetaPrimaryKey.PrepareQuery(Const IncludeSysTable:Boolean;Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_PrimaryKeys_;
  if not IncludeSysTable then
    aSQL.SQL.Add('  and (i.RDB$SYSTEM_FLAG <> 1 OR i.RDB$SYSTEM_FLAG IS NULL)');
  aSQL.SQL.Add('ORDER BY a.RDB$RELATION_NAME, a.RDB$CONSTRAINT_NAME');
End;
{______________________________________________________________________________}
Class procedure TFXMetaPrimaryKey.PrepareQuery(Const aName:String;Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_PrimaryKeys_;
  aSQL.SQL.Add(format('  and (a.RDB$RELATION_NAME=%s)',[AnsiQuotedStr(DoQuoteName(aName),'''')]));
End;
{______________________________________________________________________________}
procedure TFXMetaPrimaryKey.AddField(Const aField:TFXMetaTableField;Const aSQL:TFXCustomSQL);
Begin
  Assert(aField<>nil);
  if aField.fPrimaryKeys=nil then
    aField.fPrimaryKeys:=TList.Create;
  if fIsPrimary then
    Include(aField.fFlags,fiPrimary) else
    Include(aField.fFlags,fiUnique);
  aField.fPrimaryKeys.Add(Self);
  fFields.Add(aField);
End;
{______________________________________________________________________________}
procedure TFXMetaPrimaryKey.WriteCreateDDL(Const Stream: TStringStream);
var f:TFXMetaTableField;
    i:Integer;
begin
  Stream.WriteString('ALTER TABLE ');
  Stream.WriteString(Self.QuotedRelationName);
  Stream.WriteString(' ADD');
  if Not SameText(Copy(fName,0,6),'INTEG_') then Begin
    Stream.WriteString(' CONSTRAINT ');
    Stream.WriteString(Self.QuotedName);
    end;
  if fIsPrimary Then
    Stream.WriteString(' PRIMARY KEY(') else
    Stream.WriteString(' UNIQUE(');
  if (fFields<>nil)and(fFields.Count>0) Then Begin
    f:=TFXMetaTableField(fFields[0]);
    Stream.WriteString(f.Name);
    for i:=1 to Pred(fFields.Count) do begin
      f:=TFXMetaTableField(fFields[i]);
      Stream.WriteString(','+f.Name);
      end;
    end;
  Stream.WriteString(')');
  //using index BOX
  if Not SameText(fIndexName,fName) then Begin
    if Not SameText(Copy(fIndexName,0,4),'RDB$') then Begin
      Stream.WriteString(' USING INDEX ');
      Stream.WriteString(fIndexName);
    end end;
  WriteTerminator(Stream);
end;
{______________________________________________________________________________}
function TFXMetaPrimaryKey.AsDropDDL:String;
Begin
  Result:=Format('ALTER TABLE %s DROP CONSTRAINT %s',[Self.QuotedRelationName,Self.QuotedName]);
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaForeignKey.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aOwner:TFXMetaTable;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fOwner         := aOwner;
  fMetaObject    := moForeignKey;
  fFields        := TList.Create;
  fFKFields      := TList.Create;

  fRelation      := aSQL.Fields[_QRY_ForeignKey_RelationName  ].AsTrimString;
  FName          := aSQL.Fields[_QRY_ForeignKey_Name          ].AsTrimString;
  FIndexName     := aSQL.Fields[_QRY_ForeignKey_IndexName     ].AsTrimString;
  fFKName        := aSQL.Fields[_QRY_ForeignKey_FKRelationName].AsTrimString;
  fFKOnUpdate    := Str2ForeignKeyRule(aSQL.Fields[_QRY_ForeignKey_UpdateRule].AsTrimString);
  fFKOnDelete    := Str2ForeignKeyRule(aSQL.Fields[_QRY_ForeignKey_DeleteRule].AsTrimString);

  if fSchema.fForeignKeys=nil then
    fSchema.fForeignKeys:=TList.Create;
  fSchema.fForeignKeys.Add(Self);

  if aOwner<>nil then Begin
    if aOwner.fForeignKeys=nil then
      aOwner.fForeignKeys:=TFXList.Create;
    aOwner.fForeignKeys.Add(Self);
  End else
    fSchema.fNodes.Add(Self);
end;
{______________________________________________________________________________}
destructor TFXMetaForeignKey.Destroy;
Begin
  fFKFields.Free;
  fFields.Free;
  inherited;
end;
{______________________________________________________________________________}
Class procedure TFXMetaForeignKey.PrepareQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_ForeignKeys_+'ORDER BY A.RDB$RELATION_NAME, A.RDB$CONSTRAINT_NAME';
End;
{______________________________________________________________________________}
Class procedure TFXMetaForeignKey.PrepareQuery(Const aName:String;Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_ForeignKeys_+format('  and (a.RDB$RELATION_NAME=%s)',[AnsiQuotedStr(DoQuoteName(aName),'''')])+'ORDER BY A.RDB$CONSTRAINT_NAME';
End;
{______________________________________________________________________________}
procedure TFXMetaForeignKey.AddField(Const aField:TFXMetaTableField;Const aSQL:TFXCustomSQL);
Begin
  fFields.Add(aField);
End;
{______________________________________________________________________________}
procedure TFXMetaForeignKey.Assign2DependenciesQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.Params[0].AsTrimString:=fIndexName;
End;
{______________________________________________________________________________}
procedure TFXMetaForeignKey.WriteCreateDDL(Const Stream: TStringStream);
Var f:TFXMetaTableField;
    i:Integer;
Begin
  Stream.WriteString('ALTER TABLE ');
  Stream.WriteString(Self.QuotedRelationName);
  Stream.WriteString(' ADD');
  if Not SameText(Copy(fName,0,6),'INTEG_') then Begin
    Stream.WriteString(' CONSTRAINT ');
    Stream.WriteString(Self.QuotedName);
    end;
  Stream.WriteString(' ADD FOREIGN KEY (');

  if (fFields<>nil)and(fFields.Count>0) Then Begin
    Assert(fFKFields<>nil);
    Assert(fFKFields.Count=fFields.Count);
    f:=TFXMetaTableField(fFields[0]);
    Stream.WriteString(f.Name);
    for i:=1 to Pred(fFields.Count) do begin
      f:=TFXMetaTableField(fFields[i]);
      Stream.WriteString(','+f.Name);
      end;
    Stream.WriteString(Format(')REFERENCES %s (',[fFKRelation.QuotedName]));
    f:=TFXMetaTableField(fFKFields[0]);
    Stream.WriteString(f.Name);
    for i:=1 to Pred(fFKFields.Count) do begin
      f:=TFXMetaTableField(fFKFields[i]);
      Stream.WriteString(','+f.Name);
      end;
    Stream.WriteString(')');
    end;

  case fFKOnUpdate of
    urCascade:    Stream.WriteString(' ON UPDATE CASCADE');
    urSetNull:    Stream.WriteString(' ON UPDATE SET NULL');
    urNoAction:   Stream.WriteString(' ON UPDATE NO ACTION');
    urSetDefault: Stream.WriteString(' ON UPDATE SET DEFAULT');
    end;

  case fFKOnDelete of
    urCascade:    Stream.WriteString(' ON DELETE CASCADE');
    urSetNull:    Stream.WriteString(' ON DELETE SET NULL');
    urNoAction:   Stream.WriteString(' ON DELETE NO ACTION');
    urSetDefault: Stream.WriteString(' ON DELETE SET DEFAULT');
    end;

  // TODO using descending index
  if fIndexName<>EmptyStr then
    Stream.WriteString(' using index '+fIndexName);

  WriteTerminator(Stream);
end;
{______________________________________________________________________________}
function TFXMetaForeignKey.AsDropDDL:String;
Begin
  Result:=Format('ALTER TABLE %s DROP CONSTRAINT %s',[Self.QuotedRelationName,Self.QuotedName]);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaIndex.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aOwner:TFXMetaTable;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fOwner         := aOwner;
  fMetaObject    := moIndex;
  fFields        := TList.Create;

  fRelation      := aSQL.Fields[_QRY_Index_RelationName].AsTrimString;
  fName          := aSQL.Fields[_QRY_Index_Name        ].AsTrimString;
  fActive        :=(aSQL.Fields[_QRY_Index_ActiveFlag  ].AsInteger=0);
  fUnique        :=(aSQL.Fields[_QRY_Index_UniqueFlag  ].AsInteger=1);
  if (aSQL.Fields[_QRY_Index_TypeFlag].AsInteger=0) Then
    fOrder       := IoAscending else
    fOrder       := IoDescending;

  if fSchema.fIndexes=nil then
    fSchema.fIndexes:=TList.Create;
  fSchema.fIndexes.Add(Self);

  if aOwner<>nil then Begin
    if aOwner.fIndexes=nil then
      aOwner.fIndexes:=TFXList.Create;
    aOwner.fIndexes.Add(Self);
  End else
    fSchema.fNodes.Add(Self);
end;
{______________________________________________________________________________}
destructor TFXMetaIndex.Destroy;
Begin
  fFields.Free;
  inherited;
end;
{______________________________________________________________________________}
Class procedure TFXMetaIndex.PrepareQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Indexes_+'order by IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME';
End;
{______________________________________________________________________________}
Class procedure TFXMetaIndex.PrepareQuery(Const aName:String;Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Indexes_+format('  and (IDX.RDB$RELATION_NAME=%s)',[AnsiQuotedStr(DoQuoteName(aName),'''')])+'order by IDX.RDB$INDEX_NAME'
End;
{______________________________________________________________________________}
procedure TFXMetaIndex.AddField(Const aField:TFXMetaTableField;Const aSQL:TFXCustomSQL);
Begin
  fFields.Add(aField);
End;
{______________________________________________________________________________}
procedure TFXMetaIndex.WriteCreateDDL(Const Stream: TStringStream);
var sUNIQUE,sORDER:String;
    f:TFXMetaTableField;
    I: Integer;
Begin
  if FUnique then
    sUNIQUE:= ' UNIQUE' else
    sUNIQUE:= '';
  if FOrder = IoDescending then
    sORDER := ' DESCENDING' else
    sORDER := '';

  Stream.WriteString(Format('CREATE%s%s INDEX %s ON %s ',[sORDER,sUNIQUE,Self.QuotedName,Self.QuotedRelationName]));
  if (fFields<>nil)and(fFields.Count>0) Then Begin
    Stream.WriteString('(');
    f:=TFXMetaTableField(fFields[0]);
    Stream.WriteString(f.QuotedName);
    for i:=1 to Pred(fFields.Count) do begin
      f:=TFXMetaTableField(fFields[i]);
      Stream.WriteString(','+f.QuotedName);
      end;
    Stream.WriteString(')');
    end;

  if not FActive then Begin
    WriteTerminator(Stream);
    Stream.WriteString(Format(sLinebreak+'ALTER INDEX %s INACTIVE',[Self.QuotedName]));
    end;
    
  WriteTerminator(Stream);
end;
{______________________________________________________________________________}
function TFXMetaIndex.AsOff:String;
Begin
  Result:=Format('ALTER INDEX %s INACTIVE',[Self.QuotedName]);
end;
{______________________________________________________________________________}
function TFXMetaIndex.AsOn:String;
Begin
  Result:=Format('ALTER INDEX %s ACTIVE',[Self.QuotedName]);
end;
{______________________________________________________________________________}
function TFXMetaIndex.AsDropDDL:String;
Begin
  Result:=Format('DROP INDEX %s',[Self.QuotedName]);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaCheckConstraint.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aOwner:TFXCustomMetaRelation;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  if aOwner<>nil then
    fOwner       := aOwner else
    fOwner       := Self;
  fMetaObject    := moTrigger;

  fRelation := aSQL.Fields[_QRY_CheckConstraint_Relation].AsTrimString;
  fName     := aSQL.Fields[_QRY_CheckConstraint_Name    ].AsTrimString;
  fDescr    := aSQL.Fields[_QRY_CheckConstraint_Descr   ].AsTrimString;

  if fSchema.fCheckConstraints=nil then
    fSchema.fCheckConstraints:=TList.Create;
  fSchema.fCheckConstraints.Add(Self);

  if aOwner<>nil then Begin
    if aOwner.fCheckConstraints=nil then
      aOwner.fCheckConstraints:=TFXList.Create;
    aOwner.fCheckConstraints.Add(Self);
  End else
    fSchema.fNodes.Add(Self);
end;
{______________________________________________________________________________}
Class procedure TFXMetaCheckConstraint.PrepareQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_CheckConstraints_+'order by RDB$RELATION_NAME,RDB$CONSTRAINT_NAME';
End;
{______________________________________________________________________________}
procedure TFXMetaCheckConstraint.WriteCreateDDL(Const Stream: TStringStream);
begin
  Stream.WriteString(Format('ALTER TABLE %s ADD CONSTRAINT %s Check %s'+sLinebreak,[Self.GetQuotedRelationName,Self.QuotedName,Self.fSource]));
  WriteDependencies(Stream);
  WriteTerminator(Stream);
end;
{______________________________________________________________________________}
function TFXMetaCheckConstraint.AsDropDDL:String;
Begin
  Result:=Format('ALTER TABLE %s DROP CONSTRAINT %s'+sLinebreak,[Self.GetQuotedRelationName,Self.QuotedName]);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaTrigger.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aOwner:TFXCustomMetaRelation;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fOwner         := aOwner;
  fMetaObject    := moTrigger;

  fRelation := aSQL.Fields[_QRY_Trigger_RelationName].AsTrimString;
  fName     := aSQL.Fields[_QRY_Trigger_Name        ].AsTrimString;
  fDescr    := aSQL.Fields[_QRY_Trigger_Descr       ].AsTrimString;
  fPrefix   := DecodeTriggerPrefix(aSQL.Fields[_QRY_Trigger_Type].AsInteger);
  fSuffix   := DecodeSuffixes(aSQL.Fields[_QRY_Trigger_Type].AsInteger);
  fPosition := aSQL.Fields[_QRY_Trigger_Seq         ].AsInteger;
  fActive   :=(aSQL.Fields[_QRY_Trigger_Inactive    ].AsInteger=0);

  if fSchema.fTriggers=nil then
    fSchema.fTriggers:=TList.Create;
  fSchema.fTriggers.Add(Self);

  if aOwner<>nil then Begin
    if aOwner.fTriggers=nil then
      aOwner.fTriggers:=TFXList.Create;
    aOwner.fTriggers.Add(Self);
  End else
    fSchema.fNodes.Add(Self);
end;
{______________________________________________________________________________}
Class procedure TFXMetaTrigger.PrepareQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Triggers_+'order by TRI.RDB$TRIGGER_NAME';
End;
{______________________________________________________________________________}
procedure TFXMetaTrigger.WriteCreateDDL(Const Stream: TStringStream);
Var Suf: TFXTriggerSuffix;
    Count: Smallint;
begin
  Stream.WriteString(Format('CREATE TRIGGER %s FOR %s'+sLinebreak,[Self.QuotedName,Self.GetQuotedRelationName]));
  if not FActive then
    Stream.WriteString('  INACTIVE '+_TriggerPrefixTypes_[fPrefix]+' ') else
    Stream.WriteString('  ACTIVE '+_TriggerPrefixTypes_[fPrefix]+' ');
  Count := 0;
  for Suf := tsInsert to tsDelete do Begin
    if Suf in FSuffix then begin
      Inc(Count);
      if Count > 1 then
        Stream.WriteString(' OR ');
      Stream.WriteString(_TriggerSuffixTypes_[Suf]);
    end end;
  Stream.WriteString(Format(sLinebreak+'  POSITION %d'+sLinebreak,[FPosition]));
  if fSource='' Then Begin
    Stream.WriteString('Begin'+sLinebreak);
    Stream.WriteString('  -- Missing Source'+sLinebreak);
    Stream.WriteString('  exit;'+sLinebreak);
    Stream.WriteString('end'+sLinebreak);
  end else
    Stream.WriteString(fSource+sLinebreak);

  WriteDependencies(Stream);

  WriteTerminator(Stream);
end;
{______________________________________________________________________________}
function TFXMetaTrigger.AsDropDDL:String;
Begin
  Result:=Format('DROP TRIGGER %s',[Self.QuotedName]);
end;
{______________________________________________________________________________}
function TFXMetaTrigger.SetActiveDDL:String;
Begin
  Result:=Format('ALTER TRIGGER %s ACTIVE',[Self.QuotedName]);
end;
{______________________________________________________________________________}
function TFXMetaTrigger.SetInactiveDDL:String;
Begin
  Result:=Format('ALTER TRIGGER %s INACTIVE',[Self.QuotedName]);
end;
{______________________________________________________________________________}
function TFXCustomMetaSchema.FindTrigger(Const aName:String;Out FoundProc:TFXCustomMetaNode):Boolean;
Var MetaTrigger:TFXMetaTrigger;
    i:Integer;
Begin
  if fTriggers<>nil then Begin
    for i:=0 to Pred(fTriggers.Count) do Begin
      MetaTrigger:=TFXMetaTrigger(fTriggers[i]);
      if SameText(MetaTrigger.FName,aName) then Begin
        FoundProc:=MetaTrigger;
        Result:=True;
        exit;
    end end end;
  FoundProc:=nil;
  Result:=False;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
destructor TFXCustomMetaRelation.Destroy;
begin
  fCheckConstraints.Free;
  fTriggers.Free;
  fFields.Free;
  inherited;
end;
{______________________________________________________________________________}
function TFXCustomMetaRelation.FindField(Const aName : String) : TFXMetaTableField;
Var f:TFXMetaTableField;
    i:Integer;
begin
  Result:=nil;
  for i:=0 to Pred(fFields.Count) do Begin
    f:=TFXMetaTableField(fFields.Items[i]);
    if SameText(f.fName,aName) then Begin
      Result:=f;
      Break
    end end
end;
{______________________________________________________________________________}
function TFXCustomMetaRelation.SelectStart(Const aAlias:String;Const aFilter:TFunc<TFXMetaTableField,Boolean>): String;
Var MetaTableField:TFXMetaTableField;
    i,n:Integer;
    s:String;
begin
  s:='Select ';
  if (fFields<>nil)and(fFields.Count>0) Then Begin
    n:=0;
    for i:=0 to Pred(fFields.Count) do Begin
      MetaTableField:=TFXMetaTableField(fFields[i]);
      if MetaTableField.fIsComputed then
        Continue;
      if Assigned(aFilter) then
        if not aFilter(MetaTableField) then
          Continue;
      if n>0 Then s:=s+sLinebreak+'      ,';
      s:=s+MetaTableField.fName;
      Inc(n);
      end;
  end else
    s:=s+'*';
  s:=s+sLinebreak+'from '+Self.fName+' '+aAlias;

  // TrimRight(['.']);
  I := Length(s);
  if (I > 0) and (not CharInSet(S[I],[#0..' ','.'])) then Exit(S);
  while (I > 0) and (CharInSet(S[I],[#0..' ','.'])) do Dec(I);
  Result := Copy(S, 1, I);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaMissingRelation.CreateMissingRelation(Const aSchema:TFXCustomMetaSchema;Const aName:String);
Begin
  fSchema        := aSchema;
  fOwner         := Self;
  fMetaObject    := moTable;
  fFields        := TFXList.Create;

  fName          := aName;
  fDescr         :='Missing relation';

  if fSchema.fTables=nil then
    fSchema.fTables:=TList.Create;
  fSchema.fTables.Add(Self);
  fSchema.fNodes.Add(Self);
End;
{______________________________________________________________________________}
procedure TFXMetaMissingRelation.WriteCreateDDL(Const Stream: TStringStream);
Begin

End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaTable.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fOwner         := Self;
  fMetaObject    := moTable;
  fFields        := TFXList.Create;
  fPrimaryKeys   := TFXList.Create;

  fName          := aSQL.Fields[_QRY_Table_Name_ ].AsTrimString;
  fDescr         := aSQL.Fields[_QRY_Table_Descr_].AsTrimString;

  if fSchema.fTables=nil then
    fSchema.fTables:=TList.Create;
  fSchema.fTables.Add(Self);
  fSchema.fNodes.Add(Self);
End;
{______________________________________________________________________________}
destructor TFXMetaTable.Destroy;
begin
  fIndexes.Free;
  fForeignKeys.Free;
  fPrimaryKeys.Free;
  inherited;
end;
{______________________________________________________________________________}
Class procedure TFXMetaTable.PrepareQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Tables_+'  and (NOT REL.RDB$FLAGS IS NULL)AND(REL.RDB$SECURITY_CLASS STARTING WITH ''SQL$'')and(REL.RDB$SYSTEM_FLAG <> 1 OR REL.RDB$SYSTEM_FLAG IS NULL) ORDER BY REL.RDB$RELATION_NAME';
End;
{______________________________________________________________________________}
Class procedure TFXMetaTable.PrepareQuery(Const aName : String;Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Tables_+format('  and (REL.RDB$RELATION_NAME=%s)',[AnsiQuotedStr(DoQuoteName(aName),'''')]);
End;
{______________________________________________________________________________}
procedure TFXMetaTable.Download(Const aSQL1,aSQL2:TFXCustomSQL);
var pk:TFXMetaPrimaryKey;
    DidActivate:Boolean;
begin
  if fPrimaryKeys<>nil then
    fPrimaryKeys.Clear;
  if fFields<>nil then
    fFields.Clear;

  DidActivate:=aSQL1.Start_TR;
  aSQL1.SQL.Text:=
     _QRY_Table_Fields_+
     format('WHERE (RFR.RDB$RELATION_NAME=%s)',[AnsiQuotedStr(Self.QuotedName,'''')])+sLinebreak+
     'ORDER BY RFR.RDB$FIELD_POSITION, RFR.RDB$FIELD_NAME';
  aSQL1.ParamCheck:=False;
  aSQL1.ExecQuery;
  While Not aSQL1.Eof do Begin
    TFXMetaTableField.CreateFromSQL(Self,aSQL1,aSQL2);
    aSQL1.Next;
    end;
  aSQL1.CloseQuery;

  TFXMetaPrimaryKey.PrepareQuery(fName,aSQL1);
  aSQL1.ParamCheck:=False;
  aSQL1.ExecQuery;
  While Not aSQL1.Eof do Begin
    pk:=TFXMetaPrimaryKey.CreateFromSQL(fSchema,Self,aSQL1);
    pk.Download(aSQL2);
    aSQL1.Next;
    end;
  aSQL1.CloseQuery;

  if DidActivate then
    aSQL1.Commit_TR;
end;
{______________________________________________________________________________}
procedure TFXMetaTable.WriteCreateDDL(Const Stream: TStringStream);
Begin
{
Var MetaTableField:TFXMetaTableField;
    MetaNode:TFXCustomMetaNode;
    s,p,fn:String;
    i:Integer;
begin
  p:=StringOfChar(' ',5);
  s:=QuoteIdentifier(fName);
  Stream.WriteString(Format('CREATE TABLE %s(', [s]));
  if (fFields<>nil)and(fFields.Count>0) Then Begin
    MetaTableField:=TFXMetaTableField(fFields[0]);
    Stream.WriteString(sLinebreak+p);
    MetaTableField.SaveToDDL(Stream);
    for i:=1 to Pred(fFields.Count) do Begin
      MetaTableField:=TFXMetaTableField(fFields[i]);
      fn:=QuoteIdentifier(MetaTableField.Name);
      Stream.WriteString(','+sLinebreak+p);
      MetaTableField.SaveToDDL(Stream);
    end end;
  Stream.WriteString(sLinebreak+p+')');
  WriteTerminator(Stream);

  if OIDComment in fExtractor.fFlags Then Begin
    if fDescr<>'' Then Begin
      Stream.WriteString(Format('COMMENT ON TABLE %s IS'+sLinebreak+'%s',[s,AnsiQuotedStr(fDescr,'''')]));
      WriteTerminator(Stream);
      end;
    if (fFields<>nil)and(fFields.Count>0) Then Begin
      for i:=0 to Pred(fFields.Count) do Begin
        MetaTableField:=TFXMetaTableField(fFields[i]);
        if MetaTableField.fDescr<>'' Then Begin
          fn:=QuoteIdentifier(MetaTableField.Name);
          Stream.WriteString(Format('COMMENT ON COLUMN %s.%s IS'+sLinebreak+'%s',[s,fn,AnsiQuotedStr(MetaTableField.fDescr,'''')]));
          WriteTerminator(Stream);
    end end end end;

  if Not (OIDGroupPrimaryKeys in fExtractor.fFlags) Then Begin
    if fPrimaryKeys<>nil Then Begin
      for i:=0 to Pred(fPrimaryKeys.Count) do Begin
        MetaNode:=TFXCustomMetaNode(fPrimaryKeys[i]);
        MetaNode.SaveToDDL(Stream);
      end end;
    if fUnqCnstraints<>nil Then Begin
      for i:=0 to Pred(fUnqCnstraints.Count) do Begin
        MetaNode:=TFXCustomMetaNode(fUnqCnstraints[i]);
        MetaNode.SaveToDDL(Stream);
      end end;
    if fForeignKeys<>nil Then Begin
      for i:=0 to Pred(fForeignKeys.Count) do Begin
        MetaNode:=TFXCustomMetaNode(fForeignKeys[i]);
        MetaNode.SaveToDDL(Stream);
      end end;
    if fIndexes<>nil Then Begin
      for i:=0 to Pred(fIndexes.Count) do Begin
        MetaNode:=TFXCustomMetaNode(fIndexes[i]);
        MetaNode.SaveToDDL(Stream);
      end end;
    end;

  WriteDependencies(Stream);

  Stream.WriteString(sLinebreak)
}
End;
{______________________________________________________________________________}
function TFXMetaTable.AsDropDDL:String;
Begin
  Result:=Format('DROP TABLE %s',[Self.QuotedName]);
End;
{______________________________________________________________________________}
function TFXMetaTable.HasPrimaryKeys:Boolean;
Begin
  Result:=(fPrimaryKeys<>nil)and(fPrimaryKeys.Count>0)
End;
{______________________________________________________________________________}
function TFXMetaTable.OrderByPrimaryKeys:String;
Var FieldNode:TFXCustomMetaNode;
    MetaNode:TFXMetaPrimaryKey;
    i:Integer;
Begin
  Result:=EmptyStr;
  if (fPrimaryKeys<>nil)and(fPrimaryKeys.Count>0) then Begin
    MetaNode:=TFXMetaPrimaryKey(fPrimaryKeys[0]);
    for i:=0 to Pred(MetaNode.fFields.Count) do Begin
      FieldNode:=TFXMetaPrimaryKey(MetaNode.fFields[i]);
      if Result=EmptyStr then
        Result:=FieldNode.Name else
        Result:=Result+','+FieldNode.Name
    end end
End;
{______________________________________________________________________________}
procedure TFXMetaTable.ClearForeignKeys;
var idx:TFXMetaForeignKey;
    i:Integer;
Begin
  if fForeignKeys<>nil then Begin
    for i:=0 to Pred(fForeignKeys.Count) do Begin
      idx:=TFXMetaForeignKey(fForeignKeys[i]);
      if fSchema.fForeignKeys<>nil then Begin
        fSchema.fForeignKeys.Remove(idx);
      end end;
    fForeignKeys.Clear;
    end;
End;
{______________________________________________________________________________}
procedure TFXMetaTable.DownloadForeignKeys(Const aSQL1,aSQL2:TFXCustomSQL);
var idx:TFXMetaForeignKey;
    DidActivate:Boolean;
Begin
  Self.ClearForeignKeys;
  DidActivate:=aSQL1.Start_TR;
  TFXMetaForeignKey.PrepareQuery(fName,aSQL1);
  aSQL1.ParamCheck:=False;
  aSQL1.ExecQuery;
  While Not aSQL1.Eof do Begin
    idx:=TFXMetaForeignKey.CreateFromSQL(fSchema,Self,aSQL1);
    idx.Download(aSQL2);
    aSQL1.Next;
    end;
  aSQL1.CloseQuery;
  if DidActivate then
    aSQL1.Commit_TR;
End;
{______________________________________________________________________________}
procedure TFXMetaTable.ClearIndexes;
var idx:TFXMetaIndex;
    i:Integer;
Begin
  if fIndexes<>nil then Begin
    for i:=0 to Pred(fIndexes.Count) do Begin
      idx:=TFXMetaIndex(fIndexes[i]);
      if fSchema.fIndexes<>nil then Begin
        fSchema.fIndexes.Remove(idx);
      end end;
    fIndexes.Clear;
    end;
End;
{______________________________________________________________________________}
procedure TFXMetaTable.DownloadIndexes(Const aSQL1,aSQL2:TFXCustomSQL);
var DidActivate:Boolean;
    idx:TFXMetaIndex;
Begin
  Self.ClearIndexes;
  DidActivate:=aSQL1.Start_TR;
  TFXMetaIndex.PrepareQuery(fName,aSQL1);
  aSQL1.ParamCheck:=False;
  aSQL1.ExecQuery;
  While Not aSQL1.Eof do Begin
    idx:=TFXMetaIndex.CreateFromSQL(fSchema,Self,aSQL1);
    idx.Download(aSQL2);
    aSQL1.Next;
    end;
  aSQL1.CloseQuery;
  if DidActivate then
    aSQL1.Commit_TR;
End;
{______________________________________________________________________________}
procedure TFXMetaTable.IndexesOff(Const aSQL:TFXCustomSQL;Const OnTrace:TFXMetaTrace);
  {____________________________________________________________________________}
  procedure Trace(Const aMsg:String);
  Begin
    if Assigned(OnTrace) Then
      OnTrace(aMsg)
  end;
Var MetaNode:TFXMetaIndex;
    query:String;
    i:Integer;
Begin
  if fIndexes<>nil Then Begin
    for i:=0 to Pred(fIndexes.Count) do Begin
      MetaNode:=TFXMetaIndex(fIndexes[i]);
      if MetaNode.fActive then Begin
        query:=MetaNode.AsOff;
        Trace(query);
        aSQL.ReStart_TR;
        aSQL.SQL.Text:=query;
        aSQL.ExecQuery;
        aSQL.CloseCursor;
        aSQL.Commit_TR;
      end else Begin
        query:=MetaNode.Name+' allready off';
        Trace(query);
    end end end;
End;
{______________________________________________________________________________}
procedure TFXMetaTable.IndexesOn(Const aSQL:TFXCustomSQL;Const OnTrace:TFXMetaTrace);
  {____________________________________________________________________________}
  procedure Trace(Const aMsg:String);
  Begin
    if Assigned(OnTrace) Then
      OnTrace(aMsg)
  end;
Var MetaNode:TFXMetaIndex;
    query:String;
    i:Integer;
Begin
  if fIndexes<>nil Then Begin
    for i:=0 to Pred(fIndexes.Count) do Begin
      MetaNode:=TFXMetaIndex(fIndexes[i]);
      if not MetaNode.fActive then Begin
        query:=MetaNode.AsOn;
        Trace(query);
        aSQL.ReStart_TR;
        aSQL.SQL.Text:=query;
        aSQL.ExecQuery;
        aSQL.CloseCursor;
        aSQL.Commit_TR;
      end else Begin
        query:=MetaNode.Name+' allready on';
        Trace(query);
    end end end;
End;
{______________________________________________________________________________}
procedure TFXMetaTable.RecomputeIndexes(Const aSQL:TFXCustomSQL;Const OnTrace:TFXMetaTrace);
  {____________________________________________________________________________}
  procedure Trace(Const aMsg:String);
  Begin
    if Assigned(OnTrace) Then
      OnTrace(aMsg)
  end;
Var MetaNode:TFXCustomRelationMetaNode;
    query:String;
    i:Integer;
Begin
  if fPrimaryKeys<>nil Then Begin
    for i:=0 to Pred(fPrimaryKeys.Count) do Begin
      MetaNode:=TFXCustomRelationMetaNode(fPrimaryKeys[i]);
      query:=(MetaNode as TFXMetaPrimaryKey).AsRecomputeDDL;
      Trace('..'+query);
      aSQL.ReStart_TR;
      aSQL.SQL.Text:=query;
      aSQL.ExecQuery;
      aSQL.CloseCursor;
      aSQL.Commit_TR;
    end end;

  if fForeignKeys<>nil Then Begin
    for i:=0 to Pred(fForeignKeys.Count) do Begin
      MetaNode:=TFXCustomRelationMetaNode(fForeignKeys[i]);
      query:=(MetaNode as TFXMetaForeignKey).AsRecomputeDDL;
      Trace(query);
      aSQL.ReStart_TR;
      aSQL.SQL.Text:=query;
      aSQL.ExecQuery;
      aSQL.CloseCursor;
      aSQL.Commit_TR;
    end end;

  if fIndexes<>nil Then Begin
    for i:=0 to Pred(fIndexes.Count) do Begin
      MetaNode:=TFXCustomRelationMetaNode(fIndexes[i]);
      query:=(MetaNode as TFXMetaIndex).AsRecomputeDDL;
      Trace(query);
      aSQL.ReStart_TR;
      aSQL.SQL.Text:=query;
      aSQL.ExecQuery;
      aSQL.CloseCursor;
      aSQL.Commit_TR;
    end end;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaView.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fMetaObject    := moView;
  fFields        := TFXList.Create;

  fName          := aSQL.Fields[_QRY_Table_Name_ ].AsTrimString;
  fDescr         := aSQL.Fields[_QRY_Table_Descr_].AsTrimString;

  if fSchema.fViews=nil then
    fSchema.fViews:=TList.Create;
  fSchema.fViews.Add(Self);
  
  fSchema.fNodes.Add(Self);
End;
{______________________________________________________________________________}
Class procedure TFXMetaView.PrepareQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Views_+'  and (NOT REL.RDB$FLAGS IS NULL)AND(REL.RDB$SECURITY_CLASS STARTING WITH ''SQL$'')and(REL.RDB$SYSTEM_FLAG <> 1 OR REL.RDB$SYSTEM_FLAG IS NULL) ORDER BY REL.RDB$RELATION_NAME';
End;
{______________________________________________________________________________}
Class procedure TFXMetaView.PrepareQuery(Const aName : String;Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Views_+format('  and (REL.RDB$RELATION_NAME=%s)',[AnsiQuotedStr(DoQuoteName(aName),'''')]);
End;
{______________________________________________________________________________}
procedure TFXMetaView.Download(Const aSQL1,aSQL2:TFXCustomSQL);
var DidActivate:Boolean;
begin
  if fFields<>nil then
    fFields.Clear;

  DidActivate:=aSQL1.Start_TR;
  aSQL1.SQL.Text:=
    _QRY_Table_Fields_+
     format('WHERE (RFR.RDB$RELATION_NAME=%s)',[AnsiQuotedStr(Self.QuotedName,'''')])+sLinebreak+
    'ORDER BY RFR.RDB$FIELD_POSITION, RFR.RDB$FIELD_NAME';
  aSQL1.ParamCheck:=False;
  aSQL1.ExecQuery;
  While Not aSQL1.Eof do Begin
    TFXMetaTableField.CreateFromSQL(Self,aSQL1,aSQL2);
    aSQL1.Next;
    end;
  aSQL1.CloseQuery;

  if DidActivate then
    aSQL1.Commit_TR;
end;
{______________________________________________________________________________}
procedure TFXMetaView.WriteCreateDDL(Const Stream: TStringStream);
Begin
{
Var Field:TFXMetaField;
    vn,p:String;
    i:Integer;
begin
  p:=StringOfChar(' ',5);
  vn:=QuoteIdentifier(fName);
  Stream.WriteString(Format('CREATE VIEW %s', [vn]));
  if (fFields<>nil)and(fFields.Count>0) Then Begin
    Field:=TFXMetaField(fFields[0]);
    Stream.WriteString(' ('+sLinebreak+p+Field.Name);
    for I := 1 to Pred(fFields.Count) do Begin
      Field:=TFXMetaField(fFields[i]);
      Stream.WriteString(','+sLinebreak+p+Field.Name);
      end;
    Stream.WriteString(sLinebreak+ ')');
    end;
  Stream.WriteString(' AS'+sLinebreak);
  Stream.WriteString(fSource);
  WriteTerminator(Stream);

  if OIDComment in fExtractor.fFlags Then Begin
    if fDescr<>'' Then Begin
      Stream.WriteString(Format('COMMENT ON VIEW %s IS'+sLinebreak+'%s',[vn,AnsiQuotedStr(fDescr,'''')]));
      WriteTerminator(Stream);
    end end;

  WriteDependencies(Stream);

  Stream.WriteString(sLinebreak)
}
End;
{______________________________________________________________________________}
function TFXMetaView.AsDropDDL:String;
Begin
  Result:=Format('DROP VIEW %s',[Self.QuotedName]);
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaProcInField.CreateFromSQL(Const aOwner:TFXMetaProcedure;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aOwner.fSchema;
  fOwner         := aOwner;
  fMetaObject    := moProcInParam;

  fName          := aSQL.Fields[_QRY_Proc_Field_ParamName].AsTrimString;
  AssignFromSQL(_QRY_Proc_Field_Type,aSQL);
  AssignBlobSegmentFromSQL(_QRY_Proc_Field_SegLen,aSQL);
  // TODO Domain !!!

  if aOwner.fInputParams=nil then
    aOwner.fInputParams:=TFXList.Create;
  aOwner.fInputParams.Add(Self);
end;
{______________________________________________________________________________}
procedure TFXMetaProcInField.WriteCreateDDL(Const Stream: TStringStream);
Var l:Integer;
    s:String;
Begin
  if Self.GetQuotedName(s) then
    s:=' '+s else
    s:='  '+s;
  l:=System.Length(s);
  Stream.WriteString(s);
  Stream.WriteString(StringOfChar(' ',35-l));
  Stream.WriteString(Self.FieldTypeDLL);
end;
{______________________________________________________________________________}
constructor TFXMetaProcOutField.CreateFromSQL(Const aOwner:TFXMetaProcedure;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aOwner.fSchema;
  fOwner         := aOwner;
  fMetaObject    := moProcOutParam;

  fName          := aSQL.Fields[_QRY_Proc_Field_ParamName].AsTrimString;
  AssignFromSQL(_QRY_Proc_Field_Type,aSQL);
  AssignBlobSegmentFromSQL(_QRY_Proc_Field_SegLen,aSQL);
  // TODO Domain !!!

  if aOwner.fOutputParams=nil then
    aOwner.fOutputParams:=TFXList.Create;
  aOwner.fOutputParams.Add(Self);
end;
{______________________________________________________________________________}
procedure TFXMetaProcOutField.WriteCreateDDL(Const Stream: TStringStream);
Var l:Integer;
    s:String;
Begin
  if Self.GetQuotedName(s) then
    s:=' '+s else
    s:='  '+s;
  l:=System.Length(s);
  Stream.WriteString(s);
  Stream.WriteString(StringOfChar(' ',35-l));
  Stream.WriteString(Self.FieldTypeDLL);
end;
{______________________________________________________________________________}
constructor TFXMetaProcedure.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fOwner         := Self;
  fMetaObject    := moProcedure;

  fName          := aSQL.Fields[_QRY_Proc_Name  ].AsTrimString;
  fDescr         := aSQL.Fields[_QRY_Proc_Descr ].AsTrimString;

  if fSchema.fProcs=nil then
    fSchema.fProcs:=TList.Create;
  fSchema.fProcs.Add(Self);
  
  fSchema.fNodes.Add(Self);
  Assert(fDependenciesX=nil);
end;
{______________________________________________________________________________}
destructor TFXMetaProcedure.Destroy;
Begin
  fOutputParams.Free;
  fInputParams.Free;
  inherited;
end;
{______________________________________________________________________________}
Class procedure TFXMetaProcedure.PrepareQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Procs_;
End;
{______________________________________________________________________________}
procedure TFXMetaProcedure.DownloadParams(Const aSQL:TFXCustomSQL);
Var DidActivate:Boolean;
    ft:Integer;
Begin
  Assert(fSchema.fDB<>nil);
  Assert(fSchema.fTRx<>nil);
  Assert(fDependenciesX=nil);
  if fInputParams<>nil then
    fInputParams.Clear;
  if fOutputParams<>nil then
    fOutputParams.Clear;

  DidActivate:=aSQL.Start_TR;
  aSQL.Params[0].AsTrimString:=FName;
  aSQL.ExecQuery;
  While Not aSQL.Eof do Begin
    ft:= aSQL.Fields[_QRY_Proc_Field_ParamType].AsInteger;
    if ft=0 Then
      TFXMetaProcInField.CreateFromSQL(Self,aSQL) else
      TFXMetaProcOutField.CreateFromSQL(Self,aSQL);
    aSQL.Next;
    end;
  aSQL.CloseQuery;

  if DidActivate then
    aSQL.Commit_TR;
end;
{______________________________________________________________________________}
procedure TFXMetaProcedure.WriteHeaderDDL(Const Stream: TStringStream);
Var pof:TFXMetaProcOutField;
    pif:TFXMetaProcInField;
    i:Integer;
    p:String;
Begin
  p:='  ';
  if (fInputParams<>nil)and(fInputParams.Count>0) Then Begin
    Stream.WriteString('('+sLinebreak+p);
    pif:=TFXMetaProcInField(fInputParams[0]);
    pif.WriteCreateDDL(Stream);
    for i:=1 to Pred(fInputParams.Count) do begin
      pif:=TFXMetaProcInField(fInputParams[i]);
      Stream.WriteString(','+sLinebreak+p);
      pif.WriteCreateDDL(Stream)
      end;
    Stream.WriteString(sLinebreak+')');
    end;
  if (fOutputParams<>nil)and(fOutputParams.Count>0) Then Begin
    Stream.WriteString('RETURNS('+sLinebreak+p);
    pof:=TFXMetaProcOutField(fOutputParams[0]);
    pof.WriteCreateDDL(Stream);
    for i:=1 to Pred(fOutputParams.Count) do begin
      pof:=TFXMetaProcOutField(fOutputParams[i]);
      Stream.WriteString(','+sLinebreak+p);
      pof.WriteCreateDDL(Stream)
      end;
    Stream.WriteString(sLinebreak+')');
    end;
  Stream.WriteString(sLinebreak+'AS'+sLinebreak);
end;
{______________________________________________________________________________}
procedure TFXMetaProcedure.WriteCreateDDL(Const Stream: TStringStream);
Begin
  Stream.WriteString('CREATE OR ALTER PROCEDURE '+Self.QuotedName+' ');
  WriteHeaderDDL(Stream);
  if fSource='' Then Begin
    Stream.WriteString('Begin'+sLinebreak);
    Stream.WriteString('  /* Missing Source */'+sLinebreak);
    Stream.WriteString('  exit;'+sLinebreak);
    Stream.WriteString('end'+sLinebreak);
  end else
    Stream.WriteString(fSource+sLinebreak);

  WriteDependencies(Stream);

  WriteTerminator(Stream);
end;
{______________________________________________________________________________}
function TFXMetaProcedure.AsDummyAlterDDL:String;
var Stream:TStringStream;
begin
  Stream := TStringStream.Create('');
  try Stream.WriteString('ALTER PROCEDURE '+Self.QuotedName+' ');
      WriteHeaderDDL(Stream);
      Stream.WriteString('Begin'+sLinebreak);
      if (fOutputParams<>nil)and(fOutputParams.Count>0) Then
        Stream.WriteString('  Suspend;'+sLinebreak) else
        Stream.WriteString('  exit;'+sLinebreak);
      Stream.WriteString('end');
      Result := Stream.DataString;
  finally
      Stream.Free
  end
end;
{______________________________________________________________________________}
function TFXMetaProcedure.AsDropDDL:String;
begin
  Result:='DROP PROCEDURE '+Self.QuotedName;
end;
{______________________________________________________________________________}
function TFXCustomMetaSchema.FindProc(Const aName:String;Out FoundProc:TFXCustomMetaNode):Boolean;
Var MetaProc:TFXMetaProcedure;
    i:Integer;
Begin
  if fProcs<>nil then Begin
    for i:=0 to Pred(fProcs.Count) do Begin
      MetaProc:=TFXMetaProcedure(fProcs[i]);
      if SameText(MetaProc.FName,aName) then Begin
        FoundProc:=MetaProc;
        Result:=True;
        exit;
    end end end;
  FoundProc:=nil;
  Result:=False;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaUDFInField.CreateFromSQL(Const aUDF:TFXMetaUDF;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aUDF.fSchema;
  fOwner         := aUDF;
  fMetaObject    := moUDFInParam;

  AssignFromSQL(_QRY_UDF_Field_Type,aSQL);
  fName          :='FIELD '+aSQL.Fields[_QRY_UDF_Field_Position].AsTrimString;
  fPosition      := aSQL.Fields[_QRY_UDF_Field_Position ].AsInteger;
  fMechanism     := aSQL.Fields[_QRY_UDF_Field_Mechanism].AsInteger;

  if aUDF.fInputParams=nil then
    aUDF.fInputParams:=TFXList.Create;
  aUDF.fInputParams.Add(Self);
end;
{______________________________________________________________________________}
procedure TFXMetaUDFInField.WriteCreateDDL(Const Stream: TStringStream);
begin
  Case fFieldType of
    uftBlobData:Begin
      Stream.WriteString('BLOB');
      case fMechanism of
        -1:Stream.WriteString(' FREE_IT');
        //0=BY_VALUE');
         0:Stream.WriteString(' BY VALUE');
         //1=BY_REFERENCE
         1:Stream.WriteString(' BY REFERENCE');
         //2=BY_VMS_DESCRIPTOR
         2:Stream.WriteString(' BY DESCRIPTOR');
         //3=BY_ISC_DESCRIPTOR,It is the default for Blob
         3:;
         //5=BY_REFERENCE_WITH_NULL
         5:Stream.WriteString(' BY REFERENCE NULL');
         else Begin
           Raise Exception.CreateFmt('Unknow UDF <%s> Param Mechanism <%d>',[fName,fMechanism]);
      end end end;
    else Begin
      Stream.WriteString(Self.FieldTypeDLL);
      case fMechanism of
         //-1 Result FREE_IT
        -1:Stream.WriteString(' FREE_IT');
        //0=BY_VALUE');
         0:Stream.WriteString(' BY VALUE');
         //1=BY_REFERENCE,It is the default
         1:;
         //2=BY_VMS_DESCRIPTOR
         2:Stream.WriteString(' BY DESCRIPTOR');
         //3=BY_ISC_DESCRIPTOR
         3:Raise Exception.CreateFmt('Unknow UDF <%s> Param Mechanism <3=BY_ISC_DESCRIPTOR>',[fName]);
         //4=BY_SCALAR_ARRAY_DESCRIPTOR
         4:Raise Exception.CreateFmt('Unknow UDF <%s> Param Mechanism <4=BY_SCALAR_ARRAY_DESCRIPTOR>',[fName]);
         //5=BY_REFERENCE_WITH_NULL
         5:Stream.WriteString(' NULL');
         else Begin
           Raise Exception.CreateFmt('Unknow UDF <%s> Param Mechanism <%d>',[fName,fMechanism]);
    end end end end;
end;
{______________________________________________________________________________}
constructor TFXMetaUDF.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fOwner         := Self;
  fMetaObject    := moUDF;

  fName          := aSQL.Fields[_QRY_UDF_Name       ].AsTrimString;
  fModule        := aSQL.Fields[_QRY_UDF_ModuleName ].AsTrimString;
  fEntry         := aSQL.Fields[_QRY_UDF_EntryPoint ].AsTrimString;
  fReturn        := aSQL.Fields[_QRY_UDF_ReturnArg  ].AsInteger;
  fDescr         := aSQL.Fields[_QRY_UDF_DESCRIPTION].AsTrimString;

  if fSchema.fUDFs=nil then
    fSchema.fUDFs:=TList.Create;
  fSchema.fUDFs.Add(Self);

  fSchema.fNodes.Add(Self);
end;
{______________________________________________________________________________}
destructor TFXMetaUDF.Destroy;
Begin
  fInputParams.Free;
  inherited;
end;
{______________________________________________________________________________}
Class procedure TFXMetaUDF.PrepareQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_UDFs_;
End;
{______________________________________________________________________________}
procedure TFXMetaUDF.DownloadParams(Const aSQL:TFXCustomSQL);
Var DidActivate:Boolean;
Begin
  if fInputParams<>nil then
    fInputParams.Clear;

  DidActivate:=aSQL.Start_TR;
  aSQL.Params[0].AsTrimString:=FName;
  aSQL.ExecQuery;
  While Not aSQL.Eof do Begin
    TFXMetaUDFInField.CreateFromSQL(Self,aSQL);
    aSQL.Next;
    end;
  aSQL.CloseQuery;
  
  if DidActivate then
    aSQL.Commit_TR;
end;
{______________________________________________________________________________}
procedure TFXMetaUDF.WriteCreateDDL(Const Stream: TStringStream);
Var UDFInField:TFXMetaUDFInField;
    I,C:Integer;
Begin
  Stream.WriteString(Format('DECLARE EXTERNAL FUNCTION %s', [Self.QuotedName]));
  if (fInputParams<>nil)and(fInputParams.Count>0) Then Begin
    C := 0;
    if fReturn=0 then begin
      // return position
      for I := 0 to Pred(fInputParams.Count) do Begin
        UDFInField:=TFXMetaUDFInField(fInputParams[i]);
        if UDFInField.fPosition<>fReturn then begin
          if C > 0 then
            Stream.WriteString(','+sLinebreak+'        ') else
            Stream.WriteString(sLinebreak+'        ');
          UDFInField.WriteCreateDDL(Stream);
          Inc(C);
        end end;
      for I := 0 to Pred(fInputParams.Count) do Begin
        UDFInField:=TFXMetaUDFInField(fInputParams[i]);
        if UDFInField.fPosition=fReturn then begin
          Stream.WriteString(sLinebreak+'RETURNS ');
          UDFInField.WriteCreateDDL(Stream);
          Break;
        end end;
    end else begin
      for I := 0 to Pred(fInputParams.Count) do Begin
        UDFInField:=TFXMetaUDFInField(fInputParams[i]);
        if C > 0 then
          Stream.WriteString(','+sLinebreak+'        ') else
          Stream.WriteString(sLinebreak+'        ');
        UDFInField.WriteCreateDDL(Stream);
        Inc(C);
        end;
      Stream.WriteString(Format(sLinebreak+'RETURNS PARAMETER %d', [fReturn]));
    end end;
  Stream.WriteString(Format(sLinebreak+'ENTRY_POINT ''%s'' MODULE_NAME ''%s''',[FEntry,FModule]));
  WriteTerminator(Stream);

  if fDescr<>'' Then Begin
    Stream.WriteString(Format('COMMENT ON EXTERNAL FUNCTION %s IS'+sLinebreak+'%s',[Self.QuotedName,AnsiQuotedStr(fDescr,'''')]));
    WriteTerminator(Stream);
    end;

  WriteDependencies(Stream);

  Stream.WriteString(sLinebreak);
end;
{______________________________________________________________________________}
function TFXMetaUDF.AsDropDDL:String;
Begin
  Result:=Format('DROP EXTERNAL FUNCTION %s', [Self.QuotedName]);
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaException.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fOwner         := Self;
  fMetaObject    := moException;

  fName          := aSQL.Fields[_QRY_Exception_Name   ].AsTrimString;
  fDescr         := aSQL.Fields[_QRY_Exception_Descr  ].AsTrimString;
  fMessage       := aSQL.Fields[_QRY_Exception_Message].AsTrimString;
  fNumber        := aSQL.Fields[_QRY_Exception_Number ].AsInteger;

  if fSchema.fExceptions=nil then
    fSchema.fExceptions:=TList.Create;
  fSchema.fExceptions.Add(Self);

  fSchema.fNodes.Add(Self);
end;
{______________________________________________________________________________}
Class procedure TFXMetaException.PrepareQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Exceptions_;
End;
{______________________________________________________________________________}
procedure TFXMetaException.WriteCreateDDL(Const Stream: TStringStream);
begin
  Stream.WriteString(Format('CREATE EXCEPTION %s %s', [AnsiQuotedStr(FName,'"'),AnsiQuotedStr(FMessage,'''')]));
  WriteTerminator(Stream);

  if fDescr<>'' Then Begin
    Stream.WriteString(Format('COMMENT ON EXCEPTION %s IS'+sLinebreak+'%s',[AnsiQuotedStr(fName,'"'),AnsiQuotedStr(fDescr,'''')]));
    WriteTerminator(Stream);
    end;

  Stream.WriteString(sLinebreak)
end;
{______________________________________________________________________________}
function TFXMetaException.AsDropDDL:String;
Begin
  Result:=Format('DROP EXCEPTION %s',[Self.QuotedName]);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
Class procedure TFXMetaGenerator.PrepareQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Generators_+'ORDER BY 1';
  aSQL.ParamCheck:=False;
End;
{______________________________________________________________________________}
constructor TFXMetaGenerator.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fOwner         := Self;
  fMetaObject    := moGenerator;

  fName          := aSQL.Fields[_QRY_Generators_Name  ].AsTrimString;

  if fSchema.fGenerators=nil then
    fSchema.fGenerators:=TList.Create;
  fSchema.fGenerators.Add(Self);

  fSchema.fNodes.Add(Self);
end;
{______________________________________________________________________________}
procedure TFXMetaGenerator.WriteCreateDDL(Const Stream: TStringStream);
begin
  Stream.WriteString(Format('CREATE SEQUENCE %s', [AnsiQuotedStr(FName,'"')]));
  WriteTerminator(Stream);
  Stream.WriteString(sLinebreak)
end;
{______________________________________________________________________________}
procedure TFXCustomMetaSchema.ClearSequences;
Begin
  if fGenerators<>nil then
    fGenerators.Clear;
  fSequencesLoaded:=False;
End;
{______________________________________________________________________________}
procedure TFXCustomMetaSchema.DownloadSequences(Const aTR:TFXCustomTransaction;Const ForceReload:Boolean);
var SavTR:TFXCustomTransaction;
    DidActivate:Boolean;
begin
  if ForceReload then
    Self.ClearSequences;

  if not fSequencesLoaded then Begin
    SavTR:=fTRx;
    fSequencesLoaded:=True;
    DidActivate:=False;
    try Self.GetSQL1;
        if aTR<>nil then
          SetTr(aTR) else
        if fTRx=nil then
          SetTr(fDB.ROAutoTR);
        DidActivate:=fSQL1.Start_TR;
        // Sequences
        TFXMetaGenerator.PrepareQuery(fSQL1);
        fSQL1.ParamCheck:=False;
        fSQL1.ExecQuery;
        While Not fSQL1.Eof do Begin
          TFXMetaGenerator.CreateFromSQL(Self,fSQL1);
          fSQL1.Next;
          end;
        fSQL1.CloseQuery;
    finally
        if DidActivate then
          fSQL1.Commit_TR;
        SetTr(SavTR)
    end end;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaRole.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fOwner         := Self;
  fMetaObject    := moRole;

  fName          := aSQL.Fields[_QRY_Role_ROLE_NAME ].AsTrimString;
  fOwnerName     := aSQL.Fields[_QRY_Role_OWNER_NAME].AsTrimString;

  if fSchema.fRoles=nil then
    fSchema.fRoles:=TList.Create;
  fSchema.fRoles.Add(Self);

  fSchema.fNodes.Add(Self);
end;
{______________________________________________________________________________}
Class procedure TFXMetaRole.PrepareQuery(Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Roles_;
End;
{______________________________________________________________________________}
Class procedure TFXMetaRole.PrepareQuery(Const aName : String;Const aSQL:TFXCustomSQL);
Begin
  aSQL.SQL.Text:=_QRY_Roles_+format('Where (ROL.RDB$ROLE_NAME=%s)',[AnsiQuotedStr(DoQuoteName(aName),'''')]);
End;
{______________________________________________________________________________}
procedure TFXMetaRole.WriteCreateDDL(Const Stream: TStringStream);
begin

end;
{______________________________________________________________________________}
function TFXMetaRole.IsADMIN:Boolean;
Begin
  Result:=SameText('RDB$ADMIN',fName);
End;
{______________________________________________________________________________}
function TFXMetaRole.AsDropDDL:String;
Begin
  Result:=Format('DROP ROLE %s',[Self.QuotedName]);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXMetaCharSet.CreateFromSQL(Const aSchema:TFXCustomMetaSchema;Const aSQL:TFXCustomSQL);
Begin
  fSchema        := aSchema;
  fMetaObject    := moCharSet;

  fID            := aSQL.Fields[_QRY_Charset_CharSet     ].AsInteger;
  fName          := aSQL.Fields[_QRY_Charset_Name        ].AsTrimString;
  fBytesPerChar  := aSQL.Fields[_QRY_Charset_BytesPerChar].AsInteger;

  if fSchema.fCharsets=nil then
    fSchema.fCharsets:=TList.Create;
  fSchema.fCharsets.Add(Self);

  fSchema.fNodes.Add(Self);
End;
{______________________________________________________________________________}
constructor TFXMetaCharSet.CreateNone(Const aSchema:TFXCustomMetaSchema);
Begin
  fSchema        := aSchema;
  fMetaObject    := moCharSet;

  fID            := 0;
  fName          :='NONE';
  fBytesPerChar  := 1;

  if fSchema.fCharsets=nil then
    fSchema.fCharsets:=TList.Create;
  fSchema.fCharsets.Add(Self);

  fSchema.fNodes.Add(Self);
End;
{______________________________________________________________________________}
procedure TFXMetaCharSet.WriteCreateDDL(Const Stream: TStringStream);
Begin

End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
destructor TFXCustomMetaSchema.Destroy;
begin
  fSQLCharSet.Free;
  fSQL3.Free;
  fSQL2.Free;
  fSQL1.Free;
  inherited
end;
{______________________________________________________________________________}
procedure TFXCustomMetaSchema.Clear;
begin
  FreeAndNil(fSQLCharSet);
  FreeAndNil(fSQL3);
  FreeAndNil(fSQL2);
  FreeAndNil(fSQL1);

  fSequencesLoaded:=False;
  fLoaded:=False;

  inherited
end;
{______________________________________________________________________________}
function TFXCustomMetaSchema.GetSQL1:TFXCustomSQL;
Begin
  if fSQL1=nil then Begin
    fSQL1:=TFXCustomSQL.Create(nil);
    With fSQL1 do Begin
      Database   := fDB;
      Transaction:= fTRx;
    end end;
  Result:=fSQL1
End;
{______________________________________________________________________________}
function TFXCustomMetaSchema.GetSQL2:TFXCustomSQL;
Begin
  if fSQL2=nil then Begin
    fSQL2:=TFXCustomSQL.Create(nil);
    fSQL2.Database   := fDB;
    fSQL2.Transaction:= fTRx;
    end;
  Result:=fSQL2
End;
{______________________________________________________________________________}
function TFXCustomMetaSchema.GetSQL3:TFXCustomSQL;
Begin
  if fSQL3=nil then Begin
    fSQL3:=TFXCustomSQL.Create(nil);
    fSQL3.Database   := fDB;
    fSQL3.Transaction:= fTRx;
    end;
  Result:=fSQL3
End;
{______________________________________________________________________________}
function TFXCustomMetaSchema.GetCharSet(Const aID:Integer):TFXMetaCharSet;
Var DidActivate:Boolean;
Begin
  if fSQLCharSet=nil then Begin
    fSQLCharSet:=TFXCustomSQL.Create(nil);
    fSQLCharSet.SQL.Text:=_QRY_Charset_;
    fSQLCharSet.Database   := fDB;
    fSQLCharSet.Transaction:= fTRx;
    end;
  DidActivate := fSQLCharSet.Start_TR;
  fSQLCharSet.Prepare;
  fSQLCharSet.Params[0].AsInteger:=aID;
  fSQLCharSet.ExecQuery;
  if Not fSQLCharSet.Eof then
    result:=TFXMetaCharSet.CreateFromSQL(Self,fSQLCharSet) else
    result:=nil;
  fSQLCharSet.CloseCursor;
  if DidActivate then
    fSQLCharSet.Commit_TR;
end;
{______________________________________________________________________________}
procedure TFXCustomMetaSchema.SetTr(Const aTRx:TFXCustomTransaction);
Begin
  if fSQLCharSet<>nil then fSQLCharSet.Transaction:=aTRx;
  if fSQL3<>nil then fSQL3.Transaction:=aTRx;
  if fSQL2<>nil then fSQL2.Transaction:=aTRx;
  if fSQL1<>nil then fSQL1.Transaction:=aTRx;
  fTRx:=aTRx;
End;
{______________________________________________________________________________}
procedure TFXCustomMetaSchema.Download(Const aTR:TFXCustomTransaction;Const ForceReload:Boolean);
var SavTR:TFXCustomTransaction;
    rv:TFXCustomMetaRelation;
    pk:TFXMetaPrimaryKey;
    DidActivate:Boolean;
    s:String;
begin
  if ForceReload then
    Self.Clear;

  if not fLoaded then Begin
    SavTR:=fTRx;
    fLoaded:=True;
    DidActivate:=False;
    try Self.GetSQL1;
        Self.GetSQL2;
        if aTR<>nil then
          SetTr(aTR) else
        if fTRx=nil then
          SetTr(fDB.ROAutoTR);
        DidActivate:=fSQL1.Start_TR;
        // Tables
        TFXMetaTable.PrepareQuery(fSQL1);
        fSQL1.ParamCheck:=False;
        fSQL1.ExecQuery;
        While Not fSQL1.Eof do Begin
          TFXMetaTable.CreateFromSQL(Self,fSQL1);
          fSQL1.Next;
          end;
        fSQL1.CloseQuery;
        // Views
        TFXMetaView.PrepareQuery(fSQL1);
        fSQL1.ParamCheck:=False;
        fSQL1.ExecQuery;
        While Not fSQL1.Eof do Begin
          TFXMetaView.CreateFromSQL(Self,fSQL1);
          fSQL1.Next;
          end;
        fSQL1.CloseQuery;
        // Fields
        rv:=nil;
        TFXMetaTableField.PrepareQuery(fSQL1);
        fSQL1.ParamCheck:=False;
        fSQL1.ExecQuery;
        While Not fSQL1.Eof do Begin
          s:= fSQL1.Fields[0].AsTrimString;
          if (rv=nil)or(not SameText(rv.Name,s)) then Begin
            rv:=Self.FindRelation(s);
            end;
          if rv=nil then
            rv:=Self.CreateMissingRelation(s);
          TFXMetaTableField.CreateFromSQL(rv,fSQL1,fSQL2);
          fSQL1.Next;
          end;
        fSQL1.CloseQuery;
        // Primary keys
        rv:=nil;
        TFXMetaPrimaryKey.PrepareQuery(False,fSQL1);
        fSQL1.ParamCheck:=False;
        fSQL1.ExecQuery;
        While Not fSQL1.Eof do Begin
          s:= fSQL1.Fields[0].AsTrimString;
          if (rv=nil)or(not SameText(rv.Name,s)) then
            rv:=Self.FindRelation(s);
          if (rv is TFXMetaTable) then Begin
            pk:=TFXMetaPrimaryKey.CreateFromSQL(Self,TFXMetaTable(rv),fSQL1);
            pk.Download(fSQL2);
          end else Begin
            // TODO Orphan Index ?
            TFXMetaPrimaryKey.CreateFromSQL(Self,nil,fSQL1);
            Assert(False);
            end;
          fSQL1.Next;
          end;
        fSQL1.CloseQuery;
    finally
        if DidActivate then
          fSQL1.Commit_TR;
        SetTr(SavTR)
    end end;
end;
{______________________________________________________________________________}
function TFXCustomMetaSchema.FindCharset(Const aID:Integer;Out CharSet:TFXMetaCharSet):Boolean;
Var c:TFXMetaCharSet;
    i:Integer;
Begin
  if fCharsets<>nil then Begin
    for i:=0 to Pred(fCharsets.Count) do Begin
      c:=TFXMetaCharSet(fCharsets[i]);
      if c.fID=aID then Begin
        Result:=True;
        CharSet:=c;
        exit;
    end end end;
  if aID=0 then Begin
    CharSet:=TFXMetaCharSet.CreateNone(Self);
    result:=True;
    exit;
    end;
  CharSet:=GetCharSet(aID);
  Result:=CharSet<>nil;
End;
{______________________________________________________________________________}
function TFXCustomMetaSchema.FindNode(Const aMetaObject:TFXMetaObject;Const aName:String):TFXCustomMetaNode;
Begin
  Case aMetaObject of
    moView     :Result:=Self.FindRelation(aName);
    moTable    :Result:=Self.FindRelation(aName);
    moUDF      :Result:=Self.FindUDF(aName);
    moException:Result:=Self.FindException(aName);
    moRole     :Result:=Self.FindRole(aName);
    else        Result:=nil;
    end;
End;
{______________________________________________________________________________}
function TFXCustomMetaSchema.FindCharset(Const aID:Integer):TFXMetaCharSet;
Var c:TFXMetaCharSet;
Begin
  if FindCharset(aID,c) then
    Result:=c else
    Result:=nil
End;
{______________________________________________________________________________}
function TFXCustomMetaSchema.FindRole(Const aName:String):TFXMetaRole;
Var MetaRole:TFXMetaRole;
    i:Integer;
Begin
  Result:=nil;
  if fRoles<>nil then Begin
    for i:=0 to Pred(fRoles.Count) do Begin
      MetaRole:=TFXMetaRole(fRoles[i]);
      if SameText(MetaRole.FName,aName) then Begin
        Result:=MetaRole;
        Break;
    end end end;
end;
{______________________________________________________________________________}
function TFXCustomMetaSchema.FindUDF(Const aName:String):TFXMetaUDF;
Var MetaUDF:TFXMetaUDF;
    i:Integer;
Begin
  Result:=nil;
  if fUDFs<>nil then Begin
    for i:=0 to Pred(fUDFs.Count) do Begin
      MetaUDF:=TFXMetaUDF(fUDFs[i]);
      if SameText(MetaUDF.FName,aName) then Begin
        Result:=MetaUDF;
        Break;
    end end end;
end;
{______________________________________________________________________________}
function TFXCustomMetaSchema.FindException(Const aName:String):TFXMetaException;
Var MetaException:TFXMetaException;
    i:Integer;
Begin
  Result:=nil;
  if fExceptions<>nil then Begin
    for i:=0 to Pred(fExceptions.Count) do Begin
      MetaException:=TFXMetaException(fExceptions[i]);
      if SameText(MetaException.FName,aName) then Begin
        Result:=MetaException;
        Break;
    end end end;
end;
{______________________________________________________________________________}
function TFXCustomMetaSchema.CreateMissingRelation(Const aName:String):TFXCustomMetaRelation;
Begin
  Result:=TFXMetaMissingRelation.CreateMissingRelation(Self,aName);
End;
{______________________________________________________________________________}
function TFXCustomMetaSchema.FindRelation(Const aName:String;Out aNode:TFXCustomMetaRelation):Boolean;
Var r:TFXMetaTable;
    v:TFXMetaView;
    i:Integer;
Begin
  if fTables<>nil then Begin
    for i:=0 to Pred(fTables.Count) do Begin
      r:=TFXMetaTable(fTables[i]);
      if SameText(r.fName,aName) then Begin
        aNode:=r;
        Result:=True;
        exit;
    end end end;

  if fViews<>nil then Begin
    for i:=0 to Pred(fViews.Count) do Begin
      v:=TFXMetaView(fViews[i]);
      if SameText(v.fName,aName) then Begin
        aNode:=v;
        Result:=True;
        exit;
    end end end;

  aNode:=nil;
  Result:=False;
end;
{______________________________________________________________________________}
function TFXCustomMetaSchema.FindRelation(Const aName : String) : TFXCustomMetaRelation;
Var r:TFXMetaTable;
    v:TFXMetaView;
    i:Integer;
Begin
  if fTables<>nil then Begin
    for i:=0 to Pred(fTables.Count) do Begin
      r:=TFXMetaTable(fTables[i]);
      if SameText(r.fName,aName) then Begin
        Result:=r;
        exit;
    end end end;
  if fViews<>nil then Begin
    for i:=0 to Pred(fViews.Count) do Begin
      v:=TFXMetaView(fViews[i]);
      if SameText(v.fName,aName) then Begin
        Result:=v;
        exit;
    end end end;
  Result:=nil;
end;
{______________________________________________________________________________}
function TFXCustomMetaSchema.FindRelation(Const aTR:TFXCustomTransaction;Const aName : String) : TFXCustomMetaRelation;
var SavTR:TFXCustomTransaction;
    DidActivate:Boolean;
Begin
  Result:=Self.FindRelation(aName);
  if (Result=nil)and(not fLoaded) then Begin
    if fDB.Connected then Begin
      SavTR:=fTRx;
      DidActivate:=False;
      try GetSQL1;
          if aTR<>nil then
            SetTr(aTR) else
          if fTRx<>nil then
            SetTr(fDB.ROAutoTR);
          DidActivate:=fSQL1.Start_TR;
          Self.Download(nil,True);
          Result:=Self.FindRelation(aName);
      finally
          if DidActivate then
            fSQL1.Commit_TR;
          SetTr(SavTR)
    end end end;
End;
{______________________________________________________________________________}
function TFXCustomMetaSchema.DownloadRelation(Const aName:String;Out aNode:TFXCustomMetaRelation):Boolean;
var SavTR:TFXCustomTransaction;
    r:TFXMetaTable;
    v:TFXMetaView;
Begin
  if FindRelation(aName,aNode) then Begin
    Result:=True;
    Exit;
    end;

  SavTR:=fTRx;
  try Self.GetSQL1;
      SetTr(fDB.ROAutoTR);
      fSQL1.ReStart_TR;
      TFXMetaTable.PrepareQuery(aName,fSQL1);
      fSQL1.ParamCheck:=False;
      fSQL1.ExecQuery;
      if Not fSQL1.Eof then Begin
        Self.GetSQL2;
        Self.GetSQL3;
        r:=TFXMetaTable.CreateFromSQL(Self,fSQL1);
        fSQL1.CloseQuery;
        r.Download(fSQL2,fSQL3);
        aNode:=r;
      End else Begin
        fSQL1.CloseQuery;
        TFXMetaView.PrepareQuery(aName,fSQL1);
        fSQL1.ParamCheck:=False;
        fSQL1.ExecQuery;
        if Not fSQL1.Eof then Begin
          Self.GetSQL2;
          Self.GetSQL3;
          v:=TFXMetaView.CreateFromSQL(Self,fSQL1);
          fSQL1.CloseQuery;
          v.Download(fSQL2,fSQL3);
          aNode:=v;
        end else Begin
          fSQL1.CloseQuery;
          aNode:=nil;
        end end;
      Result:=(aNode<>nil);
  finally
      fSQL1.CloseQuery;
      fSQL1.Commit_TR;
      SetTr(SavTR)
  end;
End;
{______________________________________________________________________________}
function TFXCustomMetaSchema.DownloadRelation(Const aTR:TFXCustomTransaction;Const aName:String):TFXCustomMetaRelation;
Var r:TFXMetaTable;
    v:TFXMetaView;
Begin
  if FindRelation(aName,Result) then Begin
    Exit;
    end;

  Self.GetSQL1;
  TFXMetaTable.PrepareQuery(aName,fSQL1);
  fSQL1.ParamCheck:=False;
  fSQL1.ExecQuery;
  if Not fSQL1.Eof then Begin
    Self.GetSQL2;
    Self.GetSQL3;
    r:=TFXMetaTable.CreateFromSQL(Self,fSQL1);
    r.Download(fSQL2,fSQL3);
    fSQL1.CloseQuery;
    Result:=r;
    exit;
    end;

  fSQL1.CloseQuery;
  TFXMetaView.PrepareQuery(aName,fSQL1);
  fSQL1.ParamCheck:=False;
  fSQL1.ExecQuery;
  if Not fSQL1.Eof then Begin
    Self.GetSQL2;
    Self.GetSQL3;
    v:=TFXMetaView.CreateFromSQL(Self,fSQL1);
    v.Download(fSQL2,fSQL3);
    fSQL1.CloseQuery;
    Result:=v;
    exit;
    end;
  fSQL1.CloseQuery;
  Result:=nil;
End;
{______________________________________________________________________________}
function TFXCustomMetaSchema.FindRelationField(Const aTR:TFXCustomTransaction;Const aRelation, aField : String):TFXMetaTableField;
Var rFound:TFXCustomMetaRelation;
begin
  rFound:=FindRelation(aTR,aRelation);
  if rFound<>nil then
    Result:=rFound.FindField(aField) else
    Result:=nil
end;
{______________________________________________________________________________}
procedure TFXCustomMetaSchema.TableNames(Const aList: TStrings);
Var r:TFXCustomMetaRelation;
    i:Integer;
Begin
  with aList do begin
    BeginUpdate;
    try Clear;
        for i:=0 to Pred(fTables.Count) do Begin
          r:=TFXCustomMetaRelation(fTables[i]);
          Add(r.Name)
          end;
        for i:=0 to Pred(fViews.Count) do Begin
          r:=TFXCustomMetaRelation(fViews[i]);
          Add(r.Name)
          end;
    finally
        EndUpdate;
    end end;
End;
{______________________________________________________________________________}
procedure TFXCustomMetaSchema.FieldNames(Const aList: TStrings);
Var f:TFXMetaTableField;
    i:Integer;
Begin
  with aList do begin
    BeginUpdate;
    try Clear;
        for i:=0 to Pred(fFields.Count) do Begin
          f:=TFXMetaTableField(fFields[i]);
          Add(f.Name)
          end
    finally
        EndUpdate;
    end end;
End;

end.

