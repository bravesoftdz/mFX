unit mFX.Base;

interface

{$I mFX.Inc}

uses System.Types, System.Classes, System.SysUtils, System.StrUtils, System.Math, Data.DB,
  mFX.Classes, mFX.Header, mFX.Intf, mFX.Alias, mFX.List,
  {$IFDEF mFXTRACE}mFX.Logger,{$ENDIF mFXTRACE}
  mFX.Consts, mFX.Utils;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  TFXCustomDatabase = class;
  TFXCustomTransaction = class;

  TFXSQLBase = class;

  TFXCustomSchema = Class;
  TFXCustomSchemaNode = Class;

  /// <summary>Firebird Database Connection</summary>
  TFXCustomDatabase = class abstract(TCustomConnection)
  protected
    fLibrary           : IFXClientLib;
    fHandle            : TISC_DB_HANDLE;
    fDBAlias           : TFXDBAlias;
    fDBParams          : TStrings;
    fDBParamsChanged   : Boolean;
    fDPB               : PFXByte;
    fDPBPos            : PFXByte;
    fDPBLen            : FXShort;
    fDPBAllocLen       : Integer;
    /// Internal Default Transaction , automaticly created if no fDefaultTR ,owned by the TFXCustomDatabase
    fInternalTRRWEx    : TFXCustomTransaction;
    fInternalTRROEx    : TFXCustomTransaction;
    /// List of object ref the database ...
    fBases             : TList;
    /// List of Transaction ref the database ...   w/o fInternalTRRWEx,fInternalTRROEx
    fTRs               : TList;
    fLastException     : Exception;
    fIsReadOnly        : Boolean;
    fEncoding          : TMBCSEncoding;
    {$IFDEF mFXTRACE}
    fFXLogger          : IFXLogger;
    {$ENDIF mFXTRACE}

    /// <summary>AddSQLObject</summary>
    Procedure AddSQLObject(Const Value: TFXSQLBase);
    /// <summary>RemoveSQLObject</summary>
    procedure RemoveSQLObject(Const Value:TFXSQLBase);
    /// <summary>RemoveSQLObjects</summary>
    procedure RemoveSQLObjects;
    /// <summary>NotifySQLObjects</summary>
    procedure NotifySQLObjects;

    ///<summary>return the Connection Encoding Helper</summary>
    function GetEncoding : TEncoding;
    ///<summary>return the Connection Encoding Helper</summary>
    function GetClientCharSet: TFXCharacterSet;

    ///<summary>
    ///  GetAutoTR: Internal ReadWrite TR
    ///</summary>
    function GetRWAutoTR: TFXCustomTransaction;

    ///<summary>
    ///  GetAutoTR: Internal RO Transaction
    ///</summary>
    function GetROAutoTR: TFXCustomTransaction;

    ///<summary>
    ///  GetAutoTR: Return MasterTR or RWAutoTr
    ///</summary>
    function GetAutoTR: TFXCustomTransaction;

    ///<summary>
    ///  External Assigned TR , replacing RWAutoTR
    ///</summary>
    function GetMasterTR: TFXCustomTransaction;

    ///<summary>
    ///  External Assigned TR , will replace RWAutoTR
    ///</summary>
    procedure SetMasterTR(Const Value: TFXCustomTransaction);

    ///<summary>
    ///  Notify MasterTR Change, Reset Internal Queries
    ///</summary>
    procedure MasterTRChanged(Const OldMaster,NewMaster:TFXCustomTransaction);virtual;

    /// <summary>GetTransactionCount:</summary>
    function GetTransactionCount: Integer;
    /// <summary>GetTransaction</summary>
    function GetTransaction(Const Value:Integer):TFXCustomTransaction;
    /// <summary>AddTransaction</summary>
    procedure AddTransaction(Const Value:TFXCustomTransaction);
    /// <summary>RemoveTransaction</summary>
    procedure RemoveTransaction(Const Value: TFXCustomTransaction);
    /// <summary>RemoveTransactions</summary>
    procedure RemoveTransactions;

    /// <summary>GetDBAlias</summary>
    function GetDBAlias:TFXDBAlias;inline;
    /// <summary>GetProtocol</summary>
    function GetProtocol:TFXProtocol;inline;
    /// <summary>GetFileName</summary>
    function GetFileName:String;inline;
    /// <summary>GetFileNameNoExt</summary>
    function GetFileNameNoExt:String;inline;
    /// <summary>GetFullFileName</summary>
    function GetFullFileName:String;inline;
    /// <summary>GetFullFileName NoExt</summary>
    function GetFullFileNameNoExt:String;inline;
    /// <summary>GetServer</summary>
    function GetServerName:String;inline;

    /// <summary>GetDatabaseName</summary>
    function GetDatabaseName:TFXDBName;
    /// <summary>SetDatabaseName</summary>
    procedure SetDatabaseName(const Value: TFXDBName);
    /// <summary>GetIsReadOnly:</summary>
    function GetIsReadOnly: Boolean;
    /// <summary>Laisy create Schema:</summary>
    function GetSchema:TFXCustomSchema;virtual;abstract;
    /// <summary>Get LastFirebirdError:</summary>
    function GetLastFirebirdError:String;

    /// <summary>DBParamsChange</summary>
    procedure DBParamsChange(Sender: TObject);virtual;
    /// <summary>DBParamsChanging</summary>
    procedure DBParamsChanging(Sender: TObject);virtual;
    /// <summary>SetDBParams</summary>
    procedure SetDBParams(Const Value: TStrings);

    /// <summary>Do Before Connect</summary>
    /// Tell all SQL Objects that we 're connecting
    procedure DoBeforeConnect;
    /// <summary>Do After Connect</summary>
    procedure DoAfterConnect;

    /// <summary>Do Before Disconnect</summary>
    /// Tell all connected transactions that we're disconnecting. This is so transactions can commit/rollback, accordingly
    /// Tell all SQL Objects that we're disconnecting.
    procedure DoBeforeDisconnect(Const Silent:Boolean);
    /// <summary>Do After Disconnect</summary>
    procedure DoAfterDisconnect(Const Forced:Boolean);

    /// <summary>Set DPB Len</summary>
    procedure SetDPBAllocLen(Const NewSize: Integer);
    /// <summary>Add Value to StartParams</summary>
    /// The DPB is initially empty,
    /// with the exception that the DPB version must be the first byte of the string.
    procedure StartDPB(Const param: Byte);
    /// <summary>Add Value to StartParams</summary>
    procedure AddStrParam2DPB(Const param: Byte;Const Value: String);
    /// <summary>Add Value to StartParams</summary>
    procedure AddByteParam2DPB(Const param,val: Byte);
    /// <summary>Add Value to StartParams</summary>
    procedure AddWordParam2DPB(Const param: Byte;Const Value:Word);
    /// <summary>Add Value to StartParams</summary>
    procedure AddIntParam2DPB(Const param: Byte;Const Value:Integer);
    /// <summary>Generate DPB</summary>
    ///   Given a string containing a textual representation
    ///   of the database parameters, generate a database
    ///   parameter buffer, and return it and its length
    ///   in DPB and DPBLength, respectively.
    procedure GenerateDPB;
    /// <summary>Internal Attach;</summary>
    procedure InternalAttach;
    /// <summary>Internal Detach;</summary>
    function InternalDetach:Boolean;

    {$IFDEF mFXTRACE}
    /// <summary>Trace StartOps</summary>
    procedure LogConnecting;inline;
    /// <summary>Trace StartOps</summary>
    procedure LogConnected;inline;
    /// <summary>Trace EndOps</summary>
    procedure LogDisconnecting;inline;
    /// <summary>Trace EndOps</summary>
    procedure LogDisconnected;inline;
    {$ENDIF mFXTRACE}

    ///<summary>Pop Error</summary>
    procedure PushError(Const aMsg:String);overload;

  protected
    /// <summary>Get Connected:</summary>
    function GetConnected: Boolean; override;
    /// <summary>SetConnected</summary>
    procedure SetConnected(Value: Boolean); override;
    /// <summary>Do Before Connect Event</summary>
    procedure DoBeforeConnectEvent;virtual;
    /// <summary>Do After Connect Event</summary>
    procedure DoAfterConnectEvent;virtual;
    /// <summary>Do Before Disconnect Event</summary>
    procedure DoBeforeDisconnectEvent(Const Silent:Boolean);virtual;
    /// <summary>Do After Disconnect</summary>
    procedure DoAfterDisconnectEvent(Const Forced:Boolean);virtual;
    /// <summary>DoConnect;</summary>
    procedure DoConnect; override;
    /// <summary>DoDisconnect;</summary>
    procedure DoDisconnect; override;
    /// <summary>Notification(</summary>
    procedure Notification( AComponent: TComponent; Operation: TOperation); override;

  public
    /// <summary>constructor</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>destructor</summary>
    destructor Destroy; override;

    ///<summary>Push Error</summary>
    procedure PushError(Const e:exception);overload;
    ///<summary>Pop Error</summary>
    function PopLastError:Exception;
    ///<summary>Pop Error</summary>
    function PopLastErrorMsg:String;
    ///<summary>Pop Error</summary>
    procedure RaiseLastError;

    /// <summary>Return Serveur hosting the DataBase</summary>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Return Serveur hosting the DataBase</summary>
    procedure AssignAlias(Const Source: String);overload;
    /// <summary>Return Serveur hosting the DataBase</summary>
    procedure AssignAlias(Const Source: TFXDBAlias);overload;
    /// <summary>Return Serveur hosting the DataBase</summary>
    procedure AssignAlias(Const Source: TFXCustomDatabase);overload;
    /// <summary>Return Serveur hosting the DataBase</summary>
    procedure Assign2Alias(Const aAlias: TFXDBAlias);
    /// <summary>SetAsUser</summary>
    Class procedure BuildConnectParams(Const aUser,aPwd,aRole:String;Const aClientCharSet:TFXCharacterSet;Const aParams:TStrings);overload;

    /// <summary>Check Connected</summary>
    procedure CheckConnected;
    /// <summary>Check Not Connected</summary>
    procedure CheckNotConnected;
    /// <summary>CheckDatabaseName</summary>
    procedure CheckDatabaseName;
    /// <summary>CheckClientLibrary</summary>
    procedure CheckClientLibrary;
    /// <summary>SetClientLibrary</summary>
    procedure SetClientLibrary(Const Value:IFXClientLib);overload;
    /// <summary>SetClientLibrary</summary>
    procedure SetClientLibrary;overload;


    /// <summary>FreeInternalTR</summary>
    procedure FreeInternalTRs;
    /// <summary>Check Not In Transaction</summary>
    procedure CheckNotInTransaction;virtual;
    /// <summary>Commit_TR</summary>
    function Commit_TRs:Boolean;virtual;
    /// <summary>Commit TR if needed; Return true if some tr committed</summary>
    function TryCommit_TRs(Var Errors:Integer):Boolean;virtual;
    /// <summary>Rollback_TR</summary>
    function Rollback_TRs:Boolean;virtual;

    /// <summary>Return True if IsCurrentServer</summary>
    function IsCurrentServer(const Value:String):Boolean;overload;
    /// <summary>Return True if IsCurrentUser</summary>
    function IsCurrentUser(const Value:String):Boolean;
    /// <summary>Return Serveur hosting the DataBase</summary>
    Class function ExtractServerName(Const aDataBaseName:TFXDBName):String;
    /// <summary>Return the DataBase FileName</summary>
    Class function ExtractFullFileName(Const aDataBaseName:String):String;
    /// <summary>Return the DataBase FileName without ext</summary>
    Class function ExtractFileNameNoExt(Const aDataBaseName:String):String;
    /// <summary>Combine DBName Serveur:FileName</summary>
    Class function BuildAlias(Const aServerName,aPort,aFileName:String):String;overload;inline;
    /// <summary>Parse Alias</summary>
    Class procedure ParseAlias(Const aDataBaseName:String;Out aProtocol:TFXProtocol;Out aServerName,aPort,aFileName:String);

    /// <summary>CreateDatabase</summary>
    procedure CreateDatabase;
    /// <summary>DropDatabase</summary>
    procedure DropDatabase;
    /// <summary>Silent Disconnect Return False iff any Error</summary>
    function SilentDisconnect:Boolean;
    /// <summary>Close Registered DataSets</summary>
    procedure CloseDataSets;

    /// <summary>Test Connection</summary>
    /// poke the server to see if connected
    function TestConnection:TFXConnectResult;
    ///<summary>CheckConnection: poke the server to see if connected</summary>
    /// poke the server to see if connected -> RAise Error iff
    procedure CheckConnection;
    /// <summary>Get Nb of Connected Users</summary>
    function ConnectedUsers:Integer;overload;
    /// <summary>Get Is ShutDown:</summary>
    function IsShutDown(Const aMasterKey:String;Var ShutDown:Boolean):Boolean;

    /// <summary>Clear Schema</summary>
    procedure ClearSchema;virtual;abstract;
    ///<summary>Find Relation.Field Schema</summary>
    function FindMetaRelation(Const aRelation:String):Boolean;overload;inline;
    ///<summary>Find Relation.Field Schema</summary>
    function FindMetaRelation(Const aTR:TFXCustomTransaction;Const aRelation:String):Boolean;overload;inline;
    ///<summary>Find Relation.Field Schema</summary>
    function FindMetaRelation(Const aRelation:String;Out MetaRelation : TFXCustomSchemaNode):Boolean;overload;inline;
    ///<summary>Find Relation.Field Schema</summary>
    function FindMetaRelation(Const aTR:TFXCustomTransaction;Const aRelation:String;Out MetaRelation : TFXCustomSchemaNode):Boolean;overload;inline;
    ///<summary>Find Relation.Field Schema</summary>
    function FindMetaRelationField(Const aRelation, aField:String;Out MetaField: TFXCustomSchemaNode):Boolean;overload;inline;
    ///<summary>Find Relation.Field Schema</summary>
    function FindMetaRelationField(Const aTR:TFXCustomTransaction;Const aRelation, aField:String;Out MetaField: TFXCustomSchemaNode):Boolean;overload;inline;


    {$IFDEF mFXTRACE}
    /// <summary>Log Action</summary>
    Procedure LogMiscInfo(Const aMsg: string);overload;inline;
    /// <summary>Log Action</summary>
    Procedure LogMiscInfo(Const Sender:TComponent;Const aMsg: string);overload;inline;
    /// <summary>Log Action</summary>
    Procedure Log(Const Action:TFXLogAction;Const aMsg: string);overload;inline;
    /// <summary>Log Action</summary>
    Procedure Log(Const Sender:TComponent;Const Action:TFXLogAction;Const aMsg: string);overload;inline;
    /// <summary>Trace</summary>
    procedure LogError(Const aContext:String;Const e:Exception);overload;inline;
    /// <summary>Trace</summary>
    procedure LogError(Const Sender:TComponent;Const aContext:String;Const e:Exception);overload;inline;
    /// <summary>Trace</summary>
    procedure LogError(Const aMsg:String);overload;inline;
    /// <summary>Trace</summary>
    procedure LogError(Const Sender:TComponent;Const aMsg:String);overload;inline;
    {$ENDIF mFXTRACE}

    property Encoding          : TEncoding            read GetEncoding;
    property ClientLibrary     : IFXClientLib         read fLibrary;
    property ClientCharSet     : TFXCharacterSet      read GetClientCharSet;
    property DBAlias           : TFXDBAlias           read GetDBAlias;
    property DatabaseName      : TFXDBName            read GetDatabaseName   write SetDatabaseName stored False;
    property Protocol          : TFXProtocol          read GetProtocol;
    property FullFileName      : String               read GetFullFileName;
    property FullFileNameNoExt : String               read GetFullFileNameNoExt;
    property FileName          : String               read GetFileName;
    property FileNameNoExt     : String               read GetFileNameNoExt;
    property ServerName        : String               read GetServerName;
    property Handle            : TISC_DB_HANDLE       read fHandle;
    property RWAutoTR          : TFXCustomTransaction read GetRWAutoTR;
    property ROAutoTR          : TFXCustomTransaction read GetROAutoTR;
    property MasterTR          : TFXCustomTransaction read GetMasterTR       write SetMasterTR;
    property AutoTR            : TFXCustomTransaction read GetAutoTR;
    property Params            : TStrings             read fDBParams         write SetDBParams;
    property IsReadOnly        : Boolean              read GetIsReadOnly;
    property Schema            : TFXCustomSchema      read GetSchema;
    property FirebirdErrorMsg  : String               read GetLastFirebirdError;

    property TransactionCount  : Integer              read GetTransactionCount;
    property TRs [Const Value:Integer ]: TFXCustomTransaction read GetTransaction;
    {$IFDEF mFXTRACE}
    property FXLogger          : IFXLogger            read fFXLogger        write fFXLogger;
    {$ENDIF mFXTRACE}
  end;

  /// <summary>Firebird Transaction Object</summary>
  TFXCustomTransaction = class(TComponent)
  private
    fLibrary           : IFXClientLib;
    fHandle            : TISC_TR_HANDLE;
    FDefaultAction     : TFXTransactionAction;
    FTRParams          : TStrings;
    FTRParamsChanged   : Boolean;
    fTPB               : PFXByte;
    fTPBPos            : PFXByte;
    fTPBAllocLen       : FXShort;
    fTPBLen            : FXShort;
    {:List of object ref the Transaction ... }
    fBases             : TList;
    {:List of DB ref the Transaction ... }
    fDBs               : TList;

    /// <summary>Register Object</summary>
    procedure AddSQLObject(Const Value: TFXSQLBase);
    /// <summary>UnRegister Object</summary>
    procedure RemoveSQLObject(Const Value:TFXSQLBase);
    /// <summary>UnRegister All Objects</summary>
    procedure RemoveSQLObjects;
    /// <summary>NotifySQLObjects</summary>
    procedure NotifySQLObjects;

    /// <summary>GetDatabaseCount</summary>
    function GetDatabaseCount: Integer;
    /// <summary>GetDatabas</summary>
    function GetDatabase(Const Index: Integer): TFXCustomDatabase;
    /// <summary>GetDatabas</summary>
    function GetDefaultDatabase: TFXCustomDatabase;
    /// <summary>Set DefaultDatabase</summary>
    procedure SetDefaultDatabase(Const Value: TFXCustomDatabase);

    /// <summary>SetTRParam</summary>
    procedure SetTRParams(Const Value: TStrings);
    /// <summary>TRParamsChang</summary>
    procedure TRParamsChange(Sender: TObject);
    /// <summary>TRParamsChangin</summary>
    procedure TRParamsChanging(Sender: TObject);


    /// <summary>Set TPB Len</summary>
    procedure SetTPBAllocLen(Const NewSize: Integer);
    /// <summary>Add Value to StartParams</summary>
    /// The TPB is initially empty,
    /// with the exception that the TPB version must be the first byte of the string.
    procedure StartTPB(Const param: Byte);
    /// <summary>Add Value to StartParams</summary>
    procedure AddParam2TPB(Const param: Byte);
    /// <summary>Add Value to StartParams</summary>
    procedure AddStrParam2TPB(Const param: Byte;Const Value: String);
    /// <summary>Add Value to StartParams</summary>
    procedure AddIntParam2TPB(Const param: Byte;Const Value:Integer);
    /// <summary>Generate Transaction Parameters Bloc</summary>
    /// Given a TRParams string containing a textual representation of the transaction parameters,
    /// generate a transaction parameter buffer, and return it and its length in TPB and TPBLength, respectively.
    procedure GenerateTPB;

    /// <summary>Set Active</summary>
    procedure SetActive(Const Value: Boolean);
    /// <summary>GetInTransaction</summary>
    function GetInTransaction: Boolean;inline;

    /// <summary>Start Transaction</summary>
    procedure InternalStartTransaction;
    /// <summary>End Transaction</summary>
    ///  1- Tell all SQL Objects that we're ending Transcation .
    ///  2- Close Transaction
    ///  3- Tell all SQL Objects that Transcation did end.
    function InternalEndTransaction(Const Action: TFXTransactionAction;Const Silent: Boolean):Boolean;

  protected
    /// <summary>Notification</summary>
    procedure Notification( AComponent: TComponent; Operation: TOperation); override;

  public
    /// <summary>constructor</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>destructor</summary>
    destructor Destroy; override;

    /// <summary>FindDatabase</summary>
    function FindDatabase(Const Value: TFXCustomDatabase): Boolean;
    /// <summary>Check Not In Transaction</summary>
    procedure CheckHasDatabase(Const Value: TFXCustomDatabase);
    /// <summary>Add Database</summary>
    procedure AddDatabase(Const Value: TFXCustomDatabase);
    /// <summary>RemoveDatabase</summary>
    procedure RemoveDatabase(Const Value: TFXCustomDatabase);
    /// <summary>RemoveDatabase</summary>
    procedure RemoveDisconnectedDatabases;
    /// <summary>RemoveDatabases</summary>
    procedure RemoveDatabases;

    /// <summary>Default Isolation</summary>
    procedure DefaultIsolation;

    ///<summary>
    ///  Wait Isolation
    ///</summary>
    procedure WaitIsolation;overload;

    ///<summary>
    ///  Wait Isolation
    ///</summary>
    ///<param name="LockTimeOut">
    ///  seconds to wait
    ///</param>
    ///<remarks>
    ///  <para>
    ///    wait for the concurrent transactions. If the timeout has passed, an
    ///    error (isc_lock_timeout) is reported.
    ///  </para>
    ///  <para>
    ///    Timeout intervals are specified per transaction.
    ///  </para>
    ///</remarks>
    procedure WaitIsolation(Const LockTimeOut:Integer);overload;

    /// <summary>ReadOnly Isolation</summary>
    procedure ReadOnlyIsolation;
    /// <summary>NoWaitIsolation:Used to lock record</summary>
    procedure NoWaitIsolation;
    /// <summary>NoWaitNoVersionIsolation:Used to lock record</summary>
    procedure NoWaitNoVersionIsolation;

    /// <summary>Check In Transaction</summary>
    procedure CheckInTransaction;inline;
    /// <summary>Check Not In Transaction</summary>
    procedure CheckNotInTransaction;

    /// <summary>Start Transaction</summary>
    procedure StartTransaction;inline;
    /// <summary>Commit</summary>
    procedure Commit;inline;
    /// <summary>Commit Retaining</summary>
    procedure CommitRetaining;inline;
    /// <summary>Rollback</summary>
    procedure Rollback;inline;
    /// <summary>Rollback Retaining</summary>
    procedure RollbackRetaining;inline;
    /// <summary>ReStart_TR</summary>
    Procedure ReStart_TR;
    /// <summary>Start_TR</summary>
    function Start_TR:Boolean;
    /// <summary>Commit_TR</summary>
    function Commit_TR:Boolean;
    /// <summary>Rollback_TR</summary>
    function Rollback_TR:Boolean;
    /// <summary>RollbackRetaining_TR</summary>
    function RollbackRetaining_TR:Boolean;
    /// <summary>CommitRetaining_TR</summary>
    function CommitRetaining_TR:Boolean;
    /// <summary>Commit or Rollback according to </summary>
    ///  Do not Raise Error
    function SilentClose(Const aAction:TFXTransactionAction):Boolean;inline;

    /// <summary>DefaultClose Commit or Rollback according to </summary>
    ///  Do not Raise Error
    function SilentDefaultClose:Boolean;inline;
    /// <summary>DefaultClose Commit or Rollback according to </summary>
    procedure DefaultClose;inline;

    {$IFDEF mFXTRACE}
    /// <summary>Trace StartOps</summary>
    procedure LogTRStarting;inline;
    /// <summary>Trace EndOps</summary>
    procedure LogTREnded(Const Value:TFXTransactionAction);overload;inline;
    /// <summary>Trace EndOps</summary>
    procedure LogTREnded(Const Value:TFXTransactionAction;Const aMsg:String);overload;inline;
    /// <summary>Trace EndOps</summary>
    procedure LogTREnded(Const aErrorMsg:String);overload;inline;
    /// <summary>Trace</summary>
    procedure LogProgress(Const aMsg:String);overload;inline;
    /// <summary>Log Action</summary>
    Procedure Log(Const Action:TFXLogAction;Const aMsg: string);overload;inline;
    /// <summary>Trace</summary>
    procedure LogError(Const aContext:String;Const e:Exception);overload;inline;
    /// <summary>Trace</summary>
    procedure LogMiscInfo(Const aMsg: string);overload;inline;
    {$ENDIF mFXTRACE}

    property ClientLibrary     : IFXClientLib         read fLibrary;
    property Handle            : TISC_TR_HANDLE       read fHandle;
    property InTransaction     : Boolean              read GetInTransaction;
    property DatabaseCount     : Integer              read GetDatabaseCount;
    property DefaultDatabase   : TFXCustomDatabase    read GetDefaultDatabase  write SetDefaultDatabase;
    property DefaultAction     : TFXTransactionAction read fDefaultAction      write fDefaultAction default fxTACommit;
    property Params            : TStrings             read FTRParams           write SetTRParams;
    property TPB               : PFXByte              read fTPB;
    property TPBLength         : FXShort              read fTPBLen;
    property Active            : Boolean              read GetInTransaction    write SetActive stored False;

    property DBs [Const Value: Integer]: TFXCustomDatabase read GetDatabase;
  end;

  ///<summary>Database Def</summary>
  TFXCustomSchemaNode = class abstract(TObject)
  end;

  ///<summary>Database Def</summary>
  TFXCustomSchema = class abstract(TObject)
  protected
    fDB              : TFXCustomDatabase;
    fTRx             : TFXCustomTransaction;
    fTerminator      : Char;
    fNodes           : TFXList;
    fCharsets        : TList;
    fDomains         : TList;
    fUDFs            : TList;
    fExceptions      : TList;
    fGenerators      : TList;
    fProcs           : TList;
    fRoles           : TList;
    fTables          : TList;
    fViews           : TList;
    fFields          : TList;
    fPrimaryKeys     : TList;
    fForeignKeys     : TList;
    fIndexes         : TList;
    fCheckConstraints: TList;
    fTriggers        : TList;
    /// <summary>QuoteIdentifier</summary>
    function QuoteIdentifier(Const Value:String):String;
  public
    ///<summary>Constructor</summary>
    constructor Create(Const aDB: TFXCustomDatabase);
    ///<summary>destructor</summary>
    destructor Destroy; override;

    ///<summary>Clear all Relation.Field Schema</summary>
    procedure Clear;virtual;

    property Terminator : Char         read fTerminator write fTerminator default ';';

    property Roles      : TList        read fRoles;
    property Sequences  : TList        read fGenerators;
    property Procs      : TList        read fProcs;
    property UDFs       : TList        read fUDFs;
    property Exceptions : TList        read fExceptions;

    property Tables     : TList        read fTables;
    property Views      : TList        read fViews;
    property CheckConstraints : TList  read fCheckConstraints;
    property Triggers   : TList        read fTriggers;
    property Indexes    : TList        read fIndexes;
    property PrimaryKeys: TList        read fPrimaryKeys;
    property ForeignKeys: TList        read fForeignKeys;

  end;

  /// <summary>Base for SQL Query Component</summary>
  TFXSQLBase = class(TComponent)
  private
    fLibrary           : IFXClientLib;
    fDatabase          : TFXCustomDatabase;
    fTransaction       : TFXCustomTransaction;
    fLastException     : Exception;

    ///<summary>Set Database</summary>
    procedure SetDatabase(Const Value: TFXCustomDatabase);
    ///<summary>Get Database Handle</summary>
    function GetDBHandle: PISC_DB_HANDLE;
    /// <summary>Get Database CodePage</summary>
    function GetClientCodePage:Integer;
    /// <summary>Get Database CodePage</summary>
    function GetClientCharSet:TFXCharacterSet;

    /// <summary>GetTRHandle</summary>
    function GetTRHandle: PISC_TR_HANDLE;inline;
    ///<summary>Set Transaction</summary>
    procedure SetTransaction(Const Value: TFXCustomTransaction);

  protected
    ///<summary>Push Error</summary>
    procedure PushError;overload;
    ///<summary>Push Error</summary>
    procedure PushError(Const aContext:String;Const e:exception);overload;
    ///<summary>Raise Last Error</summary>
    procedure RaiseLastError;
    /// <summary>Default Close</summary>
    function InternalClose:Boolean;virtual;abstract;

    ///<summary>Push Firebird Error</summary>
    procedure CloseAndRaiseClientError(Const aClientError:TFXClientErrorCode);
    ///<summary>Push Firebird Error</summary>
    procedure CloseAndRaiseClientErrorMsg(Const aClientError:TFXClientErrorCode;Const aMsg:String);
    ///<summary>Push Firebird Error</summary>
    procedure CloseAndRaiseClientErrorFmt(Const aClientError:TFXClientErrorCode;const Args:array of const);

    ///<summary>Raise Last Error</summary>
    procedure ReadLastErrorCloseAndRaise;overload;inline;
    ///<summary>Raise Last Error</summary>
    procedure ReadLastErrorCloseAndRaise(Const aContext:String);overload;inline;
    ///<summary>Raise Last Error</summary>
    procedure ReadLastErrorCloseAndRaise(Const aClientError:TFXClientErrorCode;Const aMsg:String);overload;inline;
    ///<summary>Raise Last Error</summary>
    procedure ReadLastErrorCloseAndRaise(Const aClientError:TFXClientErrorCode;const Args:array of const);overload;

    ///<summary>fBase Event</summary>
    procedure BeforeDatabaseDisconnectEvent(Const aDB: TFXCustomDatabase;Const Silent:Boolean);virtual;
    ///<summary>fBase Event</summary>
    procedure DatabaseFreeEvent(Const aDB: TFXCustomDatabase);virtual;

    ///<summary>fBase Event</summary>
    procedure BeforeTransactionEndEvent(Const aTR: TFXCustomTransaction;Const Action:TFXTransactionAction;Const Silent:Boolean);virtual;
    ///<summary>fBase Event</summary>
    procedure TransactionFreeEvent(Const aTR: TFXCustomTransaction);virtual;

  public
    ///<summary>constructor</summary>
    constructor Create(AOwner: TComponent); override;
    ///<summary>destructor</summary>
    destructor Destroy; override;

    ///<summary>Push Error</summary>
    procedure PushError(Const e:exception);overload;
    ///<summary>raise error if database not assigned or not connected</summary>
    procedure CheckHasDatabase;inline;
    ///<summary>raise error if database not assigned or not connected</summary>
    procedure CheckDatabaseConnected;inline;
    ///<summary>raise error if transaction not assigned</summary>
    procedure CheckHasTransaction;inline;
    ///<summary>raise error if transaction not assigned or not started</summary>
    procedure CheckInTransaction;inline;
    ///<summary>raise error if transaction not assigned or not started</summary>
    procedure CheckNotInTransaction;inline;
    ///<summary>raise error if open</summary>
    procedure CheckClosed;virtual;abstract;

    /// <summary>ReStart_TR</summary>
    Procedure ReStart_TR;
    /// <summary>Start_TR</summary>
    function Start_TR:Boolean;
    /// <summary>Commit_TR</summary>
    function Commit_TR:Boolean;
    ///<summary>Free Handle</summary>
    procedure CloseAndRollback(Const RaiseOnError:Boolean=False);
    /// <summary>Rollback_TR</summary>
    function Rollback_TR:Boolean;
    /// <summary>RollbackRetaining_TR</summary>
    function RollbackRetaining_TR:Boolean;
    /// <summary>CommitRetaining_TR</summary>
    function CommitRetaining_TR:Boolean;

    {$IFDEF mFXTRACE}
    /// <summary>Do Log Action</summary>
    function DoLogAction(Const Action:TFXLogAction):Boolean;
    /// <summary>Log Action</summary>
    Procedure Log(Const Action:TFXLogAction);overload;inline;
    /// <summary>Log Action</summary>
    Procedure Log(Const Action:TFXLogAction;Const aMsg: string);overload;inline;
    /// <summary>Log Action</summary>
    Procedure Log(Const Sender:TObject;Const Action:TFXLogAction;Const aMsg: string);overload;inline;
    /// <summary>Trace</summary>
    procedure LogError(Const aContext:String;Const e:Exception);overload;inline;
    /// <summary>Trace</summary>
    procedure LogError(Const Sender:TObject;Const aContext:String;Const e:Exception);overload;inline;
    /// <summary>Trace Open,Close, ...</summary>
    procedure LogMiscInfo(Const aMsg: string);overload;inline;
    /// <summary>Trace</summary>
    procedure LogFetchInfo(Const aMsg: string);overload;inline;
    /// <summary>Trace</summary>
    procedure LogFetchInfo(Const Sender:TObject;Const aMsg: string);overload;inline;
    {$ENDIF mFXTRACE}

    property ClientLibrary     : IFXClientLib         read fLibrary;
    property ClientCodePage    : Integer              read GetClientCodePage;
    property ClientCharSet     : TFXCharacterSet      read GetClientCharSet;
    property DBHandle          : PISC_DB_HANDLE       read GetDBHandle;
    property TRHandle          : PISC_TR_HANDLE       read GetTRHandle;

    property Database          : TFXCustomDatabase    read fDatabase         write SetDatabase;
    property Transaction       : TFXCustomTransaction read fTransaction      write SetTransaction;

  end;

implementation

Uses mFX.Schema, mFX.MetaData, mFX.Services;

Const
  _DPBPrefix_ = 'isc_dpb_';
  _DPBConstantNames_ : array[1..isc_dpb_last_dpb_constant] of string = (
    'cdd_pathname',
    'allocation',
    'journal',
    'page_size',
    'num_buffers',
    'buffer_length',
    'debug',
    'garbage_collect',
    'verify',
    'sweep',
    'enable_journal',
    'disable_journal',
    'dbkey_scope',
    'number_of_users',
    'trace',
    'no_garbage_collect',
    'damaged',
    'license',
    'sys_user_name',
    'encrypt_key',
    'activate_shadow',
    'sweep_interval',
    'delete_shadow',
    'force_write',
    'begin_log',
    'quit_log',
    'no_reserve',
    'user_name',
    'password',
    'password_enc',
    'sys_user_name_enc',
    'interp',
    'online_dump',
    'old_file_size',
    'old_num_files',
    'old_file',
    'old_start_page',
    'old_start_seqno',
    'old_start_file',
    'drop_walfile',
    'old_dump_id',
    'wal_backup_dir',
    'wal_chkptlen',
    'wal_numbufs',
    'wal_bufsize',
    'wal_grp_cmt_wait',
    'lc_messages',
    'lc_ctype',
    'cache_manager',
    'shutdown',
    'online',
    'shutdown_delay',
    'reserved',
    'overwrite',
    'sec_attach',
    'disable_wal',
    'connect_timeout',
    'dummy_packet_interval',
    'gbak_attach',
    'sql_role_name',
    'set_page_buffers',
    'working_directory',
    'sql_dialect',
    'set_db_readonly',
    'set_db_sql_dialect',
    'gfix_attach',
    'gstat_attach'
   ,'set_db_charset'
   ,'gsec_attach_deprecated'
   ,'address_path'
   ,'process_id'
   ,'no_db_triggers'
   ,'trusted_auth'
   ,'process_name'
   ,'trusted_role'
   ,'org_filename'
   ,'utf8_filename'
   ,'ext_call_depth'
   ,'auth_block'
   ,'client_version'
   ,'remote_protocol'
   ,'host_name'
   ,'os_user'
   ,'specific_auth_data'
   ,'auth_plugin_list'
   ,'auth_plugin_name'
   ,'config'
   ,'nolinger'
    );

  _TPBPrefix_ = 'isc_tpb_';
  _TPBConstantNames_ : array[1..isc_tpb_last_tpb_constant] of string = (
    'consistency',
    'concurrency',
    'shared',
    'protected',
    'exclusive',
    'wait',
    'nowait',
    'read',
    'write',
    'lock_read',
    'lock_write',
    'verb_time',
    'commit_time',
    'ignore_limbo',
    'read_committed',
    'autocommit',
    'rec_version',
    'no_rec_version',
    'restart_requests',
    'no_auto_undo',
    'lock_timeout'
    );

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
{$IFDEF mFXTRACE}
procedure TFXCustomDatabase.LogConnecting;
Begin
  if (fFXLogger<>nil) then Begin
    if fFXLogger.DoLogAction[fxtDBConnect] then Begin
      Self.Log(fxtDBConnect   ,'Connecting to DB');
      Self.Log(fxtDBConnecting,format('CONNECT TO %s;',[AnsiQuotedStr(fDBAlias.Alias,'''')]))
    end end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.LogConnected;
Begin
  if (fFXLogger<>nil) then Begin
    if fFXLogger.DoLogAction[fxtDBConnect] then Begin
      fFXLogger.Log(Self,fxtDBConnected);
    end end
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.LogDisconnecting;
Begin
  if (fFXLogger<>nil)and(fFXLogger.DoLogAction[fxtDBConnect]) then Begin
    fFXLogger.Log(Self,fxtDBDisConnect,format('Disconnect from %s;',[AnsiQuotedStr(fDBAlias.Alias,'''')]))
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.LogDisconnected;
Begin
  if (fFXLogger<>nil)and(fFXLogger.DoLogAction[fxtDBConnect]) then Begin
    fFXLogger.Log(Self,fxtDBDisConnected);
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.LogMiscInfo(Const aMsg: string);
Begin
  if (fFXLogger<>nil)and(fFXLogger.DoLogAction[fxtInfoMsg]) then
    fFXLogger.Log(Self,fxtInfoMsg,aMsg);
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.LogMiscInfo(Const Sender:TComponent;Const aMsg: string);
Begin
  if (fFXLogger<>nil)and(fFXLogger.DoLogAction[fxtInfoMsg]) then
    fFXLogger.Log(Sender,fxtInfoMsg,aMsg);
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.Log(Const Action:TFXLogAction;Const aMsg: string);
Begin
  if (fFXLogger<>nil)and(fFXLogger.DoLogAction[Action]) then
    fFXLogger.Log(Self,Action,aMsg);
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.Log(Const Sender:TComponent;Const Action:TFXLogAction;Const aMsg: string);
Begin
  if (fFXLogger<>nil)and(fFXLogger.DoLogAction[Action]) then
    fFXLogger.Log(Sender,Action,aMsg);
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.LogError(Const aMsg: string);
Begin
  if (fFXLogger<>nil)and(fFXLogger.DoLogAction[fxtError]) then
    fFXLogger.Log(Self,fxtError,aMsg);
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.LogError(Const Sender:TComponent;Const aMsg: string);
Begin
  if (fFXLogger<>nil)and(fFXLogger.DoLogAction[fxtError]) then
    fFXLogger.Log(Sender,fxtError,aMsg);
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.LogError(Const aContext:String;Const e:Exception);
Begin
  if (fFXLogger<>nil)and(fFXLogger.DoLogAction[fxtError]) then
    fFXLogger.Log(Self,fxtError,aContext,e);
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.LogError(Const Sender:TComponent;Const aContext:String;Const e:Exception);
Begin
  if (fFXLogger<>nil)and(fFXLogger.DoLogAction[fxtError]) then
    fFXLogger.Log(Sender,fxtError,aContext,e);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXSQLBase.DoLogAction(Const Action:TFXLogAction):Boolean;
Begin
  Result:=(fDatabase<>nil)and(fDatabase.fFXLogger<>nil)and(fDatabase.fFXLogger.DoLogAction[Action])
end;
{______________________________________________________________________________}
procedure TFXSQLBase.Log(Const Action:TFXLogAction);
Begin
  if (fDatabase<>nil)and(fDatabase.fFXLogger<>nil)and(fDatabase.fFXLogger.DoLogAction[Action]) then
    fDatabase.fFXLogger.Log(Self,Action);
end;
{______________________________________________________________________________}
procedure TFXSQLBase.Log(Const Action:TFXLogAction;Const aMsg: string);
Begin
  if (fDatabase<>nil)and(fDatabase.fFXLogger<>nil)and(fDatabase.fFXLogger.DoLogAction[Action]) then
    fDatabase.fFXLogger.Log(Self,Action,aMsg);
end;
{______________________________________________________________________________}
procedure TFXSQLBase.Log(Const Sender:TObject;Const Action:TFXLogAction;Const aMsg: string);
Begin
  if (fDatabase<>nil)and(fDatabase.fFXLogger<>nil)and(fDatabase.fFXLogger.DoLogAction[Action]) then
    fDatabase.fFXLogger.Log(Sender,Action,aMsg);
end;
{______________________________________________________________________________}
procedure TFXSQLBase.LogError(Const aContext:String;Const e:Exception);
Begin
  if (fDatabase<>nil)and(fDatabase.fFXLogger<>nil)and(fDatabase.fFXLogger.DoLogAction[fxtError]) then
    fDatabase.fFXLogger.Log(Self,fxtError,aContext,e);
end;
{______________________________________________________________________________}
procedure TFXSQLBase.LogError(Const Sender:TObject;Const aContext:String;Const e:Exception);
Begin
  if (fDatabase<>nil)and(fDatabase.fFXLogger<>nil)and(fDatabase.fFXLogger.DoLogAction[fxtError]) then
    fDatabase.fFXLogger.Log(Sender,fxtError,aContext,e);
end;
{______________________________________________________________________________}
procedure TFXSQLBase.LogMiscInfo(Const aMsg: string);
Begin
  if (fDatabase<>nil)and(fDatabase.fFXLogger<>nil)and(fDatabase.fFXLogger.DoLogAction[fxtMiscInfo]) then
    fDatabase.fFXLogger.Log(Self,fxtMiscInfo,aMsg);
end;
{______________________________________________________________________________}
procedure TFXSQLBase.LogFetchInfo(Const aMsg: string);
Begin
  if (fDatabase<>nil)and(fDatabase.fFXLogger<>nil)and(fDatabase.fFXLogger.DoLogAction[fxtFetchInfo]) then
    fDatabase.fFXLogger.Log(Self,fxtFetchInfo,aMsg);
end;
{______________________________________________________________________________}
procedure TFXSQLBase.LogFetchInfo(Const Sender:TObject;Const aMsg: string);
Begin
  if (fDatabase<>nil)and(fDatabase.fFXLogger<>nil)and(fDatabase.fFXLogger.DoLogAction[fxtFetchInfo]) then
    fDatabase.fFXLogger.Log(Sender,fxtFetchInfo,aMsg);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXCustomTransaction.LogTRStarting;
Var db:TFXCustomDatabase;
    msg:String;
    i:Integer;
Begin
  db:=Self.DefaultDataBase;
  if (db<>nil)and(db.fFXLogger<>nil) Then Begin
    if (db.fFXLogger.DoLogAction[fxtTRStart]) then Begin
      msg:='DBs:';
      for i:=0 to Pred(fDBs.Count) do begin
        db:=TFXCustomDatabase(fDBs[i]);
        if (db<>nil)and(db.Connected) then Begin
          if db.Name=EmptyStr then
            msg:=msg+db.DatabaseName+';' else
            msg:=msg+db.Name+';';
        end end;
      db.fFXLogger.Log(Self,fxtTRStart,msg);
      end;
    if (db.fFXLogger.DoLogAction[fxtTRParams]) then Begin
      if fTRParams.Count>0 then begin
        msg:='Params:';
        for i := 0 to Pred(fTRParams.Count) do
          msg:=msg+fTRParams[i]+';';
        db.fFXLogger.Log(Self,fxtTRStart,msg);
      end else
        db.fFXLogger.Log(Self,fxtTRParams,'Params:Default');
    end end;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.LogTREnded(Const Value:TFXTransactionAction);
Var LogAction:TFXLogAction;
    db:TFXCustomDatabase;
Begin
  db:=Self.DefaultDataBase;
  if (db<>nil)and(db.fFXLogger<>nil) then Begin
    case Value of
      fxTARollbackRetaining:LogAction:=fxtTRRollbackRetain;
      fxTARollback         :LogAction:=fxtTRRollback;
      fxTACommitRetaining  :LogAction:=fxtTRCommitRetain;
      else                  LogAction:=fxtTRCommit;
      end;
    db.fFXLogger.Log(Self,LogAction);
    end
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.LogTREnded(Const Value:TFXTransactionAction;Const aMsg: string);
Var LogAction:TFXLogAction;
    db:TFXCustomDatabase;
Begin
  db:=Self.DefaultDataBase;
  if (db<>nil)and(db.fFXLogger<>nil) then Begin
    case Value of
      fxTARollbackRetaining:LogAction:=fxtTRRollbackRetain;
      fxTARollback         :LogAction:=fxtTRRollback;
      fxTACommitRetaining  :LogAction:=fxtTRCommitRetain;
      else                  LogAction:=fxtTRCommit;
      end;
    db.fFXLogger.Log(Self,LogAction,aMsg);
    end
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.LogTREnded(Const aErrorMsg: string);
Var db:TFXCustomDatabase;
Begin
  db:=Self.DefaultDataBase;
  if (db<>nil)and(db.fFXLogger<>nil) then Begin
    db.fFXLogger.Log(Self,fxtTRStartError,aErrorMsg);
    end
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.LogProgress(Const aMsg:String);
Var db:TFXCustomDatabase;
Begin
  db:=Self.DefaultDataBase;
  if (db<>nil)and(db.fFXLogger<>nil) then
    db.fFXLogger.Log(Self,fxtOpsProgress,aMsg);
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.Log(Const Action:TFXLogAction;Const aMsg: string);
Var db:TFXCustomDatabase;
Begin
  db:=Self.DefaultDataBase;
  if (db<>nil)and(db.fFXLogger<>nil) then
    db.fFXLogger.Log(Self,Action,aMsg);
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.LogError(Const aContext:String;Const e:Exception);
Var db:TFXCustomDatabase;
Begin
  db:=Self.DefaultDataBase;
  if (db<>nil)and(db.fFXLogger<>nil) then
    db.fFXLogger.Log(Self,fxtOpsError,aContext,e);
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.LogMiscInfo(Const aMsg: string);
Var db:TFXCustomDatabase;
Begin
  db:=Self.DefaultDataBase;
  if (db<>nil)and(db.fFXLogger<>nil) then
    db.fFXLogger.Log(Self,fxtMiscInfo,aMsg);
end;
{$ENDIF mFXTRACE}
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXCustomDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTRs      := TList.Create;
  fBases    := TList.Create;
  fDBAlias  := TFXDBAlias.Create;
  fEncoding := TUTF8Encoding.Create;
  fDBParams := TStringList.Create;
  TStringList(fDBParams).OnChanging := Self.DBParamsChanging;
  TStringList(fDBParams).OnChange := Self.DBParamsChange;
  fDBParamsChanged:=True;
  fIsReadOnly:=True;
end;
{______________________________________________________________________________}
destructor TFXCustomDatabase.Destroy;
begin
  SilentDisconnect;
  NotifySQLObjects;
  RemoveSQLObjects;
  RemoveTransactions;
  if fDPBAllocLen>0 then
    FXFree(fDPB);
  fEncoding.Free;
  fLastException.Free;
  fDBParams.Free;
  fDBAlias.Free;
  fBases.Free;
  fTRs.Free;

  fLibrary:=nil;
  {$IFDEF mFXTRACE}
  fFXLogger:=nil;
  {$ENDIF}

  inherited
end;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.PushError(Const e:exception);
Begin
  FreeAndNil(fLastException);
  fLastException:=Exception.Create(e.message);
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.PushError(Const aMsg:String);
Begin
  if (fLastException=nil) then Begin
    fLastException:=Exception.Create(aMsg);
  End else Begin
    fLastException.Message:=aMsg
    end;
End;
{______________________________________________________________________________}
function TFXCustomDatabase.PopLastError:Exception;
Begin
  Assert(fLastException<>nil);
  Result:=fLastException;
  fLastException:=nil;
End;
{______________________________________________________________________________}
function TFXCustomDatabase.PopLastErrorMsg:String;
Begin
  Assert(fLastException<>nil);
  Result:=fLastException.Message;
  FreeAndNil(fLastException);
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.RaiseLastError;
Var e:Exception;
Begin
  Assert(fLastException<>nil);
  e:=fLastException;
  fLastException:=nil;
  raise e;
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.Assign(Source: TPersistent);
Var db:TFXCustomDatabase;
Begin
  CheckNotConnected;
  if Source is TFXCustomDatabase Then Begin
    db:=TFXCustomDatabase(Source);
    Self.fDBAlias.Assign(db.fDBAlias);
    Self.LoginPrompt:=db.LoginPrompt;
    Self.Params.Assign(DB.Params);
    fDBParamsChanged:=True;
  end else
    Inherited Assign(Source)
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.AssignAlias(Const Source: String);
Begin
  fDBAlias.Alias:=Source;
  fDBParamsChanged:=True;
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.AssignAlias(Const Source: TFXDBAlias);
Begin
  fDBAlias.Assign(Source);
  fDBParamsChanged:=True;
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.AssignAlias(Const Source: TFXCustomDatabase);
Begin
  fDBAlias.Assign(Source.fDBAlias);
  fDBParamsChanged:=True;
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.Assign2Alias(Const aAlias: TFXDBAlias);
Begin
  aAlias.Assign(fDBAlias);
End;
{______________________________________________________________________________}
function TFXCustomDatabase.GetEncoding : TEncoding;
Begin
  if fEncoding=nil then Begin
    // Forced UTF8
    fEncoding:=TUTF8Encoding.Create;
    end;
  Result:=fEncoding
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetClientCharSet: TFXCharacterSet;
begin
  // Forced UTF8
  Result:=fxUTF8
end;
{______________________________________________________________________________}
Class procedure TFXCustomDatabase.BuildConnectParams(Const aUser,aPwd,aRole:String;Const aClientCharSet:TFXCharacterSet;Const aParams:TStrings);
Begin
  With aParams do Begin
    BeginUpdate;
    try Clear;
        Values['user_name'      ]:= aUser;
        Values['password'       ]:= aPwd;
        if aRole<>EmptyStr then
          Values['sql_role_name']:= aRole;
        Values['connect_timeout']:='1';
    finally
        EndUpdate
    end end;
End;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.AddSQLObject(Const Value: TFXSQLBase);
Var i:Integer;
begin
  Assert(Value<>nil);
  Assert(fBases.IndexOf(Value)<0);

  for i:=Pred(fBases.Count) downto 0 do Begin
    if fBases[i]=nil then Begin
      Value.fLibrary:=Self.fLibrary;
      Value.fDatabase:=Self;
      fBases[i]:=Value;
      exit;
    end end;

  Value.fLibrary:=Self.fLibrary;
  Value.fDatabase:=Self;
  fBases.Add(Value)
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.RemoveSQLObject(Const Value:TFXSQLBase);
var b:TFXSQLBase;
    i:Integer;
begin
  Assert(Value<>nil);
  Value.fLibrary:=nil;
  Value.fDatabase:=nil;
  for i:=Pred(fBases.Count) downto 0 do Begin
    b:=TFXSQLBase(fBases[i]);
    if b=Value then Begin
      fBases[i]:=nil;
      exit;
    end end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.RemoveSQLObjects;
var b:TFXSQLBase;
    i:Integer;
begin
  for i:=Pred(fBases.Count) downto 0 do Begin
    b:=TFXSQLBase(fBases[i]);
    if b<>nil Then Begin
      Assert(b.fDatabase=Self);
      b.fDatabase := nil;
      b.fLibrary := nil;
      fBases[i]:=nil;
    end end
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.NotifySQLObjects;
var b:TFXSQLBase;
    i:Integer;
begin
  for i :=Pred(fBases.Count) downto 0 do Begin
    b:=TFXSQLBase(fBases[i]);
    if b<>nil then Begin
      {$IFDEF MFXTRACE}b.LogMiscInfo(format('Database <%s::%s> Free',[Self.ClassName,Self.Name]));{$ENDIF}
      Assert(b.fLibrary=fLibrary);
      Assert(b.fDatabase=Self);
      b.DatabaseFreeEvent(Self);
    end end;
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.Notification( AComponent: TComponent;Operation: TOperation);
var tr:TFXCustomTransaction;
    i:Integer;
begin
  inherited Notification( AComponent, Operation);
  Case Operation of
    opRemove:Begin
      If (AComponent is TFXCustomTransaction) then begin
        for i:=Pred(fTRs.Count) downto 0 do begin
          tr:=TFXCustomTransaction(fTRs[i]);
          if tr=AComponent then begin
            Assert(tr<>fInternalTRRWEx);
            Assert(tr<>fInternalTRROEx);
            tr.RemoveDatabase(Self);
            fTRs[i]:=nil;
            break;
          end end;
    end end end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.FreeInternalTRs;
Begin
  FreeAndNil(fInternalTRROEx);
  FreeAndNil(fInternalTRRWEx);
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetROAutoTR: TFXCustomTransaction;
begin
  if fInternalTRROEx=nil then Begin
    //Create a Internal TR
    fInternalTRROEx:=TFXCustomTransaction.Create(nil);
    fInternalTRROEx.Name:=Self.Name+'_iReadOnlyTR';
    fInternalTRROEx.ReadOnlyIsolation;
    fInternalTRROEx.DefaultDataBase:=Self;
    fInternalTRROEx.fLibrary:=fLibrary;
    end;

  if fInternalTRROEx.InTransaction then Begin
    Assert(fInternalTRROEx.FindDatabase(Self));
  end else
    fInternalTRROEx.AddDatabase(Self);

  Assert(fInternalTRROEx.DefaultDataBase=Self);
  Assert(fInternalTRROEx.fLibrary=fLibrary);
  Result:=fInternalTRROEx
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetRWAutoTR: TFXCustomTransaction;
begin
  if fInternalTRRWEx=nil then Begin
    //Create a Internal TR
    fInternalTRRWEx:=TFXCustomTransaction.Create(nil);
    fInternalTRRWEx.Name:=Self.Name+'_iReadWriteTR';
    fInternalTRRWEx.DefaultIsolation;
    fInternalTRRWEx.DefaultDataBase:=Self;
    fInternalTRRWEx.fLibrary:=fLibrary;
    end;

  if fInternalTRRWEx.InTransaction then Begin
    Assert(fInternalTRRWEx.FindDatabase(Self));
  end else
    fInternalTRRWEx.AddDatabase(Self);

  Assert(fInternalTRRWEx.DefaultDataBase=Self);
  Assert(fInternalTRRWEx.fLibrary=fLibrary);
  Result:=fInternalTRRWEx
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetMasterTR:TFXCustomTransaction;
var tr:TFXCustomTransaction;
    i: Integer;
begin
  // Try to find a TR ...
  for i := 0 to Pred(fTRs.Count) do Begin
    tr:=TFXCustomTransaction(fTRs[i]);
    if (tr<>nil) then Begin
      Assert(tr<>fInternalTRRWEx);
      Assert(tr<>fInternalTRROEx);
      Assert(tr.fLibrary=fLibrary);
      Assert(tr.FindDatabase(Self));
      Result:=tr;
      exit;
    end end;

  Result:=nil;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.SetMasterTR(Const Value: TFXCustomTransaction);
var OldMaster,tr:TFXCustomTransaction;
    i:Integer;
begin
  OldMaster:=Self.GetMasterTR;
  if (Value<>nil) then begin
    if (Value<>OldMaster) then begin
      // Move To First
      Assert(Value<>fInternalTRRWEx);
      Assert(Value<>fInternalTRROEx);
      for i:=Pred(fTRs.Count) downto 0 do begin
        tr:=TFXCustomTransaction(fTRs[i]);
        if tr=Value then Begin
          fTRs[i]:=nil;
          break;
        end end;
      if (fTRs.Count>0)and(fTRs[0]=nil) then
        fTRs[0]:=Value else
        fTRs.Insert(0,Value);
      Value.AddDatabase(Self);
      // Reset Internal Queries
      Assert(Value.fLibrary=fLibrary);
      MasterTRChanged(OldMaster,Value);
    end else Begin
      // Same MasterTR
      Assert(Value<>fInternalTRRWEx);
      Assert(Value<>fInternalTRROEx);
      Assert(Value.fLibrary=fLibrary);
      end;
  end else Begin
    // Remove All Transactions
    for i:=Pred(fTRs.Count) downto 0 do Begin
      tr:=TFXCustomTransaction(fTRs[i]);
      if (tr<>nil) then Begin
        fTRs[i]:=nil;
        Assert(tr<>fInternalTRRWEx);
        Assert(tr<>fInternalTRROEx);
        tr.RemoveDatabase(Self);
      end end;
    // Reset Internal Queries
    MasterTRChanged(OldMaster,nil);
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.MasterTRChanged(Const OldMaster,NewMaster:TFXCustomTransaction);
var b:TFXSQLBase;
    i:Integer;
begin
  for i :=Pred(fBases.Count) downto 0 do Begin
    b:=TFXSQLBase(fBases[i]);
    if (b<>nil) then Begin
      Assert(b.fDatabase=Self);
      Assert(b.fLibrary=fLibrary);
      if (b.fTransaction<>nil)and(b.fTransaction=OldMaster) then Begin
        b.fTransaction:=NewMaster
    end end end;
End;
{______________________________________________________________________________}
function TFXCustomDatabase.GetAutoTR:TFXCustomTransaction;
Begin
  Result:=Self.GetMasterTR;
  if (Result=nil) then
    Result:=Self.GetRWAutoTR;
End;
{______________________________________________________________________________}
function TFXCustomDatabase.GetTransactionCount: Integer;
var tr:TFXCustomTransaction;
    i:Integer;
begin
  result := 0;
  for i := 0 to Pred(fTRs.Count) do Begin
    tr:=TFXCustomTransaction(fTRs[i]);
    if (tr<>nil) Then Begin
      Assert(tr<>fInternalTRRWEx);
      Assert(tr<>fInternalTRROEx);
      Inc(result);
    end end;
  If fInternalTRROEx<>nil then Begin
    Inc(result);
    end;
  If fInternalTRRWEx<>nil then Begin
    Inc(result);
    end;
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetTransaction(Const Value:Integer):TFXCustomTransaction;
var tr:TFXCustomTransaction;
    i, Nb:Integer;
begin
  Nb:=0;
  for i := 0 to Pred(fTRs.Count) do Begin
    tr:=TFXCustomTransaction(fTRs[i]);
    if (tr<>nil) Then Begin
      Assert(tr<>fInternalTRRWEx);
      Assert(tr<>fInternalTRROEx);
      if Nb=Value then Begin
        Result:=tr;
        exit;
      end end;
    Inc(Nb);
    end;
  If (fInternalTRROEx<>nil) then Begin
    if Nb=Value then Begin
      Result:=fInternalTRROEx;
      exit;
      end;
    Inc(Nb);
    end;
  If fInternalTRRWEx<>nil then Begin
    if Nb=Value then Begin
      Result:=fInternalTRROEx;
      exit;
    end end;
  result := nil;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.AddTransaction(Const Value: TFXCustomTransaction);
var t:TFXCustomTransaction;
    i:Integer;
begin
  Assert(Value<>nil);
  if (Value<>fInternalTRRWEx)and(Value<>fInternalTRROEx) then Begin

    for i:=Pred(fTRs.Count) downto 0 do begin
      t:=TFXCustomTransaction(fTRs[i]);
      if Value = t then begin
        Assert(Value.fDBs.IndexOf(Self)>=0);
        Assert(Value.fLibrary=fLibrary);
        exit;
      end end;

    for i:=Pred(fTRs.Count) downto 0 do begin
      if fTRs[i]=nil Then Begin
        fTRs[i]:=Value;
        Value.AddDatabase(Self);
        Assert(Value.fLibrary=fLibrary);
        exit;
      end end;

    fTRs.Add(Value);
    Value.AddDatabase(Self);
    Assert(Value.fLibrary=fLibrary);

  End else Begin
    // Do not add InternalTRs
    Assert(Value.DefaultDatabase=Self);
    Assert(Value.fLibrary=fLibrary);
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.RemoveTransaction(Const Value: TFXCustomTransaction);
var i:Integer;
begin
  if (Value<>nil) Then Begin
    if (Value<>fInternalTRRWEx)and(Value<>fInternalTRROEx) then Begin
      for i:=Pred(fTRs.Count) downto 0 do begin
        if fTRs[i]=Value then begin
          fTRs[i]:=nil;
          Value.RemoveDatabase(Self);
          exit;
        end end;
    end else Begin
      // fInternalTRRWEx,fInternalTRROEx destroy ?
      end;
  end else Begin
    // ?
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.RemoveTransactions;
Var tr:TFXCustomTransaction;
    i: Integer;
begin
  for i:=Pred(fTRs.Count) downto 0 do Begin
    tr:=TFXCustomTransaction(fTRs[i]);fTRs[i]:=nil;
    if (tr<>nil) Then Begin
      Assert(tr<>fInternalTRRWEx);
      Assert(tr<>fInternalTRROEx);
      tr.RemoveDatabase(Self);
    end end;
  FreeInternalTRs;
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetConnected: Boolean;
begin
  Result:=(fHandle<>nil)
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.CheckConnected;
begin
  if fHandle = nil then
    FXRaiseClientError(Self,fxceDatabaseClosed);
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.CheckNotConnected;
begin
  if fHandle <> nil then
    FXRaiseClientError(Self,fxceDatabaseOpen);
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.CheckNotInTransaction;
Var tr:TFXCustomTransaction;
    i:Integer;
begin
  for i:=Pred(fTRs.Count) downto 0 do Begin
    tr:=TFXCustomTransaction(fTRs[i]);
    if tr<>nil then Begin
      Assert(tr<>fInternalTRRWEx);
      Assert(tr<>fInternalTRROEx);
      tr.CheckNotInTransaction;
    end end;

  if (fInternalTRRWEx<>nil) then
    fInternalTRRWEx.CheckNotInTransaction;

  if (fInternalTRROEx<>nil) then
    fInternalTRROEx.CheckNotInTransaction;
End;
{______________________________________________________________________________}
function TFXCustomDatabase.Commit_TRs:Boolean;
Var tr:TFXCustomTransaction;
    i:Integer;
begin
  Result:=False;

  for i:=Pred(fTRs.Count) downto 0 do Begin
    tr:=TFXCustomTransaction(fTRs[i]);
    if tr<>nil then Begin
      if tr.InTransaction then Begin
        Assert(tr<>fInternalTRRWEx);
        Assert(tr<>fInternalTRROEx);
        Assert(tr.FindDatabase(Self));
        tr.Commit;
        Result:=True;
    end end end;

  if (fInternalTRRWEx<>nil) then Begin
    if fInternalTRRWEx.InTransaction then Begin
        fInternalTRRWEx.Commit;
        Result:=True;
    end end;

  if (fInternalTRROEx<>nil) then Begin
    if fInternalTRROEx.InTransaction then Begin
        fInternalTRROEx.Commit;
        Result:=True;
    end end;
End;
{______________________________________________________________________________}
function TFXCustomDatabase.TryCommit_TRs(Var Errors:Integer):Boolean;
Var tr:TFXCustomTransaction;
    i:Integer;
begin
  Result:=False;

  for i:=Pred(fTRs.Count) downto 0 do Begin
    tr:=TFXCustomTransaction(fTRs[i]);
    if tr<>nil then Begin
      if tr.InTransaction then Begin
        Assert(tr<>fInternalTRRWEx);
        Assert(tr<>fInternalTRROEx);
        Assert(tr.FindDatabase(Self));
        if tr.InTransaction then Begin
          if not tr.SilentClose(fxTACommit) then
            Inc(Errors);
          Result:=True;
    end end end end;

  if (fInternalTRRWEx<>nil) then Begin
    if fInternalTRRWEx.InTransaction then Begin
        if not fInternalTRRWEx.SilentClose(fxTACommit) then
          Inc(Errors);
        Result:=True;
    end end;

  if (fInternalTRROEx<>nil) then Begin
    if fInternalTRROEx.InTransaction then Begin
      if not fInternalTRROEx.SilentClose(fxTACommit) then
        Inc(Errors);
      Result:=True;
    end end;
End;
{______________________________________________________________________________}
function TFXCustomDatabase.Rollback_TRs:Boolean;
Var tr:TFXCustomTransaction;
    i:Integer;
begin
  Result:=False;

  for i:=Pred(fTRs.Count) downto 0 do Begin
    tr:=TFXCustomTransaction(fTRs[i]);
    if tr<>nil then Begin
      if tr.InTransaction then Begin
        Assert(tr<>fInternalTRRWEx);
        Assert(tr<>fInternalTRROEx);
        Assert(tr.FindDatabase(Self));
        tr.Rollback;
        Result:=True;
    end end end;

  if (fInternalTRRWEx<>nil) then Begin
    if fInternalTRRWEx.InTransaction then Begin
        fInternalTRRWEx.Rollback;
        Result:=True;
    end end;

  if (fInternalTRROEx<>nil) then Begin
    if fInternalTRROEx.InTransaction then Begin
        fInternalTRROEx.Rollback;
        Result:=True;
    end end;
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.SetDBParams(Const Value:TStrings);
begin
  fDBParams.Assign(Value);
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DBParamsChange(Sender: TObject);
begin
  fDBParamsChanged:=True;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DBParamsChanging(Sender: TObject);
begin
  if (csDesigning in ComponentState) Then
    Self.SilentDisconnect;
  CheckNotConnected;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.CheckClientLibrary;
Begin
  if fLibrary=nil Then
    FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,Format('DB <%s::%s> Client Lib Not Assigned',[Self.ClassName,Self.Name]));
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.SetClientLibrary;
var tr:TFXCustomTransaction;
    db:TFXCustomDatabase;
    b:TFXSQLBase;
    i,j:Integer;
Begin
  Self.CheckNotConnected;

  for i :=Pred(fBases.Count) downto 0 do Begin
    b:=TFXSQLBase(fBases[i]);
    if b<>nil then Begin
      b.fLibrary:=nil;
    end end;

  for j := Pred(fTRs.Count) downto 0 do Begin
    tr:=TFXCustomTransaction(fTRs[j]);
    if tr<>nil then Begin
      Assert(not tr.InTransaction);
      for i := Pred(tr.fDBs.Count) downto 0 do Begin
        db:=TFXCustomDatabase(tr.fDBs[i]);
        if (db<>nil)and(db<>Self) then begin
          Assert(not db.Connected);
          db.fLibrary:=nil
        end end;
      tr.fLibrary:=nil
    end end;

  if (fInternalTRRWEx<>nil) then Begin
    tr:=fInternalTRRWEx;
    for i := Pred(tr.fDBs.Count) downto 0 do Begin
      db:=TFXCustomDatabase(tr.fDBs[i]);
      if (db<>nil)and(db<>Self) then begin
        Assert(not db.Connected);
        db.fLibrary:=nil
      end end;
    tr.fLibrary:=nil
    end;

  if (fInternalTRROEx<>nil) then Begin
    tr:=fInternalTRROEx;
    for i := Pred(tr.fDBs.Count) downto 0 do Begin
      db:=TFXCustomDatabase(tr.fDBs[i]);
      if (db<>nil)and(db<>Self) then begin
        Assert(not db.Connected);
        db.fLibrary:=nil
      end end;
    tr.fLibrary:=nil
    end;

  fLibrary:=nil;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.SetClientLibrary(Const Value:IFXClientLib);
var tr:TFXCustomTransaction;
    db:TFXCustomDatabase;
    b:TFXSQLBase;
    i,j:Integer;
Begin
  Self.CheckNotConnected;

  fLibrary:=Value;
  for j := Pred(fTRs.Count) downto 0 do Begin
    tr:=TFXCustomTransaction(fTRs[j]);
    if tr<>nil then Begin
      Assert(not tr.InTransaction);
      for i := Pred(tr.fDBs.Count) downto 0 do Begin
        db:=TFXCustomDatabase(tr.fDBs[i]);
        if (db<>nil)and(db<>Self) then begin
          Assert(not db.Connected);
          db.fLibrary:=fLibrary
        end end;
      tr.fLibrary:=fLibrary
    end end;

  if (fInternalTRRWEx<>nil) then Begin
    tr:=fInternalTRRWEx;
    for i := Pred(tr.fDBs.Count) downto 0 do Begin
      db:=TFXCustomDatabase(tr.fDBs[i]);
      if (db<>nil)and(db<>Self) then begin
        Assert(not db.Connected);
        db.fLibrary:=fLibrary
      end end;
    tr.fLibrary:=fLibrary
    end;

  if (fInternalTRROEx<>nil) then Begin
    tr:=fInternalTRROEx;
    for i := Pred(tr.fDBs.Count) downto 0 do Begin
      db:=TFXCustomDatabase(tr.fDBs[i]);
      if (db<>nil)and(db<>Self) then begin
        Assert(not db.Connected);
        db.fLibrary:=fLibrary
      end end;
    tr.fLibrary:=fLibrary
    end;

  for i :=Pred(fBases.Count) downto 0 do Begin
    b:=TFXSQLBase(fBases[i]);
    if b<>nil then Begin
      b.fLibrary:=fLibrary;
    end end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.CheckDatabaseName;
begin
  fDBAlias.CheckAlias
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetDBAlias:TFXDBAlias;
Begin
  Result:=fDBAlias
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetProtocol:TFXProtocol;
Begin
  Result:=fDBAlias.Protocol
end;
{______________________________________________________________________________}
Class function TFXCustomDatabase.ExtractServerName(Const aDataBaseName:TFXDBName):String;
Begin
  Result:=TFXDBAlias.ExtractServerName(aDataBaseName)
end;
{______________________________________________________________________________}
Class function TFXCustomDatabase.ExtractFullFileName(Const aDataBaseName:String):String;
Begin
  Result:=TFXDBAlias.ExtractFullFileName(aDataBaseName)
end;
{______________________________________________________________________________}
Class function TFXCustomDatabase.ExtractFileNameNoExt(Const aDataBaseName:String):String;
Begin
  Result:=TFXDBAlias.ExtractFileNameNoExt(aDataBaseName)
end;
{______________________________________________________________________________}
Class function TFXCustomDatabase.BuildAlias(Const aServerName,aPort,aFileName:String):String;
Begin
  Result:=TFXDBAlias.BuildAlias(fxTCP,aServerName,aPort,aFileName)
end;
{______________________________________________________________________________}
Class procedure TFXCustomDatabase.ParseAlias(Const aDataBaseName:String;Out aProtocol:TFXProtocol;Out aServerName,aPort,aFileName:String);
Begin
  TFXDBAlias.ParseAlias(aDataBaseName,aProtocol,aServerName,aPort,aFileName);
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetFullFileName:String;
begin
  fDBAlias.CheckAlias;
  Result:=fDBAlias.FullFileName
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetFullFileNameNoExt:String;
Var fn:String;
    I:Integer;
begin
  fDBAlias.CheckAlias;
  fn:=fDBAlias.FullFileName;
  I := LastDelimiter('/' + '\' + DriveDelim, fn);
  Result := ChangeFileExt(Copy(fn, I + 1, MaxInt),EmptyStr);
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetFileName:String;
begin
  fDBAlias.CheckAlias;
  Result:=fDBAlias.FileName;
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetFileNameNoExt:String;
Var fn:String;
    I:Integer;
begin
  fDBAlias.CheckAlias;
  fn:=fDBAlias.FileName;
  I := LastDelimiter('/' + '\' + DriveDelim, fn);
  Result := ChangeFileExt(Copy(fn, I + 1, MaxInt),EmptyStr);
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetServerName:String;
Begin
  fDBAlias.CheckAlias;
  Result:=fDBAlias.ServerName
end;
{______________________________________________________________________________}
function TFXCustomDatabase.IsCurrentServer(const Value:String):Boolean;
Begin
  fDBAlias.CheckAlias;
  Result:=fDBAlias.IsSameServer(Value)
end;
{______________________________________________________________________________}
function TFXCustomDatabase.IsCurrentUser(const Value:String):Boolean;
Var s:String;
Begin
  s:=Self.Params.Values['user_name'];
  Result:=SameText(s,Value)
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetDatabaseName:TFXDBName;
Begin
  result:=fDBAlias.Alias
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.SetDatabaseName(const Value:TFXDBName);
Var NewAlias:TFXDBAlias;
begin
  if Value=EmptyStr then Begin
    if (csDesigning in ComponentState) then Begin
      Self.Connected:=False;
      end;
    Self.CheckNotConnected;
    fDBAlias.RAZ
  end else Begin
    NewAlias:=TFXDBAlias.Create;
    try NewAlias.Assign(fDBAlias);
        NewAlias.Alias:=Value;
        If Not SameText(fDBAlias.Alias,NewAlias.Alias) then Begin
          if (csDesigning in ComponentState) then Begin
            Self.Connected:=False;
            end;
          Self.CheckNotConnected;
          fDBAlias.Assign(NewAlias);
          end;
    finally
        NewAlias.Free
    end end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.CreateDatabase;
var AnsiCreateStment:AnsiString;
    tr_handle:TISC_TR_HANDLE;
begin
  tr_handle := nil;
  CheckNotConnected;
  CheckDatabaseName;
  CheckClientLibrary;
  Self.DoBeforeConnect;
  AnsiCreateStment:=AnsiString('CREATE DATABASE ''' + fDBAlias.Alias + ''' ' + Trim(Params.Text)+';');
  fLibrary.Check_dsql_execute_immediate(@fHandle, @tr_handle, 0, PAnsiChar(AnsiCreateStment));
  Self.DoAfterConnect;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DropDatabase;
begin
  CheckConnected;
  Self.CheckNotInTransaction;
  Self.DoBeforeDisconnect(False);
  fLibrary.Check_drop_database(@fHandle);
  Self.fHandle:=nil;
  DoAfterDisconnect(False);
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DoBeforeConnectEvent;
Begin
  if Assigned(BeforeConnect) then
    BeforeConnect(Self);
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DoBeforeConnect;
Begin
  {$IFDEF mFXTRACE}Self.LogConnecting;{$ENDIF}

  // Call BeforeConnect Event
  DoBeforeConnectEvent;

  // Clean Unused
  fBases.Pack;
  fTRs.Pack;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DoAfterConnectEvent;
Begin
  if Assigned(AfterConnect) then
    AfterConnect(Self);
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DoAfterConnect;
Begin
  // Notify DataSet, ...
  SendConnectEvent(True);

  // Call AfterConnect Event
  DoAfterConnectEvent;

  {$IFDEF mFXTRACE}Self.LogConnected;{$ENDIF}
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DoBeforeDisconnectEvent(Const Silent:Boolean);
Begin
  if Assigned(Self.BeforeDisconnect) then
    Self.BeforeDisconnect(Self);
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DoBeforeDisconnect(Const Silent:Boolean);
Var AdjustedTRAction:TFXTransactionAction;
    TR:TFXCustomTransaction;
    i:Integer;
    b:TFXSQLBase;
Begin
  {$IFDEF mFXTRACE}Self.LogDisconnecting;{$ENDIF}

  // BeforeDisconnect Event ....
  DoBeforeDisconnectEvent(Silent);

  // Notify DataSet, ...
  Self.SendConnectEvent(False);

  // Tell all SQL Objects that we're disconnecting.
  for i :=Pred(fBases.Count) downto 0 do begin
    b:=TFXSQLBase(fBases[i]);
    if b<>nil then
       b.BeforeDatabaseDisconnectEvent(Self,Silent);
    end;

  // Tell all connected transactions that we're disconnecting.
  // This is so transactions can commit/rollback, accordingly
  for i :=Pred(fTRs.Count) downto 0 do begin
    tr:=TFXCustomTransaction(fTRs[i]);
    if tr<>nil then Begin
      if tr.InTransaction then Begin
        {$IFDEF MFXTRACE}tr.LogMiscInfo(Format('Close TR Before DB <%s::%s> Disconnect',[Self.ClassName,Self.Name]));{$ENDIF}
        case tr.fDefaultAction of
          fxTARollbackRetaining,
          fxTARollback:AdjustedTRAction:=fxTARollback;
          else AdjustedTRAction:=fxTACommit;
          end;
        tr.InternalEndTransaction(AdjustedTRAction,Silent)
    end end end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DoAfterDisconnectEvent(Const Forced:Boolean);
Begin
  if Assigned(AfterDisconnect) then
    AfterDisconnect(Self);
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DoAfterDisconnect(Const Forced:Boolean);
Begin
  // AfterDisconnect Event ....
  DoAfterDisconnectEvent(Forced);

  // Clean Unused
  fBases.Pack;
  fTRs.Pack;

  {$IFDEF mFXTRACE}Self.LogDisconnected;{$ENDIF}
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.SetDPBAllocLen(Const NewSize: Integer);
Begin
  if NewSize>fDPBAllocLen then Begin
    FXReAlloc(fDPB,fDPBAllocLen,NewSize,False);
    fDPBAllocLen:=NewSize;
    End;
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.StartDPB(Const param: Byte);
Begin
  SetDPBAllocLen(128);
  fDPBPos:=fDPB;
  fDPBPos^:=Param;
  Inc(fDPBPos);
  fDPBLen:=1;
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.AddStrParam2DPB(Const param:Byte;Const Value: String);
var i,Len:FXUShort;
    a:AnsiString;
begin
  a:=AnsiString(Value);
  Len:=Length(a);
  if Len>255 then
    FXRaiseClientErrorFmt(Self,fxceDPBConstantInvalidValue,[_DPBConstantNames_[param],Value]);

  SetDPBAllocLen(fDPBLen+1+1+Len);

  fDPBPos^:=Param;
  Inc(fDPBPos);
  Inc(fDPBLen);

  fDPBPos^:=Len;
  Inc(fDPBPos);
  Inc(fDPBLen);

  for i:=1 to Len do Begin
    fDPBPos^:=Byte(a[i]);
    Inc(fDPBPos);
    Inc(fDPBLen);
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.AddByteParam2DPB(Const param,val: Byte);
Begin
  SetDPBAllocLen(fDPBLen+3);

  fDPBPos^:=Param;
  Inc(fDPBPos);
  Inc(fDPBLen);

  fDPBPos^:=1;
  Inc(fDPBPos);
  Inc(fDPBLen);

  fDPBPos^:=val;
  Inc(fDPBPos);
  Inc(fDPBLen);
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.AddWordParam2DPB(Const param: Byte;Const Value:Word);
Begin
  SetDPBAllocLen(fDPBLen+5);

  fDPBPos^:=Param;
  Inc(fDPBPos);
  Inc(fDPBLen);

  fDPBPos^:=2;
  Inc(fDPBPos);
  Inc(fDPBLen);

  fDPBPos^:=Byte(PAnsiChar(@Value)[0]);
  Inc(fDPBPos);
  Inc(fDPBLen);

  fDPBPos^:=Byte(PAnsiChar(@Value)[1]);
  Inc(fDPBPos);
  Inc(fDPBLen);
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.AddIntParam2DPB(Const param: Byte;Const Value:Integer);
Begin
  SetDPBAllocLen(fDPBLen+7);

  fDPBPos^:=Param;
  Inc(fDPBPos);
  Inc(fDPBLen);

  fDPBPos^:=4;
  Inc(fDPBPos);
  Inc(fDPBLen);

  fDPBPos^:=Byte(PAnsiChar(@Value)[0]);
  Inc(fDPBPos);
  Inc(fDPBLen);

  fDPBPos^:=Byte(PAnsiChar(@Value)[1]);
  Inc(fDPBPos);
  Inc(fDPBLen);

  fDPBPos^:=Byte(PAnsiChar(@Value)[2]);
  Inc(fDPBPos);
  Inc(fDPBLen);

  fDPBPos^:=Byte(PAnsiChar(@Value)[3]);
  Inc(fDPBPos);
  Inc(fDPBLen);
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.GenerateDPB;
var i,j,PBConst,iValue:Integer;
    p,pName,pValue:string;
Begin
  fDBParamsChanged:=False;
  StartDPB(isc_dpb_version1);
  for i := 0 to Pred(fDBParams.Count) do begin
    p:=Trim(fDBParams[i]);
    if p<>EmptyStr then begin
      j:=Pos('=',p);
      if j=0 then Begin
        pName:=LowerCase(p);
        pValue:=EmptyStr;
      end else begin
        pName:=LowerCase(Trim(Copy(p,1,Pred(j))));
        pValue:=Trim(Copy(p,Succ(j),MaxInt));
        end;
      if (Pos(_DPBPrefix_,pName) = 1) then
        Delete(pName, 1, Length(_TPBPrefix_));
      // Find the parameter
      PBConst := 0;
      for j := 1 to isc_dpb_last_dpb_constant do begin
        if (pName = _DPBConstantNames_[j]) then begin
          PBConst := j;
          break;
        end end;
      //A database parameter either contains a string value (case 1)
      // or an Integer value (case 2)
      // or no value at all (case 3)
      // or an error needs to be generated (case else)  }
      case PBConst of
        isc_dpb_user_name, isc_dpb_password, isc_dpb_password_enc,
        isc_dpb_sys_user_name, isc_dpb_license, isc_dpb_encrypt_key,
        isc_dpb_lc_messages,
        isc_dpb_sql_role_name:begin
          AddStrParam2DPB(PBConst,pValue);
          end;
        isc_dpb_sql_dialect:Begin
          iValue:=StrToInt(pValue);
          AddByteParam2DPB(PBConst,iValue);
          end;
        isc_dpb_lc_ctype:Begin
          // Just Ignore as we force UTF8 below
          //fFBClientCharSet:=Str2CharSet(pValue);
          //pValue:=CharSet2Str(fFBClientCharSet);
          //AddStrParam2DPB(PBConst,pValue);
          end;
        isc_dpb_num_buffers, isc_dpb_dbkey_scope, isc_dpb_force_write,
        isc_dpb_no_reserve, isc_dpb_damaged,
        isc_dpb_set_db_readonly,
        isc_dpb_verify:begin
          iValue:=StrToInt(pValue);
          AddByteParam2DPB(PBConst,iValue);
          end;
        isc_dpb_sweep:begin
          AddByteParam2DPB(PBConst,isc_dpb_records);
          end;
        isc_dpb_connect_timeout,
        isc_dpb_sweep_interval:begin
          iValue := StrToInt(pValue);
          Case iValue of
            0..255 :Begin
              AddByteParam2DPB(PBConst,iValue);
              end;
            256..65535:Begin
              AddWordParam2DPB(PBConst,iValue);
              end;
            else Begin
              AddIntParam2DPB(PBConst,iValue);
            end end end;
          isc_dpb_activate_shadow,
          isc_dpb_delete_shadow,
          isc_dpb_begin_log,
          isc_dpb_quit_log:begin
            AddByteParam2DPB(PBConst,0);
            end;
          else begin
            FXRaiseClientErrorFmt(Self,fxceDPBConstantUnknown,[p]);
    end end end end;

  AddStrParam2DPB(isc_dpb_lc_ctype,'UTF8');

  if fDPBLen=1 then
    fDPBLen:=0;
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.InternalAttach;
var attach_res:ISC_STATUS;
Begin
  if fDBParamsChanged then
    GenerateDPB;
  attach_res:=fLibrary.Call_attach_database(fDBAlias.Alias, @fHandle,fDPBLen,fDPB);
  if attach_res > 0 then begin
    fHandle := nil;
    fLibrary.ReadAndRaiseFirebirdError(Self,fDBAlias.Alias);
    end;
End;
{______________________________________________________________________________}
function TFXCustomDatabase.InternalDetach:Boolean;
var detach_res:ISC_STATUS;
Begin
  detach_res:=fLibrary.Call_detach_database(@fHandle);
  If (detach_res > 0) Then Begin
    fLibrary.ReadFirebirdError(Self);
    Result:=False;
  end else Begin
    fHandle:=nil;
    Result:=True;
    end;
End;
{______________________________________________________________________________}
procedure TFXCustomDatabase.SetConnected(Value: Boolean);
Begin
  if Self.Connected<>Value then Begin
    if Value then begin
      Assert(not (csReading in ComponentState));
      CheckDatabaseName;
      CheckClientLibrary;
      DoBeforeConnect;
      InternalAttach;
      DoAfterConnect;
    end else begin
      DoBeforeDisconnect(False);
      if not Self.InternalDetach then
        fLibrary.ReadAndRaiseFirebirdError(Self);
      DoAfterDisconnect(False);
    end end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DoConnect;
begin
  CheckNotConnected;
  CheckDatabaseName;
  CheckClientLibrary;
  DoBeforeConnect;
  InternalAttach;
  DoAfterConnect;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.DoDisconnect;
Begin
  if Self.Connected then Begin
    DoBeforeDisconnect(False);
    if not Self.InternalDetach then
      fLibrary.ReadAndRaiseFirebirdError(Self);
    DoAfterDisconnect(False);
    end;
End;
{______________________________________________________________________________}
function TFXCustomDatabase.SilentDisconnect:Boolean;
Begin
  if Self.Connected then Begin
    DoBeforeDisconnect(True);
    Result:=Self.InternalDetach;
    Self.fHandle:=nil;
    DoAfterDisconnect(Not Result);
  end else
    Result:=True;
End;
{______________________________________________________________________________}
function TFXCustomDatabase.GetIsReadOnly: Boolean;
var local_buffer: array[0.._FXLocalBufferLength_] of Byte;
    DatabaseInfoCommand: Byte;
    length,InfoValue: Integer;
    cl:IFXClientLib;
begin
  if Self.Connected then Begin
    cl:=Self.fLibrary;
    Assert(Self.fLibrary<>nil);
    DatabaseInfoCommand:=isc_info_ods_version;
    cl.Check_database_info(Self,@fHandle, 1, @DatabaseInfoCommand,SizeOf(local_buffer),@local_buffer[0]);
    length := mFX.Header.vax_integer(@local_buffer[1], 2);
    InfoValue:= mFX.Header.vax_integer(@local_buffer[3], length);
    if (InfoValue>=10) then Begin
      DatabaseInfoCommand:=isc_info_db_read_only;
      cl.Check_database_info(Self,@fHandle, 1, @DatabaseInfoCommand,SizeOf(local_buffer),@local_buffer[0]);
      length := mFX.Header.vax_integer(@local_buffer[1], 2);
      InfoValue:= mFX.Header.vax_integer(@local_buffer[3], length);
      fIsReadOnly:=(InfoValue<>0);
    End end;
  Result:=fIsReadOnly
end;
{______________________________________________________________________________}
function TFXCustomDatabase.IsShutDown(Const aMasterKey:String;Var ShutDown:Boolean):Boolean;
Var stat:TFXStatisticalService;
    Line,Attributes:String;
    p:Integer;
Begin
  Result:=False;
  stat:=TFXStatisticalService.Create(nil);
  Try stat.ClientLibrary:= Self.ClientLibrary;
      stat.ServerName   := Self.ServerName;
      stat.FileName     := Self.FullFileName;
      stat.Params.Values['user_name'     ]:='SYSDBA';
      stat.Params.Values['password'      ]:=aMasterKey;
      stat.Attach();
      stat.FetchStats;
      while not stat.Eof do Begin
        Line:=stat.FetchNextLine;
        p:=Pos('Attributes',Line);
        if p>0 then Begin
          // FORCE WRITE, MULTI-USER MAINTENANCE, READ ONLY
          Attributes:=UpperCase(Trim(Copy(Line,p+12,MaxInt)));
          if Pos('MULTI-USER MAINTENANCE',Attributes)>0 then Begin
            Result:=True;
          End else
          if Pos('SINGLE-USER MAINTENANCE',Attributes)>0 then Begin
            Result:=True;
          End else
          if Pos('FULL SHUTDOWN',Attributes)>0 then Begin
            Result:=True;
          End else
          if Pos('SHUTDOWN',Attributes)>0 then Begin
            Result:=True;
        end end end;
      stat.Detach;
      FreeAndNil(stat);
  except on e:exception do Begin
      Result:=False;
      stat.free;
  end end;
End;
{______________________________________________________________________________}
function TFXCustomDatabase.TestConnection:TFXConnectResult;
var local_buffer: array[0.._FXLocalBufferLength_] of Byte;
    DatabaseInfoCommand: AnsiChar;
    info_res:ISC_STATUS;
begin
  if Self.Connected then begin
    // poke the server to see if connected
    DatabaseInfoCommand := AnsiChar(isc_info_base_level);
    info_res:=fLibrary.Call_database_info(@fHandle,1,@DatabaseInfoCommand,SizeOf(local_buffer),@local_buffer[0]);
    if info_res > 0 then Begin
      fLibrary.ReadFirebirdError(Self);
      result := fxConnectionLost
    end else
      result := fxConnectionTestOK
  end else
    result := fxNotConnected
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.CheckConnection;
var local_buffer: array[0.._FXLocalBufferLength_] of AnsiChar;
    DatabaseInfoCommand:AnsiChar;
    info_res:ISC_STATUS;
    Forced:Boolean;
begin
  CheckConnected;
  //poke the server to see if connected
  DatabaseInfoCommand := AnsiChar(isc_info_base_level);
  info_res:=fLibrary.Call_database_info(@fHandle, 1, @DatabaseInfoCommand,SizeOf(local_buffer),@local_buffer[0]);
  if info_res > 0 then Begin
    // Save Error
    fLibrary.ReadFirebirdError(Self);
    // Force Close
    DoBeforeDisconnect(True);
    Forced:=Not Self.InternalDetach;
    Self.fHandle:=nil;
    DoAfterDisconnect(Forced);
    // Raise Error
    fLibrary.RaiseLastFirebirdError(Self);
    end;
end;
{______________________________________________________________________________}
function TFXCustomDatabase.ConnectedUsers:Integer;
var local_buffer: array[0.._FXHugeLocalBufferLength_] of Byte;
    DatabaseInfoCommand: AnsiChar;
    user_length:Byte;
    p:PAnsiChar;
begin
  Result:=0;
  CheckConnected;
  DatabaseInfoCommand := AnsiChar(isc_info_user_names);
  fLibrary.Check_database_info(Self,@Self.Handle,1,@DatabaseInfoCommand,SizeOf(local_buffer),@local_buffer[0]);
  p:=@local_buffer;
  while p^ = AnsiChar(isc_info_user_names) do begin
    Inc(p,3);
    user_length:=Byte(p^);
    Inc(p,1);
    Inc(p,user_length);
    Inc(Result);
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomDatabase.CloseDataSets;
var Cnt,i: Integer;
begin
  Cnt:=Self.DataSetCount;
  for i:=Pred(Cnt) downto 0 do
    if (DataSets[i] <> nil) then
      DataSets[i].Close;
end;
{______________________________________________________________________________}
function TFXCustomDatabase.GetLastFirebirdError:String;
Begin
  Result:=fLibrary.LastFirebirdErrorMsg(Self)
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXCustomTransaction.Create(AOwner: TComponent);
Var db:TFXCustomDatabase;
begin
  inherited Create(AOwner);
  fDBs       := TList.Create;
  fBases     := TList.Create;
  FTRParams  := TStringList.Create;
  TStringList(FTRParams).OnChange := TRParamsChange;
  TStringList(FTRParams).OnChanging := TRParamsChanging;
  fTRParamsChanged := True;
  fDefaultAction   := fxTACommit;
  if AOwner is TFXCustomDatabase then Begin
    db:=TFXCustomDatabase(AOwner);
    Self.SetDefaultDatabase(db);
    if db.MasterTR=nil then
      DB.MasterTR:=Self
    end;
end;
{______________________________________________________________________________}
destructor TFXCustomTransaction.Destroy;
begin
  if Self.InTransaction then
    SilentDefaultClose;
  NotifySQLObjects;
  RemoveSQLObjects;
  RemoveDatabases;
  FTRParams.Free;
  if fTPBAllocLen>0 then
    FXFree(fTPB);
  fBases.Free;
  fDBs.Free;
  inherited
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.AddSQLObject(Const Value: TFXSQLBase);
Var c:Integer;
begin
  Assert(Value<>nil);
  Assert(fBases.IndexOf(Value)<0);

  c:=Pred(fBases.Count);
  While (c>0) do Begin
    if fBases[c]<>nil then
      Break;
    if fBases[Pred(c)]<>nil then
      Break;
    Dec(c);
    end;

  If (c>=0)and(fBases[c]=nil) then
    fBases[c]:=Value else
    fBases.Add(Value)
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.RemoveSQLObject(Const Value:TFXSQLBase);
var b:TFXSQLBase;
    i:Integer;
begin
  Assert(Value<>nil);
  Assert(fBases.IndexOf(Value)>=0);
  for i:=Pred(fBases.Count) downto 0 do Begin
    b:=fBases[i];
    if b=Value then Begin
      fBases[i]:=nil;
      if b.Transaction=Self then
        b.Transaction:=nil;
      exit;
    end end;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.RemoveSQLObjects;
var b:TFXSQLBase;
    i:Integer;
begin
  for i:=Pred(fBases.Count) downto 0 do Begin
    b:=TFXSQLBase(fBases[i]);
    if b<>nil Then Begin
      fBases[i]:=nil;
      if b.Transaction=Self then
        b.Transaction:=nil;
    end end
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.NotifySQLObjects;
var i:Integer;
    b:TFXSQLBase;
begin
  for i :=Pred(fBases.Count) downto 0 do Begin
    b:=TFXSQLBase(fBases[i]);
    if b<>nil then Begin
      {$IFDEF MFXTRACE}b.LogMiscInfo(format('Transaction <%s::%s> Free',[Self.ClassName,Self.Name]));{$ENDIF}
      b.TransactionFreeEvent(Self);
    end end;
End;
{______________________________________________________________________________}
procedure TFXCustomTransaction.Notification( AComponent: TComponent;Operation: TOperation);
var db:TFXCustomDatabase;
    sql:TFXSQLBase;
    i:Integer;
begin
  inherited Notification( AComponent, Operation);
  Case Operation of
    opRemove:Begin
      If (AComponent is TFXCustomTransaction) then begin
        for i:=Pred(fDBs.Count) downto 0 do begin
          db:=TFXCustomDatabase(fDBs[i]);
          if db=AComponent then begin
            db.RemoveTransaction(Self);
            fDBs[i]:=nil;
            break;
          end end;
      end else
      If (AComponent is TFXSQLBase) then begin
        for i:=Pred(fBases.Count) downto 0 do begin
          sql:=TFXSQLBase(fBases[i]);
          if sql=AComponent then begin
            sql.Database:=nil;
            fBases[i]:=nil;
            break;
    end end end end end;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.CheckHasDatabase(Const Value: TFXCustomDatabase);
var db:TFXCustomDatabase;
    i: Integer;
begin
  //Try to find a TR ...
  for i := 0 to Pred(fDBs.Count) do Begin
    db:=TFXCustomDatabase(fDBs[i]);
    if (db=Value) Then Begin
      exit;
    end end;
  FXRaiseClientErrorMsg(Self,fxceDBNotInTransactionDBs,Format('TR <%s;%s> DB Not Assigned',[Self.ClassName,Self.Name]));
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.AddDatabase(Const Value:TFXCustomDatabase);
Var db:TFXCustomDatabase;
    i:Integer;
begin
  Assert(Value<>nil);

  for i:=Pred(fDBs.Count) downto 0 do begin
    db:=TFXCustomDatabase(fDBs[i]);
    if Value = db then begin
      exit;
    end end;

  CheckNotInTransaction;

  for i:=Pred(fDBs.Count) downto 0 do begin
    db:=TFXCustomDatabase(fDBs[i]);
    if (db<>nil) then begin
      if db.fLibrary=nil then
        db.fLibrary:=Value.fLibrary else
      if db.fLibrary<>Value.fLibrary then
        FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,Format('TR <%s;%s> Add DB <%s;%s> : Invalid Client Lib',[Self.ClassName,Self.Name,Value.ClassName,Value.Name]));
    end end;

  for i:=Pred(fDBs.Count) downto 0 do begin
    if fDBs[i]=nil Then Begin
      fDBs[i]:=Value;
      Value.AddTransaction(Self);
      exit;
    end end;

  if fLibrary=nil then
    fLibrary:=Value.fLibrary;
  fDBs.Add(Value);
  Value.AddTransaction(Self);
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.RemoveDatabase(Const Value: TFXCustomDatabase);
var i, cnt: Integer;
begin
  if Value<>nil then Begin
    cnt:=Pred(fDBs.Count);
    for i := cnt downto 0 do Begin
      if fDBs[i]=Value then begin
        CheckNotInTransaction;
        fDBs[i]:=nil;
        Value.RemoveTransaction(Self);
        Break
    end end end
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.RemoveDisconnectedDatabases;
var db: TFXCustomDatabase;
    i, cnt: Integer;
begin
  cnt:=Pred(fDBs.Count);
  for i:=cnt downto 0 do Begin
    db:= TFXCustomDatabase(fDBs[i]);
    if (db<>nil)and(not db.Connected) then Begin
      CheckNotInTransaction;
      fDBs[i]:=nil;
      db.RemoveTransaction(Self);
    end end
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.RemoveDatabases;
var db: TFXCustomDatabase;
    i, cnt: Integer;
begin
  CheckNotInTransaction;
  cnt:=Pred(fDBs.Count);
  for i:=cnt downto 0 do Begin
    db:= TFXCustomDatabase(fDBs[i]);
    if db<>nil then Begin
      fDBs[i]:=nil;
      db.RemoveTransaction(Self);
    end end
end;
{______________________________________________________________________________}
function TFXCustomTransaction.GetDatabaseCount: Integer;
var i, Cnt: Integer;
begin
  result := 0;
  Cnt := Pred(fDBs.Count);
  for i := 0 to Cnt do Begin
    if FDBs[i] <> nil then
      Inc(result);
    end;
end;
{______________________________________________________________________________}
function TFXCustomTransaction.GetDatabase(Const Index: Integer): TFXCustomDatabase;
var b:TFXCustomDatabase;
    i, Nb, Cnt: Integer;
begin
  Nb:=0;
  Cnt := Pred(fDBs.Count);
  for i := 0 to Cnt do Begin
    b:=TFXCustomDatabase(fDBs[i]);
    if b=nil then
      Continue;
    if Nb=Index then Begin
      Result:=b;
      exit;
      end;
    Inc(Nb);
    end;
  result := nil;
end;
{______________________________________________________________________________}
function TFXCustomTransaction.GetDefaultDatabase:TFXCustomDatabase;
Begin
  if fDBs.Count>0 then
    Result:=TFXCustomDatabase(fDBs[0]) else
    Result:=nil
End;
{______________________________________________________________________________}
procedure TFXCustomTransaction.SetDefaultDatabase(Const Value: TFXCustomDatabase);
var db:TFXCustomDatabase;
    i:Integer;
begin
  if Value=nil then Begin
    // Remove All !
    Self.RemoveDatabases;
    Exit;
    end;

  // Find if in List
  for i:=Pred(fDBs.Count) downto 0 do begin
    db:=TFXCustomDatabase(fDBs[i]);
    if db=Value then Begin
      if i<>0 then
        fDBs.Move(i,0);
      Exit;
    end end;

  // Add to
  CheckNotInTransaction;

  for i:=Pred(fDBs.Count) downto 0 do begin
    db:=TFXCustomDatabase(fDBs[i]);
    if (db<>nil) then begin
      if db.fLibrary=nil then
        db.fLibrary:=Value.fLibrary else
      if db.fLibrary<>Value.fLibrary then
        FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,Format('TR <%s;%s> Add DB <%s;%s> : Invalid Client Lib',[Self.ClassName,Self.Name,Value.ClassName,Value.Name]));
    end end;

  Self.fLibrary:=Value.fLibrary;
  if (fDBs.Count>0)and(fDBs[0]=nil) then
    fDBs[0]:=Value else
    fDBs.Insert(0,Value);
  Value.AddTransaction(Self);
end;
{______________________________________________________________________________}
function TFXCustomTransaction.FindDatabase(Const Value: TFXCustomDatabase): Boolean;
var i, cnt: Integer;
begin
  cnt:=Pred(fDBs.Count);
  for i := 0 to cnt do Begin
    if Value=fDBs[i] then begin
      exit(True);
    end end;
  result := False;
end;
{______________________________________________________________________________}
function TFXCustomTransaction.GetInTransaction: Boolean;
begin
  result := (fHandle <> nil);
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.CheckNotInTransaction;
begin
  if (fHandle <> nil) then
    FXRaiseClientError(Self,fxceInTransaction);
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.CheckInTransaction;
begin
  if (fHandle = nil) then
    FXRaiseClientError(Self,fxceNotInTransaction);
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.SetTRParams(Const Value: TStrings);
begin
  FTRParams.Assign(Value);
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.TRParamsChange(Sender: TObject);
begin
  FTRParamsChanged := True;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.TRParamsChanging(Sender: TObject);
begin
  if csDesigning in ComponentState then
    Self.SilentDefaultClose;
  CheckNotInTransaction;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.DefaultIsolation;
Begin
  CheckNotInTransaction;
  With Self.Params do Begin
    Clear;
    Add('read_committed');
    Add('rec_version');
    Add('nowait');
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.WaitIsolation;
Begin
  CheckNotInTransaction;
  With Self.Params do Begin
    Clear;
    Add('read_committed');
    Add('rec_version');
    Add('wait');
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.WaitIsolation(Const LockTimeOut:Integer);
Begin
  CheckNotInTransaction;
  With Self.Params do Begin
    Clear;
    Add('read_committed');
    Add('rec_version');
    Add('wait');
    Add('lock_timeout='+IntToStr(LockTimeOut));
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.ReadOnlyIsolation;
Begin
  CheckNotInTransaction;
  With Self.Params do Begin
    Clear;
    Add('read_committed');
    Add('rec_version');
    Add('nowait');
    Add('read');
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.NoWaitIsolation;
Begin
  CheckNotInTransaction;
  With Self.Params do Begin
    Clear;
    Add('concurrency');
    //TODO Add('rec_version');
    Add('nowait');
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.NoWaitNoVersionIsolation;
Begin
  CheckNotInTransaction;
  With Self.Params do Begin
    Clear;
    Add('read_committed');
    Add('no_rec_version');
    Add('nowait');
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.SetActive(Const Value: Boolean);
begin
  if Not (csReading in ComponentState) then Begin
    if (Value) Then Begin
      if not Self.InTransaction then
        Self.InternalStartTransaction;
    end else Begin
      if (InTransaction) then
        Self.DefaultClose;
    end end;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.SetTPBAllocLen(Const NewSize: Integer);
Begin
  if NewSize>fTPBAllocLen then Begin
    FXReAlloc(fTPB,fTPBAllocLen,NewSize,False);
    fTPBAllocLen:=NewSize;
    End;
End;
{______________________________________________________________________________}
procedure TFXCustomTransaction.StartTPB(Const param: Byte);
Begin
  SetTPBAllocLen(128);
  fTPBPos:=fTPB;
  fTPBPos^:=Param;
  Inc(fTPBPos);
  fTPBLen:=1;
End;
{______________________________________________________________________________}
procedure TFXCustomTransaction.AddParam2TPB(Const param: Byte);
Begin
  SetTPBAllocLen(fTPBLen+1);

  fTPBPos^:=Param;
  Inc(fTPBPos);
  Inc(fTPBLen);
End;
{______________________________________________________________________________}
procedure TFXCustomTransaction.AddStrParam2TPB(Const param:Byte;Const Value: String);
var i,Len:FXUShort;
    a:AnsiString;
begin
  a:=AnsiString(Value);
  Len:=Length(a);
  if Len>255 then
    FXRaiseClientErrorFmt(Self,fxceTPBConstantInvalidValue,[_DPBConstantNames_[param],Value]);

  SetTPBAllocLen(fTPBLen+1+1+Len);

  fTPBPos^:=Param;
  Inc(fTPBPos);
  Inc(fTPBLen);

  fTPBPos^:=Len;
  Inc(fTPBPos);
  Inc(fTPBLen);
  for i:=1 to Len do Begin
    fTPBPos^:=Byte(a[i]);
    Inc(fTPBPos);
    Inc(fTPBLen);
    end;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.AddIntParam2TPB(Const param: Byte;Const Value:Integer);
Begin
  SetTPBAllocLen(fTPBLen+7);

  fTPBPos^:=Param;
  Inc(fTPBPos);
  Inc(fTPBLen);

  fTPBPos^:=4;
  Inc(fTPBPos);
  Inc(fTPBLen);

  fTPBPos^:=Byte(PAnsiChar(@Value)[0]);
  Inc(fTPBPos);
  Inc(fTPBLen);

  fTPBPos^:=Byte(PAnsiChar(@Value)[1]);
  Inc(fTPBPos);
  Inc(fTPBLen);

  fTPBPos^:=Byte(PAnsiChar(@Value)[2]);
  Inc(fTPBPos);
  Inc(fTPBLen);

  fTPBPos^:=Byte(PAnsiChar(@Value)[3]);
  Inc(fTPBPos);
  Inc(fTPBLen);
End;
{______________________________________________________________________________}
procedure TFXCustomTransaction.GenerateTPB;
var i, j, PBConst, iValue: Integer;
    p,pName,pValue:string;
begin
  StartTPB(isc_tpb_version3);
  fTRParamsChanged := False;
  for i := 0 to Pred(fTRParams.Count) do begin
    p:=Trim(fTRParams[i]);
    if p<>EmptyStr then begin
      j:=Pos('=',p);
      if j=0 then Begin
        pName:=LowerCase(p);
        pValue:=EmptyStr;
      end else begin
        pName:=LowerCase(Trim(Copy(p,1,Pred(j))));
        pValue:=Trim(Copy(p,Succ(j),MaxInt));
        end;
      if (Pos(_TPBPrefix_,pName) = 1) then
        Delete(pName, 1, Length(_TPBPrefix_));
      // Find the parameter
      PBConst := 0;
      for j := 1 to isc_tpb_last_tpb_constant do begin
        if (pName = _TPBConstantNames_[j]) then begin
          PBConst := j;
          break;
        end end;
      // Now act on it
      case PBConst of
        isc_tpb_consistency, isc_tpb_exclusive, isc_tpb_protected,
        isc_tpb_concurrency, isc_tpb_shared, isc_tpb_wait, isc_tpb_nowait,
        isc_tpb_read, isc_tpb_write, isc_tpb_ignore_limbo,
        isc_tpb_read_committed, isc_tpb_rec_version, isc_tpb_no_rec_version,
        isc_tpb_no_auto_undo:Begin
          AddParam2TPB(PBConst);
          end;
        isc_tpb_lock_read, isc_tpb_lock_write:begin
          AddStrParam2TPB(PBConst,pValue);
          end;
        isc_tpb_lock_timeout:begin
          iValue := StrToInt(pValue);
          if iValue>0 then
            AddIntParam2TPB(PBConst,iValue) else
            AddIntParam2TPB(PBConst,-1);
          end;
        else begin
          FXRaiseClientErrorFmt(Self,fxceTPBConstantUnknown, [p]);
    end end end end;

  if fTPBLen=1 then
    fTPBLen:=0;
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.StartTransaction;
Begin
  CheckNotInTransaction;
  InternalStartTransaction;
End;
{______________________________________________________________________________}
procedure TFXCustomTransaction.Commit;
begin
  CheckInTransaction;
  InternalEndTransaction(fxTACommit,False)
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.CommitRetaining;
begin
  CheckInTransaction;
  InternalEndTransaction(fxTACommitRetaining,False)
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.Rollback;
begin
  CheckInTransaction;
  InternalEndTransaction(fxTARollback,False)
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.RollbackRetaining;
begin
  CheckInTransaction;
  InternalEndTransaction(fxTARollbackRetaining,False)
end;
{______________________________________________________________________________}
function TFXCustomTransaction.Start_TR:Boolean;
Begin
  Result:=False;
  if not Intransaction Then Begin
    StartTransaction;
    Result:=true;
    end
end;
{______________________________________________________________________________}
Procedure TFXCustomTransaction.ReStart_TR;
Begin
  if Intransaction Then
    Commit;
  StartTransaction;
end;
{______________________________________________________________________________}
function TFXCustomTransaction.Commit_TR:Boolean;
Begin
  Result:=False;
  if Intransaction Then Begin
    Result:=True;
    Commit;
    end
end;
{______________________________________________________________________________}
function TFXCustomTransaction.CommitRetaining_TR:Boolean;
Begin
  Result:=False;
  if Intransaction Then Begin
    CommitRetaining;
    Result:=True;
    end
end;
{______________________________________________________________________________}
function TFXCustomTransaction.Rollback_TR:Boolean;
Begin
  Result:=False;
  if Intransaction Then Begin
    Result:=True;
    Rollback;
    end
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.DefaultClose;
Var AdjustedAction:TFXTransactionAction;
Begin
  CheckInTransaction;
  case fDefaultAction of
    fxTARollbackRetaining,
    fxTARollback:AdjustedAction:=fxTARollback;
    else AdjustedAction:=fxTACommit;
    end;
  InternalEndTransaction(AdjustedAction,False)
end;
{______________________________________________________________________________}
function TFXCustomTransaction.RollbackRetaining_TR:Boolean;
Begin
  Result:=False;
  if Intransaction Then Begin
    RollbackRetaining;
    Result:=True;
    end
end;
{______________________________________________________________________________}
function TFXCustomTransaction.SilentClose(Const aAction:TFXTransactionAction):Boolean;
Var AdjustedAction:TFXTransactionAction;
Begin
  if (fHandle <> nil) then Begin
    case aAction of
      fxTARollbackRetaining,
      fxTARollback:AdjustedAction:=fxTARollback;
      else AdjustedAction:=fxTACommit;
      end;
    Result:=InternalEndTransaction(AdjustedAction,True);
  end else
    Result:=True
end;
{______________________________________________________________________________}
function TFXCustomTransaction.SilentDefaultClose:Boolean;
Var AdjustedAction:TFXTransactionAction;
Begin
  if (fHandle <> nil) then Begin
    case fDefaultAction of
      fxTARollbackRetaining,
      fxTARollback:AdjustedAction:=fxTARollback;
      else AdjustedAction:=fxTACommit;
      end;
    Result:=InternalEndTransaction(AdjustedAction,True);
  end else
    Result:=True
end;
{______________________________________________________________________________}
procedure TFXCustomTransaction.InternalStartTransaction;
var db:TFXCustomDatabase;
    start_res:ISC_STATUS;
    pteb:PISC_TEB_ARRAY;
    cl:IFXClientLib;
    ErrMsg:String;
    i,j,Nb:Integer;
begin
  {$IFDEF mFXTRACE}Self.LogTRStarting;{$ENDIF mFXTRACE}

  // Remove not connected DBs
  cl:=nil;
  Nb:=0;
  for i :=Pred(fDBs.Count) downto 0 do begin
    db:=TFXCustomDatabase(fDBs[i]);
    if db<>nil then begin
      if not db.Connected then Begin
        Self.RemoveDatabase(db);
      end else
      if cl=nil then Begin
        db.CheckClientLibrary;
        cl:=db.fLibrary;
        Inc(Nb);
      end else
      if (db.fLibrary=cl) then Begin
        Inc(Nb);
      end else Begin
        ErrMsg:=Format('TR <%s;%s> DB <%s;%s> : Invalid Client Lib',[Self.ClassName,Self.Name,db.ClassName,db.Name]);
        {$IFDEF mFXTRACE}Self.LogTREnded(ErrMsg);{$ENDIF mFXTRACE}
        FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,ErrMsg);
    end end end;
  if Nb = 0 then Begin
    {$IFDEF mFXTRACE}Self.LogTREnded('No DBs');{$ENDIF mFXTRACE}
    FXRaiseClientError(Self,fxceNoDatabasesInTransaction);
    end;

  if fTRParamsChanged then
    Self.GenerateTPB;

  pteb:=nil;
  FXAlloc(Pointer(pteb),Nb*SizeOf(TISC_TEB),False);
  try j:=0;
      for i := 0 to Pred(fDBs.Count) do begin
        db:=TFXCustomDatabase(fDBs[i]);
        if  db<>nil then begin
          pteb[j].db_handle  :=@(db.Handle);
          pteb[j].tpb_length := fTPBLen;
          if fTPBLen>0 then
            pteb[j].tpb_address:=fTPB else
            pteb[j].tpb_address:=nil;
          Inc(j)
        end end;
      start_res:=cl.Call_start_multiple(@fHandle,Nb,@pteb[0]);
      if start_res > 0 then begin
        fHandle := nil;
        {$IFDEF mFXTRACE}Self.LogTREnded('Start Error');{$ENDIF mFXTRACE}
        cl.ReadAndRaiseFirebirdError(Self)
        end;
  finally
      FXFree(Pointer(pteb))
  end
end;
{______________________________________________________________________________}
function TFXCustomTransaction.InternalEndTransaction(Const Action: TFXTransactionAction;Const Silent: Boolean):Boolean;
var cl:IFXClientLib;
    end_res:ISC_STATUS;
    Forced:Boolean;
    b:TFXSQLBase;
    i:Integer;
begin
  ///  1- Tell all SQL Objects that we're ending Transcation .
  for i :=Pred(fBases.Count) downto 0 do begin
    b:=TFXSQLBase(fBases[i]);
    if b<>nil then
      b.BeforeTransactionEndEvent(Self,Action,Silent);
    end;

  // Call TR API
  cl:=Self.ClientLibrary;
  Case Action of
    fxTARollbackRetaining:end_res := cl.Call_rollback_retaining(@fHandle);
    fxTARollback         :end_res := cl.Call_rollback_transaction(@fHandle);
    fxTACommitRetaining  :end_res := cl.Call_commit_retaining(@fHandle);
    else                  end_res := cl.Call_Commit_transaction(@fHandle);
    end;
  if (end_res>0) Then Begin
    cl.ReadFirebirdError(Self);
    if not Silent then
      cl.RaiseLastFirebirdError(Self);
    Forced:=True;
    Result:=False;
  End else Begin
    Forced:=False;
    Result:=True;
    end;
  Case Action of
    fxTARollbackRetaining,
    fxTACommitRetaining  :Begin
      //Handle still Valid
      if Forced then
        fHandle:=nil;
      End;
    else Begin
      fHandle:=nil;
    end end;

  //Some SQL Objects can be removed
  fBases.Pack;
  fDBs.Pack;

  {$IFDEF mFXTRACE}Self.LogTREnded(Action);{$ENDIF mFXTRACE}
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXCustomSchema.Create(Const aDB: TFXCustomDatabase);
begin
  fTerminator    := ';';
  fDB            := aDB;
  if aDB<>nil then
    fTRx         := aDB.ROAutoTR;
  fNodes         := TFXList.Create;
  fTables        := TList.Create;
  fViews         := TList.Create;
  fFields        := TList.Create;
  fPrimaryKeys   := TList.Create;
end;
{______________________________________________________________________________}
destructor TFXCustomSchema.Destroy;
begin
  fCheckConstraints.Free;
  fTriggers      .Free;
  fIndexes       .Free;
  fForeignKeys   .Free;
  fPrimaryKeys   .Free;
  fFields        .Free;
  fTables        .Free;
  fViews         .Free;

  fUDFs          .Free;
  fExceptions    .Free;
  fGenerators    .Free;
  fProcs         .Free;
  fRoles         .Free;
  fDomains       .Free;
  fCharsets      .Free;

  fNodes         .Free;

  inherited;
end;
{______________________________________________________________________________}
procedure TFXCustomSchema.Clear;
begin
  if fCheckConstraints<>nil then
    fCheckConstraints.Clear;
  if fTriggers<>nil then
    fTriggers.Clear;
  if fIndexes<>nil then
    fIndexes.Clear;
  if fForeignKeys<>nil then
    fForeignKeys.Clear;
  if fPrimaryKeys<>nil then
    fPrimaryKeys.Clear;
  if fFields<>nil then
    fFields.Clear;
  if fTables<>nil then
    fTables.Clear;
  if fViews<>nil then
    fViews.Clear;

  if fUDFs<>nil then
    fUDFs.Clear;
  if fExceptions<>nil then
    fExceptions.Clear;
  if fGenerators<>nil then
    fGenerators.Clear;
  if fProcs<>nil then
    fProcs.Clear;
  if fRoles<>nil then
    fRoles.Clear;
  if fDomains<>nil then
    fDomains.Clear;
  if fCharsets<>nil then
    fCharsets.Clear;

  fNodes.Clear;
end;
{______________________________________________________________________________}
function TFXCustomSchema.QuoteIdentifier(Const Value:String):String;
Var s:String;
Begin
  s:=UpperCase(Value);
  if s<>Value Then Begin
    //do Quote !
    Result:=AnsiQuotedStr(Value,'"');
  end else Begin
    //do not Quote !
    Result:=Value
    end;
end;
{______________________________________________________________________________}
function TFXCustomDatabase.FindMetaRelation(Const aRelation:String):Boolean;
Var MetaRelation:TFXCustomMetaRelation;
    Schema:TFXCustomMetaSchema;
Begin
  Schema:=Self.GetSchema as TFXCustomMetaSchema;
  MetaRelation:=Schema.FindRelation(nil,aRelation);
  Result:=MetaRelation<>nil
end;
{______________________________________________________________________________}
function TFXCustomDatabase.FindMetaRelation(Const aTR:TFXCustomTransaction;Const aRelation:String):Boolean;
Var MetaRelation:TFXCustomMetaRelation;
    Schema:TFXCustomMetaSchema;
Begin
  Schema:=Self.GetSchema as TFXCustomMetaSchema;
  MetaRelation:=Schema.FindRelation(aTR,aRelation);
  Result:=MetaRelation<>nil
end;
{______________________________________________________________________________}
function TFXCustomDatabase.FindMetaRelation(Const aRelation:String;Out MetaRelation : TFXCustomSchemaNode):Boolean;
Var Schema:TFXCustomMetaSchema;
Begin
  Schema:=Self.GetSchema as TFXCustomMetaSchema;
  MetaRelation:=Schema.FindRelation(nil,aRelation);
  Result:=MetaRelation<>nil
end;
{______________________________________________________________________________}
function TFXCustomDatabase.FindMetaRelation(Const aTR:TFXCustomTransaction;Const aRelation:String;Out MetaRelation : TFXCustomSchemaNode):Boolean;
Var Schema:TFXCustomMetaSchema;
Begin
  Schema:=Self.GetSchema as TFXCustomMetaSchema;
  MetaRelation:=Schema.FindRelation(aTR,aRelation);
  Result:=MetaRelation<>nil
end;
{______________________________________________________________________________}
function TFXCustomDatabase.FindMetaRelationField(Const aRelation, aField:String;Out MetaField: TFXCustomSchemaNode):Boolean;
Var Schema:TFXCustomMetaSchema;
Begin
  Schema:=Self.GetSchema as TFXCustomMetaSchema;
  MetaField:=Schema.FindRelationField(nil,aRelation, aField);
  Result:=MetaField<>nil
end;
{______________________________________________________________________________}
function TFXCustomDatabase.FindMetaRelationField(Const aTR:TFXCustomTransaction;Const aRelation, aField:String;Out MetaField: TFXCustomSchemaNode):Boolean;
Var Schema:TFXCustomMetaSchema;
Begin
  Schema:=Self.GetSchema as TFXCustomMetaSchema;
  MetaField:=Schema.FindRelationField(aTR,aRelation, aField);
  Result:=MetaField<>nil
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXSQLBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TFXCustomDatabase then
    Self.SetDatabase(TFXCustomDatabase(AOwner)) else
  if AOwner is TFXCustomTransaction then
    Self.SetTransaction(TFXCustomTransaction(AOwner)) else
  if AOwner is TFXSQLBase then Begin
    Self.SetDatabase(TFXSQLBase(AOwner).Database);
    Self.SetTransaction(TFXSQLBase(AOwner).Transaction);
    end;
end;
{______________________________________________________________________________}
destructor TFXSQLBase.Destroy;
Var otr:TFXCustomTransaction;
    odb:TFXCustomDatabase;
begin
  Self.InternalClose;

  otr:=fTransaction;
  fTransaction:=nil;
  if (otr<> nil) then
    otr.RemoveSQLObject(Self);

  odb:=fDatabase;
  fDatabase:=nil;
  fLibrary :=nil;
  if (odb<> nil) then
    odb.RemoveSQLObject(Self);

  fLastException.Free;

  inherited
end;
{______________________________________________________________________________}
procedure TFXSQLBase.CheckHasDatabase;
begin
  if (fDatabase=nil)or(fLibrary=nil) then
    FXRaiseClientError(Self,fxceDatabaseNotAssigned);
  Assert(fDatabase.fLibrary=fLibrary);
end;
{______________________________________________________________________________}
function TFXSQLBase.GetClientCodePage:Integer;
Begin
  CheckHasDatabase;
  fDatabase.CheckConnected;
  Result:=CharSet2CodePage(fDatabase.ClientCharSet);
End;
{______________________________________________________________________________}
function TFXSQLBase.GetClientCharSet:TFXCharacterSet;
Begin
  CheckHasDatabase;
  fDatabase.CheckConnected;
  Result:=fDatabase.ClientCharSet
End;
{______________________________________________________________________________}
procedure TFXSQLBase.CheckDatabaseConnected;
begin
  CheckHasDatabase;
  fDatabase.CheckConnected;
end;
{______________________________________________________________________________}
function TFXSQLBase.GetDBHandle: PISC_DB_HANDLE;
begin
  CheckHasDatabase;
  fDatabase.CheckConnected;
  result := @fDatabase.Handle;
end;
{______________________________________________________________________________}
procedure TFXSQLBase.SetDatabase(Const Value: TFXCustomDatabase);
begin
  if (fDatabase<>Value) then Begin
    Self.CheckClosed;
    if fDatabase<>nil then
      fDatabase.RemoveSQLObject(Self);
    if (Value <> nil) then begin
      Value.AddSQLObject(Self);
      Assert(fLibrary=Value.fLibrary);
      Assert(fDatabase=Value);
    end else Begin
      fLibrary  := nil;
      fDatabase := nil;
      end;
  end else Begin
    // Keep
    end;
end;
{______________________________________________________________________________}
procedure TFXSQLBase.CheckHasTransaction;
begin
  if (fTransaction = nil) then
    FXRaiseClientError(Self,fxceTransactionNotAssigned);
end;
{______________________________________________________________________________}
procedure TFXSQLBase.CheckInTransaction;
begin
  CheckHasTransaction;
  fTransaction.CheckInTransaction;
end;
{______________________________________________________________________________}
procedure TFXSQLBase.CheckNotInTransaction;
Begin
  if fTransaction<>nil then
    fTransaction.CheckNotInTransaction;
End;
{______________________________________________________________________________}
function TFXSQLBase.GetTRHandle: PISC_TR_HANDLE;
begin
  CheckHasTransaction;
  fTransaction.CheckInTransaction;
  result := @fTransaction.Handle;
end;
{______________________________________________________________________________}
procedure TFXSQLBase.SetTransaction(Const Value: TFXCustomTransaction);
Var otr:TFXCustomTransaction;
begin
  if (fTransaction <> Value) then Begin
    otr:=fTransaction;
    if (Value <> nil) then begin
      fTransaction := Value;
      fTransaction.AddSQLObject(Self);
      if (Self.fDatabase=nil) then
        SetDatabase(fTransaction.DefaultDatabase) else
        fTransaction.AddDatabase(fDatabase);
    end else
      fTransaction := nil;
    if (otr <> nil) then
      otr.RemoveSQLObject(Self);
  end else Begin
    // Keep unchanged
    End;
end;
{______________________________________________________________________________}
function TFXSQLBase.Start_TR:Boolean;
Begin
  Result:=False;
  CheckHasDatabase;
  CheckHasTransaction;
  fDatabase.CheckConnected;
  With fTransaction do Begin
    if not Intransaction Then Begin
      InternalStartTransaction;
      Result:=true;
    end end;
end;
{______________________________________________________________________________}
Procedure TFXSQLBase.ReStart_TR;
Begin
  CheckHasDatabase;
  CheckHasTransaction;
  fDatabase.CheckConnected;
  With fTransaction do Begin
    if Intransaction Then
      InternalEndTransaction(fxTACommit,False);
    InternalStartTransaction;
    end
end;
{______________________________________________________________________________}
function TFXSQLBase.Commit_TR:Boolean;
Begin
  Result:=False;
  if fTransaction<>nil Then Begin
    With fTransaction do Begin
      if Intransaction Then Begin
        InternalEndTransaction(fxTACommit,False);
        Result:=True;
    end end end;
end;
{______________________________________________________________________________}
function TFXSQLBase.CommitRetaining_TR:Boolean;
Begin
  Result:=False;
  if fTransaction<>nil Then Begin
    With fTransaction do Begin
      if Intransaction Then Begin
        InternalEndTransaction(fxTACommitRetaining,False);
        Result:=True;
    end end end
end;
{______________________________________________________________________________}
function TFXSQLBase.Rollback_TR:Boolean;
var cl:IFXClientLib;
Begin
  Result:=False;
  if fTransaction<>nil Then Begin
    With fTransaction do Begin
      if Intransaction Then Begin
        if not InternalEndTransaction(fxTARollback,False) then Begin
          cl:=Self.ClientLibrary;
          cl.ReadAndRaiseFirebirdError(Self);
          end;
        Result:=True;
    end end end
end;
{______________________________________________________________________________}
function TFXSQLBase.RollbackRetaining_TR:Boolean;
Begin
  Result:=False;
  if fTransaction<>nil Then Begin
    With fTransaction do Begin
      if Intransaction Then Begin
        InternalEndTransaction(fxTARollbackRetaining,False);
        Result:=True;
    end end end
end;
{______________________________________________________________________________}
procedure TFXSQLBase.CloseAndRollback(Const RaiseOnError:Boolean=False);
Begin
  if not InternalClose then Begin
    if RaiseOnError then Begin
      Self.Rollback_TR;
      RaiseLastError;
    end end;

  if fTransaction<>nil Then
    fTransaction.SilentClose(fxTARollback)
End;
{______________________________________________________________________________}
Procedure TFXSQLBase.PushError;
var cl:IFXClientLib;
Begin
  FreeAndNil(fLastException);
  cl:=Self.ClientLibrary;
  cl.ReadFirebirdError(Self);
  fLastException:=cl.LastFirebirdError(Self);
End;
{______________________________________________________________________________}
Procedure TFXSQLBase.PushError(Const e:exception);
Begin
  FreeAndNil(fLastException);
  fLastException:=Exception.Create(e.message);
End;
{______________________________________________________________________________}
Procedure TFXSQLBase.PushError(Const aContext:String;Const e:exception);
Begin
  FreeAndNil(fLastException);
  fLastException:=Exception.Create(e.message);
End;
{______________________________________________________________________________}
Procedure TFXSQLBase.RaiseLastError;
Var e:Exception;
Begin
  Assert(fLastException<>nil);
  e:=fLastException;
  fLastException:=nil;
  raise e;
End;
{______________________________________________________________________________}
Procedure TFXSQLBase.CloseAndRaiseClientError(Const aClientError:TFXClientErrorCode);
Begin
  Self.InternalClose;
  Raise EFXClientError.Create(Self,aClientError);
End;
{______________________________________________________________________________}
Procedure TFXSQLBase.CloseAndRaiseClientErrorMsg(Const aClientError:TFXClientErrorCode;Const aMsg:String);
Begin
  Self.InternalClose;
  Raise EFXClientError.CreateMsg(Self,aClientError,aMsg);
End;
{______________________________________________________________________________}
Procedure TFXSQLBase.CloseAndRaiseClientErrorFmt(Const aClientError:TFXClientErrorCode;const Args:array of const);
Begin
  Self.InternalClose;
  Raise EFXClientError.CreateFmt(Self,aClientError,Args);
End;
{______________________________________________________________________________}
Procedure TFXSQLBase.ReadLastErrorCloseAndRaise;
var cl:IFXClientLib;
    e:EFXError;
Begin
  cl:=Self.ClientLibrary;
  cl.ReadFirebirdError(Self);
  e:=cl.LastFirebirdError(Self);
  InternalClose;
  raise e;
End;
{______________________________________________________________________________}
Procedure TFXSQLBase.ReadLastErrorCloseAndRaise(Const aContext:String);
var cl:IFXClientLib;
    e:EFXError;
Begin
  cl:=Self.ClientLibrary;
  cl.ReadFirebirdError(Self,aContext);
  e:=cl.LastFirebirdError(Self);
  InternalClose;
  raise e;
End;
{______________________________________________________________________________}
Procedure TFXSQLBase.ReadLastErrorCloseAndRaise(Const aClientError:TFXClientErrorCode;Const aMsg:String);
Var e:Exception;
Begin
  e:=EFXClientError.CreateMsg(Self,aClientError,aMsg);
  Self.InternalClose;
  raise e;
End;
{______________________________________________________________________________}
Procedure TFXSQLBase.ReadLastErrorCloseAndRaise(Const aClientError:TFXClientErrorCode;const Args:array of const);
Var e:Exception;
Begin
  e:=EFXClientError.CreateFmt(Self,aClientError,Args);
  InternalClose;
  raise e;
End;
{______________________________________________________________________________}
procedure TFXSQLBase.BeforeDatabaseDisconnectEvent(Const aDB: TFXCustomDatabase;Const Silent:Boolean);
begin
  if not Self.InternalClose then
    if not Silent then
      Self.RaiseLastError;
end;
{______________________________________________________________________________}
procedure TFXSQLBase.DatabaseFreeEvent(Const aDB:TFXCustomDatabase);
begin
  Self.SetDatabase(nil);
end;
{______________________________________________________________________________}
procedure TFXSQLBase.BeforeTransactionEndEvent(Const aTR: TFXCustomTransaction;Const Action:TFXTransactionAction;Const Silent:Boolean);
begin
  case Action of
    fxTARollbackRetaining,
    fxTACommitRetaining:Begin
      //Do not Close
      end;
    else Begin
      if not Self.InternalClose then
        if not Silent then
          Self.RaiseLastError;
    end end;
end;
{______________________________________________________________________________}
procedure TFXSQLBase.TransactionFreeEvent(Const aTR: TFXCustomTransaction);
begin
  Self.SetTransaction(nil);
end;

end.

