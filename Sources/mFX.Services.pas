unit mFX.Services;

interface

{$I mFX.Inc}

uses Classes,
  mFX.Classes, mFX.Intf, mFX.Header, mFX.Alias, mFX.Base, mFX.List;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  TFXSvcParam = (
    spbUserName,
    spbSysUserName,
    spbSysUserNameEnc,
    spbPassword,
    spbPasswordEnc,
    spbCommandLine,
    spbDBName,
    spbVerbose,
    spbOptions,
    spbConnectTimeout,
    spbDummyPacketInterval,
    spbSQL_role_name
    );

  TFXSvcPropertyOption = (
    spDatabase,
    spConfigParameters,
    spVersion
    );
  TFXSvcPropertyOptions = set of TFXSvcPropertyOption;

  TFXSvcValidateOption = (
    voLimboTransactions,
    voCheckDB,
    voIgnoreChecksum,
    voKillShadows,
    voMendDB,
    voSweepDB,
    voValidateDB,
    voValidateFull
    );
  TFXSvcValidateOptions = set of TFXSvcValidateOption;

  TFXSvcTransactionGlobalAction = (
    taCommitGlobal,
    taRollbackGlobal,
    taRecoverTwoPhaseGlobal,
    taNoGlobalAction
    );

  TFXSvcTransactionState = (
    tsLimboState,
    tsCommitState,
    tsRollbackState,
    tsUnknownState
    );

  TFXSvcTransactionAdvise = (
    taCommitAdvise,
    taRollbackAdvise,
    taUnknownAdvise
    );

  TFXSvcShutdownMode = (
    smForced,
    smDenyTransaction,
    smDenyAttachment
    );

  TFXSvcBackupOption = (
    boIgnoreChecksums,
    boIgnoreLimbo,
    boMetadataOnly,
    boNoGarbageCollection,
    boOldMetadataDesc,
    boNonTransportable,
    boConvertExtTables,
    boVerbose
    );
  TFXSvcBackupOptions = set of TFXSvcBackupOption;

  TFXSvcRestoreOption = (
    roDeactivateIndexes,
    roNoShadow,
    roNoValidityCheck,
    roOneRelationAtATime,
    roReplace,
    roCreateNewDB,
    roUseAllSpace,
    roVerbose
    );
  TFXSvcRestoreOptions = set of TFXSvcRestoreOption;

  TFXSvcStatMode = (
    soHeader,
    soFullStat
    );

  TFXSvcStatOption = (
    soDataPages,
    soIndexPages,
    soSystemRelations
    );
  TFXSvcStatOptions = set of TFXSvcStatOption;

  TFXSvcSecurityModifyParam = (
    smFirstName,
    smMiddleName,
    smLastName,
    smUserId,
    smGroupId,
    smPassword
    );
  TFXSvcSecurityModifyParams = set of TFXSvcSecurityModifyParam;


  TFXSvcVersionInfo = class
  private
    fServerVersion            : String;
    fServerImplementation     : string;
    fServiceVersion           : Integer;
    /// <SUMMARY>Clear</SUMMARY>
    procedure Clear;
  public
    property ServerVersion    : String read fServerVersion;
    property ServerImplementation : string read fServerImplementation;
    property ServiceVersion   : Integer read fServiceVersion;
  end;

  TFXSvcDatabaseInfo = class
  private
    fNoOfAttachments          : Integer;
    fNoOfDatabases            : Integer;
    fDbNames                  : TStrings;
    /// <SUMMARY>Clear</SUMMARY>
    procedure Clear;
    /// <SUMMARY>Get DbName</SUMMARY>
    function GetDbName(Const Idx:Integer):String;
  public
    destructor Destroy; override;

    property NoOfAttachments  : Integer read fNoOfAttachments;
    property NoOfDatabases    : Integer read fNoOfDatabases;
    property DbName[Const Idx:Integer] : String read GetDbName;
  end;

  TFXSvcServerConfigParams = class
  private
    fBaseLocation             : string;
    fLockFileLocation         : string;
    fMessageFileLocation      : string;
    fSecurityDatabaseLocation : string;
    /// <SUMMARY>Clear</SUMMARY>
    procedure Clear;
  public
    property BasePath         : string read fBaseLocation;
    property LockFilePath     : string read fLockFileLocation;
    property MessageFilePath  : string read fMessageFileLocation;
    property SecurityDBPath   : string read fSecurityDatabaseLocation;
  end;

  TFXSvcLimboTransactionInfo = class
  private
    fMultiDatabase            : Boolean;
    fID                       : Integer;
    fHostSite                 : String;
    fRemoteSite               : String;
    fRemoteDatabasePath       : String;
    fState                    : TFXSvcTransactionState;
    fAdvise                   : TFXSvcTransactionAdvise;
    fAction                   : TFXTransactionAction;
  public
    property MultiDatabase    : Boolean read fMultiDatabase;
    property ID               : Integer read fID;
    property HostSite         : String  read fHostSite;
    property RemoteSite       : String  read fRemoteSite;
    property RemoteDatabasePath : String read fRemoteDatabasePath;
    property State            : TFXSvcTransactionState read fState;
    property Advise           : TFXSvcTransactionAdvise read fAdvise;
    property Action           : TFXTransactionAction read fAction;
  end;

  TFXSvcUserInfo = class
  private
    fUserName                 : string;
    fFirstName                : string;
    fMiddleName               : string;
    fLastName                 : string;
    fGroupID                  : Integer;
    fUserID                   : Integer;
    fPassword                 : string;
    fSQLRole                  : string;
    /// <SUMMARY>Clear</SUMMARY>
    procedure Clear;
  public
    property UserName         : string  read fUserName;
    property FirstName        : string  read fFirstName;
    property MiddleName       : string  read fMiddleName;
    property LastName         : string  read fLastName;
    property GroupID          : Integer read fGroupID;
    property UserID           : Integer read fUserID;
    property Password         : string  read fPassword;
    property SQLRole          : string  read fSQLRole;
  end;

  TFXFirebirdService = class(TComponent)
  protected
    fLibrary             : IFXClientLib;
    fDBAlias             : TFXDBAlias;
    FHandle              : TISC_SVC_HANDLE;
    FOnAttach            : TNotifyEvent;
    FParamsChanged       : Boolean;
    FParams              : TStrings;
    fAttachSPB           : PFXByte;
    fAttachSPBPos        : PFXByte;
    fAttachSPBLen        : FXShort;
    fAttachSPBAllocLen   : Integer;
    fStartSPB            : PFXByte;
    fStartSPBPos         : PFXByte;
    fStartSPBLen         : Integer;
    fStartSPBAllocLen    : Integer;
    fOutputBufferAllocLen: Integer;
    fOutputBufferPos     : Integer;
    fOutputBuffer        : PFXByte;
    fOutputString        : AnsiString;
    fEof                 : Boolean;
    /// <SUMMARY>Check ClientLibrary</SUMMARY>
    procedure CheckClientLibrary;
    /// <SUMMARY>Get Active</SUMMARY>
    function GetActive: Boolean;
    /// <SUMMARY>Get Server</SUMMARY>
    function GetServerName:String;
    /// <SUMMARY>Get FileName</SUMMARY>
    function GetFileName:TFXDBName;
    /// <SUMMARY>Set FileName</SUMMARY>
    procedure SetFileName(const Value: TFXDBName);
    /// <SUMMARY>Set Server</SUMMARY>
    procedure SetServerName(const Value: string);
    /// <SUMMARY>Set params</SUMMARY>
    procedure SetParams(const Value: TStrings);
    /// <SUMMARY>On params Change</SUMMARY>
    procedure ParamsChange(Sender: TObject);
    /// <SUMMARY>On params Changing</SUMMARY>
    procedure ParamsChanging(Sender: TObject);
    /// <SUMMARY>Set Active</SUMMARY>
    procedure SetActive(const Value: Boolean);

    /// <SUMMARY>Set AttachSPB Len</SUMMARY>
    procedure SetAttachSPBLen(Const NewSize: Integer);
    /// <SUMMARY>Add Value to AttachSPB</SUMMARY>
    procedure AddParam2AttachSPB(Const param: Byte); overload;
    /// <SUMMARY>Add Value to AttachSPB</SUMMARY>
    procedure AddParam2AttachSPB(Const param:Byte;Const Value: String);overload;
    /// <SUMMARY>Generate SPB</SUMMARY>
    /// Given a string containing a textual representation of the Service parameters,
    /// generate a service parameter buffer, and return it and its length in SPB and SPBLength, respectively.
    procedure GenerateAttachSPB;

    /// <SUMMARY>Set StartSPB Len</SUMMARY>
    procedure SetStartSPBAllocLen(Const NewSize: Integer);
    /// <SUMMARY>Add Value to StartParams</SUMMARY>
    procedure StartSPB(Const param: Byte);
    /// <SUMMARY>Add Value to StartParams</SUMMARY>
    procedure AddParam2StartSPB(Const param: Byte); overload;
    /// <SUMMARY>Add Value to StartParams</SUMMARY>
    procedure AddParam2StartSPB(Const param: Byte;Const Value: Integer); overload;
    /// <SUMMARY>Add Value to StartParams</SUMMARY>
    procedure AddParam2StartSPB(Const param: Byte;Const Value: String); overload;
    /// <SUMMARY>Add Value to StartParams</SUMMARY>
    procedure AddParam2StartSPB(Const param: Byte;Const Value: Ansistring); overload;
    /// <SUMMARY>Add BackupOptions to StartParams</SUMMARY>
    procedure AddBackupOptions2StartSPB(Const BackupOptions:TFXSvcBackupOptions);
    /// <SUMMARY>Add RestoreOptions to StartParams</SUMMARY>
    procedure AddRestoreOptions2StartSPB(Const RestoreOptions:TFXSvcRestoreOptions);
    /// <SUMMARY>Internal Service Start</SUMMARY>
    procedure InternalServiceStart;
    /// <SUMMARY>Internal Service Query</SUMMARY>
    procedure InternalServiceQuery;

    /// <SUMMARY>AllocOutputBuffer</SUMMARY>
    procedure AllocOutputBuffer(Const Size:Integer);
    /// <SUMMARY>ParseString</SUMMARY>
    function ExtractStrFromOutputBuffer(var RunLen: Integer;Out Value:String):Boolean;
    /// <SUMMARY>Fetch</SUMMARY>
    procedure QueryFetch;
    /// <SUMMARY>FetchMore</SUMMARY>
    function QueryFetchMore:Boolean;
    /// <SUMMARY>ParseString</SUMMARY>
    function QueryFetchString:String;overload;
    /// <SUMMARY>ParseString</SUMMARY>
    function QueryFetchString(Const Prefix:Byte):String;overload;
    /// <SUMMARY>FetchInteger</SUMMARY>
    function QueryFetchInteger(Const IntLen:Byte):Integer;overload;
    /// <SUMMARY>FetchInteger</SUMMARY>
    function QueryFetchInteger(Const Prefix,IntLen:Byte):Integer;overload;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign (Source: TPersistent); override;

    /// <SUMMARY>CheckActive</SUMMARY>
    procedure CheckActive;
    /// <SUMMARY>CheckInactive</SUMMARY>
    procedure CheckInactive;
    /// <SUMMARY>Attach</SUMMARY>
    procedure Attach;
    /// <SUMMARY>Detach</SUMMARY>
    procedure Detach;

    /// <summary>GetNextChunk:</summary>
    function FetchNextChunk:String;
    /// <summary>Fetch NextLine:</summary>
    function FetchNextLine:String;

    /// <SUMMARY>Get Is Service Running</SUMMARY>
    ///  Returns true if the current attachment has a service running
    ///  This can be used before isc_service_start to determine if a service is already running.
    ///  If isc_service_start is called and a service is running, an error is returned
    function IsServiceRunning: Boolean;
    /// <summary>Repair1:Two Phase Recovery</summary>
    procedure Repair1(Const aFullFileName:String);
    /// <summary>Repair2:Mend Ignoring</summary>
    procedure Repair2(Const aFullFileName:String);
    /// <summary>Backup:</summary>
    procedure Backup(Const aFullFileName,BackupFile:String;Const BackupOptions:TFXSvcBackupOptions);
    /// <summary>Restore:</summary>
    procedure Restore(Const BackupFile,NewDBName:String;Const PageSize,PageBuffers:Integer;Const RestoreOptions:TFXSvcRestoreOptions);
    /// <summary>Shutdown</summary>
    procedure Shutdown(Const Options: TFXSvcShutdownMode;Const Wait: Integer);
    /// <summary>Shutdown</summary>
    function TryShutdown(Const Options: TFXSvcShutdownMode;Const Wait: Integer):ISC_STATUS;
    /// <summary>Online</summary>
    procedure Online;
    /// <summary>Online</summary>
    function TryOnline:ISC_STATUS;
    /// <summary>ActivateShadow</summary>
    procedure ActivateShadow;
    /// <summary>SetSweepInterval</summary>
    procedure SetSweepInterval(Const Value: Integer);
    /// <summary>SetDBSqlDialect</summary>
    procedure SetDBSqlDialect(Const Value: Integer);
    /// <summary>SetPageBuffers</summary>
    procedure SetPageBuffers(Const Value: Integer);
    /// <summary>SetReserveSpace</summary>
    procedure SetReserveSpace(Const Value: Boolean);
    /// <summary>SetAsyncMode</summary>
    procedure SetAsyncMode(Const Value: Boolean);
    /// <summary>SetReadOnly</summary>
    procedure SetReadOnly(Const Value: Boolean);
    /// <summary>SetReadOnly</summary>
    function TrySetReadOnly(Const Value: Boolean):ISC_STATUS;

    property ClientLibrary     : IFXClientLib         read fLibrary          write fLibrary;
    property DBAlias           : TFXDBAlias           read fDBAlias;
    property FileName          : TFXDBName            read GetFileName       write SetFileName   stored False;

    property Handle            : TISC_SVC_HANDLE      read FHandle;
    property Active            : Boolean              read GetActive         write SetActive;

    property Eof               : boolean              read fEof;

  published
    property ServerName        : String               read GetServerName     write SetServerName;
    property Params            : TStrings             read FParams           write SetParams;
    property OnAttach          : TNotifyEvent         read FOnAttach         write FOnAttach;

  end;

  TFXSvcServerProperties = class(TFXFirebirdService)
  private
    fOptions      : TFXSvcPropertyOptions;
    FDatabaseInfo : TFXSvcDatabaseInfo;
    fVersionInfo  : TFXSvcVersionInfo;
    fConfigParams : TFXSvcServerConfigParams;
  public
    /// <summary>constructor</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>destructor;</summary>
    destructor Destroy; override;

    /// <summary>Fetch</summary>
    procedure Fetch;
    /// <summary>FetchDatabaseInfo</summary>
    procedure FetchDatabaseInfo;
    /// <summary>FetchConfigParams</summary>
    procedure FetchConfigParams;
    /// <summary>FetchVersionInfo</summary>
    procedure FetchVersionInfo;
    
    property DatabaseInfo: TFXSvcDatabaseInfo       read FDatabaseInfo;
    property VersionInfo : TFXSvcVersionInfo        read fVersionInfo;
    property ConfigParams: TFXSvcServerConfigParams read fConfigParams;
    
  published
    property Options     : TFXSvcPropertyOptions    read fOptions write fOptions;
  end;

  TFXStatisticalService = class(TFXFirebirdService)
  private
    fMode         : TFXSvcStatMode;
    fOptions      : TFXSvcStatOptions;
  public
    /// <summary>FetchStats</summary>
    /// isc_action_svc_db_stats 	Retrieves database statistics 	SYSDBA or database owner
    procedure FetchStats;
  published
    property Mode        : TFXSvcStatMode           read fMode    write fMode;
    property Options     : TFXSvcStatOptions        read fOptions write fOptions;
  end;

  TFXBackupService = class (TFXFirebirdService)
  private
    fOptions       : TFXSvcBackupOptions;
    fBackupFiles   : TStrings;
    FBlockingFactor: Integer;
    /// <summary>SetBackupFiles</summary>
    procedure SetBackupFiles(const Value: TStrings);
  public
    /// <summary>constructor</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>destructor;</summary>
    destructor Destroy; override;

    /// <summary>Backup</summary>
    procedure Backup;

  published
    property BackupFiles   : TStrings            read fBackupFiles    write SetBackupFiles;
    property BlockingFactor: Integer             read FBlockingFactor write FBlockingFactor;
    property Options       : TFXSvcBackupOptions read fOptions        write fOptions;
  end;

  TFXRestoreService = class (TFXFirebirdService)
  private
    fFiles       : TStrings;
    fBackupFiles : TStrings;
    fOptions     : TFXSvcRestoreOptions;
    FPageSize    : Integer;
    FPageBuffers : Integer;
    /// <summary>SetFiles</summary>
    procedure SetFiles(const Value: TStrings);
    /// <summary>SetBackupFiles</summary>
    procedure SetBackupFiles(const Value: TStrings);
  public
    /// <summary>constructor</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>destructor;</summary>
    destructor Destroy; override;

    /// <summary>Restore</summary>
    procedure Restore;

  published
    property Files      : TStrings             read fFiles       write SetFiles;
    property BackupFiles: TStrings             read fBackupFiles write SetBackupFiles;
    property PageSize   : Integer              read FPageSize    write FPageSize default 4096;
    property PageBuffers: Integer              read FPageBuffers write FPageBuffers;
    property Options    : TFXSvcRestoreOptions read fOptions     write fOptions  default [roCreateNewDB];
  end;

  TFXValidationService = class(TFXFirebirdService)
  private
    fOptions      : TFXSvcValidateOptions;
    FGlobalAction : TFXSvcTransactionGlobalAction;
    fLimboTRs     : TFXList;
    /// <SUMMARY>Clear</SUMMARY>
    procedure Clear;
    /// <summary>GetLimboTransactionInfoCount:</summary>
    function GetLimboTransactionInfoCount: integer;
    /// <summary>GetLimboTransactionInfo</summary>
    function GetLimboTransactionInfo(Const Idx: integer):TFXSvcLimboTransactionInfo;
  public
    /// <summary>destructor;</summary>
    destructor Destroy; override;

    procedure FetchLimboTransactionInfo;
    procedure StartFixLimboTransactionErrors;
    procedure Start;

    property LimboTransactionInfoCount: Integer read GetLimboTransactionInfoCount;
    property LimboTransactionInfo[Const Idx: integer]: TFXSvcLimboTransactionInfo read GetLimboTransactionInfo;

  published
    property Options     : TFXSvcValidateOptions         read fOptions      write fOptions;
    property GlobalAction: TFXSvcTransactionGlobalAction read FGlobalAction write FGlobalAction;
  end;

  TFXSecurityService = class(TFXFirebirdService)
  private
    fCurrentUser   : TFXSvcUserInfo;
    fUsers         : TFXList;
    fModifyParams  : TFXSvcSecurityModifyParams;

    /// <summary>GetUserName</summary>
    function GetUserName:String;
    /// <summary>SetUserName</summary>
    procedure SetUserName(Const Value: String);
    /// <summary>GetSQLRole</summary>
    function GetSQLRole:String;
    /// <summary>SetSQLRole</summary>
    procedure SetSQLRole(Const Value: String);
    /// <summary>GetFirstName</summary>
    function GetFirstName:String;
    /// <summary>SetFirstName</summary>
    procedure SetFirstName(Const Value: String);
    /// <summary>GetMiddleName</summary>
    function GetMiddleName:String;
    /// <summary>SetMiddleName</summary>
    procedure SetMiddleName(Const Value: String);
    /// <summary>GetLastName</summary>
    function GetLastName:String;
    /// <summary>SetLastName</summary>
    procedure SetLastName(Const Value: String);
    /// <summary>GetPassword</summary>
    function GetPassword:String;
    /// <summary>SetPassword</summary>
    procedure SetPassword(Const Value: String);
    /// <summary>GetUserId</summary>
    function GetUserId:Integer;
    /// <summary>SetUserId</summary>
    procedure SetUserId(Const Value: Integer);
    /// <summary>GetGroupId</summary>
    function GetGroupId:Integer;
    /// <summary>SetGroupId</summary>
    procedure SetGroupId(Const Value: Integer);

    /// <summary>GetUsersCount</summary>
    function GetUsersCount:Integer;
    /// <summary>GetUserInfo</summary>
    function GetUserInfo(Const Idx: Integer): TFXSvcUserInfo;
    /// <summary>ParseUsersFetched</summary>
    procedure ParseUsersFetched;

  public
    /// <summary>constructor</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>destructor;</summary>
    destructor Destroy; override;

    /// <summary>Clear</summary>
    procedure Clear;
    /// <summary>AddUser</summary>
    procedure AddUser;
    /// <summary>DeleteUser</summary>
    procedure DeleteUser;
    /// <summary>ModifyUser</summary>
    procedure ModifyUser;

    /// <summary>FetchUsers</summary>
    procedure FetchUsers;
    /// <summary>FetchUser</summary>
    procedure FetchUser(Const aUserName:string);

    property  User[Const Idx: Integer]: TFXSvcUserInfo read GetUserInfo;
    property  UsersCount: Integer read GetUsersCount;

    property SQlRole    : string  read GetSQLRole    write SetSQLRole;
    property UserName   : string  read GetUserName   write SetUserName;
    property FirstName  : string  read GetFirstName  write SetFirstName;
    property MiddleName : string  read GetMiddleName write SetMiddleName;
    property LastName   : string  read GetLastName   write SetLastName;
    property UserID     : Integer read GetUserID     write SetUserID;
    property GroupID    : Integer read GetGroupID    write SetGroupID;
    property Password   : string  read GetPassword   write SetPassword;

  end;


implementation

uses SysUtils, StrUtils, RTLConsts,
  mFX.Consts, mFX.Utils;

const
  _DefaultBufferSize_ = 10;//32000;

  _SPBPrefix_ = 'isc_spb_';

  _SPBConstantNames_ : array[TFXSvcParam] of String = (
    'user_name',
    'sys_user_name',
    'sys_user_name_enc',
    'password',
    'password_enc',
    'command_line',
    'db_name',
    'verbose',
    'options',
    'connect_timeout',
    'dummy_packet_interval',
    'sql_role_name'
    );

  _SPBConstantValues_ : array[TFXSvcParam] of Byte = (
    isc_spb_user_name,
    isc_spb_sys_user_name,
    isc_spb_sys_user_name_enc,
    isc_spb_password,
    isc_spb_password_enc,
    isc_spb_command_line,
    isc_spb_dbname,
    isc_spb_verbose,
    isc_spb_options,
    isc_spb_connect_timeout,
    isc_spb_dummy_packet_interval,
    isc_spb_sql_role_name
  );

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSvcVersionInfo.Clear;
Begin
  fServerVersion       :='Unknown';
  fServerImplementation:='Unknown';
  fServiceVersion      := 0;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
destructor TFXSvcDatabaseInfo.Destroy;
begin
  fDbNames.Free;
  inherited
end;
{______________________________________________________________________________}
procedure TFXSvcDatabaseInfo.Clear;
Begin
  fNoOfAttachments     := 0;
  fNoOfDatabases       := 0;
  if fDbNames=nil then
    fDbNames:=TStringList.Create else
    fDbNames.Clear;
End;
{______________________________________________________________________________}
function TFXSvcDatabaseInfo.GetDbName(Const Idx:Integer):String;
Begin
  if fDbNames=nil then
    TList.Error(@SListIndexError,Idx);
  Result:=fDbNames[Idx]
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSvcServerConfigParams.Clear;
Begin
 fBaseLocation            :='?';
 fLockFileLocation        :='?';
 fMessageFileLocation     :='?';
 fSecurityDatabaseLocation:='?';
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSvcUserInfo.Clear;
begin
  fUserName     := '';
  FFirstName    := '';
  FMiddleName   := '';
  FLastName     := '';
  FGroupID      := 0;
  FUserID       := 0;

  FPassword     := '';
  fSQLRole      := '';
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXFirebirdService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDBAlias:=TFXDBAlias.Create;
  FParams := TStringList.Create;
  TStringList(FParams).OnChange := ParamsChange;
  TStringList(FParams).OnChanging := ParamsChanging;
  if AOwner is TFXCustomDatabase then
    Self.Assign(AOwner);
  fParamsChanged := True;
end;
{______________________________________________________________________________}
destructor TFXFirebirdService.Destroy;
begin
  if FHandle <> nil then Begin
    fLibrary.Call_service_detach(@FHandle);
    FHandle := nil;
    end;

  if fStartSPBAllocLen>0 then
    FXFree(fStartSPB);

  if fOutputBufferAllocLen>0 Then
    FXFree(fOutputBuffer);

  if fAttachSPBAllocLen>0 then
    FXFree(fAttachSPB);

  FParams.Free;
  fDBAlias.Free;

  fLibrary:=nil;

  inherited Destroy;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.Assign(Source: TPersistent);
Var db:TFXCustomDatabase;
begin
  if Source is TFXCustomDatabase then begin
    Self.CheckInactive;
    DB:=TFXCustomDatabase(Source);
    if DB.ClientLibrary<>nil then Begin
      fLibrary:=DB.ClientLibrary;
      end;
    Self.fDBAlias.Assign(DB.DBAlias);
    With Params do Begin
      Clear;
      Values['user_name']:=DB.Params.Values['user_name'];
      Values['password' ]:=DB.Params.Values['password' ];
      end;
  end else
    inherited
end;
{______________________________________________________________________________}
function TFXFirebirdService.GetServerName:String;
Begin
  Assert(fDBAlias<>nil);
  Result:=fDBAlias.ServerName
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.SetServerName(const Value: string);
Begin
  If not fDBAlias.IsSameServer(Value) Then Begin
    CheckInactive;
    fDBAlias.ServerName:=Value
    end;
end;
{______________________________________________________________________________}
function TFXFirebirdService.GetFileName:TFXDBName;
Begin
  Result:=fDBAlias.FullFileName
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.SetFileName(const Value: TFXDBName);
begin
  If not fDBAlias.IsSameFileName(Value) Then Begin
    CheckInactive;
    fDBAlias.ChgFileName(Value)
    end;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.ParamsChange(Sender: TObject);
begin
  fParamsChanged := True;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.ParamsChanging(Sender: TObject);
begin
  CheckInactive;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.CheckClientLibrary;
Begin
  if fLibrary=nil Then
    FXRaiseClientErrorMsg(Self,fxceFirebirdMissing,'CheckClientLibrary');
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.CheckInactive;
begin
  if FHandle <> nil then
    FXRaiseClientError(Self,fxceServiceInActive);
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.CheckActive;
begin
  if FHandle = nil then
    FXRaiseClientError(Self,fxceServiceActive);
end;
{______________________________________________________________________________}
function TFXFirebirdService.GetActive: Boolean;
begin
  result := FHandle <> nil;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.SetActive(const Value: Boolean);
begin
  Assert(not (csReading in ComponentState));
  if Value <> Active then Begin
    if Value then
      Attach else
      Detach
    end
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.Detach;
Var detach_err:ISC_STATUS;
begin
  CheckActive;
  fOutputString:='';
  if fOutputBufferAllocLen>0 Then Begin
    fOutputBufferAllocLen:=0;
    FXFree(fOutputBuffer);
    End;
  if fStartSPBAllocLen>0 then Begin
    fStartSPBAllocLen:=0;
    FXFree(fStartSPB);
    end;
  detach_err:=fLibrary.Call_service_detach(@FHandle);
  if detach_err > 0 then begin
    FHandle := nil;
    fLibrary.ReadAndRaiseFirebirdError(Self);
  end else
    FHandle := nil;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.Attach;
var ConnectString: AnsiString;
    attach_err:ISC_STATUS;
begin
  CheckInactive;
  CheckClientLibrary;

  // Generate a new SPB if necessary 
  if fParamsChanged then
    GenerateAttachSPB;

  case fDBAlias.Protocol of
    fxLocal:ConnectString := 'service_mgr';
    else ConnectString := AnsiString(fDBAlias.ServerName) + ':service_mgr';
    end;

  attach_err:=fLibrary.Call_service_attach(Length(ConnectString), PAnsiChar(ConnectString), @fHandle, fAttachSPBLen, Pointer(fAttachSPB));
  if attach_err > 0 then Begin
    Assert(fHandle=nil);
    fLibrary.ReadAndRaiseFirebirdError(Self);
    End;

  if Assigned(FOnAttach) then
    FOnAttach(Self);
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.SetAttachSPBLen(Const NewSize: Integer);
Begin
  if NewSize>fAttachSPBAllocLen then Begin
    if fAttachSPBAllocLen=0 then
      FXAlloc(fAttachSPB,NewSize,False) else
      FXReAlloc(fAttachSPB,fAttachSPBAllocLen,NewSize,False);
    fAttachSPBAllocLen:=NewSize;
    End;
End;
{______________________________________________________________________________}
procedure TFXFirebirdService.AddParam2AttachSPB(Const param: Byte);
begin
  SetAttachSPBLen(fAttachSPBLen+1);

  fAttachSPBPos^:=Param;
  Inc(fAttachSPBPos);
  Inc(fAttachSPBLen);
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.AddParam2AttachSPB(Const param:Byte;Const Value: String);
var a:AnsiString;
    i,Len:Byte;
begin
  a:=AnsiString(Value);
  Len:=Length(a);

  SetAttachSPBLen(fAttachSPBLen+1+1+Len);

  fAttachSPBPos^:=Param;
  Inc(fAttachSPBPos);
  Inc(fAttachSPBLen);

  fAttachSPBPos^:=Byte(Len);
  Inc(fAttachSPBPos);
  Inc(fAttachSPBLen);

  for i:=1 to Len do Begin
    fAttachSPBPos^:=Byte(a[i]);
    Inc(fAttachSPBPos);
    Inc(fAttachSPBLen);
    end;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.GenerateAttachSPB;
var p,pName,pValue:string;
    c,cParam:TFXSvcParam;
    cFound:Boolean;
    i,j:Integer;
begin
  fAttachSPBLen:=0;
  SetAttachSPBLen(512);
  fParamsChanged := False;
  fAttachSPBPos:=fAttachSPB;
  AddParam2AttachSPB(isc_spb_version);
  AddParam2AttachSPB(isc_spb_current_version);
  //Iterate through the textual service parameters, constructing a SPB on-the-fly
  for i := 0 to Pred(fParams.Count) do begin
    // Get the parameter's name and value from the list,and make sure that the name is all lowercase with no leading 'isc_spb_' prefix
    p:=Trim(fParams[i]);
    if p='' then
      Continue;
    j:=Pos('=',p);
    if j=0 then Begin
      pName:=LowerCase(p);
      pValue:='';
    end else begin
      pName:=Trim(Copy(p,1,Pred(j)));
      pValue:=Trim(Copy(p,Succ(j),MaxInt));
      end;
    if (Pos(_SPBPrefix_,pName) = 1) then
      Delete(pName, 1, Length(_SPBPrefix_));
    // We want to translate the parameter name to some integer value. We do this by scanning through a list of known service parameter names (SPBConstantNames, defined above).
    cParam:=low(c);
    cFound:=False;
    for c :=low(c) to high(c) do begin
      if SameText(pName,_SPBConstantNames_[c]) then begin
        cFound:=True;
        cParam:=c;
        break;
      end end;
    if cFound then Begin
      case cParam of
        spbUserName,
        spbPassword:begin
          AddParam2AttachSPB(_SPBConstantValues_[c],pValue);
          end;
        else begin
          // Just Ignore FXRaiseClientErrorFmt(Self,fxceSPBConstantNotSupported,[pName])
    	  Assert(False);
        end end;
    end else Begin
      // Just Ignore FXRaiseClientErrorFmt(Self,fxceSPBConstantUnknown, [pName]);
	  Assert(False);
    end end;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.SetStartSPBAllocLen(Const NewSize: Integer);
Begin
  if NewSize>fStartSPBAllocLen then Begin
    FXReAlloc(fStartSPB,fStartSPBAllocLen,NewSize,False);
    fStartSPBAllocLen:=NewSize;
    End;
End;
{______________________________________________________________________________}
procedure TFXFirebirdService.StartSPB(Const param: Byte);
Begin
  SetStartSPBAllocLen(128);
  fStartSPBPos:=fStartSPB;
  fStartSPBPos^:=Param;
  Inc(fStartSPBPos);
  fStartSPBLen:=1;
End;
{______________________________________________________________________________}
procedure TFXFirebirdService.AddParam2StartSPB(Const param:Byte;Const Value: String);
var i,Len:FXUShort;
    a:AnsiString;
begin
  a:=AnsiString(Value);
  Len:=Length(a);

  SetStartSPBAllocLen(fStartSPBLen+1+2+Len);

  fStartSPBPos^:=Param;
  Inc(fStartSPBPos);
  Inc(fStartSPBLen);

  fStartSPBPos^:=Byte(PAnsiChar(@Len)[0]);
  Inc(fStartSPBPos);
  Inc(fStartSPBLen);

  fStartSPBPos^:=Byte(PAnsiChar(@Len)[1]);
  Inc(fStartSPBPos);
  Inc(fStartSPBLen);

  for i:=1 to Len do Begin
    fStartSPBPos^:=Byte(a[i]);
    Inc(fStartSPBPos);
    Inc(fStartSPBLen);
    end;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.AddParam2StartSPB(Const param:Byte;Const Value: AnsiString);
var i,Len:FXUShort;
begin
  Len:=Length(Value);

  SetStartSPBAllocLen(fStartSPBLen+1+2+Len);

  fStartSPBPos^:=Param;
  Inc(fStartSPBPos);
  Inc(fStartSPBLen);

  fStartSPBPos^:=Byte(PAnsiChar(@Len)[0]);
  Inc(fStartSPBPos);
  Inc(fStartSPBLen);

  fStartSPBPos^:=Byte(PAnsiChar(@Len)[1]);
  Inc(fStartSPBPos);
  Inc(fStartSPBLen);

  for i:=1 to Len do Begin
    fStartSPBPos^:=Byte(Value[i]);
    Inc(fStartSPBPos);
    Inc(fStartSPBLen);
    end;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.AddParam2StartSPB(Const param: Byte);
begin
  SetStartSPBAllocLen(fStartSPBLen+1);

  fStartSPBPos^:=Param;
  Inc(fStartSPBPos);
  Inc(fStartSPBLen);
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.AddParam2StartSPB(Const param: Byte;Const Value: Integer);
begin
  SetStartSPBAllocLen(fStartSPBLen+5);

  fStartSPBPos^:=Param;
  Inc(fStartSPBPos);
  Inc(fStartSPBLen);

  fStartSPBPos^:=Byte(PAnsiChar(@Value)[0]);
  Inc(fStartSPBPos);
  Inc(fStartSPBLen);

  fStartSPBPos^:=Byte(PAnsiChar(@Value)[1]);
  Inc(fStartSPBPos);
  Inc(fStartSPBLen);

  fStartSPBPos^:=Byte(PAnsiChar(@Value)[2]);
  Inc(fStartSPBPos);
  Inc(fStartSPBLen);

  fStartSPBPos^:=Byte(PAnsiChar(@Value)[3]);
  Inc(fStartSPBPos);
  Inc(fStartSPBLen);
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.AddBackupOptions2StartSPB(Const BackupOptions:TFXSvcBackupOptions);
Var param:Integer;
begin
  param:=0;
  if (boIgnoreChecksums in BackupOptions) then
    param:=param or isc_spb_bkp_ignore_checksums;
  if (boIgnoreLimbo in BackupOptions) then
    param:=param or isc_spb_bkp_ignore_limbo;
  if (boMetadataOnly in BackupOptions) then
    param:=param or isc_spb_bkp_metadata_only;
  if (boNoGarbageCollection in BackupOptions) then
    param:=param or isc_spb_bkp_no_garbage_collect;
  if (boOldMetadataDesc in BackupOptions) then
    param:=param or isc_spb_bkp_old_descriptions;
  if (boNonTransportable in BackupOptions) then
    param:=param or isc_spb_bkp_non_transportable;
  if (boConvertExtTables in BackupOptions) then
    param:=param or isc_spb_bkp_convert;

  if param>0 then
    AddParam2StartSPB(isc_spb_options,param);
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.AddRestoreOptions2StartSPB(Const RestoreOptions:TFXSvcRestoreOptions);
Var param:Integer;
begin
  param:=0;
  if (roDeactivateIndexes in RestoreOptions) then
    param:=param or isc_spb_res_deactivate_idx;
  if (roNoShadow in RestoreOptions) then
    param:=param or isc_spb_res_no_shadow;
  if (roNoValidityCheck in RestoreOptions) then
    param:=param or isc_spb_res_no_validity;
  if (roOneRelationAtATime in RestoreOptions) then
    param:=param or isc_spb_res_one_at_a_time;
  if (roReplace in RestoreOptions) then
    param:=param or isc_spb_res_replace;
  if (roCreateNewDB in RestoreOptions) then
    param:=param or isc_spb_res_create;
  if (roUseAllSpace in RestoreOptions) then
    param:=param or isc_spb_res_use_all_space;

  if param>0 then
    AddParam2StartSPB(isc_spb_options,param);
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.AllocOutputBuffer(Const Size:Integer);
Begin
  if (fOutputBufferAllocLen=0) then Begin
    fOutputBufferAllocLen:= Size;
    FXAlloc(fOutputBuffer,fOutputBufferAllocLen,False);
  end else
  if (fOutputBufferAllocLen<Size) then Begin
    FXReAlloc(fOutputBuffer,fOutputBufferAllocLen,Size,False);
    fOutputBufferAllocLen:=Size;
    end
End;
{______________________________________________________________________________}
procedure TFXFirebirdService.InternalServiceQuery;
Var query_err:ISC_STATUS;
begin
  CheckActive;
  CheckClientLibrary;
  fOutputBufferPos:=0;
  AllocOutputBuffer(_DefaultBufferSize_);

  query_err:=fLibrary.Call_service_query(@FHandle,nil,0,nil,fStartSPBLen,fStartSPB,fOutputBufferAllocLen,fOutputBuffer);
  if query_err > 0 then
    fLibrary.ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.InternalServiceStart;
Var start_err:ISC_STATUS;
begin
  fEof:=False;
  CheckActive;
  start_err:=fLibrary.Call_service_start(@FHandle, nil,fStartSPBLen,Pointer(fStartSPB));
  if start_err>0 then
    fLibrary.ReadAndRaiseFirebirdError(Self);
end;
{______________________________________________________________________________}
function TFXFirebirdService.FetchNextChunk: String;
var pb:PFXByte;
    Len,l:Integer;
Begin
  Len:=0;
  AllocOutputBuffer(1024);
  SetLength(fOutputString,0);
  StartSPB(isc_info_svc_to_eof);
  repeat QueryFetch;
         if (fOutputBuffer^=isc_info_svc_to_eof) then Begin
           l:=mFX.Header.vax_integer(fOutputBuffer,fOutputBufferPos,2);
           Inc(fOutputBufferPos,2);
           if (l=0) then Begin
             FEof:=True;
             Break;
             end;
           SetLength(fOutputString,Len+l);
           pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
           Move(pb^,fOutputString[Len+1],l);
           Inc(fOutputBufferPos,l);
           Inc(Len,l);
           Inc(pb,l);
           case pb^ of
             isc_info_truncated:Begin
               //Continue;
               end;
             isc_info_end:Begin
               Break;
               end;
             else Begin
               FXRaiseClientError(Self,fxceOutputParsingError);
             end end;
         end else
           FEof:=True;
  until fOutputBuffer^<>isc_info_svc_to_eof;
  Result:=String(fOutputString);
end;
{______________________________________________________________________________}
function TFXFirebirdService.FetchNextLine: String;
var pb:PFXByte;
    Len,l:Integer;
Begin
  Len:=0;
  AllocOutputBuffer(256);
  SetLength(fOutputString,0);
  StartSPB(isc_info_svc_line);
  repeat QueryFetch;
         if (fOutputBuffer^=isc_info_svc_line) then Begin
           l:=mFX.Header.vax_integer(fOutputBuffer,fOutputBufferPos,2);
           Inc(fOutputBufferPos,2);
           if (l=0) then Begin
             FEof:=True;
             Break;
             end;
           SetLength(fOutputString,Len+l);
           pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
           Move(pb^,fOutputString[Len+1],l);
           Inc(fOutputBufferPos,l);
           Inc(Len,l);
           Inc(pb,l);
           case pb^ of
             isc_info_truncated:Begin
               //Continue;
               end;
             isc_info_end:Begin
               Break;
               end;
             else Begin
               FXRaiseClientError(Self,fxceOutputParsingError);
             end end;
         end else
           FEof:=True;
  until fOutputBuffer^<>isc_info_svc_line;
  Result:=String(fOutputString);
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.QueryFetch;
Var query_err:ISC_STATUS;
Begin
  fOutputBufferPos:=0;
  AllocOutputBuffer(_DefaultBufferSize_);
  query_err:=fLibrary.Call_service_query(@FHandle,nil,0,nil,fStartSPBLen,Pointer(fStartSPB),fOutputBufferAllocLen,fOutputBuffer);
  if query_err > 0 then
    fLibrary.ReadAndRaiseFirebirdError(Self);
  case fOutputBuffer^ of
    isc_info_svc_get_users:Begin
      //Don't have any use for the combined length so increment past by 2
      Inc(fOutputBufferPos,3);
      end;
    isc_info_svc_line:Begin
      Inc(fOutputBufferPos);
      end;
    isc_info_svc_to_eof:Begin
      Inc(fOutputBufferPos);
      end;
    else Begin
      FXRaiseClientError(Self,fxceOutputParsingError);
    end end;
End;
{______________________________________________________________________________}
function TFXFirebirdService.QueryFetchMore:Boolean;
var pb:PFXByte;
Begin
  if fOutputBufferPos>=fOutputBufferAllocLen-2 then Begin
    pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
    Case pb^ of
      isc_info_end:Begin
        Self.InternalServiceQuery;
        pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
        Case pb^ of
          isc_info_svc_get_users:Begin
            //Don't have any use for the combined length so increment past by 2
            if fStartSPB^<>isc_info_svc_get_users then
              FXRaiseClientError(Self,fxceOutputParsingError);
            Inc(fOutputBufferPos,3);
            Result:=True;
            end;
          else Begin
            FXRaiseClientError(Self,fxceOutputParsingError);
            Result:=False;
        end end end;
      else Begin
        Result:=False;
      end end;
  end else
    Result:=False;
End;
{______________________________________________________________________________}
function TFXFirebirdService.QueryFetchString:String;
var pb:PFXByte;
    Len,i:Integer;
begin
  Result := '';
  Len:=QueryFetchInteger(2);
  if (Len>0) then begin
    SetLength(fOutputString,Len);
    if (Len<fOutputBufferAllocLen-fOutputBufferPos) then Begin
      pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
      Move(pb^,fOutputString[1],Len);
      Inc(fOutputBufferPos,Len);
    end else Begin
      i:=1;
      While i<=Len do Begin
        Self.QueryFetchMore;
        pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
        fOutputString[i]:=AnsiChar(pb^);
        Inc(fOutputBufferPos);
        Inc(i);
      end end;
    Result:=String(fOutputString);
    end
end;
{______________________________________________________________________________}
function TFXFirebirdService.QueryFetchString(Const Prefix:Byte):String;
var pb:PFXByte;
    Len,i:Integer;
begin
  Result := '';
  Self.QueryFetchMore;
  pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
  if pb^<>Prefix then
    FXRaiseClientError(Self,fxceOutputParsingError);
  Inc(fOutputBufferPos);
  Len:=QueryFetchInteger(2);
  if (Len>0) then begin
    SetLength(fOutputString,Len);
    if (Len<fOutputBufferAllocLen-fOutputBufferPos) then Begin
      pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
      Move(pb^,fOutputString[1],Len);
      Inc(fOutputBufferPos,Len);
    end else Begin
      i:=1;
      While i<=Len do Begin
        Self.QueryFetchMore;
        pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
        fOutputString[i]:=AnsiChar(pb^);
        Inc(fOutputBufferPos);
        Inc(i);
      end end;
    Result:=String(fOutputString);
    end
end;
{______________________________________________________________________________}
function TFXFirebirdService.QueryFetchInteger(Const IntLen:Byte):Integer;
var pb:PFXByte;
    Shift:Integer;
    i:Byte;
begin
	shift:=0;
	Result:=0;
  for i:=1 to IntLen do Begin
    Self.QueryFetchMore;
    pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
		Result:=Result+(pb^ shl shift);
    Inc(fOutputBufferPos);
		Inc(shift,8);
    end
end;
{______________________________________________________________________________}
function TFXFirebirdService.QueryFetchInteger(Const Prefix,IntLen:Byte):Integer;
var pb:PFXByte;
    Shift:Integer;
    i:Byte;
begin
  Self.QueryFetchMore;
  pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
  if pb^<>Prefix then
    FXRaiseClientError(Self,fxceOutputParsingError);
  Inc(fOutputBufferPos);

	shift:=0;
	Result:=0;
  for i:=1 to IntLen do Begin
    Self.QueryFetchMore;
    pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
		Result:=Result+(pb^ shl shift);
    Inc(fOutputBufferPos);
		Inc(shift,8);
    end
end;
{______________________________________________________________________________}
function TFXFirebirdService.ExtractStrFromOutputBuffer(var RunLen: Integer;Out Value:String):Boolean;
var pb:PFXByte;
    tmp:AnsiString;
    Len:FXUShort;
    i:Integer;
begin
  Len:=mFX.Header.vax_integer(fOutputBuffer,RunLen,2);
  Inc(RunLen,2);
  if (Len>0) then begin
    if (Len<fOutputBufferAllocLen-RunLen) then Begin
      SetLength(tmp,Len);
      pb:=fOutputBuffer;Inc(pb,RunLen);
      Move(pb^,tmp[1],Len);
      Value:=String(tmp);
      Inc(RunLen,Len);
      Result:=True;
    end else Begin
      Len:=fOutputBufferAllocLen-RunLen;
      SetLength(tmp,Len);
      for i:=1 to Len do Begin
        pb:=fOutputBuffer;Inc(pb,RunLen);
        case pb^ of
          isc_info_end:Begin
            SetLength(tmp,Pred(i));
            Break;
            end;
          else Begin
            tmp[i]:=AnsiChar(pb^);
            Inc(RunLen);
        end end end;
      Value:=String(tmp);
      Result:=False;
      end;
  end else Begin
    Result:=True;
    Value := '';
    end;
end;
{______________________________________________________________________________}
function TFXFirebirdService.IsServiceRunning:Boolean;
var i: FXLong;
begin
  CheckActive;
  StartSPB(isc_info_svc_running);
  Self.InternalServiceQuery;
  if (fOutputBuffer^ <> isc_info_svc_running) then
    FXRaiseClientError(Self,fxceOutputParsingError);
  i:=mFX.Header.vax_integer(fOutputBuffer,1,4);
  result:=(i=1)
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.Shutdown(Const Options: TFXSvcShutdownMode;Const Wait: Integer);
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_properties);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  Case Options of
    smForced         :AddParam2StartSPB(isc_spb_prp_shutdown_db          ,Wait);
    smDenyTransaction:AddParam2StartSPB(isc_spb_prp_deny_new_transactions,Wait);
    else              AddParam2StartSPB(isc_spb_prp_deny_new_attachments ,Wait);
    end;
  InternalServiceStart;
end;
{______________________________________________________________________________}
function TFXFirebirdService.TryShutdown(Const Options: TFXSvcShutdownMode;Const Wait: Integer):ISC_STATUS;
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_properties);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  Case Options of
    smForced         :AddParam2StartSPB(isc_spb_prp_shutdown_db          ,Wait);
    smDenyTransaction:AddParam2StartSPB(isc_spb_prp_deny_new_transactions,Wait);
    else              AddParam2StartSPB(isc_spb_prp_deny_new_attachments ,Wait);
    end;
  result:=fLibrary.Call_service_start(@FHandle, nil,fStartSPBLen,Pointer(fStartSPB));
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.Online;
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_properties);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  AddParam2StartSPB(isc_spb_options,isc_spb_prp_db_online);
  InternalServiceStart;
end;
{______________________________________________________________________________}
function TFXFirebirdService.TryOnline:ISC_STATUS;
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_properties);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  AddParam2StartSPB(isc_spb_options,isc_spb_prp_db_online);
  result:=fLibrary.Call_service_start(@FHandle, nil,fStartSPBLen,Pointer(fStartSPB));
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.ActivateShadow;
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_properties);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  AddParam2StartSPB(isc_spb_options,isc_spb_prp_activate);
  InternalServiceStart;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.SetAsyncMode(Const Value: Boolean);
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_properties);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  AddParam2StartSPB(isc_spb_prp_write_mode);
  if Value then
    AddParam2StartSPB(isc_spb_prp_wm_async) else
    AddParam2StartSPB(isc_spb_prp_wm_sync);
  InternalServiceStart;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.SetPageBuffers(Const Value: Integer);
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_properties);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  AddParam2StartSPB(isc_spb_prp_page_buffers,Value);
  InternalServiceStart;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.SetReadOnly(Const Value: Boolean);
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_properties);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  AddParam2StartSPB(isc_spb_prp_access_mode);
  if Value then
    AddParam2StartSPB(isc_spb_prp_am_readonly) else
    AddParam2StartSPB(isc_spb_prp_am_readwrite);
  InternalServiceStart;
end;
{______________________________________________________________________________}
function TFXFirebirdService.TrySetReadOnly(Const Value: Boolean):ISC_STATUS;
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_properties);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  AddParam2StartSPB(isc_spb_prp_access_mode);
  if Value then
    AddParam2StartSPB(isc_spb_prp_am_readonly) else
    AddParam2StartSPB(isc_spb_prp_am_readwrite);
  result:=fLibrary.Call_service_start(@FHandle, nil,fStartSPBLen,Pointer(fStartSPB));
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.SetReserveSpace(Const Value: Boolean);
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_properties);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  AddParam2StartSPB(isc_spb_prp_reserve_space);
  if Value then
    AddParam2StartSPB(isc_spb_prp_res) else
    AddParam2StartSPB(isc_spb_prp_res_use_full);
  InternalServiceStart;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.SetSweepInterval(Const Value: Integer);
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_properties);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  AddParam2StartSPB(isc_spb_prp_sweep_interval,Value);
  InternalServiceStart;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.SetDBSqlDialect(Const Value: Integer);
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_properties);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  AddParam2StartSPB(isc_spb_prp_set_sql_dialect,Value);
  InternalServiceStart;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.Repair1(Const aFullFileName:String);
Begin
  CheckActive;
  StartSPB(isc_action_svc_repair);
  AddParam2StartSPB(isc_spb_dbname,aFullFileName);
  AddParam2StartSPB(isc_spb_options,isc_spb_rpr_recover_two_phase or isc_spb_rpr_full);
  InternalServiceStart;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.Repair2(Const aFullFileName:String);
Begin
  CheckActive;
  StartSPB(isc_action_svc_repair);
  AddParam2StartSPB(isc_spb_dbname,aFullFileName);
  AddParam2StartSPB(isc_spb_options,isc_spb_rpr_mend_db or isc_spb_rpr_ignore_checksum);
  InternalServiceStart;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.Backup(Const aFullFileName,BackupFile:String;Const BackupOptions:TFXSvcBackupOptions);
Begin
  CheckActive;
  StartSPB(isc_action_svc_backup);
  AddParam2StartSPB(isc_spb_dbname,aFullFileName);
  AddParam2StartSPB(isc_spb_bkp_file,BackupFile);
  AddBackupOptions2StartSPB(BackupOptions);
  if boVerbose in BackupOptions then
    AddParam2StartSPB(isc_spb_verbose);
  InternalServiceStart;
end;
{______________________________________________________________________________}
procedure TFXFirebirdService.Restore(Const BackupFile,NewDBName:String;Const PageSize,PageBuffers:Integer;Const RestoreOptions:TFXSvcRestoreOptions);
Begin
  CheckActive;
  StartSPB(isc_action_svc_restore);
  AddRestoreOptions2StartSPB(RestoreOptions);
  if PageSize>0 Then
    AddParam2StartSPB(isc_spb_res_page_size,PageSize);
  if PageBuffers>0 then
    AddParam2StartSPB(isc_spb_res_buffers,PageBuffers);
  if roVerbose in RestoreOptions then
    AddParam2StartSPB(isc_spb_verbose);
  AddParam2StartSPB(isc_spb_bkp_file,BackupFile);
  AddParam2StartSPB(isc_spb_dbname,NewDBName);
  InternalServiceStart;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXSvcServerProperties.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatabaseInfo := TFXSvcDatabaseInfo.Create;
  fVersionInfo  := TFXSvcVersionInfo.Create;
  fConfigParams := TFXSvcServerConfigParams.Create;
end;
{______________________________________________________________________________}
destructor TFXSvcServerProperties.Destroy;
begin
  fConfigParams.Free;
  fVersionInfo.Free;
  FDatabaseInfo.Free;
  inherited Destroy;
end;
{______________________________________________________________________________}
procedure TFXSvcServerProperties.Fetch;
begin
  if (spVersion in Options) then
    FetchVersionInfo;
  if (spDatabase in Options) then
    FetchDatabaseInfo;
  if (spConfigParameters in Options) then
    FetchConfigParams;
end;
{______________________________________________________________________________}
procedure TFXSvcServerProperties.FetchVersionInfo;
var pb:PFXByte;
    RunLen:Integer;
begin
  fVersionInfo.Clear;
  AllocOutputBuffer(100);
  StartSPB(isc_info_svc_version);
  AddParam2StartSPB(isc_info_svc_server_version);
  AddParam2StartSPB(isc_info_svc_implementation);
  Self.InternalServiceQuery;

  RunLen := 0;
  While RunLen < fOutputBufferAllocLen do begin
    pb:=fOutputBuffer;Inc(pb,RunLen);
    case pb^ of
      isc_info_svc_version:Begin
        Inc(RunLen);
        fVersionInfo.fServiceVersion := mFX.Header.vax_integer(fOutputBuffer,RunLen,4);
        Inc(RunLen,4);
        end;
      isc_info_svc_server_version:Begin
        Inc(RunLen);
        if not ExtractStrFromOutputBuffer(RunLen,fVersionInfo.fServerVersion) then Begin
          fOutputBuffer^:=isc_info_truncated;
          RunLen:=0;
        end end;
      isc_info_svc_implementation:Begin
        Inc(RunLen);
        if not ExtractStrFromOutputBuffer(RunLen,fVersionInfo.fServerImplementation) then Begin
          fOutputBuffer^:=isc_info_truncated;
          RunLen:=0;
        end end;
      isc_info_truncated:Begin
        fVersionInfo.Clear;
        AllocOutputBuffer(fOutputBufferAllocLen+_DefaultBufferSize_);
        Self.InternalServiceQuery;
        RunLen := 0;
        end;
      isc_info_end:Begin
        break;
        end;
      else Begin
        FXRaiseClientError(Self,fxceOutputParsingError);
    end end end;
end;
{______________________________________________________________________________}
procedure TFXSvcServerProperties.FetchDatabaseInfo;
var pb:PFXByte;
    RunLen:Integer;
    dbn:String;
begin
  fDatabaseInfo.Clear;
  AllocOutputBuffer(40);
  StartSPB(isc_info_svc_svr_db_info);
  Self.InternalServiceQuery;

  RunLen := 0;
  While RunLen < fOutputBufferAllocLen do begin
    pb:=fOutputBuffer;Inc(pb,RunLen);
    case pb^ of
      isc_info_svc_svr_db_info:Begin
        Inc(RunLen);
        end;
      isc_spb_num_att:Begin
        Inc(RunLen);
        fDatabaseInfo.fNoOfAttachments := mFX.Header.vax_integer(fOutputBuffer,RunLen,4);
        Inc(RunLen,4);
        end;
      isc_spb_num_db:Begin
        Inc(RunLen);
        fDatabaseInfo.fNoOfDatabases := mFX.Header.vax_integer(fOutputBuffer,RunLen,4);
        Inc(RunLen,4);
        end;
      isc_spb_dbname:Begin
        Inc(RunLen);
        if not ExtractStrFromOutputBuffer(RunLen,dbn) then Begin
          fOutputBuffer^:=isc_info_truncated;
          RunLen:=0;
        end else
          fDatabaseInfo.fDbNames.Add(dbn);
        end;
      isc_info_truncated:Begin
        fDatabaseInfo.Clear;
        AllocOutputBuffer(fOutputBufferAllocLen+_DefaultBufferSize_);
        Self.InternalServiceQuery;
        RunLen := 0;
        end;
      isc_info_flag_end:Begin
        break;
        end;
      else Begin
        FXRaiseClientError(Self,fxceOutputParsingError);
    end end end;
end;
{______________________________________________________________________________}
procedure TFXSvcServerProperties.FetchConfigParams;
var pb:PFXByte;
    RunLen:Integer;
begin
  fConfigParams.Clear;
  StartSPB(isc_info_svc_get_env);
  AddParam2StartSPB(isc_info_svc_get_env_lock);
  AddParam2StartSPB(isc_info_svc_get_env_msg);
  AddParam2StartSPB(isc_info_svc_user_dbpath);
  Self.InternalServiceQuery;

  RunLen := 0;
  While RunLen < fOutputBufferAllocLen do begin
    pb:=fOutputBuffer;Inc(pb,RunLen);
    case pb^ of
      isc_info_svc_get_env:begin
        Inc (RunLen);
        if not ExtractStrFromOutputBuffer(RunLen,fConfigParams.fBaseLocation) then Begin
          fOutputBuffer^:=isc_info_truncated;
          RunLen:=0;
        end end;
      isc_info_svc_get_env_lock:begin
        Inc (RunLen);
        if not ExtractStrFromOutputBuffer(RunLen,fConfigParams.fLockFileLocation) then Begin
          fOutputBuffer^:=isc_info_truncated;
          RunLen:=0;
        end end;
      isc_info_svc_get_env_msg:begin
        Inc (RunLen);
        if not ExtractStrFromOutputBuffer(RunLen,fConfigParams.fMessageFileLocation) then Begin
          fOutputBuffer^:=isc_info_truncated;
          RunLen:=0;
        end end;
      isc_info_svc_user_dbpath:begin
        Inc (RunLen);
        if not ExtractStrFromOutputBuffer(RunLen,fConfigParams.fSecurityDatabaseLocation) then Begin
          fOutputBuffer^:=isc_info_truncated;
          RunLen:=0;
        end end;
      isc_info_truncated:Begin
        fConfigParams.Clear;
        AllocOutputBuffer(fOutputBufferAllocLen+_DefaultBufferSize_);
        Self.InternalServiceQuery;
        RunLen := 0;
        end;
      isc_info_end:Begin
        break;
        end;
      else Begin
        FXRaiseClientError(Self,fxceOutputParsingError);
    end end end;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXStatisticalService.FetchStats;
var param: Integer;
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_db_stats);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);

  param := 0;
  Case fMode of
    soHeader:Begin
      param := param or isc_spb_sts_hdr_pages;
      end;
    else Begin
      if (soDataPages in fOptions) then
        param := param or isc_spb_sts_data_pages;
      if (soIndexPages in fOptions) then
        param := param or isc_spb_sts_idx_pages;
      if (soSystemRelations in fOptions) then
        param := param or isc_spb_sts_sys_relations;
    end end;
  if param>0 then
    AddParam2StartSPB(isc_spb_options,param);

  InternalServiceStart;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXBackupService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBackupFiles := TStringList.Create;
end;
{______________________________________________________________________________}
destructor TFXBackupService.Destroy;
begin
  fBackupFiles.Free;
  inherited Destroy;
end;
{______________________________________________________________________________}
procedure TFXBackupService.SetBackupFiles(const Value: TStrings);
begin
  // a name=value pair of filename and length
  fBackupFiles.Assign(Value);
end;
{______________________________________________________________________________}
procedure TFXBackupService.Backup;
var param,i,p:Integer;
    desti:String;
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_backup);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);

  param := 0;
  if (boIgnoreChecksums in Options) then
    param := param or isc_spb_bkp_ignore_checksums;
  if (boIgnoreLimbo in Options) then
    param := param or isc_spb_bkp_ignore_limbo;
  if (boMetadataOnly in Options) then
    param := param or isc_spb_bkp_metadata_only;
  if (boNoGarbageCollection in Options) then
    param := param or isc_spb_bkp_no_garbage_collect;
  if (boOldMetadataDesc in Options) then
    param := param or isc_spb_bkp_old_descriptions;
  if (boNonTransportable in Options) then
    param := param or isc_spb_bkp_non_transportable;
  if (boConvertExtTables in Options) then
    param := param or isc_spb_bkp_convert;
  if param>0 then
    AddParam2StartSPB(isc_spb_options,param);

  for i := 0 to Pred(fBackupFiles.Count) do begin
    desti:=Trim(fBackupFiles[i]);
    desti:=AnsiReplaceStr(desti,'\','/');
    if desti<>'' then Begin
      p:=Pos('=',desti);
      if p>0 then begin
        AddParam2StartSPB(isc_spb_bkp_file,Copy(Desti,1,p-1));
        AddParam2StartSPB(isc_spb_bkp_length,StrToInt(Copy(desti,p+1,MaxInt)));
      end else
        AddParam2StartSPB(isc_spb_bkp_file,desti);
    end end;

  if boVerbose in fOptions then
    AddParam2StartSPB(isc_spb_verbose);

  if FBlockingFactor > 0 then
    AddParam2StartSPB(FBlockingFactor, isc_spb_bkp_factor);

  InternalServiceStart;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXRestoreService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fFiles       := TStringList.Create;
  fBackupFiles := TStringList.Create;
  Include (fOptions, roCreateNewDB);
  FPageSize := 4096;
end;
{______________________________________________________________________________}
destructor TFXRestoreService.Destroy;
begin
  fBackupFiles.Free;
  fFiles.Free;
  inherited Destroy;
end;
{______________________________________________________________________________}
procedure TFXRestoreService.SetBackupFiles(const Value: TStrings);
begin
  // a name=value pair of filename and length
  fBackupFiles.Assign(Value);
end;
{______________________________________________________________________________}
procedure TFXRestoreService.SetFiles(const Value: TStrings);
begin
  // a name=value pair of filename and length
  fFiles.Assign(Value);
end;
{______________________________________________________________________________}
procedure TFXRestoreService.Restore;
var i, p: Integer;
    desti: String;
begin
  CheckActive;

  StartSPB(isc_action_svc_restore);
  AddRestoreOptions2StartSPB(fOptions);
  if roVerbose in fOptions then
    AddParam2StartSPB(isc_spb_verbose);
  if FPageSize > 0 then
    AddParam2StartSPB(isc_spb_res_page_size,FPageSize);
  if FPageBuffers > 0 then
    AddParam2StartSPB(isc_spb_res_buffers,FPageBuffers);

  for i := 0 to Pred(fBackupFiles.Count) do begin
    desti:=Trim(fBackupFiles[i]);
    if (desti<>'') then Begin
      p:=Pos('=',desti);
      if p>0 then begin
        AddParam2StartSPB(isc_spb_bkp_file,Copy(Desti,1,p-1));
        AddParam2StartSPB(isc_spb_bkp_length,StrToInt(Copy(desti,p+1,MaxInt)));
      end else
        AddParam2StartSPB(isc_spb_bkp_file,fBackupFiles[i]);
    end end;

  for i := 0 to Pred(fFiles.Count) do begin
    desti:=Trim(fFiles[i]);
    if (desti<>'') then Begin
      p:=Pos('=',desti);
      if p>0 then begin
        AddParam2StartSPB(isc_spb_dbname,Copy(Desti,1,p-1));
        AddParam2StartSPB(isc_spb_res_length,StrToInt(Copy(desti,p+1,MaxInt)));
      end else
        AddParam2StartSPB(isc_spb_dbname,Desti);
    end end;
    
  InternalServiceStart;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
destructor TFXValidationService.Destroy;
begin
  fLimboTRs.Free;
  inherited Destroy;
end;
{______________________________________________________________________________}
procedure TFXValidationService.Clear;
begin
  if fLimboTRs<>nil then
    fLimboTRs.Clear;
end;
{______________________________________________________________________________}
procedure TFXValidationService.FetchLimboTransactionInfo;
var t:TFXSvcLimboTransactionInfo;
    pb:PFXByte;
    RunLen:Integer;
begin
  Self.Clear;
  CheckActive;

  StartSPB(isc_info_svc_limbo_trans);
  Self.InternalServiceQuery;

  RunLen := 0;
  While RunLen < fOutputBufferAllocLen do begin
    pb:=fOutputBuffer;Inc(pb,RunLen);
    case pb^ of
      isc_info_svc_limbo_trans:Begin
        Inc(RunLen);
        end;
      isc_spb_single_tra_id:Begin
        Inc(RunLen);
        if fLimboTRs=nil then
          fLimboTRs:=TFXList.Create;
        t:=TFXSvcLimboTransactionInfo.Create;
        fLimboTRs.Add(t);
        t.fMultiDatabase := False;
        t.fID:=mFX.Header.vax_integer(fOutputBuffer,RunLen,4);
        Inc(RunLen,4);
        end;
      isc_spb_multi_tra_id:Begin
        Inc(RunLen);
        if fLimboTRs=nil then
          fLimboTRs:=TFXList.Create;
        t:=TFXSvcLimboTransactionInfo.Create;
        fLimboTRs.Add(t);
        t.fMultiDatabase := True;
        t.fID:=mFX.Header.vax_integer(fOutputBuffer,RunLen,4);
        Inc(RunLen,4);
        if not ExtractStrFromOutputBuffer(RunLen,t.fHostSite) then Begin
          fOutputBuffer^:=isc_info_truncated;
          RunLen:=0;
          Continue;
          end;
        pb:=fOutputBuffer;Inc(pb,RunLen);
        if (pb^<> isc_spb_tra_state) then
          FXRaiseClientError(Self,fxceOutputParsingError);
        Inc(RunLen);
        pb:=fOutputBuffer;Inc(pb,RunLen);
        Case pb^ of
          isc_spb_tra_state_limbo   :t.fState := tsLimboState;
          isc_spb_tra_state_commit  :t.fState := tsCommitState;
          isc_spb_tra_state_rollback:t.fState := tsRollbackState;
          else                       t.fState := tsUnknownState;
          end;
        Inc(RunLen);
        if not ExtractStrFromOutputBuffer(RunLen,t.fRemoteSite) then Begin
          fOutputBuffer^:=isc_info_truncated;
          RunLen:=0;
          Continue;
          end;
        if not ExtractStrFromOutputBuffer(RunLen,t.fRemoteDatabasePath) then Begin
          fOutputBuffer^:=isc_info_truncated;
          RunLen:=0;
          Continue;
          end;
        pb:=fOutputBuffer;Inc(pb,RunLen);
        Case pb^ of
          isc_spb_tra_advise_commit:begin
            t.fAdvise := taCommitAdvise;
            t.fAction:= fxTACommit;
            end;
          isc_spb_tra_advise_rollback:begin
            t.fAdvise := taRollbackAdvise;
            t.fAction := fxTARollback;
            end;
          else Begin
            // if no advice commit as default
            t.fAdvise := taUnknownAdvise;
            t.fAction := fxTACommit;
          end end;
        Inc(RunLen);
        end;
      isc_info_truncated:Begin
        Self.Clear;
        AllocOutputBuffer(fOutputBufferAllocLen+_DefaultBufferSize_);
        Self.InternalServiceQuery;
        RunLen := 0;
        end;
      isc_info_end:Begin
        break;
        end;
      else Begin
        FXRaiseClientError(Self,fxceOutputParsingError);
    end end end;
end;
{______________________________________________________________________________}
procedure TFXValidationService.StartFixLimboTransactionErrors;
var t:TFXSvcLimboTransactionInfo;
    i: Integer;
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_repair);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);
  Case FGlobalAction of
    taCommitGlobal:Begin
      for i:=0 to Pred(fLimboTRs.Count) do Begin
        t:=TFXSvcLimboTransactionInfo(fLimboTRs[i]);
        if t.fID <> 0 then
          AddParam2StartSPB(isc_spb_rpr_commit_trans,t.ID);
      end end;
    taRollbackGlobal:Begin
      for i:=0 to Pred(fLimboTRs.Count) do Begin
        t:=TFXSvcLimboTransactionInfo(fLimboTRs[i]);
        if t.fID <> 0 then
          AddParam2StartSPB(isc_spb_rpr_rollback_trans,t.ID);
      end end;
    taRecoverTwoPhaseGlobal:begin
      for i:=0 to Pred(fLimboTRs.Count) do Begin
        t:=TFXSvcLimboTransactionInfo(fLimboTRs[i]);
        if t.fID <> 0 then
          AddParam2StartSPB(isc_spb_rpr_recover_two_phase,t.ID);
      end end;
    else begin
      // taNoGlobalAction
      for i:=0 to Pred(fLimboTRs.Count) do Begin
        t:=TFXSvcLimboTransactionInfo(fLimboTRs[i]);
        if t.fID <> 0 then begin
          Case t.fAction of
            fxTACommit,fxTACommitRetaining:Begin
              AddParam2StartSPB(isc_spb_rpr_commit_trans,t.fID)
              end;
            else Begin
              AddParam2StartSPB(isc_spb_rpr_rollback_trans,t.fID);
    end end end end end end;

  Self.InternalServiceStart;
end;
{______________________________________________________________________________}
procedure TFXValidationService.Start;
var param:Integer;
begin
  CheckActive;
  fDBAlias.CheckAlias;

  StartSPB(isc_action_svc_repair);
  AddParam2StartSPB(isc_spb_dbname,fDBAlias.FullFileName);

  param := 0;
  if (voSweepDB in Options) then
    param := param or isc_spb_rpr_sweep_db;
  if (voValidateDB in Options) then
    param := param or isc_spb_rpr_validate_db;
  if (voLimboTransactions in Options) then
    param := param or isc_spb_rpr_list_limbo_trans;
  if (voCheckDB in Options) then
    param := param or isc_spb_rpr_check_db;
  if (voIgnoreChecksum in Options) then
    param := param or isc_spb_rpr_ignore_checksum;
  if (voKillShadows in Options) then
    param := param or isc_spb_rpr_kill_shadows;
  if (voMendDB in Options) then
    param := param or isc_spb_rpr_mend_db;
  if (voValidateFull in Options) then begin
    param := param or isc_spb_rpr_full;
    if not (voMendDB in Options) then
      param := param or isc_spb_rpr_validate_db;
    end;
  if param > 0 then
    AddParam2StartSPB(isc_spb_options,param);

  Self.InternalServiceStart;
end;
{______________________________________________________________________________}
function TFXValidationService.GetLimboTransactionInfo(Const Idx: integer): TFXSvcLimboTransactionInfo;
begin
  if fLimboTRs=nil then
    TList.Error(@SListIndexError,Idx);
  result := TFXSvcLimboTransactionInfo(fLimboTRs[Idx]);
end;
{______________________________________________________________________________}
function TFXValidationService.GetLimboTransactionInfoCount: integer;
begin
  if fLimboTRs=nil then
    Result := 0 else
    Result := fLimboTRs.Count
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXSecurityService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCurrentUser:=TFXSvcUserInfo.Create;
  fUsers:=TFXList.Create;
  FModifyParams := [];
end;
{______________________________________________________________________________}
destructor TFXSecurityService.Destroy;
begin
  fUsers.Free;
  fCurrentUser.Free;
  inherited Destroy;
end;
{______________________________________________________________________________}
procedure TFXSecurityService.Clear;
Begin
  fModifyParams := [];
  fCurrentUser.Clear;
  fUsers.Clear;
End;
{______________________________________________________________________________}
function TFXSecurityService.GetUserInfo(Const Idx: Integer): TFXSvcUserInfo;
begin
  result := TFXSvcUserInfo(fUsers[Idx])
end;
{______________________________________________________________________________}
function TFXSecurityService.GetUsersCount: Integer;
begin
  Result := fUsers.Count
end;
{______________________________________________________________________________}
procedure TFXSecurityService.FetchUser(Const aUserName:string);
begin
  Clear;
  CheckActive;

  StartSPB(isc_action_svc_display_user);
  AddParam2StartSPB(isc_spb_sec_username,aUserName);
  InternalServiceStart;
  ParseUsersFetched
end;
{______________________________________________________________________________}
procedure TFXSecurityService.FetchUsers;
begin
  Clear;
  CheckActive;

  StartSPB(isc_action_svc_display_user);
  InternalServiceStart;
  ParseUsersFetched
end;
{______________________________________________________________________________}
procedure TFXSecurityService.ParseUsersFetched;
var u: TFXSvcUserInfo;
    pb:PFXByte;
Begin
  Self.Clear;
  AllocOutputBuffer(250);
  StartSPB(isc_info_svc_get_users);

  Self.QueryFetch;
  While fOutputBufferPos < fOutputBufferAllocLen do begin
    pb:=fOutputBuffer;Inc(pb,fOutputBufferPos);
    case pb^ of
      isc_spb_sec_username:Begin
        u:=TFXSvcUserInfo.Create;
        fUsers.Add(u);
        u.fUserName  :=QueryFetchString (isc_spb_sec_username  );
        u.fFirstName :=QueryFetchString (isc_spb_sec_firstname );
        u.fMiddleName:=QueryFetchString (isc_spb_sec_middlename);
        u.fLastName  :=QueryFetchString (isc_spb_sec_lastname  );
        u.fUserId    :=QueryFetchInteger(isc_spb_sec_userId    ,4);
        u.fGroupID   :=QueryFetchInteger(isc_spb_sec_groupid   ,4);
        end;
      isc_info_end:Begin
        if not Self.QueryFetchMore then
          Break;
        end;
      else Begin
        FXRaiseClientError(Self,fxceOutputParsingError);
    end end end;
end;
{______________________________________________________________________________}
function TFXSecurityService.GetUserName:String;
begin
  Result:=fCurrentUser.fUserName
end;
{______________________________________________________________________________}
procedure TFXSecurityService.SetUserName (Const Value: String);
begin
  fCurrentUser.fUserName := Value;
end;
{______________________________________________________________________________}
function TFXSecurityService.GetSQLRole:String;
begin
  Result:=fCurrentUser.fSQLRole
end;
{______________________________________________________________________________}
procedure TFXSecurityService.SetSQLRole (Const Value: String);
begin
  fCurrentUser.fSQLRole := Value;
end;
{______________________________________________________________________________}
function TFXSecurityService.GetFirstName:String;
begin
  Result:=fCurrentUser.FFirstName
end;
{______________________________________________________________________________}
procedure TFXSecurityService.SetFirstName (Const Value: String);
begin
  Include (FModifyParams, smFirstName);
  fCurrentUser.FFirstName := Value;
end;
{______________________________________________________________________________}
function TFXSecurityService.GetMiddleName:String;
begin
  Result:=fCurrentUser.FMiddleName
end;
{______________________________________________________________________________}
procedure TFXSecurityService.SetMiddleName (Const Value: String);
begin
  Include (FModifyParams, smMiddleName);
  fCurrentUser.FMiddleName := Value;
end;
{______________________________________________________________________________}
function TFXSecurityService.GetLastName:String;
begin
  Result:=fCurrentUser.FLastName
end;
{______________________________________________________________________________}
procedure TFXSecurityService.SetLastName (Const Value: String);
begin
  Include (FModifyParams, smLastName);
  fCurrentUser.FLastName := Value;
end;
{______________________________________________________________________________}
function TFXSecurityService.GetPassword:String;
begin
  Result:=fCurrentUser.FPassword
end;
{______________________________________________________________________________}
procedure TFXSecurityService.SetPassword (Const Value: String);
begin
  Include (FModifyParams, smPassword);
  fCurrentUser.FPassword := Value;
end;
{______________________________________________________________________________}
function TFXSecurityService.GetUserId:Integer;
begin
  Result:=fCurrentUser.FUserId
end;
{______________________________________________________________________________}
procedure TFXSecurityService.SetUserId (Const Value: Integer);
begin
  Include (FModifyParams, smUserId);
  fCurrentUser.FUserId := Value;
end;
{______________________________________________________________________________}
function TFXSecurityService.GetGroupId:Integer;
begin
  Result:=fCurrentUser.FGroupId
end;
{______________________________________________________________________________}
procedure TFXSecurityService.SetGroupId (Const Value: Integer);
begin
  Include (FModifyParams, smGroupId);
  fCurrentUser.FGroupId := Value;
end;
{______________________________________________________________________________}
procedure TFXSecurityService.AddUser;
Var Len:Integer;
begin
  CheckActive;
  if ( Pos(' ', fCurrentUser.FUserName) > 0 ) then
    FXRaiseClientError(Self,fxceStartParamsError);
  Len := Length(fCurrentUser.FUserName);
  if (Len = 0) then
    FXRaiseClientError(Self,fxceStartParamsError);

  StartSPB(isc_action_svc_add_user);
  AddParam2StartSPB(isc_spb_sec_username  ,fCurrentUser.fUserName  );
  AddParam2StartSPB(isc_spb_sec_userid    ,fCurrentUser.fUserID    );
  AddParam2StartSPB(isc_spb_sec_groupid   ,fCurrentUser.fGroupID   );
  AddParam2StartSPB(isc_spb_sec_password  ,fCurrentUser.fPassword  );
  AddParam2StartSPB(isc_spb_sec_firstname ,fCurrentUser.fFirstName );
  AddParam2StartSPB(isc_spb_sec_middlename,fCurrentUser.fMiddleName);
  AddParam2StartSPB(isc_spb_sec_lastname  ,fCurrentUser.fLastName  );
  AddParam2StartSPB(isc_spb_sql_role_name ,fCurrentUser.fSQLRole   );

  Self.InternalServiceStart;
end;
{______________________________________________________________________________}
procedure TFXSecurityService.DeleteUser;
Var Len:Integer;
begin
  CheckActive;
  Len := Length(fCurrentUser.FUserName);
  if (Len = 0) then
    FXRaiseClientError(Self,fxceStartParamsError);

  StartSPB(isc_action_svc_delete_user);
  AddParam2StartSPB(isc_spb_sec_username,fCurrentUser.FUserName);

  Self.InternalServiceStart;
end;
{______________________________________________________________________________}
procedure TFXSecurityService.ModifyUser;
Var Len:Integer;
begin
  CheckActive;
  Len := Length(fCurrentUser.FUserName);
  if (Len = 0) then
    FXRaiseClientError(Self,fxceStartParamsError);

  StartSPB(isc_action_svc_modify_user);
  AddParam2StartSPB(isc_spb_sec_username,fCurrentUser.FUserName);
  if (smUserId in FModifyParams) then
    AddParam2StartSPB(isc_spb_sec_userid,fCurrentUser.FUserID);
  if (smGroupId in FModifyParams) then
    AddParam2StartSPB(isc_spb_sec_groupid,fCurrentUser.FGroupID);
  if (smPassword in FModifyParams) then
    AddParam2StartSPB(isc_spb_sec_password,fCurrentUser.FPassword);
  if (smFirstName in FModifyParams) then
    AddParam2StartSPB(isc_spb_sec_firstname,fCurrentUser.FFirstName);
  if (smMiddleName in FModifyParams) then
    AddParam2StartSPB(isc_spb_sec_middlename,fCurrentUser.FMiddleName);
  if (smLastName in FModifyParams) then
    AddParam2StartSPB(isc_spb_sec_lastname,fCurrentUser.FLastName);
  AddParam2StartSPB(isc_spb_sql_role_name,fCurrentUser.fSQLRole);

  Self.InternalServiceStart;
end;


end.

