unit mFX.DatabaseInfo;

interface

{$I mFX.Inc}

uses System.Classes,
  mFX.Header, mFX.Classes, mFX.Intf, mFX.Base;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  TFXDatabaseInfo = class(TComponent)
  protected
    FDatabase     : TFXCustomDatabase;
    FUserNames    : TStringList;
    FBackoutCounts: TStringList;
    FDeleteCounts : TStringList;
    FExpungeCounts: TStringList;
    FInsertCounts : TStringList;
    FPurgeCounts  : TStringList;
    FReadIdxCounts: TStringList;
    FReadSeqCounts: TStringList;
    FUpdateCounts : TStringList;
    function GetAllocation:FXLong;
    function GetBaseLevel:FXLong;
    function GetDBFileName:String;
    function GetDBSiteName:String;
    function GetDBImplementationNo: FXLong;
    function GetDBImplementationClass: FXLong;
    function GetNoReserve: FXLong;
    function GetODSMinorVersion: FXLong;
    function GetODSMajorVersion: FXLong;
    function GetPageSize: FXLong;
    function GetVersion: String;
    function GetCurrentMemory: FXLong;
    function GetForcedWrites: FXLong;
    function GetMaxMemory: FXLong;
    function GetNumBuffers: FXLong;
    function GetSweepInterval: FXLong;
    function GetFetches: FXLong;
    function GetMarks: FXLong;
    function GetReads: FXLong;
    function GetWrites: FXLong;
    function GetReadOnly: FXLong;
    function GetUserNames    : TStringList;
    function GetBackoutCounts: TStringList;
    function GetDeleteCounts : TStringList;
    function GetExpungeCounts: TStringList;
    function GetInsertCounts : TStringList;
    function GetPurgeCounts  : TStringList;
    function GetReadIdxCounts: TStringList;
    function GetReadSeqCounts: TStringList;
    function GetUpdateCounts : TStringList;
    procedure SetDatabase(Const Value:TFXCustomDatabase);
    procedure GetOperationCounts(Const Value:Integer;Const Values:TStringList);
    function GetLongDatabaseInfo(Const Value:Integer;Out InfoValue:FXLong):Boolean;
    function GetStringDatabaseInfo(Const Value:Byte;Out InfoValue:String):Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    Procedure DumpDBInfo(Const Values:TStringList);overload;
    Class Procedure DumpDBInfo(Const TheDB:TFXCustomDatabase;Const Values:TStringList);overload;

    property Allocation           : FXLong            read GetAllocation;
    property BaseLevel            : FXLong            read GetBaseLevel;
    property DBFileName           : String            read GetDBFileName;
    property DBSiteName           : String            read GetDBSiteName;
    property DBImplementationNo   : FXLong            read GetDBImplementationNo;
    property DBImplementationClass: FXLong            read GetDBImplementationClass;
    property NoReserve            : FXLong            read GetNoReserve;
    property ODSMinorVersion      : FXLong            read GetODSMinorVersion;
    property ODSMajorVersion      : FXLong            read GetODSMajorVersion;
    property PageSize             : FXLong            read GetPageSize;
    property Version              : String            read GetVersion;
    property CurrentMemory        : FXLong            read GetCurrentMemory;
    property ForcedWrites         : FXLong            read GetForcedWrites;
    property MaxMemory            : FXLong            read GetMaxMemory;
    property NumBuffers           : FXLong            read GetNumBuffers;
    property SweepInterval        : FXLong            read GetSweepInterval;
    property UserNames            : TStringList       read GetUserNames;
    property Fetches              : FXLong            read GetFetches;
    property Marks                : FXLong            read GetMarks;
    property Reads                : FXLong            read GetReads;
    property Writes               : FXLong            read GetWrites;
    property ReadOnly             : FXLong            read GetReadOnly;
    property BackoutCounts        : TStringList       read GetBackoutCounts;
    property DeleteCounts         : TStringList       read GetDeleteCounts;
    property ExpungeCounts        : TStringList       read GetExpungeCounts;
    property InsertCounts         : TStringList       read GetInsertCounts;
    property PurgeCounts          : TStringList       read GetPurgeCounts;
    property ReadIdxCounts        : TStringList       read GetReadIdxCounts;
    property ReadSeqCounts        : TStringList       read GetReadSeqCounts;
    property UpdateCounts         : TStringList       read GetUpdateCounts;
  published
    property Database             : TFXCustomDatabase read FDatabase write SetDatabase;
  end;

implementation

uses SysUtils,
  mFX.ErrorCodes;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXDatabaseInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TFXCustomDatabase then Begin
    FDatabase:=TFXCustomDatabase(AOwner)
  end else
  if AOwner is TFXSQLBase then Begin
    FDatabase:=TFXSQLBase(AOwner).Database
  end else
  if AOwner is TFXCustomTransaction then Begin
    FDatabase:=TFXCustomTransaction(AOwner).DefaultDatabase
    end
end;
{______________________________________________________________________________}
destructor TFXDatabaseInfo.Destroy;
begin
  SetDatabase(nil);
  inherited Destroy;
end;
{______________________________________________________________________________}
procedure TFXDatabaseInfo.SetDatabase(Const Value:TFXCustomDatabase);
Begin
  if Value<>FDatabase then Begin
    FDatabase:=Value;
    end;
  if FDatabase=nil then Begin
    FreeAndNil(FUserNames);
    FreeAndNil(FBackoutCounts);
    FreeAndNil(FDeleteCounts);
    FreeAndNil(FExpungeCounts);
    FreeAndNil(FInsertCounts);
    FreeAndNil(FPurgeCounts);
    FreeAndNil(FReadIdxCounts);
    FreeAndNil(FReadSeqCounts);
    FreeAndNil(FUpdateCounts);
    end;
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetLongDatabaseInfo(Const Value : Integer;Out InfoValue:FXLong):Boolean;
var local_buffer: array[0.._FXLocalBufferLength_] of Byte;
    DatabaseInfoCommand: AnsiChar;
    cl:IFXClientLib;
    length: Integer;
begin
  Result:=False;
  if (Self.FDatabase<>nil)and(Self.FDatabase.Connected) Then Begin
    cl:=Self.FDatabase.ClientLibrary;
    DatabaseInfoCommand := AnsiChar(Value);
    cl.Check_database_info(Self,@Self.FDatabase.Handle, 1, @DatabaseInfoCommand, _FXLocalBufferLength_,@local_buffer);
    length := mFX.Header.vax_integer(@local_buffer,1,2);
    InfoValue:= mFX.Header.vax_integer(@local_buffer,3,length);
    Result:=True
    end;
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetStringDatabaseInfo(Const Value:Byte;Out InfoValue:String):Boolean;
var local_buffer: array[0.._FXBigLocalBufferLength_ - 1] of Byte;
    DatabaseInfoCommand: Byte;
    cl:IFXClientLib;
begin
  Result:=False;
  InfoValue:=EmptyStr;
  if (Self.FDatabase<>nil)and(Self.FDatabase.Connected) Then Begin
    DatabaseInfoCommand := Value;
    cl:=Self.FDatabase.ClientLibrary;
    cl.Check_database_info(Self,@Self.FDatabase.Handle, 1, @DatabaseInfoCommand, _FXBigLocalBufferLength_,@local_buffer);
    local_buffer[4 + local_buffer[3]]:=0;
    InfoValue:=String(PAnsiChar(@local_buffer[4]));
    Result:=True
    end;
end;
{______________________________________________________________________________}
procedure TFXDatabaseInfo.GetOperationCounts(Const Value:Integer;Const Values:TStringList);
var local_buffer: array[0.._FXHugeLocalBufferLength_] of Byte;
    i, qtd_tables, id_table, qtd_operations: Integer;
    DatabaseInfoCommand: Byte;
    cl:IFXClientLib;
begin
  Assert(Values<>nil);
  if (Self.FDatabase<>nil)and(Self.FDatabase.Connected) Then Begin
    DatabaseInfoCommand := Value;
    cl:=Self.FDatabase.ClientLibrary;
    cl.Check_database_info(Self,@Self.FDatabase.Handle, 1, @DatabaseInfoCommand, _FXHugeLocalBufferLength_,@local_buffer);
    // 1. 1 byte specifying the item type requested (e.g., isc_info_insert_count).
    // 2. 2 bytes telling how many bytes compose the subsequent value pairs.
    // 3. A pair of values for each table in the database on wich the requested
    //   type of operation has occurred since the database was last attached.
    // Each pair consists of:
    // 1. 2 bytes specifying the table ID.
    // 2. 4 bytes listing the number of operations (e.g., inserts) done on that table.
    qtd_tables := trunc(mFX.Header.vax_integer(@local_buffer,1,2)/6);
    for i := 0 to qtd_tables - 1 do begin
      id_table := mFX.Header.vax_integer(@local_buffer,3+(i*6),2);
      qtd_operations := mFX.Header.vax_integer(@local_buffer,5+(i*6),4);
      Values.Add(IntToStr(id_table)+'='+IntToStr(qtd_operations));
      end;
  end else
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetAllocation: FXLong;
begin
  if Not GetLongDatabaseInfo(isc_info_allocation,Result) Then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetBaseLevel: FXLong;
var local_buffer: array[0.._FXLocalBufferLength_] of Byte;
    DatabaseInfoCommand: Byte;
    cl:IFXClientLib;
begin
  if (Self.FDatabase<>nil)and(Self.FDatabase.Connected) Then Begin
    cl:=Self.FDatabase.ClientLibrary;
    DatabaseInfoCommand:=isc_info_base_level;
    cl.Check_database_info(Self,@Self.FDatabase.Handle, 1, @DatabaseInfoCommand, _FXLocalBufferLength_,@local_buffer);
    result := mFX.Header.vax_integer(@local_buffer,4,1);
  end else Begin
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned);
    result := 0
    end;
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetDBFileName: String;
var local_buffer: array[0.._FXLocalBufferLength_] of Byte;
    DatabaseInfoCommand: Byte;
    cl:IFXClientLib;
    p,pp:PAnsiChar;
    fnl:FXInt;
begin
  if (Self.FDatabase<>nil)and(Self.FDatabase.Connected) Then Begin
    cl:=Self.FDatabase.ClientLibrary;
    DatabaseInfoCommand := isc_info_db_id;
    cl.Check_database_info(Self,@Self.FDatabase.Handle, 1, @DatabaseInfoCommand, _FXLocalBufferLength_,@local_buffer);
    fnl:=local_buffer[4];
    p := @local_buffer[5 ];
    pp:= p + fnl;
    pp^ := #0;
    result := String(p);
  end else
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetDBSiteName: String;
var local_buffer: array[0.._FXLocalBufferLength_] of Byte;
    DatabaseInfoCommand: Byte;
    cl:IFXClientLib;
    p,pp:PAnsiChar;
    fnl,snl:FXInt;
begin
  if (Self.FDatabase<>nil)and(Self.FDatabase.Connected) Then Begin
    cl:=Self.FDatabase.ClientLibrary;
    DatabaseInfoCommand := isc_info_db_id;
    cl.Check_database_info(Self,@Self.FDatabase.Handle, 1, @DatabaseInfoCommand, _FXLocalBufferLength_,@local_buffer);
    fnl:=local_buffer[4];
    snl:=local_buffer[5 + fnl ];
    p := @local_buffer[5 + fnl + 1 ];
    pp:= p + snl;
    pp^ := #0;
    result := String(p);
  end else
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetDBImplementationNo: FXLong;
var local_buffer: array[0.._FXLocalBufferLength_] of Byte;
    DatabaseInfoCommand: Byte;
    cl:IFXClientLib;
begin
  if (Self.FDatabase<>nil)and(Self.FDatabase.Connected) Then Begin
    cl:=Self.FDatabase.ClientLibrary;
    DatabaseInfoCommand := isc_info_implementation;
    cl.Check_database_info(Self,@Self.FDatabase.Handle, 1, @DatabaseInfoCommand, _FXLocalBufferLength_, @local_buffer);
    Result := mFX.Header.vax_integer(@local_buffer,3,1);
  end else Begin
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned);
    result := 0
    end;
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetDBImplementationClass: FXLong;
var local_buffer: array[0.._FXLocalBufferLength_] of Byte;
    DatabaseInfoCommand: Byte;
    cl:IFXClientLib;
begin
  if (Self.FDatabase<>nil)and(Self.FDatabase.Connected) Then Begin
    cl:=Self.FDatabase.ClientLibrary;
    DatabaseInfoCommand := isc_info_implementation;
    cl.Check_database_info(Self,@Self.FDatabase.Handle, 1, @DatabaseInfoCommand, _FXLocalBufferLength_, @local_buffer);
    Result := mFX.Header.vax_integer(@local_buffer,4,1);
  end else Begin
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned);
    result := 0
    end;
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetNoReserve: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_no_reserve,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetODSMinorVersion: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_ods_minor_version,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetODSMajorVersion: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_ods_version,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetPageSize: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_page_size,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetVersion: String;
var local_buffer:array[0.._FXLocalBufferLength_] of Byte;
    DatabaseInfoCommand: Byte;
    cl:IFXClientLib;
begin
  if (Self.FDatabase<>nil)and(Self.FDatabase.Connected) Then Begin
    cl:=Self.FDatabase.ClientLibrary;
    DatabaseInfoCommand := isc_info_version;
    cl.Check_database_info(Self,@Self.FDatabase.Handle, 1, @DatabaseInfoCommand, _FXLocalBufferLength_,@local_buffer);
    local_buffer[5 + local_buffer[4]] := 0;
    result := String(PAnsiChar(@local_buffer[5]));
  end else
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetCurrentMemory: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_current_memory,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetForcedWrites: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_forced_writes,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetMaxMemory: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_max_memory,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetNumBuffers: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_num_buffers,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetSweepInterval: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_sweep_interval,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetFetches: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_fetches,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetMarks: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_marks,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetReads: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_reads,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetWrites: FXLong;
begin
  If Not GetLongDatabaseInfo(isc_info_writes,Result) then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetUserNames: TStringList;
var local_buffer: array[0.._FXHugeLocalBufferLength_] of Byte;
    DatabaseInfoCommand: Byte;
    cl:IFXClientLib;
    user_length:FXLong;
    s:AnsiString;
    p:PAnsiChar;
begin
  if fUserNames=nil then
     fUserNames:=TStringList.Create else
     fUserNames.Clear;
  if (Self.FDatabase<>nil)and(Self.FDatabase.Connected) Then Begin
    cl:=Self.FDatabase.ClientLibrary;
    DatabaseInfoCommand := isc_info_user_names;
    cl.Check_database_info(Self,@Self.FDatabase.Handle, 1, @DatabaseInfoCommand, SizeOf(local_buffer), @local_buffer);
    p:=@local_buffer;
    while p^ = AnsiChar(isc_info_user_names) do begin
      // skip "isc_info_user_names byte" & two unknown bytes of structure (see below)
      Inc(p,3);
      user_length:=Byte(p^);
      Inc(p,1);
      s:=copy(p,0,user_length);
      FUserNames.Add(String(s));
      Inc(p,user_length);
      end;
  end else
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned);
  result := FUserNames;
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetBackoutCounts: TStringList;
begin
  if FBackoutCounts=nil then
    FBackoutCounts:=TStringList.Create else
    FBackoutCounts.Clear;
  GetOperationCounts(isc_info_backout_count,FBackoutCounts);
  Result:=FBackoutCounts
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetDeleteCounts: TStringList;
begin
  if FDeleteCounts=nil then
    FDeleteCounts:=TStringList.Create else
    FDeleteCounts.Clear;
  GetOperationCounts(isc_info_delete_count,FDeleteCounts);
  Result:=FDeleteCounts
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetExpungeCounts: TStringList;
begin
  if FExpungeCounts=nil then
    FExpungeCounts:=TStringList.Create else
    FExpungeCounts.Clear;
  GetOperationCounts(isc_info_expunge_count,FExpungeCounts);
  Result:=FExpungeCounts
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetInsertCounts: TStringList;
begin
  if FInsertCounts=nil then
    FInsertCounts:=TStringList.Create else
    FInsertCounts.Clear;
  GetOperationCounts(isc_info_insert_count,FInsertCounts);
  Result:=FInsertCounts
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetPurgeCounts: TStringList;
begin
  if FPurgeCounts=nil then
    FPurgeCounts:=TStringList.Create else
    FPurgeCounts.Clear;
  GetOperationCounts(isc_info_purge_count,FPurgeCounts);
  Result:=FPurgeCounts
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetReadIdxCounts: TStringList;
begin
  if FReadIdxCounts=nil then
    FReadIdxCounts:=TStringList.Create else
    FReadIdxCounts.Clear;
  GetOperationCounts(isc_info_read_idx_count,FReadIdxCounts);
  Result:=FReadIdxCounts
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetReadSeqCounts: TStringList;
begin
  if FReadSeqCounts=nil then
    FReadSeqCounts:=TStringList.Create else
    FReadSeqCounts.Clear;
  GetOperationCounts(isc_info_read_seq_count,FReadSeqCounts);
  Result:=FReadSeqCounts
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetUpdateCounts: TStringList;
begin
  if FUpdateCounts=nil then
    FUpdateCounts:=TStringList.Create else
    FUpdateCounts.Clear;
  GetOperationCounts(isc_info_update_count,FUpdateCounts);
  Result:=FUpdateCounts
end;
{______________________________________________________________________________}
function TFXDatabaseInfo.GetReadOnly:FXLong;
begin
  if Not GetLongDatabaseInfo(isc_info_db_read_only,Result) Then
    FXRaiseClientError(fDatabase,fxceDatabaseNotAssigned)
end;
{______________________________________________________________________________}
Class Procedure TFXDatabaseInfo.DumpDBInfo(Const TheDB:TFXCustomDatabase;Const Values:TStringList);
Var aDBInfo:TFXDatabaseInfo;
Begin
  Assert(Values<>nil);
  aDBInfo:=TFXDatabaseInfo.Create(TheDb);
  try aDBInfo.DumpDBInfo(Values);
  finally
      FreeAndNil(aDBInfo);
  end
end;
{______________________________________________________________________________}
Procedure TFXDatabaseInfo.DumpDBInfo(Const Values:TStringList);
Var i:Integer;
Begin
  Values.BeginUpdate;
  try Values.Clear;
      Values.Add(       '');
      Values.Add(       'Server Info');
      Values.Add(       '-----------');
      Values.Add(format('Name         = %s'               ,[Self.DBSiteName   ]));
      Values.Add(format('Version      = %s'               ,[Self.Version      ]));
      Values.Add(format('Level        = %d'               ,[Self.BaseLevel    ]));
      Values.Add(format('Implemenation= %d.%d'            ,[Self.DBImplementationNo,Self.DBImplementationClass]));
      Values.Add(       '');
      Values.Add(       'Database Info');
      Values.Add(       '-------------');
      Values.Add(format('DBFileName   = %s'               ,[Self.DBFileName   ]));
      Values.Add(format('ODS          = %d.%d'            ,[Self.ODSMajorVersion,Self.ODSMinorVersion]));
      Values.Add(format('ReadOnly     = %d'               ,[Self.ReadOnly     ]));
      Values.Add(format('Sweep        = %d'               ,[Self.SweepInterval]));
      Values.Add(format('NoReserve    = %d'               ,[Self.NoReserve    ]));
      Values.Add(format('Reads/Writes = %d / %d'          ,[Self.Reads,Self.Writes]));
      Values.Add(format('Marks/Fetches= %d / %d'          ,[Self.Marks,Self.Fetches]));
      Values.Add(format('PageSize     = %d'               ,[Self.PageSize     ]));
      Values.Add(format('Disk Allocat.= %d (%.0f MB)'     ,[Self.Allocation,Self.PageSize*Self.Allocation/1024/1024]));
      Values.Add(format('MaxMemory    = %.3f KB'          ,[Self.MaxMemory/1024]));
      Values.Add(format('CurrentMemory= %.3f KB (Buffers %d)',[Self.CurrentMemory/1024,Self.NumBuffers]));
      Values.Add(format('ForcedWrites = %d'               ,[Self.ForcedWrites ]));

      Values.Add('');
      Values.Add('Users');
      Values.Add('-----');
      Self.GetUserNames;
      for i:=0 to Pred(Self.fUserNames.Count) do
        Values.Add(Self.fUserNames[i]);

      Self.GetBackoutCounts;
      if Self.fBackoutCounts.Count>0 Then Begin
        Values.Add('');
        Values.Add('BackoutCounts:');
        Values.Add('--------------');
        for i:=0 to Pred(Self.fBackoutCounts.Count) do
          Values.Add(Self.fBackoutCounts[i]);
        end;

      Self.GetDeleteCounts;
      if Self.fDeleteCounts.Count>0 Then Begin
        Values.Add('');
        Values.Add('DeleteCounts.:');
        Values.Add('--------------');
        for i:=0 to Pred(Self.fDeleteCounts.Count) do
          Values.Add(Self.fDeleteCounts[i]);
        end;

      Self.GetExpungeCounts;
      if Self.fExpungeCounts.Count>0 Then Begin
        Values.Add('');
        Values.Add('ExpungeCounts:');
        Values.Add('--------------');
        for i:=0 to Pred(Self.fExpungeCounts.Count) do
          Values.Add(Self.fExpungeCounts[i]);
        end;

      Self.GetInsertCounts;
      if Self.fInsertCounts.Count>0 Then Begin
        Values.Add('');
        Values.Add('InsertCounts.:');
        Values.Add('--------------');
        for i:=0 to Pred(Self.fInsertCounts.Count) do
          Values.Add(Self.fInsertCounts[i]);
        end;

      Self.GetPurgeCounts;
      if Self.fPurgeCounts.Count>0 Then Begin
        Values.Add('');
        Values.Add('PurgeCounts..:');
        Values.Add('--------------');
        for i:=0 to Pred(Self.fPurgeCounts.Count) do
          Values.Add(Self.fPurgeCounts[i]);
        end;

      Self.GetReadIdxCounts;
      if Self.fReadIdxCounts.Count>0 Then Begin
        Values.Add('');
        Values.Add('ReadIdxCounts:');
        Values.Add('--------------');
        for i:=0 to Pred(Self.fReadIdxCounts.Count) do
          Values.Add(Self.fReadIdxCounts[i]);
        end;

      Self.GetReadSeqCounts;
      if Self.fReadSeqCounts.Count>0 Then Begin
        Values.Add('');
        Values.Add('ReadSeqCounts:');
        Values.Add('--------------');
        for i:=0 to Pred(Self.fReadSeqCounts.Count) do
          Values.Add(Self.fReadSeqCounts[i]);
        end;

      Self.GetUpdateCounts;
      if Self.fUpdateCounts.Count>0 Then Begin
        Values.Add('');
        Values.Add('UpdateCounts.:');
        Values.Add('--------------');
        for i:=0 to Pred(Self.fUpdateCounts.Count) do
          Values.Add(Self.fUpdateCounts[i]);
        end;
  finally
      Values.EndUpdate;
  end;
End;

end.
