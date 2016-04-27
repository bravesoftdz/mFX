unit mFX.Blob;

interface

{$I mFX.Inc}

uses System.Classes, System.SysUtils, System.Variants, Data.DB,
  mFX.Header, mFX.Intf, mFX.Classes, mFX.Base,
  {$IFDEF mFXTRACE}mFX.Logger,{$ENDIF mFXTRACE}
  mFX.Consts, mFX.Utils, mFX.ErrorCodes,
  mFX.SQL;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  /// <summary>TFXBlobStream</summary>
  TFXCustomBlobStream = class(TStream)
  protected
    fSQLBase        : TFXSQLBase;
    FBlobID         : TISC_QUAD;
    FBlobSize       : FXLong;
    FBlobHandle     : TISC_BLOB_HANDLE;

    ///<summary>Get Database</summary>
    function GetDatabase: TFXCustomDatabase;inline;
    ///<summary>Get Database Handle</summary>
    function GetDBHandle: PISC_DB_HANDLE;

    /// <summary>GetTRHandle</summary>
    function GetTRHandle: PISC_TR_HANDLE;inline;
    ///<summary>Get Transaction</summary>
    function GetTransaction: TFXCustomTransaction;inline;

    ///<summary>Set SQLBase</summary>
    procedure SetSQLBase(Const aSQLBase:TFXSQLBase);

  protected
    {$IFDEF mFXTRACE}
    /// <SUMMARY>Log Action</SUMMARY>
    Procedure Log(Const Action:TFXLogAction;Const aMsg: string);overload;inline;
    /// <summary>Trace</summary>
    procedure LogError(Const aContext:String;Const e:Exception);overload;inline;
    /// <summary>Trace</summary>
    procedure LogInfo(Const aMsg: string);overload;inline;
    {$ENDIF mFXTRACE}

    property Database      : TFXCustomDatabase    read GetDatabase;
    property DBHandle      : PISC_DB_HANDLE       read GetDBHandle;
    property Transaction   : TFXCustomTransaction read GetTransaction;
    property TRHandle      : PISC_TR_HANDLE       read GetTRHandle;
    property SQLBase       : TFXSQLBase           read fSQLBase        write SetSQLBase;
    property BlobHandle    : TISC_BLOB_HANDLE     read FBlobHandle;
    property BlobSize      : FXLong               read FBlobSize;
  end;

  /// <summary>TFXBlobStream</summary>
  TFXBlobStream = class(TFXCustomBlobStream)
  private
    fMaxSegmentSize : FXLong;
    fNumSegments    : FXLong;
    fBlobType       : FXShort;  { 0 = segmented, 1 = streamed }
    FBuffer         : PFXByte;
    FBlobInitialized: Boolean;
    FMode           : TBlobStreamMode;
    FModified       : Boolean;
    FPosition       : Integer;

    ///<summary>Set SQLBase</summary>
    procedure SetAsQuad(Const aQuad:TISC_QUAD);

    /// <summary>Ensure BlobInitialized</summary>
    procedure EnsureBlobInitialized;
    /// <summary>SetMode</summary>
    procedure SetMode(Const Value: TBlobStreamMode);
    /// <summary>CreateBlob</summary>
    procedure CreateBlob;

  protected
    /// <summary>Open Blob</summary>
    procedure OpenBlob;

  public
    /// <summary>constructor</summary>
    constructor Create(Const AOwner: TComponent);reintroduce;
    /// <summary>destructor;</summary>
    destructor Destroy; override;

    /// <summary>CheckReadable</summary>
    procedure CheckReadable;inline;
    /// <summary>CheckWritable</summary>
    procedure CheckWritable;inline;
    /// <summary>CheckInitialized</summary>
    procedure CheckInitialized;inline;

    /// <summary>Cancel</summary>
    procedure Cancel;
    /// <summary>Finalize</summary>
    procedure Finalize;

    /// <summary>Assign</summary>
    procedure Assign(Const Source:TFXSQLVAR);
    /// <summary>Read</summary>
    function Read(var DstBuffer;Count: Longint): Longint; override;
    /// <summary>SaveToFile</summary>
    procedure SaveToFile(Const Filename: string);
    /// <summary>SaveToStream</summary>
    procedure SaveToStream(Const Stream: TStream);
    /// <summary>Seek</summary>
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    /// <summary>SetSize</summary>
    procedure SetSize(NewSize:Longint); override;
    /// <summary>Write</summary>
    function Write(const SrcBuffer;SrcBufferLen: Longint): Longint; override;

    ///<SUMMARY>Load FromFile</SUMMARY>
    procedure LoadFromFile(Const Filename: string);
    ///<SUMMARY>Load FromStream</SUMMARY>
    procedure LoadFromStream(Const Stream: TStream);
    ///<SUMMARY>Load ByteArray</SUMMARY>
    procedure LoadFromByteArray(Const Data:Variant);overload;
    ///<SUMMARY>Load ByteArray</SUMMARY>
    procedure LoadFromByteArray(Const PData:Pointer;Const Len:Integer);overload;

    property Database;
    property DBHandle;
    property Transaction;
    property TRHandle;
    property SQLBase;

    property BlobHandle;
    property BlobSize;

    property BlobType      : FXShort              read fBlobType;
    property BlobBuffer    : PFXByte              read fBuffer;

    property Mode          : TBlobStreamMode      read FMode           write SetMode        default bmRead;
    property Modified      : Boolean              read FModified;
    property MaxSegmentSize: FXLong               read fMaxSegmentSize;
    property NumSegments   : FXLong               read fNumSegments;

    property AsQuad        : TISC_QUAD            read FBlobID         write SetAsQuad;
  end;

  /// <summary>TFXBlobStream</summary>
  TFXWriteBlobStream = class(TFXCustomBlobStream)
  private
    /// <summary>Ensure Blob Open</summary>
    procedure EnsureBlobOpen;
    ///<summary>Set SQLBase</summary>
    function GetAsQuad:TISC_QUAD;
  public
    /// <summary>constructor</summary>
    constructor Create(Const AOwner: TComponent);reintroduce;
    /// <summary>destructor;</summary>
    destructor Destroy; override;

    /// <summary>Seek</summary>
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    /// <summary>Write</summary>
    function Write(const SrcBuffer;SrcBufferLen: Longint): Longint; override;

    property AsQuad        : TISC_QUAD            read GetAsQuad;
  end;

implementation

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
{$IFDEF mFXTRACE}
procedure TFXCustomBlobStream.Log(Const Action:TFXLogAction;Const aMsg: string);
Begin
  if (fSQLBase<>nil) then
    fSQLBase.Log(Self,Action,aMsg);
end;
{______________________________________________________________________________}
procedure TFXCustomBlobStream.LogError(Const aContext:String;Const e:Exception);
Begin
  if (fSQLBase<>nil) then
    fSQLBase.LogError(Self,aContext,e);
end;
{______________________________________________________________________________}
procedure TFXCustomBlobStream.LogInfo(Const aMsg: string);
Begin
  if (fSQLBase<>nil) then
    fSQLBase.Log(Self,fxtBlobInfo,aMsg);
end;
{$ENDIF mFXTRACE}
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXCustomBlobStream.SetSQLBase(Const aSQLBase:TFXSQLBase);
begin
  fSQLBase:=aSQLBase;
end;
{______________________________________________________________________________}
function TFXCustomBlobStream.GetDatabase: TFXCustomDatabase;
begin
  if fSQLBase<>nil then
    result := fSQLBase.Database else
    result := nil
end;
{______________________________________________________________________________}
function TFXCustomBlobStream.GetDBHandle: PISC_DB_HANDLE;
begin
  if fSQLBase<>nil then
    result := fSQLBase.DBHandle else
    result := nil;
end;
{______________________________________________________________________________}
function TFXCustomBlobStream.GetTransaction: TFXCustomTransaction;
begin
  if fSQLBase<>nil then
    result := fSQLBase.Transaction else
    result := nil;
end;
{______________________________________________________________________________}
function TFXCustomBlobStream.GetTRHandle: PISC_TR_HANDLE;
begin
  if fSQLBase<>nil then
    result := fSQLBase.TRHandle else
    result := nil
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXBlobStream.Create(Const AOwner: TComponent);
begin
  inherited Create;
  fMaxSegmentSize:=FX_DefaultBlobSegmentSize;
  if aOwner is TFXSQLBase then
    SetSQLBase(TFXSQLBase(aOwner))
end;
{______________________________________________________________________________}
destructor TFXBlobStream.Destroy;
var cl:IFXClientLib;
    Err:ISC_STATUS;
begin
  if (FBlobHandle <> nil) then Begin
    cl:=fSQLBase.ClientLibrary;
    Err:=cl.Call_close_blob(@fBlobHandle);
    if (Err>0) then
      cl.ReadFirebirdError(Self);
    end;
  FXFree(FBuffer);
  inherited
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXBlobStream.CheckReadable;
begin
  if fSQLBase=nil then
    FXRaiseClientError(Self,fxceDatabaseNotAssigned);
  if FMode = bmWrite then
    FXRaiseClientError(Self,fxceBlobCannotBeRead);
end;
{______________________________________________________________________________}
procedure TFXBlobStream.CheckWritable;
begin
  if FMode = bmRead then
    FXRaiseClientError(Self,fxceBlobCannotBeWritten);
end;
{______________________________________________________________________________}
procedure TFXBlobStream.CheckInitialized;
begin
  if fSQLBase=nil then
    FXRaiseClientError(Self,fxceDatabaseNotAssigned);
  if not FBlobInitialized then
    FXRaiseClientError(Self,fxceBlobNotIntialized);
end;
{______________________________________________________________________________}
procedure TFXBlobStream.CreateBlob;
begin
  Self.CheckWritable;
  FBlobInitialized := True;
  FBlobID.gds_quad_high := 0;
  FBlobID.gds_quad_low := 0;
  Self.SetSize(0);
end;
{______________________________________________________________________________}
procedure TFXBlobStream.EnsureBlobInitialized;
begin
  if not FBlobInitialized then Begin
    case FMode of
      bmWrite:Begin
        Self.CreateBlob;
        end;
      bmReadWrite: begin
        if (FBlobID.gds_quad_high = 0) and (FBlobID.gds_quad_low = 0) then
          Self.CreateBlob else
          Self.OpenBlob;
        end;
      else Begin
        Self.OpenBlob;
    end end end;
end;
{______________________________________________________________________________}
procedure TFXBlobStream.Finalize;
Var cl:IFXClientLib;
begin
  Self.CheckWritable;
  Self.CheckInitialized;
  Assert(FBlobHandle=nil);
  {$IFDEF MFXTRACE}Self.LogInfo('Write Blob '+IntToStr(FBlobSize)+' bytes');{$ENDIF}

  cl:=fSQLBase.ClientLibrary;
  cl.Check_create_blob2(fSQLBase.DBHandle, fSQLBase.TRHandle, @FBlobHandle, @FBlobID);
  cl.WriteBlob(@FBlobHandle, FBuffer, FBlobSize);
  cl.Check_close_blob(@FBlobHandle);

  fModified := False;
end;
{______________________________________________________________________________}
procedure TFXBlobStream.LoadFromFile(Const Filename: string);
var Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try LoadFromStream(Stream)
  finally
      Stream.Free
  end
end;
{______________________________________________________________________________}
procedure TFXBlobStream.LoadFromStream(Const Stream: TStream);
Var NewSize:Int64;
begin
  Self.CheckWritable;
  EnsureBlobInitialized;
  Stream.Position := 0;
  NewSize:=Stream.Size;
  if NewSize>0 then Begin
    SetSize(NewSize);
    Assert(NewSize=FBlobSize);
    Stream.ReadBuffer(FBuffer^,NewSize);
  End else
    SetSize(0);
  FModified := True;
end;
{______________________________________________________________________________}
procedure TFXBlobStream.LoadFromByteArray(Const Data:Variant);
var NewSize:Integer;
    PData:Pointer;
    vt:TVarType;
Begin
  vt:=System.Variants.VarType(Data);
  if (vt and varArray)<>varArray then
    FXRaiseClientError(Self,fxceNotPermitted);
  if (vt and VarTypeMask)<>varByte Then
    FXRaiseClientError(Self,fxceNotPermitted);
  if VarArrayDimCount(Data)<>1 Then
    FXRaiseClientError(Self,fxceNotPermitted);

  Self.CheckWritable;
  EnsureBlobInitialized;
  NewSize:=VarArrayHighBound(Data,1)+1;
  if NewSize>0 then Begin
    SetSize(NewSize);
    Assert(NewSize=FBlobSize);
    PData:=VarArrayLock(Data);
    try Move(PData^,FBuffer^,NewSize);
    finally
        VarArrayUnlock(Data);
    end
  End else
    SetSize(0);
  FModified := True;
End;
{______________________________________________________________________________}
procedure TFXBlobStream.LoadFromByteArray(Const PData:Pointer;Const Len:Integer);
Begin
  if Len>0 then Begin
    SetSize(Len);
    Assert(Len=FBlobSize);
    Move(PData^,FBuffer^,Len);
  End else
    SetSize(0);
  FModified := True;
End;
{______________________________________________________________________________}
procedure TFXBlobStream.OpenBlob;
Var cl:IFXClientLib;
    iBlobSize:FXLong;
    res:ISC_STATUS;
begin
  Self.CheckReadable;
  {$IFDEF MFXTRACE}Self.LogInfo('Read Blob');{$ENDIF}

  cl:=fSQLBase.ClientLibrary;
  cl.Check_open_blob2(fSQLBase.DBHandle, fSQLBase.TRHandle, @FBlobHandle, @FBlobID);

  res:=cl.Call_BlobInfo(@FBlobHandle, fNumSegments, fMaxSegmentSize, iBlobSize, fBlobType);
  if res>0 then Begin
    Self.SetSize(0);
    fBlobInitialized:=False;
    cl.ReadFirebirdError(Self);
    cl.Call_close_blob(@FBlobHandle);
    cl.RaiseLastFirebirdError(Self);
    end;

  if iBlobSize>0 then Begin
    Self.SetSize(iBlobSize);
    if not cl.Call_ReadBlob(@FBlobHandle, FBuffer, fNumSegments, fMaxSegmentSize, FBlobSize) then Begin
      Self.SetSize(0);
      fBlobInitialized:=False;
      cl.ReadFirebirdError(Self);
      cl.Call_close_blob(@FBlobHandle);
      cl.RaiseLastFirebirdError(Self);
      end;
  End else
    Self.SetSize(0);

  if cl.Call_close_blob(@FBlobHandle)>0 then Begin
    Self.SetSize(0);
    fBlobInitialized:=False;
    cl.ReadFirebirdError(Self);
    cl.Call_close_blob(@FBlobHandle);
    cl.RaiseLastFirebirdError(Self);
    end;

  {$IFDEF MFXTRACE}Self.LogInfo('Blob Size '+IntToStr(FBlobSize)+' bytes');{$ENDIF}
  fBlobInitialized := True;
end;
{______________________________________________________________________________}
function TFXBlobStream.Read(var DstBuffer;Count: Longint): Longint;
Var p:PByte;
begin
  Self.CheckReadable;
  EnsureBlobInitialized;
  if (Count <= 0) then begin
    result := 0;
    exit;
    end;

  if (FPosition + Count > FBlobSize) then
    result := FBlobSize - FPosition
  else
    result := Count;

  p:=Pointer(FBuffer);
  Inc(p,FPosition);
  Move(p^,DstBuffer, result);
  Inc(FPosition, Result);
end;
{______________________________________________________________________________}
procedure TFXBlobStream.SaveToFile(Const Filename: string);
var Stream: TStream;
begin
  EnsureBlobInitialized;
  if FBlobSize <> 0 then begin
    Stream := TFileStream.Create(FileName, fmCreate);
    try // DoNot CheckReadable
        Seek(0, soFromBeginning);
        Stream.WriteBuffer(FBuffer^,FBlobSize);
    finally
        Stream.Free;
    end end;
end;
{______________________________________________________________________________}
procedure TFXBlobStream.SaveToStream(Const Stream: TStream);
begin
  CheckReadable;
  EnsureBlobInitialized;
  if FBlobSize <> 0 then begin
    Seek(0, soFromBeginning);
    Stream.WriteBuffer(FBuffer^,FBlobSize);
    end
end;
{______________________________________________________________________________}
function TFXBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  EnsureBlobInitialized;
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent  : Inc(FPosition, Offset);
    soFromEnd      : FPosition := FBlobSize + Offset;
    end;
  result := FPosition;
end;
{______________________________________________________________________________}
procedure TFXBlobStream.SetAsQuad(Const aQuad:TISC_QUAD);
Begin
  System.Move(aQuad, FBlobID, SizeOf(TISC_QUAD));
  fBlobInitialized := False;
End;
{______________________________________________________________________________}
procedure TFXBlobStream.Assign(Const Source:TFXSQLVAR);
Var AsQuad:TISC_QUAD;
begin
  if Source<>nil then Begin
    if Not Source.IsNull then Begin
       fSQLBase:=Source.SQL;
       AsQuad:=Source.AsQuad;
       System.Move(AsQuad, FBlobID, SizeOf(TISC_QUAD));
       fBlobInitialized := False;
    end else
      Self.SetSize(0);
  end else
    Self.SetSize(0);
end;
{______________________________________________________________________________}
procedure TFXBlobStream.SetMode(Const Value: TBlobStreamMode);
begin
  FMode := Value;
  FBlobInitialized := False;
end;
{______________________________________________________________________________}
procedure TFXBlobStream.SetSize(NewSize: Longint);
begin
  if (NewSize <> FBlobSize) then begin
    if NewSize = 0 then
      FXFree(FBuffer) else
    if fBlobSize=0 then
      FXAlloc(FBuffer,NewSize,False) else
    if NewSize>fBlobSize then
      FXReAlloc(FBuffer,FBlobSize,NewSize,False);
    FBlobSize := NewSize;
    end;
  if fPosition>fBlobSize then
    fPosition:=fBlobSize;
end;
{______________________________________________________________________________}
function TFXBlobStream.Write(const SrcBuffer; SrcBufferLen: Longint): Longint;
var pb:PFXByte;
begin
  Self.CheckWritable;
  EnsureBlobInitialized;

  result:=SrcBufferLen;
  if SrcBufferLen>0 then Begin
    if (FPosition + SrcBufferLen > FBlobSize) then
      SetSize(FPosition + SrcBufferLen);
    pb:=fBuffer;Inc(pb,fPosition);
    Move(SrcBuffer,pb^,SrcBufferLen);
    Inc(FPosition, SrcBufferLen);
    FModified := True;
    end;
end;
{______________________________________________________________________________}
procedure TFXBlobStream.Cancel;
begin
  if (not FBlobInitialized) or (FMode = bmRead) then
    exit;
  if FModified then
    OpenBlob;
  FModified := False;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXWriteBlobStream.Create(Const AOwner: TComponent);
begin
  inherited Create;
  if aOwner is TFXSQLBase then
    SetSQLBase(TFXSQLBase(aOwner))
end;
{______________________________________________________________________________}
destructor TFXWriteBlobStream.Destroy;
var cl:IFXClientLib;
    Err:ISC_STATUS;
begin
  if (FBlobHandle <> nil) then Begin
    cl:=fSQLBase.ClientLibrary;
    Err:=cl.Call_close_blob(@fBlobHandle);
    if (Err>0) then
      cl.ReadFirebirdError(Self);
    end;
  inherited
end;
{______________________________________________________________________________}
procedure TFXWriteBlobStream.EnsureBlobOpen;
Var cl:IFXClientLib;
begin
  if FBlobHandle=nil then Begin
    cl:=fSQLBase.ClientLibrary;
    cl.Check_create_blob2(fSQLBase.DBHandle, fSQLBase.TRHandle, @FBlobHandle, @FBlobID);
    end;
end;
{______________________________________________________________________________}
function TFXWriteBlobStream.GetAsQuad: TISC_QUAD;
Var cl:IFXClientLib;
begin
  if (FBlobHandle<>nil) then Begin
    cl:=fSQLBase.ClientLibrary;
    cl.Check_close_blob(@FBlobHandle);
    Result:=FBlobID;
  end else Begin
    Result.gds_quad_high:=0;
    Result.gds_quad_low :=0;
    end;
end;
{______________________________________________________________________________}
function TFXWriteBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  // Just ignore
  Result:=FBlobSize;
end;
{______________________________________________________________________________}
function TFXWriteBlobStream.Write(const SrcBuffer; SrcBufferLen: Longint): Longint;
var cl:IFXClientLib;
begin
  Self.EnsureBlobOpen;
  cl:=fSQLBase.ClientLibrary;
  cl.WriteBlob(@FBlobHandle, @SrcBuffer, SrcBufferLen);
  Inc(FBlobSize,SrcBufferLen);
  result:=SrcBufferLen;
end;

end.

