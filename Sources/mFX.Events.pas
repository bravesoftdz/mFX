unit mFX.Events;

interface

{$I mFX.Inc}

uses System.SysUtils, System.Classes, Data.DB,
  mFX.Header, mFX.Intf, mFX.Classes, mFX.Base;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  {:The wanted events are splitted in bloc de 16, so to get the EventName [(EventGroup * FX_MAX_EVENT_BLOCK) + WhichEvent)] }
  TFXErrorEvent=procedure(Sender: TObject;ErrorCode:integer) of object;
  TFXEventAlert=procedure(Sender: TObject;EventGroup: Integer;Var EventsCount:TFXEventsCount) of object;

  TFXEventsClient = class(TComponent)
  private
    fConnected     : Boolean;
    fEvents        : TStrings;
    fBlocks        : TList;
    fBase          : TFXBase;
    fOnEventAlert  : TFXEventAlert;
    fOnError       : TFXErrorEvent;
    fHWnd          : THandle;
    procedure EventChange(Sender: TObject);
    procedure SetEvents(Const Value: TStrings);
    function GetDatabase: TFXCustomDatabase;
    procedure SetDatabase(Const Value: TFXCustomDatabase);
    function GetConnected: Boolean;
    procedure SetConnected(Const Value: Boolean);
    procedure DoBeforeDatabaseDisconnect(Sender: TObject);
  protected
    {$IFDEF mFXTRACE}
    /// <SUMMARY>Is Log Started</SUMMARY>
    function IsLogStarted(Const Action:TFXLogAction):Boolean;
    /// <SUMMARY>Log Action</SUMMARY>
    Procedure LogAction(Const Action:TFXLogAction;Const Msg: string);overload;
    /// <SUMMARY>Log Action</SUMMARY>
    Procedure LogAction(Const Action:TFXLogAction;const Fmt: string; const Args: array of const);overload;
    /// <SUMMARY>Log Error</SUMMARY>
    Procedure LogError(Const Action:TFXLogAction;Const aContext:String;Const e:Exception);overload;inline;
    {$ENDIF}
    procedure InternalConnect;
    procedure InternalDisConnect;
    procedure WndProc( var msg: TMessage );
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckClosed;
    procedure CheckDatabase;
    function EventName(Const EventGroup: Integer; Const Event: TFXEventIdx;Out Value:String):Boolean;
    property Connected    : Boolean           read GetConnected    write SetConnected;

  published
    property Database     : TFXCustomDatabase read GetDatabase     write SetDatabase;
    property Events       : TStrings          read fEvents         write SetEvents;
    property OnEventAlert : TFXEventAlert     read fOnEventAlert   write fOnEventAlert;
    property OnError      : TFXErrorEvent     read fOnError        write fOnError;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFDEF LINUX}Libc,{$ENDIF}
  mFX.Consts, SyncObjs;

Type
  TFXEventBlockState = (
    ebUndefined,
    ebStarted,
    ebCancelled
    );

  {:A Block of 16 events ...}
  TFXEventBlock = class
  private
    fOwner         : TFXEventsClient;
    fHWnd          : THandle;
    fEventGroup    : Integer;
    fEventCount    : Integer;
    fEventBufferLen: ISC_LONG;
    fEventBuffer   : PAnsiChar;
    fResultBuffer  : PAnsiChar;
    fEventID       : ISC_LONG;
    fState         : TFXEventBlockState;
    procedure Start;
    procedure Stop;
  public
    constructor Create(Const Owner: TFXEventsClient;Const TheEventGrp: Integer);
    destructor Destroy;override;
  end;

const
  mFX_EVENT = WM_USER + 8822;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure EventBlockCallback(P: Pointer; Length: Short; Updated: PAnsiChar); cdecl;
Var pp:TFXEventBlock;
begin
  Assert(P<>nil);
  pp:=TFXEventBlock(P);
  Assert(pp.fOwner<>nil);
  If (Updated=nil) then Begin
    // Check_Cancel_Events will call EventBlockCallback in the same thread !!!!
    Assert(pp.fState<ebCancelled);
    pp.fState:=ebCancelled;
    Assert(pp.fEventID<>0);
    pp.fEventID:=0;
    exit;
    end;
  Assert(pp.fHWnd<>0);
  Assert(pp.fState<ebCancelled);
  Assert(pp.fOwner.fBlocks<>nil);
  Assert(pp.fOwner.fBlocks.IndexOf(pp)>=0);
  //Copy Update locally !!!! should use critical section !!!!
  Move(Updated[0], pp.fResultBuffer[0], Length);
  //Post to main thread ....
  if Not PostMessage( pp.fHWnd , mFX_EVENT, WPARAM(pp), LPARAM(pp) ) then begin
    //Why ??? --> disconnect TFXEventsClient
    Assert(False);
    end;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXEventBlock.Create(Const Owner: TFXEventsClient;Const TheEventGrp: Integer);
Begin
  fOwner     :=Owner;
  fEventGroup:=TheEventGrp;
  fHWnd      :=Owner.fHWnd;

  fEventCount  := (fOwner.fEvents.Count - (fEventGroup * FX_MAX_EVENT_BLOCK));
  if (fEventCount > FX_MAX_EVENT_BLOCK) then
    fEventCount:= FX_MAX_EVENT_BLOCK;

end;
{______________________________________________________________________________}
destructor TFXEventBlock.Destroy;
Var cl:TFXCustomLibrary;
begin
  Assert(fEventID=0);
  Assert(fOwner<>nil);
  Assert(fOwner.fBase<>nil);
  Assert(fState<>ebStarted);
  cl:=fOwner.fBase.ClientLibrary;
  If fEventBuffer<>nil then Begin
    cl.isc_free(fEventBuffer);
    fEventBuffer := nil;
    end;
  if fResultBuffer<>nil then Begin
    cl.isc_free(fResultBuffer);
    fResultBuffer := nil;
    end;
  Inherited;
end;
{______________________________________________________________________________}
procedure TFXEventBlock.Start;
  function EBP(Index: Integer): PAnsiChar;
  begin
    Inc(Index, (fEventGroup * FX_MAX_EVENT_BLOCK));
    if (Index > fOwner.fEvents.Count) then
      Result := nil else
      Result := PAnsiChar(fOwner.fEvents[Index - 1]);
  end;
Var cl:TFXCustomLibrary;
    db:TFXCustomDatabase;
Begin
  //Add needed Blocs of event  ...
  Assert(fOwner<>nil);
  Assert(fEventCount>0);
  Assert(fEventBuffer=nil);
  Assert(fResultBuffer=nil);
  Assert(fOwner.Database<>nil);
  Assert(fOwner.Database.Connected);

  db:=fOwner.Database;
  cl:=db.ClientLibrary;

  fEventBufferLen:=cl.isc_event_block(
   @fEventBuffer,
   @fResultBuffer,
    fEventCount,
    EBP(1),
    EBP(2),
    EBP(3),
    EBP(4),
    EBP(5),
    EBP(6),
    EBP(7),
    EBP(8),
    EBP(9),
    EBP(10),
    EBP(11),
    EBP(12),
    EBP(13),
    EBP(14),
    EBP(15),
    EBP(16),
    EBP(17),
    EBP(18),
    EBP(19),
    EBP(20)
    );

  cl.Check_Queue_Events(
   @db.Handle,
   @fEventID,
    fEventBufferLen,
    fEventBuffer,
    @EventBlockCallback,
    PFXVoid(Self)
    );
end;
{______________________________________________________________________________}
procedure TFXEventBlock.Stop;
Var cl:TFXCustomLibrary;
    db:TFXCustomDatabase;
Begin
  fHWnd:=0;
  db:=fOwner.Database;
  cl:=db.ClientLibrary;

  If (fEventID<>0) then Begin
    Assert(fState<>ebCancelled);
    Assert(fOwner.Database<>nil);
    Assert(fOwner.Database.Connected);
    // Check_Cancel_Events will call EventBlockCallback in the same thread !!!!
    cl.Check_Cancel_Events(
     @db.Handle,
     @fEventID
      );
    Assert(fEventBuffer<>nil);
    cl.isc_free(fEventBuffer);
    fEventBuffer := nil;
    Assert(fResultBuffer<>nil);
    cl.isc_free(fResultBuffer);
    fResultBuffer := nil;
    Sleep(15);
    end;
  Assert(fState=ebCancelled);
  Assert(fEventID=0);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXEventsClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBase := TFXBase.Create(Self);
  fBase.BeforeDatabaseDisconnect := DoBeforeDatabaseDisconnect;
  if AOwner is TFXCustomDatabase then
    SetDatabase(TFXCustomDatabase(AOwner));
  fEvents := TStringList.Create;
  with TStringList(fEvents) do begin
    Sorted     := True;        // dupIgnore only works when the TStringList is sorted
    OnChange   := EventChange; // assign the routine which validates the event lenghts
    Duplicates := dupIgnore;   // don't allow duplicate events
    end;
  fBlocks:=TList.Create;
end;
{______________________________________________________________________________}
destructor TFXEventsClient.Destroy;
begin
  Self.InternalDisConnect;
  if FHwnd<>0 then
    DeallocateHWnd(FHwnd);
  FreeAndNil(Self.fBlocks);
  FreeAndNil(Self.fEvents);
  FreeAndNil(Self.fBase);
  inherited Destroy;
End;
{______________________________________________________________________________}
procedure TFXEventsClient.WndProc( var msg: TMessage );
Var EventsCount:TFXEventsCount;
    db:TFXCustomDatabase;
    cl:TFXCustomLibrary;
    pp:TFXEventBlock;
Begin
  case msg.msg of
    mFX_EVENT:Begin
      Assert(Self.fBase<>nil);
      Assert(Self.fBase.Database<>nil);
      Assert(Self.fBase.Database.Connected);

      db:=Self.fBase.Database;
      cl:=db.ClientLibrary;
      pp:=TFXEventBlock(msg.LParam);
      //Call isc_event_counts to get the Count and to reset the counter
      EventsCount:=cl.Call_Event_Counts(
        pp.fEventBufferLen,
        pp.fEventBuffer,
        pp.fResultBuffer
        );
      if pp.fState=ebStarted Then Begin
        if Assigned(Self.fOnEventAlert) then
          Self.fOnEventAlert(
            Self,
            pp.fEventGroup,
            EventsCount
            );
      end else
        pp.fState:=ebStarted;
      //Continue to listen ....
      cl.Check_Queue_Events(
       @db.Handle,
       @pp.fEventID,
        pp.fEventBufferLen,
        pp.fEventBuffer,
        @EventBlockCallback,
        PFXVoid(pp)
        );
    end end;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
{$IFDEF mFXTRACE}
function TFXEventsClient.IsLogStarted(Const Action:TFXLogAction):Boolean;
Var cl:TFXCustomLibrary;
Begin
  Assert(Self.fBase<>nil);
  cl:=Self.fBase.ClientLibrary;
  Result:=(cl<>nil)and(cl.IsLogStarted(Action))
end;
{______________________________________________________________________________}
Procedure TFXEventsClient.LogAction(Const Action:TFXLogAction;Const Msg: string);
Var cl:TFXCustomLibrary;
Begin
  cl:=Self.fBase.ClientLibrary;
  cl.LogAction(Self,Action,Msg)
end;
{______________________________________________________________________________}
Procedure TFXEventsClient.LogAction(Const Action:TFXLogAction;const Fmt: string; const Args: array of const);
Var cl:TFXCustomLibrary;
Begin
  cl:=Self.fBase.ClientLibrary;
  cl.LogAction(Self,Action,Fmt,Args)
end;
{______________________________________________________________________________}
Procedure TFXEventsClient.LogError(Const Action:TFXLogAction;Const aContext:String;Const e:Exception);
Var cl:TFXCustomLibrary;
Begin
  cl:=Self.fBase.ClientLibrary;
  cl.LogError(Self,Action,aContext,e)
end;
{$ENDIF}
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXEventsClient.EventName(Const EventGroup: Integer; Const Event: TFXEventIdx;Out Value:String):Boolean;
Var Idx:Integer;
Begin
  Idx:=(EventGroup * FX_MAX_EVENT_BLOCK);
  Inc(Idx,Ord(Event));
  If (Idx<fEvents.Count) then Begin
    Value:=fEvents[Idx];
    Result:=True
  end else Begin
    Value:=EmptyStr;
    Result:=False
    end;
end;
{______________________________________________________________________________}
procedure TFXEventsClient.EventChange(Sender: TObject);
var TooLong,AnyEmpty: Boolean;
    i: Integer;
begin
  TooLong:=False;
  AnyEmpty:=False;
  Self.CheckClosed;
  TStringList(fEvents).OnChange := nil;
  try
      for i := Pred(fEvents.Count) downto 0 do begin
        if (fEvents[i] = EmptyStr) then begin
          AnyEmpty := True;
          fEvents.Delete(i);
          Continue;
          end;
        if (Length(fEvents[i]) > Pred(FX_MAX_EVENT_LENGTH)) then begin
          fEvents[i] := Copy(fEvents[i], 1, Pred(FX_MAX_EVENT_LENGTH));
          TooLong := True;
          Continue;
          end;
        //OK Event correct..
        end;
      if AnyEmpty then
        FXRaiseClientError(fxceInvalidEvents);
      if TooLong then
        FXRaiseClientError(fxceInvalidEvents);
  finally
      TStringList(fEvents).OnChange := Self.EventChange;
  end;
end;
{______________________________________________________________________________}
procedure TFXEventsClient.SetEvents(Const Value: TStrings);
Begin
  Self.CheckClosed;
  fEvents.Assign(value);
end;
{______________________________________________________________________________}
function TFXEventsClient.GetDatabase: TFXCustomDatabase;
begin
  result := fBase.Database;
end;
{______________________________________________________________________________}
procedure TFXEventsClient.SetDatabase(Const Value: TFXCustomDatabase);
begin
  if (Value <> fBase.Database) then begin
    Self.CheckClosed;
    fBase.Database:=Value
    end;
end;
{______________________________________________________________________________}
function TFXEventsClient.GetConnected: Boolean;
Begin
  Result:=fConnected;
end;
{______________________________________________________________________________}
procedure TFXEventsClient.SetConnected(Const Value: Boolean);
Begin
  if Value Then
    InternalConnect else
    InternalDisConnect;
end;
{______________________________________________________________________________}
procedure TFXEventsClient.DoBeforeDatabaseDisconnect(Sender: TObject);
Begin
  InternalDisConnect;
end;
{______________________________________________________________________________}
procedure TFXEventsClient.CheckDatabase;
begin
  fBase.CheckDatabase;
end;
{______________________________________________________________________________}
procedure TFXEventsClient.CheckClosed;
{$IFOPT C+}Var i:Integer;{$ENDIF}
begin
  if Self.Connected then
    FXRaiseClientError(fxceEventAlreadyRegistered);
  {$IFOPT C+}
  for i:=Pred(fBlocks.Count) downto 0 do Begin
    Assert(fBlocks[i]=nil);
    end;
  {$ENDIF}
end;
{______________________________________________________________________________}
procedure TFXEventsClient.InternalConnect;
Var b:TFXEventBlock;
    i,c:Integer;
Begin
  CheckClosed;
  CheckDatabase;

  if FHwnd=0 then
    FHWnd:=AllocateHWnd(Self.WndProc);

  if (fEvents.Count = 0) then
    FXRaiseClientError(fxceNoEventsRegistered);

  c:=Pred(fEvents.Count) div FX_MAX_EVENT_BLOCK;
  try fBlocks.Count:=Succ(c);
      for i := 0 to c do Begin
        b:=TFXEventBlock.Create(Self,i);
        fBlocks[i]:=b;
        b.Start
        end;
      fConnected:=True;
  except on e:Exception do Begin
      {$IFDEF mFXTRACE}
      if Self.IsLogStarted(fxtError) Then
        Self.LogError(fxtError,'TFXEventsClient.InternalConnect',e);
      {$ENDIF mFXTRACE}
      InternalDisConnect;
      Raise
  end end;

end;
{______________________________________________________________________________}
procedure TFXEventsClient.InternalDisConnect;
Var b:TFXEventBlock;
    i:Integer;
Begin
  fConnected:=False;
  Assert(fBlocks<>nil);
  for i:=Pred(fBlocks.Count) downto 0 do Begin
    b:=fBlocks[i];
    if b<>nil then Begin
      b.Stop;
      Sleep(15);
    end end;
  for i:=Pred(fBlocks.Count) downto 0 do Begin
    b:=fBlocks[i];
    fBlocks[i]:=nil;
    if b<>nil then Begin
      b.Free;
      Sleep(15);
    end end;
end;

end.
