unit mFX.Database;

interface

{$I mFX.Inc}

uses System.SysUtils, System.Classes, System.Math, System.typInfo, Data.DB,
  mFX.Header, mFX.Intf, mFX.Classes, mFX.Alias, mFX.Base, mFX.Consts, mFX.ErrorCodes,
  {$IFDEF mFXTRACE}mFX.Logger,{$ENDIF mFXTRACE}
  mFX.MetaData, mFX.Schema, mFX.SQL;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  /// <summary>Firebird Database Connection</summary>
  TFXDatabase = class(TFXCustomDatabase)
  private
    fConnectionLost      : Boolean;
    fQuoteIden           : Boolean;
    fDefaultCharSet      : String;
    fSchema              : TFXSchema;

    /// <summary>Get DefaultCharSet</summary>
    function GetDefaultCharSet:String;

  protected
    /// <summary>Check Schema</summary>
    procedure CheckSchema;
    /// <summary>GetSchema:</summary>
    function GetSchema: TFXCustomSchema;override;
    /// <summary>Do Before Disconnect Event</summary>
    procedure DoAfterConnectEvent;override;

  protected
    /// <summary>Create ClientLib</summary>
    procedure CreateClientLib;virtual;

  public
    /// <summary>constructor </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>destructor</summary>
    destructor Destroy; override;

    /// <summary>Clear Schema</summary>
    procedure ClearSchema;override;

    /// <summary>GetServeurDate</summary>
    function  GetServeurDate:TDateTime;
    /// <summary>GetServeurDateTime</summary>
    function  GetServeurDateTime:TDateTime;

    /// <summary>Generator functions</summary>
    function GetGen(Const aGenName:String;Const aStep:Integer):Integer;
    /// <summary>Generator functions</summary>
    function SetGen(Const aGenName:String;Const aNewValue:Integer):Integer;

    /// <summary>ConnectedUsers</summary>
    procedure ConnectedUsers(Const Values:TStrings);

    property DefaultCharSet    : String               read GetDefaultCharSet;
    property Schema            : TFXCustomSchema      read GetSchema;

  published
    property Connected stored False;
    property DatabaseName stored False;
    property ClientLibrary;
    property Params;
    property MasterTR;
    property QuoteIden         : Boolean              read fQuoteIden        write fQuoteIden default False;
    property ConnectionLost    : Boolean              read fConnectionLost   write fConnectionLost;
    property AfterConnect;
    property AfterDisconnect;
    property BeforeConnect;
    property BeforeDisconnect;
  end;

  /// <summary>Firebird Transaction Object</summary>
  TFXTransaction = class(TFXCustomTransaction)
  published
    property Active;
    property DefaultDatabase;
    property DefaultAction;
    property Params;
  end;

implementation

uses mFX.SystemTable, mFX.Utils, mFX.ClientLib;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXDatabase.Create(AOwner: TComponent);
Begin
  Inherited;
  CreateClientLib;
end;
{______________________________________________________________________________}
destructor TFXDatabase.Destroy;
begin
  fSchema.Free;
  inherited;
end;
{______________________________________________________________________________}
Procedure TFXDatabase.CreateClientLib;
Begin
  Assert(fLibrary=nil);
  fLibrary:=mFX.Intf.LoadLib
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
{$IFDEF xmFXTRACE}
Procedure TFXCustomDatabase.LogBeforeAction(Const Action: TFXLogAction);
Begin
  if fLibrary<>nil Then
    if fLibrary.IsLogStarted(Action) then
      fLibrary.LogBeforeAction(Self,Action)
end;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.LogAfterAction(Const Action: TFXLogAction);
Begin
  if fLibrary<>nil Then
    if fLibrary.IsLogStarted(Action) then
      fLibrary.LogAfterAction(Self,Action)
end;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.LogAction(Const Action:TFXLogAction;Const Msg:String);
Begin
  fLibrary.LogAction(Self,Action,Msg)
end;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.LogActionIf(Const Action:TFXLogAction);
Begin
  if fLibrary<>nil Then
    if fLibrary.IsLogStarted(Action) then
      fLibrary.LogAction(Self,Action)
end;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.LogActionIf(Const Action:TFXLogAction;Const Msg:String);
Begin
  if fLibrary<>nil Then
    if fLibrary.IsLogStarted(Action) then
      fLibrary.LogAction(Self,Action,Msg)
end;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.LogActionIf(Const Action:TFXLogAction;const Fmt:String; const Args: array of const);
Begin
  if fLibrary<>nil Then
    if fLibrary.IsLogStarted(Action) then
      fLibrary.LogAction(Self,Action,Format(Fmt,Args))
end;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.LogActionIf(Const Action:TFXLogAction;Const aParent:TComponent;Const Msg:String);
Begin
  if fLibrary<>nil Then
    if fLibrary.IsLogStarted(Action) Then
      fLibrary.LogAction(aParent,Action,Msg)
end;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.LogAction(Const Action:TFXLogAction;Const aParent:TComponent;Const Msg:String);
Begin
  if aParent<>nil then Begin
    if aParent.Name<>'' then
      fLibrary.LogAction(Self,Action,aParent.ClassName+'.'+aParent.Name+':'+Msg) else
      fLibrary.LogAction(Self,Action,aParent.ClassName+':'+Msg);
  end else
    fLibrary.LogAction(Self,Action,Msg)
end;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.LogAction(Const Action:TFXLogAction;const Fmt:String; const Args: array of const);
Var Msg:String;
Begin
  Msg:=Format(Fmt,Args);
  fLibrary.LogAction(Self,Action,Msg)
end;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.LogError(Const Action:TFXLogAction;Const aContext:String;Const Err:ISC_STATUS);
Begin
  fLibrary.LogError(Self,Action,aContext,Err);
end;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.LogError(Const Action:TFXLogAction;Const aContext:String;Const e:Exception);
Begin
  fLibrary.LogError(Self,Action,aContext,e);
end;
{______________________________________________________________________________}
Procedure TFXCustomDatabase.LogError(Const aParent:TComponent;Const Action:TFXLogAction;Const aContext:String;Const e:Exception);
Begin
  if aParent<>nil then Begin
    if aParent.Name<>'' then
      fLibrary.LogError(Self,Action,aParent.ClassName+'.'+aParent.Name+':'+aContext,e) else
      fLibrary.LogError(Self,Action,aParent.ClassName+':'+aContext,e);
  end else
    fLibrary.LogError(Self,fxtError,aContext,e)
end;
{$ENDIF xmFXTRACE}
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
{$IFDEF xmFXTRACE}
Procedure TFXCustomTransaction.LogBeforeAction(Const Action: TFXLogAction);
Begin
  if fLibrary<>nil Then
    if fLibrary.IsLogStarted(Action) then
      fLibrary.LogBeforeAction(Self,Action)
end;
{______________________________________________________________________________}
Procedure TFXCustomTransaction.LogAfterAction(Const Action: TFXLogAction);
Begin
  if fLibrary<>nil Then
    if fLibrary.IsLogStarted(Action) then
      fLibrary.LogAfterAction(Self,Action)
end;
{______________________________________________________________________________}
Procedure TFXCustomTransaction.LogActionIf(Const Action:TFXLogAction);
Begin
  if fLibrary<>nil Then
    if fLibrary.IsLogStarted(Action) then
      fLibrary.LogAction(Self,Action)
end;
{______________________________________________________________________________}
Procedure TFXCustomTransaction.LogActionIf(Const Action:TFXLogAction;Const Msg:String);
Begin
  if fLibrary<>nil Then
    if fLibrary.IsLogStarted(Action) then
      fLibrary.LogAction(Self,Action,Msg)
end;
{______________________________________________________________________________}
Procedure TFXCustomTransaction.LogActionIf(Const Action:TFXLogAction;const Fmt:String; const Args: array of const);
Begin
  if fLibrary<>nil Then
    if fLibrary.IsLogStarted(Action) then
      fLibrary.LogAction(Self,Action,Format(Fmt,Args))
end;
{$ENDIF xmFXTRACE}
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXDatabase.ClearSchema;
begin
  if fSchema<>nil then
    fSchema.Clear;
end;
{______________________________________________________________________________}
procedure TFXDatabase.CheckSchema;
Begin
  if fSchema=nil then
    fSchema := TFXSchema.Create(Self);
end;
{______________________________________________________________________________}
function TFXDatabase.GetSchema: TFXCustomSchema;
Begin
  Self.CheckSchema;
  Result:=fSchema
End;
{______________________________________________________________________________}
procedure TFXDatabase.DoAfterConnectEvent;
Begin
  if Assigned(Self.AfterConnect) then
    Self.AfterConnect(Self);
  Self.ClearSchema;
End;
{______________________________________________________________________________}
procedure TFXDatabase.ConnectedUsers(Const Values:TStrings);
var local_buffer: array[0.._FXHugeLocalBufferLength_] of Byte;
    DatabaseInfoCommand: AnsiChar;
    user_length:Byte;
    s:AnsiString;
    p:PAnsiChar;
Begin
  CheckConnected;
  Values.BeginUpdate;
  try Values.Clear;
      DatabaseInfoCommand := AnsiChar(isc_info_user_names);
      ClientLibrary.Check_database_info(Self,@Self.Handle,1,@DatabaseInfoCommand,SizeOf(local_buffer),@local_buffer[0]);
      p:=@local_buffer;
      while p^ = AnsiChar(isc_info_user_names) do begin
        Inc(p,3);
        user_length:=Byte(p^);
        Inc(p,1);
        s:=copy(p,0,user_length);
        Values.Add(String(s));
        Inc(p,user_length);
        end
  finally
      Values.EndUpdate
  end
end;
{______________________________________________________________________________}
function TFXDatabase.GetDefaultCharSet:String;
var TempoSQL:TFXCustomSQL;
    DidStart:Boolean;
    s:String;
Begin
  CheckConnected;
  if fDefaultCharSet=EmptyStr then Begin
    DidStart:=False;
    TempoSQL:=TFXCustomSQL.Create(Self);
    try DidStart:=TempoSQL.Start_TR;
        TempoSQL.SQL.Text:='Select RDB$CHARACTER_SET_NAME from RDB$DATABASE';
        TempoSQL.ParamCheck:=False;
        TempoSQL.ExecQuery;
        If Not TempoSQL.Eof then Begin
          s:=TempoSQL.Fields[0].AsTrimString;
          if s<>EmptyStr then
            fDefaultCharSet:=s else
            fDefaultCharSet:='NONE';
        end else
          fDefaultCharSet:='NONE';
        TempoSQL.CloseQuery;
    finally
        if DidStart then
          TempoSQL.Commit_TR;
        TempoSQL.Free;
    end end;
  Result:=fDefaultCharSet
end;
{______________________________________________________________________________}
function  TFXDatabase.GetServeurDate:TDateTime;
var TempoSQL:TFXCustomSQL;
    DidStart:Boolean;
Begin
  if Self.Connected then Begin
    TempoSQL:=TFXCustomSQL.Create(Self);
    try DidStart:=TempoSQL.Start_TR;
        TempoSQL.SQL.Text:='Select current_date From RDB$DATABASE';
        TempoSQL.DefaultFieldsCount:=1;
        TempoSQL.ParamCheck:=False;
        TempoSQL.ExecQuery;
        Result:=TempoSQL.Fields[0].AsDate;
        TempoSQL.CloseQuery;
        if DidStart then
          TempoSQL.Commit_TR;
    finally
        TempoSQL.free
  end end else
    Result:=System.SysUtils.Date;
end;
{______________________________________________________________________________}
function TFXDatabase.GetServeurDateTime:TDateTime;
var TempoSQL:TFXCustomSQL;
    DidStart:Boolean;
Begin
  if Self.Connected then Begin
    TempoSQL:=TFXCustomSQL.Create(Self);
    try DidStart:=TempoSQL.Start_TR;
        TempoSQL.SQL.Text:='Select current_timestamp From RDB$DATABASE';
        TempoSQL.DefaultFieldsCount:=1;
        TempoSQL.ParamCheck:=False;
        TempoSQL.ExecQuery;
        Result:=TempoSQL.Fields[0].AsDateTime;
        TempoSQL.CloseQuery;
        if DidStart then
          TempoSQL.Commit_TR;
    finally
        TempoSQL.free
  end end else
    Result:=System.SysUtils.Date;
end;
{______________________________________________________________________________}
function TFXDatabase.GetGen(Const aGenName:String;Const aStep:Integer):Integer;
var TempoSQL:TFXCustomSQL;
    DidStart:Boolean;
Begin
  Self.CheckConnected;
  TempoSQL:=TFXCustomSQL.Create(Self);
  try DidStart:=TempoSQL.Start_TR;
      TempoSQL.SQL.Text:=format('select GEN_ID(%s,%d) from RDB$DATABASE',[aGenName,aStep]);
      TempoSQL.DefaultFieldsCount:=1;
      TempoSQL.ParamCheck:=False;
      Try TempoSQL.ExecQuery;
          Result:=TempoSQL.Fields[0].AsInt64;
          if aStep>1 then
            Result:=Result-aStep+1;
          TempoSQL.CloseQuery;
      except on e:EFXFirebirdError do Begin
          TempoSQL.CloseQuery;
          if e.ErrorCode=isc_invalid_blr then Begin
            TempoSQL.SQL.Text:=format('Create Generator %s',[aGenName]);
            TempoSQL.ExecQuery;
            TempoSQL.CloseQuery;
            TempoSQL.SQL.Text:=format('Set Generator %s to %d',[aGenName,aStep]);
            TempoSQL.ExecQuery;
            TempoSQL.CloseQuery;
            Result:=1;
          end else Begin
            Result:=1;
            Raise
          end end;
      else begin
          TempoSQL.CloseQuery;
          Result:=1;
          Raise
      end end;
      if DidStart then
        TempoSQL.Commit_TR;
  finally
      TempoSQL.free
  end
end;
{______________________________________________________________________________}
function TFXDatabase.SetGen(Const aGenName:String;Const aNewValue:Integer):Integer;
var TempoSQL:TFXCustomSQL;
    DidStart:Boolean;
Begin
  result:=aNewValue;
  Self.CheckConnected;
  TempoSQL:=TFXCustomSQL.Create(Self);
  try DidStart:=TempoSQL.Start_TR;
      TempoSQL.sql.Text:=format('SET GENERATOR %s to %d ',[aGenName,aNewValue]);
      TempoSQL.ParamCheck:=False;
      Try TempoSQL.ExecQuery;
          TempoSQL.CloseQuery;
      except on e:EFXFirebirdError do Begin
          TempoSQL.CloseQuery;
          if e.ErrorCode=isc_invalid_blr then Begin
            TempoSQL.SQL.Text:=format('Create Generator %s',[aGenName]);
            TempoSQL.ExecQuery;
            TempoSQL.CloseQuery;
            TempoSQL.SQL.Text:=format('Set Generator %s to %d',[aGenName,aNewValue]);
            TempoSQL.ExecQuery;
            TempoSQL.CloseQuery;
          end else
            Raise
          end;
      on ee:Exception do begin
          TempoSQL.CloseQuery;
          Raise
      end end;
      if DidStart then
        TempoSQL.Commit_TR;
  finally
      TempoSQL.free
  end
End;

end.

