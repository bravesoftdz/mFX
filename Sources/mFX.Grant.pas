unit mFX.Grant;

interface

{$I mFX.Inc}

uses System.Classes, System.SysUtils, System.Types,
  mFX.Intf, mFX.Header, mFX.Classes, mFX.Utils, mFX.Consts, mFX.ErrorCodes, mFX.Base, mFX.SQL;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  TFXGrant = class(TFXSQL)
  private
    fOnlyTrace     : Boolean;
    fTerminator    : Char;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddRole(Value:String);

    procedure Grant(Value:String);
    procedure RevokeAll(Value:String);

  published
    property Terminator: Char          read fTerminator write fTerminator default ';';
    property OnlyTrace : Boolean       read fOnlyTrace  write fOnlyTrace;
  end;

implementation

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXGrant.Create(AOwner: TComponent);
Begin
  inherited;
  fTerminator:=';'
end;
{______________________________________________________________________________}
procedure TFXGrant.AddRole(Value:String);
Begin
//fsdqf  Raise Exception.Create('todo');
end;
{______________________________________________________________________________}
procedure TFXGrant.Grant(Value:String);
Begin
//fdsqfds  Raise Exception.Create('todo');
end;
{______________________________________________________________________________}
procedure TFXGrant.RevokeAll(Value:String);
Begin
//fdsqfsdq  Raise Exception.Create('todo');
end;

end.
