unit mFX.Schema;

interface

{$I mFX.Inc}
{$M-}

uses Classes, SysUtils,
  mFX.Header, mFX.Intf, mFX.List, mFX.Classes,
  mFX.Base, mFX.MetaData;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  ///<SUMMARY>FX DB Cached Schema Status Flag</SUMMARY>
  TFXSchemaFlag = (
    sfReloadTables,
    sfReloadViews,
    sfReloadProcs,
    sfReloadRoles
    );
  TFXSchemaFlags = set of TFXSchemaFlag;

  ///<SUMMARY>Database Def</SUMMARY>
  TFXSchema = class(TFXCustomMetaSchema)
  private
    fFlags         : TFXSchemaFlags;
  public
    ///<SUMMARY>Clear all Relation.Field Schema</SUMMARY>
    procedure Clear;override;

    ///<SUMMARY>Find Relation.Field Schema</SUMMARY>
    function FindRelation(Const aTR:TFXCustomTransaction;Const aName:String):TFXCustomMetaRelation;override;

  end;

implementation

Uses StrUtils,
  mFX.Consts, mFX.SystemTable;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSchema.Clear;
begin
  fFlags:=[];
  inherited
end;
{______________________________________________________________________________}
function TFXSchema.FindRelation(Const aTR:TFXCustomTransaction;Const aName : String) : TFXCustomMetaRelation;
var SavTR:TFXCustomTransaction;
    DidActivate:Boolean;
    r:TFXMetaTable;
    v:TFXMetaView;
Begin
  Result:=Self.FindRelation(aName);
  if (Result=nil)and(not fLoaded) then Begin
    if fDB.Connected then Begin
      SavTR:=fTRx;
      DidActivate:=False;
      try Self.GetSQL1;
          if aTR<>nil then
            SetTr(aTR) else
          if fTRx<>nil then
            SetTr(fDB.ROAutoTR);
          DidActivate := fSQL1.Start_TR;
          TFXMetaTable.PrepareQuery(aName,fSQL1);
          fSQL1.ParamCheck:=False;
          fSQL1.ExecQuery;
          if Not fSQL1.Eof then Begin
            Self.GetSQL2;
            Self.GetSQL3;
            r:=TFXMetaTable.CreateFromSQL(Self,fSQL1);
            r.Download(fSQL2,fSQL3);
            Result:=r;
            end;
          fSQL1.CloseQuery;
          if (Result=nil) then Begin
            TFXMetaView.PrepareQuery(aName,fSQL1);
            fSQL1.ParamCheck:=False;
            fSQL1.ExecQuery;
            if Not fSQL1.Eof then Begin
              Self.GetSQL2;
              Self.GetSQL3;
              v:=TFXMetaView.CreateFromSQL(Self,fSQL1);
              v.Download(fSQL2,fSQL3);
              Result:=v;
              end;
            fSQL1.CloseQuery;
            end;
      finally
          if DidActivate then
            fSQL1.Commit_TR;
          SetTr(SavTR);
    end end end;
End;

end.


