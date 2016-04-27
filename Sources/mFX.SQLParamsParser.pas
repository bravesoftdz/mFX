unit mFX.SQLParamsParser;

interface

{$I mFX.Inc}
{$M-}

Uses Types, Classes,
  mFX.List, mFX.Classes, mFX.Parser;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  TFXSQLParamsParser = Class(TFXCustomParser)
  private
    fQuery           : String;
    fParamsNames     : TStrings;
    /// <summary>AddParam</summary>
    procedure AddParam(Const aParamName:String);overload;
  public
    /// <summary>destructor/summary>
    destructor Destroy; override;

    /// <summary>Clear</summary>
    procedure Clear;override;
    /// <summary>Parse Query</summary>
    procedure ExtractParams(Const Value:String);

    property Query       : String          read fQuery;
    property ParamsNames : TStrings        read fParamsNames;
  end;

implementation

Uses SysUtils,
  mFX.Consts;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
destructor TFXSQLParamsParser.Destroy;
Begin
  fParamsNames.Free;
  inherited;
End;
{______________________________________________________________________________}
procedure TFXSQLParamsParser.Clear;
Begin
  if fParamsNames<>nil then
    fParamsNames.Clear;
  fQuery:=EmptyStr;
  inherited Clear;
End;
{______________________________________________________________________________}
procedure TFXSQLParamsParser.ExtractParams(Const Value:String);
Var s:Integer;
Begin
  Self.Clear;
  Self.Start(Value);
  Self.SkipSpace;
  fStart:=fIdx;
  while fIdx<=fLen do Begin
    case fChar^ of
      #9,#10,#13,' ':Begin
        fQuery:=fQuery+Copy(fLines,fStart,fIdx-fStart)+' ';
        Self.IncPos;
        Self.SkipSpace;
        fStart:=fIdx;
        end;
      '"':Begin
        if fStart<fIdx Then Begin
          fQuery:=fQuery+Copy(fLines,fStart,fIdx-fStart);
          fStart:=fIdx;
          end;
        ReadQuotedToken('"');
        Assert(fLines[fIdx]='"');
        Assert(fLines[fStart]='"');
        fQuery:=fQuery+Copy(fLines,fStart,fIdx-fStart+1);
        Self.IncPos;
        fStart:=fIdx;
        end;
      '''':Begin
        if fStart<fIdx Then Begin
          fQuery:=fQuery+Copy(fLines,fStart,fIdx-fStart);
          fStart:=fIdx;
          end;
        ReadQuotedToken('''');
        Assert(fLines[fIdx]='''');
        Assert(fLines[fStart]='''');
        fQuery:=fQuery+Copy(fLines,fStart,fIdx-fStart+1)+' ';
        Self.IncPos;
        fStart:=fIdx;
        end;
      '-':Begin
        if Self.IsKeyword(kwLineComment,s) then Begin
          if fStart<fIdx Then Begin
            fQuery:=fQuery+Copy(fLines,fStart,fIdx-fStart);
            fStart:=fIdx;
            end;
          SkipLineComment;
        end else
          Self.IncPos;
        end;
      '/':Begin
        if Self.IsKeyword(kwComment1Start,s) then Begin
          if fStart<fIdx Then Begin
            fQuery:=fQuery+Copy(fLines,fStart,fIdx-fStart);
            fStart:=fIdx;
            end;
          SkipComment1;
        end else
          Self.IncPos;
        end;
      ':','?':Begin
        if fStart<fIdx Then Begin
          fQuery:=fQuery+Copy(fLines,fStart,fIdx-fStart);
          fStart:=fIdx;
          end;
        ReadParamName;
        fTempoStr:=Copy(fLines,fStart+1,fIdx-fStart);
        Self.AddParam(fTempoStr);
        fQuery:=fQuery+'?';
        Self.IncPos;
        fStart:=fIdx;
        end;
      else Begin
        IncPos;
    end end end;
  if fStart<fIdx then
    fQuery:=fQuery+Copy(fLines,fStart,fIdx-fStart+1);
End;
{______________________________________________________________________________}
procedure TFXSQLParamsParser.AddParam(Const aParamName:String);
Var pn:String;
Begin
  if fParamsNames=nil then
    fParamsNames:=TStringList.Create;
  if aParamName='' then Begin
    pn:='AUTO_'+IntToStr(fParamsNames.Count);
  End else
    pn:=aParamName;
  fParamsNames.Add(pn);
End;

end.

