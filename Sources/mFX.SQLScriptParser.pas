unit mFX.SQLScriptParser;

interface

{$I mFX.Inc}
{$M-}

Uses System.Types, System.Classes, System.SysUtils,
  mFX.Classes, mFX.Parser;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  TFXSQLScriptParser = Class(TFXCustomParser)
  protected
    fCommand         : String;
    fHasCodeBlock    : Boolean;

    /// <SUMMARY>Read Bloc</SUMMARY>
    procedure ReadBloc;

    /// <SUMMARY>Parse Bloc</SUMMARY>
    procedure ParseBloc;
    /// <SUMMARY>Set Command</SUMMARY>
    procedure SkipCodeBlock;

    /// <summary>ReadTerminator</summary>
    procedure ReadTerminator;inline;
    /// <SUMMARY>Exec Query</SUMMARY>
    procedure ExecQueryBloc;virtual;abstract;

    /// <summary>Parse Comment1</summary>
    procedure ParseComment1;inline;
    /// <summary>Parse LineComment</summary>
    procedure ParseLineComment;inline;
    /// <SUMMARY>Set Command</SUMMARY>
    procedure PushComment;
    /// <SUMMARY>Exec Command</SUMMARY>
    procedure ExecCommentBloc;virtual;abstract;

  public
    /// <summary>Clear</summary>
    procedure Clear;override;
    /// <summary>Init</summary>
    procedure Init(Const Value:String;Const Parsed:Boolean;Const aTerm:Char);

    /// <SUMMARY>EOF</SUMMARY>
    function EOF:Boolean;inline;
    /// <summary>Read One Query</summary>
    procedure Read;

    property HasCodeBlock: Boolean         read fHasCodeBlock;
    property Command     : String          read fCommand;
  End;

implementation


{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSQLScriptParser.Clear;
Begin
  inherited Clear;
  fHasCodeBlock:=False;
  fCommand:='';
End;
{______________________________________________________________________________}
procedure TFXSQLScriptParser.Init(Const Value:String;Const Parsed:Boolean;Const aTerm:Char);
Begin
  Self.Clear;
  fLines:=Value;
  if Parsed then
    fCommand:=Value;
  fActualTerm:=aTerm;
  fChar:=PChar(fLines);
  fLen:=Length(fLines);
End;
{______________________________________________________________________________}
function TFXSQLScriptParser.EOF:Boolean;
Begin
  Result:=(fLen<=0)or(fIdx>fLen);
End;
{______________________________________________________________________________}
procedure TFXSQLScriptParser.ParseLineComment;
Begin
  //End Of Line
  Assert(fChar^='-');
  Assert((fChar+1)^='-');

  IncPos(2);
  while (fIdx<=fLen)and(not (CharInSet(fChar^,[#13,#10]))) do
    IncPos;
  PushComment;
  SkipEOL;
End;
{______________________________________________________________________________}
procedure TFXSQLScriptParser.ParseComment1;
Var EOC:Boolean;
    s:Integer;
Begin
  // Comment type /* ... */
  Assert(fChar^='/');
  Assert((fChar+1)^='*');

  IncPos(2);
  EOC:=False;
  while (fIdx<=fLen) do Begin
    if Self.IsKeyword(kwComment1End,s) then Begin
      EOC:=True;
      IncPos(s);
      Break;
      end;
    IncPos;
    end;
  if not EOC then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError,['Missing EndOfComment']);
  PushComment;
  SkipEOL;
End;
{______________________________________________________________________________}
procedure TFXSQLScriptParser.PushComment;
Var i,j:Integer;
Begin
  i:=fStart;
  j:=fIdx;
  While i<j do Begin
    Case fLines[i] of
      #0..#32:Inc(i);
      else Break;
    end end;
  While i<j do Begin
    Case fLines[j] of
      #0..#32:Dec(j);
      else Break;
    end end;
  if i<j then Begin
    fCommand:=Copy(fLines,i,j-i+1);
    Self.ExecCommentBloc;
    fCommand:='';
    end;
End;
{______________________________________________________________________________}
procedure TFXSQLScriptParser.SkipCodeBlock;
Var BeginCounter,s:Integer;
Begin
  // Begin .. End
  Inc(fChar,5);
  Inc(fIdx,5);
  BeginCounter:=1;
  fHasCodeBlock:=True;
  while fIdx<=fLen do Begin
    Case fChar^ of
      '-':Begin
        if Self.IsKeyword(kwLineComment,s) then Begin
          SkipLineComment;
          Continue;
        end end;
      '/':Begin
        if Self.IsKeyword(kwComment1Start,s) then Begin
          SkipComment1;
          Continue;
        end end;
      '"':Begin
        ReadQuotedToken('"');
        Self.IncPos;
        Continue;
        end;
      '''':Begin
        ReadQuotedToken('''');
        Self.IncPos;
        Continue;
        end;
      #9,#10,#13,' ':Begin
        SkipSpace;
        Continue;
        end;
      'B','b':Begin
        if Self.IsKeyword(kwBegin,s) then Begin
          Inc(BeginCounter);
          IncPos(s);
          Continue;
        end end;
      'E','e':Begin
        if Self.IsKeyword(kwEnd,s) then Begin
          Dec(BeginCounter);
          IncPos(s);
          if BeginCounter=0 then
            Break;
          Continue;
      end end end;
    IncPos
    end;
  if BeginCounter<>0 then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError,['Missing End'])
End;
{______________________________________________________________________________}
procedure TFXSQLScriptParser.ReadTerminator;
Var i,l:Integer;
    s:String;
Begin
  i:=1;
  l:=Length(fCommand);
  s:=Copy(fCommand,i,3);
  if not SameText(s,'SET') then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError,['Invalid Set Command '+fCommand]);

  Inc(i,3);
  While i<l do Begin
    Case fCommand[i] of
      #0..#32:Inc(i);
      else Break;
    end end;
  s:=Copy(fCommand,i,4);
  if not SameText(s,'TERM') then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError,['Invalid Set Command '+fCommand]);

  Inc(i,4);
  While i<l do Begin
    Case fCommand[i] of
      #0..#32:Inc(i);
      else Break;
    end end;
  if i>l then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError,['Invalid Set Command '+fCommand]);
  fActualTerm:=fCommand[i];
End;
{______________________________________________________________________________}
procedure TFXSQLScriptParser.ParseBloc;
Var kw:TFXSQLKeyWord;
    j,s:Integer;
Begin
  kw:=kwUnknownCmd;
  j:=fIdx;
  if j>fLen then
    j:=fLen;
  While fStart<=j do Begin
    Case fLines[fStart] of
      #0..#32:Begin
        Inc(fStart);
        end;
      'S','s':Begin
        if Self.IsKeyword(fStart,kwSelect,s) then Begin
          kw:=kwSelect;
          Break;
          end;
        if Self.IsKeyword(fStart,kwSetTerm,s) then Begin
          kw:=kwSetTerm;
          Break;
          end;
        if Self.IsKeyword(fStart,kwSetSQLDialect,s) then Begin
          kw:=kwSetSQLDialect;
          Break;
          end;
        Break;
        end;
      else Begin
        Break;
    end end end;
  if fStart>j then
    fStart:=j;

  While fStart<=j do Begin
    Case fLines[j] of
      #0..#32:Begin
        Dec(j);
        end;
      else Begin
        Break;
    end end end;
  if j<fStart then
    j:=fStart;

  case kw of
    kwSetTerm:Begin
      fCommand:=Copy(fLines,fStart,j-fStart+1);
      ReadTerminator;
      fCommand:='';
      end;
    kwSetSQLDialect:Begin
      fCommand:=Copy(fLines,fStart,j-fStart+1);
      Self.ExecCommentBloc;
      fCommand:='';
      end;
    else Begin
      fCommand:=Copy(fLines,fStart,j-fStart+1);
      Self.ExecQueryBloc;
      fCommand:='';
    end end;
End;
{______________________________________________________________________________}
procedure TFXSQLScriptParser.ReadBloc;
Var s:Integer;
Begin
  While fIdx<=fLen do Begin
    if fChar^=fActualTerm then Begin
      IncPos(-1);
      Self.ParseBloc;
      IncPos(2);
      SkipSpace;
      exit;
      End;
    Case fChar^ of
      '-':Begin
          if Self.IsKeyword(kwLineComment,s) then Begin
            SkipLineComment;
            Continue;
          end end;
      '/':Begin
          if Self.IsKeyword(kwComment1Start,s) then Begin
            SkipComment1;
            Continue;
          end end;
      '"':Begin
        ReadQuotedToken('"');
        Self.IncPos;
        Continue;
        end;
      '''':Begin
        ReadQuotedToken('''');
        Self.IncPos;
        Continue;
        end;
      'B','b':Begin
        if Self.IsKeyword(kwBegin,s) then Begin
          SkipCodeBlock;
          Continue;
      end end end;
    Self.IncPos;
    end;
  Self.ParseBloc;
  SkipSpace;
End;
{______________________________________________________________________________}
procedure TFXSQLScriptParser.Read;
Var s:Integer;
Begin
  SkipSpace;
  fStart:=fIdx;
  fHasCodeBlock:=False;
  if fIdx<=fLen then Begin
    Case fChar^ of
      '-':Begin
          if Self.IsKeyword(kwLineComment,s) then Begin
            ParseLineComment;
            SkipSpace;
            Exit;
          end end;
      '/':Begin
          if Self.IsKeyword(kwComment1Start,s) then Begin
            ParseComment1;
            SkipSpace;
            Exit;
      end end end;
    Self.ReadBloc;
    SkipSpace;
    end 
end;

end.

