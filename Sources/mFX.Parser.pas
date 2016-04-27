unit mFX.Parser;

interface

{$I mFX.Inc}
{$M-}

Uses Types, Classes, SysUtils,
  mFX.List, mFX.Classes;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  /// <summary>SQL Script KeyWord</summary>
  TFXSQLKeyWord = (
    kwPunt,
    kwCommat,
    kwLineComment,    // --
    kwComment1Start,  // /*
    kwComment1End,    // */
    kwDoubleQuote,
    kwSingleQuote,
    kwBegin,
    kwEnd,
    kwSetTerm,
    kwSetSQLDialect,
    kwSelect,
    kwInsert,
    kwInsertReturning,
    kwUpdate,
    kwUpdateReturning,
    kwDelete,
    kwDeleteReturning,
    kwDDLCommentOn,
    kwSetGenerator,
    kwSingleQuotedToken,
    kwDoubleQuotedToken,
    kwBrakedBlock,
    kwParam,
    kwCast,
    kwAs,
    kwFromTbls,
    kwJoinTbl,
    kwWhere,
    kwAnd,
    kwOr,
    kwGroupBy,
    kwOrderBy,
    kwHaving,
    kwUnknownCmd,
    kwInvalidQuery
    );
  TFXSQLKeyWords = Set Of TFXSQLKeyWord;

  /// <summary>SQL Query Parser</summary>
  TFXCustomParser = Class(TObject)
  protected
    fLines           : String;
    fStart           : Integer;
    fIdx             : Integer;
    fChar            : PChar;
    fLen             : Integer;
    fTempoStr        : String;
    fBlocStr         : String;
    fActualTerm      : Char;
    /// <summary>Start</summary>
    procedure Start(Const Value:String);inline;
    /// <summary>Is EOF</summary>
    function EOF:Boolean;overload;inline;
    /// <summary>Seek Next Char</summary>
    procedure IncPos(Const Count:Integer=1);inline;
    /// <summary>Is Space</summary>
    function IsSpace(Const aIdx:Integer):Boolean;overload;inline;
    /// <summary>Next Keyword</summary>
    function NextKeyword(Const Value:String;Var aIdx:Integer):Boolean;overload;
    /// <summary>Is Keyword</summary>
    function IsKeyword(Const aKeyword:TFXSQLKeyWord;Out Size:Integer):Boolean;overload;inline;
    /// <summary>Is Keyword</summary>
    function IsKeyword(Const WantedKeywords:TFXSQLKeyWords;Out Terminator:TFXSQLKeyWord;Out Size:Integer):Boolean;overload;inline;
    /// <summary>Is Keyword</summary>
    function IsKeyword(Const aIdx:Integer;Const aKeyword:TFXSQLKeyWord;Out Size:Integer):Boolean;overload;inline;
    /// <summary>Is Keyword</summary>
    function Keyword(Out Keyword:TFXSQLKeyWord):Boolean;overload;inline;
    /// <summary>Skip EOL</summary>
    procedure SkipEOL;inline;
    /// <summary>Skip Tab,Space,... till Start Next token</summary>
    procedure SkipSpace;inline;
    /// <summary>Skip Comment1</summary>
    procedure SkipComment1;inline;
    /// <summary>Skip LineComment</summary>
    procedure SkipLineComment;inline;
    /// <summary>Read DoubleQuote token</summary>
    procedure ReadQuotedToken(Const aChar:Char);inline;
    /// <summary>Read Braked Bloc</summary>
    procedure ReadBrakedBloc(Const aOpen,aClose:Char);inline;
    /// <summary>Read DoubleQuote token</summary>
    procedure ReadParamName;inline;
    /// <summary>Read Bloc</summary>
    function SkipTerminator(Const Terminator:TFXSQLKeyWord):Boolean;overload;
    /// <summary>Read Bloc</summary>
    function ReadBloc(Const Terminator:TFXSQLKeyWord):Boolean;overload;
    /// <summary>Read DoubleQuote token</summary>
    function ReadBloc(Const Terminators:TFXSQLKeyWords;Out Terminator:TFXSQLKeyWord):Boolean;overload;

  public
    /// <summary>Clear</summary>
    procedure Clear;virtual;

  End;


implementation

Uses mFX.Consts;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXCustomParser.Clear;
Begin
  fTempoStr:='';
  fLines:='';
  fChar:=nil;
  fStart:=1;
  fIdx:=1;
End;
{______________________________________________________________________________}
procedure TFXCustomParser.Start(Const Value:String);
Begin
  fTempoStr:='';

  fIdx:=1;
  fStart:=1;
  fLines:=Value;
  fChar:=PChar(fLines);
  fLen:=Length(fLines);
End;
{______________________________________________________________________________}
function TFXCustomParser.EOF:Boolean;
Begin
  Result:=(fIdx>fLen)
End;
{______________________________________________________________________________}
procedure TFXCustomParser.IncPos(Const Count:Integer=1);
Begin
  Inc(fChar,Count);
  Inc(fIdx,Count);
End;
{______________________________________________________________________________}
function TFXCustomParser.IsSpace(Const aIdx:Integer):Boolean;
Begin
  if aIdx<=fLen then Begin
    case fLines[aIdx] of
      #9,#10,#13,' ':Begin
        Result:=True;
        end;
      else begin
        Result:=(fLines[aIdx]=fActualTerm);
      end end;
  End else
    Result:=True
End;
{______________________________________________________________________________}
function TFXCustomParser.NextKeyword(Const Value:String;Var aIdx:Integer):Boolean;
Var i,l:Integer;
    s:String;
Begin
  //Bypass Space
  i:=aIdx;
  aIdx:=0;
  while i<=fLen do Begin
    case fLines[i] of
      #9,#10,#13,' ':Begin
        Inc(aIdx);
        Inc(i);
        Continue;
        end;
      else Begin
        Break;
    end end end;
  //Get
  if i<=fLen then Begin
    l:=Length(Value);
    if Self.IsSpace(i+l) then Begin
      s:=Copy(fLines,i,l);
      if SameText(s,Value) then Begin
        Inc(aIdx,l);
        Result:=True;
      End else
        Result:=False;
    End else
      Result:=False;
  End else
    Result:=False;
End;
{______________________________________________________________________________}
function TFXCustomParser.IsKeyword(Const aKeyword:TFXSQLKeyWord;Out Size:Integer):Boolean;
Var NextChar:PChar;
    i:Integer;
Begin
  case aKeyword of
    kwLineComment:Begin
      case fChar^ of
        '-':Begin
            if (fIdx<fLen) then Begin
              NextChar:=fChar;
              Inc(NextChar);
              if (NextChar^='-') then Begin
                Result:=True;
                Size:=2;
                exit;
      end end end end end;
    kwComment1Start:Begin
      case fChar^ of
        '/':Begin
            if (fIdx<fLen) then Begin
              NextChar:=fChar;
              Inc(NextChar);
              if (NextChar^='*') then Begin
                Result:=True;
                Size:=2;
                exit;
      end end end end end;
    kwComment1End:Begin
      case fChar^ of
        '*':Begin
            if (fIdx<fLen) then Begin
              NextChar:=fChar;
              Inc(NextChar);
              if (NextChar^='/') then Begin
                Result:=True;
                Size:=2;
                exit;
      end end end end end;
    kwBegin:Begin
      case fChar^ of
        'B','b':Begin
          if Self.IsSpace(fIdx+5) then Begin
            fTempoStr:=Copy(fLines,fIdx,5);
            if SameText(fTempoStr,'BEGIN') then Begin
              Result:=True;
              Size:=5;
              exit;
      end end end end end;
    kwEnd:Begin
      case fChar^ of
        'E','e':Begin
          if Self.IsSpace(fIdx+3) then Begin
            fTempoStr:=Copy(fLines,fIdx,3);
            if SameText(fTempoStr,'END') then Begin
              Result:=True;
              Size:=3;
              exit;
      end end end end end;
    kwFromTbls:Begin
      case fChar^ of
        'F','f':Begin
          if Self.IsSpace(fIdx+4) then Begin
            fTempoStr:=Copy(fLines,fIdx,4);
            if SameText(fTempoStr,'FROM') then Begin
              Result:=True;
              Size:=4;
              exit;
      end end end end end;
    kwCommat:Begin
      case fChar^ of
        ',':Begin
          Result:=True;
          Size:=1;
          exit;
      end end end;
    kwPunt:Begin
      case fChar^ of
        '.':Begin
          Result:=True;
          Size:=1;
          exit;
      end end end;
    kwWhere:Begin
      case fChar^ of
        'W','w':Begin
          if Self.IsSpace(fIdx+5) then Begin
            fTempoStr:=Copy(fLines,fIdx,5);
            if SameText(fTempoStr,'WHERE') then Begin
              Result:=True;
              Size:=5;
              exit;
      end end end end end;
    kwGroupBy:Begin
      case fChar^ of
        'G','g':Begin
          if Self.IsSpace(fIdx+5) then Begin
            fTempoStr:=Copy(fLines,fIdx,5);
            if SameText(fTempoStr,'GROUP') then Begin
              i:=fIdx+5;
              if NextKeyword('BY',i) then Begin
                Result:=True;
                Size:=5+i;
                exit;
      end end end end end end;
    kwOrderBy:Begin
      case fChar^ of
        'O','o':Begin
          if Self.IsSpace(fIdx+5) then Begin
            fTempoStr:=Copy(fLines,fIdx,5);
            if SameText(fTempoStr,'ORDER') then Begin
              i:=fIdx+5;
              if NextKeyword('BY',i) then Begin
                Result:=True;
                Size:=5+i;
                exit;
      end end end end end end;
    kwHaving:Begin
      case fChar^ of
        'H','h':Begin
          if Self.IsSpace(fIdx+6) then Begin
            fTempoStr:=Copy(fLines,fIdx,6);
            if SameText(fTempoStr,'HAVING') then Begin
              Result:=True;
              Size:=6;
              exit;
      end end end end end;
    else Begin
      Assert(False);
    end end;
  Result:=False;
  Size:=0;
End;
{______________________________________________________________________________}
function TFXCustomParser.IsKeyword(Const WantedKeywords:TFXSQLKeyWords;Out Terminator:TFXSQLKeyWord;Out Size:Integer):Boolean;
Var t:TFXSQLKeyWord;
Begin
  if WantedKeywords<>[] then Begin
    for t:=low(t) to High(t) do Begin
      if (t in WantedKeywords)and(IsKeyword(t,Size)) then Begin
        Terminator:=t;
        Result:=True;
        exit;
    end end end;
  Terminator:=kwUnknownCmd;
  Result:=False;
  Size:=0;
End;
{______________________________________________________________________________}
function TFXCustomParser.IsKeyword(Const aIdx:Integer;Const aKeyword:TFXSQLKeyWord;Out Size:Integer):Boolean;
Var NextChar:Char;
    i1,i2:Integer;
Begin
  case aKeyword of
    kwComment1Start:Begin
      case fLines[aIdx] of
        '/':Begin
            if (aIdx<fLen) then Begin
              NextChar:=fLines[Succ(aIdx)];
              if (NextChar='*') then Begin
                Result:=True;
                Size:=2;
                exit;
      end end end end end;
    kwComment1End:Begin
      case fLines[aIdx] of
        '*':Begin
            if (fIdx<fLen) then Begin
              NextChar:=fLines[Succ(aIdx)];
              if (NextChar='/') then Begin
                Result:=True;
                Size:=2;
                exit;
      end end end end end;
    kwSelect:Begin
      if Self.IsSpace(aIdx+6) then Begin
        fTempoStr:=Copy(fLines,aIdx,6);
        if SameText(fTempoStr,'SELECT') then Begin
          Result:=True;
          Size:=6;
          exit;
      end end end;
    kwSetSQLDialect,
    kwSetTerm:Begin
      if Self.IsSpace(aIdx+3) then Begin
        fTempoStr:=Copy(fLines,aIdx,3);
        if SameText(fTempoStr,'SET') then Begin
          case aKeyword of
            kwSetTerm:Begin
              i1:=aIdx+3;
              if NextKeyword('TERM',i1) then Begin
                Result:=True;
                Size:=3+i1;
                exit;
              end end;
            kwSetSQLDialect:Begin
              i1:=aIdx+3;
              if NextKeyword('SQL',i1) then Begin
                i2:=aIdx+3+i1;
                if NextKeyword('DIALECT',i2) then Begin
                  Size:=3+i1+i2;
                  Result:=True;
                  exit;
      end end end end end end end;
    else Begin
      Assert(False);
    end end;
  Result:=False;
  Size:=0;
End;
{______________________________________________________________________________}
function TFXCustomParser.Keyword(Out Keyword:TFXSQLKeyWord):Boolean;
Var NextChar:PChar;
Begin
  case fChar^ of
    '"':Begin
      Keyword:=kwDoubleQuote;
      Result:=True;
      exit;
      end;
    '''':Begin
      Keyword:=kwSingleQuote;
      Result:=True;
      exit;
      end;
    '-':Begin
      if (fIdx<fLen) then Begin
        NextChar:=fChar;
        Inc(NextChar);
        if (NextChar^='-') then Begin
          Keyword:=kwLineComment;
          Result:=True;
          exit;
      end end end;
    '/':Begin
      if (fIdx<fLen) then Begin
        NextChar:=fChar;
        Inc(NextChar);
        if (NextChar^='*') then Begin
          Keyword:=kwComment1Start;
          Result:=True;
          exit;
    end end end end;
  Result:=False;
end;
{______________________________________________________________________________}
procedure TFXCustomParser.SkipEOL;
Begin
  while (fIdx<=fLen)and(CharInSet(fChar^,[#10,#13])) do
    IncPos;
End;
{______________________________________________________________________________}
procedure TFXCustomParser.SkipSpace;
Begin
  //SPACE SPACE SPACE
  while (fIdx<=fLen)and(CharInSet(fChar^,[#9,#10,#13,' '])) do
    IncPos;
end;
{______________________________________________________________________________}
procedure TFXCustomParser.ReadQuotedToken(Const aChar:Char);
Var EndQuoteFound:Boolean;
Begin
  //" ... "
  Assert(fChar^=aChar);
  Assert(fLines[fIdx]=aChar);

  IncPos;
  EndQuoteFound:=False;
  while fIdx<=fLen do Begin
    if fChar^=aChar then Begin
      if (fIdx<fLen)and(fLines[Succ(fIdx)]=aChar) Then Begin
        IncPos(2);
      end else Begin
        EndQuoteFound:=True;
        Break;
        end;
    end else Begin
      IncPos;
    end end;
  if not EndQuoteFound then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError,['Missing EndQuote <'+aChar+'>']);
  Assert(fLines[fIdx]=aChar);
End;
{______________________________________________________________________________}
procedure TFXCustomParser.ReadBrakedBloc(Const aOpen,aClose:Char);
Var EndBrakedFound:Integer;
Begin
  //" ... "
  Assert(fChar^=aOpen);
  Assert(fLines[fIdx]=aOpen);

  IncPos;
  EndBrakedFound:=1;
  while fIdx<=fLen do Begin
    if fChar^=aOpen then Begin
      Inc(EndBrakedFound);
      IncPos;
    End else
    if fChar^=aClose then Begin
      Dec(EndBrakedFound);
      if (EndBrakedFound=0) then
        Break;
      IncPos;
    end else Begin
      IncPos;
    end end;

  if EndBrakedFound<>0 then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError,['Missing End Braked <'+aClose+'>']);
  Assert(fLines[fIdx]=aClose);
End;
{______________________________________________________________________________}
procedure TFXCustomParser.ReadParamName;
Begin
  Assert(fChar^=fLines[fIdx]);
  Assert(CharInSet(fChar^,[':','?']));
  Assert(CharInSet(fLines[fIdx],[':','?']));

  IncPos;
  while fIdx<=fLen do Begin
    Case fChar^ of
     'A'..'Z', 'a'..'z', '0'..'9', '_', '$':Begin
       IncPos;
       end;
     else Begin
       IncPos(-1);
       Break;
    end end end;
End;
{______________________________________________________________________________}
function TFXCustomParser.SkipTerminator(Const Terminator:TFXSQLKeyWord):Boolean;
Var s:Integer;
Begin
  // Skip Space 
  SkipSpace;

  // Skip Terminator
  if IsKeyword(Terminator,s) then Begin
    Self.IncPos(s);
    Self.SkipSpace;
    Result:=True;
  end else
    Result:=False;
  fStart:=fIdx;
End;
{______________________________________________________________________________}
function TFXCustomParser.ReadBloc(Const Terminator:TFXSQLKeyWord):Boolean;
Var SavStart,s:Integer;
Begin
  // TODO Handle Select Select * from
  Assert(fStart=fIdx);

  // Skip Space 
  SkipSpace;

  // Goto Terminator
  fStart:=fIdx;
  SavStart:=fIdx;
  while fIdx<=fLen do Begin
    case fChar^ of
      '"':Begin
        ReadQuotedToken('"');
        Self.IncPos;
        Continue;
        End;
      '''':Begin
        ReadQuotedToken('''');
        Self.IncPos;
        Continue;
        End;
      '-':Begin
        if Self.IsKeyword(kwLineComment,s) then Begin
          SkipLineComment;
          Continue;
        end end;
      '/':Begin
        if Self.IsKeyword(kwComment1Start,s) then Begin
          SkipComment1;
          Continue;
      end end end;
    if IsKeyword(Terminator,s) then Begin
      fBlocStr:=Copy(fLines,SavStart,fIdx-SavStart);
      fStart:=fIdx;
      Result:=True;
      Exit;
      end;
    Self.IncPos;
    end;

  if (SavStart<fLen) then Begin
    fBlocStr:=Copy(fLines,SavStart,MaxInt);
    Self.IncPos;
    fStart:=fIdx;
    Result:=True;
  end else Begin
    fBlocStr:='';
    Result:=False;
    End;
End;
{______________________________________________________________________________}
function TFXCustomParser.ReadBloc(Const Terminators:TFXSQLKeyWords;Out Terminator:TFXSQLKeyWord):Boolean;
Var SavStart,s:Integer;
Begin
  // TODO Handle Select Select * from
  Assert(fStart=fIdx);

  // Skip Space
  SkipSpace;

  // Goto Terminators
  fStart:=fIdx;
  SavStart:=fIdx;
  while fIdx<=fLen do Begin
    case fChar^ of
      '"':Begin
        ReadQuotedToken('"');
        Self.IncPos;
        Continue;
        End;
      '''':Begin
        ReadQuotedToken('''');
        Self.IncPos;
        Continue;
        End;
      '-':Begin
        if Self.IsKeyword(kwLineComment,s) then Begin
          SkipLineComment;
          Continue;
        end end;
      '/':Begin
        if Self.IsKeyword(kwComment1Start,s) then Begin
          SkipComment1;
          Continue;
      end end end;
    if IsKeyword(Terminators,Terminator,s) then Begin
      fBlocStr:=Copy(fLines,SavStart,fIdx-SavStart);
      fStart:=fIdx;
      Result:=True;
      Exit;
      end;
    Self.IncPos;
    end;

  if (SavStart<=fLen) then Begin
    fBlocStr:=Copy(fLines,SavStart,MaxInt);
    Terminator:=kwUnknownCmd;
    Self.IncPos;
    fStart:=fIdx;
    Result:=True;
  end else Begin
    Terminator:=kwUnknownCmd;
    fBlocStr:='';
    Result:=False;
    End;
End;
{______________________________________________________________________________}
procedure TFXCustomParser.SkipLineComment;
Begin
  //End Of Line
  Assert(fChar^='-');
  Assert((fChar+1)^='-');

  IncPos(2);
  while (fIdx<=fLen)and(not (CharInSet(fChar^,[#13,#10]))) do
    IncPos;
  SkipEOL;
End;
{______________________________________________________________________________}
procedure TFXCustomParser.SkipComment1;
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
      SkipEOL;
      Break;
      end;
    IncPos;
    end;
  if not EOC then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError,['Missing EndOfComment'])
End;

end.

