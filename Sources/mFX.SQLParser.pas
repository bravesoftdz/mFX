unit mFX.SQLParser;

interface

{$I mFX.Inc}
{$M-}

Uses Types, Classes,
  mFX.List, mFX.Classes, mFX.Parser;

Type
  TFXSQLParser = Class;

  TFXSQLQueryToken = Class;
    TFXSQLFieldsToken = Class;
      TFXSQLFieldToken = Class;
    TFXSQLFromToken = Class;
    TFXSQLWhereToken = Class;
    TFXSQLGroupToken = Class;
    TFXSQLOrderToken = Class;
    TFXSQLHavingToken = Class;

  /// <summary>SQL Query Parser</summary>
  TFXCustomSQLParserToken = Class(TFXCustomParser)
  private
    fSQLParser       : TFXSQLParser;
    fOwner           : TFXCustomSQLParserToken;
    fStr             : String;
    fChilds          : TFXList;
    /// <summary>Extract Params</summary>
    procedure ExtractParams;
    /// <summary>Parse</summary>
    procedure Parse;virtual;
    /// <summary>As String</summary>
    function AsString:String;virtual;
    /// <summary>Add</summary>
    procedure Add(Const aChild:TFXCustomSQLParserToken);
    /// <summary>constructor </summary>
    constructor Create(Const aSQLParser:TFXSQLParser;Const aOwner:TFXCustomSQLParserToken);
  public
    /// <summary>destructor/summary>
    destructor Destroy; override;
  End;

  /// <summary>SQL Query Parser</summary>
  TFXSQLFieldsToken = Class(TFXCustomSQLParserToken)
  private
    /// <summary>Parse</summary>
    procedure Parse;override;
    /// <summary>As String</summary>
    function AsString:String;override;
  end;

  /// <summary>SQL Query Parser</summary>
  TFXSQLFieldToken = Class(TFXCustomSQLParserToken)
  private
    /// <summary>Parse</summary>
    procedure Parse;override;
  end;

  /// <summary>SQL Query Parser</summary>
  TFXSQLFromToken = Class(TFXCustomSQLParserToken)
  private
    /// <summary>Parse</summary>
    procedure Parse;override;
  end;

  /// <summary>SQL Query Parser</summary>
  TFXSQLWhereToken = Class(TFXCustomSQLParserToken)
  private
    /// <summary>Parse</summary>
    procedure Parse;override;
  end;

  TFXSQLGroupToken = Class(TFXCustomSQLParserToken)
  private
    /// <summary>Parse</summary>
    procedure Parse;override;
  End;

  TFXSQLOrderToken = Class(TFXCustomSQLParserToken)
  private
    /// <summary>Parse</summary>
    procedure Parse;override;
  End;

  TFXSQLHavingToken = Class(TFXCustomSQLParserToken)
  private
    /// <summary>Parse</summary>
    procedure Parse;override;
  End;

  /// <summary>SQL Query Parser</summary>
  TFXSQLQueryToken = Class(TFXCustomSQLParserToken)
  private
    /// <summary>Parse</summary>
    procedure Parse;override;
    /// <summary>Parse Select</summary>
    /// Select FIELD As Alias[,FIELD As Alias]
    /// FROM Tbl alias[,Tbl alias]
    /// Where Clause[LOGICALOP Clause]
    /// Group By
    /// Order By
    /// Having
    procedure ParseSelect;
    /// <summary>Parse Update</summary>
    ///  Update Table Set
    ///     Field = Value
    ///  Where
    procedure ParseUpdate;
    /// <summary>Parse Insert</summary>
    ///  Insert () Values ();
    procedure ParseInsert;
    /// <summary>Parse Delete</summary>
    ///  Delete From
    procedure ParseDelete;
    /// <summary>Parse DDL Comment</summary>
    procedure ParseDDLComment;
    /// <summary>Parse Unknown</summary>
    procedure ParseUnknown;
  End;

  /// <summary>SQL Query Parser</summary>
  TFXSQLParser = Class(TFXCustomParser)
  protected
    fRoot            : TFXSQLQueryToken;
    fCommandType     : TFXSQLKeyWord;
    fParamsNames     : TStrings;
    fFieldsNames     : TStrings;
    fPendingToken    : Boolean;
    fTokenStr        : String;
    fTokenKeyWord    : TFXSQLKeyWord;
    /// <summary>Skip Current Token</summary>
    procedure SkipCurrentToken;inline;
    /// <summary>Skip Tab,Space,... till Start Next token</summary>
    function NextToken:Boolean;overload;
    /// <summary>AddParam</summary>
    procedure AddParam(Const aToken:TFXCustomSQLParserToken);overload;
    /// <summary>AddParam</summary>
    procedure AddParam(Const aParamName:String);overload;
    /// <summary>AddField</summary>
    procedure AddField(Const aToken:TFXSQLFieldToken);

  public
    /// <summary>destructor/summary>
    destructor Destroy; override;

    /// <summary>Clear</summary>
    procedure Clear;override;
    /// <summary>Parse Query</summary>
    procedure ParseQuery(Const Value:String);
    /// <summary>Query</summary>
    function Query:String;
    /// <summary>Query</summary>
    function QueryType:TFXQueryType;

    property PendingToken: Boolean         read fPendingToken;
    property TokenStr    : String          read fTokenStr;
    property TokenKeyWord: TFXSQLKeyWord   read fTokenKeyWord;

    property CommandType : TFXSQLKeyWord   read fCommandType;
    property ParamsNames : TStrings        read fParamsNames;
    property FieldsNames : TStrings        read fFieldsNames;
  end;

implementation

Uses SysUtils,
  mFX.Consts;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
destructor TFXSQLParser.Destroy;
Begin
  fParamsNames.Free;
  fFieldsNames.Free;
  fRoot.Free;
  inherited;
End;
{______________________________________________________________________________}
procedure TFXSQLParser.Clear;
Begin
  fBlocStr:='';
  fTokenStr:='';
  fPendingToken:=False;
  fTokenKeyWord:=kwInvalidQuery;
  fCommandType:=kwInvalidQuery;
  if fParamsNames<>nil then
    fParamsNames.Clear;
  if fFieldsNames<>nil then
    fFieldsNames.Clear;
  if fRoot<>nil then Begin
    if fRoot.fChilds<>nil then
      fRoot.fChilds.Clear;
    fRoot.fStr:='';
    fRoot.Clear;
    end;
  inherited Clear;
End;
{______________________________________________________________________________}
procedure TFXSQLParser.ParseQuery(Const Value:String);
Begin
  if fRoot=nil then
    fRoot:=TFXSQLQueryToken.Create(Self,nil);
  Start(Value);
  fRoot.Parse
End;
{______________________________________________________________________________}
function TFXSQLParser.Query:String;
Begin
  if fRoot=nil then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError,['Invalid Query']);
  result:=fRoot.AsString;
End;
{______________________________________________________________________________}
function TFXSQLParser.QueryType:TFXQueryType;
Begin
  Case fCommandType of
    kwInsertReturning,kwUpdateReturning,kwDeleteReturning,
    kwSelect      :Result:=fxSQLSelectSelect;
    kwInsert      :Result:=fxSQLInsert;
    kwUpdate      :Result:=fxSQLUpdate;
    kwDelete      :Result:=fxSQLDelete;
    kwSetGenerator:Result:=fxSQLSetGenerator;
    kwDDLCommentOn:Result:=fxSQLDDLComment;
    kwUnknownCmd  :Result:=fxSQLUnknown;
    else Begin
      Self.Clear;
      Result:=fxSQLUnknown;
      FXRaiseClientErrorFmt(Self,fxceSQLParseError,['Invalid Query'])
    end end;
End;
{______________________________________________________________________________}
procedure TFXSQLParser.AddParam(Const aToken:TFXCustomSQLParserToken);
Var pn:String;
Begin
  if fParamsNames=nil then
    fParamsNames:=TStringList.Create;
  if aToken.fStr='' then Begin
    pn:='AUTO_'+IntToStr(fParamsNames.Count);
  End else
    pn:=aToken.fStr;
  fParamsNames.Add(pn);
End;
{______________________________________________________________________________}
procedure TFXSQLParser.AddParam(Const aParamName:String);
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
{______________________________________________________________________________}
procedure TFXSQLParser.AddField(Const aToken:TFXSQLFieldToken);
Begin
  if fFieldsNames=nil then
    fFieldsNames:=TStringList.Create;
  fFieldsNames.Add(aToken.fStr);
End;
{______________________________________________________________________________}
procedure TFXSQLParser.SkipCurrentToken;
Begin
  Assert(fPendingToken);
  fPendingToken:=False;
End;
{______________________________________________________________________________}
function TFXSQLParser.NextToken:Boolean;
Var i,s:Integer;
Begin
  Assert(fStart=fIdx);
  Assert(Not fPendingToken);

  while fIdx<=fLen do Begin
    case fChar^ of
      #9,#10,#13,' ':Begin
        SkipSpace;
        Continue;
      end end;
    Break;
    end;

  fStart:=fIdx;
  while fIdx<=fLen do Begin
    case fChar^ of
      '"':Begin
        ReadQuotedToken('"');
        Assert(fLines[fIdx]='"');
        Assert(fLines[fStart]='"');
        fTokenStr:=Copy(fLines,fStart,fIdx-fStart+1);
        fTokenKeyWord:=kwDoubleQuotedToken;
        fPendingToken:=True;
        Self.IncPos;
        fStart:=fIdx;
        Result:=True;
        exit;
        end;
      '''':Begin
        ReadQuotedToken('''');
        Assert(fLines[fIdx]='''');
        Assert(fLines[fStart]='''');
        fTokenStr:=Copy(fLines,fStart,fIdx-fStart+1);
        fTokenKeyWord:=kwDoubleQuotedToken;
        fPendingToken:=True;
        Self.IncPos;
        fStart:=fIdx;
        Result:=True;
        exit;
        end;
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
    Break;
    end;
    
  if fIdx<=fLen Then Begin
    //Singletion
    case fChar^ of
      '.':Begin
        fTokenKeyWord:=kwPunt;
        fPendingToken:=True;
        fTokenStr:='.';
        IncPos;
        fStart:=fIdx;
        Result:=True;
        exit;
        end;
      ',':Begin
        fTokenKeyWord:=kwCommat;
        fPendingToken:=True;
        fTokenStr:=',';
        IncPos;
        fStart:=fIdx;
        Result:=True;
        exit;
      end end;
    //Find Token Last Char
    fStart:=fIdx;
    while fIdx<=fLen do Begin
      case fChar^ of
        #9,#10,#13,' ',',':Begin
          IncPos(-1);
          Break;
        end end;
      if fIdx=fLen then
        Break;
      IncPos
      end;
    Assert(fStart<=fIdx);
    fPendingToken:=True;
    fTokenStr:=Copy(fLines,fStart,fIdx-fStart+1);
    Case fLines[fStart] of
      'C','c':Begin
        if SameText(fTokenStr,'COMMENT') then Begin
          fTokenKeyWord:=kwDDLCommentOn;
          Self.IncPos;
          fStart:=fIdx;
          Result:=True;
          exit;
        end end;
      'F','f':Begin
        if SameText(fTokenStr,'FROM') then Begin
          fTokenKeyWord:=kwFromTbls;
          Self.IncPos;
          fStart:=fIdx;
          Result:=True;
          exit;
        end end;
      'A','a':Begin
        if SameText(fTokenStr,'AS') then Begin
          fTokenKeyWord:=kwAs;
          Self.IncPos;
          fStart:=fIdx;
          Result:=True;
          exit;
        end end;
      'J','j':Begin
        if SameText(fTokenStr,'JOIN') then Begin
          fTokenKeyWord:=kwJoinTbl;
          Self.IncPos;
          fStart:=fIdx;
          Result:=True;
          exit;
        end end;
      'L','l':Begin
        if SameText(fTokenStr,'LEFT') then Begin
          i:=fStart+4;
          if NextKeyword('JOIN',i) then Begin
            fTokenStr:=Copy(fLines,fStart,fIdx-fStart+i+1);
            fTokenKeyWord:=kwJoinTbl;
            Self.IncPos(i+1);
            fStart:=fIdx;
            Result:=True;
            exit;
        end end end;
      'R','r':Begin
        if SameText(fTokenStr,'RIGHT') then Begin
          i:=fStart+5;
          if NextKeyword('JOIN',i) then Begin
            fTokenStr:=Copy(fLines,fStart,fIdx-fStart+i+1);
            fTokenKeyWord:=kwJoinTbl;
            Self.IncPos(i+1);
            fStart:=fIdx;
            Result:=True;
            exit;
        end end end;
      'W','w':Begin
        if SameText(fTokenStr,'WHERE') then Begin
          fTokenKeyWord:=kwWhere;
          Self.IncPos;
          fStart:=fIdx;
          Result:=True;
          exit;
        end end;
      'O','o':Begin
        if SameText(fTokenStr,'ORDER') then Begin
          i:=fStart+5;
          if NextKeyword('BY',i) then Begin
            fTokenStr:=Copy(fLines,fStart,fIdx-fStart+i+1);
            fTokenKeyWord:=kwOrderBy;
            Self.IncPos(i+1);
            fStart:=fIdx;
            Result:=True;
            exit;
        end end end;
      'G','g':Begin
        if SameText(fTokenStr,'GROUP') then Begin
          i:=fStart+5;
          if NextKeyword('BY',i) then Begin
            fTokenStr:=Copy(fLines,fStart,fIdx-fStart+i+1);
            fTokenKeyWord:=kwGroupBy;
            Self.IncPos(i+1);
            fStart:=fIdx;
            Result:=True;
            exit;
        end end end;
      'H','h':Begin
        if SameText(fTokenStr,'HAVING') then Begin
          fTokenKeyWord:=kwHaving;
          Self.IncPos;
          fStart:=fIdx;
          Result:=True;
          exit;
        end end;
      'S','s':Begin
        if SameText(fTokenStr,'SELECT') then Begin
          fTokenKeyWord:=kwSelect;
          Self.IncPos;
          fStart:=fIdx;
          Result:=True;
          exit;
        end end;
      'I','i':Begin
        if SameText(fTokenStr,'INSERT') then Begin
          fTokenKeyWord:=kwInsert;
          Self.IncPos;
          fStart:=fIdx;
          Result:=True;
          exit;
        end end;
      'U','u':Begin
        if SameText(fTokenStr,'UPDATE') then Begin
          fTokenKeyWord:=kwUpdate;
          Self.IncPos;
          fStart:=fIdx;
          Result:=True;
          exit;
        end end;
      'D','d':Begin
        if SameText(fTokenStr,'DELETE') then Begin
          fTokenKeyWord:=kwDelete;
          Self.IncPos;
          fStart:=fIdx;
          Result:=True;
          exit;
      end end end;
    fTokenKeyWord:=kwUnknownCmd;
    Self.IncPos;
    fStart:=fIdx;
    Result:=True;
  end else
    Result:=False
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXCustomSQLParserToken.Create(Const aSQLParser:TFXSQLParser;Const aOwner:TFXCustomSQLParserToken);
Begin
  fSQLParser:= aSQLParser;
  if aOwner<>nil then
    fOwner  := aOwner else
    fOwner  := Self;
End;
{______________________________________________________________________________}
destructor TFXCustomSQLParserToken.Destroy;
Begin
  fChilds.Free;
  inherited;
End;
{______________________________________________________________________________}
procedure TFXCustomSQLParserToken.Add(Const aChild:TFXCustomSQLParserToken);
Begin
  if fChilds=nil then
    fChilds:=TFXList.Create;
  fChilds.Add(aChild);
End;
{______________________________________________________________________________}
procedure TFXCustomSQLParserToken.Parse;
Begin
  FXRaiseClientErrorFmt(Self,fxceSQLParseError, ['Internal Error']);
End;
{______________________________________________________________________________}
procedure TFXCustomSQLParserToken.ExtractParams;
Var s:Integer;
Begin
  Self.SkipSpace;
  fStart:=fIdx;
  while fIdx<=fLen do Begin
    case fChar^ of
      #9,#10,#13,' ':Begin
        fStr:=fStr+Copy(fLines,fStart,fIdx-fStart)+' ';
        Self.IncPos;
        Self.SkipSpace;
        fStart:=fIdx;
        end;
      '"':Begin
        if fStart<fIdx Then Begin
          fStr:=fStr+Copy(fLines,fStart,fIdx-fStart);
          fStart:=fIdx;
          end;
        ReadQuotedToken('"');
        Assert(fLines[fIdx]='"');
        Assert(fLines[fStart]='"');
        fStr:=fStr+Copy(fLines,fStart,fIdx-fStart+1);
        Self.IncPos;
        fStart:=fIdx;
        end;
      '''':Begin
        if fStart<fIdx Then Begin
          fStr:=fStr+Copy(fLines,fStart,fIdx-fStart);
          fStart:=fIdx;
          end;
        ReadQuotedToken('''');
        Assert(fLines[fIdx]='''');
        Assert(fLines[fStart]='''');
        fStr:=fStr+Copy(fLines,fStart,fIdx-fStart+1)+' ';
        Self.IncPos;
        fStart:=fIdx;
        end;
      '-':Begin
        if Self.IsKeyword(kwLineComment,s) then Begin
          if fStart<fIdx Then Begin
            fStr:=fStr+Copy(fLines,fStart,fIdx-fStart);
            fStart:=fIdx;
            end;
          SkipLineComment;
        end else
          Self.IncPos;
        end;
      '/':Begin
        if Self.IsKeyword(kwComment1Start,s) then Begin
          if fStart<fIdx Then Begin
            fStr:=fStr+Copy(fLines,fStart,fIdx-fStart);
            fStart:=fIdx;
            end;
          SkipComment1;
        end else
          Self.IncPos;
        end;
      ':','?':Begin
        if fStart<fIdx Then Begin
          fStr:=fStr+Copy(fLines,fStart,fIdx-fStart);
          fStart:=fIdx;
          end;
        ReadParamName;
        fTempoStr:=Copy(fLines,fStart+1,fIdx-fStart);
        fSQLParser.AddParam(fTempoStr);
        fStr:=fStr+'?';
        Self.IncPos;
        fStart:=fIdx;
        end;
      else Begin
        IncPos;
    end end end;
  if fStart<fIdx then
    fStr:=fStr+Copy(fLines,fStart,fIdx-fStart+1);
End;
{______________________________________________________________________________}
function TFXCustomSQLParserToken.AsString:String;
Var i:Integer;
Begin
  result:=fStr;
  if (fChilds<>nil)and(fChilds.Count>0) then Begin
    for i:=0 to Pred(fChilds.Count) do Begin
      result:=result+CRLF+TFXCustomSQLParserToken(fChilds[i]).AsString;
    end end;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSQLQueryToken.Parse;
Begin
  //Skip Space, Comment, ...
  if not fSQLParser.NextToken then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError, ['Empty Query']);
  case fSQLParser.fTokenKeyWord of
    kwSelect      :Self.ParseSelect;
    kwUpdate      :Self.ParseUpdate;
    kwInsert      :Self.ParseInsert;
    kwDelete      :Self.ParseDelete;
    kwDDLCommentOn:Self.ParseDDLComment;
    else           Self.ParseUnknown;
    end;
End;
{______________________________________________________________________________}
procedure TFXSQLQueryToken.ParseSelect;
Var token:TFXCustomSQLParserToken;
Begin
  fStr:=fSQLParser.fTokenStr;
  fSQLParser.SkipCurrentToken;
  fSQLParser.fCommandType:=kwSelect;

  // Select FIELD As Alias[,FIELD As Alias] ....................................
  token:=TFXSQLFieldsToken.Create(fSQLParser,Self);
  Self.Add(token);
  token.Parse;

  // FROM Tbl alias[,Tbl alias] ................................................
  if (Not fSQLParser.NextToken)or(fSQLParser.TokenKeyWord<>kwFromTbls) then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError, ['No From Tbls']);
  token:=TFXSQLFromToken.Create(fSQLParser,Self);
  Self.Add(token);
  token.Parse;

  // Where Clause[LOGICALOP Clause].............................................
  if (fSQLParser.PendingToken or fSQLParser.NextToken)and(fSQLParser.TokenKeyWord=kwWhere) then Begin
    token:=TFXSQLWhereToken.Create(fSQLParser,Self);
    Self.Add(token);
    token.Parse;
    end;

  // Group By ..................................................................
  if (fSQLParser.PendingToken or fSQLParser.NextToken)and(fSQLParser.TokenKeyWord=kwGroupBy) then Begin
    token:=TFXSQLGroupToken.Create(fSQLParser,Self);
    Self.Add(token);
    token.Parse;
    end;

  // Order By...................................................................
  if (fSQLParser.PendingToken or fSQLParser.NextToken)and(fSQLParser.TokenKeyWord=kwOrderBy) then Begin
    token:=TFXSQLOrderToken.Create(fSQLParser,Self);
    Self.Add(token);
    token.Parse;
    end;

  // Having  ...................................................................
  if (fSQLParser.PendingToken or fSQLParser.NextToken)and(fSQLParser.TokenKeyWord=kwHaving) then Begin
    token:=TFXSQLHavingToken.Create(fSQLParser,Self);
    Self.Add(token);
    token.Parse;
    end;

  if (fSQLParser.PendingToken or fSQLParser.NextToken) then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError, ['Invalid Select Query '+fSQLParser.TokenStr]);

  Self.Clear;
End;
{______________________________________________________________________________}
procedure TFXSQLQueryToken.ParseUpdate;
Begin
  fSQLParser.SkipCurrentToken;
  fSQLParser.fCommandType:=kwUpdate;

  fStr:='';
  Self.Start(fSQLParser.fLines);
  Self.ExtractParams;
  Self.Clear;
end;
{______________________________________________________________________________}
procedure TFXSQLQueryToken.ParseInsert;
Begin
  fSQLParser.SkipCurrentToken;
  fSQLParser.fCommandType:=kwInsert;

  fStr:='';
  Self.Start(fSQLParser.fLines);
  Self.ExtractParams;
  Self.Clear;
end;
{______________________________________________________________________________}
procedure TFXSQLQueryToken.ParseDelete;
Begin
  fSQLParser.SkipCurrentToken;
  fSQLParser.fCommandType:=kwDelete;

  fStr:='';
  Self.Start(fSQLParser.fLines);
  Self.ExtractParams;
  Self.Clear;
end;
{______________________________________________________________________________
procedure TFXSQLQueryToken.ParseSetAssigment;
Begin
  // Set Term
  // Set Generator

End;
{______________________________________________________________________________}
procedure TFXSQLQueryToken.ParseDDLComment;
Begin
  // Comment On Table XXXX is YYYYYYY;
  fStr:=fSQLParser.fTokenStr;
  fSQLParser.fCommandType:=kwDDLCommentOn;
  fSQLParser.SkipCurrentToken;
  fStr:=fSQLParser.fLines;
  Self.Clear;
End;
{______________________________________________________________________________}
procedure TFXSQLQueryToken.ParseUnknown;
Begin
  fStr:=fSQLParser.fTokenStr;
  fSQLParser.fCommandType:=kwUnknownCmd;
  fSQLParser.SkipCurrentToken;
  fStr:=fSQLParser.fLines;
  Self.Clear;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXSQLFieldsToken.AsString:String;
Var i:Integer;
Begin
  if (fChilds<>nil)and(fChilds.Count>0) then Begin
    result:=              '        '+(fChilds[0] as TFXSQLFieldToken).AsString;
    for i:=1 to Pred(fChilds.Count) do
      result:=result+CRLF+'       ,'+(fChilds[i] as TFXSQLFieldToken).AsString;
  End else
    result:='';
End;
{______________________________________________________________________________}
procedure TFXSQLFieldsToken.Parse;
Var Field:TFXSQLFieldToken;
Begin
  Self.Clear;
  if not fSQLParser.ReadBloc(kwFromTbls) then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError, ['No Fields']);
  Self.Start(fSQLParser.fBlocStr);
  if not Self.ReadBloc(kwCommat) then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError, ['No Fields']);

  repeat  Field:=TFXSQLFieldToken.Create(fSQLParser,Self);
          Self.Add(Field);
          Field.Parse;
          Self.SkipTerminator(kwCommat);
          if Self.ReadBloc(kwCommat) then
            Continue;
          Break;
  until (False);
  Self.Clear;
End;
{______________________________________________________________________________}
procedure TFXSQLFieldToken.Parse;
Begin
  Self.Start(fOwner.fBlocStr);
  Self.ExtractParams;
  fSQLParser.AddField(Self);
  Self.Clear;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSQLFromToken.Parse;
Var t:TFXSQLKeyWord;
Begin
  Self.Clear;
  fStr:='From ';
  fSQLParser.SkipCurrentToken;
  if not fSQLParser.ReadBloc([kwWhere,kwGroupBy,kwOrderBy,kwHaving],t) Then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError, ['No From Tbls']);

  Self.Start(fSQLParser.fBlocStr);
  ExtractParams;
  Self.Clear;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSQLWhereToken.Parse;
Var t:TFXSQLKeyWord;
Begin
  Self.Clear;
  fStr:='Where ';
  fSQLParser.SkipCurrentToken;
  if not fSQLParser.ReadBloc([kwGroupBy,kwOrderBy,kwHaving],t) Then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError, ['No Where Clause']);

  Self.Start(fSQLParser.fBlocStr);
  ExtractParams;
  Self.Clear;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSQLGroupToken.Parse;
Begin
  fStr:='Group By';
  repeat  fSQLParser.SkipCurrentToken;
          if not fSQLParser.NextToken then
            FXRaiseClientErrorFmt(Self,fxceSQLParseError, ['Invalid Group By']);

          Case fSQLParser.TokenKeyWord of
            kwOrderBy,kwHaving:Begin
              Break;
              End;
            kwUnknownCmd:Begin
              fStr   := fStr +' '+fSQLParser.TokenStr;
              fSQLParser.SkipCurrentToken;
              if not fSQLParser.NextToken then
                Break;
              if fSQLParser.TokenKeyWord<>kwCommat then
                Break;
              fStr   := fStr +' ,';
              Continue;
              end;
            else Begin
              FXRaiseClientErrorFmt(Self,fxceSQLParseError, ['Invalid Group By']);
            end end;
  until (False);
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSQLOrderToken.Parse;
Var t:TFXSQLKeyWord;
Begin
  Self.Clear;
  fStr:='Order By ';
  fSQLParser.SkipCurrentToken;
  if not fSQLParser.ReadBloc([kwHaving],t) Then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError, ['No Order By Clause']);
  fStr:=fStr+fSQLParser.fBlocStr;
  Self.Clear;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXSQLHavingToken.Parse;
Var t:TFXSQLKeyWord;
Begin
  Self.Clear;
  fStr:='Having ';
  fSQLParser.SkipCurrentToken;
  if not fSQLParser.ReadBloc([],t) Then
    FXRaiseClientErrorFmt(Self,fxceSQLParseError, ['No Having Clause']);

  Start(fSQLParser.fBlocStr);
  ExtractParams;
End;

end.

