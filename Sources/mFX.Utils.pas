unit mFX.Utils;

interface

{$I mFX.Inc}

Uses System.Types, System.Classes, System.Variants, System.SysUtils, System.Math, System.AnsiStrings, System.DateUtils, System.SysConst;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  TFXCharacterSet = (
    fxNoneAnsi,
    fxNoneOem,
    fxUTF8,
    fxUNICODE_FSS,
    fxISO8859_1,
    fxISO8859_2,
    fxISO8859_7,
    fxISO8859_8,
    fxSJIS_0208,
    fxBIG5
    );

function FXBool(Const Value:Boolean):String;inline;
function FXBool2Str(Const Value:Boolean):String;
Function FXStr2Bool(Const Value:String):Boolean;overload;inline;
Function FXVar2Bool(Const Value:Variant):Boolean;overload;inline;
Function FXVar2BoolDef(Const Value:Variant;Const aDef:Boolean):Boolean;overload;inline;

function FXFormatDate(Const Value:TDateTime):String;
function FXFormatTime(Const Value:TDateTime):String;
function FXFormatDateTime(Const Value:TDateTime):String;
function FXFormatFloat(Const FmtStr:String;Const Value:Currency):String;

function FXRandomString(Const iLength: Integer): String;
function FXRandomInteger(Const iLow, iHigh: Integer): Integer;

function FXFormatIdentifier(Const Value: String): String;
function FXExtractIdentifier(Const Value: String): String;

/// <summary>NoneCharSet</summary>
function NoneCharSet(Const Value:TFXCharacterSet): TFXCharacterSet;
/// <summary>NoneCharSet</summary>
function IsNoneCharSet(Const Value:TFXCharacterSet): Boolean;

/// <summary>Str2CharSet</summary>
function Str2CharSet(Const Value:String): TFXCharacterSet;
/// <summary>CharSet2Str</summary>
function CharSet2Str(Const Value:TFXCharacterSet):String;
/// <summary>CharSet2CodePage</summary>
function CharSet2CodePage(Const Value:TFXCharacterSet):Integer;
/// <summary>CharSetSize</summary>
function CharSetSize(Const Value:TFXCharacterSet):Integer;


implementation

Uses
  {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.ShellAPI, WinApi.Mapi,{$ENDIF}
  {$IFDEF LINUX}Libc,{$ENDIF}
  mFX.Consts;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function FXBool(Const Value:Boolean):String;
Begin
  if Value Then
    Result:=ibTrue else
    Result:=ibFalse
End;
{______________________________________________________________________________}
Function FXBool2Str(Const Value:Boolean):String;
Begin
  if Value Then
    Result:='TRUE' else
    Result:='FALSE'
End;
{______________________________________________________________________________}
Function FXStr2Bool(Const Value:String):Boolean;
Var s:String;
Begin
  s:=UpperCase(Trim(Value));
  if s<>EmptyStr then
    Result:=(CharInSet(s[1],['T','Y','O','1']))or(s='TRUE') else
    Result:= False
End;
{______________________________________________________________________________}
Function FXVar2BoolDef(Const Value:Variant;Const aDef:Boolean):Boolean;
Var vt:TVarType;
    Val:String;
Begin
  If VarIsNull(Value) or VarIsEmpty(Value) then Begin
    Result:=aDef
  end else Begin
    vt:=VarType(Value);
    Case vt of
      varBoolean:
        Result:=Value;
      varSmallInt, varInteger, varShortInt,
      varByte, varWord, varLongWord, varInt64, varUInt64:
        Result:=(Value<>0);
      varSingle, varDouble, varCurrency:
        Result:=(Value<>0);
      else Begin
        Val:=Value;
        Result:=mFX.Utils.FXStr2Bool(Val)
    end end end;
End;
{______________________________________________________________________________}
Function FXVar2Bool(Const Value:Variant):Boolean;
Begin
  Result:=FXVar2BoolDef(Value,False);
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function FXFormatDate(Const Value:TDateTime):String;
Var FormatSettings:TFormatSettings;
Begin
  if (Value<>0) then Begin
    FormatSettings.DateSeparator:='.';
    FormatSettings.TimeSeparator:=':';
    DateTimeToString(Result,'dd.mm.yyyy',Value,FormatSettings);
    Result:=''''+Result+''''
  end else
    result:='null'
end;
{______________________________________________________________________________}
function FXFormatTime(Const Value:TDateTime):String;
Var FormatSettings:TFormatSettings;
Begin
  if (Value<>0) then Begin
    FormatSettings.DateSeparator:='.';
    FormatSettings.TimeSeparator:=':';
    DateTimeToString(Result,'hh:nn:ss',Value,FormatSettings);
    Result:=''''+Result+''''
  end else
    result:='null'
end;
{______________________________________________________________________________}
function FXFormatDateTime(Const Value:TDateTime):String;
Var FormatSettings:TFormatSettings;
    d,t:TDateTime;
Begin
  FormatSettings.DateSeparator:='.';
  FormatSettings.TimeSeparator:=':';
  d:=Trunc(Value);
  t:=Frac(Value);
  if (d<>0)and(t<>0) then Begin
    DateTimeToString(Result,'dd.mm.yyyy hh:nn:ss',Value,FormatSettings);
    Result:=''''+Result+''''
  end else
  if (d<>0) then Begin
    DateTimeToString(Result,'dd.mm.yyyy',d,FormatSettings);
    Result:=''''+Result+''''
  end else
  if (t<>0) then Begin
    DateTimeToString(Result,'hh:nn:ss',t,FormatSettings);
    Result:=''''+Result+''''
  end else
    result:='null'
end;
{______________________________________________________________________________}
function FXFormatFloat(Const FmtStr:String;Const Value:Currency):String;
Var FormatSettings:TFormatSettings;
Begin
  FormatSettings.DecimalSeparator:='.';
  FormatSettings.ThousandSeparator:=#0;
  Result:=System.SysUtils.Format(FmtStr,[Value],FormatSettings)
end;
{______________________________________________________________________________}
function FXRandomString(Const iLength: Integer): String;
begin
  result := '';
  while Length(result) < iLength do
    result := result + IntToStr(FXRandomInteger(0, High(Integer)-1));
  if Length(result) > iLength then
    result := Copy(result, 1, iLength);
end;
{______________________________________________________________________________}
function FXRandomInteger(Const iLow, iHigh: Integer): Integer;
begin
  result := Random(iHigh - iLow +1) + iLow;
end;
{______________________________________________________________________________}
function FXFormatIdentifier(Const Value: String): String;
Var Len:Integer;
begin
  Assert(Value<> '');
  if (Value[1] = '"') then Begin
    Len:=Length(Value);
    if (Value[Len] = '"') then Begin
      Assert(Result=AnsiQuotedStr(Value,'"'));
      Result:=Value
    end else
      Result:= AnsiUpperCase(Value);
  end else
    Result:= AnsiUpperCase(Value);
end;
{______________________________________________________________________________}
function FXExtractIdentifier(Const Value: String): String;
Var Len:Integer;
begin
  Assert(Value<> '');
  if (Value[1] = '"') then Begin
    Len:=Length(Value);
    if (Value[Len] = '"') then Begin
      Result:=AnsiDequotedStr(Value,'"')
    end else
     Result := AnsiUpperCase(Value);
   end else
    Result := AnsiUpperCase(Value);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function NoneCharSet(Const Value:TFXCharacterSet): TFXCharacterSet;
Begin
  Case Value of
    fxNoneAnsi,
    fxNoneOem:
      Result:=Value;
    else
      Result:=fxNoneAnsi;
    end;
End;
{______________________________________________________________________________}
function IsNoneCharSet(Const Value:TFXCharacterSet): Boolean;
Begin
  Case Value of
    fxNoneAnsi,
    fxNoneOem:
      Result:=True;
    else
      Result:=False;
    end;
End;
{______________________________________________________________________________}
function Str2CharSet(Const Value:String): TFXCharacterSet;
Begin
  if SameText(Value,'UTF8') or SameText(Value,'UTF-8') then
    Result := fxUTF8 else
  if SameText(Value,'UNICODE_FSS') then
    Result := fxUNICODE_FSS else
  if(SameText(Value,'ISO8859_1'))or(SameText(Value,'ISO8859_15')) then
    Result := fxISO8859_1 else
  if SameText(Value,'ISO8859_2') then
    Result := fxISO8859_2 else
  if SameText(Value,'ISO8859_7') then
    Result := fxISO8859_7 else
  if SameText(Value,'ISO8859_8') then
    Result := fxISO8859_8 else
  if SameText(Value,'SJIS_0208') then
    Result := fxSJIS_0208 else
  if SameText(Value,'BIG_5') then
    Result := fxBIG5 else
  if SameText(Value,'ANSI') then
    Result := fxNoneAnsi else
  if SameText(Value,'OEM') then
    Result := fxNoneOEM else
    Result := fxNoneAnsi;
End;
{______________________________________________________________________________}
function CharSet2Str(Const Value:TFXCharacterSet):String;
Begin
  case Value of
    fxUTF8       :Result:='UTF8';
    fxUNICODE_FSS:Result:='UNICODE_FSS';
    fxISO8859_1  :Result:='ISO8859_1';
    fxISO8859_2  :Result:='ISO8859_2';
    fxISO8859_7  :Result:='ISO8859_7';
    fxISO8859_8  :Result:='ISO8859_8';
    fxSJIS_0208  :Result:='SJIS_0208';
    fxBIG5       :Result:='BIG_5';
    else          Result:='NONE';
    end;
End;
{______________________________________________________________________________}
function CharSet2CodePage(Const Value:TFXCharacterSet):Integer;
Begin
  case Value of
    fxUTF8       :Result:=65001;                                                // Only Unicode Firebird can handle
    fxUNICODE_FSS:Result:=65001;                                                // Deprecated
    fxISO8859_1  :Result:=1252;                                                 // Western and West European languages
    fxISO8859_2  :Result:=1250;                                                 // Central and East European languages (Czech, Polish, etc.)
    fxISO8859_7  :Result:=1253;                                                 // Modern Greek
    fxISO8859_8  :Result:=1255;                                                 // Hebrew
    fxSJIS_0208  :Result:=932;
    fxBIG5       :Result:=950;
    fxNoneOem    :Result:=CP_OEMCP;                                             // OEM  code page
    else          Result:=CP_ACP;                                               // ANSI code page
    end;
End;
{______________________________________________________________________________}
function CharSetSize(Const Value:TFXCharacterSet):Integer;
Begin
  case Value of
    fxUTF8       :Result:=1;                                                    // Only Unicode Firebird can handle
    else          Result:=1;
    end;
End;

end.

