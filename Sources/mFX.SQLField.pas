unit mFX.SQLField;

interface

{$R-,T-,H+,X+}
{$I mFX.Inc}

uses System.SysUtils, System.Classes, System.Types, System.Variants, Data.Db,
  mFX.List, mFX.Intf, mFX.Header, mFX.Base, mFX.Classes, mFX.Blob, mFX.SQL;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  TFXBCDField = Class;
  TFXMemoField = Class;
  TFXStringField = Class;

  /// <summary>TFXStringField allows us to have strings longer than 8196 </summary>
  TFXStringField = class(TWideStringField)
  private
    fBuffer : PFXByte;
    fBytes  : Word;
    function GetValue(var Value:String): Boolean;
  protected
    function GetAsString:string;override;
    function GetAsVariant:Variant;override;

    function GetAsAnsiString: AnsiString; override;
    function GetAsWideString: String; override;

    procedure SetAsString(const Value: string); override;

    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(Value: Boolean); override;

  public
    ///<summary>constructor </summary>
    constructor Create(AOwner: TComponent); override;
    ///<summary>destructor</summary>
    destructor Destroy; override;

    property Bytes : Word read fBytes write fBytes;
  published
    property Transliterate default False;
  end;

  /// <summary>TFXStringField allows us to have strings longer than 8196 </summary>
  TFXMemoField = class(TWideMemoField)
  end;

  /// <summary>TFXBCDField</summary>
  /// Actually, there is no BCD involved in this type,
  ///  instead it deals with currency types.
  ///  In IB, this is an encapsulation of Numeric (x, y)
  ///  where x < 18 and y <= 4.
  ///  Note: y > 4 will default to Floats
  TFXBCDField = class(TBCDField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsCurrency: Currency; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Size default 8;
  end;

  /// <summary>TFXFieldsValue Save Current Record Fields Values</summary>
  TFXRecordValue = class(TObject)
  private
    fFieldNo   : Integer;
    fFieldName : String;
    fDataType  : TFieldType;
    fValue     : Variant;
    ///<summary>constructor </summary>
    constructor CreateFromField(Const aField:TField);
  end;

  /// <summary>TFXFieldsValue Save Current Record Fields Values</summary>
  TFXRecordValues = class(TFXList)
  private
  public
    ///<summary>Push RecordValues</summary>
    procedure Push(Const aDataSet:TDataSet);
    ///<summary>Pop RecordValues</summary>
    procedure Pop(Const aDataSet:TDataSet);
  end;


implementation

uses
  mFX.Consts, mFX.Utils;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXStringField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Transliterate:=False;
end;
{______________________________________________________________________________}
destructor TFXStringField.Destroy;
begin
  FXFree(fBuffer);
  inherited;
end;
{______________________________________________________________________________}
function TFXStringField.GetValue(var Value: String): Boolean;
var Buffer: TValueBuffer;
    NullIndex: Integer;
    Str: string;
    sz:Integer;
begin
  sz:=Self.DataSize;
  if sz > dsMaxStringSize + SizeOf(Char) then
    SetLength(Buffer, ((sz div 2) + 1) * SizeOf(Char)) else
    SetLength(Buffer, dsMaxStringSize + SizeOf(Char));
  Result := GetData(Buffer, False);
  if Result then begin
    Str := TEncoding.Unicode.GetString(Buffer);
    NullIndex := Str.IndexOf(#0);
    if NullIndex >= 0 then
      Value := Str.Remove(NullIndex) else
      Value := Str
    end
end;
{______________________________________________________________________________}
function TFXStringField.GetAsString: string;
var TempStr: String;
begin
  if GetValue(TempStr) then Begin
    if not FixedChar then
      Result := Trim(TempStr) else
      Result := TempStr
  end else
    Result := EmptyStr
end;
{______________________________________________________________________________}
function TFXStringField.GetAsVariant: Variant;
var TempStr: String;
begin
  if GetValue(TempStr) then Begin
    if not FixedChar then
      Result := Trim(TempStr) else
      Result := TempStr
  end else
    Result := System.Variants.Null;
end;
{______________________________________________________________________________}
function TFXStringField.GetAsAnsiString: AnsiString;
var TempStr: UnicodeString;
begin
  if GetValue(TempStr) then Begin
    if not FixedChar then
      TempStr := Trim(TempStr) else
      TempStr := TempStr;
    Result := AnsiString(TempStr)
  end else
    Result := EmptyAnsiStr
end;
{______________________________________________________________________________}
function TFXStringField.GetAsWideString: UnicodeString;
var TempStr: UnicodeString;
begin
  if GetValue(TempStr) then Begin
    if not FixedChar then
      Result := Trim(TempStr) else
      Result := TempStr
  end else
    Result := EmptyWideStr
end;
{______________________________________________________________________________}
procedure TFXStringField.SetAsString(const Value: string);
var Buffer: TValueBuffer;
begin
  Assert(SizeOf(Char)=2);
  Buffer := TEncoding.Unicode.GetBytes(Value);
  SetLength(Buffer, Length(Buffer) + SizeOf(Char));
  Buffer[Length(Buffer) - 2] := 0;
  Buffer[Length(Buffer) - 1] := 0;
  SetData(Buffer, False);
end;
{______________________________________________________________________________}
function TFXStringField.GetAsBoolean: Boolean;
var TempStr: String;
begin
  if not IsNull then Begin
    TempStr:=Trim(Self.AsString);
    Result:=SameText(TempStr,ibTrue)
  end else
    result := false;
End;
{______________________________________________________________________________}
procedure TFXStringField.SetAsBoolean(Value: Boolean);
Begin
  if Value then
    SetAsString(ibTrue) else
    SetAsString(ibFalse)
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXBCDField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftBCD);
  Size := 8;
end;
{______________________________________________________________________________}
class procedure TFXBCDField.CheckTypeSize(Value: Integer);
begin
{ No need to check as the base type is currency, not BCD }
end;
{______________________________________________________________________________}
function TFXBCDField.GetAsCurrency: Currency;
begin
  if not GetValue(Result) then
    Result := 0
end;
{______________________________________________________________________________}
function TFXBCDField.GetAsString: string;
var C: System.Currency;
begin
  if GetValue(C) then
    Result := CurrToStr(C) else
    Result := EmptyStr
end;
{______________________________________________________________________________}
function TFXBCDField.GetAsVariant: Variant;
var C: System.Currency;
begin
  if GetValue(C) then
    Result := C else
    Result := Null;
end;
{______________________________________________________________________________}
function TFXBCDField.GetDataSize: Integer;
begin
  Result := 8;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
constructor TFXRecordValue.CreateFromField(Const aField:TField);
Begin
  fFieldNo   := aField.FieldNo;
  fDataType  := aField.DataType;
  fFieldName := aField.FieldName;
End;
{______________________________________________________________________________}
procedure TFXRecordValues.Push(Const aDataSet:TDataSet);
Var fv:TFXRecordValue;
    v:Variant;
    i:Integer;
    f:TField;
Begin
  Self.Clear;
  for i:=0 to Pred(aDataSet.Fields.Count) do Begin
    f:=aDataSet.Fields[i];
    if f.FieldKind=fkData then Begin
      v:=f.NewValue;
      if (not VarIsEmpty(v))and(not VarIsNull(v)) then Begin
        fv:=TFXRecordValue.CreateFromField(f);
        fv.fValue:=v;
        Self.Add(fv);
    end end end;
End;
{______________________________________________________________________________}
procedure TFXRecordValues.Pop(Const aDataSet:TDataSet);
Var f:TFXRecordValue;
    i:Integer;
Begin
  for i:=0 to Pred(Self.Count) do Begin
    f:=TFXRecordValue(Self[i]);
    aDataSet.FieldByName(f.fFieldName).Value:=f.fValue
    end
End;

initialization

  System.Classes.RegisterClasses([
    TFXStringField,
    TFXMemoField,
    TFXBCDField
    ]);

end.
