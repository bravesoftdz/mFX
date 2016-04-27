unit mFX.ExternalTable;

interface

{$I mFX.Inc}

uses Classes, SysUtils,
  mFX.Header;

Type
  ///<summary>Create Binary ExternalTable</summary>
  /// 22nd September 2006: Henrique Netzka describes a technique he has developed to convert data from another DBMS to Firebird using external files.
  ///  Based on orignal TIBRecord
  ///  BUG Date,TimeStamp,Time can not be Consecutives
  TFXCustomExternalTable = Class
  public
    fFields            : Integer;
    fCharSize          : Integer;
    fLine              : Array[0..2047] of AnsiChar;
    fBufPos            : Integer;
    fFieldPos          : Integer;
    fEncoding          : TEncoding;
    fUTF8Encoding      : TEncoding;
    fStream            : TStream;
    fOwnedStream       : TStream;

    ///<summary>Start New Line</summary>
    procedure Reset;

    ///	<summary>
    ///	  Calc Field Alignment. from Pavel Cisar &lt;pcisar@ibphoenix.cz&gt;
    ///	</summary>
    ///	<param name="aBufPos">
    ///	  position in output buffer where the last field ends
    ///	</param>
    ///	<param name="aNbFields">
    ///	  The position (number) of field in table structure
    ///	</param>
    ///	<param name="aFieldType">
    ///	  FieldType = Any value to distinguish few field types with special
    ///	  handling
    ///	</param>
    ///	<param name="aFieldLength">
    ///	  Length of the value (smallint = 2, int = 4, bigint, datetime, double
    ///	  = 8, char/varchar (definition length))
    ///	</param>
    ///	<returns>
    ///	  The function returns (new) position in result buffer where the last
    ///	  value ends (so the next value start at next byte). If buffer is
    ///	  initialized by zeros, the previous value is padded by zeros
    ///	  automatically.
    ///	</returns>
    ///	<remarks>
    ///	  <para>
    ///	    The formula is relatively simple:
    ///	  </para>
    ///	  <para>
    ///	    1. char doesn't need any alignment, so the buffer position remains
    ///	    the same
    ///	  </para>
    ///	  <para>
    ///	    2. varchar is padded by two bytes (smallint value length) that
    ///	    needs to be aligned
    ///	  </para>
    ///	  <para>
    ///	    3. other are aligned by MIN(value_size_by_type,8)
    ///	  </para>
    ///	  <para>
    ///	    4. It's necessary to apply correction for missing NULL flag(s) (4
    ///	    bytes) per every 32 fields.
    ///	  </para>
    ///	  <para>
    ///	    i.e. add the correction value (4,8,12 etc.) before the alignment
    ///	    calculation and then substract it.
    ///	  </para>
    ///	  <para>
    ///	    Example:
    ///	  </para>
    ///	  <para>
    ///	    Table with CHAR(5),SMALLINT,CHAR(5),INT,CHAR(5),BIGINT has (first
    ///	    byte in buffer has index 1):
    ///	  </para>
    ///	  <para>
    ///	    align(0, 1, 'char', 5) = 0 (value in bytes 1-5)
    ///	  </para>
    ///	  <para>
    ///	    align(5, 2, 'smallint', 2) = 6 (value in bytes 7-8)
    ///	  </para>
    ///	  <para>
    ///	    align(8, 3, 'char', 5) = 8 (value in bytes 9-13)
    ///	  </para>
    ///	  <para>
    ///	    align(13, 4, 'int', 4) = 16 (value in bytes 17-20) align(20, 5,
    ///	    'char', 5) = 20 (value in bytes 21-25)
    ///	  </para>
    ///	  <para>
    ///	    align(25, 6, 'bigint', 8) = 28 (value in bytes 29-36)
    ///	  </para>
    ///	</remarks>
    Class Function Align(Const aBufPos,aNbFields,aFieldType,aFieldLength:Integer):Integer;

    ///<summary>Assign File Stream</summary>
    procedure SetStream(const AStream:TStream);

  protected
    ///<summary>Append Integer 64</summary>
    /// Remember: Firebird's INT64 is 64 bits, just as Delphi's
    procedure AppendInt64(Const AValor: Int64);

    ///<summary>Append Integer 32</summary>
    /// Remember: Firebird's INTEGER is 32 bits, just as Delphi's
    procedure AppendInt32(Const AValor: Int32);

    ///<summary>Append SmallInt 32</summary>
    /// Smallint, also like Delphi's; 16 bits.
    procedure AppendSmallInt(Const AValor: SmallInt);

    ///<summary>Append Float as Int32</summary>
    procedure AppendDecimal32(Const AValor: Double;Const Dec:Integer);
    ///<summary>Append Float as Int64</summary>
    procedure AppendDecimal64(Const AValor: Double;Const Dec:Integer);

    ///<summary>Append Date</summary>
    procedure AppendDate(Const AValor: TDate);

    ///<summary>Append TimeStamp</summary>
    procedure AppendTimeStamp(Const AValor: TDateTime);

    ///<summary>Append TimeStamp</summary>
    procedure AppendTime(Const AValor: TDateTime);

    ///<summary>Append Char</summary>
    procedure AppendChar(Const AValor: String;Const ASize: Integer);overload;
    ///<summary>Append Char</summary>
    procedure AppendChar(Const AValor: AnsiString;Const ASize: Integer);overload;
    ///<summary>Append Char</summary>
    procedure AppendChar(Const AValor: TBytes;Const ASize: Integer);overload;

    ///<summary>Append Char</summary>
    procedure AppendUTF8(Const AValor: String;Const ASize: Integer);overload;

    ///<summary>Append VarChar</summary>
    procedure AppendVarChar(Const AValor: String;Const ASize: Integer);overload;
    ///<summary>Append VarChar</summary>
    procedure AppendVarChar(Const AValor: AnsiString;Const ASize: Integer);overload;

    ///	<summary>
    ///	  Append VarChar
    ///	</summary>
    ///	<remarks>
    ///	  Bug in VARCHAR format ! Tobe checked
    ///	</remarks>
    procedure AppendVarChar(Const AValor: TBytes;Const ASize: Integer);overload;

    ///<summary>Push Line</summary>
    /// The Record must be builded using the AppendXXXX functions.
    procedure PushLine;
  public
    /// <summary>destructor</summary>
    destructor Destroy; override;

    property Stream: TStream read fStream write SetStream;
  End;

implementation

Uses Math;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
destructor TFXCustomExternalTable.Destroy;
Begin
  fOwnedStream.Free;
  fUTF8Encoding.Free;
  fEncoding.Free;
  inherited;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXCustomExternalTable.Reset;
begin
  fBufPos  := 0;
  fFieldPos:= 1;
  FillChar(fLine[1],SizeOf(fLine),#0);
end;
{______________________________________________________________________________}
Class Function TFXCustomExternalTable.Align(Const aBufPos,aNbFields,aFieldType,aFieldLength:Integer):Integer;
Var correction,alignment,BufPos:Integer;
begin
 Case aFieldType of
   SQL_TEXT:Begin
     Result:=aBufPos;
     exit;
     end;
   SQL_VARYING:Begin
     alignment:=2
     end;
   else Begin
     alignment  := aFieldLength;
   end end;

 // It's necessary to apply correction for missing NULL flag(s) (4 bytes) per every 32 fields.
 // I.e. add the correction value (4,8,12 etc.) before the alignment calculation and then substract it.
 correction := ((aNbFields div 32)+1) * 4;
 alignment := min(alignment,8);

 BufPos:= aBufPos+correction;
 Result:=( (BufPos + alignment - 1) And (Not (alignment - 1)));
 Result:= Result-correction;
end;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.SetStream(const AStream:TStream);
begin
  FreeAndNil(fOwnedStream);
  if AStream<>nil then begin
    fStream:=AStream;
  end else Begin
    fOwnedStream:=TMemoryStream.Create;
    fStream:=fOwnedStream;
    end;
  fStream.Position:=0;
  fStream.Size:=0;
  Self.Reset;
end;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.PushLine;
Begin
  fStream.Write(fLine[1],fBufPos);
  Self.Reset;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendChar(Const AValor: TBytes;Const ASize: Integer);
var Trg:PAnsiChar;
    a,l:Integer;
begin
  a:=Align(fBufPos,fFields,SQL_TEXT,ASize*fCharSize);
  fBufPos:=a+ASize*fCharSize;
  Inc(fFieldPos);

  l:=Length(AValor);
  Trg:=@FLine[a+1];
  if l<ASize*fCharSize then Begin
    FillChar(FLine[a+1], ASize*fCharSize, #32);
    if l>0 then
      System.Move(AValor[0],trg^,l)
  end else
    System.Move(AValor[0],trg^,ASize*fCharSize);
end;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendChar(Const AValor: String;Const ASize: Integer);
Var Val: TBytes;
Begin
  Val:=fEncoding.GetBytes(AValor);
  AppendChar(Val,ASize);
End;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendChar(Const AValor: AnsiString;Const ASize: Integer);
Var Val: TBytes;
    v:String;
Begin
  v:=String(AValor);
  Val:=fEncoding.GetBytes(v);
  AppendChar(Val,ASize);
End;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendVarChar(Const AValor: TBytes;Const ASize: Integer);
var Trg:PAnsiChar;
    a,l:Integer;
begin
  a:=Align(fBufPos,fFields,SQL_VARYING,ASize*fCharSize);
  fBufPos:=a+ASize*fCharSize;
  Inc(fFieldPos);

  l:=Length(AValor);
  fLine[a+1] := AnsiChar(Lo(l));
  fLine[a+2] := AnsiChar(Hi(l));

  Trg:=@FLine[a+3];
  if l<ASize*fCharSize then Begin
    FillChar(FLine[a+3], ASize*fCharSize, #32);
    if l>0 then
      System.Move(AValor[0],trg^,l)
  end else
    System.Move(AValor[0],trg^,ASize*fCharSize);
end;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendVarChar(Const AValor: String;Const ASize: Integer);
Var Val: TBytes;
Begin
  Val:=fEncoding.GetBytes(AValor);
  AppendVarChar(Val,ASize)
End;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendVarChar(Const AValor: AnsiString;Const ASize: Integer);
Var Val: TBytes;
    v:String;
Begin
  v:=String(AValor);
  Val:=fEncoding.GetBytes(v);
  AppendVarChar(Val,ASize)
End;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendUTF8(Const AValor: String;Const ASize: Integer);
var Trg:PAnsiChar;
    a,l:Integer;
    Val:TBytes;
Begin
  Val:=fUTF8Encoding.GetBytes(AValor);

  a:=Align(fBufPos,fFields,SQL_TEXT,ASize*4);
  fBufPos:=a+ASize*4;
  Inc(fFieldPos);

  l:=Length(AValor);
  Trg:=@FLine[a+1];
  if l<ASize*4 then Begin
    FillChar(FLine[a+1], ASize*4, #32);
    if l>0 then
      System.Move(Val[0],trg^,l)
  end else
    System.Move(Val[0],trg^,ASize*4);
End;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendDate(Const AValor: TDate);
Var d: ISC_DATE;
    a:Integer;
    Trg:PByte;
begin
  a:=Align(fBufPos,fFields,SQL_DATE,4);
  fBufPos:=a+4;
  Inc(fFieldPos);
  Trg:=@FLine[a+1];

  // date Trunc(DateTime)+DateOffset=15018
  d := EncodeDateTime2SQLDate(AValor);
  Trg[0] :=   d and  255;
  Trg[1] := ((d AND (255 SHL  8)) SHR  8);
  Trg[2] := ((d AND (255 SHL 16)) SHR 16);
  Trg[3] := ((d AND (255 SHL 24)) SHR 24);
end;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendTimeStamp(Const AValor: TDateTime);
Var d:ISC_DATE;
    t:ISC_TIME;
    a:Integer;
    Trg:PByte;
begin
  a:=Align(fBufPos,fFields,SQL_TIMESTAMP,8);
  fBufPos:=a+8;
  Inc(fFieldPos);
  Trg:=@FLine[a+1];

  // date Trunc(DateTime)+DateOffset=15018
  d := EncodeDateTime2SQLDate(AValor);
  Trg[0] :=   d and  255;
  Trg[1] := ((d AND (255 SHL  8)) SHR  8);
  Trg[2] := ((d AND (255 SHL 16)) SHR 16);
  Trg[3] := ((d AND (255 SHL 24)) SHR 24);

  // Time Round(Frac(DateTime) * 10 * TimeCoeff=24*60*60*1000);
  t:=mFX.Header.EncodeDateTime2SQLTime(AValor);
  Trg[4] :=   t and  255;
  Trg[5] := ((t AND (255 SHL  8)) SHR  8);
  Trg[6] := ((t AND (255 SHL 16)) SHR 16);
  Trg[7] := ((t AND (255 SHL 24)) SHR 24);
end;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendTime(Const AValor: TDateTime);
Var t:ISC_TIME;
    a:Integer;
    Trg:PByte;
begin
  a:=Align(fBufPos,fFields,SQL_TIMESTAMP,4);
  fBufPos:=a+4;
  Inc(fFieldPos);
  Trg:=@FLine[a+1];

  // Time Round(Frac(DateTime) * 10 * TimeCoeff=24*60*60*1000);
  t:=mFX.Header.EncodeDateTime2SQLTime(AValor);
  Trg[0] :=   t and  255;
  Trg[1] := ((t AND (255 SHL  8)) SHR  8);
  Trg[2] := ((t AND (255 SHL 16)) SHR 16);
  Trg[3] := ((t AND (255 SHL 24)) SHR 24);
end;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendDecimal32(Const AValor: Double;Const Dec:Integer);
Var a:Integer;
    Trg:PByte;
    i64:Int64;
begin
  a:=Align(fBufPos,fFields,SQL_BIGINT,4);
  fBufPos:=a+4;
  Inc(fFieldPos);
  Trg:=@FLine[a+1];

  i64:=Round(AValor*IntPower(10,Dec));

  Trg[0] :=   i64 and  Int64(255);
  Trg[1] := ((i64 AND (Int64(255) SHL  8)) SHR  8);
  Trg[2] := ((i64 AND (Int64(255) SHL 16)) SHR 16);
  Trg[3] := ((i64 AND (Int64(255) SHL 24)) SHR 24);
end;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendDecimal64(Const AValor: Double;Const Dec:Integer);
Var a:Integer;
    Trg:PByte;
    i64:Int64;
begin
  a:=Align(fBufPos,fFields,SQL_BIGINT,8);
  fBufPos:=a+8;
  Inc(fFieldPos);
  Trg:=@FLine[a+1];

  i64:=Round(AValor*IntPower(10,Dec));

  Trg[0] :=   i64 and  Int64(255);
  Trg[1] := ((i64 AND (Int64(255) SHL  8)) SHR  8);
  Trg[2] := ((i64 AND (Int64(255) SHL 16)) SHR 16);
  Trg[3] := ((i64 AND (Int64(255) SHL 24)) SHR 24);
  Trg[4] := ((i64 AND (Int64(255) SHL 32)) SHR 32);
  Trg[5] := ((i64 AND (Int64(255) SHL 40)) SHR 40);
  Trg[6] := ((i64 AND (Int64(255) SHL 48)) SHR 48);
  Trg[7] := ((i64 AND (Int64(255) SHL 56)) SHR 56);
end;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendInt64(Const AValor: Int64);
Var a:Integer;
    Trg:PByte;
begin
  a:=Align(fBufPos,fFields,SQL_BIGINT,8);
  fBufPos:=a+8;
  Inc(fFieldPos);
  Trg:=@FLine[a+1];

  Trg[0] :=   AValor and  Int64(255);
  Trg[1] := ((AValor AND (Int64(255) SHL  8)) SHR  8);
  Trg[2] := ((AValor AND (Int64(255) SHL 16)) SHR 16);
  Trg[3] := ((AValor AND (Int64(255) SHL 24)) SHR 24);
  Trg[4] := ((AValor AND (Int64(255) SHL 32)) SHR 32);
  Trg[5] := ((AValor AND (Int64(255) SHL 40)) SHR 40);
  Trg[6] := ((AValor AND (Int64(255) SHL 48)) SHR 48);
  Trg[7] := ((AValor AND (Int64(255) SHL 56)) SHR 56);
end;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendInt32(Const AValor: Int32);
Var a:Integer;
    Trg:PByte;
begin
  a:=Align(fBufPos,fFields,SQL_LONG,4);
  fBufPos:=a+4;
  Inc(fFieldPos);
  Trg:=@FLine[a+1];

  Trg[0] :=  (AValor and  255);
  Trg[1] := ((AValor and (255 shl 8)) shr 8);
  Trg[2] := ((AValor and (255 shl 16)) shr 16);
  Trg[3] := ((AValor and (255 shl 24)) shr 24);
end;
{______________________________________________________________________________}
procedure TFXCustomExternalTable.AppendSmallInt(Const AValor: SmallInt);
Var a:Integer;
    Trg:PByte;
begin
  a:=Align(fBufPos,fFields,SQL_Short,2);
  fBufPos:=a+2;
  Inc(fFieldPos);
  Trg:=@FLine[a+1];

  Trg[0] :=  (AValor and  255);
  Trg[1] := ((AValor and (255 shl 8)) shr 8);
end;

end.

