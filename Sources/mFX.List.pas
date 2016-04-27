unit mFX.List;

interface

Uses System.SysUtils;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Const
  _FXListMaxSize_ = Maxint div 16;

Type
  TFXList = class;
  PFXObjectList = ^TFXObjectList;
  TFXObjectList = array[0.._FXListMaxSize_ - 1] of TObject;
  TFXListSortCompare = function (Const Item1, Item2: TObject): Integer;

  TFXList = class(TObject)
  private
    FList     : PFXObjectList;
    FCount    : Integer;
    FCapacity : Integer;
    /// <SUMMARY>Grow Capacity</SUMMARY>
    procedure Grow;
    procedure SetCapacity(Const NewCapacity: Integer);
    procedure SetCount(Const NewCount: Integer);

    /// <SUMMARY>Get Object at Index</SUMMARY>
    function Get(Const Index: Integer): TObject;
    /// <SUMMARY>Set Object at Index</SUMMARY>
    procedure Put(Const Index: Integer;Const Item: TObject);
  public
    /// <SUMMARY>destructor</SUMMARY>
    destructor Destroy; override;

    /// <summary>Clear</summary>
    procedure Clear;virtual;

    function Pack:Boolean;
    procedure TrimExcess;
    function Expand:Boolean;

    class procedure Error(const Msg: string;Const Data: Integer); overload;
    class procedure Error(Const Msg: PResStringRec;Const Data: Integer); overload;

    function First: TObject;
    function IndexOf(Const Item: TObject): Integer;
    function Last: TObject;
    function Remove(Const Item: TObject): Integer;
    /// <SUMMARY>Set Null do not Free Item </SUMMARY>
    function SetNull(Const Index: Integer): TObject;overload;
    /// <SUMMARY>Set Null do not Free Item </SUMMARY>
    function SetNull(Const Item: TObject): TObject;overload;

    function Add(Const Item: TObject): Integer;
    procedure Insert(Const Index: Integer;Const Item: TObject);
    procedure Delete(Const Index: Integer);
    procedure Move(Const CurIndex, NewIndex: Integer);
    procedure Exchange(Const Index1, Index2: Integer);
    procedure Sort(Const Compare: TFXListSortCompare);
    function Extract(Const Item: TObject): TObject;

    property Capacity: Integer       read FCapacity write SetCapacity;
    property Count   : Integer       read FCount    write SetCount;
    property List    : PFXObjectList read FList;
    property Items[Const Index: Integer]: TObject read Get write Put; default;
  end;

  EFXListError = class(Exception);

implementation

uses RTLConsts;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
destructor TFXList.Destroy;
begin
  Self.Clear;
  inherited;
end;
{______________________________________________________________________________}
class procedure TFXList.Error(const Msg: string;Const Data: Integer);
begin
  raise EFXListError.CreateFmt(Msg, [Data])
end;
{______________________________________________________________________________}
class procedure TFXList.Error(Const Msg: PResStringRec;Const Data: Integer);
begin
  TFXList.Error(LoadResString(Msg), Data);
end;
{______________________________________________________________________________}
procedure TFXList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;
{______________________________________________________________________________}
procedure TFXList.TrimExcess;
Begin
  SetCapacity(fCount)
End;
{______________________________________________________________________________}
function TFXList.Expand:Boolean;
begin
  if FCount = FCapacity then Begin
    Result:=True;
    Grow;
  end else
    Result:=False;
end;
{______________________________________________________________________________}
function TFXList.Pack:Boolean;
var PackedCount : Integer;
    StartIndex : Integer;
    EndIndex : Integer;
begin
  if FCount>0 then Begin
    PackedCount := 0;
    StartIndex := 0;
    repeat  // Locate the first/next non-nil element in the list
            while (StartIndex < FCount) and (FList^[StartIndex] = nil) do
              Inc(StartIndex);
            // There is nothing more to do
            if StartIndex < FCount then begin
              // Locate the next nil pointer
              EndIndex := StartIndex;
              while (EndIndex < FCount) and (FList^[EndIndex] <> nil) do
                Inc(EndIndex);
              Dec(EndIndex);
              // Move this block of non-null items to the index recorded in PackedToCount:
              // If this is a contiguous non-nil block at the start of the list then
              // StartIndex and PackedToCount will be equal (and 0) so don't bother with the move.
              if StartIndex > PackedCount then
                System.Move(FList^[StartIndex],FList^[PackedCount],(EndIndex - StartIndex + 1) * SizeOf(Pointer));
              // Set the PackedToCount to reflect the number of items in the list
              // that have now been packed.
              Inc(PackedCount, EndIndex - StartIndex + 1);
              // Reset StartIndex to the element following EndIndex
              StartIndex := EndIndex + 1;
              end;
    Until StartIndex >= FCount;
    // Set Count so that the 'free' item
    Result:=FCount<>PackedCount;
    FCount:=PackedCount;
  End else
    Result:=False;
end;
{______________________________________________________________________________}
procedure TFXList.SetCapacity(Const NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > _FXListMaxSize_) then
    Error(SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;
{______________________________________________________________________________}
procedure TFXList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then Begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then Begin
    Delta := 16
  end else Begin
    Delta := 4;
    end;
  SetCapacity(FCapacity + Delta);
end;
{______________________________________________________________________________}
procedure TFXList.SetCount(Const NewCount: Integer);
var I: Integer;
begin
  if (NewCount < 0) or (NewCount > _FXListMaxSize_) then
    Error(SListCountError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then Begin
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  end else Begin
    for I := FCount - 1 downto NewCount do
      Delete(I);
    end;
  FCount := NewCount;
end;
{______________________________________________________________________________}
procedure TFXList.Delete(Const Index: Integer);
var Temp: TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  Temp := FList^[Index];
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],(FCount - Index) * SizeOf(Pointer));
  Temp.Free;
end;
{______________________________________________________________________________}
function TFXList.Add(Const Item: TObject): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;
{______________________________________________________________________________}
procedure TFXList.Insert(Const Index: Integer;Const Item: TObject);
begin
  if (Index < 0) or (Index > FCount) then
    Error(SListIndexError, Index);

  if FCount = FCapacity then
    Grow;

  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Pointer));

  FList^[Index] := Item;
  Inc(FCount);
end;
{______________________________________________________________________________}
procedure TFXList.Exchange(Const Index1, Index2: Integer);
var Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(SListIndexError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;
{______________________________________________________________________________}
procedure TFXList.Move(Const CurIndex, NewIndex: Integer);
var CurrItem: Pointer;
begin
  if CurIndex <> NewIndex then begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(SListIndexError, NewIndex);
    if (CurIndex < 0) or (CurIndex >= FCount) then
      Error(SListIndexError, CurIndex);
    CurrItem := FList^[CurIndex];
    FList^[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList^[NewIndex] := CurrItem;
    end
end;
{______________________________________________________________________________}
procedure QuickSort(Const SortList: PFXObjectList;L, R: Integer;Const SCompare: TFXListSortCompare);
var I, J: Integer;
    P, T: Pointer;
begin
  repeat  I := L;
          J := R;
          P := SortList^[(L + R) shr 1];
          repeat  while SCompare(SortList^[I], P) < 0 do
                    Inc(I);
                  while SCompare(SortList^[J], P) > 0 do
                    Dec(J);
                  if I <= J then begin
                    if I <> J then begin
                      T := SortList^[I];
                      SortList^[I] := SortList^[J];
                      SortList^[J] := T;
                      end;
                    Inc(I);
                    Dec(J);
                    end;
          until I > J;
          if L < J then
            QuickSort(SortList, L, J, SCompare);
          L := I;
  until I >= R;
end;
{______________________________________________________________________________}
procedure TFXList.Sort(Const Compare: TFXListSortCompare);
begin
  if (FList <> nil) and (Count > 1) then
    QuickSort(FList, 0, Count - 1, Compare);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXList.Get(Const Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  Result := FList^[Index];
end;
{______________________________________________________________________________}
function TFXList.First: TObject;
begin
  Result := Get(0);
end;
{______________________________________________________________________________}
function TFXList.Last: TObject;
var LCount: Integer;
begin
  LCount := FCount;
  if LCount > 0 then Begin
    Result := FList^[LCount - 1]
  end else begin
    Error(SListIndexError, 0);
    Result := nil;
    end;
end;
{______________________________________________________________________________}
function TFXList.IndexOf(Const Item: TObject): Integer;
var LList: PFXObjectList;
    LPCount: Integer;
begin
  LPCount:= Pred(FCount);
  LList  := Pointer(FList);
  for Result := 0 to LPCount do
    // new optimizer doesn't use [esp] for Result
    if LList[Result] = Item then
      Exit;
  Result := -1;
end;
{______________________________________________________________________________}
procedure TFXList.Put(Const Index: Integer;Const Item: TObject);
var LItem : TObject;
    Temp: TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);

  LItem  :=Item;
  if LItem <> FList^[Index] then begin
    Temp := FList^[Index];
    FList^[Index] := Item;
    Temp.Free
    end;
end;
{______________________________________________________________________________}
function TFXList.Remove(Const Item: TObject): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;
{______________________________________________________________________________}
function TFXList.SetNull(Const Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  Result := FList^[Index];
  FList^[Index]:=nil;
end;
{______________________________________________________________________________}
function TFXList.SetNull(Const Item: TObject): TObject;
var I: Integer;
begin
  Result := nil;
  I := IndexOf(Item);
  if I >= 0 then begin
    Result := Item;
    FList^[I]:=nil;
    end;
end;
{______________________________________________________________________________}
function TFXList.Extract(Const Item: TObject): TObject;
var I: Integer;
begin
  Result := nil;
  I := IndexOf(Item);
  if I >= 0 then begin
    Result := Item;
    FList^[I] := nil;
    Delete(I);
    end;
end;

end.
