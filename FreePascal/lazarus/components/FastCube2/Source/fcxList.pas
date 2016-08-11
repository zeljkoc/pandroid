{*******************************************************}
{                                                       }
{               FastCube 2 fcList unit                  }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxList;
{$INCLUDE fcx.inc}

interface
uses
  Classes, fcxTypes, fcxQSort;
//FMX uses
{$ELSE}
{$INCLUDE fcx.inc}

interface
uses
  System.Classes, FMX.fcxTypes, FMX.fcxQSort;
{$ENDIF}

type
  {$IFDEF Delphi_16UP}
  TfcxBaseListType = TPointerList;
  {$ELSE}
  TfcxBaseListType = PPointerList;
  {$ENDIF}

  // Persistent gives RTTI for FS
  TfcxList = class(TPersistent)
  private
    FAutoFree: Boolean;
  protected
    FList: TfcxBaseListType;
    FCount: Integer;
    FCapacity: Integer;
    fAddBy: Integer;
    function Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; Item: Pointer); virtual;
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
  public
    class procedure Error(const Msg: String; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    constructor Create(AAddBy: Integer); overload; virtual;
    constructor Create; overload; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure ReCapacity;
    function Add(Item: Pointer): Integer; virtual;
    procedure Insert(Index: Integer; Item: Pointer); virtual;
    function IndexOf(Item: Pointer): Integer;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: TfcxBaseListType read FList;
    property AutoFree: Boolean read FAutoFree write FAutoFree;
  end;

  TfcxOrderList = class(TfcxList)
  private
    FAutoSort: Boolean;
    FSorted: Boolean;
    FCompare: TfcxIntegerCompare;
    FMoving: Boolean;
    FUseRandomMiddle: Boolean;
    function GetIndexAtOrder(AOrder: Integer): Integer;
    function GetOrderAtIndex(AIndex: Integer): Integer;
    procedure SetCompare(const Value: TfcxIntegerCompare);
    function FindPlace(const AIndex: Integer; out AOrder: Integer): Boolean;
    procedure SetAutoSort(const Value: Boolean);
    function GetItemsByOrder(Order: Integer): Pointer;
    procedure SetItemsByOrder(Order: Integer; const Value: Pointer);
  protected
// [rus] массив индексов объектов в fList. хранит индекс объекта в fList. порядок задан ключом.
    FIndex: PfcxIntegerArray;
// [rus] массив индексов сортировки полей. хранит индекс сортировки. порядок совпадает с порядком в fList.
    FOrder: PfcxIntegerArray;
    procedure Put(Index: Integer; Item: Pointer); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure Sort(AStart, AEnd: integer);
    procedure SortRandomMiddle(AStart, AEnd: integer);
    function InternalCompare(const AIndex1, AIndex2: Integer): Integer;
  public
    procedure ReIndex; virtual;
    constructor Create; override;
    constructor Create(AAutoSort: Boolean); overload; virtual;
    procedure Delete(Index: Integer); override;
    function Add(Item: Pointer): Integer; override;
    procedure Insert(Index: Integer; Item: Pointer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    property Order[AIndex: Integer]: Integer read GetOrderAtIndex;
    property BaseIndex[AOrder: Integer]: Integer read GetIndexAtOrder;
    property AutoSort: Boolean read FAutoSort write SetAutoSort;
    property UseRandomMiddle: Boolean read FUseRandomMiddle write FUseRandomMiddle;
    property Sorted: Boolean read FSorted;
    property Compare: TfcxIntegerCompare read FCompare write SetCompare;
    property ItemsByOrder[Order: Integer]: Pointer read GetItemsByOrder write SetItemsByOrder;
  end;

implementation
//VCL uses section
{$IFNDEF FMX}
uses
{$IFDEF DELPHI_6UP}
  RTLConsts, Math;
{$ELSE}
  consts;
{$ENDIF}
//FMX uses
{$ELSE}
uses
  System.RTLConsts, System.Math;
{$ENDIF}

resourcestring
  fcSListNotSortedError = 'Operation not allowed with nonsorted list';

{ TfcxList }

class procedure TfcxList.Error(const Msg: String; Data: Integer);

{$ifndef fpc}
  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;
{$endif}

begin
  raise EListError.CreateFmt(Msg, [Data]) at {$ifndef fpc}ReturnAddr{$else}get_caller_addr(get_frame){$endif};
end;

class procedure TfcxList.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

constructor TfcxList.Create(AAddBy: Integer);
begin
  Create;
  if AAddBy > 0 then
    FAddBy := AAddBy;
end;

constructor TfcxList.Create;
begin
  fAddBy := 0;
  FAutoFree := False;
end;

destructor TfcxList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TfcxList.Clear;
var
  i: integer;
  Temp: Pointer;
begin
  for I := Count - 1 downto 0 do
  begin
    Temp := FList[i];
    if Temp <> nil then
      Notify(Temp, lnDeleted);
  end;
  FCount := 0;
  SetCapacity(0);
end;

procedure TfcxList.Delete(Index: Integer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Temp := Items[Index];
  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(Pointer));
  if Temp <> nil then
    Notify(Temp, lnDeleted);
end;

procedure TfcxList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if FAutoFree then
    if Action = lnDeleted then
      TObject(Ptr).Free;
end;

function TfcxList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList[Result] := Item;
  Inc(FCount);
end;

procedure TfcxList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then
    Error(@SListIndexError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList[Index] := Item;
  Inc(FCount);
end;

function TfcxList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TfcxList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(@SListIndexError, NewIndex);
    Item := Get(CurIndex);
    FList[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList[NewIndex] := Item;
  end;
end;

function TfcxList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index];
end;

procedure TfcxList.Put(Index: Integer; Item: Pointer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Temp := FList[Index];
  FList[Index] := Item;
  if Temp <> nil then
    Notify(Temp, lnDeleted);
end;

procedure TfcxList.Grow;
  procedure CommonGrow;
  var
    Delta: Integer;
  begin
    if FCapacity > 64 then
      Delta := FCapacity div 4
    else if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
    SetCapacity(FCapacity + Delta);
  end;
begin
  if fAddBy = 0 then
    CommonGrow
  else (*if*)
    SetCapacity(Capacity + fAddBy);
end;

procedure TfcxList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity <> FCapacity then
  begin
    {$IFDEF Delphi_16UP}
    SetLength(FList, NewCapacity);
    {$ELSE}
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    {$ENDIF}
    FCapacity := NewCapacity;
  end;
end;

procedure TfcxList.ReCapacity;
begin
  SetCapacity(Count);
end;

{ TfcxOrderList }

function TfcxOrderList.Add(Item: Pointer): Integer;
var
  AOrder, i: integer;
begin
  Result := inherited Add(Item);
  FOrder[FCount - 1] := -1;
  if FAutoSort then
  begin
    FIndex[FCount - 1] := -1;
    FindPlace(Result, AOrder);
// [rus] 1. в массиве index сделать сдвиг вниз начиная с позиции  AOrder
    System.Move(FIndex^[AOrder], FIndex^[AOrder + 1],
      (FCount - AOrder - 1) * SizeOf(Integer));
    FIndex[AOrder] := Result;
// [rus] 2. в массиве order установить значения для нового и больших по сортировке
    for i := AOrder to FCount - 1 do
      FOrder[FIndex[i]] := i;
  end
  else
  begin
    if FSorted then
      FSorted := False;
    FIndex[FCount - 1] := FCount - 1;
  end
end;

constructor TfcxOrderList.Create;
begin
  inherited;
  FMoving := False;
  FIndex := nil;
  FOrder := nil;
  FCompare := nil;
  FAutoSort := False;
  FSorted := False;
  FUseRandomMiddle := False;
end;

constructor TfcxOrderList.Create(AAutoSort: Boolean);
begin
  Create;
  FAutoSort := AAutoSort;
  FSorted := True;
end;

procedure TfcxOrderList.Delete(Index: Integer);
var
  AOrder, i: integer;
begin
  inherited Delete(Index);
  if not FMoving then
    if FAutoSort then
    begin
      AOrder := FOrder[Index];
// [rus] 1. в массиве index установить значения для сдвигаемых по ключу
      for i := Index + 1 to FCount do
        FIndex[FOrder[i]] := i - 1;
// [rus] 2. в массиве order сделать сдвиг вверх начиная к позиции  Index
      System.Move(FOrder^[Index + 1], FOrder^[Index],
        (FCount - Index) * SizeOf(Integer));
// [rus] 3. в массиве index сделать сдвиг вверх начиная к позиции  AOrder
      System.Move(FIndex^[AOrder + 1], FIndex^[AOrder],
        (FCount - AOrder) * SizeOf(Integer));
// [rus] 4. в массиве order установить значения для нового и больших по сортировке
      for i := AOrder to FCount - 1 do
        FOrder[FIndex[i]] := i;
    end
    else
    begin
      FSorted := False;
// to do not make BaseIndex Is Broken
      FIndex[FOrder[FCount]] := FIndex[FCount];
    end;
end;

function TfcxOrderList.FindPlace(const AIndex: Integer; out AOrder: Integer): Boolean;
var
  ALeft, ARight, AMiddle, ACompare: Integer;
begin
  Result := False;
  ALeft := 0;
  ARight := FCount - 2;
  while ALeft <= ARight do
  begin
    AMiddle := (ALeft + ARight) shr 1; // middle
    ACompare := InternalCompare(FIndex[AMiddle], AIndex);
    if ACompare < 0 then
      ALeft := AMiddle + 1
    else if ACompare > 0 then
      ARight := AMiddle - 1
    else
    begin
      Result := True;
      // Found
      AOrder := AMiddle;
      Exit;
    end;
  end;
  AOrder := ALeft; // Place!
end;

function TfcxOrderList.GetIndexAtOrder(AOrder: Integer): Integer;
begin
  if (AOrder < 0) or (AOrder >= FCount) then
    Error(@SListIndexError, AOrder);
  Result := FIndex[AOrder];
end;

function TfcxOrderList.GetItemsByOrder(Order: Integer): Pointer;
begin
  if not FSorted then
    Error(@fcSListNotSortedError, Order);
  Result := Items[GetIndexAtOrder(Order)];
end;

function TfcxOrderList.GetOrderAtIndex(AIndex: Integer): Integer;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    Error(@SListIndexError, AIndex);
  Result := FOrder[AIndex];
end;

procedure TfcxOrderList.Insert(Index: Integer; Item: Pointer);
var
  AOrder, i: integer;
begin
  inherited Insert(Index, Item);
  FOrder[FCount - 1] := -1;
  if not FMoving then
    if FAutoSort then
    begin
      FIndex[FCount - 1] := -1;
// [rus] 1. в массиве index установить значения для сдвинутых по ключу
      for i := Index to FCount - 2 do
        FIndex[FOrder[i]] := i + 1;
// [rus] 2. в массиве order сделать сдвиг вниз начиная с позиции  Index
      System.Move(FOrder^[Index], FOrder^[Index + 1],
        (FCount - Index - 1) * SizeOf(Integer));
      FindPlace(Index, AOrder);
// [rus] 3. в массиве index сделать сдвиг вниз начиная с позиции  AOrder
      System.Move(FIndex^[AOrder], FIndex^[AOrder + 1],
        (FCount - AOrder - 1) * SizeOf(Integer));
      FIndex[AOrder] := Index;
// [rus] 4. в массиве order установить значения для нового и больших по сортировке
      for i := AOrder to FCount - 1 do
        FOrder[FIndex[i]] := i;
    end
    else
    begin
      if FSorted then
        FSorted := False;
// to do not make BaseIndex Is Broken
      FIndex[FCount - 1] := FCount - 1;
    end
  else
    FIndex[FCount - 1] := -1;
end;

function TfcxOrderList.InternalCompare(const AIndex1,
  AIndex2: Integer): Integer;
begin
  if Assigned(FCompare) then
  begin
    Result := FCompare(AIndex1, AIndex2);
//to create unique
    if Result = 0 then
      Result := fcxIntCompare(AIndex1, AIndex2)
  end
  else
//to create unique
    Result := fcxIntCompare(AIndex1, AIndex2)
end;

procedure TfcxOrderList.Move(CurIndex, NewIndex: Integer);
var
  ATemp, i: integer;
begin
  if CurIndex = NewIndex then
    Exit;
  FMoving := True;
  inherited Move(CurIndex, NewIndex);
  FMoving := False;
  if FAutoSort then
  begin
    ATemp := FOrder[CurIndex];
    if CurIndex < NewIndex then
    begin
// [rus] в массиве order сделать сдвиг вверх начиная c CurIndex + 1 по NewIndex к позиции  CurIndex
      System.Move(FOrder^[CurIndex + 1], FOrder^[CurIndex],
        (NewIndex - CurIndex) * SizeOf(Integer));
      FOrder[NewIndex] := ATemp;
      for i := CurIndex to NewIndex do
        FIndex[FOrder[i]] := i;
    end
    else
    begin
// [rus] в массиве order сделать сдвиг вниз начиная c NewIndex по CurIndex - 1 к позиции  NewIndex + 1
      System.Move(FOrder^[NewIndex], FOrder^[NewIndex + 1],
        (CurIndex - NewIndex) * SizeOf(Integer));
      FOrder[NewIndex] := ATemp;
      for i := NewIndex to CurIndex do
        FIndex[FOrder[i]] := i;
    end;
  end
  else
    FSorted := False;
end;

procedure TfcxOrderList.Put(Index: Integer; Item: Pointer);
var
  ALastOrder, AOrder, i: integer;
begin
  inherited;
  if FAutoSort then
  begin
    ALastOrder := FOrder[Index];
    FindPlace(Index, AOrder);
    if ALastOrder = AOrder then
      Exit;
    FOrder[Index] := AOrder;
    if ALastOrder < AOrder then
    begin
// [rus] сдвинуть от ALastOrder до AOrder вверх
      System.Move(FIndex^[ALastOrder + 1], FIndex^[ALastOrder],
        (AOrder - ALastOrder) * SizeOf(Integer));
      FIndex[AOrder] := Index;
      for i := ALastOrder to AOrder do
        FOrder[FIndex[i]] := i;
    end
    else
    begin
// [rus] сдвинуть от AOrder до ALastOrder вниз
      System.Move(FIndex^[AOrder], FIndex^[AOrder + 1],
        (ALastOrder - AOrder) * SizeOf(Integer));
      FIndex[AOrder] := Index;
      for i := AOrder to ALastOrder do
        FOrder[FIndex[i]] := i;
    end;
  end
  else
    FSorted := False;
end;

procedure TfcxOrderList.ReIndex;
begin
  if FUseRandomMiddle then
    SortRandomMiddle(0, FCount - 1)
  else
    Sort(0, FCount - 1);
  FSorted := True;
end;

procedure TfcxOrderList.SetAutoSort(const Value: Boolean);
begin
  FAutoSort := Value;
  if FAutoSort and not FSorted then
    ReIndex;
end;

procedure TfcxOrderList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity <> FCapacity then
  begin
    inherited;
    ReallocMem(FIndex, FCapacity * SizeOf(Integer));
    ReallocMem(FOrder, FCapacity * SizeOf(Integer));
  end;
end;

procedure TfcxOrderList.SetCompare(const Value: TfcxIntegerCompare);
begin
  FCompare := Value;
  if FAutoSort then
    ReIndex
  else
    FSorted := False;
end;

procedure TfcxOrderList.SetItemsByOrder(Order: Integer;
  const Value: Pointer);
begin
  if not FSorted then
    Error(@fcSListNotSortedError, Order);
  Items[GetIndexAtOrder(Order)] := Value;
end;

procedure TfcxOrderList.Sort(AStart, AEnd: integer);
var k: integer;
  procedure Insert(const AILeft, AIRight: integer);
  var
    i1, j1: integer;
    AIObject: integer;
  begin
    for i1 := AILeft + 1 to AIRight do
    begin
      AIObject := FIndex[i1];
      for j1 := i1 - 1 downto AILeft do
      begin
        if InternalCompare(FIndex[j1], AIObject) <= 0 then
          break;
        FIndex[j1 + 1] := FIndex[j1];
      end;
      FIndex[j1 + 1] := AIObject;
    end;
  end;

  procedure InternalSort(ASLeft, ASRight: Integer);
  var
    i, j, AMiddle: integer;
    AObject1, AObject2: integer;
    ALeftStack, ARightStack: PfcxIntegerArray;
    AStackSize: integer;
    AStackPos: integer;
    ALeft, ARight: Integer;
  begin
    AStackSize := 2048;
    GetMem(ALeftStack, AStackSize * SizeOf(integer));
    GetMem(ARightStack, AStackSize * SizeOf(integer));
    AStackPos := 0;
    ALeftStack[0] := ASLeft;
    ARightStack[0] := ASRight;
    while AStackPos >= 0 do
    begin
      ALeft := ALeftStack[AStackPos];
      ARight := ARightStack[AStackPos];
      dec(AStackPos);
      while (ARight - ALeft) > 12 do
      begin
        AMiddle := (ALeft + ARight) shr 1;
        AObject1 := FIndex[AMiddle];
        FIndex[AMiddle] := FIndex[ALeft];
        i := succ(ALeft);
        j := ARight;
        while true do
        begin
          while (i < j) and (InternalCompare(AObject1, FIndex[i]) > 0) do
            inc(i);
          while (j >= i) and (InternalCompare(FIndex[j], AObject1) > 0) do
            dec(j);
          if (i >= j) then
            break;
          AObject2 := FIndex[i];
          FIndex[i] := FIndex[j];
          FIndex[j] := AObject2;
          dec(j);
          inc(i);
        end;
        FIndex[ALeft] := FIndex[j];
        FIndex[j] := AObject1;
{$IFDEF COMPARE_LENGTH}
        if (j - ALeft) <= (ARight - j) then
        begin
{$ENDIF}
          if (j + 1) < ARight then
          begin
            inc(AStackPos);
            if AStackPos > (AStackSize - 1) then
            begin
              AStackSize := AStackSize + 2048;
              ReallocMem(ALeftStack, AStackSize * SizeOf(integer));
              ReallocMem(ARightStack, AStackSize * SizeOf(integer));
            end;
            ALeftStack[AStackPos] := j + 1;
            ARightStack[AStackPos] := ARight;
          end;
          ARight := j - 1;
{$IFDEF COMPARE_LENGTH}
        end
        else
        begin
          if (j - 1) > ALeft then
          begin
            inc(AStackPos);
            if AStackPos > (AStackSize - 1) then
            begin
              AStackSize := AStackSize + 2048;
              ReallocMem(ALeftStack, AStackSize * SizeOf(integer));
              ReallocMem(ARightStack, AStackSize * SizeOf(integer));
            end;
            ALeftStack[AStackPos] := ALeft;
            ARightStack[AStackPos] := j - 1;
          end;
          ALeft := j + 1;
        end;
{$ENDIF}
      end;
      if ALeft < ARight then
        Insert(ALeft, ARight);
    end;
    FreeMem(ALeftStack);
    FreeMem(ARightStack);
  end;
begin
  if (AEnd - AStart) > 12 then
    InternalSort(AStart, AEnd)
  else
    Insert(AStart, AEnd);
  for k := AStart to AEnd do
    FOrder[FIndex[k]] := k;
end;

procedure TfcxOrderList.SortRandomMiddle(AStart, AEnd: integer);
var k: integer;
  procedure Insert(const AILeft, AIRight: integer);
  var
    i1, j1: integer;
    AIObject: integer;
  begin
    for i1 := AILeft + 1 to AIRight do
    begin
      AIObject := FIndex[i1];
      for j1 := i1 - 1 downto AILeft do
      begin
        if InternalCompare(FIndex[j1], AIObject) <= 0 then
          break;
        FIndex[j1 + 1] := FIndex[j1];
      end;
      FIndex[j1 + 1] := AIObject;
    end;
  end;

  procedure InternalSort(ASLeft, ASRight: Integer);
  var
    i, j, AMiddle: integer;
    AObject1, AObject2: integer;
    ALeftStack, ARightStack: PfcxIntegerArray;
    AStackSize: integer;
    AStackPos: integer;
    ALeft, ARight: Integer;
  begin
    AStackSize := 2048;
    GetMem(ALeftStack, AStackSize * SizeOf(integer));
    GetMem(ARightStack, AStackSize * SizeOf(integer));
    AStackPos := 0;
    ALeftStack[0] := ASLeft;
    ARightStack[0] := ASRight;
    while AStackPos >= 0 do
    begin
      ALeft := ALeftStack[AStackPos];
      ARight := ARightStack[AStackPos];
      dec(AStackPos);
      while (ARight - ALeft) > 12 do
      begin
        AMiddle := Random(ARight - ALeft) + ALeft;
        AObject1 := FIndex[AMiddle];
        FIndex[AMiddle] := FIndex[ALeft];
        i := succ(ALeft);
        j := ARight;
        while true do
        begin
          while (i < j) and (InternalCompare(AObject1, FIndex[i]) > 0) do
            inc(i);
          while (j >= i) and (InternalCompare(FIndex[j], AObject1) > 0) do
            dec(j);
          if (i >= j) then
            break;
          AObject2 := FIndex[i];
          FIndex[i] := FIndex[j];
          FIndex[j] := AObject2;
          dec(j);
          inc(i);
        end;
        FIndex[ALeft] := FIndex[j];
        FIndex[j] := AObject1;
{$IFDEF COMPARE_LENGTH}
        if (j - ALeft) <= (ARight - j) then
        begin
{$ENDIF}
          if (j + 1) < ARight then
          begin
            inc(AStackPos);
            if AStackPos > (AStackSize - 1) then
            begin
              AStackSize := AStackSize + 2048;
              ReallocMem(ALeftStack, AStackSize * SizeOf(integer));
              ReallocMem(ARightStack, AStackSize * SizeOf(integer));
            end;
            ALeftStack[AStackPos] := j + 1;
            ARightStack[AStackPos] := ARight;
          end;
          ARight := j - 1;
{$IFDEF COMPARE_LENGTH}
        end
        else
        begin
          if (j - 1) > ALeft then
          begin
            inc(AStackPos);
            if AStackPos > (AStackSize - 1) then
            begin
              AStackSize := AStackSize + 2048;
              ReallocMem(ALeftStack, AStackSize * SizeOf(integer));
              ReallocMem(ARightStack, AStackSize * SizeOf(integer));
            end;
            ALeftStack[AStackPos] := ALeft;
            ARightStack[AStackPos] := j - 1;
          end;
          ALeft := j + 1;
        end;
{$ENDIF}
      end;
      if ALeft < ARight then
        Insert(ALeft, ARight);
    end;
    FreeMem(ALeftStack);
    FreeMem(ARightStack);
  end;

begin
  if (AEnd - AStart) > 12 then
  begin
    Randomize;
    InternalSort(AStart, AEnd)
  end
  else
    Insert(AStart, AEnd);
  for k := AStart to AEnd do
    FOrder[FIndex[k]] := k;
end;

end.


