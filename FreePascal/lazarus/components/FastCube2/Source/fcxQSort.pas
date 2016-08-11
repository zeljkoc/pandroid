{*******************************************************}
{                                                       }
{              FastCube 2 QuickSort Unit                }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxQSort;
{$include fcx.inc}
{$DEFINE COMPARE_LENGTH}

interface
uses
  fcxTypes;
//FMX uses
{$ELSE}
{$include fcx.inc}
{$DEFINE COMPARE_LENGTH}

interface
uses
  FMX.fcxTypes;
{$ENDIF}

type
  TfcxSortPointerCompare = function(AItem1, AItem2: Pointer): Integer of Object;

  TfcxQSStack = ^_fcxQSStack;
  _fcxQSStack = record
    Size: integer;
    Left, Right: PfcxIntegerArray;
  end;

  procedure SortRecurse(AList: PfcxPointerArray; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
  procedure SortNoRecurse(AList: PfcxPointerArray; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
  procedure SortNoRecurseGlobalStack(AList: PfcxPointerArray; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare; AStack: TfcxQSStack);
  procedure SortNoRecurseRandomMiddle(AList: PfcxPointerArray; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
  procedure SortNoRecurseNotSwapEqual(AList: PfcxPointerArray; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);

implementation

  procedure SortRecurse(AList: PfcxPointerArray; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
    procedure InternalSort(ALeft, ARight: Integer);
      procedure Insert(const AILeft, AIRight: integer);
      var
        i, j: integer;
        Key: Pointer;
      begin
        for i := AILeft + 1 to AIRight do
        begin
          Key := AList[i];
          j := i - 1;
          while (j >= 0) and (Compare(AList[j], Key) > 0) do
          begin
            AList[j + 1] := AList[j];
            dec(j);
          end;
          AList[j + 1] := Key;
        end;
      end;
    var
      i, j, AMiddle: integer;
      AObject1, AObject2: Pointer;
    begin
      while (ARight - ALeft) > 12 do
      begin
        AMiddle := (ALeft + ARight) shr 1;
        AObject1 := AList[AMiddle];
        AList[AMiddle] := AList[ALeft];
        i := succ(ALeft);
        j := ARight;
        while true do
        begin
          while (i < j) and (Compare(AObject1, AList[i]) > 0) do
            inc(i);
          while (j >= i) and (Compare(AList[j], AObject1) > 0) do
            dec(j);
          if (i >= j) then
            break;
          AObject2 := AList[i];
          AList[i] := AList[j];
          AList[j] := AObject2;
          dec(j);
          inc(i);
        end;
        AList[ALeft] := AList[j];
        AList[j] := AObject1;
        if ((j - ALeft) <= (ARight - j)) then
        begin
          InternalSort(ALeft, j - 1);
          ALeft := succ(j);
        end
        else
        begin
          InternalSort(j + 1, ARight);
          ARight := pred(j);
        end;
      end;
      if ALeft < ARight then
        Insert(ALeft, ARight);
    end;
  begin
    InternalSort(AStart, AEnd);
  end;

  procedure SortNoRecurse(AList: PfcxPointerArray; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
    procedure Insert(const AILeft, AIRight: integer);
    var
      i, j: integer;
      Key: Pointer;
    begin
      for i := AILeft + 1 to AIRight do
      begin
        Key := AList[i];
        j := i - 1;
        while (j >= 0) and (Compare(AList[j], Key) > 0) do
        begin
          AList[j + 1] := AList[j];
          dec(j);
        end;
        AList[j + 1] := Key;
      end;
    end;

    procedure InternalSort(ASLeft, ASRight: Integer);
    var
      i, j, AMiddle: integer;
      AObject1, AObject2: pointer;
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
          AObject1 := AList[AMiddle];
          AList[AMiddle] := AList[ALeft];
          i := succ(ALeft);
          j := ARight;
          while true do
          begin
            while (i < j) and (Compare(AObject1, AList[i]) > 0) do
              inc(i);
            while (j >= i) and (Compare(AList[j], AObject1) > 0) do
              dec(j);
            if (i >= j) then
              break;
            AObject2 := AList[i];
            AList[i] := AList[j];
            AList[j] := AObject2;
            dec(j);
            inc(i);
          end;
          AList[ALeft] := AList[j];
          AList[j] := AObject1;
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
  end;

  procedure SortNoRecurseGlobalStack(AList: PfcxPointerArray; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare; AStack: TfcxQSStack);
    procedure Insert(const AILeft, AIRight: integer);
    var
      i, j: integer;
      Key: Pointer;
    begin
      for i := AILeft + 1 to AIRight do
      begin
        Key := AList[i];
        j := i - 1;
        while (j >= 0) and (Compare(AList[j], Key) > 0) do
        begin
          AList[j + 1] := AList[j];
          dec(j);
        end;
        AList[j + 1] := Key;
      end;
    end;

    procedure InternalSort(ASLeft, ASRight: Integer);
    var
      i, j, AMiddle: integer;
      AObject1, AObject2: pointer;
//      ALeftStack, ARightStack: PfcxIntegerArray;
//      AStackSize: integer;
      AStackPos: integer;
      ALeft, ARight: Integer;
    begin
//      AStackSize := 2048;
//      GetMem(ALeftStack, AStackSize * SizeOf(integer));
//      GetMem(ARightStack, AStackSize * SizeOf(integer));
      AStackPos := 0;
      AStack.Left[0] := ASLeft;
      AStack.Right[0] := ASRight;
      while AStackPos >= 0 do
      begin
        ALeft := AStack.Left[AStackPos];
        ARight := AStack.Right[AStackPos];
        dec(AStackPos);
        while (ARight - ALeft) > 12 do
        begin
          AMiddle := (ALeft + ARight) shr 1;
          AObject1 := AList[AMiddle];
          AList[AMiddle] := AList[ALeft];
          i := succ(ALeft);
          j := ARight;
          while true do
          begin
            while (i < j) and (Compare(AObject1, AList[i]) > 0) do
              inc(i);
            while (j >= i) and (Compare(AList[j], AObject1) > 0) do
              dec(j);
            if (i >= j) then
              break;
            AObject2 := AList[i];
            AList[i] := AList[j];
            AList[j] := AObject2;
            dec(j);
            inc(i);
          end;
          AList[ALeft] := AList[j];
          AList[j] := AObject1;
{$IFDEF COMPARE_LENGTH}
          if (j - ALeft) <= (ARight - j) then
          begin
{$ENDIF}
            if (j + 1) < ARight then
            begin
              inc(AStackPos);
              if AStackPos > (AStack.Size - 1) then
              begin
                AStack.Size := AStack.Size + 2048;
                ReallocMem(AStack.Left, AStack.Size * SizeOf(integer));
                ReallocMem(AStack.Right, AStack.Size * SizeOf(integer));
              end;
              AStack.Left[AStackPos] := j + 1;
              AStack.Right[AStackPos] := ARight;
            end;
            ARight := j - 1;
{$IFDEF COMPARE_LENGTH}
          end
          else
          begin
            if (j - 1) > ALeft then
            begin
              inc(AStackPos);
              if AStackPos > (AStack.Size - 1) then
              begin
                AStack.Size := AStack.Size + 2048;
                ReallocMem(AStack.Left, AStack.Size * SizeOf(integer));
                ReallocMem(AStack.Right, AStack.Size * SizeOf(integer));
              end;
              AStack.Left[AStackPos] := ALeft;
              AStack.Right[AStackPos] := j - 1;
            end;
            ALeft := j + 1;
          end;
{$ENDIF}
        end;
        if ALeft < ARight then
          Insert(ALeft, ARight);
      end;
//      FreeMem(ALeftStack);
//      FreeMem(ARightStack);
    end;
  begin
    if (AEnd - AStart) > 12 then
      InternalSort(AStart, AEnd)
    else
      Insert(AStart, AEnd);
  end;

  procedure SortNoRecurseRandomMiddle(AList: PfcxPointerArray; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
    procedure Insert(const AILeft, AIRight: integer);
    var
      i, j: integer;
      Key: Pointer;
    begin
      for i := AILeft + 1 to AIRight do
      begin
        Key := AList[i];
        j := i - 1;
        while (j >= 0) and (Compare(AList[j], Key) > 0) do
        begin
          AList[j + 1] := AList[j];
          dec(j);
        end;
        AList[j + 1] := Key;
      end;
    end;

    procedure InternalSort(ASLeft, ASRight: Integer);
    var
      i, j, AMiddle: integer;
      AObject1, AObject2: pointer;
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
          AObject1 := AList[AMiddle];
          AList[AMiddle] := AList[ALeft];
          i := succ(ALeft);
          j := ARight;
          while true do
          begin
            while (i < j) and (Compare(AObject1, AList[i]) > 0) do
              inc(i);
            while (j >= i) and (Compare(AList[j], AObject1) > 0) do
              dec(j);
            if (i >= j) then
              break;
            AObject2 := AList[i];
            AList[i] := AList[j];
            AList[j] := AObject2;
            dec(j);
            inc(i);
          end;
          AList[ALeft] := AList[j];
          AList[j] := AObject1;
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
  end;

  procedure SortNoRecurseNotSwapEqual(AList: PfcxPointerArray; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
    procedure Insert(const AILeft, AIRight: integer);
    var
      i, j: integer;
      Key: Pointer;
    begin
      for i := AILeft + 1 to AIRight do
      begin
        Key := AList[i];
        j := i - 1;
        while (j >= 0) and (Compare(AList[j], Key) > 0) do
        begin
          AList[j + 1] := AList[j];
          dec(j);
        end;
        AList[j + 1] := Key;
      end;
    end;

    procedure InternalSort(ASLeft, ASRight: Integer);
    var
      i, j, AMiddle: integer;
      AObject1, AObject2: pointer;
      ALeftStack, ARightStack: PfcxIntegerArray;
      AStackSize: integer;
      AStackPos: integer;
      ALeft, ARight: Integer;
      ARes1, ARes2: Integer;
    begin
      AStackSize := 2048;
      GetMem(ALeftStack, AStackSize * SizeOf(integer));
      GetMem(ARightStack, AStackSize * SizeOf(integer));
      AStackPos := 0;
      ALeftStack[0] := ASLeft;
      ARightStack[0] := ASRight;
      ARes1 := 0;
      ARes2 := 0;
      while AStackPos >= 0 do
      begin
        ALeft := ALeftStack[AStackPos];
        ARight := ARightStack[AStackPos];
        dec(AStackPos);
        while (ARight - ALeft) > 12 do
        begin
          AMiddle := (ALeft + ARight) shr 1;
          AObject1 := AList[AMiddle];
          AList[AMiddle] := AList[ALeft];
          i := succ(ALeft);
          j := ARight;
          while true do
          begin
            while (i < j) do
            begin
              ARes1 := Compare(AObject1, AList[i]); 
              if ARes1 <= 0 then
                break;
              inc(i);
            end;
            while (j >= i) do
            begin
              ARes2 := Compare(AList[j], AObject1); 
              if ARes2 <= 0 then
                break;
              dec(j);
            end;
            if (i >= j) then
              break;
            if (ARes1 <> 0) or (ARes2 <> 0) then
            begin
              AObject2 := AList[i];
              AList[i] := AList[j];
              AList[j] := AObject2;
            end;
            dec(j);
            inc(i);
          end;
          AList[ALeft] := AList[j];
          AList[j] := AObject1;
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
  end;
end.
