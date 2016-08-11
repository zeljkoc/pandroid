{*******************************************************}
{                                                       }
{           FastCube 2 QuickSort Templates              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcQSortTemplate;
{$DEFINE COMPARE_LENGTH}

interface

implementation

type
  _fcxPointerArray = array[0..0] of Pointer;
  PfcxPointerArray = ^_fcxPointerArray;
  _fcxIntegerArray = array[0..0] of Integer;
  PfcxIntegerArray = ^_fcxIntegerArray;

  TfcxSortPointerCompare = function(AItem1, AItem2: Pointer): Integer of Object;

  function SampleCompare(AItem1, AItem2: Pointer): integer;
  begin
    if integer(AItem1) > integer(AItem2) then
      Result := 1
    else if integer(AItem1) < integer(AItem2) then
      Result := -1
    else
      Result := 0;
  end;

// быстрая сортировка с использованием частичной рекурсии
  procedure SortRecurse(AList: PfcxPointerArray {тут менять}; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
    procedure InternalSort(ALeft, ARight: Integer);
    var
      i, j, AMiddle: integer;
      AObject1, AObject2: Pointer; {тут менять}

      procedure Insert(const AILeft, AIRight: integer);
      var
        i1, j1: integer;
        AIObject: Pointer; {тут менять}
      begin
        for i1 := AILeft + 1 to AIRight do
        begin

          AIObject := AList[i1]; {тут менять}
          for j1 := i1 - 1 downto AILeft do
          begin
            if Compare(AList[j1], AIObject) <= 0 then {тут менять}
              break;
            AList[j1 + 1] := AList[j1]; {тут менять}
          end;
          AList[j1 + 1] := AIObject; {тут менять}
        end;
      end;

    begin
      while (ARight - ALeft) > 12 do
      begin
        AMiddle := (ALeft + ARight) shr 1;
        AObject1 := AList[AMiddle]; {тут менять}
        AList[AMiddle] := AList[ALeft]; {тут менять}
        i := succ(ALeft);
        j := ARight;
        while true do
        begin
          while (i < j) and (Compare(AObject1, AList[i]) > 0) do {тут менять}
            inc(i);
          while (j >= i) and (Compare(AList[j], AObject1) > 0) do {тут менять}
            dec(j);
          if (i >= j) then
            break;
          AObject2 := AList[i]; {тут менять}
          AList[i] := AList[j]; {тут менять}
          AList[j] := AObject2; {тут менять}
          dec(j);
          inc(i);
        end;
        AList[ALeft] := AList[j]; {тут менять}
        AList[j] := AObject1; {тут менять}
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

// быстрая сортировка без использования рекурсии
  procedure SortNoRecurse(AList: PfcxPointerArray {тут менять}; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
    procedure Insert(const AILeft, AIRight: integer);
    var
      i1, j1: integer;
      AIObject: Pointer; {тут менять}
    begin
      for i1 := AILeft + 1 to AIRight do
      begin

        AIObject := AList[i1]; {тут менять}
        for j1 := i1 - 1 downto AILeft do
        begin
          if Compare(AList[j1], AIObject) <= 0 then {тут менять}
            break;
          AList[j1 + 1] := AList[j1]; {тут менять}
        end;
        AList[j1 + 1] := AIObject; {тут менять}
      end;
    end;

    procedure InternalSort(ASLeft, ASRight: Integer);
    var
      i, j, AMiddle: integer;
      AObject1, AObject2: pointer; {тут менять}
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
          AObject1 := AList[AMiddle]; {тут менять}
          AList[AMiddle] := AList[ALeft]; {тут менять}
          i := succ(ALeft);
          j := ARight;
          while true do
          begin
            while (i < j) and (Compare(AObject1, AList[i]) > 0) do {тут менять}
              inc(i);
            while (j >= i) and (Compare(AList[j], AObject1) > 0) do {тут менять}
              dec(j);
            if (i >= j) then
              break;
            AObject2 := AList[i]; {тут менять}
            AList[i] := AList[j]; {тут менять}
            AList[j] := AObject2; {тут менять}
            dec(j);
            inc(i);
          end;
          AList[ALeft] := AList[j]; {тут менять}
          AList[j] := AObject1; {тут менять}
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

// быстрая сортировка без использования рекурсии с модификацией:
// если оба элемента обмена равны срединному, то обмен не производится.
// дает ускорение при наличии повторяющихся ключей.
  procedure SortNoRecurseNotSwapEqual(AList: PfcxPointerArray {тут менять}; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
    procedure Insert(const AILeft, AIRight: integer);
    var
      i1, j1: integer;
      AIObject: Pointer; {тут менять}
    begin
      for i1 := AILeft + 1 to AIRight do
      begin

        AIObject := AList[i1]; {тут менять}
        for j1 := i1 - 1 downto AILeft do
        begin
          if Compare(AList[j1], AIObject) <= 0 then {тут менять}
            break;
          AList[j1 + 1] := AList[j1]; {тут менять}
        end;
        AList[j1 + 1] := AIObject; {тут менять}
      end;
    end;

    procedure InternalSort(ASLeft, ASRight: Integer);
    var
      i, j, AMiddle: integer;
      AObject1, AObject2: pointer; {тут менять}
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
          AObject1 := AList[AMiddle]; {тут менять}
          AList[AMiddle] := AList[ALeft]; {тут менять}
          i := succ(ALeft);
          j := ARight;
          while true do
          begin
            while (i < j) do
            begin
              ARes1 := Compare(AObject1, AList[i]);  {тут менять}
              if ARes1 <= 0 then
                break;
              inc(i);
            end;
            while (j >= i) do
            begin
              ARes2 := Compare(AList[j], AObject1);  {тут менять}
              if ARes2 <= 0 then
                break;
              dec(j);
            end;
            if (i >= j) then
              break;
            if (ARes1 <> 0) or (ARes2 <> 0) then
            begin
              AObject2 := AList[i]; {тут менять}
              AList[i] := AList[j]; {тут менять}
              AList[j] := AObject2; {тут менять}
            end;
            dec(j);
            inc(i);
          end;
          AList[ALeft] := AList[j]; {тут менять}
          AList[j] := AObject1; {тут менять}
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

  procedure SortRecurseComments(AList: PfcxPointerArray {тут менять}; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
    procedure InternalSort(ALeft, ARight: Integer);
    var
      i, j, AMiddle: integer;
      AObject1, AObject2: Pointer; {тут менять}

      procedure Insert(const AILeft, AIRight: integer);
      var
        i1, j1: integer;
        AIObject: Pointer; {тут менять}
      begin
        for i1 := AILeft + 1 to AIRight do
        begin

          AIObject := AList[i1]; {тут менять}
          for j1 := i1 - 1 downto AILeft do
          begin
            if Compare(AList[j1], AIObject) <= 0 then {тут менять}
              break;
            AList[j1 + 1] := AList[j1]; {тут менять}
          end;
          AList[j1 + 1] := AIObject; {тут менять}
        end;
      end;

    begin
// цикл, пока число элементов больше 12. если 12 и меньше, то сортировка вставкой
      while (ARight - ALeft) > 12 do
      begin
// середина
        AMiddle := (ALeft + ARight) shr 1;
// сохраняем центральный элемент
        AObject1 := AList[AMiddle]; {тут менять}
// на место центрального элемента зачем-то пишем левый крайний элемент ??
        AList[AMiddle] := AList[ALeft]; {тут менять}
// раскидываем элементы [ALeft+1..ARight] относительно центрального
        i := succ(ALeft);
        j := ARight;
        while true do
        begin
// ищем слева элемент, больший или равный центральному. при этом смотрим, что-бы не забраться НА встречный элемент [j]
          while (i < j) and (Compare(AObject1, AList[i]) > 0) do {тут менять}
            inc(i);
// ищем справа элемент, меньший или равный центральному. при этом смотрим, что-бы не забраться ЗА встречный элемент [i]
          while (j >= i) and (Compare(AList[j], AObject1) > 0) do {тут менять}
            dec(j);
// Найден один и тот-же элемент или забрались ЗА встречный элемент. хватит делать расстановку.
          if (i >= j) then
            break;
// Найдено два элемент. меняем их местами. SWAP
          AObject2 := AList[i]; {тут менять}
          AList[i] := AList[j]; {тут менять}
          AList[j] := AObject2; {тут менять}
// переходим к следующим элементам. (сдвигаем указатели к центру).
          dec(j);
          inc(i);
        end;
// Вышли из цикла, значит расстановка сделана.
// элемент [j+1] - больше, чем срединный элемент, а [j-1] - меньше
// а элемент [j] - тоже меньше, чем срединный элемент. Поэтому:
// Переносим элемент [j] на позицию левого крайнего [ALeft].
        AList[ALeft] := AList[j]; {тут менять}
// а на его место записываем наш сохраненный срединный
        AList[j] := AObject1; {тут менять}
// в результате на промежутке [ALeft..ARight] у нас раскиданы элементы относительно позиции j:
// слева меньше или равные, а справа больше или равные елементу [j]
// дальше надо раскидать соответственно левую и правую половины
// для уменьшения рекурсии вызываем InternalSort для меньшей половины. а большую продолжаем кидать в цикле.
        if ((j - ALeft) <= (ARight - j)) then
        begin
// меньшая половина слева. вызываем InternalSort для неё: [ALeft, j - 1]
          InternalSort(ALeft, j - 1);
// большая половина справа. устанавливаем для текущего участка новую левую границу
          ALeft := succ(j);
        end
        else
        begin
// меньшая половина справа. вызываем InternalSort для неё: [j + 1, ARight]
          InternalSort(j + 1, ARight);
// большая половина слева. устанавливаем для текущего участка новую правую границу
          ARight := pred(j);
        end;
      end;
// сортировка вставкой остатка. можно сделать проверку на ALeft < ARight, чтобы избежать вызова для одного элемента
      if ALeft < ARight then
        Insert(ALeft, ARight);
    end;
  begin
    InternalSort(AStart, AEnd);
  end;

  procedure SortNoRecurseComments(AList: PfcxPointerArray {тут менять}; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
    procedure Insert(const AILeft, AIRight: integer);
    var
      i1, j1: integer;
      AIObject: Pointer; {тут менять}
    begin
      for i1 := AILeft + 1 to AIRight do
      begin

        AIObject := AList[i1]; {тут менять}
        for j1 := i1 - 1 downto AILeft do
        begin
          if Compare(AList[j1], AIObject) <= 0 then {тут менять}
            break;
          AList[j1 + 1] := AList[j1]; {тут менять}
        end;
        AList[j1 + 1] := AIObject; {тут менять}
      end;
    end;

    procedure InternalSort(ASLeft, ASRight: Integer);
    var
      i, j, AMiddle: integer;
      AObject1, AObject2: pointer; {тут менять}
      ALeftStack, ARightStack: PfcxIntegerArray;
      AStackSize: integer;
      AStackPos: integer;
      ALeft, ARight: Integer;
    begin
// выделим память под стек
      AStackSize := 2048;
      GetMem(ALeftStack, AStackSize * SizeOf(integer));
      GetMem(ARightStack, AStackSize * SizeOf(integer));

// запишем в стек основной промежуток
      AStackPos := 0;
      ALeftStack[0] := ASLeft;
      ARightStack[0] := ASRight;
// цикл, пока есть претендены на разбор
      while AStackPos >= 0 do
      begin
// границы промежутка
        ALeft := ALeftStack[AStackPos];
        ARight := ARightStack[AStackPos];
// сдвигаем указатель стека назад
        dec(AStackPos);
// цикл, пока число элементов больше 12. если 12 и меньше, то сортировка вставкой
        while (ARight - ALeft) > 12 do
        begin
// середина
          AMiddle := (ALeft + ARight) shr 1;
// сохраняем центральный элемент
          AObject1 := AList[AMiddle]; {тут менять}
// на место центрального элемента зачем-то пишем левый крайний элемент ??
          AList[AMiddle] := AList[ALeft]; {тут менять}
// раскидываем элементы [ALeft+1..ARight] относительно центрального
          i := succ(ALeft);
          j := ARight;
          while true do
          begin
// ищем слева элемент, больший или равный центральному. при этом смотрим, что-бы не забраться НА встречный элемент [j]
            while (i < j) and (Compare(AObject1, AList[i]) > 0) do {тут менять}
              inc(i);
// ищем справа элемент, меньший или равный центральному. при этом смотрим, что-бы не забраться ЗА встречный элемент [i]
            while (j >= i) and (Compare(AList[j], AObject1) > 0) do {тут менять}
              dec(j);
// Найден один и тот-же элемент или забрались ЗА встречный элемент. хватит делать расстановку. выходим из цикла.
            if (i >= j) then
              break;
// Найдено два элемент. меняем их местами. SWAP
            AObject2 := AList[i]; {тут менять}
            AList[i] := AList[j]; {тут менять}
            AList[j] := AObject2; {тут менять}
// переходим к следующим элементам. (сдвигаем указатели к центру).
            dec(j);
            inc(i);
          end;
// Вышли из цикла, значит расстановка сделана.
// элемент [j+1] - больше или равен срединному элементу, а [j-1] - меньше или равен
// а элемент [j] - тоже меньше или равен срединному элементу, а нам надо, что-бы был равен. Поэтому:
// Переносим элемент [j] на позицию левого крайнего [ALeft].
          AList[ALeft] := AList[j]; {тут менять}
// а на его место записываем наш сохраненный срединный
          AList[j] := AObject1; {тут менять}
// теперь j указывает на разделитель левого и правого промежутков,
// в результате на промежутке [ALeft..ARight] у нас раскиданы элементы относительно позиции j:
// слева меньше или равные, а справа больше или равные елементу [j]
// дальше надо раскидать соответственно левую и правую половины
// правую половину запишем в стек (по уму надо писать в стек бОльшую половину, а меньшую сразу разбирать, но попробуем съэкономить на сравнениях)
          if (j + 1) < ARight then
          begin
// если больше чем 1 элемент в правой половине промежутка
// то записываем правый промежуток в стек для дальнейшего разбора
            inc(AStackPos);
            if AStackPos > (AStackSize - 1) then
            begin
// увеличиваем размер стека, если надо
              AStackSize := AStackSize + 2048;
              ReallocMem(ALeftStack, AStackSize * SizeOf(integer));
              ReallocMem(ARightStack, AStackSize * SizeOf(integer));
            end;
            ALeftStack[AStackPos] := j + 1;
            ARightStack[AStackPos] := ARight;
          end;
// а левую продолжим разбирать
// устанавливаем правую границу для разбора левой половины промежутка
          ARight := j - 1;
        end;
// сортировка вставкой остатка для ускорения. можно сделать проверку на ALeft < ARight, чтобы избежать вызова для одного элемента
        if ALeft < ARight then
          Insert(ALeft, ARight);
      end;
// освобождение стека
      FreeMem(ALeftStack);
      FreeMem(ARightStack);
    end;
  begin
    if (AEnd - AStart) > 12 then
      InternalSort(AStart, AEnd)
    else
      Insert(AStart, AEnd);
  end;

  procedure SortNoRecurseNotSwapEqualComments(AList: PfcxPointerArray {тут менять}; AStart, AEnd: Integer; Compare: TfcxSortPointerCompare);
    procedure Insert(const AILeft, AIRight: integer);
    var
      i1, j1: integer;
      AIObject: Pointer; {тут менять}
    begin
      for i1 := AILeft + 1 to AIRight do
      begin

        AIObject := AList[i1]; {тут менять}
        for j1 := i1 - 1 downto AILeft do
        begin
          if Compare(AList[j1], AIObject) <= 0 then {тут менять}
            break;
          AList[j1 + 1] := AList[j1]; {тут менять}
        end;
        AList[j1 + 1] := AIObject; {тут менять}
      end;
    end;

    procedure InternalSort(ASLeft, ASRight: Integer);
    var
      i, j, AMiddle: integer;
      AObject1, AObject2: pointer; {тут менять}
      ALeftStack, ARightStack: PfcxIntegerArray;
      AStackSize: integer;
      AStackPos: integer;
      ALeft, ARight: Integer;
      ARes1, ARes2: Integer;
    begin
// выделим память под стек
      AStackSize := 2048;
      GetMem(ALeftStack, AStackSize * SizeOf(integer));
      GetMem(ARightStack, AStackSize * SizeOf(integer));

// запишем в стек основной промежуток
      AStackPos := 0;
      ALeftStack[0] := ASLeft;
      ARightStack[0] := ASRight;
      ARes1 := 0;
      ARes2 := 0;
// цикл, пока есть претендены на разбор
      while AStackPos >= 0 do
      begin
// границы промежутка
        ALeft := ALeftStack[AStackPos];
        ARight := ARightStack[AStackPos];
// сдвигаем указатель стека назад
        dec(AStackPos);
// цикл, пока число элементов больше 12. если 12 и меньше, то сортировка вставкой
        while (ARight - ALeft) > 12 do
        begin
// середина
          AMiddle := (ALeft + ARight) shr 1;
// сохраняем центральный элемент
          AObject1 := AList[AMiddle]; {тут менять}
// на место центрального элемента зачем-то пишем левый крайний элемент ??
          AList[AMiddle] := AList[ALeft]; {тут менять}
// раскидываем элементы [ALeft+1..ARight] относительно центрального
          i := succ(ALeft);
          j := ARight;
          while true do
          begin
// ищем слева элемент, больший или равный центральному. при этом смотрим, что-бы не забраться НА встречный элемент [j]
            while (i < j) do {тут менять}
            begin
              ARes1 := Compare(AObject1, AList[i]);
              if ARes1 <= 0 then
                break;
              inc(i);
            end;
// ищем справа элемент, меньший или равный центральному. при этом смотрим, что-бы не забраться ЗА встречный элемент [i]
            while (j >= i) do {тут менять}
            begin
              ARes2 := Compare(AList[j], AObject1);
              if ARes2 <= 0 then
                break;
              dec(j);
            end;
// Найден один и тот-же элемент или забрались ЗА встречный элемент. хватит делать расстановку. выходим из цикла.
            if (i >= j) then
              break;
// Найдено два элемент. меняем их местами. SWAP
            if (ARes1 <> 0) or (ARes2 <> 0) then
            begin
// Но только, если они не равны
              AObject2 := AList[i]; {тут менять}
              AList[i] := AList[j]; {тут менять}
              AList[j] := AObject2; {тут менять}
            end;
// переходим к следующим элементам. (сдвигаем указатели к центру).
            dec(j);
            inc(i);
          end;
// Вышли из цикла, значит расстановка сделана.
// элемент [j+1] - больше или равен срединному элементу, а [j-1] - меньше или равен
// а элемент [j] - тоже меньше или равен срединному элементу, а нам надо, что-бы был равен. Поэтому:
// Переносим элемент [j] на позицию левого крайнего [ALeft].
          AList[ALeft] := AList[j]; {тут менять}
// а на его место записываем наш сохраненный срединный
          AList[j] := AObject1; {тут менять}
// теперь j указывает на разделитель левого и правого промежутков,
// в результате на промежутке [ALeft..ARight] у нас раскиданы элементы относительно позиции j:
// слева меньше или равные, а справа больше или равные елементу [j]
// дальше надо раскидать соответственно левую и правую половины
// правую половину запишем в стек (по уму надо писать в стек бОльшую половину, а меньшую сразу разбирать, но попробуем съэкономить на сравнениях)
          if (j + 1) < ARight then
          begin
// если больше чем 1 элемент в правой половине промежутка
// то записываем правый промежуток в стек для дальнейшего разбора
            inc(AStackPos);
            if AStackPos > (AStackSize - 1) then
            begin
// увеличиваем размер стека, если надо
              AStackSize := AStackSize + 2048;
              ReallocMem(ALeftStack, AStackSize * SizeOf(integer));
              ReallocMem(ARightStack, AStackSize * SizeOf(integer));
            end;
            ALeftStack[AStackPos] := j + 1;
            ARightStack[AStackPos] := ARight;
          end;
// а левую продолжим разбирать
// устанавливаем правую границу для разбора левой половины промежутка
          ARight := j - 1;
        end;
// сортировка вставкой остатка для ускорения. можно сделать проверку на ALeft < ARight, чтобы избежать вызова для одного элемента
        if ALeft < ARight then
          Insert(ALeft, ARight);
      end;
// освобождение стека
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
