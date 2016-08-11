{*******************************************************}
{                                                       }
{              FastCube 2 Sort Utils unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxSort;
{$INCLUDE fcx.inc}

interface
uses
  Classes, SysUtils,
{$IFDEF FPC}
  LCLType,
{$ELSE}
  Windows,
{$ENDIF}
  fcxTypes, fcxUniqueArray, fcxUniqueValue;
//FMX uses
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  System.Classes, System.SysUtils,
  FMX.fcxTypes, FMX.fcxUniqueArray, FMX.fcxUniqueValue;
{$ENDIF FMX}

Type
  TfcxCompareFunc =  function(const AValue1, AValue2: Pointer): Integer of Object;

procedure SortUniqueList(AStart, AEnd: integer; AList: PfcxCUVArray; ACompare: TfcxCompareFunc; ANullValue: PfcxCommonUV; AExtraList: PfcxArrPointerArray; AExtraCount: integer; AOrderIndex: integer);
//procedure SortStringUniqueList(AStart, AEnd: integer; AList: PfcxCUVArray; AExtraList: PfcxPointerArray);
//procedure SortStringUniqueList(AStart, AEnd: integer; AList: PfcxPointerArray; ASplitList: PfcxArrPointerArray; AIndexOrderField: integer);
//procedure SortUniqueList(AStart, AEnd: integer; AList: PfcxPointerArray; ASplitList: PfcxArrPointerArray; ACompare: TfcxCompareFunc; AIndexOrderField: integer);

implementation

procedure SortUniqueList(AStart, AEnd: integer; AList: PfcxCUVArray; ACompare: TfcxCompareFunc; ANullValue: PfcxCommonUV; AExtraList: PfcxArrPointerArray; AExtraCount: integer; AOrderIndex: integer);
var  x1s: PfcxPointerArray;
  procedure QuickSortOL(L, R: Integer);
  var
    i, j, p: integer;
    v, b: PfcxCommonUV;

    procedure Insert(const l1, n1: integer);
    var
      i1, j1, v1: integer;
      x1: PfcxCommonUV;

    begin
      for i1 := l1 + 1 to n1 do
      begin
        x1 := AList[i1];
        v1 := l1;
        for j1 := i1 - 1 downto l1 do
        begin
          if ACompare(AList[j1], x1) <= 0 then
          begin
            v1 := j1 + 1;
            break;
          end;
          AList[j1 + 1] := AList[j1];

        end;
        AList[v1] := x1;
      end;
    end;

  begin
    while (l < r) do
    begin
      if r - l <= 12 then
      begin
        Insert(l, r);
        exit;
      end;

      p := (l + r) shr 1;
// вопрос поиска середины - не ипользовать ли random?
      V := AList[p];

      AList[p] := AList[l];


      //     сортируем l+1..r относительно центра
      i := succ(l);
      j := r;
      while true do
      begin
        while (i < j) and (ACompare(v, AList[i]) > 0) do
          inc(i);
        while (j >= i) and (ACompare(AList[j], v) > 0) do
          dec(j);
        if (i >= j) then
          break;
        b := AList[i];

        AList[i] := AList[j];
        AList[j] := b;


        dec(j);
        inc(i);
      end;
      //     центр в a[j]
      AList[l] := AList[j];
      AList[j] := v;


      if ((j - l) <= (r - j)) then
      begin
        QuickSortOL(l, j - 1);
        l := succ(j);
      end
      else
      begin
        QuickSortOL(j + 1, r);
        r := pred(J);
      end;
    end;
  end;
  procedure QuickSortOLWithExtraList(L, R: Integer);
  var
    i, j, p, AExtraIndex: integer;
    v, b: PfcxCommonUV;
    bs: Pointer;
    procedure Insert(const l1, n1: integer);
    var
      i1, j1, v1, AExtraIndex: integer;
      x1: PfcxCommonUV;
    begin
      for i1 := l1 + 1 to n1 do
      begin
        x1 := AList[i1];
        v1 := l1;
        for AExtraIndex := 0 to AExtraCount - 1 do
          x1s[AExtraIndex] := AExtraList[AExtraIndex][i1];
        for j1 := i1 - 1 downto l1 do
        begin
          if ACompare(AList[j1], x1) <= 0 then
          begin
            v1 := j1 + 1;
            break;
          end;
          AList[j1 + 1] := AList[j1];
          for AExtraIndex := 0 to AExtraCount - 1 do
            AExtraList[AExtraIndex][j1 + 1] := AExtraList[AExtraIndex][j1];
        end;
        AList[v1] := x1;
        for AExtraIndex := 0 to AExtraCount - 1 do
          AExtraList[AExtraIndex][v1] := x1s[AExtraIndex];
      end;
    end;

  begin
    while (l < r) do
    begin
      if r - l <= 12 then
      begin
        Insert(l, r);
        exit;
      end;

      p := (l + r) shr 1;
// вопрос поиска середины - не ипользовать ли random?
      V := AList[p];
      AList[p] := AList[l];
      for AExtraIndex := 0 to AExtraCount - 1 do
      begin
        x1s[AExtraIndex] := AExtraList[AExtraIndex][p];
//      Vs := AExtraList[p];
        AExtraList[AExtraIndex][p] := AExtraList[AExtraIndex][l];
      end;

      //     сортируем l+1..r относительно центра
      i := succ(l);
      j := r;
      while true do
      begin
        while (i < j) and (ACompare(v, AList[i]) > 0) do
          inc(i);
        while (j >= i) and (ACompare(AList[j], v) > 0) do
          dec(j);
        if (i >= j) then
          break;
        b := AList[i];
        AList[i] := AList[j];
        AList[j] := b;
        for AExtraIndex := 0 to AExtraCount - 1 do
        begin
          bs := AExtraList[AExtraIndex][i];
          AExtraList[AExtraIndex][i] := AExtraList[AExtraIndex][j];
          AExtraList[AExtraIndex][j] := bs;
        end;
        dec(j);
        inc(i);
      end;
      //     центр в a[j]
      AList[l] := AList[j];
      AList[j] := v;
      for AExtraIndex := 0 to AExtraCount - 1 do
      begin
        AExtraList[AExtraIndex][l] := AExtraList[AExtraIndex][j];
        AExtraList[AExtraIndex][j] := x1s[AExtraIndex];//vs;
      end;
      if ((j - l) <= (r - j)) then
      begin
        QuickSortOLWithExtraList(l, j - 1);
        l := succ(j);
      end
      else
      begin
        QuickSortOLWithExtraList(j + 1, r);
        r := pred(J);
      end;
    end;
  end;

  procedure QuickSortOLWithExtraListWithOrder(L, R: Integer);
  var
    i, j, p, AExtraIndex: integer;
    v, b: PfcxCommonUV;
    bs: Pointer;
    procedure Insert(const l1, n1: integer);
    var
      i1, j1, v1, AExtraIndex: integer;
      x1: PfcxCommonUV;
    begin
      for i1 := l1 + 1 to n1 do
      begin
        x1 := AList[i1];
        v1 := l1;
        for AExtraIndex := 0 to AExtraCount - 1 do
          x1s[AExtraIndex] := AExtraList[AExtraIndex][i1];
        for j1 := i1 - 1 downto l1 do
        begin
//          if ACompare(AList[j1], x1) <= 0 then
          if fcxIntCompare(PfcxCommonUV(AExtraList[AOrderIndex][j1]).Index, PfcxCommonUV(x1s[AOrderIndex]).Index) <= 0 then
          begin
            v1 := j1 + 1;
            break;
          end;
          AList[j1 + 1] := AList[j1];
          for AExtraIndex := 0 to AExtraCount - 1 do
            AExtraList[AExtraIndex][j1 + 1] := AExtraList[AExtraIndex][j1];
        end;
        AList[v1] := x1;
        for AExtraIndex := 0 to AExtraCount - 1 do
          AExtraList[AExtraIndex][v1] := x1s[AExtraIndex];
      end;
    end;

  begin
    while (l < r) do
    begin
      if r - l <= 12 then
      begin
        Insert(l, r);
        exit;
      end;

      p := (l + r) shr 1;
// вопрос поиска середины - не ипользовать ли random?
      V := AList[p];
      AList[p] := AList[l];
      for AExtraIndex := 0 to AExtraCount - 1 do
      begin
        x1s[AExtraIndex] := AExtraList[AExtraIndex][p];
//      Vs := AExtraList[p];
        AExtraList[AExtraIndex][p] := AExtraList[AExtraIndex][l];
      end;

      //     сортируем l+1..r относительно центра
      i := succ(l);
      j := r;
      while true do
      begin
//        while (i < j) and (ACompare(v, AList[i]) > 0) do
        while (i < j) and (fcxIntCompare(PfcxCommonUV(x1s[AOrderIndex]).Index, PfcxCommonUV(AExtraList[AOrderIndex][i]).Index) > 0) do
          inc(i);
//        while (j >= i) and (ACompare(AList[j], v) > 0) do
        while (j >= i) and (fcxIntCompare(PfcxCommonUV(AExtraList[AOrderIndex][j]).Index, PfcxCommonUV(x1s[AOrderIndex]).Index) > 0) do
          dec(j);
        if (i >= j) then
          break;
        b := AList[i];
        AList[i] := AList[j];
        AList[j] := b;
        for AExtraIndex := 0 to AExtraCount - 1 do
        begin
          bs := AExtraList[AExtraIndex][i];
          AExtraList[AExtraIndex][i] := AExtraList[AExtraIndex][j];
          AExtraList[AExtraIndex][j] := bs;
        end;
        dec(j);
        inc(i);
      end;
      //     центр в a[j]
      AList[l] := AList[j];
      AList[j] := v;
      for AExtraIndex := 0 to AExtraCount - 1 do
      begin
        AExtraList[AExtraIndex][l] := AExtraList[AExtraIndex][j];
        AExtraList[AExtraIndex][j] := x1s[AExtraIndex];//vs;
      end;
      if ((j - l) <= (r - j)) then
      begin
        QuickSortOLWithExtraListWithOrder(l, j - 1);
        l := succ(j);
      end
      else
      begin
        QuickSortOLWithExtraListWithOrder(j + 1, r);
        r := pred(J);
      end;
    end;
  end;

var
  I, AExtraIndex: integer;
  ACommonUV: PfcxCommonUV;
  APointer: Pointer;
begin
  GetMem(x1s, AExtraCount * fcPointerSize);
// first move NULL value to first position
  if ANullValue <> nil then
    for I := AStart to AEnd do
    begin
      if (AList[I] = ANullValue) then
      begin
        if I <> AStart then
        begin
          ACommonUV := AList[I];
          AList[I] := AList[AStart];
          AList[AStart] := ACommonUV;
          if AExtraList <> nil then
          begin
            for AExtraIndex := 0 to AExtraCount - 1 do
            begin
              APointer := AExtraList[AExtraIndex][I];
              AExtraList[AExtraIndex][I] := AExtraList[AExtraIndex][AStart];
              AExtraList[AExtraIndex][AStart] := APointer;
            end;
          end;
        end;
        inc(AStart);
        Break;
      end;
    end;
// ¬опрос по рандому
  if AExtraList <> nil then
    if (AOrderIndex > -1) and (AOrderIndex < AExtraCount) then
      QuickSortOLWithExtraListWithOrder(AStart, AEnd)
    else
      QuickSortOLWithExtraList(AStart, AEnd)
  else
    QuickSortOL(AStart, AEnd);
  FreeMem(x1s);
end;

(*
procedure SortStringUniqueList(AStart, AEnd: integer; AList: PfcxCUVArray; AExtraList: PfcxPointerArray);
  procedure Sort;
  var
    p: pointer;
    procedure inssort(a: PfcxCUVArray; n, d: integer);
    var
      pi, pj: PfcxCUVArray;
      s, t: PChar;
    begin
      pi := PfcxCUVArray(Cardinal(a) + fcPointerSize);
      dec(n);
      while n > 0 do
      begin
        pj := pi;
        while Cardinal(pj) > Cardinal(a) do
        begin
          s := PChar(PfcxStringUV(PfcxCUVArray(Cardinal(pj) - fcPointerSize)[0]).Value) + d;
          t := PChar(PfcxStringUV(pj[0]).Value) + d;
          while (s^ = t^) and (s^ <> #0) do
          begin
            inc(s);
            inc(t);
          end;
          if (s^ <= t^) then
            break;
//swap
          p := pointer(pj^);
          pointer(pj^) := pointer(PfcxCUVArray(PChar(pj) - 4 {1 shl 2})^);
          pointer(PfcxCUVArray(PChar(pj) - 4 {1 shl 2})^) := p;
//swap
          dec(pj);
        end;
        dec(n);
        inc(pi);
      end;
    end;

    procedure ssort2(a: PfcxCUVArray; n, depth: integer);
      procedure vecswap2(x_i, x_j: PfcxCUVArray; n: integer);
      var
        p: pointer;
      begin
        while (n > 0) do
        begin
          dec(n);
          p := pointer(x_i^);
          pointer(x_i^) := pointer(x_j^);
          pointer(x_j^) := p;
          inc(PChar(x_i), 4 {1 shl 2});
          inc(PChar(x_j), 4 {1 shl 2});
        end;
      end;

      function med3(a, b, c: PfcxCUVArray): PfcxCUVArray;
      var
        va, vb, vc: integer;
      begin
        va := Integer(PfcxStringUV(a^).Value[depth]);
        vb := Integer(PfcxStringUV(b^).Value[depth]);
        if (va = vb) then
        begin
          result := a;
          exit;
        end;
        vc := Integer(PfcxStringUV(c^).Value[depth]);
        if (vc = va) or (vc = vb) then
        begin
          result := c;
          exit;
        end;
        if va < vb then
        begin
          if vb < vc then
            Result := b
          else if va < vc then
            Result := c
          else
            Result := a;
        end
        else
        begin
          if vb > vc then
            Result := b
          else if va < vc then
            Result := a
          else
            Result := c;
        end;
      end;

    var
      d, r, partval: integer;
      pa, pb, pc, pd, pl, pm, pn: PfcxCUVArray;
    begin
      3if (n < 10) then
      begin
        inssort(a, n, depth);
        exit;
      end;
      pl := a;
      pm := PfcxCUVArray(PChar(a) + (n shr 1) shl 2); // div 2
      pn := PfcxCUVArray(PChar(a) + (n - 1) shl 2);
      if (n > 30) then
      begin // On big arrays, pseudomedian of 9
        d := n shr 3; // div 8
        pl := med3(pl, PfcxCUVArray(PChar(pl) + d shl 2), PfcxCUVArray(PChar(pl)
          + d shl 4));
        pm := med3(PfcxCUVArray(PChar(pm) - d shl 2), pm, PfcxCUVArray(PChar(pm)
          + d shl 2));
        pn := med3(PfcxCUVArray(PChar(pn) - d shl 4), PfcxCUVArray(PChar(pn) - d
          shl 2), pn);
      end;
      pm := med3(pl, pm, pn);

      p := pointer(a^);
      pointer(a^) := pointer(pm^);
      pointer(pm^) := p;
      partval := Integer(PChar(PfcxUniqueValue(a^).Value)[depth]);
      pa := PfcxCUVArray(PChar(a) + 4 {1 shl 2});
      pb := pa;
      pc := PfcxCUVArray(PChar(a) + (n - 1) shl 2);
      pd := pc;

      while true do
      begin
        while (Cardinal(pb) <= Cardinal(pc)) do
        begin
          r := Integer(PChar(PfcxUniqueValue(pb^).Value)[depth]) - partval;
          if r > 0 then
            break;
          if (r = 0) then
          begin
            p := pointer(pa^);
            pointer(pa^) := pointer(pb^);
            pointer(pb^) := p;
            inc(pa)
          end;
          inc(pb);
        end;
        while (Cardinal(pb) <= Cardinal(pc)) do
        begin
          r := Integer(PChar(PfcxUniqueValue(pc^).Value)[depth]) - partval;
          if r < 0 then
            break;
          if (r = 0) then
          begin
            p := pointer(pc^);
            pointer(pc^) := pointer(pd^);
            pointer(pd^) := p;
            dec(pd);
          end;
          dec(pc);
        end;
        if (Cardinal(pb) > Cardinal(pc)) then
          break;
        p := pointer(pb^);
        pointer(pb^) := pointer(pc^);
        pointer(pc^) := p;
        inc(pb);
        dec(pc);
      end;
      pn := PfcxCUVArray(PChar(a) + n shl 2);
      r := min((Cardinal(pa) - Cardinal(a)) shr 2, (Cardinal(pb) - Cardinal(pa))
        shr 2);
      vecswap2(a, PfcxCUVArray(PChar(pb) - r shl 2), r);
      r := min((Cardinal(pd) - Cardinal(pc)) shr 2, (Cardinal(pn) - Cardinal(pd))
        shr 2 - 1);
      vecswap2(pb, PfcxCUVArray(PChar(pn) - r shl 2), r);
      r := (Cardinal(pb) - Cardinal(pa)) shr 2;
      if (r > 1) then
        ssort2(a, r, depth);
      if Integer(PChar(PfcxUniqueValue(PfcxCUVArray(PChar(a) + r shl
        2)^).Value)[depth]) <> 0 then
        ssort2(PfcxCUVArray(PChar(a) + r shl 2), (Cardinal(pa) - Cardinal(a) +
          Cardinal(pn) - Cardinal(pd)) shr 2 - 1, depth + 1);
      r := (Cardinal(pd) - Cardinal(pc)) shr 2;
      if (r > 1) then
        ssort2(PfcxCUVArray(PChar(a) + (n - r) shl 2), r, depth);
    end;
  var
    i: integer;
    HaveNull: Boolean;
  begin
{ TODO -cЌеобходимо брать HaveNull из —”« }
    HaveNull := False;
    for i := AStart to AEnd do
      if TfcxDataTypeProcessor.IsNull(AList[i]) then
      begin
        HaveNull := True;
        if i <> AStart then // swap
        begin
          p := AList[AStart];
          AList[AStart] := AList[i];
          AList[i] := p;
        end;
        break;
      end;
    if HaveNull then
    begin
      ssort2(PfcxCUVArray(AList + SizeOf(Pointer)), AEnd, AStart);
    end
    else
      ssort2(AList, AEnd + 1, AStart);
  end;
*)
(*
  procedure SortWithExtarList;
  var
    p, p_x: pointer;
    procedure inssort(a: PfcxPointerArray; n, d: integer; a_x: PfcxArrPointerArray);
    var
      pi, pj: PfcxPointerArray;
      pi_x, pj_x: PfcxArrPointerArray;
      s, t: PChar;
    begin
      pi := PfcxPointerArray(PChar(a) + 4 {1 shl 2});
      pi_x := PfcxArrPointerArray(PChar(a_x) + 4 {1 shl 2});
      dec(n);
      while n > 0 do
      begin
        pj := pi;
        pj_x := pi_x;
        while Cardinal(pj) > Cardinal(a) do
        begin
          s := PChar(PfcxUniqueValue(PfcxPointerArray(PChar(pj) - 4 {1 shl 2})[0]).Value) + d;
          t := PChar(PfcxUniqueValue(pj[0]).Value) + d;
          while (s^ = t^) and (s^ <> #0) do
          begin
            inc(s);
            inc(t);
          end;
          if (s^ <= t^) then
            break;
//swap
          p := pointer(pj^);
          pointer(pj^) := pointer(PfcxPointerArray(PChar(pj) - 4 {1 shl 2})^);
          pointer(PfcxPointerArray(PChar(pj) - 4 {1 shl 2})^) := p;

          p_x := pointer(pj_x^);
          pointer(pj_x^) := pointer(PfcxArrPointerArray(PChar(pj_x) - 4 {1 shl 2})^);
          pointer(PfcxArrPointerArray(PChar(pj_x) - 4 {1 shl 2})^) := p_x;
//swap
          dec(pj);
          dec(pj_x);
        end;
        dec(n);
        inc(pi);
        inc(pi_x);
      end;
    end;

    procedure ssort2(a: PfcxPointerArray; n, depth: integer; a_x: PfcxArrPointerArray);
      procedure vecswap2(x_i, x_j: PfcxPointerArray; n: integer; x_i_x, x_j_x: PfcxArrPointerArray);
      var
        p, p_x: pointer;
      begin
        while (n > 0) do
        begin
          dec(n);
          p := pointer(x_i^);
          pointer(x_i^) := pointer(x_j^);
          pointer(x_j^) := p;
          inc(PChar(x_i), 4 {1 shl 2});
          inc(PChar(x_j), 4 {1 shl 2});

          p_x := pointer(x_i_x^);
          pointer(x_i_x^) := pointer(x_j_x^);
          pointer(x_j_x^) := p_x;
          inc(PChar(x_i_x), 4 {1 shl 2});
          inc(PChar(x_j_x), 4 {1 shl 2});
        end;
      end;

      function med3(a, b, c: PfcxPointerArray;a_x, b_x, c_x: PfcxArrPointerArray; var result_x: PfcxArrPointerArray): PfcxPointerArray;
      var
        va, vb, vc: integer;
      begin
        va := Integer(PChar(PfcxUniqueValue(a^).Value)[depth]);
        vb := Integer(PChar(PfcxUniqueValue(b^).Value)[depth]);
        if (va = vb) then
        begin
          result := a;
          result_x := a_x;
          exit;
        end;
        vc := Integer(PChar(PfcxUniqueValue(c^).Value)[depth]);
        if (vc = va) or (vc = vb) then
        begin
          result := c;
          result_x := c_x;
          exit;
        end;
        if va < vb then
        begin
          if vb < vc then
          begin
            Result := b;
            result_x := b_x;
          end
          else if va < vc then
          begin
            Result := c;
            result_x := c_x;
          end
          else
          begin
            Result := a;
            result_x := a_x;
          end;
        end
        else
        begin
          if vb > vc then
          begin
            Result := b;
            result_x := b_x;
          end
          else if va < vc then
          begin
            Result := a;
            result_x := a_x;
          end
          else
          begin
            Result := c;
            result_x := c_x;
          end;
        end;
      end;

    var
      d, r, partval: integer;
      pa, pb, pc, pd, pl, pm, pn: PfcxPointerArray;
      pa_x, pb_x, pc_x, pd_x, pl_x, pm_x, pn_x: PfcxArrPointerArray;
    begin
      if (n < 10) then
      begin
        inssort(a, n, depth, a_x);
        exit;
      end;
      pl := a;
      pm := PfcxPointerArray(PChar(a) + (n shr 1) shl 2); // div 2
      pn := PfcxPointerArray(PChar(a) + (n - 1) shl 2);
      pl_x := a_x;
      pm_x := PfcxArrPointerArray(PChar(a_x) + (n shr 1) shl 2); // div 2
      pn_x := PfcxArrPointerArray(PChar(a_x) + (n - 1) shl 2);
      if (n > 30) then
      begin // On big arrays, pseudomedian of 9
        d := n shr 3; // div 8
        pl := med3(pl, PfcxPointerArray(PChar(pl) + d shl 2), PfcxPointerArray(PChar(pl) + d shl 4),
           pl_x, PfcxArrPointerArray(PChar(pl_x) + d shl 2), PfcxArrPointerArray(PChar(pl_x) + d shl 4),
           pl_x);
        pm := med3(PfcxPointerArray(PChar(pm) - d shl 2), pm, PfcxPointerArray(PChar(pm) + d shl 2),
           PfcxArrPointerArray(PChar(pm_x) - d shl 2), pm_x, PfcxArrPointerArray(PChar(pm_x) + d shl 2),
           pm_x);
        pn := med3(PfcxPointerArray(PChar(pn) - d shl 4), PfcxPointerArray(PChar(pn) - d shl 2), pn,
           PfcxArrPointerArray(PChar(pn_x) - d shl 4), PfcxArrPointerArray(PChar(pn_x) - d shl 2), pn_x,
           pn_x);
      end;
      pm := med3(pl, pm, pn, pl_x, pm_x, pn_x, pm_x);

      p := pointer(a^);
      pointer(a^) := pointer(pm^);
      pointer(pm^) := p;
      p_x := pointer(a_x^);
      pointer(a_x^) := pointer(pm_x^);
      pointer(pm_x^) := p_x;

      partval := Integer(PChar(PfcxUniqueValue(a^).Value)[depth]);
      pa := PfcxPointerArray(PChar(a) + 4 {1 shl 2});
      pb := pa;
      pc := PfcxPointerArray(PChar(a) + (n - 1) shl 2);
      pd := pc;
      pa_x := PfcxArrPointerArray(PChar(a_x) + 4 {1 shl 2});
      pb_x := pa_x;
      pc_x := PfcxArrPointerArray(PChar(a_x) + (n - 1) shl 2);
      pd_x := pc_x;

      while true do
      begin
        while (Cardinal(pb) <= Cardinal(pc)) do
        begin
          r := Integer(PChar(PfcxUniqueValue(pb^).Value)[depth]) - partval;
          if r > 0 then
            break;
          if (r = 0) then
          begin
            p := pointer(pa^);
            pointer(pa^) := pointer(pb^);
            pointer(pb^) := p;
            inc(pa);
            p_x := pointer(pa_x^);
            pointer(pa_x^) := pointer(pb_x^);
            pointer(pb_x^) := p_x;
            inc(pa_x);
          end;
          inc(pb);
          inc(pb_x);
        end;
        while (Cardinal(pb) <= Cardinal(pc)) do
        begin
          r := Integer(PChar(PfcxUniqueValue(pc^).Value)[depth]) - partval;
          if r < 0 then
            break;
          if (r = 0) then
          begin
            p := pointer(pc^);
            pointer(pc^) := pointer(pd^);
            pointer(pd^) := p;
            dec(pd);
            p_x := pointer(pc_x^);
            pointer(pc_x^) := pointer(pd_x^);
            pointer(pd_x^) := p_x;
            dec(pd_x);
          end;
          dec(pc);
          dec(pc_x);
        end;
        if (Cardinal(pb) > Cardinal(pc)) then
          break;
        p := pointer(pb^);
        pointer(pb^) := pointer(pc^);
        pointer(pc^) := p;
        inc(pb);
        dec(pc);
        p_x := pointer(pb_x^);
        pointer(pb_x^) := pointer(pc_x^);
        pointer(pc_x^) := p_x;
        inc(pb_x);
        dec(pc_x);
      end;
      pn := PfcxPointerArray(PChar(a) + n shl 2);
      pn_x := PfcxArrPointerArray(PChar(a_x) + n shl 2);
      r := min((Cardinal(pa) - Cardinal(a)) shr 2, (Cardinal(pb) - Cardinal(pa))
        shr 2);
      vecswap2(a, PfcxPointerArray(PChar(pb) - r shl 2), r, a_x, PfcxArrPointerArray(PChar(pb_x) - r shl 2));
      r := min((Cardinal(pd) - Cardinal(pc)) shr 2, (Cardinal(pn) - Cardinal(pd))
        shr 2 - 1);
      vecswap2(pb, PfcxPointerArray(PChar(pn) - r shl 2), r, pb_x, PfcxArrPointerArray(PChar(pn_x) - r shl 2));
      r := (Cardinal(pb) - Cardinal(pa)) shr 2;
      if (r > 1) then
        ssort2(a, r, depth, a_x);
      if Integer(PChar(PfcxUniqueValue(PfcxPointerArray(PChar(a) + r shl
        2)^).Value)[depth]) <> 0 then
        ssort2(PfcxPointerArray(PChar(a) + r shl 2), (Cardinal(pa) - Cardinal(a) +
          Cardinal(pn) - Cardinal(pd)) shr 2 - 1, depth + 1, PfcxArrPointerArray(PChar(a_x) + r shl 2));
      r := (Cardinal(pd) - Cardinal(pc)) shr 2;
      if (r > 1) then
        ssort2(PfcxPointerArray(PChar(a) + (n - r) shl 2), r, depth, PfcxArrPointerArray(PChar(a_x) + (n - r) shl 2));
    end;
  var
    i: integer;
    HaveNull: Boolean;
  begin
    HaveNull := False;
    for i := AStart to AEnd do
      if PfcxUniqueValue(AList[i]).Value = nil then
      begin
        HaveNull := True;
        if i <> AStart then // swap
        begin
          p := AList[AStart];
          AList[AStart] := AList[i];
          AList[i] := p;
          p_x := ASplitList[AStart];
          ASplitList[AStart] := ASplitList[i];
          ASplitList[i] := p_x;
        end;
        break;
      end;
    if HaveNull then
    begin
      ssort2(PfcxPointerArray(PChar(AList) + 4 {1 shl 2}), AEnd, AStart, PfcxArrPointerArray(PChar(ASplitList) + 4 {1 shl 2}));
    end
    else
      ssort2(AList, AEnd + 1, AStart, ASplitList);
  end;
*)
{
begin
// ¬опрос по рандому
  if AExtraList <> nil then
    SortWithExtraList
  else
    Sort;
end;
}
(*
procedure SortStringUniqueList(AStart, AEnd: integer; AList: PfcxPointerArray; ASplitList: PfcxArrPointerArray; AIndexOrderField: integer);
  procedure Sort;
  var
    p: pointer;
    procedure inssort(a: PfcxPointerArray; n, d: integer);
    var
      pi, pj: PfcxPointerArray;
      s, t: PChar;
    begin
      pi := PfcxPointerArray(PChar(a) + 4 {1 shl 2});
      dec(n);
      while n > 0 do
      begin
        pj := pi;
        while Cardinal(pj) > Cardinal(a) do
        begin
          s := PChar(PfcxUniqueValue(PfcxPointerArray(PChar(pj) - 4 {1 shl 2})[0]).Value) + d;
          t := PChar(PfcxUniqueValue(pj[0]).Value) + d;
          while (s^ = t^) and (s^ <> #0) do
          begin
            inc(s);
            inc(t);
          end;
          if (s^ <= t^) then
            break;
//swap
          p := pointer(pj^);
          pointer(pj^) := pointer(PfcxPointerArray(PChar(pj) - 4 {1 shl 2})^);
          pointer(PfcxPointerArray(PChar(pj) - 4 {1 shl 2})^) := p;
//swap
          dec(pj);
        end;
        dec(n);
        inc(pi);
      end;
    end;

    procedure ssort2(a: PfcxPointerArray; n, depth: integer);
      procedure vecswap2(x_i, x_j: PfcxPointerArray; n: integer);
      var
        p: pointer;
      begin
        while (n > 0) do
        begin
          dec(n);
          p := pointer(x_i^);
          pointer(x_i^) := pointer(x_j^);
          pointer(x_j^) := p;
          inc(PChar(x_i), 4 {1 shl 2});
          inc(PChar(x_j), 4 {1 shl 2});
        end;
      end;

      function med3(a, b, c: PfcxPointerArray): PfcxPointerArray;
      var
        va, vb, vc: integer;
      begin
        va := Integer(PChar(PfcxUniqueValue(a^).Value)[depth]);
        vb := Integer(PChar(PfcxUniqueValue(b^).Value)[depth]);
        if (va = vb) then
        begin
          result := a;
          exit;
        end;
        vc := Integer(PChar(PfcxUniqueValue(c^).Value)[depth]);
        if (vc = va) or (vc = vb) then
        begin
          result := c;
          exit;
        end;
        if va < vb then
        begin
          if vb < vc then
            Result := b
          else if va < vc then
            Result := c
          else
            Result := a;
        end
        else
        begin
          if vb > vc then
            Result := b
          else if va < vc then
            Result := a
          else
            Result := c;
        end;
      end;

    var
      d, r, partval: integer;
      pa, pb, pc, pd, pl, pm, pn: PfcxPointerArray;
    begin
      if (n < 10) then
      begin
        inssort(a, n, depth);
        exit;
      end;
      pl := a;
      pm := PfcxPointerArray(PChar(a) + (n shr 1) shl 2); // div 2
      pn := PfcxPointerArray(PChar(a) + (n - 1) shl 2);
      if (n > 30) then
      begin // On big arrays, pseudomedian of 9
        d := n shr 3; // div 8
        pl := med3(pl, PfcxPointerArray(PChar(pl) + d shl 2), PfcxPointerArray(PChar(pl)
          + d shl 4));
        pm := med3(PfcxPointerArray(PChar(pm) - d shl 2), pm, PfcxPointerArray(PChar(pm)
          + d shl 2));
        pn := med3(PfcxPointerArray(PChar(pn) - d shl 4), PfcxPointerArray(PChar(pn) - d
          shl 2), pn);
      end;
      pm := med3(pl, pm, pn);

      p := pointer(a^);
      pointer(a^) := pointer(pm^);
      pointer(pm^) := p;
      partval := Integer(PChar(PfcxUniqueValue(a^).Value)[depth]);
      pa := PfcxPointerArray(PChar(a) + 4 {1 shl 2});
      pb := pa;
      pc := PfcxPointerArray(PChar(a) + (n - 1) shl 2);
      pd := pc;

      while true do
      begin
        while (Cardinal(pb) <= Cardinal(pc)) do
        begin
          r := Integer(PChar(PfcxUniqueValue(pb^).Value)[depth]) - partval;
          if r > 0 then
            break;
          if (r = 0) then
          begin
            p := pointer(pa^);
            pointer(pa^) := pointer(pb^);
            pointer(pb^) := p;
            inc(pa)
          end;
          inc(pb);
        end;
        while (Cardinal(pb) <= Cardinal(pc)) do
        begin
          r := Integer(PChar(PfcxUniqueValue(pc^).Value)[depth]) - partval;
          if r < 0 then
            break;
          if (r = 0) then
          begin
            p := pointer(pc^);
            pointer(pc^) := pointer(pd^);
            pointer(pd^) := p;
            dec(pd);
          end;
          dec(pc);
        end;
        if (Cardinal(pb) > Cardinal(pc)) then
          break;
        p := pointer(pb^);
        pointer(pb^) := pointer(pc^);
        pointer(pc^) := p;
        inc(pb);
        dec(pc);
      end;
      pn := PfcxPointerArray(PChar(a) + n shl 2);
      r := min((Cardinal(pa) - Cardinal(a)) shr 2, (Cardinal(pb) - Cardinal(pa))
        shr 2);
      vecswap2(a, PfcxPointerArray(PChar(pb) - r shl 2), r);
      r := min((Cardinal(pd) - Cardinal(pc)) shr 2, (Cardinal(pn) - Cardinal(pd))
        shr 2 - 1);
      vecswap2(pb, PfcxPointerArray(PChar(pn) - r shl 2), r);
      r := (Cardinal(pb) - Cardinal(pa)) shr 2;
      if (r > 1) then
        ssort2(a, r, depth);
      if Integer(PChar(PfcxUniqueValue(PfcxPointerArray(PChar(a) + r shl
        2)^).Value)[depth]) <> 0 then
        ssort2(PfcxPointerArray(PChar(a) + r shl 2), (Cardinal(pa) - Cardinal(a) +
          Cardinal(pn) - Cardinal(pd)) shr 2 - 1, depth + 1);
      r := (Cardinal(pd) - Cardinal(pc)) shr 2;
      if (r > 1) then
        ssort2(PfcxPointerArray(PChar(a) + (n - r) shl 2), r, depth);
    end;
  var
    i: integer;
    HaveNull: Boolean;
  begin
    HaveNull := False;
    for i := AStart to AEnd do
      if PfcxUniqueValue(AList[i]).Value = nil then
      begin
        HaveNull := True;
        if i <> AStart then // swap
        begin
          p := AList[AStart];
          AList[AStart] := AList[i];
          AList[i] := p;
        end;
        break;
      end;
    if HaveNull then
    begin
      ssort2(PfcxPointerArray(PChar(AList) + 4 {1 shl 2}), AEnd, AStart);
    end
    else
      ssort2(AList, AEnd + 1, AStart);
  end;

  procedure SortWithSplit;
  var
    p, p_x: pointer;
    procedure inssort(a: PfcxPointerArray; n, d: integer; a_x: PfcxArrPointerArray);
    var
      pi, pj: PfcxPointerArray;
      pi_x, pj_x: PfcxArrPointerArray;
      s, t: PChar;
    begin
      pi := PfcxPointerArray(PChar(a) + 4 {1 shl 2});
      pi_x := PfcxArrPointerArray(PChar(a_x) + 4 {1 shl 2});
      dec(n);
      while n > 0 do
      begin
        pj := pi;
        pj_x := pi_x;
        while Cardinal(pj) > Cardinal(a) do
        begin
          s := PChar(PfcxUniqueValue(PfcxPointerArray(PChar(pj) - 4 {1 shl 2})[0]).Value) + d;
          t := PChar(PfcxUniqueValue(pj[0]).Value) + d;
          while (s^ = t^) and (s^ <> #0) do
          begin
            inc(s);
            inc(t);
          end;
          if (s^ <= t^) then
            break;
//swap
          p := pointer(pj^);
          pointer(pj^) := pointer(PfcxPointerArray(PChar(pj) - 4 {1 shl 2})^);
          pointer(PfcxPointerArray(PChar(pj) - 4 {1 shl 2})^) := p;

          p_x := pointer(pj_x^);
          pointer(pj_x^) := pointer(PfcxArrPointerArray(PChar(pj_x) - 4 {1 shl 2})^);
          pointer(PfcxArrPointerArray(PChar(pj_x) - 4 {1 shl 2})^) := p_x;
//swap
          dec(pj);
          dec(pj_x);
        end;
        dec(n);
        inc(pi);
        inc(pi_x);
      end;
    end;

    procedure ssort2(a: PfcxPointerArray; n, depth: integer; a_x: PfcxArrPointerArray);
      procedure vecswap2(x_i, x_j: PfcxPointerArray; n: integer; x_i_x, x_j_x: PfcxArrPointerArray);
      var
        p, p_x: pointer;
      begin
        while (n > 0) do
        begin
          dec(n);
          p := pointer(x_i^);
          pointer(x_i^) := pointer(x_j^);
          pointer(x_j^) := p;
          inc(PChar(x_i), 4 {1 shl 2});
          inc(PChar(x_j), 4 {1 shl 2});

          p_x := pointer(x_i_x^);
          pointer(x_i_x^) := pointer(x_j_x^);
          pointer(x_j_x^) := p_x;
          inc(PChar(x_i_x), 4 {1 shl 2});
          inc(PChar(x_j_x), 4 {1 shl 2});
        end;
      end;

      function med3(a, b, c: PfcxPointerArray;a_x, b_x, c_x: PfcxArrPointerArray; var result_x: PfcxArrPointerArray): PfcxPointerArray;
      var
        va, vb, vc: integer;
      begin
        va := Integer(PChar(PfcxUniqueValue(a^).Value)[depth]);
        vb := Integer(PChar(PfcxUniqueValue(b^).Value)[depth]);
        if (va = vb) then
        begin
          result := a;
          result_x := a_x;
          exit;
        end;
        vc := Integer(PChar(PfcxUniqueValue(c^).Value)[depth]);
        if (vc = va) or (vc = vb) then
        begin
          result := c;
          result_x := c_x;
          exit;
        end;
        if va < vb then
        begin
          if vb < vc then
          begin
            Result := b;
            result_x := b_x;
          end
          else if va < vc then
          begin
            Result := c;
            result_x := c_x;
          end
          else
          begin
            Result := a;
            result_x := a_x;
          end;
        end
        else
        begin
          if vb > vc then
          begin
            Result := b;
            result_x := b_x;
          end
          else if va < vc then
          begin
            Result := a;
            result_x := a_x;
          end
          else
          begin
            Result := c;
            result_x := c_x;
          end;
        end;
      end;

    var
      d, r, partval: integer;
      pa, pb, pc, pd, pl, pm, pn: PfcxPointerArray;
      pa_x, pb_x, pc_x, pd_x, pl_x, pm_x, pn_x: PfcxArrPointerArray;
    begin
      if (n < 10) then
      begin
        inssort(a, n, depth, a_x);
        exit;
      end;
      pl := a;
      pm := PfcxPointerArray(PChar(a) + (n shr 1) shl 2); // div 2
      pn := PfcxPointerArray(PChar(a) + (n - 1) shl 2);
      pl_x := a_x;
      pm_x := PfcxArrPointerArray(PChar(a_x) + (n shr 1) shl 2); // div 2
      pn_x := PfcxArrPointerArray(PChar(a_x) + (n - 1) shl 2);
      if (n > 30) then
      begin // On big arrays, pseudomedian of 9
        d := n shr 3; // div 8
        pl := med3(pl, PfcxPointerArray(PChar(pl) + d shl 2), PfcxPointerArray(PChar(pl) + d shl 4),
           pl_x, PfcxArrPointerArray(PChar(pl_x) + d shl 2), PfcxArrPointerArray(PChar(pl_x) + d shl 4),
           pl_x);
        pm := med3(PfcxPointerArray(PChar(pm) - d shl 2), pm, PfcxPointerArray(PChar(pm) + d shl 2),
           PfcxArrPointerArray(PChar(pm_x) - d shl 2), pm_x, PfcxArrPointerArray(PChar(pm_x) + d shl 2),
           pm_x);
        pn := med3(PfcxPointerArray(PChar(pn) - d shl 4), PfcxPointerArray(PChar(pn) - d shl 2), pn,
           PfcxArrPointerArray(PChar(pn_x) - d shl 4), PfcxArrPointerArray(PChar(pn_x) - d shl 2), pn_x,
           pn_x);
      end;
      pm := med3(pl, pm, pn, pl_x, pm_x, pn_x, pm_x);

      p := pointer(a^);
      pointer(a^) := pointer(pm^);
      pointer(pm^) := p;
      p_x := pointer(a_x^);
      pointer(a_x^) := pointer(pm_x^);
      pointer(pm_x^) := p_x;

      partval := Integer(PChar(PfcxUniqueValue(a^).Value)[depth]);
      pa := PfcxPointerArray(PChar(a) + 4 {1 shl 2});
      pb := pa;
      pc := PfcxPointerArray(PChar(a) + (n - 1) shl 2);
      pd := pc;
      pa_x := PfcxArrPointerArray(PChar(a_x) + 4 {1 shl 2});
      pb_x := pa_x;
      pc_x := PfcxArrPointerArray(PChar(a_x) + (n - 1) shl 2);
      pd_x := pc_x;

      while true do
      begin
        while (Cardinal(pb) <= Cardinal(pc)) do
        begin
          r := Integer(PChar(PfcxUniqueValue(pb^).Value)[depth]) - partval;
          if r > 0 then
            break;
          if (r = 0) then
          begin
            p := pointer(pa^);
            pointer(pa^) := pointer(pb^);
            pointer(pb^) := p;
            inc(pa);
            p_x := pointer(pa_x^);
            pointer(pa_x^) := pointer(pb_x^);
            pointer(pb_x^) := p_x;
            inc(pa_x);
          end;
          inc(pb);
          inc(pb_x);
        end;
        while (Cardinal(pb) <= Cardinal(pc)) do
        begin
          r := Integer(PChar(PfcxUniqueValue(pc^).Value)[depth]) - partval;
          if r < 0 then
            break;
          if (r = 0) then
          begin
            p := pointer(pc^);
            pointer(pc^) := pointer(pd^);
            pointer(pd^) := p;
            dec(pd);
            p_x := pointer(pc_x^);
            pointer(pc_x^) := pointer(pd_x^);
            pointer(pd_x^) := p_x;
            dec(pd_x);
          end;
          dec(pc);
          dec(pc_x);
        end;
        if (Cardinal(pb) > Cardinal(pc)) then
          break;
        p := pointer(pb^);
        pointer(pb^) := pointer(pc^);
        pointer(pc^) := p;
        inc(pb);
        dec(pc);
        p_x := pointer(pb_x^);
        pointer(pb_x^) := pointer(pc_x^);
        pointer(pc_x^) := p_x;
        inc(pb_x);
        dec(pc_x);
      end;
      pn := PfcxPointerArray(PChar(a) + n shl 2);
      pn_x := PfcxArrPointerArray(PChar(a_x) + n shl 2);
      r := min((Cardinal(pa) - Cardinal(a)) shr 2, (Cardinal(pb) - Cardinal(pa))
        shr 2);
      vecswap2(a, PfcxPointerArray(PChar(pb) - r shl 2), r, a_x, PfcxArrPointerArray(PChar(pb_x) - r shl 2));
      r := min((Cardinal(pd) - Cardinal(pc)) shr 2, (Cardinal(pn) - Cardinal(pd))
        shr 2 - 1);
      vecswap2(pb, PfcxPointerArray(PChar(pn) - r shl 2), r, pb_x, PfcxArrPointerArray(PChar(pn_x) - r shl 2));
      r := (Cardinal(pb) - Cardinal(pa)) shr 2;
      if (r > 1) then
        ssort2(a, r, depth, a_x);
      if Integer(PChar(PfcxUniqueValue(PfcxPointerArray(PChar(a) + r shl
        2)^).Value)[depth]) <> 0 then
        ssort2(PfcxPointerArray(PChar(a) + r shl 2), (Cardinal(pa) - Cardinal(a) +
          Cardinal(pn) - Cardinal(pd)) shr 2 - 1, depth + 1, PfcxArrPointerArray(PChar(a_x) + r shl 2));
      r := (Cardinal(pd) - Cardinal(pc)) shr 2;
      if (r > 1) then
        ssort2(PfcxPointerArray(PChar(a) + (n - r) shl 2), r, depth, PfcxArrPointerArray(PChar(a_x) + (n - r) shl 2));
    end;
  var
    i: integer;
    HaveNull: Boolean;
  begin
    HaveNull := False;
    for i := AStart to AEnd do
      if PfcxUniqueValue(AList[i]).Value = nil then
      begin
        HaveNull := True;
        if i <> AStart then // swap
        begin
          p := AList[AStart];
          AList[AStart] := AList[i];
          AList[i] := p;
          p_x := ASplitList[AStart];
          ASplitList[AStart] := ASplitList[i];
          ASplitList[i] := p_x;
        end;
        break;
      end;
    if HaveNull then
    begin
      ssort2(PfcxPointerArray(PChar(AList) + 4 {1 shl 2}), AEnd, AStart, PfcxArrPointerArray(PChar(ASplitList) + 4 {1 shl 2}));
    end
    else
      ssort2(AList, AEnd + 1, AStart, ASplitList);
  end;

  procedure SortWithSplitAndOrderField;
  var
    p, p_x: pointer;
    procedure inssort(a: PfcxPointerArray; n, d: integer; a_x: PfcxArrPointerArray);
    var
      pi, pj: PfcxPointerArray;
      pi_x, pj_x: PfcxArrPointerArray;
      s, t: PChar;
    begin
      pi := PfcxPointerArray(PChar(a) + 4 {1 shl 2});
      pi_x := PfcxArrPointerArray(PChar(a_x) + 4 {1 shl 2});
      dec(n);
      while n > 0 do
      begin
        pj := pi;
        pj_x := pi_x;
        while Cardinal(pj_x) > Cardinal(a_x) do
        begin
          s := PChar(PfcxUniqueValue(PfcxArrPointerArray(PChar(pj_x) - 4 {1 shl 2})[0][AIndexOrderField]).Value) + d;
          t := PChar(PfcxUniqueValue(pj_x[0][AIndexOrderField]).Value) + d;
          while (s^ = t^) and (s^ <> #0) do
          begin
            inc(s);
            inc(t);
          end;
          if (s^ <= t^) then
            break;
//swap
          p := pointer(pj^);
          pointer(pj^) := pointer(PfcxPointerArray(PChar(pj) - 4 {1 shl 2})^);
          pointer(PfcxPointerArray(PChar(pj) - 4 {1 shl 2})^) := p;

          p_x := pointer(pj_x^);
          pointer(pj_x^) := pointer(PfcxArrPointerArray(PChar(pj_x) - 4 {1 shl 2})^);
          pointer(PfcxArrPointerArray(PChar(pj_x) - 4 {1 shl 2})^) := p_x;
//swap
          dec(pj);
          dec(pj_x);
        end;
        dec(n);
        inc(pi);
        inc(pi_x);
      end;
    end;

    procedure ssort2(a: PfcxPointerArray; n, depth: integer; a_x: PfcxArrPointerArray);
      procedure vecswap2(x_i, x_j: PfcxPointerArray; n: integer; x_i_x, x_j_x: PfcxArrPointerArray);
      var
        p, p_x: pointer;
      begin
        while (n > 0) do
        begin
          dec(n);
          p := pointer(x_i^);
          pointer(x_i^) := pointer(x_j^);
          pointer(x_j^) := p;
          inc(PChar(x_i), 4 {1 shl 2});
          inc(PChar(x_j), 4 {1 shl 2});

          p_x := pointer(x_i_x^);
          pointer(x_i_x^) := pointer(x_j_x^);
          pointer(x_j_x^) := p_x;
          inc(PChar(x_i_x), 4 {1 shl 2});
          inc(PChar(x_j_x), 4 {1 shl 2});
        end;
      end;

      function med3(a, b, c: PfcxPointerArray;a_x, b_x, c_x: PfcxArrPointerArray; var result_x: PfcxArrPointerArray): PfcxPointerArray;
      var
        va, vb, vc: integer;
      begin
        va := Integer(PChar(PfcxUniqueValue(PfcxPointerArray(a_x^)[AIndexOrderField]).Value)[depth]);
        vb := Integer(PChar(PfcxUniqueValue(PfcxPointerArray(b_x^)[AIndexOrderField]).Value)[depth]);
        if (va = vb) then
        begin
          result := a;
          result_x := a_x;
          exit;
        end;
        vc := Integer(PChar(PfcxUniqueValue(PfcxPointerArray(c_x^)[AIndexOrderField]).Value)[depth]);
        if (vc = va) or (vc = vb) then
        begin
          result := c;
          result_x := c_x;
          exit;
        end;
        if va < vb then
        begin
          if vb < vc then
          begin
            Result := b;
            result_x := b_x;
          end
          else if va < vc then
          begin
            Result := c;
            result_x := c_x;
          end
          else
          begin
            Result := a;
            result_x := a_x;
          end;
        end
        else
        begin
          if vb > vc then
          begin
            Result := b;
            result_x := b_x;
          end
          else if va < vc then
          begin
            Result := a;
            result_x := a_x;
          end
          else
          begin
            Result := c;
            result_x := c_x;
          end;
        end;
      end;

    var
      d, r, partval: integer;
      pa, pb, pc, pd, pl, pm, pn: PfcxPointerArray;
      pa_x, pb_x, pc_x, pd_x, pl_x, pm_x, pn_x: PfcxArrPointerArray;
    begin
      if (n < 10) then
      begin
        inssort(a, n, depth, a_x);
        exit;
      end;
      pl := a;
      pm := PfcxPointerArray(PChar(a) + (n shr 1) shl 2); // div 2
      pn := PfcxPointerArray(PChar(a) + (n - 1) shl 2);
      pl_x := a_x;
      pm_x := PfcxArrPointerArray(PChar(a_x) + (n shr 1) shl 2); // div 2
      pn_x := PfcxArrPointerArray(PChar(a_x) + (n - 1) shl 2);
      if (n > 30) then
      begin // On big arrays, pseudomedian of 9
        d := n shr 3; // div 8
        pl := med3(pl, PfcxPointerArray(PChar(pl) + d shl 2), PfcxPointerArray(PChar(pl) + d shl 4),
           pl_x, PfcxArrPointerArray(PChar(pl_x) + d shl 2), PfcxArrPointerArray(PChar(pl_x) + d shl 4),
           pl_x);
        pm := med3(PfcxPointerArray(PChar(pm) - d shl 2), pm, PfcxPointerArray(PChar(pm) + d shl 2),
           PfcxArrPointerArray(PChar(pm_x) - d shl 2), pm_x, PfcxArrPointerArray(PChar(pm_x) + d shl 2),
           pm_x);
        pn := med3(PfcxPointerArray(PChar(pn) - d shl 4), PfcxPointerArray(PChar(pn) - d shl 2), pn,
           PfcxArrPointerArray(PChar(pn_x) - d shl 4), PfcxArrPointerArray(PChar(pn_x) - d shl 2), pn_x,
           pn_x);
      end;
      pm := med3(pl, pm, pn, pl_x, pm_x, pn_x, pm_x);

      p := pointer(a^);
      pointer(a^) := pointer(pm^);
      pointer(pm^) := p;
      p_x := pointer(a_x^);
      pointer(a_x^) := pointer(pm_x^);
      pointer(pm_x^) := p_x;

      partval := Integer(PChar(PfcxUniqueValue(PfcxPointerArray(a_x^)[AIndexOrderField]).Value)[depth]);
      pa := PfcxPointerArray(PChar(a) + 4 {1 shl 2});
      pb := pa;
      pc := PfcxPointerArray(PChar(a) + (n - 1) shl 2);
      pd := pc;
      pa_x := PfcxArrPointerArray(PChar(a_x) + 4 {1 shl 2});
      pb_x := pa_x;
      pc_x := PfcxArrPointerArray(PChar(a_x) + (n - 1) shl 2);
      pd_x := pc_x;

      while true do
      begin
        while (Cardinal(pb) <= Cardinal(pc)) do
        begin
          r := Integer(PChar(PfcxUniqueValue(PfcxPointerArray(pb_x^)[AIndexOrderField]).Value)[depth]) - partval;
          if r > 0 then
            break;
          if (r = 0) then
          begin
            p := pointer(pa^);
            pointer(pa^) := pointer(pb^);
            pointer(pb^) := p;
            inc(pa);
            p_x := pointer(pa_x^);
            pointer(pa_x^) := pointer(pb_x^);
            pointer(pb_x^) := p_x;
            inc(pa_x);
          end;
          inc(pb);
          inc(pb_x);
        end;
        while (Cardinal(pb) <= Cardinal(pc)) do
        begin
          r := Integer(PChar(PfcxUniqueValue(PfcxPointerArray(pc_x^)[AIndexOrderField]).Value)[depth]) - partval;
          if r < 0 then
            break;
          if (r = 0) then
          begin
            p := pointer(pc^);
            pointer(pc^) := pointer(pd^);
            pointer(pd^) := p;
            dec(pd);
            p_x := pointer(pc_x^);
            pointer(pc_x^) := pointer(pd_x^);
            pointer(pd_x^) := p_x;
            dec(pd_x);
          end;
          dec(pc);
          dec(pc_x);
        end;
        if (Cardinal(pb) > Cardinal(pc)) then
          break;
        p := pointer(pb^);
        pointer(pb^) := pointer(pc^);
        pointer(pc^) := p;
        inc(pb);
        dec(pc);
        p_x := pointer(pb_x^);
        pointer(pb_x^) := pointer(pc_x^);
        pointer(pc_x^) := p_x;
        inc(pb_x);
        dec(pc_x);
      end;
      pn := PfcxPointerArray(PChar(a) + n shl 2);
      pn_x := PfcxArrPointerArray(PChar(a_x) + n shl 2);
      r := min((Cardinal(pa) - Cardinal(a)) shr 2, (Cardinal(pb) - Cardinal(pa))
        shr 2);
      vecswap2(a, PfcxPointerArray(PChar(pb) - r shl 2), r, a_x, PfcxArrPointerArray(PChar(pb_x) - r shl 2));
      r := min((Cardinal(pd) - Cardinal(pc)) shr 2, (Cardinal(pn) - Cardinal(pd))
        shr 2 - 1);
      vecswap2(pb, PfcxPointerArray(PChar(pn) - r shl 2), r, pb_x, PfcxArrPointerArray(PChar(pn_x) - r shl 2));
      r := (Cardinal(pb) - Cardinal(pa)) shr 2;
      if (r > 1) then
        ssort2(a, r, depth, a_x);
      if Integer(PChar(PfcxUniqueValue(PfcxPointerArray(PfcxArrPointerArray(PChar(a_x) + r shl
        2)^)[AIndexOrderField]).Value)[depth]) <> 0 then
        ssort2(PfcxPointerArray(PChar(a) + r shl 2), (Cardinal(pa) - Cardinal(a) +
          Cardinal(pn) - Cardinal(pd)) shr 2 - 1, depth + 1, PfcxArrPointerArray(PChar(a_x) + r shl 2));
      r := (Cardinal(pd) - Cardinal(pc)) shr 2;
      if (r > 1) then
        ssort2(PfcxPointerArray(PChar(a) + (n - r) shl 2), r, depth, PfcxArrPointerArray(PChar(a_x) + (n - r) shl 2));
    end;
  var
    i: integer;
    HaveNull: Boolean;
  begin
    HaveNull := False;
    for i := AStart to AEnd do
      if PfcxUniqueValue(ASplitList[i][AIndexOrderField]).Value = nil then
      begin
        HaveNull := True;
        if i <> AStart then // swap
        begin
          p := AList[AStart];
          AList[AStart] := AList[i];
          AList[i] := p;
          p_x := ASplitList[AStart];
          ASplitList[AStart] := ASplitList[i];
          ASplitList[i] := p_x;
        end;
        break;
      end;
    if HaveNull then
    begin
      ssort2(PfcxPointerArray(PChar(AList) + 4 {1 shl 2}), AEnd, AStart, PfcxArrPointerArray(PChar(ASplitList) + 4 {1 shl 2}));
    end
    else
      ssort2(AList, AEnd + 1, AStart, ASplitList);
  end;

begin
  if AIndexOrderField >= 0 then
    SortWithSplitAndOrderField
  else
  if ASplitList = nil then
    Sort
  else
    SortWithSplit
end;

procedure SortUniqueList(AStart, AEnd: integer; AList: PfcxPointerArray; ASplitList: PfcxArrPointerArray; ACompare: TfcxCompareFunc; AIndexOrderField: integer);
  procedure QuickSortOL(L, R: Integer);
  var
    i, j, p: integer;
    v, b: PfcxUniqueValue;

    procedure Insert(const l1, n1: integer);
    var
      i1, j1: integer;
      x1: PfcxUniqueValue;

    begin
      for i1 := l1 + 1 to n1 do
      begin
        x1 := AList[i1];

        for j1 := i1 - 1 downto l1 do
        begin
          if ACompare(PfcxUniqueValue(AList[j1]).Value, x1.Value) <= 0 then
            break;
          AList[j1 + 1] := AList[j1];

        end;
        AList[j1 + 1] := x1;

      end;
    end;

  begin
    while (l < r) do
    begin
      if r - l <= 12 then
      begin
        Insert(l, r);
        exit;
      end;
      //    p := l + ((r - l) shr 1);
      p := (l + r) shr 1;
        // упрощенный эквивалент пред. строки
      V := AList[p];

      AList[p] := AList[l];


      //     сортируем l+1..r относительно центра
      i := succ(l);
      j := r;
      while true do
      begin
        while (i < j) and (ACompare(v.Value, PfcxUniqueValue(AList[i]).Value) > 0) do
          inc(i);
        while (j >= i) and (ACompare(PfcxUniqueValue(AList[j]).Value, v.Value) > 0) do
          dec(j);
        if (i >= j) then
          break;
        b := AList[i];

        AList[i] := AList[j];
        AList[j] := b;


        dec(j);
        inc(i);
      end;
      //     центр в a[j]
      AList[l] := AList[j];
      AList[j] := v;


      if ((j - l) <= (r - j)) then
      begin
        QuickSortOL(l, j - 1);
        l := succ(j);
      end
      else
      begin
        QuickSortOL(j + 1, r);
        r := pred(J);
      end;
    end;
  end;
  procedure QuickSortOLWithSplit(L, R: Integer);
  var
    i, j, p: integer;
    v, b: PfcxUniqueValue;
    vs, bs: PfcxPointerArray;
    procedure Insert(const l1, n1: integer);
    var
      i1, j1: integer;
      x1: PfcxUniqueValue;
      x1s: PfcxPointerArray;
    begin
      for i1 := l1 + 1 to n1 do
      begin
        x1 := AList[i1];
        x1s := ASplitList[i1];
        for j1 := i1 - 1 downto l1 do
        begin
          if ACompare(PfcxUniqueValue(AList[j1]).Value, x1.Value) <= 0 then
            break;
          AList[j1 + 1] := AList[j1];
          ASplitList[j1 + 1] := ASplitList[j1];
        end;
        AList[j1 + 1] := x1;
        ASplitList[j1 + 1] := x1s;
      end;
    end;

  begin
    while (l < r) do
    begin
      if r - l <= 12 then
      begin
        Insert(l, r);
        exit;
      end;
      //    p := l + ((r - l) shr 1);
      p := (l + r) shr 1;
        // упрощенный эквивалент пред. строки
      V := AList[p];
      Vs := ASplitList[p];
      AList[p] := AList[l];
      ASplitList[p] := ASplitList[l];

      //     сортируем l+1..r относительно центра
      i := succ(l);
      j := r;
      while true do
      begin
        while (i < j) and (ACompare(v.Value, PfcxUniqueValue(AList[i]).Value) > 0) do
          inc(i);
        while (j >= i) and (ACompare(PfcxUniqueValue(AList[j]).Value, v.Value) > 0) do
          dec(j);
        if (i >= j) then
          break;
        b := AList[i];
        bs := ASplitList[i];
        AList[i] := AList[j];
        AList[j] := b;
        ASplitList[i] := ASplitList[j];
        ASplitList[j] := bs;
        dec(j);
        inc(i);
      end;
      //     центр в a[j]
      AList[l] := AList[j];
      AList[j] := v;
      ASplitList[l] := ASplitList[j];
      ASplitList[j] := vs;
      if ((j - l) <= (r - j)) then
      begin
        QuickSortOLWithSplit(l, j - 1);
        l := succ(j);
      end
      else
      begin
        QuickSortOLWithSplit(j + 1, r);
        r := pred(J);
      end;
    end;
  end;
  procedure QuickSortOLWithSplitAndOrderField(L, R: Integer);
  var
    i, j, p: integer;
    v, b: PfcxUniqueValue;
    vs, bs: PfcxPointerArray;

    procedure Insert(const l1, n1: integer);
    var
      i1, j1: integer;
      x1: PfcxUniqueValue;
      x1s: PfcxPointerArray;
    begin
      for i1 := l1 + 1 to n1 do
      begin
        x1 := AList[i1];
        x1s := ASplitList[i1];
        for j1 := i1 - 1 downto l1 do
        begin
          if ACompare(PfcxUniqueValue(ASplitList[j1][AIndexOrderField]).Value, PfcxUniqueValue(x1s[AIndexOrderField]).Value) <= 0 then
            break;
          AList[j1 + 1] := AList[j1];
          ASplitList[j1 + 1] := ASplitList[j1];
        end;
        AList[j1 + 1] := x1;
        ASplitList[j1 + 1] := x1s;
      end;
    end;

  begin
    while (l < r) do
    begin
      if r - l <= 12 then
      begin
        Insert(l, r);
        exit;
      end;
      //    p := l + ((r - l) shr 1);
      p := (l + r) shr 1;
        // упрощенный эквивалент пред. строки
      V := AList[p];
      Vs := ASplitList[p];
      AList[p] := AList[l];
      ASplitList[p] := ASplitList[l];

      //     сортируем l+1..r относительно центра
      i := succ(l);
      j := r;
      while true do
      begin
        while (i < j) and (ACompare(PfcxUniqueValue(vs[AIndexOrderField]).Value, PfcxUniqueValue(ASplitList[i][AIndexOrderField]).Value) > 0) do
          inc(i);
        while (j >= i) and (ACompare(PfcxUniqueValue(ASplitList[j][AIndexOrderField]).Value, PfcxUniqueValue(vs[AIndexOrderField]).Value) > 0) do
          dec(j);
        if (i >= j) then
          break;
        b := AList[i];
        bs := ASplitList[i];
        AList[i] := AList[j];
        AList[j] := b;
        ASplitList[i] := ASplitList[j];
        ASplitList[j] := bs;
        dec(j);
        inc(i);
      end;
      //     центр в a[j]
      AList[l] := AList[j];
      AList[j] := v;
      ASplitList[l] := ASplitList[j];
      ASplitList[j] := vs;
      if ((j - l) <= (r - j)) then
      begin
        QuickSortOLWithSplitAndOrderField(l, j - 1);
        l := succ(j);
      end
      else
      begin
        QuickSortOLWithSplitAndOrderField(j + 1, r);
        r := pred(J);
      end;
    end;
  end;

begin
  if AIndexOrderField >= 0 then
    QuickSortOLWithSplitAndOrderField(AStart, AEnd)
  else
  if ASplitList = nil then
    QuickSortOL(AStart, AEnd)
  else
    QuickSortOLWithSplit(AStart, AEnd)
end;
*)
end.


