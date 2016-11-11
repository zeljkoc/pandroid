{
    Delphi/Kylix compatibility unit: String handling routines.

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
{$inline on}
unit uPSC_strutils;

interface

uses
  SysUtils {, Types}
  ,Classes
  ,uPSComponent
  ,uPSRuntime
  ,uPSCompiler
  ;

type
(*----------------------------------------------------------------------------*)
  TPSImport_StrUtils = class(TPSPlugin)
  public
//    procedure CompOnUses(CompExec: TPSScript); override;
//    procedure ExecOnUses(CompExec: TPSScript); override;
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;


{ compile-time registration functions }
procedure SIRegister_StrUtils(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_StrUtils_Routines(S: TPSExec);

procedure Register;

{ ---------------------------------------------------------------------
    Case insensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiStartsText(const ASubText, AText: AnsiString): Boolean;inline;
Function AnsiEndsText(const ASubText, AText: AnsiString): Boolean;inline;
Function AnsiReplaceText(const AText, AFromText, AToText: AnsiString): string;inline;
Function AnsiMatchText(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
Function AnsiIndexText(const AText: AnsiString; const AValues: array of AnsiString): Integer;

{ ---------------------------------------------------------------------
    Case sensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiContainsStr(const AText, ASubText: AnsiString): Boolean;inline;
Function AnsiStartsStr(const ASubText, AText: AnsiString): Boolean;inline;
Function AnsiEndsStr(const ASubText, AText: AnsiString): Boolean;inline;
Function AnsiReplaceStr(const AText, AFromText, AToText: AnsiString): string;inline;
Function AnsiMatchStr(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
Function AnsiIndexStr(const AText: AnsiString; const AValues: array of AnsiString): Integer;

{ ---------------------------------------------------------------------
    Miscellaneous
  ---------------------------------------------------------------------}

Function DupeString(const AText: string; ACount: Integer): string;
Function ReverseString(const AText: string): string;
Function AnsiReverseString(const AText: AnsiString): String;inline;
Function StuffString(const AText: string; AStart, ALength: Cardinal;  const ASubText: string): string;
Function RandomFrom(const AValues: array of string): string; overload;
Function IfThen(AValue: Boolean; const ATrue: string; const AFalse: string = ''): string; overload;

{ ---------------------------------------------------------------------
    VB emulations.
  ---------------------------------------------------------------------}

Function LeftStr(const AText: String; const ACount: Integer): String;inline;
Function RightStr(const AText: String; const ACount: Integer): String;
Function MidStr(const AText: String; const AStart, ACount: Integer): String;inline;
Function RightBStr(const AText: String; const AByteCount: Integer): String;inline;
Function MidBStr(const AText: String; const AByteStart, AByteCount: Integer): String;inline;
Function AnsiLeftStr(const AText: String; const ACount: Integer): String;inline;
Function AnsiRightStr(const AText: String; const ACount: Integer): String;inline;
Function AnsiMidStr(const AText: String; const AStart, ACount: Integer): String;inline;
Function LeftBStr(const AText: String; const AByteCount: Integer): String;inline;

{ ---------------------------------------------------------------------
    Extended search and replace
  ---------------------------------------------------------------------}

const
  { Default word delimiters are any character except the core alphanumerics. }
  WordDelimiters: set of Char = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0'];
  
resourcestring
  SErrAmountStrings        = 'Amount of search and replace strings don''t match';

type
  TStringSearchOption = (soDown, soMatchCase, soWholeWord);
  TStringSearchOptions = set of TStringSearchOption;

Function SearchBuf(Buf: PChar; BufLen: Integer; SelStart, SelLength: Integer; SearchString: String; Options: TStringSearchOptions): PChar;
Function SearchBuf(Buf: PChar; BufLen: Integer; SelStart, SelLength: Integer; SearchString: String): PChar;inline; // ; Options: TStringSearchOptions = [soDown]
Function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;
Function PosEx(const SubStr, S: string): Integer;inline; // Offset: Cardinal = 1
Function PosEx(c:char; const S: string; Offset: Cardinal): Integer;

type
  TReplaceFlag = (rfReplaceAll, rfIgnoreCase);
  TReplaceFlags = set of TReplaceFlag;

function StringsReplace(const S: string; OldPattern, NewPattern: array of string;  Flags: TReplaceFlags): string;
Function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;

{ ---------------------------------------------------------------------
    Soundex Functions.
  ---------------------------------------------------------------------}

type
  TSoundexLength = 1..MaxInt;

Function Soundex(const AText: string; ALength: TSoundexLength): string;
Function Soundex(const AText: string): string;inline; // ; ALength: TSoundexLength = 4

type
  TSoundexIntLength = 1..8;

Function SoundexInt(const AText: string; ALength: TSoundexIntLength): Integer;
Function SoundexInt(const AText: string): Integer;inline; //; ALength: TSoundexIntLength = 4
Function DecodeSoundexInt(AValue: Integer): string;
Function SoundexWord(const AText: string): Word;
Function DecodeSoundexWord(AValue: Word): string;
Function SoundexSimilar(const AText, AOther: string; ALength: TSoundexLength): Boolean;inline;
Function SoundexSimilar(const AText, AOther: string): Boolean;inline; //; ALength: TSoundexLength = 4
Function SoundexCompare(const AText, AOther: string; ALength: TSoundexLength): Integer;inline;
Function SoundexCompare(const AText, AOther: string): Integer;inline; //; ALength: TSoundexLength = 4
Function SoundexProc(const AText, AOther: string): Boolean;

{ ---------------------------------------------------------------------
    Other functions, based on RxStrUtils.
  ---------------------------------------------------------------------}

type
  TSysCharSet = Set of char;

Function IsEmptyStr(const S: string; const EmptyChars: TSysCharSet): Boolean;
function DelSpace(const S: string): string;
function DelChars(const S: string; aChr: Char): string;
function DelSpace1(const S: string): string;
Function Tab2Space(const S: string; Numb: Byte): string;
Function NPos(const C: string; S: string; N: Integer): Integer;
Function AddChar(C: Char; const S: string; N: Integer): string;
Function AddCharR(C: Char; const S: string; N: Integer): string;
Function PadLeft(const S: string; N: Integer): string;inline;
Function PadRight(const S: string; N: Integer): string;inline;
Function PadCenter(const S: string; Len: Integer): string;
Function Copy2Symb(const S: string; Symb: Char): string;
Function Copy2SymbDel(var S: string; Symb: Char): string;
Function Copy2Space(const S: string): string;inline;
Function Copy2SpaceDel(var S: string): string;inline;
Function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;
Function WordCount(const S: string; const WordDelims: TSysCharSet): Integer;
Function WordPosition(const N: Integer; const S: string; const WordDelims: TSysCharSet): Integer;
Function ExtractWord(N: Integer; const S: string;  const WordDelims: TSysCharSet): string;inline;
Function ExtractWordPos(N: Integer; const S: string; const WordDelims: TSysCharSet; var Pos: Integer): string;
Function ExtractDelimited(N: Integer; const S: string;  const Delims: TSysCharSet): string;
Function ExtractSubstr(const S: string; var Pos: Integer;  const Delims: TSysCharSet): string;
Function IsWordPresent(const W, S: string; const WordDelims: TSysCharSet): Boolean;
Function FindPart(const HelpWilds, InputStr: string): Integer;
Function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;

Procedure RemoveLeadingChars(VAR S : String; Const CSet:TSysCharset);
Procedure RemoveTrailingChars(VAR S : String;Const CSet:TSysCharset);
Procedure RemovePadChars(VAR S : String;Const CSet:TSysCharset);

Function TrimLeftSet(const S: String;const CSet:TSysCharSet): String;
Function TrimRightSet(const S: String;const CSet:TSysCharSet): String;
Function TrimSet(const S: String;const CSet:TSysCharSet): String;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_StrUtils]);
end;

{ ---------------------------------------------------------------------
   Possibly Exception raising functions
  ---------------------------------------------------------------------}


Function Hex2Dec(const S: string): Longint;
var
  HexStr: string;
begin
  if Pos('$',S)=0 then
    HexStr:='$'+ S
  else
    HexStr:=S;
  Result:=StrToInt(HexStr);
end;

{
  We turn off implicit exceptions, since these routines are tested, and it 
  saves 20% codesize (and some speed) and don't throw exceptions, except maybe 
  heap related. If they don't, that is consider a bug.

  In the future, be wary with routines that use strtoint, floating point 
  and/or format() derivatives. And check every divisor for 0.
}

{$IMPLICITEXCEPTIONS OFF}

{ ---------------------------------------------------------------------
    Case insensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiStartsText(const ASubText, AText: AnsiString): Boolean;inline;
begin
  Result:=AnsiCompareText(Copy(AText,1,Length(AsubText)),ASubText)=0;
end;


Function AnsiEndsText(const ASubText, AText: AnsiString): Boolean;inline;
begin
 result:=AnsiCompareText(Copy(AText,Length(AText)-Length(ASubText)+1,Length(ASubText)),asubtext)=0;
end;


Function AnsiReplaceText(const AText, AFromText, AToText: AnsiString): string;inline;
begin
  Result := StringReplace(AText,AFromText,AToText,[rfReplaceAll,rfIgnoreCase]);
end;


Function AnsiMatchText(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
begin
  Result:=(AnsiIndexText(AText,AValues)<>-1)
end;


Function AnsiIndexText(const AText: AnsiString; const AValues: array of AnsiString): Integer;

var i : longint;

begin
  result:=-1;
  if high(AValues)=-1 Then
    Exit;
  for i:=low(AValues) to High(Avalues) do
     if CompareText(avalues[i],atext)=0 Then
       exit(i);  // make sure it is the first val.
end;


{ ---------------------------------------------------------------------
    Case sensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiContainsStr(const AText, ASubText: AnsiString): Boolean;inline;
begin
  Result := AnsiPos(ASubText,AText)>0;
end;


Function AnsiStartsStr(const ASubText, AText: AnsiString): Boolean;inline;
begin
  Result := AnsiPos(ASubText,AText)=1;
end;


Function AnsiEndsStr(const ASubText, AText: AnsiString): Boolean;inline;
begin
 Result := AnsiCompareStr(Copy(AText,length(AText)-length(ASubText)+1,length(ASubText)),ASubText)=0;
end;


Function AnsiReplaceStr(const AText, AFromText, AToText: AnsiString): string;inline;
begin
Result := StringReplace(AText,AFromText,AToText,[rfReplaceAll]);
end;


Function AnsiMatchStr(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
begin
  Result:=AnsiIndexStr(AText,Avalues)<>-1;
end;


Function AnsiIndexStr(const AText: AnsiString; const AValues: array of AnsiString): Integer;
var
  i : longint;
begin
  result:=-1;
  if high(AValues)=-1 Then
    Exit;
  for i:=low(AValues) to High(Avalues) do
     if (avalues[i]=AText) Then
       exit(i);                                 // make sure it is the first val.
end;


{ ---------------------------------------------------------------------
    Playthingies
  ---------------------------------------------------------------------}

Function DupeString(const AText: string; ACount: Integer): string;
var i,l : integer;
begin
 result:='';
 if aCount>=0 then
   begin
     l:=length(atext);
     SetLength(result,aCount*l);
     for i:=0 to ACount-1 do
       move(atext[1],Result[l*i+1],l);
   end;
end;

Function ReverseString(const AText: string): string;
var
    i,j:longint;

begin
  setlength(result,length(atext));
  i:=1; j:=length(atext);
  while (i<=j) do
    begin
      result[i]:=atext[j-i+1];
      inc(i);
    end;
end;

Function AnsiReverseString(const AText: AnsiString): String;inline;
begin
  Result:=ReverseString(AText);
end;

Function StuffString(const AText: string; AStart, ALength: Cardinal;  const ASubText: string): string;
var i,j,k : SizeUInt;
begin
  j:=length(ASubText);
  i:=length(AText);
  if AStart>i then 
    aStart:=i+1;
  k:=i+1-AStart;
  if ALength> k then
    ALength:=k;
  SetLength(Result,i+j-ALength);
  move (AText[1],result[1],AStart-1);
  move (ASubText[1],result[AStart],j);
  move (AText[AStart+ALength], Result[AStart+j],i+1-AStart-ALength);
end;

Function RandomFrom(const AValues: array of string): string; overload;
begin
  if high(AValues)=-1 then exit('');
  result:=Avalues[random(High(AValues)+1)];
end;

Function IfThen(AValue: Boolean; const ATrue: string; const AFalse: string = ''): string; overload;
begin
  if avalue then
    result:=atrue
  else
    result:=afalse;
end;

{ ---------------------------------------------------------------------
    VB emulations.
  ---------------------------------------------------------------------}

Function LeftStr(const AText: String; const ACount: Integer): String;inline;
begin
  Result:=Copy(AText,1,ACount);
end;

Function RightStr(const AText: String; const ACount: Integer): String;
var j,l:integer;
begin
  l:=length(atext);
  j:=ACount;
  if j>l then j:=l;
  Result:=Copy(AText,l-j+1,j);
end;

Function MidStr(const AText: String; const AStart, ACount: Integer): String;inline;
begin
  if (ACount=0) or (AStart>length(atext)) then
    exit('');
  Result:=Copy(AText,AStart,ACount);
end;

Function LeftBStr(const AText: String; const AByteCount: Integer): String;inline;
begin
  Result:=LeftStr(AText,AByteCount);
end;

Function RightBStr(const AText: String; const AByteCount: Integer): String;inline;
begin
  Result:=RightStr(Atext,AByteCount);
end;

Function MidBStr(const AText: String; const AByteStart, AByteCount: Integer): String;inline;
begin
  Result:=MidStr(AText,AByteStart,AByteCount);
end;

Function AnsiLeftStr(const AText: String; const ACount: Integer): String;inline;
begin
  Result := copy(AText,1,ACount);
end;

Function AnsiRightStr(const AText: String; const ACount: Integer): String;inline;
begin
  Result := copy(AText,length(AText)-ACount+1,ACount);
end;

Function AnsiMidStr(const AText: String; const AStart, ACount: Integer): String;inline;
begin
  Result:=Copy(AText,AStart,ACount);
end;

{ ---------------------------------------------------------------------
    Extended search and replace
  ---------------------------------------------------------------------}

type
  TEqualFunction = Function (const a,b : char) : boolean;

Function EqualWithCase (const a,b : char) : boolean;
begin
  result := (a = b);
end;

Function EqualWithoutCase (const a,b : char) : boolean;
begin
  result := (lowerCase(a) = lowerCase(b));
end;

Function IsWholeWord (bufstart, bufend, wordstart, wordend : pchar) : boolean;
begin
            // Check start
  result := ((wordstart = bufstart) or ((wordstart-1)^ in worddelimiters)) and
            // Check end
            ((wordend = bufend) or ((wordend+1)^ in worddelimiters));
end;

Function SearchDown(buf,aStart,endchar:pchar; SearchString:string;
    Equals : TEqualFunction; WholeWords:boolean) : pchar;
var Found : boolean;
    s, c : pchar;
begin
  result := aStart;
  Found := false;
  while not Found and (result <= endchar) do
    begin
    // Search first letter
    while (result <= endchar) and not Equals(result^,SearchString[1]) do
      inc (result);
    // Check if following is searchstring
    c := result;
    s := @(Searchstring[1]);
    Found := true;
    while (c <= endchar) and (s^ <> #0) and Found do
      begin
      Found := Equals(c^, s^);
      inc (c);
      inc (s);
      end;
    if s^ <> #0 then
      Found := false;
    // Check if it is a word
    if Found and WholeWords then
      Found := IsWholeWord(buf,endchar,result,c-1);
    if not found then
      inc (result);
    end;
  if not Found then
    result := nil;
end;

Function SearchUp(buf,aStart,endchar:pchar; SearchString:string;
    equals : TEqualFunction; WholeWords:boolean) : pchar;
var Found : boolean;
    s, c, l : pchar;
begin
  result := aStart;
  Found := false;
  l := @(SearchString[length(SearchString)]);
  while not Found and (result >= buf) do
    begin
    // Search last letter
    while (result >= buf) and not Equals(result^,l^) do
      dec (result);
    // Check if before is searchstring
    c := result;
    s := l;
    Found := true;
    while (c >= buf) and (s >= @SearchString[1]) and Found do
      begin
      Found := Equals(c^, s^);
      dec (c);
      dec (s);
      end;
    if (s >= @(SearchString[1])) then
      Found := false;
    // Check if it is a word
    if Found and WholeWords then
      Found := IsWholeWord(buf,endchar,c+1,result);
    if found then
      result := c+1
    else
      dec (result);
    end;
  if not Found then
    result := nil;
end;

//Function SearchDown(buf,aStart,endchar:pchar; SearchString:string; equal : TEqualFunction; WholeWords:boolean) : pchar;
Function SearchBuf(Buf: PChar;BufLen: Integer;SelStart: Integer;SelLength: Integer;
    SearchString: String;Options: TStringSearchOptions):PChar;
var
  equal : TEqualFunction;
begin
  SelStart := SelStart + SelLength;
  if (SearchString = '') or (SelStart > BufLen) or (SelStart < 0) then
    result := nil
  else
    begin
    if soMatchCase in Options then
      Equal := @EqualWithCase
    else
      Equal := @EqualWithoutCase;
    if soDown in Options then
      result := SearchDown(buf,buf+SelStart,Buf+(BufLen-1), SearchString, Equal, (soWholeWord in Options))
    else
      result := SearchUp(buf,buf+SelStart,Buf+(Buflen-1), SearchString, Equal, (soWholeWord in Options));
    end;
end;


Function SearchBuf(Buf: PChar; BufLen: Integer; SelStart, SelLength: Integer; SearchString: String): PChar;inline; // ; Options: TStringSearchOptions = [soDown]
begin
  Result:=SearchBuf(Buf,BufLen,SelStart,SelLength,SearchString,[soDown]);
end;

Function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;

var
  i,MaxLen, SubLen : SizeInt;
  SubFirst: Char;
  pc : pchar;
begin
  PosEx:=0;
  SubLen := Length(SubStr);
  if (SubLen > 0) and (Offset > 0) and (Offset <= Cardinal(Length(S))) then
   begin
    MaxLen := Length(S)- SubLen;
    SubFirst := SubStr[1];
    i := indexbyte(S[Offset],Length(S) - Offset + 1, Byte(SubFirst));
    while (i >= 0) and ((i + sizeint(Offset) - 1) <= MaxLen) do
    begin
      pc := @S[i+SizeInt(Offset)];
      //we know now that pc^ = SubFirst, because indexbyte returned a value > -1
      if (CompareByte(Substr[1],pc^,SubLen) = 0) then
      begin
        PosEx := i + SizeInt(Offset);
        Exit;
      end;
      //point Offset to next char in S
      Offset := sizeuint(i) + Offset + 1;
      i := indexbyte(S[Offset],Length(S) - Offset + 1, Byte(SubFirst));
    end;
  end;
end;

Function PosEx(c:char; const S: string; Offset: Cardinal): Integer;

var
  Len : longint;
  p: SizeInt;
begin
  Len := length(S);
  if (Offset < 1) or (Offset > SizeUInt(Length(S))) then exit(0);
  Len := length(S);
  p := indexbyte(S[Offset],Len-offset+1,Byte(c));
  if (p < 0) then
    PosEx := 0
  else
    PosEx := p + sizeint(Offset);
end; 

Function PosEx(const SubStr, S: string): Integer;inline; // Offset: Cardinal = 1
begin
  posex:=posex(substr,s,1);
end;

Function StringsReplace(const S: string; OldPattern, NewPattern: array of string;  Flags: TReplaceFlags): string;
var pc,pcc,lastpc : pchar;
    strcount      : integer;
    ResStr,
    CompStr       : string;
    Found         : Boolean;
    sc            : integer;
begin
  sc := length(OldPattern);
  if sc <> length(NewPattern) then
    raise exception.Create(SErrAmountStrings);

  dec(sc);

  if rfIgnoreCase in Flags then
    begin
    CompStr:=AnsiUpperCase(S);
    for strcount := 0 to sc do
      OldPattern[strcount] := AnsiUpperCase(OldPattern[strcount]);
    end
  else
    CompStr := s;

  ResStr := '';
  pc := @CompStr[1];
  pcc := @s[1];
  lastpc := pc+Length(S);

  while pc < lastpc do
    begin
    Found := False;
    for strcount := 0 to sc do
      begin
      if (length(OldPattern[strcount])>0) and
         (OldPattern[strcount][1]=pc^) and
         (Length(OldPattern[strcount]) <= (lastpc-pc)) and
         (CompareByte(OldPattern[strcount][1],pc^,Length(OldPattern[strcount]))=0) then
        begin
        ResStr := ResStr + NewPattern[strcount];
        pc := pc+Length(OldPattern[strcount]);
        pcc := pcc+Length(OldPattern[strcount]);
        Found := true;
        end
      end;
    if not found then
      begin
      ResStr := ResStr + pcc^;
      inc(pc);
      inc(pcc);
      end
    else if not (rfReplaceAll in Flags) then
      begin
      ResStr := ResStr + StrPas(pcc);
      break;
      end;
    end;
  Result := ResStr;
end;

Function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;
var
  Srch,OldP,RemS: string; // Srch and Oldp can contain uppercase versions of S,OldPattern
  P : Integer;
begin
  Srch:=S;
  OldP:=OldPattern;
  if rfIgnoreCase in Flags then
    begin
    Srch:=AnsiUpperCase(Srch);
    OldP:=AnsiUpperCase(OldP);
    end;
  RemS:=S;
  Result:='';
  while (Length(Srch)<>0) do
    begin
    P:=AnsiPos(OldP, Srch);
    if P=0 then
      begin
      Result:=Result+RemS;
      Srch:='';
      end
    else
      begin
      Result:=Result+Copy(RemS,1,P-1)+NewPattern;
      P:=P+Length(OldP);
      RemS:=Copy(RemS,P,Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then
        begin
        Result:=Result+RemS;
        Srch:='';
        end
      else
         Srch:=Copy(Srch,P,Length(Srch)-P+1);
      end;
    end;
end;

{ ---------------------------------------------------------------------
    Soundex Functions.
  ---------------------------------------------------------------------}
Const
SScore : array[1..255] of Char =
     ('0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 1..32
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 33..64
      '0','1','2','3','0','1','2','i','0','2','2','4','5','5','0','1','2','6','2','3','0','1','i','2','i','2', // 64..90
      '0','0','0','0','0','0', // 91..95
      '0','1','2','3','0','1','2','i','0','2','2','4','5','5','0','1','2','6','2','3','0','1','i','2','i','2', // 96..122
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 123..154
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 155..186
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 187..218
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 219..250
      '0','0','0','0','0'); // 251..255


Function Soundex(const AText: string; ALength: TSoundexLength): string;
Var
  S,PS : Char;
  I,L : integer;
begin
  Result:='';
  PS:=#0;
  If Length(AText)>0 then
    begin
    Result:=Upcase(AText[1]);
    I:=2;
    L:=Length(AText);
    While (I<=L) and (Length(Result)<ALength) do
      begin
      S:=SScore[Ord(AText[i])];
      If Not (S in ['0','i',PS]) then
        Result:=Result+S;
      If (S<>'i') then
        PS:=S;
      Inc(I);
      end;
    end;
  L:=Length(Result);
  If (L<ALength) then
    Result:=Result+StringOfChar('0',Alength-L);
end;

Function Soundex(const AText: string): string;inline; // ; ALength: TSoundexLength = 4
begin
  Result:=Soundex(AText,4);
end;

Const
  Ord0 = Ord('0');
  OrdA = Ord('A');

Function SoundexInt(const AText: string; ALength: TSoundexIntLength): Integer;
var
  SE: string;
  I: Integer;
begin
  Result:=-1;
  SE:=Soundex(AText,ALength);
  If Length(SE)>0 then
    begin
    Result:=Ord(SE[1])-OrdA;
    if ALength > 1 then
      begin
      Result:=Result*26+(Ord(SE[2])-Ord0);
      for I:=3 to ALength do
        Result:=(Ord(SE[I])-Ord0)+Result*7;
      end;
    Result:=ALength+Result*9;
    end;
end;


Function SoundexInt(const AText: string): Integer;inline; //; ALength: TSoundexIntLength = 4
begin
  Result:=SoundexInt(AText,4);
end;


Function DecodeSoundexInt(AValue: Integer): string;
var
  I, Len: Integer;
begin
  Result := '';
  Len := AValue mod 9;
  AValue := AValue div 9;
  for I:=Len downto 3 do
    begin
    Result:=Chr(Ord0+(AValue mod 7))+Result;
    AValue:=AValue div 7;
    end;
  if Len>1 then
    begin
    Result:=Chr(Ord0+(AValue mod 26))+Result;
    AValue:=AValue div 26;
    end;
  Result:=Chr(OrdA+AValue)+Result;
end;


Function SoundexWord(const AText: string): Word;
Var
  S : String;
begin
  S:=SoundEx(Atext,4);
  Result:=Ord(S[1])-OrdA;
  Result:=Result*26+ord(S[2])-48;
  Result:=Result*7+ord(S[3])-48;
  Result:=Result*7+ord(S[4])-48;
end;

Function DecodeSoundexWord(AValue: Word): string;
begin
  Result := Chr(Ord0+ (AValue mod 7));
  AValue := AValue div 7;
  Result := Chr(Ord0+ (AValue mod 7)) + Result;
  AValue := AValue div 7;
  Result := IntToStr(AValue mod 26) + Result;
  AValue := AValue div 26;
  Result := Chr(OrdA+AValue) + Result;
end;

Function SoundexSimilar(const AText, AOther: string; ALength: TSoundexLength): Boolean;inline;
begin
  Result:=Soundex(AText,ALength)=Soundex(AOther,ALength);
end;

Function SoundexSimilar(const AText, AOther: string): Boolean;inline; //; ALength: TSoundexLength = 4
begin
  Result:=SoundexSimilar(AText,AOther,4);
end;

Function SoundexCompare(const AText, AOther: string; ALength: TSoundexLength): Integer;inline;
begin
  Result:=AnsiCompareStr(Soundex(AText,ALength),Soundex(AOther,ALength));
end;

Function SoundexCompare(const AText, AOther: string): Integer;inline; //; ALength: TSoundexLength = 4
begin
  Result:=SoundexCompare(AText,AOther,4);
end;

Function SoundexProc(const AText, AOther: string): Boolean;
begin
  Result:=SoundexSimilar(AText,AOther);
end;

{ ---------------------------------------------------------------------
    RxStrUtils-like Functions.
  ---------------------------------------------------------------------}

Function IsEmptyStr(const S: string; const EmptyChars: TSysCharSet): Boolean;
var
  i,l: Integer;
begin
  l:=Length(S);
  i:=1;
  Result:=True;
  while Result and (i<=l) do
    begin
    Result:=(S[i] in EmptyChars);
    Inc(i);
    end;
end;
function DelSpace(const S: String): string;

begin
  Result:=DelChars(S,' ');
end;

function DelChars(const S: string; aChr: Char): string;
var
  I,J: Integer;
begin
  Result:=S;
  I:=Length(Result);
  While I>0 do
    begin
    if Result[I]=aChr then
      begin
      J:=I-1;
      While (J>0) and (Result[J]=aChr) do
        Dec(j);
      Delete(Result,J+1,I-J);
      I:=J+1;
      end;
    dec(I);
    end;
end;

function DelSpace1(const S: string): string;
var
  i: Integer;

begin
  Result:=S;
  for i:=Length(Result) downto 2 do
    if (Result[i]=' ') and (Result[I-1]=' ') then
      Delete(Result,I,1);
end;

Function Tab2Space(const S: string; Numb: Byte): string;
var
  I: Integer;
begin
  I:=1;
  Result:=S;
  while I <= Length(Result) do
    if Result[I]<>Chr(9) then
      inc(I)
    else
      begin
      Result[I]:=' ';
      If (Numb>1) then
        Insert(StringOfChar(' ',Numb-1),Result,I);
      Inc(I,Numb);
      end;
end;

Function NPos(const C: string; S: string; N: Integer): Integer;
var
  i,p,k: Integer;
begin
  Result:=0;
  if N<1 then
    Exit;
  k:=0;
  i:=1;
  Repeat
    p:=pos(C,S);
    Inc(k,p);
    if p>0 then
      delete(S,1,p);
    Inc(i);
  Until (i>n) or (p=0);
  If (P>0) then
    Result:=K;
end;

Function AddChar(C: Char; const S: string; N: Integer): string;
Var
  l : Integer;
begin
  Result:=S;
  l:=Length(Result);
  if l<N then
    Result:=StringOfChar(C,N-l)+Result;
end;

Function AddCharR(C: Char; const S: string; N: Integer): string;
Var
  l : Integer;
begin
  Result:=S;
  l:=Length(Result);
  if l<N then
    Result:=Result+StringOfChar(C,N-l);
end;

Function PadRight(const S: string; N: Integer): string;inline;
begin
  Result:=AddCharR(' ',S,N);
end;

Function PadLeft(const S: string; N: Integer): string;inline;
begin
  Result:=AddChar(' ',S,N);
end;

Function Copy2Symb(const S: string; Symb: Char): string;
var
  p: Integer;
begin
  p:=Pos(Symb,S);
  if p=0 then
    p:=Length(S)+1;
  Result:=Copy(S,1,p-1);
end;

Function Copy2SymbDel(var S: string; Symb: Char): string;
var
  p: Integer;
begin
  p:=Pos(Symb,S);
  if p=0 then
    begin
      result:=s;
      s:='';
    end
  else
    begin	
      Result:=Copy(S,1,p-1);
      delete(s,1,p);		
    end;
end;

Function Copy2Space(const S: string): string;inline;
begin
  Result:=Copy2Symb(S,' ');
end;

Function Copy2SpaceDel(var S: string): string;inline;
begin
  Result:=Copy2SymbDel(S,' ');
end;

Function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;
var
//  l :  Integer;
  P,PE : PChar;
begin
  Result:=AnsiLowerCase(S);
  P:=PChar(pointer(Result));
  PE:=P+Length(Result);
  while (P<PE) do
    begin
    while (P<PE) and (P^ in WordDelims) do
      inc(P);
    if (P<PE) then
      P^:=UpCase(P^);
    while (P<PE) and not (P^ in WordDelims) do
      inc(P);
    end;
end;

Function WordCount(const S: string; const WordDelims: TSysCharSet): Integer;
var
  P,PE : PChar;
begin
  Result:=0;
  P:=Pchar(pointer(S));
  PE:=P+Length(S);
  while (P<PE) do
    begin
    while (P<PE) and (P^ in WordDelims) do
      Inc(P);
    if (P<PE) then
      inc(Result);
    while (P<PE) and not (P^ in WordDelims) do
      inc(P);
    end;
end;

Function WordPosition(const N: Integer; const S: string; const WordDelims: TSysCharSet): Integer;
var
  PS,P,PE : PChar;
  Count: Integer;
begin
  Result:=0;
  Count:=0;
  PS:=PChar(pointer(S));
  PE:=PS+Length(S);
  P:=PS;
  while (P<PE) and (Count<>N) do
    begin
    while (P<PE) and (P^ in WordDelims) do
      inc(P);
    if (P<PE) then
      inc(Count);
    if (Count<>N) then
      while (P<PE) and not (P^ in WordDelims) do
        inc(P)
    else
      Result:=(P-PS)+1;
    end;
end;


Function ExtractWord(N: Integer; const S: string; const WordDelims: TSysCharSet): string;inline;
var
  i: Integer;
begin
  Result:=ExtractWordPos(N,S,WordDelims,i);
end;


Function ExtractWordPos(N: Integer; const S: string; const WordDelims: TSysCharSet; var Pos: Integer): string;
var
  i,j,l: Integer;
begin
  j:=0;
  i:=WordPosition(N, S, WordDelims);
  Pos:=i;
  if (i<>0) then
    begin
    j:=i;
    l:=Length(S);
    while (j<=L) and not (S[j] in WordDelims) do
      inc(j);
    end;
  SetLength(Result,j-i);
  If ((j-i)>0) then
    Move(S[i],Result[1],j-i);
end;

Function ExtractDelimited(N: Integer; const S: string; const Delims: TSysCharSet): string;
var
  w,i,l,len: Integer;
begin
  w:=0;
  i:=1;
  l:=0;
  len:=Length(S);
  SetLength(Result, 0);
  while (i<=len) and (w<>N) do
    begin
    if s[i] in Delims then
      inc(w)
    else
      begin
      if (N-1)=w then
        begin
        inc(l);
        SetLength(Result,l);
        Result[L]:=S[i];
        end;
      end;
    inc(i);
    end;
end;

Function ExtractSubstr(const S: string; var Pos: Integer; const Delims: TSysCharSet): string;
var
  i,l: Integer;
begin
  i:=Pos;
  l:=Length(S);
  while (i<=l) and not (S[i] in Delims) do
    inc(i);
  Result:=Copy(S,Pos,i-Pos);
  while (i<=l) and (S[i] in Delims) do
    inc(i);
  Pos:=i;
end;

Function isWordPresent(const W, S: string; const WordDelims: TSysCharSet): Boolean;
var
  i,Count : Integer;
begin
  Result:=False;
  Count:=WordCount(S, WordDelims);
  I:=1;
  While (Not Result) and (I<=Count) do
    begin
    Result:=ExtractWord(i,S,WordDelims)=W;
    Inc(i);
    end;
end;


Function PadCenter(const S: string; Len: Integer): string;
begin
  if Length(S)<Len then
    begin
    Result:=StringOfChar(' ',(Len div 2) -(Length(S) div 2))+S;
    Result:=Result+StringOfChar(' ',Len-Length(Result));
    end
  else
    Result:=S;
end;

Function FindPart(const HelpWilds, inputStr: string): Integer;
var
  i, J: Integer;
  Diff: Integer;
begin
  Result:=0;
  i:=Pos('?',HelpWilds);
  if (i=0) then
    Result:=Pos(HelpWilds, inputStr)
  else
    begin
    Diff:=Length(inputStr) - Length(HelpWilds);
    for i:=0 to Diff do
      begin
      for J:=1 to Length(HelpWilds) do
        if (inputStr[i + J] = HelpWilds[J]) or (HelpWilds[J] = '?') then
          begin
          if (J=Length(HelpWilds)) then
            begin
            Result:=i+1;
            Exit;
            end;
          end
        else
          Break;
      end;
    end;
end;

Function isWild(inputStr, Wilds: string; ignoreCase: Boolean): Boolean;

 Function SearchNext(var Wilds: string): Integer;
 begin
   Result:=Pos('*', Wilds);
   if Result>0 then
     Wilds:=Copy(Wilds,1,Result - 1);
 end;

var
  CWild, CinputWord: Integer; { counter for positions }
  i, LenHelpWilds: Integer;
  MaxinputWord, MaxWilds: Integer; { Length of inputStr and Wilds }
  HelpWilds: string;
begin
  if Wilds = inputStr then begin
    Result:=True;
    Exit;
  end;
  repeat { delete '**', because '**' = '*' }
    i:=Pos('**', Wilds);
    if i > 0 then
      Wilds:=Copy(Wilds, 1, i - 1) + '*' + Copy(Wilds, i + 2, Maxint);
  until i = 0;
  if Wilds = '*' then begin { for fast end, if Wilds only '*' }
    Result:=True;
    Exit;
  end;
  MaxinputWord:=Length(inputStr);
  MaxWilds:=Length(Wilds);
  if ignoreCase then begin { upcase all letters }
    inputStr:=AnsiUpperCase(inputStr);
    Wilds:=AnsiUpperCase(Wilds);
  end;
  if (MaxWilds = 0) or (MaxinputWord = 0) then begin
    Result:=False;
    Exit;
  end;
  CinputWord:=1;
  CWild:=1;
  Result:=True;
  repeat
    if inputStr[CinputWord] = Wilds[CWild] then begin { equal letters }
      { goto next letter }
      inc(CWild);
      inc(CinputWord);
      Continue;
    end;
    if Wilds[CWild] = '?' then begin { equal to '?' }
      { goto next letter }
      inc(CWild);
      inc(CinputWord);
      Continue;
    end;
    if Wilds[CWild] = '*' then begin { handling of '*' }
      HelpWilds:=Copy(Wilds, CWild + 1, MaxWilds);
      i:=SearchNext(HelpWilds);
      LenHelpWilds:=Length(HelpWilds);
      if i = 0 then begin
        { no '*' in the rest, compare the ends }
        if HelpWilds = '' then Exit; { '*' is the last letter }
        { check the rest for equal Length and no '?' }
        for i:=0 to LenHelpWilds - 1 do begin
          if (HelpWilds[LenHelpWilds - i] <> inputStr[MaxinputWord - i]) and
            (HelpWilds[LenHelpWilds - i]<> '?') then
          begin
            Result:=False;
            Exit;
          end;
        end;
        Exit;
      end;
      { handle all to the next '*' }
      inc(CWild, 1 + LenHelpWilds);
      i:=FindPart(HelpWilds, Copy(inputStr, CinputWord, Maxint));
      if i= 0 then begin
        Result:=False;
        Exit;
      end;
      CinputWord:=i + LenHelpWilds;
      Continue;
    end;
    Result:=False;
    Exit;
  until (CinputWord > MaxinputWord) or (CWild > MaxWilds);
  { no completed evaluation }
  if CinputWord <= MaxinputWord then Result:=False;
  if (CWild <= MaxWilds) and (Wilds[MaxWilds] <> '*') then Result:=False;
end;

Function XorString(const Key, Src: String): String;
var
  i: Integer;
begin
  Result:=Src;
  if Length(Key) > 0 then
    for i:=1 to Length(Src) do
      Result[i]:=Chr(Byte(Key[1 + ((i - 1) mod Length(Key))]) xor Ord(Src[i]));
end;

Function XorEncode(const Key, Source: string): string;
var
  i: Integer;
  C: Byte;
begin
  Result:='';
  for i:=1 to Length(Source) do
    begin
    if Length(Key) > 0 then
      C:=Byte(Key[1 + ((i - 1) mod Length(Key))]) xor Byte(Source[i])
    else
      C:=Byte(Source[i]);
    Result:=Result+AnsiLowerCase(intToHex(C, 2));
    end;
end;

Function XorDecode(const Key, Source: string): string;
var
  i: Integer;
  C: Char;
begin
  Result:='';
  for i:=0 to Length(Source) div 2 - 1 do
    begin
    C:=Chr(StrTointDef('$' + Copy(Source, (i * 2) + 1, 2), Ord(' ')));
    if Length(Key) > 0 then
      C:=Chr(Byte(Key[1 + (i mod Length(Key))]) xor Byte(C));
    Result:=Result + C;
    end;
end;

Procedure RemoveLeadingChars(VAR S : String; Const CSet:TSysCharset);
VAR I,J : Longint;
Begin
 I:=Length(S); 
 IF (I>0) Then
  Begin
   J:=1;
   While (J<=I) And (S[J] IN CSet) DO 
     INC(J);
   IF J>1 Then
    Delete(S,1,J-1);
   End;
End;


Function TrimLeftSet(const S: String;const CSet:TSysCharSet): String;
begin
  result:=s;
  removeleadingchars(result,cset); 
end;

Procedure RemoveTrailingChars(VAR S : String;Const CSet:TSysCharset);
VAR I,J: LONGINT;
Begin
 I:=Length(S);
 IF (I>0) Then
  Begin
   J:=I;
   While (j>0) and (S[J] IN CSet) DO DEC(J);
   IF J<>I Then
    SetLength(S,J);
  End;
End;

Function TrimRightSet(const S: String;const CSet:TSysCharSet): String;
begin
  result:=s;
  RemoveTrailingchars(result,cset); 
end;

Procedure RemovePadChars(VAR S : String;Const CSet:TSysCharset);
VAR I,J,K: LONGINT;
Begin
 I:=Length(S);
 IF (I>0) Then
  Begin
   J:=I;
   While (j>0) and (S[J] IN CSet) DO DEC(J);
   if j=0 Then
     begin 
       s:='';
       exit;
     end;
   k:=1;
   While (k<=I) And (S[k] IN CSet) DO 
     INC(k);
   IF k>1 Then
     begin
       move(s[k],s[1],j-k+1);
       setlength(s,j-k+1);
     end
   else
     setlength(s,j);  
  End;
End;

Function TrimSet(const S: String;const CSet:TSysCharSet): String;
begin
  result:=s;
  RemovePadChars(result,cset); 
end;

(* === compile-time registration Functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_StrUtils(CL: TPSPascalCompiler);
begin
 CL.AddDelphiFunction('Function AnsiStartsText( const ASubText, AText : AnsiString) : Boolean');
 CL.AddDelphiFunction('Function AnsiEndsText( const ASubText, AText : AnsiString) : Boolean');
 CL.AddDelphiFunction('Function AnsiReplaceText( const AText, AFromText, AToText : AnsiString) : AnsiString');
 CL.AddDelphiFunction('Function AnsiMatchText( const AText : AnsiString; const AValues : array of AnsiString) : Boolean');
 CL.AddDelphiFunction('Function AnsiIndexText( const AText : AnsiString; const AValues : array of AnsiString) : Integer');
 CL.AddDelphiFunction('Function AnsiContainsStr( const AText, ASubText : AnsiString) : Boolean');
 CL.AddDelphiFunction('Function AnsiStartsStr( const ASubText, AText : AnsiString) : Boolean');
 CL.AddDelphiFunction('Function AnsiEndsStr( const ASubText, AText : AnsiString) : Boolean');
 CL.AddDelphiFunction('Function AnsiReplaceStr( const AText, AFromText, AToText : AnsiString) : AnsiString');
 CL.AddDelphiFunction('Function AnsiMatchStr( const AText : AnsiString; const AValues : array of AnsiString) : Boolean');
 CL.AddDelphiFunction('Function AnsiIndexStr( const AText : AnsiString; const AValues : array of AnsiString) : Integer');
 CL.AddDelphiFunction('Function DupeString( const AText : string; ACount : Integer) : string');
 CL.AddDelphiFunction('Function ReverseString( const AText : string) : string');
 CL.AddDelphiFunction('Function AnsiReverseString( const AText : AnsiString) : String');
 CL.AddDelphiFunction('Function StuffString( const AText : string; AStart, ALength : Cardinal; const ASubText : string) : string');
 CL.AddDelphiFunction('Function RandomFrom( const AValues : array of string) : string;');
 CL.AddDelphiFunction('Function IfThen( AValue : Boolean; const ATrue : string; AFalse : string) : string;');
 CL.AddDelphiFunction('Function LeftStr( const AText : String; const ACount : Integer) : String;');
 CL.AddDelphiFunction('Function RightStr( const AText : String; const ACount : Integer) : String;');
 CL.AddDelphiFunction('Function MidStr( const AText : String; const AStart, ACount : Integer) : String;');
 CL.AddDelphiFunction('Function LeftBStr( const AText : String; const AByteCount : Integer) : String');
 CL.AddDelphiFunction('Function RightBStr( const AText : String; const AByteCount : Integer) : String');
 CL.AddDelphiFunction('Function MidBStr( const AText : String; const AByteStart, AByteCount : Integer) : String');
 CL.AddDelphiFunction('Function AnsiLeftStr( const AText : String; const ACount : Integer) : String');
 CL.AddDelphiFunction('Function AnsiRightStr( const AText : String; const ACount : Integer) : String');
 CL.AddDelphiFunction('Function AnsiMidStr( const AText : String; const AStart, ACount : Integer) : String');
 CL.AddTypeS('TStringSearchOption', '( soDown, soMatchCase, soWholeWord )');
 CL.AddTypeS('TStringSearchOptions', 'set of TStringSearchOption');
 CL.AddDelphiFunction('Function SearchBuf( Buf : PChar; BufLen : Integer; SelStart, SelLength : Integer; SearchString : String; Options : TStringSearchOptions) : PChar');
 CL.AddDelphiFunction('Function PosEx( const SubStr, S : string; Offset : Cardinal) : Integer');
 CL.AddTypeS('TSoundexLength', 'Integer');
 CL.AddDelphiFunction('Function Soundex( const AText : string; ALength : TSoundexLength) : string');
 CL.AddTypeS('TSoundexIntLength', 'Integer');
 CL.AddDelphiFunction('Function SoundexInt( const AText : string; ALength : TSoundexIntLength) : Integer');
 CL.AddDelphiFunction('Function DecodeSoundexInt( AValue : Integer) : string');
 CL.AddDelphiFunction('Function SoundexWord( const AText : string) : Word');
 CL.AddDelphiFunction('Function DecodeSoundexWord( AValue : Word) : string');
 CL.AddDelphiFunction('Function SoundexSimilar( const AText, AOther : string; ALength : TSoundexLength) : Boolean');
 CL.AddDelphiFunction('Function SoundexCompare( const AText, AOther : string; ALength : TSoundexLength) : Integer');
 CL.AddDelphiFunction('Function SoundexProc( const AText, AOther : string) : Boolean');
 CL.AddDelphiFunction('Function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;');

 CL.AddTypeS('TReplaceFlag', '( rfReplaceAll, rfIgnoreCase )');
 CL.AddTypeS('TReplaceFlags', 'set of TReplaceFlag');
 CL.AddDelphiFunction('Function StringsReplace(const S: string; OldPattern, NewPattern: array of string;  Flags: TReplaceFlags): string;');
 CL.AddDelphiFunction('Function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;');

 CL.AddTypeS('TSysCharSet','Set of char');
 CL.AddDelphiFunction('Function IsEmptyStr(const S: string; const EmptyChars: TSysCharSet): Boolean;');
 CL.AddDelphiFunction('function DelSpace(const S: string): string;');
 CL.AddDelphiFunction('function DelChars(const S: string; aChr: Char): string;');
 CL.AddDelphiFunction('function DelSpace1(const S: string): string;');

 CL.AddDelphiFunction('Function Tab2Space(const S: string; Numb: Byte): string;');
 CL.AddDelphiFunction('Function NPos(const C: string; S: string; N: Integer): Integer;');
 CL.AddDelphiFunction('Function AddChar(C: Char; const S: string; N: Integer): string;');
 CL.AddDelphiFunction('Function AddCharR(C: Char; const S: string; N: Integer): string;');
 CL.AddDelphiFunction('Function PadLeft(const S: string; N: Integer): string;inline;');
 CL.AddDelphiFunction('Function PadRight(const S: string; N: Integer): string;inline;');
 CL.AddDelphiFunction('Function PadCenter(const S: string; Len: Integer): string;');
 CL.AddDelphiFunction('Function Copy2Symb(const S: string; Symb: Char): string;');
 CL.AddDelphiFunction('Function Copy2SymbDel(var S: string; Symb: Char): string;');
 CL.AddDelphiFunction('Function Copy2Space(const S: string): string;inline;');
 CL.AddDelphiFunction('Function Copy2SpaceDel(var S: string): string;inline;');
 CL.AddDelphiFunction('Function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;');
 CL.AddDelphiFunction('Function WordCount(const S: string; const WordDelims: TSysCharSet): Integer;');
 CL.AddDelphiFunction('Function WordPosition(const N: Integer; const S: string; const WordDelims: TSysCharSet): Integer;');
 CL.AddDelphiFunction('Function ExtractWord(N: Integer; const S: string;  const WordDelims: TSysCharSet): string;inline;');
 CL.AddDelphiFunction('Function ExtractWordPos(N: Integer; const S: string; const WordDelims: TSysCharSet; var Pos: Integer): string;');
 CL.AddDelphiFunction('Function ExtractDelimited(N: Integer; const S: string;  const Delims: TSysCharSet): string;');
 CL.AddDelphiFunction('Function ExtractSubstr(const S: string; var Pos: Integer;  const Delims: TSysCharSet): string;');
 CL.AddDelphiFunction('Function IsWordPresent(const W, S: string; const WordDelims: TSysCharSet): Boolean;');
 CL.AddDelphiFunction('Function FindPart(const HelpWilds, InputStr: string): Integer;');
 CL.AddDelphiFunction('Function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;');
 CL.AddDelphiFunction('Function XorString(const Key, Src: String): String;');
 CL.AddDelphiFunction('Function XorEncode(const Key, Source: string): string;');
 CL.AddDelphiFunction('Function XorDecode(const Key, Source: string): string;');
 CL.AddDelphiFunction('Procedure RemoveLeadingChars(VAR S : String; Const CSet:TSysCharset);');
 CL.AddDelphiFunction('Procedure RemoveTrailingChars(VAR S : String;Const CSet:TSysCharset);');
 CL.AddDelphiFunction('Procedure RemovePadChars(VAR S : String;Const CSet:TSysCharset);');
 CL.AddDelphiFunction('Function TrimLeftSet(const S: String;const CSet:TSysCharSet): String;');
 CL.AddDelphiFunction('Function TrimRightSet(const S: String;const CSet:TSysCharSet): String;');
 CL.AddDelphiFunction('Function TrimSet(const S: String;const CSet:TSysCharSet): String;');

end;

procedure RIRegister_StrUtils_Routines(S: TPSExec);
begin
 S.RegisterDelphiFunction(@AnsiStartsText, 'AnsiStartsText', cdRegister);
 S.RegisterDelphiFunction(@AnsiEndsText, 'AnsiEndsText', cdRegister);
 S.RegisterDelphiFunction(@AnsiReplaceText, 'AnsiReplaceText', cdRegister);
 S.RegisterDelphiFunction(@AnsiMatchText, 'AnsiMatchText', cdRegister);
 S.RegisterDelphiFunction(@AnsiIndexText, 'AnsiIndexText', cdRegister);
 S.RegisterDelphiFunction(@AnsiContainsStr, 'AnsiContainsStr', cdRegister);
 S.RegisterDelphiFunction(@AnsiStartsStr, 'AnsiStartsStr', cdRegister);
 S.RegisterDelphiFunction(@AnsiEndsStr, 'AnsiEndsStr', cdRegister);
 S.RegisterDelphiFunction(@AnsiReplaceStr, 'AnsiReplaceStr', cdRegister);
 S.RegisterDelphiFunction(@AnsiMatchStr, 'AnsiMatchStr', cdRegister);
 S.RegisterDelphiFunction(@AnsiIndexStr, 'AnsiIndexStr', cdRegister);
 S.RegisterDelphiFunction(@DupeString, 'DupeString', cdRegister);
 S.RegisterDelphiFunction(@ReverseString, 'ReverseString', cdRegister);
 S.RegisterDelphiFunction(@AnsiReverseString, 'AnsiReverseString', cdRegister);
 S.RegisterDelphiFunction(@StuffString, 'StuffString', cdRegister);
 S.RegisterDelphiFunction(@RandomFrom, 'RandomFrom', cdRegister);
 S.RegisterDelphiFunction(@IfThen, 'IfThen', cdRegister);
 S.RegisterDelphiFunction(@LeftStr, 'LeftStr', cdRegister);
 S.RegisterDelphiFunction(@RightStr, 'RightStr', cdRegister);
 S.RegisterDelphiFunction(@MidStr, 'MidStr', cdRegister);
 S.RegisterDelphiFunction(@LeftBStr, 'LeftBStr', cdRegister);
 S.RegisterDelphiFunction(@RightBStr, 'RightBStr', cdRegister);
 S.RegisterDelphiFunction(@MidBStr, 'MidBStr', cdRegister);
 S.RegisterDelphiFunction(@AnsiLeftStr, 'AnsiLeftStr', cdRegister);
 S.RegisterDelphiFunction(@AnsiRightStr, 'AnsiRightStr', cdRegister);
 S.RegisterDelphiFunction(@AnsiMidStr, 'AnsiMidStr', cdRegister);
 S.RegisterDelphiFunction(@SearchBuf, 'SearchBuf', cdRegister);
 S.RegisterDelphiFunction(@PosEx, 'PosEx', cdRegister);
 S.RegisterDelphiFunction(@Soundex, 'Soundex', cdRegister);
 S.RegisterDelphiFunction(@SoundexInt, 'SoundexInt', cdRegister);
 S.RegisterDelphiFunction(@DecodeSoundexInt, 'DecodeSoundexInt', cdRegister);
 S.RegisterDelphiFunction(@SoundexWord, 'SoundexWord', cdRegister);
 S.RegisterDelphiFunction(@DecodeSoundexWord, 'DecodeSoundexWord', cdRegister);
 S.RegisterDelphiFunction(@SoundexSimilar, 'SoundexSimilar', cdRegister);
 S.RegisterDelphiFunction(@SoundexCompare, 'SoundexCompare', cdRegister);
 S.RegisterDelphiFunction(@SoundexProc, 'SoundexProc', cdRegister);
 S.RegisterDelphiFunction(@SearchBuf,'SerandomizearchBuf',cdRegister);
 S.RegisterDelphiFunction(@PosEx,'PosEx', cdRegister);
 S.RegisterDelphiFunction(@StringsReplace,'StringsReplace',cdRegister);
 S.RegisterDelphiFunction(@StringReplace,'StringReplace',cdRegister);

 S.RegisterDelphiFunction(@IsEmptyStr,'IsEmptyStr',cdRegister);
 S.RegisterDelphiFunction(@DelSpace,'DelSpace',cdRegister);
 S.RegisterDelphiFunction(@DelChars,'DelChars',cdRegister);
 S.RegisterDelphiFunction(@DelSpace1,'DelSpace1',cdRegister);
 S.RegisterDelphiFunction(@Tab2Space,'Tab2Space',cdRegister);
 S.RegisterDelphiFunction(@NPos,'NPos',cdRegister);
 S.RegisterDelphiFunction(@AddChar,'AddChar',cdRegister);
 S.RegisterDelphiFunction(@AddCharR,'AddCharR',cdRegister);
 S.RegisterDelphiFunction(@PadLeft,'PadLeft',cdRegister);
 S.RegisterDelphiFunction(@PadRight,'PadRight',cdRegister);
 S.RegisterDelphiFunction(@PadCenter,'PadCenter',cdRegister);
 S.RegisterDelphiFunction(@Copy2Symb,'Copy2Symb',cdRegister);
 S.RegisterDelphiFunction(@Copy2SymbDel,'Copy2SymbDel',cdRegister);
 S.RegisterDelphiFunction(@Copy2Space,'Copy2Space',cdRegister);
 S.RegisterDelphiFunction(@Copy2SpaceDel,'Copy2SpaceDel',cdRegister);
 S.RegisterDelphiFunction(@AnsiProperCase,'AnsiProperCase',cdRegister);
 S.RegisterDelphiFunction(@WordCount,'WordCount',cdRegister);
 S.RegisterDelphiFunction(@WordPosition,'WordPosition',cdRegister);

 S.RegisterDelphiFunction(@ExtractWord,'ExtractWord',cdRegister);
 S.RegisterDelphiFunction(@ExtractWordPos,'ExtractWordPos',cdRegister);
 S.RegisterDelphiFunction(@ExtractDelimited,'ExtractDelimited',cdRegister);
 S.RegisterDelphiFunction(@ExtractSubstr,'ExtractSubstr',cdRegister);
 S.RegisterDelphiFunction(@IsWordPresent,'IsWordPresent',cdRegister);
 S.RegisterDelphiFunction(@FindPart,'FindPart',cdRegister);
 S.RegisterDelphiFunction(@IsWild,'IsWild',cdRegister);

 S.RegisterDelphiFunction(@XorString,'XorString',cdRegister);
 S.RegisterDelphiFunction(@XorEncode,'XorEncode',cdRegister);
 S.RegisterDelphiFunction(@XorDecode,'XorDecode',cdRegister);
 S.RegisterDelphiFunction(@RemoveLeadingChars,'RemoveLeadingChars',cdRegister);
 S.RegisterDelphiFunction(@RemoveTrailingChars,'RemoveTrailingChars',cdRegister);
 S.RegisterDelphiFunction(@RemovePadChars,'RemovePadChars',cdRegister);
 S.RegisterDelphiFunction(@TrimLeftSet,'TrimLeftSet',cdRegister);
 S.RegisterDelphiFunction(@TrimRightSet,'TrimRightSet',cdRegister);
 S.RegisterDelphiFunction(@TrimSet,'TrimSet',cdRegister);
end;


{ TPSImport_StrUtils }

(*----------------------------------------------------------------------------*)
procedure TPSImport_StrUtils.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_StrUtils(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_StrUtils.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_StrUtils_Routines(CompExec.Exec); // comment it if no routines
end;
(*----------------------------------------------------------------------------*)

end.
