unit EncodeDecodeCodePage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


function CP1250ToUTF8(const s: string): string; // central europe
function UTF8ToCP1250(const s: string; SetTargetCodePage: boolean = false): RawByteString; // central europe

implementation

type
  TCharToUTF8Table = array[char] of PChar;
  TUnicodeToCharID = function(Unicode: cardinal): integer;


const
ArrayCP1250ToUTF8: TCharToUTF8Table = (
 #0,                 // #0
 #1,                 // #1
 #2,                 // #2
 #3,                 // #3
 #4,                 // #4
 #5,                 // #5
 #6,                 // #6
 #7,                 // #7
 #8,                 // #8
 #9,                 // #9
 #10,                // #10
 #11,                // #11
 #12,                // #12
 #13,                // #13
 #14,                // #14
 #15,                // #15
 #16,                // #16
 #17,                // #17
 #18,                // #18
 #19,                // #19
 #20,                // #20
 #21,                // #21
 #22,                // #22
 #23,                // #23
 #24,                // #24
 #25,                // #25
 #26,                // #26
 #27,                // #27
 #28,                // #28
 #29,                // #29
 #30,                // #30
 #31,                // #31
 ' ',                // ' '
 '!',                // '!'
 '"',                // '"'
 '#',                // '#'
 '$',                // '$'
 '%',                // '%'
 '&',                // '&'
 '''',               // ''''
 '(',                // '('
 ')',                // ')'
 '*',                // '*'
 '+',                // '+'
 ',',                // ','
 '-',                // '-'
 '.',                // '.'
 '/',                // '/'
 '0',                // '0'
 '1',                // '1'
 '2',                // '2'
 '3',                // '3'
 '4',                // '4'
 '5',                // '5'
 '6',                // '6'
 '7',                // '7'
 '8',                // '8'
 '9',                // '9'
 ':',                // ':'
 ';',                // ';'
 '<',                // '<'
 '=',                // '='
 '>',                // '>'
 '?',                // '?'
 '@',                // '@'
 'A',                // 'A'
 'B',                // 'B'
 'C',                // 'C'
 'D',                // 'D'
 'E',                // 'E'
 'F',                // 'F'
 'G',                // 'G'
 'H',                // 'H'
 'I',                // 'I'
 'J',                // 'J'
 'K',                // 'K'
 'L',                // 'L'
 'M',                // 'M'
 'N',                // 'N'
 'O',                // 'O'
 'P',                // 'P'
 'Q',                // 'Q'
 'R',                // 'R'
 'S',                // 'S'
 'T',                // 'T'
 'U',                // 'U'
 'V',                // 'V'
 'W',                // 'W'
 'X',                // 'X'
 'Y',                // 'Y'
 'Z',                // 'Z'
 '[',                // '['
 '\',                // '\'
 ']',                // ']'
 '^',                // '^'
 '_',                // '_'
 '`',                // '`'
 'a',                // 'a'
 'b',                // 'b'
 'c',                // 'c'
 'd',                // 'd'
 'e',                // 'e'
 'f',                // 'f'
 'g',                // 'g'
 'h',                // 'h'
 'i',                // 'i'
 'j',                // 'j'
 'k',                // 'k'
 'l',                // 'l'
 'm',                // 'm'
 'n',                // 'n'
 'o',                // 'o'
 'p',                // 'p'
 'q',                // 'q'
 'r',                // 'r'
 's',                // 's'
 't',                // 't'
 'u',                // 'u'
 'v',                // 'v'
 'w',                // 'w'
 'x',                // 'x'
 'y',                // 'y'
 'z',                // 'z'
 '{',                // '{'
 '|',                // '|'
 '}',                // '}'
 '~',                // '~'
 #127,               // #127
 #226#130#172,       // #128
 #194#129,           // #129
 #226#128#154,       // #130
 #194#131,           // #131
 #226#128#158,       // #132
 #226#128#166,       // #133
 #226#128#160,       // #134
 #226#128#161,       // #135
 #194#136,           // #136
 #226#128#176,       // #137
 #197#160,           // #138
 #226#128#185,       // #139
 #197#154,           // #140
 #197#164,           // #141
 #197#189,           // #142
 #197#185,           // #143
 #194#144,           // #144
 #226#128#152,       // #145
 #226#128#153,       // #146
 #226#128#156,       // #147
 #226#128#157,       // #148
 #226#128#162,       // #149
 #226#128#147,       // #150
 #226#128#148,       // #151
 #194#152,           // #152
 #226#132#162,       // #153
 #197#161,           // #154
 #226#128#186,       // #155
 #197#155,           // #156
 #197#165,           // #157
 #197#190,           // #158
 #197#186,           // #159
 #194#160,           // #160
 #203#135,           // #161
 #203#152,           // #162
 #197#129,           // #163
 #194#164,           // #164
 #196#132,           // #165
 #194#166,           // #166
 #194#167,           // #167
 #194#168,           // #168
 #194#169,           // #169
 #197#158,           // #170
 #194#171,           // #171
 #194#172,           // #172
 #194#173,           // #173
 #194#174,           // #174
 #197#187,           // #175
 #194#176,           // #176
 #194#177,           // #177
 #203#155,           // #178
 #197#130,           // #179
 #194#180,           // #180
 #194#181,           // #181
 #194#182,           // #182
 #194#183,           // #183
 #194#184,           // #184
 #196#133,           // #185
 #197#159,           // #186
 #194#187,           // #187
 #196#189,           // #188
 #203#157,           // #189
 #196#190,           // #190
 #197#188,           // #191
 #197#148,           // #192
 #195#129,           // #193
 #195#130,           // #194
 #196#130,           // #195
 #195#132,           // #196
 #196#185,           // #197
 #196#134,           // #198
 #195#135,           // #199
 #196#140,           // #200
 #195#137,           // #201
 #196#152,           // #202
 #195#139,           // #203
 #196#154,           // #204
 #195#141,           // #205
 #195#142,           // #206
 #196#142,           // #207
 #196#144,           // #208
 #197#131,           // #209
 #197#135,           // #210
 #195#147,           // #211
 #195#148,           // #212
 #197#144,           // #213
 #195#150,           // #214
 #195#151,           // #215
 #197#152,           // #216
 #197#174,           // #217
 #195#154,           // #218
 #197#176,           // #219
 #195#156,           // #220
 #195#157,           // #221
 #197#162,           // #222
 #195#159,           // #223
 #197#149,           // #224
 #195#161,           // #225
 #195#162,           // #226
 #196#131,           // #227
 #195#164,           // #228
 #196#186,           // #229
 #196#135,           // #230
 #195#167,           // #231
 #196#141,           // #232
 #195#169,           // #233
 #196#153,           // #234
 #195#171,           // #235
 #196#155,           // #236
 #195#173,           // #237
 #195#174,           // #238
 #196#143,           // #239
 #196#145,           // #240
 #197#132,           // #241
 #197#136,           // #242
 #195#179,           // #243
 #195#180,           // #244
 #197#145,           // #245
 #195#182,           // #246
 #195#183,           // #247
 #197#153,           // #248
 #197#175,           // #249
 #195#186,           // #250
 #197#177,           // #251
 #195#188,           // #252
 #195#189,           // #253
 #197#163,           // #254
 #203#153            // #255
);

function UnicodeToCP1250(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127,129,131,136,144,152: Result:=Unicode;
  160: Result:=160;
  164: Result:=164;
  166..169: Result:=Unicode;
  171..174: Result:=Unicode;
  176..177: Result:=Unicode;
  180..184: Result:=Unicode;
  187: Result:=187;
  193..194: Result:=Unicode;
  196: Result:=196;
  199: Result:=199;
  201: Result:=201;
  203: Result:=203;
  205..206: Result:=Unicode;
  211..212: Result:=Unicode;
  214..215: Result:=Unicode;
  218: Result:=218;
  220..221: Result:=Unicode;
  223: Result:=223;
  225..226: Result:=Unicode;
  228: Result:=228;
  231: Result:=231;
  233: Result:=233;
  235: Result:=235;
  237..238: Result:=Unicode;
  243..244: Result:=Unicode;
  246..247: Result:=Unicode;
  250: Result:=250;
  252..253: Result:=Unicode;
  258: Result:=195;
  259: Result:=227;
  260: Result:=165;
  261: Result:=185;
  262: Result:=198;
  263: Result:=230;
  268: Result:=200;
  269: Result:=232;
  270: Result:=207;
  271: Result:=239;
  272: Result:=208;
  273: Result:=240;
  280: Result:=202;
  281: Result:=234;
  282: Result:=204;
  283: Result:=236;
  313: Result:=197;
  314: Result:=229;
  317: Result:=188;
  318: Result:=190;
  321: Result:=163;
  322: Result:=179;
  323: Result:=209;
  324: Result:=241;
  327: Result:=210;
  328: Result:=242;
  336: Result:=213;
  337: Result:=245;
  340: Result:=192;
  341: Result:=224;
  344: Result:=216;
  345: Result:=248;
  346: Result:=140;
  347: Result:=156;
  350: Result:=170;
  351: Result:=186;
  352: Result:=138;
  353: Result:=154;
  354: Result:=222;
  355: Result:=254;
  356: Result:=141;
  357: Result:=157;
  366: Result:=217;
  367: Result:=249;
  368: Result:=219;
  369: Result:=251;
  377: Result:=143;
  378: Result:=159;
  379: Result:=175;
  380: Result:=191;
  381: Result:=142;
  382: Result:=158;
  711: Result:=161;
  728: Result:=162;
  729: Result:=255;
  731: Result:=178;
  733: Result:=189;
  8211..8212: Result:=Unicode-8061;
  8216..8217: Result:=Unicode-8071;
  8218: Result:=130;
  8220..8221: Result:=Unicode-8073;
  8222: Result:=132;
  8224..8225: Result:=Unicode-8090;
  8226: Result:=149;
  8230: Result:=133;
  8240: Result:=137;
  8249: Result:=139;
  8250: Result:=155;
  8364: Result:=128;
  8482: Result:=153;
  else Result:=-1;
  end;
end;


function SingleByteToUTF8(const s: string; const Table: TCharToUTF8Table): string;
var
  len: Integer;
  i: Integer;
  Src: PChar;
  Dest: PChar;
  p: PChar;
  c: Char;
begin
  if s='' then begin
    Result:='';
    exit;
  end;
  len:=length(s);
  SetLength(Result,len*4);// UTF-8 is at most 4 bytes
  Src:=PChar(s);
  Dest:=PChar(Result);
  for i:=1 to len do begin
    c:=Src^;
    inc(Src);
    if ord(c)<128 then begin
      Dest^:=c;
      inc(Dest);
    end else begin
      p:=Table[c];
      if p<>nil then begin
        while p^<>#0 do begin
          Dest^:=p^;
          inc(p);
          inc(Dest);
        end;
      end;
    end;
  end;
  SetLength(Result,{%H-}PtrUInt(Dest)-PtrUInt(Result));

end;

function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal;
begin
    if p<>nil then begin
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is a normal char, this is pascal ;)
      Result:=ord(p^);
      CharLen:=1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then begin
      // starts with %110 => could be double byte character
      if (ord(p[1]) and %11000000) = %10000000 then begin
        CharLen:=2;
        Result:=((ord(p^) and %00011111) shl 6)
                or (ord(p[1]) and %00111111);
        if Result<(1 shl 7) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and %11110000) = %11100000) then begin
      // starts with %1110 => could be triple byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then begin
        CharLen:=3;
        Result:=((ord(p^) and %00011111) shl 12)
                or ((ord(p[1]) and %00111111) shl 6)
                or (ord(p[2]) and %00111111);
        if Result<(1 shl 11) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and %11111000) = %11110000) then begin
      // starts with %11110 => could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then begin
        CharLen:=4;
        Result:=((ord(p^) and %00001111) shl 18)
                or ((ord(p[1]) and %00111111) shl 12)
                or ((ord(p[2]) and %00111111) shl 6)
                or (ord(p[3]) and %00111111);
        if Result<(1 shl 16) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else begin
      // invalid character
      Result:=ord(p^);
      CharLen:=1;
    end;
  end else begin
    Result:=0;
    CharLen:=0;
  end;
end;

function UTF8ToSingleByte(const s: string; const UTF8CharConvFunc: TUnicodeToCharID): string;
var
  len: Integer;
  Src: PChar;
  Dest: PChar;
  c: Char;
  Unicode: LongWord;
  CharLen: integer;
  i: integer;
begin
  if s='' then begin
    Result:='';
    exit;
  end;
  len:=length(s);
  SetLength(Result,len);
  Src:=PChar(s);
  Dest:=PChar(Result);
  while len>0 do begin
    c:=Src^;
    if c<#128 then begin
      Dest^:=c;
      inc(Dest);
      inc(Src);
      dec(len);
    end else begin
      Unicode:=UTF8CharacterToUnicode(Src,CharLen);
      inc(Src,CharLen);
      dec(len,CharLen);
      i:=UTF8CharConvFunc(Unicode);
      //writeln('UTF8ToSingleByte Unicode=',Unicode,' CharLen=',CharLen,' c="',copy(s,Src-PChar(s)+1-CharLen,CharLen),'" i=',i);
      if i>=0 then begin
        Dest^:=chr(i);
        inc(Dest);
      end;
    end;
  end;
  SetLength(Result,Dest-PChar(Result));

end;

procedure InternalUTF8ToCP(const s: string; TargetCodePage: TSystemCodePage;
  SetTargetCodePage: boolean;
  const UTF8CharConvFunc: TUnicodeToCharID;
  out TheResult: RawByteString); inline;
begin
  if not Assigned(UTF8CharConvFunc) then
  begin
    TheResult:=s;
    SetCodePage(TheResult, TargetCodePage, True);
    if not SetTargetCodePage then
      SetCodePage(TheResult, CP_ACP, False);
  end else begin
    TheResult:=UTF8ToSingleByte(s,UTF8CharConvFunc);
    if SetTargetCodePage then
      SetCodePage(TheResult, TargetCodePage, False);
  end;
end;

//***************************************************************

function CP1250ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1250ToUTF8);
end;

function UTF8ToCP1250(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
    // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,1250,SetTargetCodePage,@UnicodeToCP1250,Result);
end;

end.

