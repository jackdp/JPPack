unit JPP.Strings;

// To jest mój stary moduł z roku 2000 dla Borland Pascala 7.0
// W kolejnych latach rozbudowywany i dostosowywany do nowszych wersji Delphi.
{ TODO : Zrobić generalny porządek z tym bałaganem! }

interface

uses
  Windows, SysUtils,
  //Classes,
  StrUtils;

const
  CR = #13;
  CRCR = #13#13;
  LF = #10;
  LFLF = #10#10;
  CRLF = #13#10;
  TAB = #9;
  ENDL = CRLF;


function Rbs(Dir: string): string;
function Qs(const s: string): string;
function FixFileName(fName: string; s: string = '_'; ZamieniajCudzyslowy: Boolean = True): string;
function FixString(Text: string; i: integer; znak: char = ' '): string;
function PadString(Text: string; i: integer; znak: char = ' '): string;
function Pad(Text: string; i: integer; znak: char = ' '): string;
function PadRight(Text: string; i: integer; znak: char = ' '): string;
function UnquoteStr(s: string; bDoubleQuote: Boolean = True): string;
function GetLastCharIndex(s: string; c: Char): integer;
function DelFirstCharInString(s: string; ToDelete: Char): string;
function DelLastCharInString(s: string; ToDelete: Char): string;
function RemoveNum(s: string): string;
function GetLastBIndex(s: string): integer;

function RemoveSlashes(Text: string): string;
function ReplaceSpecialChars(s: string; sc: CHar = '_'): string;
function RemoveAll(Text, ToRemove: string): string;
function RemoveNonLetters(s: string): string;
function ReplaceAll(SrcStr, OldStr, NewStr: string): string;

function RemovePolishChars(s: string): string;
function IsBigLetterPL(c: Char): Boolean;
function IsSmallLetterPL(c: Char): Boolean;
function IsLetterPL(c: Char): Boolean;
function IsBigLetter(c: Char): Boolean;
function IsSmallLetter(c: Char): Boolean;
function IsLetter(c: Char): Boolean;
function IsNumber(c: Char): Boolean;

function DistinctChars(s: string; IgnoreCase: Boolean = True): Boolean;
function MakeDistinctChars(s: string): string;
function CharCount(c: Char; s: string): integer;
function ReverseStr(s: string): string;
function FillStrToLen(s: string; Len: integer; FillValue: Char = ' '): string;
function AnsiUpCase(zn: Char; Default: Char = #0): Char;
function AnsiLowCase(zn: Char; Default: Char = #0): Char;
function RemoveChars(SrcStr: string; CharsToRemove: string): string;
function LeaveChars(SrcStr: string; CharsToLeave: string): string;
function CutStrBefore(s, CutBeforeText: string; IgnoreCase: Boolean = False): string;
function CutStrAfter(s, CutAfterText: string; IgnoreCase: Boolean = False; IncludeSearchText: Boolean = True): string;
function GetFileExt(fName: string; bRemoveFirstDot: Boolean = True; Upper: Boolean = False): string;



function HtmlStringToStr(HTMLStr: string): string;
function GetHref(s: string): string;
function GetAnchorText(s: string; FixHTMLSpecialChars: Boolean = True): string;
function GetFirstDigitIndex(s: string): integer;
function GetFirstNonDigitIndex(s: string): integer;

function GetRandomHexStr(Bytes: integer = 4; ByteSeparator: string = ''; bLowerCase: Boolean = False): string;
function GetRandomIntStr(Len: integer = 10): string;
function GetFileSizeString(const FileSize: int64): string;

function FirstWord(s: string): string;
function ShuffleString(s: string): string;

function IntToStrEx(x: int64; c: Char = ' '): string;
function IntToStrEx2(x: int64; Sep: string = ' '): string;
function InsertNumSep(NumStr: string; Separator: string = ' '; NumBlockSize: integer = 3): string;
function StrRemove(s: string; StringToRemove: string): string;
function IsStringFromChars(s: string; Chars: array of Char; IgnoreCase: Boolean = False): Boolean;

function UpperAndTrim(s: string): string;
function TrimUp(s: string): string;
function LowerAndTrim(s: string): string;
function RemoveSpaces(s: string): string;
function TrimBounds(s: string; LeftBound, RightBound: string): string;
function AddBounds(const s: string; LeftBound, RightBound: Char): string; overload;
function AddBounds(const s: string; LeftBound, RightBound: string): string; overload;
function AddBounds(const s: string; StringToBoundSeparator: string = ' '; BoundChar: Char = '-'; BoundLen: Integer = 16): string; overload;

function PointToStr(Point: TPoint; Separator: string = '   '): string;
function RectToStr(Rect: TRect; ShowWidthHeight: Boolean = True; Separator: string = '   '): string;
function ReplaceDecimalSeparator(FloatStr: string; NewSeparator: string = '.'): string;
function IsAsciiString(s: string): Boolean;
function GetIntNumStr(s: string): string;
function GetFloatNumStr(s: string): string;
function FormatByteSize(const Bytes: integer): string;

function StripFileExt(const FileName: string): string;
function StartsWithNumber(s: string): Boolean;
function Unescape(s: string): string;

function TrimFromEnd(const s: string; const StringToCut: string): string;
function TrimFromStart(const s: string; const StringToCut: string): string;
function TrimExtDot(const FileExtension: string): string;
function AddFileNameSuffix(const FileName, Suffix: string): string;
function AddFileNamePrefix(const FileName, Prefix: string): string;
procedure SplitFileName(fName: string; out Dir, BaseFileName, Ext: string; bIncludePathDelimiter: Boolean = True; bRemoveDotFromExt: Boolean = False);



implementation


procedure SplitFileName(fName: string; out Dir, BaseFileName, Ext: string; bIncludePathDelimiter: Boolean = True; bRemoveDotFromExt: Boolean = False);
begin
  Dir := ExtractFileDir(fName);
  if bIncludePathDelimiter then Dir := IncludeTrailingPathDelimiter(Dir) else Dir := ExcludeTrailingPathDelimiter(Dir);

  BaseFileName := ExtractFileName(fName);
  BaseFileName := ChangeFileExt(BaseFileName, '');

  //Ext := GetFileExt(BaseFileName, bRemoveDotFromExt);
  Ext := GetFileExt(fName, bRemoveDotFromExt);
end;

function AddFileNameSuffix(const FileName, Suffix: string): string;
var
  Dir, ShortName, Ext: string;
begin
  if Suffix = '' then Exit(FileName);
//  Dir := Rbs(ExtractFileDir(FileName));
//  ShortName := ExtractFileName(FileName);
//  Ext := ExtractFileExt(ShortName);
//  ShortName := ChangeFileExt(ShortName, '');
  SplitFileName(FileName, Dir, ShortName, Ext, False, False);
  Result := Dir + PathDelim + ShortName + Suffix + Ext;
end;

function AddFileNamePrefix(const FileName, Prefix: string): string;
var
  Dir, ShortName, Ext: string;
begin
  if Prefix = '' then Exit(FileName);
  SplitFileName(FileName, Dir, ShortName, Ext, False, False);
  Result := Dir + PathDelim + Prefix + ShortName + Ext;
end;

function GetFileExt(fName: string; bRemoveFirstDot: Boolean = True; Upper: Boolean = False): string;
begin
  fName := ExtractFileExt(fName);
  if bRemoveFirstDot then
    if Copy(fName, 1, 1) = '.' then fName := Copy(fName, 2, Length(fName));
  if Upper then fName := UpperCase(fName);
  Result := fName;
end;

function TrimExtDot(const FileExtension: string): string;
begin
  Result := TrimFromStart(FileExtension, '.');
end;

function TrimFromStart(const s: string; const StringToCut: string): string;
begin
  if Copy(s, 1, Length(StringToCut)) = StringToCut then Result := Copy(s, Length(StringToCut) + 1, Length(s))
  else Result := s;
end;

function TrimFromEnd(const s: string; const StringToCut: string): string;
begin
  if Copy(s, Length(s) - Length(StringToCut) + 1, Length(StringToCut)) = StringToCut then Result := Copy(s, 1, Length(s) - Length(StringToCut))
  else Result := s;
end;

function Unescape(s: string): string;
begin
  //s := StringReplace(s, '\t', TAB, [rfReplaceAll]);
  s := StringReplace(s, '\:', ':', [rfReplaceAll]);
  s := StringReplace(s, '\''', '''', [rfReplaceAll]);
  s := StringReplace(s, '\"', '"', [rfReplaceAll]);
  s := StringReplace(s, '\?', '?', [rfReplaceAll]);

  s := StringReplace(s, '\n', CRLF, [rfReplaceAll]); // LF
  s := StringReplace(s, '\r', CRLF, [rfReplaceAll]); // CR

  s := StringReplace(s, '\\', '\', [rfReplaceAll]);

  Result := s;
end;

function StartsWithNumber(s: string): Boolean;
var
  i: integer;
  c: Char;
  sNum: string;
begin
  //Result := False;
  s := Trim(s);
  sNum := '';
  for i := 1 to Length(s) do
  begin
    c := s[i];
    if not IsNumber(c) then Break;
    sNum := sNum + c;
  end;
  Result := sNum <> '';
end;

function StripFileExt(const FileName: string): string;
begin
  Result := ChangeFileExt(FileName, '');
end;

function FormatByteSize(const Bytes: integer): string;
const
  B = 1; //byte
  KB = 1024 * B;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if Bytes > GB then Result := FormatFloat('#.## GB', Bytes / GB)
  else if Bytes > MB then Result := FormatFloat('#.## MB', Bytes / MB)
  else if Bytes > KB then Result := FormatFloat('#.## KB', Bytes / KB)
  else Result := FormatFloat('#.## bytes', Bytes) ;
end;

function GetFloatNumStr(s: string): string;
var
  i, x: integer;
  sRes: string;
  Sep: Char;
begin
  for i := 1 to Length(s) do
    if ( CharInSet(s[i], ['0'..'9']) ) or (s[i] = '.') or (s[i] = ',') then sRes := sRes + s[i];
  Sep := FormatSettings.DecimalSeparator;
  sRes := StringReplace(sRes, '.', Sep, [rfReplaceAll]);
  sRes := StringReplace(sRes, ',', Sep, [rfReplaceAll]);
  if CharCount(Sep, sRes) > 1 then
  begin
//    x := LastDelimiter(Sep, sRes);
//    for i := x - 1 downto 1 do
//     if sRes[i] = Sep then Delete(sRes, i, 1);
    x := Pos(Sep, sRes);
    for i := Length(sRes) downto x + 1 do if sRes[i] = Sep then Delete(sRes, i, 1);
  end;
  if sRes = Sep then sRes := '';
  Result := sRes;
end;

function GetIntNumStr(s: string): string;
var
  i: integer;
  sRes: string;
begin
  for i := 1 to Length(s) do
    if CharInSet(s[i], ['0'..'9']) then sRes := sRes + s[i];
  Result := sRes;
end;


function IsAsciiString(s: string): Boolean;
var
  i: integer;
begin
  Result := True;
  for i := 1 to Length(s) do
    if Ord(s[1]) < 20 then
    begin
      Result := False;
      Break;
    end;
end;


function ReplaceDecimalSeparator(FloatStr: string; NewSeparator: string = '.'): string;
begin
  Result := StringReplace(FloatStr, FormatSettings.DecimalSeparator, NewSeparator, [rfReplaceAll, rfIgnoreCase]);
end;

function RectToStr(Rect: TRect; ShowWidthHeight: Boolean = True; Separator: string = '   '): string;
begin
  Result :=
    'Left = ' + IntToStr(Rect.Left) + Separator +
    'Top = ' + IntToStr(Rect.Top) + Separator +
    'Right = ' + IntToStr(Rect.Right) + Separator +
    'Bottom = ' + IntToStr(Rect.Bottom);
  if ShowWidthHeight then Result := Result + Separator + 'Width = ' + IntToStr(Rect.Width) + Separator + 'Height = ' + IntToStr(Rect.Height);
end;

function PointToStr(Point: TPoint; Separator: string = '   '): string;
begin
  Result :=
    'X = ' + IntToStr(Point.X) + Separator +
    'Y = ' + IntToStr(Point.Y);
end;

function AddBounds(const s: string; LeftBound, RightBound: Char): string;
begin
  Result := LeftBound + s + RightBound;
end;

function AddBounds(const s: string; LeftBound, RightBound: string): string;
begin
  Result := LeftBound + s + RightBound;
end;

function AddBounds(const s: string; StringToBoundSeparator: string = ' '; BoundChar: Char = '-'; BoundLen: Integer = 16): string; overload;
var
  sb: string;
begin
  sb := StringOfChar(BoundChar, BoundLen);
  Result := sb + StringToBoundSeparator + s + StringToBoundSeparator + sb;
end;



function TrimBounds(s: string; LeftBound, RightBound: string): string;
begin
  if Copy(s, 1, Length(LeftBound)) = LeftBound then s := Copy(s, 1 + Length(LeftBound), Length(s));
  if Copy(s, Length(s) - Length(RightBound) + 1, Length(RightBound)) = RightBound then s := Copy(s, 1, Length(s) - Length(RightBound));
             //s := Copy(s, Length(s), 1);
  Result := s;
end;

function RemoveSpaces(s: string): string;
begin
  Result := StringReplace(s, ' ', '', [rfReplaceAll]);
end;

function UpperAndTrim(s: string): string;
begin
  Result := Trim(UpperCase(s));
end;

function TrimUp(s: string): string;
begin
  Result := UpperAndTrim(s);
end;

function LowerAndTrim(s: string): string;
begin
  Result := Trim(Lowercase(s));
end;

function IsStringFromChars(s: string; Chars: array of Char; IgnoreCase: Boolean = False): Boolean;
begin
  Result := False;
end;

function StrRemove(s: string; StringToRemove: string): string;
begin
  Result := StringReplace(s, StringToRemove, '', [rfReplaceAll]);
end;


// wymieszanie łańcucha znaków
function ShuffleString(s: string): string;
var
  i: integer;
  RandomIndex: integer;
  TempChar: Char;
begin
  Randomize;

  for i := 1 to Length(s) do
  begin
    RandomIndex := Random(Length(s)) + 1;
    if RandomIndex = i then Continue;

    // zamiana znaków
    TempChar := s[i];
    s[i] := s[RandomIndex];
    s[RandomIndex] := TempChar;
  end;

  Result := s;
end;

function FirstWord(s: string): string;
var
  x: integer;
begin
  s := Trim(s);
  s := StringReplace(s, ',', ' ', []);
  s := StringReplace(s, '.', ' ', []);
  s := StringReplace(s, ':', ' ', []);
  x := Pos(' ', s);
  if x > 0 then Result := Copy(s, 1, x - 1)
  else Result := s;
end;

function GetFileSizeString(const FileSize: int64): string;
var
  fs: extended;
  s: ShortString;
begin
  Result := IntToStr(FileSize);
  fs := FileSize;
  if fs < 1024 then
  begin
    str(
    fs: 2: 0, s);
    Result := string(s) + ' bytes';
  end
  else if (fs >= 1024) and (fs < (1024 * 1024)) then
  begin
    fs := fs / 1024;
    str(fs: 2: 2, s);
    Result := string(s) + ' KB';
  end
  else if (fs >= 1024 * 1024) and (fs < (1024 * 1024 * 1024)) then
  begin
    fs := (fs / 1024) / 1024;
    str(fs: 2: 2, s);
    Result := string(s) + ' MB';
  end
  else
  begin
    fs := (fs / 1024) / 1024 / 1024;
    str(fs: 2: 2, s);
    Result := string(s) + ' GB';
  end;
end;

function GetRandomHexStr(Bytes: integer = 4; ByteSeparator: string = ''; bLowerCase: Boolean = False): string;
var
  i: integer;
  bt: Byte;
begin
  Result := '';

  for i := 1 to Bytes do
  begin
    bt := Random(255);
    Result := Result + IntToHex(bt, 2);// + ByteSeparator;
    if i < Bytes then Result := Result + ByteSeparator;
  end;
  if bLowerCase then Result := LowerCase(Result);
end;

function GetRandomIntStr(Len: integer = 10): string;
const
  Nums = '0123456789';
var
  i, x: integer;
begin
  Result := '';

  for i := 1 to Len do
  begin
    x := Random(10);
    Result := Result + Nums[x + 1];
  end;
end;

function GetFirstNonDigitIndex(s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    //if not (s[i] in ['0'..'9']) then
    if not CharInSet(s[i], ['0'..'9']) then
    begin
      Result := i;
      Break;
    end;
end;

function GetFirstDigitIndex(s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    //if s[i] in ['0'..'9'] then
    if CharInSet(s[i], ['0'..'9']) then
    begin
      Result := i;
      Break;
    end;
end;

function GetHref(s: string): string;
var
  xp: integer;
begin
  xp := Pos('href="', AnsiLowercase(s));
  if xp > 0 then
  begin
    s := Copy(s, xp + Length('href="'), Length(s));
    xp := Pos('"', s);
    if xp > 0 then s := Copy(s, 1, xp - 1);
  end;
  Result := s;
end;

function GetAnchorText(s: string; FixHTMLSpecialChars: Boolean = True): string;
var
  xp: integer;
begin
  xp := Pos('<a ', AnsiLowerCase(s));
  if xp > 0 then
  begin
    s := Copy(s, xp, Length(s));
    xp := Pos('>', s);
    if xp > 0 then
    begin
      s := Copy(s, xp + 1, Length(s));
      xp := Pos('</a>', AnsiLowerCase(s));
      if xp > 0 then s := Copy(s, 1, xp - 1);
    end;
  end;
  if FixHTMLSpecialChars then s := HtmlStringToStr(s);
  Result := s;
end;

function HtmlStringToStr(HTMLStr: string): string;
begin
  HTMLStr := StringReplace(HTMLStr, '&lt;', '<', [rfReplaceAll, rfIgnorecase]);
  HTMLStr := StringReplace(HTMLStr, '&gt;', '>', [rfReplaceAll, rfIgnorecase]);
  HTMLStr := StringReplace(HTMLStr, '&amp;', '&', [rfReplaceAll, rfIgnorecase]);
  Result := HTMLStr;
end;



// obcina łańcuch s po wystąpieniu w nim tekstu CutAfterText
function CutStrAfter(s, CutAfterText: string; IgnoreCase: Boolean = False; IncludeSearchText: Boolean = True): string;
var
  xp: integer;
begin
  if IgnoreCase then xp := Pos(AnsiUpperCase(CutAfterText), AnsiUpperCase(s))
  else xp := Pos(CutAfterText, s);
  if xp > 0 then
  begin
    s := Copy(s, 1, xp - 1);
    if not IncludeSearchText then s := Copy(s, 1, Length(s) - Length(CutAfterText));
  end;
  Result := s;
end;

// obcina łańcuch s przed wystąpieniem w nim tekstu CutBeforeText
function CutStrBefore(s, CutBeforeText: string; IgnoreCase: Boolean = False): string;
var
  xp: integer;
begin
  if IgnoreCase then xp := Pos(AnsiUpperCase(CutBeforeText), AnsiUpperCase(s))
  else xp := Pos(CutBeforeText, s);
  //if xp > 0 then s := Copy(s, 1, xp - 1); - ???
  if xp > 0 then s := Copy(s, xp, Length(s));
  Result := s;
end;

function GetStrBefore(s, GetStrBeforeText: string; IgnoreCase: Boolean = False): string;
var
  xp: integer;
begin
  if IgnoreCase then xp := Pos(AnsiUpperCase(GetStrBeforeText), AnsiUpperCase(s))
  else xp := Pos(GetStrBeforeText, s);
  if xp > 0 then s := Copy(s, 1, xp - 1);
  Result := s;
end;





function LeaveChars(SrcStr: string; CharsToLeave: string): string;
var
  x, y: integer;
  bExists: Boolean;
begin
  for x := Length(SrcStr) downto 1 do
  begin
    bExists := False;
    for y := 1 to Length(CharsToLeave) do
      if SrcStr[x] = CharsToLeave[y] then
      begin
        bExists := True;
        Break;
      end;
    if not bExists then Delete(SrcStr, x, 1);
  end;

  Result := SrcStr;
end;


function RemoveChars(SrcStr: string; CharsToRemove: string): string;
var
  x, y: integer;
begin
  for x := 1 to Length(CharsToRemove) do
    for y := Length(SrcStr) downto 1 do
      if SrcStr[y] = CharsToRemove[x] then
      begin
        Delete(SrcStr, y, 1);
        Break;
      end;


  Result := SrcStr;
end;


function AnsiUpCase(zn: Char; Default: Char): Char;
var
  s: string;
begin
  s := AnsiUpperCase(zn);
  if s = '' then s := Default;
  Result := s[1];
end;


function AnsiLowCase(zn: Char; Default: Char): Char;
var
  s: string;
begin
  s := AnsiLowerCase(zn);
  if s = '' then s := Default;
  Result := s[1];
end;



function FillStrToLen(s: string; Len: integer; FillValue: Char): string;
var
  i, x: integer;
begin
  x := Length(s);
  for i := x + 1 to Len do
    s := s + FillValue;
  Result := s;
end;


function ReverseStr(s: string): string;
begin
  Result := StrUtils.ReverseString(s);
end;

function CharCount(c: Char; s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] = c then Inc(Result);
end;


function MakeDistinctChars(s: string): string;
var
  i: integer;
  sr: string;
begin
  sr := '';
  for i := Length(s) downto 1 do
  begin
    if CharCount(s[i], s) > 1 then
    begin
      Delete(s, i, 1);
      Continue;
    end;
    sr := sr + s[i];
  end;
  Result := ReverseStr(sr);
end;


function DistinctChars(s: string; IgnoreCase: Boolean): Boolean;
var
  i, x: integer;
begin
  if IgnoreCase then s := AnsiUpperCase(s);
  Result := False;
  for i := 1 to Length(s) do
    for x := i + 1 to Length(s) do
      if s[x] = s[i] then Exit;
  Result := True;
end;



function IsNumber(c: Char): Boolean;
begin
  //Result := c in ['0'..'9'];
  Result := CharInSet(c, ['0'..'9']);
end;

function IsLetterPL(c: Char): Boolean;
begin
  Result := IsSmallLetterPL(c) or IsBigLetterPL(c);
end;

function IsSmallLetterPL(c: Char): Boolean;
begin
  //Result := c in ['a'..'z', 'ą', 'ć', 'ę', 'ł', 'ó', 'ś', 'ź', 'ż'];
  //97..122 + Polish chars
  Result := CharInSet(c, ['a'..'z', 'ą', 'ć', 'ę', 'ł', 'ó', 'ś', 'ź', 'ż']);
end;

function IsBigLetterPL(c: Char): Boolean;
begin
  //Result := c in ['A'..'Z', 'Ą', 'Ć', 'Ę', 'Ł', 'Ó', 'Ś', 'Ź', 'Ż'];
  // 65..90 + Polish chars
  Result := CharInSet(c, ['A'..'Z', 'Ą', 'Ć', 'Ę', 'Ł', 'Ó', 'Ś', 'Ź', 'Ż']);
end;



function IsLetter(c: Char): Boolean;
begin
  Result := IsSmallLetter(c) or IsBigLetter(c);
end;

function IsSmallLetter(c: Char): Boolean;
begin
  //Result := c in ['a'..'z'];
  //97..122
  Result := CharInSet(c, ['a'..'z']);
end;

function IsBigLetter(c: Char): Boolean;
begin
  //Result := c in ['A'..'Z'];
  // 65..90
  Result := CharInSet(c, ['A'..'Z']);
end;

function RemoveNonLetters(s: string): string;
var
  i: integer;
  sr: string;
begin
  sr := '';
  for i := 1 to Length(s) do
    if IsLetter(s[i]) then sr := sr + s[i];
  Result := sr;
end;

function RemoveAll(Text, ToRemove: string): string;
begin
  Result := StringReplace(Text, ToRemove, '', [rfReplaceAll]);
end;

function ReplaceAll(SrcStr, OldStr, NewStr: string): string;
begin
  Result := StringReplace(SrcStr, OldStr, NewStr, [rfReplaceAll]);
end;





function ReplaceSpecialChars(s: string; sc: Char): string;
var
  i: integer;
begin
  for i := 1 to Length(s) do
    if Ord(s[i]) < 32 then s[i] := sc;
  Result := s;
end;


function RemoveSlashes(Text: string): string;
var
  s: string;
begin
  s := Text;
  s := StringReplace(s, '''''', '''', [rfReplaceAll]);
  s := StringReplace(s, '\"', '"', [rfReplaceAll]);
  s := StringReplace(s, '\\', '\', [rfReplaceAll]);
  s := StringReplace(s, '\t', TAB, [rfReplaceAll]);
  s := StringReplace(s, '\r\n', CRLF, [rfReplaceAll]);
  s := StringReplace(s, '\r', CRLF, [rfReplaceAll]);
  s := StringReplace(s, '\n', CRLF, [rfReplaceAll]);

  Result := s;
end;


function GetLastBIndex(s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
    if s[i] = '\' then
    begin
      Result := i;
      Exit;
    end;
end;


function RemoveNum(s: string): string;
var
  i, x: integer;
  c: Char;
  s2: string;
begin
  x := 0;
  for i := 1 to Length(s) do
  begin
    c := s[i];
    case c of
      '0'..'9': Continue;
    else
      x := i;
      Break;
    end;
    s2 := s2 + c;
  end;
  if x > 0 then s2 := s2 + Copy(s, x, Length(s));
  s2 := Trim(s2);
  if Copy(s2, 1, 1) = '.' then s2 := Trim(Copy(s2, 2, Length(s2)));
  if Copy(s2, 1, 1) = '-' then s2 := Trim(Copy(s2, 2, Length(s2)));
  if Copy(s2, 1, 1) = '_' then s2 := Trim(Copy(s2, 2, Length(s2)));
  Result := s2;
end;



function GetLastCharIndex(s: string; c: Char): integer;
var
  x, i: integer;
begin
  x := 0;
  for i := 1 to Length(s) do
    if s[i] = c then x := i;
  Result := x;
end;


function DelFirstCharInString(s: string; ToDelete: Char): string;
begin
  if Copy(s, 1, 1) = ToDelete then
    s := Copy(s, 2, Length(s));
  Result := s;
end;


function DelLastCharInString(s: string; ToDelete: Char): string;
begin
  if Copy(s, Length(s), 1) = ToDelete then
    s := Copy(s, 1, Length(s) - 1);
  Result := s;
end;


function RemovePolishChars(s: string): string;
var
  i: integer;
begin
  s := StringReplace(s, 'ą', 'a', [rfReplaceAll]);
  s := StringReplace(s, 'ć', 'c', [rfReplaceAll]);
  s := StringReplace(s, 'ę', 'e', [rfReplaceAll]);
  s := StringReplace(s, 'ł', 'l', [rfReplaceAll]);
  s := StringReplace(s, 'ń', 'n', [rfReplaceAll]);
  s := StringReplace(s, 'ó', 'o', [rfReplaceAll]);
  s := StringReplace(s, 'ś', 's', [rfReplaceAll]);
  s := StringReplace(s, 'ź', 'z', [rfReplaceAll]);
  s := StringReplace(s, 'ż', 'z', [rfReplaceAll]);

  s := StringReplace(s, 'Ą', 'A', [rfReplaceAll]);
  s := StringReplace(s, 'Ć', 'C', [rfReplaceAll]);
  s := StringReplace(s, 'Ę', 'E', [rfReplaceAll]);
  s := StringReplace(s, 'Ł', 'L', [rfReplaceAll]);
  s := StringReplace(s, 'Ń', 'N', [rfReplaceAll]);
  s := StringReplace(s, 'Ó', 'O', [rfReplaceAll]);
  s := StringReplace(s, 'Ś', 'S', [rfReplaceAll]);
  s := StringReplace(s, 'Ź', 'Z', [rfReplaceAll]);
  s := StringReplace(s, 'Ż', 'Z', [rfReplaceAll]);

  s := StringReplace(s, 'ę', 'e', [rfReplaceAll]);
  //s := StringReplace(s, 'A', 'a', [rfReplaceAll]);
  s := StringReplace(s, '', 'l', [rfReplaceAll]);
  s := StringReplace(s, 'ť', 'L', [rfReplaceAll]);
  s := StringReplace(s, 'ä', 'n', [rfReplaceAll]);
  s := StringReplace(s, '©', 'e', [rfReplaceAll]);
  s := StringReplace(s, 'ę', 'e', [rfReplaceAll]);

  for i := 1 to Length(s) do if not Ord(s[i]) in [65..122] then s[i] := '_';

  Result := s;
end;



function UnquoteStr(s: string; bDoubleQuote: Boolean = True): string;
var
  qc: Char;
begin
  if bDoubleQuote then qc := '"'
  else qc := '''';
  if Copy(s, 1, 1) = qc then Delete(s, 1, 1);
  if Copy(s, Length(s), 1) = qc then Delete(s, Length(s), 1);
  Result := s;
end;


function FixString(Text: string; i: integer; znak: char = ' '): string;
var
  x, y, k: integer;
  s: string;
begin
  if length(Text) < i then
  begin
    x := length(Text);
    y := i - x;
    for k := 1 to y do
      s := s + znak;
    Text := s + Text;
  end;
  Result := Text;
end;

function PadString(Text: string; i: integer; znak: char = ' '): string;
begin
  Result := FixString(Text, i, znak);
end;

function Pad(Text: string; i: integer; znak: char = ' '): string;
begin
  Result := FixString(Text, i, znak);
end;

function PadRight(Text: string; i: integer; znak: char = ' '): string;
var
  x, y, k: integer;
  s: string;
begin
  if Length(Text) < i then
  begin
    x := Length(Text);
    y := i - x;
    for k := 1 to y do
      s := s + znak;
    Text := Text + s;
  end;
  Result := Text;
end;

// Authoring a Conditional "Please Wait . . ." Message Box
function FixFileName(fName: string; s: string = '_'; ZamieniajCudzyslowy: Boolean = True): string;
begin
  if ZamieniajCudzyslowy then fName := StringReplace(fName, '"', '''', [rfReplaceAll]);
  fName := StringReplace(fName, '?', s, [rfReplaceAll]);
  fName := StringReplace(fName, '*', s, [rfReplaceAll]);
  fName := StringReplace(fName, ':', s, [rfReplaceAll]);
  fName := StringReplace(fName, '/', s, [rfReplaceAll]);
  fName := StringReplace(fName, '\', s, [rfReplaceAll]);
  fName := StringReplace(fName, '<', s, [rfReplaceAll]);
  fName := StringReplace(fName, '>', s, [rfReplaceAll]);
  fName := StringReplace(fName, '|', s, [rfReplaceAll]);

  Result := fName;
end;



function Qs(const s: string): string;
begin
  Result := '"' + s + '"';
end;

function Rbs(Dir: string): string;
begin
{  Result := Dir;
  if Dir <> '' then
    if Dir[Length(Dir)] = '\' then Result := Copy(Dir, 1, Length(Dir) - 1);   }
  Dir := ExcludeTrailingPathDelimiter(Dir);
  if Copy(Dir, Length(Dir) - 1, 2) = '\.' then Delete(Dir, Length(Dir) - 1, 2);
  Result := Dir;
end;

function IntToStrEx(x: int64; c: Char = ' '): string;
var
  s: string;
  i, k, Len: integer;
begin
  s := IntToStr(x);

  Len := Length(s);
  k := Len div 3;
  for i := 1 to k do Insert(c, s, Len - (i * 3) + 1);

  Result := Trim(s);
end;

function IntToStrEx2(x: int64; Sep: string = ' '): string;
var
  s: string;
  i, k, Len: integer;
begin
  s := IntToStr(x);

  Len := Length(s);
  k := Len div 3; // k - liczba wstawień separatora

  for i := 1 to k do Insert(Sep, s, Len - (i * 3) + 1);

  if Copy(s, 1, Length(Sep)) = Sep then Delete(s, 1, Length(Sep));
  Result := s;
end;

function InsertNumSep(NumStr: string; Separator: string = ' '; NumBlockSize: integer = 3): string;
var
  s: string;
  i, k, Len, LastDigitPos, xp: integer;
begin
  s := NumStr;
  LastDigitPos := Length(s);
  xp := Pos('.', s);
  if xp > 0 then LastDigitPos := xp - 1;

  xp := Pos(FormatSettings.DecimalSeparator, s);
  if xp > 0 then LastDigitPos := xp - 1;
  //Len := Length(s);
  Len := LastDigitPos;
  k := Len div NumBlockSize; // k - liczba wstawień separatora

  for i := 1 to k do Insert(Separator, s, Len - (i * NumBlockSize) + 1);

  if Copy(s, 1, Length(Separator)) = Separator then Delete(s, 1, Length(Separator));
  Result := s;
end;


end.

