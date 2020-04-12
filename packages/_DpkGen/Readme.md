# Info

To execute the `generate_packages.bat` file, you need:
1. **DpkGen** from http://www.pazera-software.com/products/dpk-generator/
1. **StrRep** http://www.pazera-software.com/files/StrRep.exe (source below).

```pascal
program StrRep;

// Disable extended RTTI
{$IF CompilerVersion >= 21.0} // >= Delphi 2010
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

{$SetPEFlags 1}   // IMAGE_FILE_RELOCS_STRIPPED
{$SetPEFlags $20} // IMAGE_FILE_LARGE_ADDRESS_AWARE

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  SysUtils, StrUtils, Classes,
  JPL.Console, JPL.FileSearch // <-- https://github.com/jackdp/JPLib/tree/master/Base
  ;


var
  MyName: string;
  fName: string;
  s, s2, sOld, sNew: string;
  slFileContent, slFileNames, slFileMasks, slTemp: TStringList;
  x, i: integer;
  rf: TReplaceFlags;
  bIgnoreCase: Boolean;
  xSub: Byte;
  xReplaceCount, xModifiedFiles: integer;


function StrToBool(s: string): Boolean;
begin
  s := Trim(AnsiUpperCase(s));
  if (s = 'TRUE') or (s = 'T') or (s = 'TAK') or (s = 'YES') or (s = 'Y') or (s = '1') then Result := True
  else Result := False;
end;

procedure ShowInfo;
begin
  Writeln(MyName, ' replaces occurrences of the substring specified by OldPattern with the substring specified by NewPattern in the given file/files.');
  Writeln;
  TConsole.WriteTaggedTextLine(
    'Usage: ' + MyName +
    ' <color=LightBlue>FileNames</color> <color=LightYellow>OldPattern</color> <color=LightGreen>NewPattern</color>' +
    ' | <color=LightRed>xSub</color> | <color=LightMagenta>IgnoreCase</color> | <color=cyan>First/All</color>'
  );
  TConsole.WriteTaggedTextLine('  <color=LightBlue>FileNames</color> - Comma separated list of file names and/or file masks.');
  TConsole.WriteTaggedTextLine('  <color=LightYellow>OldPattern</color> - Old string.');
  TConsole.WriteTaggedTextLine('  <color=LightGreen>NewPattern</color> - New string.');
  TConsole.WriteTaggedTextLine('  <color=LightRed>xSub</color> - Recurse to xSub subdirectory (default: 0).');
  TConsole.WriteTaggedTextLine('  <color=LightMagenta>IgnoreCase</color> - Ignore character case: True|Yes|1 or False (default: False).');
  TConsole.WriteTaggedTextLine('  <color=cyan>First/All</color> - Replace First or All occurences (default: First).');
  Writeln;
  TConsole.WriteTaggedTextLine(
    'Example: ' + MyName +
    ' <color=LightBlue>"*.txt,file.ini,long file name.xml"</color> <color=LightYellow>"old string"</color>' +
    ' <color=LightGreen>"new string"</color> <color=LightRed>0</color> <color=LightMagenta>true</color>' +
    ' <color=cyan>all</color>'
  );
end;

begin

  MyName := ExtractFileName(ParamStr(0));

  fName := ParamStr(1);

  if ParamCount < 3 then
  begin
    ShowInfo;
    Exit;
  end;

  sOld := ParamStr(2);
  sOld := StringReplace(sOld, '&quot;', '"', [rfReplaceAll]);
  sNew := ParamStr(3);
  sNew := StringReplace(sNew, '&quot;', '"', [rfReplaceAll]);

  xSub := 0;
  if ParamCount >= 4 then
  try
    xSub := StrToInt(ParamStr(4));
  except
  end;

  TConsole.WriteTaggedTextLine('Replace: <color=yellow>' + sOld + '</color> with <color=LightGreen>' + sNew + '</color>');
  Writeln('Recurse depth (xSub): ', xSub);

   
  rf := [];

  bIgnoreCase := StrToBool(ParamStr(5));
  if bIgnoreCase then rf := rf + [rfIgnoreCase];

  s := UpperCase(ParamStr(6));
  if (s = 'ALL') then rf := rf + [rfReplaceAll];


  slFileNames := TStringList.Create;
  slFileContent := TStringList.Create;
  slFileMasks := TStringList.Create;
  slTemp := TStringList.Create;

  try


    s := ParamStr(1);
    x := Pos(',', s);
    if x > 0 then
      while x <> 0 do
      begin
        s2 := Copy(s, 1, x - 1);
        s2 := Trim(s2);
        slFileMasks.Add(s2);
        s := Trim(Copy(s, x + 1, Length(s)));
        x := Pos(',', s);
      end;

    if s <> '' then slFileMasks.Add(Trim(s));

    for i := 0 to slFileMasks.Count - 1 do
      slFileMasks[i] := ExpandFileName(slFileMasks[i]);


    for i := 0 to slFileMasks.Count - 1 do
    begin
      slTemp.Clear;
      s := slFileMasks[i];
      JPGetFileList(ExtractFileName(s), ExtractFileDir(s), slTemp, xSub, False);
      slFileNames.AddStrings(slTemp);
    end;

    slFileNames.Sort;

    xModifiedFiles := 0;
    xReplaceCount := 0;

    for x := 0 to slFileNames.Count - 1 do
    begin

      fName := slFileNames[x];
      TConsole.WriteTaggedText('Processing file: <color=LightBlue>' + fName + '</color>');

      slFileContent.LoadFromFile(fName);

      for i := 0 to slFileContent.Count - 1 do
      begin

        s := slFileContent[i];

        if bIgnoreCase then
        begin
          if not AnsiContainsText(s, sOld) then Continue;
        end
        else
        begin
          if not AnsiContainsStr(s, sOld) then Continue;
        end;

        Inc(xReplaceCount);

        s := StringReplace(s, sOld, sNew, rf);
        slFileContent[i] := s;

      end;

      if xReplaceCount > 0 then
      begin
        Inc(xModifiedFiles);
        slFileContent.SaveToFile(fName);
        TConsole.WriteTaggedTextLine(' - <color=LightMagenta>file modified!</color>');
      end
      else
        Writeln('');


    end; // for x


  finally
    slFileContent.Free;
    slFileNames.Free;
    slFileMasks.Free;
    slTemp.Free;
  end;


  Writeln('Modified files: ', xModifiedFiles);
  Writeln('Done');

end.

```