unit JPP.MemIniFile deprecated 'Use JPL.MemIniFile instead';



{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF DCC}
  Winapi.Windows,
  System.SysUtils, System.Classes, System.IniFiles, System.ZLib, Vcl.Graphics, Vcl.Dialogs,
  {$ELSE}
  SysUtils, Classes, IniFiles, Graphics,
  {$ENDIF}
  JPL.Strings, JPL.Colors, JPL.Math, JPL.Conversion,
  JPP.Common.Procs
  ;


type


  {$region ' ---------------------- INT - TJppMemIniFile ----------------------- '}
  TJppMemIniFile = class(TMemIniFile)
  private const
    DEFAULT_SECTION = 'MAIN';
  private
    FLeftStringBound: string;
    FRightStringBound: string;
    FCurrentSection: string;
    procedure SetLeftStringBound(const Value: string);
    procedure SetRightStringBound(const Value: string);
    procedure SetCurrentSection(const Value: string);
    function GetCurrentSection: string;
    procedure CheckCurrentSection;
  public

    constructor Create(const AFileName: string); overload;
    constructor Create(const AFileName: string; const AEncoding: TEncoding); overload;


    procedure WriteColor(const Section: string; const Ident: string; const Color: TColor); overload;
    procedure WriteColor(const Ident: string; const Color: TColor); overload;
    function ReadColor(const Section: string; const Ident: string; const Default: TColor): TColor; overload;
    function ReadColor(const Ident: string; const Default: TColor): TColor; overload;


    procedure WriteHtmlColor(const Section: string; const Ident: string; const AColor: TColor); overload;
    procedure WriteHtmlColor(const Ident: string; const AColor: TColor); overload;
    function ReadHtmlColor(const Section, Ident: string; const Default: TColor): TColor; overload;
    function ReadHtmlColor(const Ident: string; const Default: TColor): TColor; overload;

    function ReadIntegerInRange(const Section, Ident: string; const Default, Min, Max: integer): integer; overload;
    function ReadIntegerInRange(const Ident: string; const Default, Min, Max: integer): integer; overload;


    procedure WriteFontStyle(const Section: string; const Ident: string; FontStyles: TFontStyles); overload;
    procedure WriteFontStyle(const Ident: string; FontStyles: TFontStyles); overload;
    function ReadFontStyle(const Section: string; const Ident: string; Default: TFontStyles): TFontStyles; overload;
    function ReadFontStyle(const Ident: string; Default: TFontStyles): TFontStyles; overload;

    procedure WriteStrings(const Section: string; Items: TStrings {$IFDEF DCC}; Compress: Boolean = False{$ENDIF});
    procedure ReadStrings(const Section: string; Items: TStrings {$IFDEF DCC}; ItemsCompressed: Boolean = False{$ENDIF});

    procedure WriteBoundString(const Section, Ident, Value: string); overload;
    procedure WriteBoundString(const Ident, Value: string); overload;
    function ReadBoundString(const Section, Ident, Default: string): string; overload;
    function ReadBoundString(const Ident, Default: string): string; overload;

    {$IFDEF DCC}
    procedure WriteInt64(const Section, Ident: string; const Value: Int64); overload;
    procedure WriteInt64(const Ident: string; const Value: Int64); overload;
    function ReadInt64(const Section, Ident: string; const Default: Int64): Int64; overload;
    function ReadInt64(const Ident: string; const Default: Int64): Int64; overload;
    {$ENDIF}

    procedure WriteDotFloat(const Section, Ident: string; const Value: Double); overload;
    procedure WriteDotFloat(const Ident: string; const Value: Double); overload;
    function ReadDotFloat(const Section, Ident: string; const Default: Double): Double; overload;
    function ReadDotFloat(const Ident: string; const Default: Double): Double; overload;



    property LeftStringBound: string read FLeftStringBound write SetLeftStringBound;
    property RightStringBound: string read FRightStringBound write SetRightStringBound;
    property CurrentSection: string read GetCurrentSection write SetCurrentSection;
  end;
  {$endregion}


implementation




{$region ' ------------------------ IMP - TJppMemIniFile --------------------------- '}

constructor TJppMemIniFile.Create(const AFileName: string);
begin
  Create(AFilename, nil);
end;

constructor TJppMemIniFile.Create(const AFileName: string; const AEncoding: TEncoding);
begin
  inherited Create(AFileName, AEncoding);
  FLeftStringBound := '[';
  FRightStringBound := ']';
  FCurrentSection := 'MAIN';
end;


procedure TJppMemIniFile.CheckCurrentSection;
begin
  if Trim(FCurrentSection) = '' then FCurrentSection := DEFAULT_SECTION;
end;

function TJppMemIniFile.GetCurrentSection: string;
begin
  CheckCurrentSection;
  Result := FCurrentSection;
end;



procedure TJppMemIniFile.WriteBoundString(const Section, Ident, Value: string);
begin
  WriteString(Section, Ident, FLeftStringBound + Value + FRightStringBound);
end;

procedure TJppMemIniFile.WriteBoundString(const Ident, Value: string);
begin
  WriteBoundString(CurrentSection, Ident, Value);
end;

function TJppMemIniFile.ReadBoundString(const Section, Ident, Default: string): string;
begin
  Result := TrimBounds(ReadString(Section, Ident, Default), FLeftStringBound, FRightStringBound);
end;

function TJppMemIniFile.ReadBoundString(const Ident, Default: string): string;
begin
  Result := ReadBoundString(CurrentSection, Ident, Default);
end;



function TJppMemIniFile.ReadColor(const Section: string; const Ident: string; const Default: TColor): TColor;
var
  sColor: string;
  xColor: integer;
begin
  sColor := ReadString(Section, Ident, ColorToString(Default));
  if not IdentToColor(sColor, xColor) then
  try
    xColor := StringToColor(sColor);
  except
    xColor := Default;
  end;

  Result := xColor;
end;

function TJppMemIniFile.ReadColor(const Ident: string; const Default: TColor): TColor;
begin
  Result := ReadColor(CurrentSection, Ident, Default);
end;


procedure TJppMemIniFile.WriteColor(const Section, Ident: string; const Color: TColor);
begin
  WriteString(Section, Ident, ColorToString(Color));
end;



procedure TJppMemIniFile.WriteColor(const Ident: string; const Color: TColor);
begin
  WriteColor(CurrentSection, Ident, Color);
end;



function TJppMemIniFile.ReadDotFloat(const Ident: string; const Default: Double): Double;
begin
  Result := ReadDotFloat(CurrentSection, Ident, Default);
end;

function TJppMemIniFile.ReadDotFloat(const Section, Ident: string; const Default: Double): Double;
var
  FloatStr: string;
begin
  FloatStr := ReadString(Section, Ident, '');
  FloatStr := StringReplace(FloatStr, '.', FormatSettings.DecimalSeparator, []);
  Result := Default;
  if FloatStr <> '' then
    if not TryStrToFloat(FloatStr, Result) then Result := Default;
//  try
//    Result := StrToFloat(FloatStr);
//  except
//    on EConvertError do
//      // Ignore EConvertError exceptions
//    else
//      raise;
//  end;
end;


procedure TJppMemIniFile.WriteDotFloat(const Section, Ident: string; const Value: Double);
var
  s: string;
begin
  s := FloatToStr(Value);
  s := StringReplace(s, FormatSettings.DecimalSeparator, '.', []);
  WriteString(Section, Ident, s);
end;

procedure TJppMemIniFile.WriteDotFloat(const Ident: string; const Value: Double);
begin
  WriteDotFloat(CurrentSection, Ident, Value);
end;


procedure TJppMemIniFile.WriteFontStyle(const Section: string; const Ident: string; FontStyles: TFontStyles);
begin
  WriteString(Section, Ident, FontStylesToStr(FontStyles));
end;

procedure TJppMemIniFile.WriteFontStyle(const Ident: string; FontStyles: TFontStyles);
begin
  WriteFontStyle(CurrentSection, Ident, FontStyles);
end;

function TJppMemIniFile.ReadFontStyle(const Section: string; const Ident: string; Default: TFontStyles): TFontStyles;
var
  s: string;
begin
  s := FontStylesToStr(Default);
  s := ReadString(Section, Ident, s);
  Result := StrToFontStyles(s);
end;

function TJppMemIniFile.ReadFontStyle(const Ident: string; Default: TFontStyles): TFontStyles;
begin
  Result := ReadFontStyle(CurrentSection, Ident, Default);
end;


procedure TJppMemIniFile.WriteHtmlColor(const Section, Ident: string; const AColor: TColor);
begin
  WriteString(Section, Ident, ColorToHtmlColorStr(AColor, '#', True));
end;

procedure TJppMemIniFile.WriteHtmlColor(const Ident: string; const AColor: TColor);
begin
  WriteHtmlColor(CurrentSection, Ident, AColor);
end;


function TJppMemIniFile.ReadHtmlColor(const Section, Ident: string; const Default: TColor): TColor;
var
  s: string;
begin
  s := ReadString(Section, Ident, '');
  if s = '' then Exit(Default);
  if not TryHtmlStrToColor(s, Result) then Result := Default;
end;

function TJppMemIniFile.ReadHtmlColor(const Ident: string; const Default: TColor): TColor;
begin
  Result := ReadHtmlColor(CurrentSection, Ident, Default);
end;

function TJppMemIniFile.ReadIntegerInRange(const Section, Ident: string; const Default, Min, Max: integer): integer;
var
  x: integer;
begin
  x := ReadInteger(Section, Ident, Default);
  Result := GetIntInRange(x, Min, Max);
end;

function TJppMemIniFile.ReadIntegerInRange(const Ident: string; const Default, Min, Max: integer): integer;
begin
  Result := ReadIntegerInRange(CurrentSection, Ident, Default, Min, Max);
end;





{$IFDEF DCC}
procedure TJppMemIniFile.WriteInt64(const Section, Ident: string; const Value: Int64);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

procedure TJppMemIniFile.WriteInt64(const Ident: string; const Value: Int64);
begin
  WriteInt64(CurrentSection, Ident, Value);
end;

function TJppMemIniFile.ReadInt64(const Section, Ident: string; const Default: Int64): Int64;
var
  s: string;
  x: Int64;
begin
  s := ReadString(Section, Ident, 'ERR');
  if TryStrToInt64(s, x) then Result := x else Result := Default;
end;

function TJppMemIniFile.ReadInt64(const Ident: string; const Default: Int64): Int64;
begin
  Result := ReadInt64(CurrentSection, Ident, Default);
end;
{$ENDIF}







procedure TJppMemIniFile.WriteStrings(const Section: string; Items: TStrings {$IFDEF DCC}; Compress: Boolean{$ENDIF});
var
  i: integer;
  {$IFDEF DCC}
  s: string;
  StringStream: TStringStream;
  MemoryStream: TMemoryStream;
  Buffer: array[0..99] of Byte; //
  k, xRead: integer;
  {$ENDIF}
begin
  {$IFDEF DCC}if not Compress then {$ENDIF}
  begin
    for i := 0 to Items.Count - 1 do
      WriteString(Section, 'Line_' + PadLeft(IntToStr(i + 1), 3, '0'), Items[i]);
  end

  {$IFDEF DCC}
  else

  // compression
  begin

    s := Items.Text;
    StringStream := TStringStream.Create(s);
    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.Size := 0;
      ZCompressStream(StringStream, MemoryStream);

      MemoryStream.Position := 0;
      for i := 0 to MemoryStream.Size div SizeOf(Buffer) do
      begin
        s := '';
        xRead := MemoryStream.Read(Buffer, SizeOf(Buffer));
        for k := 0 to xRead - 1 do s := s + IntToHex(Buffer[k], 2);
        WriteString(Section, 'Buf_' + PadLeft(IntToStr(i + 1), 3, '0'), s);
      end;

    finally
      StringStream.Free;
      MemoryStream.Free;
    end;

  end;
  {$ENDIF}


end;



procedure TJppMemIniFile.ReadStrings(const Section: string; Items: TStrings {$IFDEF DCC}; ItemsCompressed: Boolean{$ENDIF});
var
  sl: TStringList;
  i, xp: integer;
  {$IFDEF DCC}
  ss: TStringStream;
  ms: TMemoryStream;
  x: integer;
  s, Hex: string;
  xb: Byte;
  {$ENDIF}
begin
  if SectionExists(Section) then
  begin

    sl := TStringList.Create;
    try

      ReadSectionValues(Section, sl);


      {$IFDEF DCC}if not ItemsCompressed then {$ENDIF}
      begin
        for i := 0 to sl.Count - 1 do
        begin
          xp := Pos('=', sl[i]);
          if xp > 0 then sl[i] := Copy(sl[i], xp + 1, Length(sl[i]));
        end;

        Items.Assign(sl);
      end

      {$IFDEF DCC}
      else


      // decompression
      begin

        ms := TMemoryStream.Create;
        ss := TStringStream.Create;
        try

          s := '';
          for i := 0 to sl.Count - 1 do
          begin
            xp := Pos('=', sl[i]);
            if xp > 0 then s := s + Copy(sl[i], xp + 1, Length(sl[i])); // zapisanie wszystkich wartoœci do ³añcucha s
          end;

          Items.Text := '';
          if s <> '' then
          begin

            for i := 1 to (Length(s) div 2) do
            begin
              x := (i * 2) - 1;
              Hex := '$' + Copy(s, x, 2);
              try
                xb := StrToInt(Hex);
              except
                Items.Text := ''; // niew³aœciwe dane!!!
                //ShowMessage('Invalid input data!');
                Exit;
              end;
              ms.Write(xb, 1);
            end;

            ss.Size := 0;
            ms.Position := 0;
            ZDecompressStream(ms, ss);
            Items.Text := ss.DataString;

          end;

        finally
          ss.Free;
          ms.Free;
        end;

      end;
      {$ENDIF}

    finally
      sl.Free;
    end;

  end;
end;



procedure TJppMemIniFile.SetCurrentSection(const Value: string);
begin
  FCurrentSection := Value;
end;

procedure TJppMemIniFile.SetLeftStringBound(const Value: string);
begin
  if FLeftStringBound = Value then Exit;
  FLeftStringBound := Value;
end;

procedure TJppMemIniFile.SetRightStringBound(const Value: string);
begin
  if FRightStringBound = Value then Exit;
  FRightStringBound := Value;
end;

{$endregion TJppMemIniFile}

end.
