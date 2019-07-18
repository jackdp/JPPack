unit JPP.Common.Procs;

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF DCC}
  Winapi.Windows,
  System.SysUtils, System.Classes, System.UITypes,
  Vcl.Controls, Vcl.Buttons, Vcl.Graphics, Vcl.Dialogs //, JPL.Math;
  {$ELSE}
  SysUtils, Classes, Controls, Graphics, LCLType, LCLIntf //, JPL.Math;
  {$ENDIF}
  , JPP.Common;


function FontStylesToStr(FontStyles: TFontStyles): string;
function StrToFontStyles(FontStylesStr: string): TFontStyles;
function PenStyleToStr(PenStyle: TPenStyle): string;
function StrToPenStyle(PenStyleStr: string; Default: TPenStyle = psSolid): TPenStyle;
function AlignmentToStr(Alignment: TAlignment): string;
function StrToAlignment(AlignmentStr: string; Default: TAlignment = taLeftJustify): TAlignment;

procedure MakeListFromStr(LineToParse: string; var List: TStringList; Separator: string = ',');
function PadLeft(const Text: string; const PadToLen: integer; PaddingChar: Char = ' '): string;
procedure SaveStringToFile(s: string; fName: string);

procedure JppFrame3D(Canvas: TCanvas; var Rect: TRect; LeftColor, RightColor, TopColor, BottomColor: TColor; Width: Integer); overload;
procedure JppFrame3D(Canvas: TCanvas; var Rect: TRect; Color: TColor; Width: integer); overload;
procedure DrawTopBorder(Canvas: TCanvas; Rect: TRect; Pen: TPen; b3D: Boolean = True);
procedure DrawBottomBorder(Canvas: TCanvas; Rect: TRect; Pen: TPen; b3D: Boolean = True);
procedure DrawLeftBorder(Canvas: TCanvas; Rect: TRect; Pen: TPen; b3D: Boolean = True);
procedure DrawRightBorder(Canvas: TCanvas; Rect: TRect; Pen: TPen; b3D: Boolean = True);


function GetMiddlePosY(const R: TRect; const TextHeight: integer): integer;
procedure DrawRectTopBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; PenWidth: integer; PenStyle: TPenStyle = psSolid; ExtraSpace: integer = 0);
procedure DrawRectBottomBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; PenWidth: integer; PenStyle: TPenStyle = psSolid; ExtraSpace: integer = 0);
procedure DrawRectLeftBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; PenWidth: integer; PenStyle: TPenStyle = psSolid; ExtraSpace: integer = 0);
procedure DrawRectRightBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; PenWidth: integer; PenStyle: TPenStyle = psSolid; ExtraSpace: integer = 0);

procedure DrawCenteredText(Canvas: TCanvas; Rect: TRect; const Text: string; DeltaX: integer = 0; DeltaY: integer = 0);

procedure InflateRectEx(var ARect: TRect; const DeltaLeft, DeltaRight, DeltaTop, DeltaBottom: integer); overload;
procedure InflateRectEx(var ARect: TRect; const Margins: TJppMargins); overload;


implementation


procedure InflateRectEx(var ARect: TRect; const DeltaLeft, DeltaRight, DeltaTop, DeltaBottom: integer);
begin
  ARect.Inflate(DeltaLeft, DeltaTop, DeltaRight, DeltaBottom);
end;

procedure InflateRectEx(var ARect: TRect; const Margins: TJppMargins); overload;
begin
  InflateRectEx(ARect, -Margins.Left, -Margins.Right, -Margins.Top, -Margins.Bottom);
end;


function GetMiddlePosY(const R: TRect; const TextHeight: integer): integer;
begin
  Result := R.Top + (R.Height div 2) - (TextHeight div 2);
end;


{$region '              Drawing procs                 '}

procedure DrawCenteredText(Canvas: TCanvas; Rect: TRect; const Text: string; DeltaX: integer = 0; DeltaY: integer = 0);
var
  x, y: integer;
begin
  with Canvas do
  begin
    x := Rect.Left + (Rect.Width div 2) - (TextWidth(Text) div 2) + DeltaX;
    y := Rect.Top + (Rect.Height div 2) - (TextHeight(Text) div 2) + DeltaY;
    TextOut(x, y, Text);
  end;
end;


procedure DrawRectTopBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; PenWidth: integer; PenStyle: TPenStyle = psSolid; ExtraSpace: integer = 0);
var
  OldPenWidth, i: integer;
  OldColor: TColor;
  OldPenStyle: TPenStyle;
begin
  OldPenWidth := Canvas.Pen.Width;
  OldColor := Canvas.Pen.Color;
  OldPenStyle := Canvas.Pen.Style;

  with Canvas do
  begin
    Pen.Width := 1;
    Pen.Color := Color;
    Pen.Style := PenStyle;
    for i := 0 to PenWidth - 1 do
    begin
      MoveTo(Rect.Left, Rect.Top + i + ExtraSpace);
      LineTo(Rect.Right, Rect.Top + i + ExtraSpace);
    end;
  end;

  Canvas.Pen.Width := OldPenWidth;
  Canvas.Pen.Color := OldColor;
  Canvas.Pen.Style := OldPenStyle;
end;

procedure DrawRectBottomBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; PenWidth: integer; PenStyle: TPenStyle = psSolid; ExtraSpace: integer = 0);
var
  OldPenWidth, i: integer;
  OldColor: TColor;
  OldPenStyle: TPenStyle;
begin
  OldPenWidth := Canvas.Pen.Width;
  OldColor := Canvas.Pen.Color;
  OldPenStyle := Canvas.Pen.Style;

  with Canvas do
  begin
    Pen.Width := 1;
    Pen.Color := Color;
    Pen.Style := PenStyle;
    for i := 0 to PenWidth - 1 do
    begin
      MoveTo(Rect.Left, Rect.Bottom - i - ExtraSpace);
      LineTo(Rect.Right, Rect.Bottom - i - ExtraSpace);
    end;
  end;

  Canvas.Pen.Width := OldPenWidth;
  Canvas.Pen.Color := OldColor;
  Canvas.Pen.Style := OldPenStyle;
end;

procedure DrawRectLeftBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; PenWidth: integer; PenStyle: TPenStyle = psSolid; ExtraSpace: integer = 0);
var
  OldPenWidth, i: integer;
  OldColor: TColor;
  OldPenStyle: TPenStyle;
begin
  OldPenWidth := Canvas.Pen.Width;
  OldColor := Canvas.Pen.Color;
  OldPenStyle := Canvas.Pen.Style;

  with Canvas do
  begin
    Pen.Width := 1;
    Pen.Color := Color;
    Pen.Style := PenStyle;
    for i := 0 to PenWidth - 1 do
    begin
      MoveTo(Rect.Left + i + ExtraSpace, Rect.Top);
      LineTo(Rect.Left + i + ExtraSpace, Rect.Bottom);
    end;
  end;

  Canvas.Pen.Width := OldPenWidth;
  Canvas.Pen.Color := OldColor;
  Canvas.Pen.Style := OldPenStyle;
end;

procedure DrawRectRightBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; PenWidth: integer; PenStyle: TPenStyle = psSolid; ExtraSpace: integer = 0);
var
  OldPenWidth, i: integer;
  OldColor: TColor;
  OldPenStyle: TPenStyle;
begin
  OldPenWidth := Canvas.Pen.Width;
  OldColor := Canvas.Pen.Color;
  OldPenStyle := Canvas.Pen.Style;

  with Canvas do
  begin
    Pen.Width := 1;
    Pen.Color := Color;
    Pen.Style := PenStyle;
    for i := 0 to PenWidth - 1 do
    begin
      MoveTo(Rect.Right - i - ExtraSpace, Rect.Top);
      LineTo(Rect.Right - i - ExtraSpace, Rect.Bottom);
    end;
  end;

  Canvas.Pen.Width := OldPenWidth;
  Canvas.Pen.Color := OldColor;
  Canvas.Pen.Style := OldPenStyle;
end;



procedure DrawTopBorder(Canvas: TCanvas; Rect: TRect; Pen: TPen; b3D: Boolean = True);
var
  OldWidth, Width: integer;
begin
  OldWidth := Canvas.Pen.Width;
  Canvas.Pen.Width := 1;
  Width := Pen.Width;

  Canvas.Pen.Color := Pen.Color;
  Canvas.Pen.Style := Pen.Style;
  Canvas.Pen.Mode := Pen.Mode;

  while Width > 0 do
  begin
    Dec(Width);
    Canvas.MoveTo(Rect.Left, Rect.Top);
    Canvas.LineTo(Rect.Right, Rect.Top);
    if b3D then InflateRect(Rect, -1, -1)
    else Inc(Rect.Top);
  end;

  Canvas.Pen.Width := OldWidth;
end;

procedure DrawRightBorder(Canvas: TCanvas; Rect: TRect; Pen: TPen; b3D: Boolean = True);
var
  OldWidth, Width: integer;
begin
  OldWidth := Canvas.Pen.Width;
  Canvas.Pen.Width := 1;
  Width := Pen.Width;
  Dec(Rect.Right);

  Canvas.Pen.Color := Pen.Color;
  Canvas.Pen.Style := Pen.Style;
  Canvas.Pen.Mode := Pen.Mode;

  while Width > 0 do
  begin
    Dec(Width);
    Canvas.MoveTo(Rect.Right, Rect.Top);
    Canvas.LineTo(Rect.Right, Rect.Bottom);
    if b3D then InflateRect(Rect, -1, -1)
    else Dec(Rect.Right);
  end;

  Canvas.Pen.Width := OldWidth;

end;

procedure DrawBottomBorder(Canvas: TCanvas; Rect: TRect; Pen: TPen; b3D: Boolean = True);
var
  OldWidth, Width: integer;
begin
  OldWidth := Canvas.Pen.Width;
  Canvas.Pen.Width := 1;
  Width := Pen.Width;
  Dec(Rect.Bottom);
  Dec(Rect.Left);
  Dec(Rect.Right);

  Canvas.Pen.Color := Pen.Color;
  Canvas.Pen.Style := Pen.Style;
  Canvas.Pen.Mode := Pen.Mode;

  while Width > 0 do
  begin
    Dec(Width);
    Canvas.MoveTo(Rect.Right, Rect.Bottom);
    Canvas.LineTo(Rect.Left, Rect.Bottom);
    if b3D then InflateRect(Rect, -1, -1)
    else Dec(Rect.Bottom);
  end;

  Canvas.Pen.Width := OldWidth;
end;

procedure DrawLeftBorder(Canvas: TCanvas; Rect: TRect; Pen: TPen; b3D: Boolean = True);
var
  OldWidth, Width: integer;
begin
  OldWidth := Canvas.Pen.Width;
  Canvas.Pen.Width := 1;
  Width := Pen.Width;
  Dec(Rect.Top);
  Dec(Rect.Bottom);

  Canvas.Pen.Color := Pen.Color;
  Canvas.Pen.Style := Pen.Style;
  Canvas.Pen.Mode := Pen.Mode;

  while Width > 0 do
  begin
    Dec(Width);
    Canvas.MoveTo(Rect.Left, Rect.Bottom);
    Canvas.LineTo(Rect.Left, Rect.Top);
    if b3D then InflateRect(Rect, -1, -1)
    else Inc(Rect.Left);
  end;

  Canvas.Pen.Width := OldWidth;
end;

procedure JppFrame3D(Canvas: TCanvas; var Rect: TRect; LeftColor, RightColor, TopColor, BottomColor: TColor; Width: Integer);

  procedure DoRect;
  begin
    with Canvas, Rect do
    begin
      Pen.Color := TopColor;
      MoveTo(Left, Top);
      LineTo(Right, Top);

      Pen.Color := RightColor;
      LineTo(Right, Bottom);

      Pen.Color := BottomColor;
      LineTo(Left, Bottom);

      Pen.Color := LeftColor;
      LineTo(Left, Top);
    end;
  end;

begin
  Canvas.Pen.Width := 1;

  Dec(Rect.Bottom);
  Dec(Rect.Right);

  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;

  Inc(Rect.Bottom);
  Inc(Rect.Right);
end;

procedure JppFrame3D(Canvas: TCanvas; var Rect: TRect; Color: TColor; Width: integer);
begin
  JppFrame3D(Canvas, Rect, Color, Color, Color, Color, Width);
end;
{$endregion Drawing procs}

procedure SaveStringToFile(s: string; fName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := s;
    sl.SaveToFile(fName);
  finally
    sl.Free;
  end;
end;

function PadLeft(const Text: string; const PadToLen: integer; PaddingChar: Char = ' '): string;
begin
  if Length(Text) < PadToLen then Result := StringOfChar(PaddingChar, PadToLen - Length(Text)) + Text
  else Result := Text;
end;

procedure MakeListFromStr(LineToParse: string; var List: TStringList; Separator: string = ',');
var
  xp: integer;
  s: string;
begin
  if not Assigned(List) then Exit;

  xp := Pos(Separator, LineToParse);
  while xp > 0 do
  begin
    s := Trim(Copy(LineToParse, 1, xp - 1));
    List.Add(s);
    Delete(LineToParse, 1, xp + Length(Separator) - 1);
    LineToParse := Trim(LineToParse);
    xp := Pos(Separator, LineToParse);
  end;

  if LineToParse <> '' then
  begin
    LineToParse := Trim(LineToParse);
    if LineToParse <> '' then List.Add(LineToParse);
  end;

end;

function FontStylesToStr(FontStyles: TFontStyles): string;
var
  s: string;
begin
  s := '';
  if fsBold in FontStyles then s := 'Bold';
  if fsItalic in FontStyles then s := s + ',Italic';
  if fsUnderline in FontStyles then s := s + ',Underline';
  if fsStrikeOut in FontStyles then s := s + ',StrikeOut';
  if Copy(s, 1, 1) = ',' then Delete(s, 1, 1);
  Result := s;
end;

function StrToFontStyles(FontStylesStr: string): TFontStyles;
begin
  Result := [];
  FontStylesStr := UpperCase(FontStylesStr);
  if Pos('BOLD', FontStylesStr) > 0 then Result := Result + [fsBold];
  if Pos('ITALIC', FontStylesStr) > 0 then Result := Result + [fsItalic];
  if Pos('UNDERLINE', FontStylesStr) > 0 then Result := Result + [fsUnderline];
  if Pos('STRIKEOUT', FontStylesStr) > 0 then Result := Result + [fsStrikeOut];
end;

function PenStyleToStr(PenStyle: TPenStyle): string;
begin
  case PenStyle of
    psSolid: Result := 'Solid';
    psClear: Result := 'Clear';
    psDash: Result := 'Dash';
    psDashDot: Result := 'DashDot';
    psDashDotDot: Result := 'DashDotDot';
    psDot: Result := 'Dot';
    psInsideFrame: Result := 'InsideFrame';
  else
    Result := 'Solid';
  end;
end;

function StrToPenStyle(PenStyleStr: string; Default: TPenStyle = psSolid): TPenStyle;
begin
  PenStyleStr := Trim(UpperCase(PenStyleStr));
  if PenStyleStr = 'SOLID' then Result := psSolid
  else if PenStyleStr = 'CLEAR' then Result := psClear
  else if PenStyleStr = 'DASH' then Result := psDash
  else if PenStyleStr = 'DASHDOT' then Result := psDashDot
  else if PenStyleStr = 'DASHDOTDOT' then Result := psDashDotDot
  else if PenStyleStr = 'DOT' then Result := psDot
  else if PenStyleStr = 'INSIDEFRAME' then Result := psInsideFrame
  else Result := Default;
end;

function AlignmentToStr(Alignment: TAlignment): string;
begin
  case Alignment of
    taLeftJustify: Result := 'Left';
    taRightJustify: Result := 'Right';
  else
    Result := 'Center';
  end;
end;

function StrToAlignment(AlignmentStr: string; Default: TAlignment = taLeftJustify): TAlignment;
begin
  AlignmentStr := Trim(UpperCase(AlignmentStr));
  if AlignmentStr = 'LEFT' then Result := taLeftJustify
  else if AlignmentStr = 'RIGHT' then Result := taRightJustify
  else if AlignmentStr = 'CENTER' then Result := taCenter
  else Result := Default;
end;

end.
