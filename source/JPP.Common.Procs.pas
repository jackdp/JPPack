unit JPP.Common.Procs;

{$I jpp.inc}
{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, Types, {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}{$ENDIF}
  Forms, Controls, Graphics, StdCtrls,
  {$IFDEF FPC}LCLType, LCLIntf,{$ENDIF}
  JPL.Strings, JPL.TStr, JPL.Rects, JPP.Common;


function FontStylesToStr(FontStyles: TFontStyles): string;
function StrToFontStyles(FontStylesStr: string): TFontStyles;
function PenStyleToStr(PenStyle: TPenStyle): string;
function StrToPenStyle(PenStyleStr: string; Default: TPenStyle = psSolid): TPenStyle;
function AlignmentToStr(Alignment: TAlignment): string;
function StrToAlignment(AlignmentStr: string; Default: TAlignment = taLeftJustify): TAlignment;

procedure MakeListFromStr(LineToParse: string; var List: TStringList; Separator: string = ',');
function PadLeft(const Text: string; const PadToLen: integer; PaddingChar: Char = ' '): string;
//procedure SaveStringToFile(const Content, FileName: string); // use SaveStringToFile from JPL.Strings (JPLib)

procedure JppFrame3D(Canvas: TCanvas; var Rect: TRect; LeftColor, RightColor, TopColor, BottomColor: TColor; Width: Integer); overload;
procedure JppFrame3D(Canvas: TCanvas; var Rect: TRect; Color: TColor; Width: integer); overload;
procedure DrawTopBorder(Canvas: TCanvas; Rect: TRect; Pen: TPen; b3D: Boolean = True);
procedure DrawBottomBorder(Canvas: TCanvas; Rect: TRect; Pen: TPen; b3D: Boolean = True);
procedure DrawLeftBorder(Canvas: TCanvas; Rect: TRect; Pen: TPen; b3D: Boolean = True);
procedure DrawRightBorder(Canvas: TCanvas; Rect: TRect; Pen: TPen; b3D: Boolean = True);
procedure DrawRectEx(const Canvas: TCanvas; const ARect: TRect; bDrawLeft, bDrawRight, bDrawTop, bDrawBottom: Boolean; Border3D: Boolean = False);

procedure DrawRectTopBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; PenWidth: integer; PenStyle: TPenStyle = psSolid; ExtraSpace: integer = 0);
procedure DrawRectBottomBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; PenWidth: integer; PenStyle: TPenStyle = psSolid; ExtraSpace: integer = 0);
procedure DrawRectLeftBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; PenWidth: integer; PenStyle: TPenStyle = psSolid; ExtraSpace: integer = 0);
procedure DrawRectRightBorder(Canvas: TCanvas; Rect: TRect; Color: TColor; PenWidth: integer; PenStyle: TPenStyle = psSolid; ExtraSpace: integer = 0);

procedure DrawCenteredText(Canvas: TCanvas; Rect: TRect; const Text: string; DeltaX: integer = 0; DeltaY: integer = 0);

function GetFontName(const FontNamesArray: array of string): string; overload;
function GetFontName(const CommaSeparatedFontNames: string): string; overload;
procedure GetControlDefaultFontParams(out FontName: string; out FontSize: integer);
procedure SetFontDefaultParams(AFont: TFont);

procedure InflateRectWithMargins(var ARect: TRect; const Margins: TJppMargins);


procedure DrawShadowText(const Canvas: TCanvas; const Text: string; ARect: TRect; const Flags: Cardinal; const NormalColor, ShadowColor: TColor;
  ShadowShiftX: ShortInt = 1; ShadowShiftY: ShortInt = 1);

function TextPosToRectPos(const Alignment: TAlignment; const Layout: TTextLayout): TRectPos;

function TrimTextToFitRect(const Canvas: TCanvas; const Text: string; const ARect: TRect; TextFlags: UINT; Delimiters: string = ' '#9; UseEllipsis: Boolean = True): string;

function DrawTextEx(DC: HDC; const Text: string; var ARect: TRect; Flags: UINT): integer; overload;
function DrawTextEx(Canvas: TCanvas; const Text: string; var ARect: TRect; Flags: UINT): integer; overload;
function DrawTextEx(Canvas: TCanvas; const Text: string; var ARect: TRect; Flags: UINT; const Layout: TTextLayout; const Alignment: TAlignment;
  const WordWrap: Boolean; const EllipsisPosition: TEllipsisPosition; const ShowAccelChar: Boolean): integer; overload;
function DrawTextEx(Canvas: TCanvas; const Text: string; var ARect: TRect; Flags: UINT; const Layout: TTextLayout; const Alignment: TAlignment;
  const WordWrap: Boolean; const EllipsisPosition: TEllipsisPosition; const ShowAccelChar: Boolean; ShadowParams: TJppShadowParamsRec): integer; overload;

function DrawCtrlTextBiDiModeFlags(AControl: TControl; Flags: Longint): Longint;



implementation


function DrawCtrlTextBiDiModeFlags(AControl: TControl; Flags: Longint): Longint;
var
  Alignment: TAlignment;
begin
  Result := Flags;
  if AControl.UseRightToLeftAlignment then
  begin
    if Flags and DT_RIGHT <> 0 then Alignment := taRightJustify
    else if Flags and DT_CENTER <> 0 then Alignment := taCenter
    else Alignment := taLeftJustify;

    case Alignment of
      taLeftJustify: Result := Result or DT_RIGHT;
      taRightJustify: Result := Result and not DT_RIGHT;
    end;
  end;

  if AControl.UseRightToLeftReading then Result := Result or DT_RTLREADING;
end;

function TrimTextToFitRect(const Canvas: TCanvas; const Text: string; const ARect: TRect; TextFlags: UINT; Delimiters: string = ' '#9; UseEllipsis: Boolean = True): string;
var
  EllipsisWidth, FontHeight, RectHeight, DelimPos: integer;
  R: TRect;
  s, TempText: string;
begin
  Result := Text;
  if Text = '' then Exit;
  EllipsisWidth := Canvas.TextWidth('...');
  FontHeight := Canvas.Font.Height;
  TextFlags := TextFlags or DT_CALCRECT;
  s := Text;
  TempText := Text;

  while True do
  begin

    R := ARect;
    if UseEllipsis then Dec(R.Right, EllipsisWidth);
    DrawTextEx(Canvas, TempText, R, TextFlags);
    RectHeight := R.Height;

    if (RectHeight > ARect.Height) and (RectHeight > FontHeight) then
    begin
      DelimPos := LastDelimiter(DELIMITERS, s);
      if DelimPos = 0 then DelimPos := Length(s);
      Dec(DelimPos);

      // ByteType:
      //   http://docwiki.embarcadero.com/Libraries/Sydney/en/System.SysUtils.ByteType
      //   https://www.freepascal.org/docs-html/rtl/sysutils/bytetype.html
      if ByteType(s, DelimPos) = mbLeadByte then Dec(DelimPos);

      s := Copy(s, 1, DelimPos);

      if UseEllipsis then TempText := s + '...'
      else TempText := s;

      if s = '' then Break;
    end
    else Break;

  end; // while

  Result := TempText;

end;

function DrawTextEx(DC: HDC; const Text: string; var ARect: TRect; Flags: UINT): integer; overload;
begin
  {$IFDEF MSWINDOWS}

    {$IFDEF FPC}
    Result := DrawText(DC, PChar(Text), -1, ARect, Flags);
    {$ELSE}
    Result := DrawTextW(DC, PChar(Text), -1, ARect, Flags);
    {$ENDIF}

  {$ELSE}
  // FPC Linux
  Result := DrawText(DC, PChar(Text), -1, ARect, Flags);
  {$ENDIF}
end;

function DrawTextEx(Canvas: TCanvas; const Text: string; var ARect: TRect; Flags: UINT): integer; overload;
begin
  Result := DrawTextEx(Canvas.Handle, Text, ARect, Flags);
end;

function DrawTextEx(Canvas: TCanvas; const Text: string; var ARect: TRect; Flags: UINT; const Layout: TTextLayout; const Alignment: TAlignment;
  const WordWrap: Boolean; const EllipsisPosition: TEllipsisPosition; const ShowAccelChar: Boolean): integer; overload;
var
  ShadowParams: TJppShadowParamsRec;
begin
  ShadowParams.Clear;
  Result := DrawTextEx(Canvas, Text, ARect, Flags, Layout, Alignment, WordWrap, EllipsisPosition, ShowAccelChar, ShadowParams);
end;

{.$DEFINE DEBUG_DRAWTEXTEX}
function DrawTextEx(Canvas: TCanvas; const Text: string; var ARect: TRect; Flags: UINT; const Layout: TTextLayout; const Alignment: TAlignment;
  const WordWrap: Boolean; const EllipsisPosition: TEllipsisPosition; const ShowAccelChar: Boolean; ShadowParams: TJppShadowParamsRec): integer; overload;
var
  RectPos: TRectPos;
  TextRect: TRect;
  bCalcOnly: Boolean;
  OldFontColor: TColor;
  DisplayText: string;
begin
  Result := 0;
  if Text = '' then Exit;

  DisplayText := Text;
  bCalcOnly := Flags and DT_CALCRECT = DT_CALCRECT;
  RectPos := TextPosToRectPos(Alignment, Layout);

  if WordWrap then Flags := Flags or DT_WORDBREAK
  else Flags := Flags or DT_SINGLELINE;

  case EllipsisPosition of
    epPathEllipsis: Flags := Flags or DT_PATH_ELLIPSIS;
    epEndEllipsis: Flags := Flags or DT_END_ELLIPSIS;
    epWordEllipsis: Flags := Flags or DT_WORD_ELLIPSIS;
  end;

  case Alignment of
    taLeftJustify: Flags := Flags or DT_LEFT;
    taCenter: Flags := Flags or DT_CENTER;
    taRightJustify: Flags := Flags or DT_RIGHT;
  end;

  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;

  TextRect := ARect;
  Result := DrawTextEx(Canvas, DisplayText, TextRect, Flags or DT_CALCRECT);

  // DrawText does not handle Ellipsis if the DT_WORDBREAK flag is set
  if WordWrap and (EllipsisPosition in [epEndEllipsis, epWordEllipsis]) then
    DisplayText := TrimTextToFitRect(Canvas, DisplayText, ARect, Flags and not DT_EXPANDTABS, ' '#9, True);

  AlignRect(ARect, TextRect, RectPos);
  if TextRect.Bottom > ARect.Bottom then TextRect.Offset(0, -(TextRect.Bottom - ARect.Bottom));
  if TextRect.Top < ARect.Top then TextRect.SetLocation(TextRect.Left, ARect.Top);
  if TextRect.Height > ARect.Height then TextRect.Height := ARect.Height;

  ARect := TextRect;
  if bCalcOnly then Exit;

  if (ShadowParams.Color <> clNone) and ( (ShadowParams.ShiftX <> 0) or (ShadowParams.ShiftY <> 0) ) then
  begin
    OffsetRect(TextRect, ShadowParams.ShiftX, ShadowParams.ShiftY);
    OldFontColor := Canvas.Font.Color;
    Canvas.Font.Color := ShadowParams.Color;
    DrawTextEx(Canvas, DisplayText, TextRect, Flags);
    Canvas.Font.Color := OldFontColor;
    OffsetRect(TextRect, -ShadowParams.ShiftX, -ShadowParams.ShiftY);
  end;

  Result := DrawTextEx(Canvas, DisplayText, TextRect, Flags);

  {$IFDEF DEBUG_DRAWTEXTEX}
  Canvas.Pen.Color := clLime;
  Canvas.Pen.Style := psDot;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(TextRect);
  {$ENDIF}
end;

function TextPosToRectPos(const Alignment: TAlignment; const Layout: TTextLayout): TRectPos;
begin
  Result := rpTopLeft;

  case Alignment of
    taLeftJustify:
        case Layout of
          tlTop: Result := rpTopLeft;
          tlCenter: Result := rpLeftCenter;
          tlBottom: Result := rpBottomLeft;
        end;
    taCenter:
      case Layout of
        tlTop: Result := rpTopCenter;
        tlCenter: Result := rpCenter;
        tlBottom: Result := rpBottomCenter;
      end;
    taRightJustify:
      case Layout of
        tlTop: Result := rpTopRight;
        tlCenter: Result := rpRightCenter;
        tlBottom: Result := rpBottomRight;
      end;
  end;
end;

procedure DrawShadowText(const Canvas: TCanvas; const Text: string; ARect: TRect; const Flags: Cardinal; const NormalColor, ShadowColor: TColor;
  ShadowShiftX: ShortInt = 1; ShadowShiftY: ShortInt = 1);
begin
  if ShadowColor <> clNone then
  begin
    OffsetRect(ARect, ShadowShiftX, ShadowShiftY);
    Canvas.Font.Color := ShadowColor;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), ARect, Flags);
    OffsetRect(ARect, -ShadowShiftX, -ShadowShiftY);
  end;

  Canvas.Font.Color := NormalColor;
  DrawText(Canvas.Handle, PChar(Text), Length(Text), ARect, Flags);
end;

function GetFontName(const FontNamesArray: array of string): string;
var
  FontName: string;
  i: integer;
begin
  for i := Low(FontNamesArray) to High(FontNamesArray) do
  begin
    FontName := FontNamesArray[i];
    if Screen.Fonts.IndexOf(FontName) >= 0 then
    begin
      Result := FontName;
      Break;
    end;
  end;
end;

function GetFontName(const CommaSeparatedFontNames: string): string;
var
  Arr: TStringDynArray;
begin
  SplitStrToArrayEx(CommaSeparatedFontNames, Arr, ',');
  Result := GetFontName(Arr);
end;

procedure GetControlDefaultFontParams(out FontName: string; out FontSize: integer);
begin
  FontName := GetFontName(['Segoe UI', 'Tahoma', 'MS Sans Serif']);
  if FontName = 'Segoe UI' then FontSize := 9 else FontSize := 8;
end;

procedure SetFontDefaultParams(AFont: TFont);
var
  FontName: string;
  FontSize: integer;
begin
  GetControlDefaultFontParams(FontName, FontSize);
  AFont.Name := FontName;
  AFont.Size := FontSize;
end;


procedure InflateRectWithMargins(var ARect: TRect; const Margins: TJppMargins);
begin
  JPL.Rects.InflateRectEx(ARect, -Margins.Left, -Margins.Right, -Margins.Top, -Margins.Bottom);
end;



{$region '              Drawing procs                 '}

procedure DrawCenteredText(Canvas: TCanvas; Rect: TRect; const Text: string; DeltaX: integer = 0; DeltaY: integer = 0);
var
  x, y: integer;
begin
  with Canvas do
  begin
    x := Rect.Left + (RectWidth(Rect) div 2) - (TextWidth(Text) div 2) + DeltaX;
    y := Rect.Top + (RectHeight(Rect) div 2) - (TextHeight(Text) div 2) + DeltaY;
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
  Width := Pen.Width;
  Canvas.Pen.Width := 1;

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
  Width := Pen.Width;
  Canvas.Pen.Width := 1;

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
  Width := Pen.Width;
  Canvas.Pen.Width := 1;

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
  Width := Pen.Width;
  Canvas.Pen.Width := 1;

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

procedure DrawRectEx(const Canvas: TCanvas; const ARect: TRect; bDrawLeft, bDrawRight, bDrawTop, bDrawBottom: Boolean; Border3D: Boolean = False);
begin
  if bDrawLeft then DrawLeftBorder(Canvas, ARect, Canvas.Pen, Border3D);
  if bDrawRight then DrawRightBorder(Canvas, ARect, Canvas.Pen, Border3D);
  if bDrawTop then DrawTopBorder(Canvas, ARect, Canvas.Pen, Border3D);
  if bDrawBottom then DrawBottomBorder(Canvas, ARect, Canvas.Pen, Border3D);
end;

procedure JppFrame3D(Canvas: TCanvas; var Rect: TRect; LeftColor, RightColor, TopColor, BottomColor: TColor; Width: Integer);

  procedure DoRect;
  begin
    with Canvas, Rect do
    begin

      if TopColor <> clNone then
      begin
        Pen.Color := TopColor;
        MoveTo(Left, Top);
        LineTo(Right, Top);
      end
      else MoveTo(Right, Top);

      if RightColor <> clNone then
      begin
        Pen.Color := RightColor;
        LineTo(Right, Bottom);
      end
      else MoveTo(Right, Bottom);

      if BottomColor <> clNone then
      begin
        Pen.Color := BottomColor;
        LineTo(Left, Bottom);
      end
      else MoveTo(Left, Bottom);

      if LeftColor <> clNone then
      begin
        Pen.Color := LeftColor;
        LineTo(Left, Top);
      end
      else MoveTo(Left, Top);

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


//procedure SaveStringToFile(const Content, FileName: string);
//var
//  sl: TStringList;
//begin
//  sl := TStringList.Create;
//  try
//    sl.Text := Content;
//    sl.SaveToFile(FileName);
//  finally
//    sl.Free;
//  end;
//end;

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
