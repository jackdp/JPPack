{ $ID: JPP.Graphics $ }
{
  ------------------------------------------------------
  Routines related to Bitmap and PNG processing and other
  ------------------------------------------------------
}
unit JPP.Graphics;


{$I jpp.inc}
{$IFDEF FPC} {$mode delphi}{$H+} {$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS} Windows, ShellAPI, {$ENDIF}
  SysUtils, Classes, Math, Graphics
  {$IFDEF DCC}
    {$IFDEF HAS_UNIT_SCOPE}, Vcl.Imaging.pngimage{$ELSE}, pngimage{$ENDIF}
  {$ENDIF}
  {$IFDEF FPC}, GraphType, LCLType, LCLIntf, FpImage, intfGraphics, EasyLazFreeType, LazFreeTypeIntfDrawer{$ENDIF}
  ;


const
  MaxPixelCount = 32767;


type

  {$IFDEF FPC} TPngImage = TPortableNetworkGraphic; {$ENDIF}

  TRGBRec = packed record
    R, G, B: BYTE;
  end;

  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..MaxPixelCount] of TRGBRec;

  PRGB32Array = ^TRGB32Array;
  TRGB32Array = packed array[0..MaxInt div SizeOf(TRGBQuad)-1] of TRGBQuad;

  TRGBLine = array[Word] of TRGBTriple;
  PRGBLine = ^TRGBLine;
  TRGBALine = array[Word] of TRGBQuad;
  PRGBALine = ^TRGBALine;

  TBrightInt = -100..100;

  TJppBlendIntensity = (biNone, biLow, biBelowNormal, biNormal, biBelowMax, biMax);
  TJppGrayscaleIntensity = (
    giNone,
    gi10Percent, gi20Percent, gi30Percent, gi40Percent, gi50Percent,
    gi60Percent, gi70Percent, gi80Percent, gi90Percent, gi100Percent
  );

  {$IFDEF MSWINDOWS}
  TPixelConv = record
  public
    class procedure Init(Canvas: TCanvas); static;
    class function ToPixelsX(const Millimeters: Single): Single; static;
    class function ToPixelsY(const Millimeters: Single): Single; static;
    class function InfoStr: string; static;
    class var PixelsPerMillimeterX: Single;
    class var PixelsPerMillimeterY: Single;
    class var HorizontalRes: integer;
    class var VerticalRes: integer;
    class var HorizontalSize: integer;
    class var VerticalSize: integer;
  end;
  {$ENDIF} // MSWINDOWS


{$region '   INT - Bitmap Procs   '}
procedure BitmapGrayscale(Bmp: TBitmap);
function FlipBitmap(Bmp: TBitmap): Boolean;
function InvertBitmap(Bmp: TBitmap): Boolean;
function MirrorBitmap(Bmp: TBitmap): Boolean;
function SetBitmapBrightness(Value: TBrightInt; Bmp: TBitmap): Boolean; overload;
function SetBitmapBrightness(Value: TBrightInt; Bmp: TBitmap; ColorUnchanged: TColor): Boolean; overload;
function SetBitmapContrast(Value: Single; Bmp: TBitmap): Boolean; overload;
function SetBitmapContrast(Value: Single; Bmp: TBitmap; ColorUnchanged: TColor): Boolean; overload;
function SetBitmapGamma(Value: Single; Bmp: TBitmap): Boolean;
function SetBitmapGammaR(Value: Single; Bmp: TBitmap): Boolean;
function SetBitmapGammaG(Value: Single; Bmp: TBitmap): Boolean;
function SetBitmapGammaB(Value: Single; Bmp: TBitmap): Boolean;
function RotateBitmap90(Bmp: TBitmap): Boolean;
function RotateBitmap180(Bmp: TBitmap): Boolean;
function RotateBitmap270(Bmp: TBitmap): Boolean;
{$endregion Bitmap procs}


{$region '   INT - PNG Procs   '}

(*
  PngMakeGrayscale
    Amount = 255 - full grayscale
    Amount = 0 - no change (no grayscale)
    Amount < 255 - partially
*)
procedure PngGrayscale(Png: TPngImage; Amount: Byte = 255);
procedure PngGrayscale2(Png: TPngImage; Amount: Byte = 255);
function PngGetCopy(SrcPng: TPngImage; var DestPng: TPngImage): Boolean;
function PngHasAlphaChannel(Png: TPngImage): Boolean;

procedure PngSetAlpha(const Png: TPngImage; const Alpha: Byte);
procedure SetPngAlpha(const Png: TPngImage; Alpha: Byte); deprecated 'Use PngSetAlpha instead';

(*
  PngSetBrightness
    Value = 0 - no change
    Value > 0 - lighter
    Value < 0 - darker
    Value = -100 - black
    Value = 100 - white
*)
function PngSetBrightness(Png: TPngImage; Value: TBrightInt): Boolean; overload;
function PngSetBrightness(Value: TBrightInt; Png: TPngImage): Boolean; overload; deprecated 'Use PngSetBrightness(Png, Value) instead';
function SetPngBrightness(Value: TBrightInt; Png: TPngImage): Boolean; deprecated 'Use PngSetBrightness(Png, Value) instead';

(*
  PngSetContrast
    Value = 0 - no change
    Value > 0 - more contrast
    Value < 0 - less contrast
  Modified Contrast procedure from janFX.pas written by Jan Verhoeven
*)
procedure PngSetContrast(Png: TPngImage; const Amount: integer);
function SetPngContrast(Value: Single; Png: TPngImage): Boolean; deprecated 'Use PngSetContrast instead';

(*
  PngSetGamma
    Value = 1 - no change
    Value > 0 - more gamma
    Value < 0 - less gammma
*)
function PngSetGamma(Png: TPngImage; Value: Single): Boolean; overload;
function PngSetGamma(Value: Single; Png: TPngImage): Boolean; overload; deprecated 'Use PngSetGamma(Png, Value) instead';
function SetPngGamma(Value: Single; Png: TPngImage): Boolean; deprecated 'Use PngSetGamma(Png, Value) instead';

{$endregion PNG procs}


{$IFDEF MSWINDOWS}
function GetIconCount(const FileName: string): Integer;
{$ENDIF}



function PixelFormatToStr(const pf: TPixelFormat): string;
procedure GrayscaleRGB(var R, G, B: Byte; Amount: Byte = 255);

procedure DrawCheckerboard(const Canvas: TCanvas; ColorSquare1, ColorSquare2: TColor; SquareSize: Byte = 16);

{$IFDEF FPC}
function GetTransparentPng(const PngWidth, PngHeight: integer): TPortableNetworkGraphic;
procedure SetTransparentPng(const Png: TPortableNetworkGraphic);
{$ENDIF}

function StrToPenStyle(const PenStyleStr: string): TPenStyle;
function PenStyleToStr(const PenStyle: TPenStyle): string;



implementation

uses
  JPP.Common.Procs;



function PenStyleToStr(const PenStyle: TPenStyle): string;
begin
  case PenStyle of
    psSolid: Result := 'Solid';
    psDash: Result := 'Dash';
    psDot: Result := 'Dot';
    psDashDot: Result := 'DashDot';
    psDashDotDot: Result := 'DashDotDot';
  else
    Result := 'Solid';
  end;
end;

function StrToPenStyle(const PenStyleStr: string): TPenStyle;
var
  s: string;
begin
  s := Trim(LowerCase(PenStyleStr));
  if s = 'solid' then Result := psSolid
  else if s = 'dash' then Result := psDash
  else if s = 'dot' then Result := psDot
  else if s = 'dashdot' then Result := psDashDot
  else if s = 'dashdotdot' then Result := psDashDotDot
  else Result := psSolid;
end;

{$IFDEF FPC}
// https://forum.lazarus.freepascal.org/index.php/topic,35424.msg234088.html#msg234088
procedure SetTransparentPng(const Png: TPortableNetworkGraphic);
var
  img: TLazIntfImage;
  drw: TIntfFreeTypeDrawer;
begin
  img := TLazIntfImage.Create(0, 0, [riqfRGB, riqfAlpha]);
  try
    img.SetSize(Png.Width, Png.Height);
    drw := TIntfFreeTypeDrawer.Create(img);
    try
      drw.FillPixels(colTransparent);
      Png.LoadFromIntfImage(img);
    finally
      drw.Free;
    end;
  finally
    img.Free;
  end;
end;

function GetTransparentPng(const PngWidth, PngHeight: integer): TPortableNetworkGraphic;
begin
  Result := TPortableNetworkGraphic.Create;
  Result.SetSize(PngWidth, PngHeight);
  SetTransparentPng(Result);
end;
{$ENDIF} // FPC

procedure DrawCheckerboard(const Canvas: TCanvas; ColorSquare1, ColorSquare2: TColor; SquareSize: Byte = 16);
var
  cl: TColor;
  xSize: SmallInt;
  x, y, xw, xh: integer;
  Bmp: TBitmap;
  R, R2: TRect;
  b, br: Boolean;
begin
  R := Canvas.ClipRect;
  if (R.Width < 2) or (R.Height < 2) then Exit;

  xSize := SquareSize;

  Bmp := TBitmap.Create;
  try

    xw := R.Width;
    xh := R.Height;
    Bmp.SetSize(xw, xh);
    br := False;

    with Bmp.Canvas do
    begin

      Pen.Style := psClear;
      Brush.Style := bsSolid;

      for x := 0 to (xw div xSize) do
      begin

        br := not br;
        b := br;


        for y := 0 to (xh div xSize) do
        begin
          b := not b;
          if b then cl := ColorSquare1 else cl := ColorSquare2;
          Brush.Color := cl;

          R2.Left := x * xSize;
          R2.Top := y * xSize;
          R2.Width := xSize;
          R2.Height := xSize;

          FillRect(R2);
        end;

      end; // for x

    end; // with

    Canvas.Draw(R.Left, R.Top, Bmp);

  finally
    Bmp.Free;
  end;
end;

// from PngComponents - PngFunctions.pas
procedure GrayscaleRGB(var R, G, B: Byte; Amount: Byte = 255);
{ Performance optimized version without floating point operations by Christian Budde }
var
  X: Byte;
begin
  X := (R * 77 + G * 150 + B * 29) shr 8;
  R := ((R * (255 - Amount)) + (X * Amount) + 128) shr 8;
  G := ((G * (255 - Amount)) + (X * Amount) + 128) shr 8;
  B := ((B * (255 - Amount)) + (X * Amount) + 128) shr 8;
  (* original code
  X := Round(R * 0.30 + G * 0.59 + B * 0.11);
  R := Round(R / 256 * (256 - Amount - 1)) + Round(X / 256 * (Amount + 1));
  G := Round(G / 256 * (256 - Amount - 1)) + Round(X / 256 * (Amount + 1));
  B := Round(B / 256 * (256 - Amount - 1)) + Round(X / 256 * (Amount + 1));
  *)
end;

function PixelFormatToStr(const pf: TPixelFormat): string;
begin
  case pf of
    pfDevice: Result := 'Device';
    pf1bit: Result := '1-bit';
    pf4bit: Result := '4-bit';
    pf8bit: Result := '8-bit';
    pf15bit: Result := '15-bit';
    pf16bit: Result := '16-bit';
    pf24bit: Result := '24-bit';
    pf32bit: Result := '32-bit';
  else
    Result := 'Custom';
  end;
end;

{$IFDEF MSWINDOWS}
function GetIconCount(const FileName: string): Integer;
begin
  Result := ExtractIcon(hInstance, PChar(FileName), DWORD(-1));
end;
{$ENDIF}

{$region '            PNG Procs                '}

{$IFDEF DCC}
procedure PngGrayscale(Png: TPngImage; Amount: Byte = 255);
var
  X, Y: Integer;
  Line: PRGBLine;
  //LineA: PRGBALine;
begin
  if Amount = 0 then Exit;

//  if PngHasAlphaChannel(Png) then
//
//    for Y := 0 to Png.Height - 1 do
//    begin
//      LineA := Png.{%H-}Scanline[Y];
//      for X := 0 to Png.Width - 1 do
//        GrayscaleRGB(LineA^[X].rgbRed, LineA^[X].rgbGreen, LineA^[X].rgbBlue, Amount);
//    end
//
//  else

    for Y := 0 to Png.Height - 1 do
    begin
      Line := Png.{%H-}Scanline[Y];
      for X := 0 to Png.Width - 1 do
        GrayscaleRGB(Line^[X].rgbtRed, Line^[X].rgbtGreen, Line^[X].rgbtBlue, Amount);
    end;
end;
{$ENDIF}

{$IFDEF FPC}
procedure PngGrayscale(Png: TPngImage; Amount: Byte = 255);
var
  X, Y: Integer;
  Line: PRGBLine;
  LineA: PRGBALine;
begin
  if Amount = 0 then Exit;

  if PngHasAlphaChannel(Png) then

    for Y := 0 to Png.Height - 1 do
    begin
      LineA := Png.{%H-}Scanline[Y];
      for X := 0 to Png.Width - 1 do
        GrayscaleRGB(LineA^[X].rgbRed, LineA^[X].rgbGreen, LineA^[X].rgbBlue, Amount);
    end

  else

    for Y := 0 to Png.Height - 1 do
    begin
      Line := Png.{%H-}Scanline[Y];
      for X := 0 to Png.Width - 1 do
        GrayscaleRGB(Line^[X].rgbtRed, Line^[X].rgbtGreen, Line^[X].rgbtBlue, Amount);
    end;
end;
{$ENDIF}

procedure PngGrayscale2(Png: TPngImage; Amount: Byte = 255);
var
  X, Y: Integer;
  Line: PRGBLine;
  LineA: PRGBALine;
  PngTemp: TPngImage;
begin
  if Amount = 0 then Exit;
  { TODO : FPC: Why this don't work without temporary PNG hack? }
  PngTemp := TPngImage.Create;
  try

    if not PngGetCopy(Png, PngTemp) then Exit;

    if PngHasAlphaChannel(PngTemp) then

      for Y := 0 to PngTemp.Height - 1 do
      begin
        LineA := PngTemp.{%H-}Scanline[Y];
        for X := 0 to PngTemp.Width - 1 do
          GrayscaleRGB(LineA^[X].rgbRed, LineA^[X].rgbGreen, LineA^[X].rgbBlue, Amount);
      end

    else

      for Y := 0 to PngTemp.Height - 1 do
      begin
        Line := PngTemp.{%H-}Scanline[Y];
        for X := 0 to PngTemp.Width - 1 do
          GrayscaleRGB(Line^[X].rgbtRed, Line^[X].rgbtGreen, Line^[X].rgbtBlue, Amount);
      end;

    Png.Assign(PngTemp);

  finally
    PngTemp.Free;
  end;
end;

function PngGetCopy(SrcPng: TPngImage; var DestPng: TPngImage): Boolean;
var
  ms: TMemoryStream;
begin
  Result := False;
  if not Assigned(SrcPng) then Exit;
  if SrcPng.Empty then Exit;
  if (SrcPng.Width <= 0) or (SrcPng.Height <= 0) then Exit;

  ms := TMemoryStream.Create;
  try
    SrcPng.SaveToStream(ms);
    ms.Position := 0;
    //DestPng.SetSize(SrcPng.Width, SrcPng.Height);
    DestPng.LoadFromStream(ms);
  finally
    ms.Free;
  end;
  Result := True;
end;

function PngHasAlphaChannel(Png: TPngImage): Boolean;
begin
  {$IFDEF FPC}
  Result := Png.RawImage.Description.Format = GraphType.ricfRGBA;
  {$ELSE}
  Result := Png.Header.ColorType in [COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA];
  {$ENDIF}
end;


{$IFDEF DCC}

procedure PngSetAlpha(const Png: TPngImage; const Alpha: Byte);
var
  LineA: pByteArray;
  x, y : Integer;
begin
  if Alpha = 255 then Png.RemoveTransparency
  else
  begin
    Png.CreateAlpha;
    for y := 0 to Png.Height - 1 do
    begin
      LineA := Png.AlphaScanline[y];
      for x := 0 to Png.Width - 1 do LineA[x] := Alpha;
    end;
  end;

  Png.Modified := True;
end;

procedure SetPngAlpha(const Png: TPngImage; Alpha: Byte); // deprecated
begin
  PngSetAlpha(Png, Alpha);
end;

{$ELSE}
procedure PngSetAlpha(const Png: TPngImage; const Alpha: Byte);
var
  LineA: PRGB32Array;
  i, j: integer;
  A: Byte;
begin
  if not PngHasAlphaChannel(Png) then Exit;

  for i := 0 to Png.Height - 1 do
  begin
    LineA := Png.{%H-}ScanLine[i];

    for j := 0 to Png.Width - 1 do
    begin
      A := LineA^[j].rgbReserved;
      if A > Alpha then A := Alpha;
      LineA^[j].rgbReserved := A;
    end;
  end;
end;

procedure SetPngAlpha(const Png: TPngImage; Alpha: Byte); // deprecated
begin
  PngSetAlpha(Png, Alpha);
end;
{$ENDIF}


  {$region ' -------------- PngSetContrast --------------- '}
procedure PngSetContrast(Png: TPngImage; const Amount: integer);
var
  Line: PByteArray;
  rg, gg, bg, r, g, b, x, y, Alpha: integer;

  function IntToByte(const i: integer): Byte;
  begin
    if i > 255 then Result := 255
    else if i < 0 then Result := 0
    else Result := Byte(i);
  end;

begin
  if Amount = 0 then Exit; // no change

  if PngHasAlphaChannel(Png) then

    for y := 0 to Png.Height - 1 do
    begin
      Line := Png.{%H-}ScanLine[y];
      for x := 0 to Png.Width - 1 do
      begin
        r := Line^[x * 4];
        g := Line^[x * 4 + 1];
        b := Line^[x * 4 + 2];
        Alpha := Line^[x * 4 + 3];

        rg := (Abs(127 - r) * Amount) div 255;
        gg := (Abs(127 - g) * Amount) div 255;
        bg := (Abs(127 - b) * Amount) div 255;

        if r > 127 then r := r + rg
        else r := r - rg;

        if g > 127 then g := g + gg
        else g := g - gg;

        if b > 127 then b := b + bg
        else b := b - bg;

        Line^[x * 4] := IntToByte(r);
        Line^[x * 4 + 1] := IntToByte(g);
        Line^[x * 4 + 2] := IntToByte(b);
        Line^[x * 4 + 3] := Alpha;
      end;
    end

  else

    for y := 0 to Png.Height - 1 do
    begin
      Line := Png.{%H-}ScanLine[y];
      for x := 0 to Png.Width - 1 do
      begin
        r := Line^[x * 3];
        g := Line^[x * 3 + 1];
        b := Line^[x * 3 + 2];

        rg := (Abs(127 - r) * Amount) div 255;
        gg := (Abs(127 - g) * Amount) div 255;
        bg := (Abs(127 - b) * Amount) div 255;

        if r > 127 then r := r + rg
        else r := r - rg;

        if g > 127 then g := g + gg
        else g := g - gg;

        if b > 127 then b := b + bg
        else b := b - bg;

        Line^[x * 3] := IntToByte(r);
        Line^[x * 3 + 1] := IntToByte(g);
        Line^[x * 3 + 2] := IntToByte(b);
      end;
    end;
end;


function SetPngContrast(Value: Single; Png: TPngImage): Boolean; // deprecated!
var
  LUT: array[0..255] of double;
  i, j, RValue, GValue, BValue: Integer;
  R, G, B: array[0..255] of double;
  Color: TColor;
  p: PRGBArray;
begin
  Result := False;
  if Png.Empty then Exit;

  //Bmp.PixelFormat := pf24Bit;
  if Value < 0.05 then Value := 0.05;

  for i := 0 to 255 do
    if (Value * i) > 255 then LUT[i] := 255
    else LUT[i] := Value * i;

  FillChar(R, SizeOf(R), 0);
  FillChar(G, SizeOf(G), 0);
  FillChar(B, SizeOf(B), 0);

  for i := 0 to Png.Height - 1 do
  begin
    p := Png.{%H-}{%H-}ScanLine[i];

    for j := 0 to Png.Width - 1 do
    begin
      RValue := p^[j].R;
      GValue := p^[j].G;
      BValue := p^[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      p^[j].R := Color and $000000FF;
      p^[j].G := (Color and $0000FF00) shr 8;
      p^[j].B := (Color and $00FF0000) shr 16;
    end;

  end;

  Result := True;
end;
  {$endregion PngSetContrast}


  {$region ' ----------------- PngSetBrightness -------------- '}
function PngSetBrightness(Png: TPngImage; Value: TBrightInt): Boolean; overload;
var
  i, j, Val: integer;
  Line: PRGBArray;
  LineA: PRGB32Array;
begin
  Result := False;
  if Value = 0 then Exit;
  if Png.Empty then Exit;

  Val := Value * 255 div 100;

  if PngHasAlphaChannel(Png) then
  begin

    for i := 0 to Png.Height - 1 do
    begin
      LineA := Png.{%H-}ScanLine[i];

      for j := 0 to Png.Width - 1 do
        with LineA^[j] do
          if Val > 0 then
          begin
            if rgbBlue + Val > 255 then rgbBlue := 255 else rgbBlue := rgbBlue + Val;
            if rgbGreen + Val > 255 then rgbGreen := 255 else rgbGreen := rgbGreen + Val;
            if rgbRed + Val > 255 then rgbRed := 255 else rgbRed := rgbRed + Val;
          end
          else
          begin
            if rgbBlue + Val < 0 then rgbBlue := 0 else rgbBlue := rgbBlue + Val;
            if rgbGreen + Val < 0 then rgbGreen := 0 else rgbGreen := rgbGreen + Val;
            if rgbRed + Val < 0 then rgbRed := 0 else rgbRed := rgbRed + Val;
          end;

    end; // for i

  end

  else

  begin

    for i := 0 to Png.Height - 1 do
    begin
      Line := Png.{%H-}ScanLine[i];

      for j := 0 to Png.Width - 1 do
        with Line^[j] do
          if Val > 0 then
          begin
            if B + Val > 255 then B := 255 else B := B + Val;
            if G + Val > 255 then G := 255 else G := G + Val;
            if R + Val > 255 then R := 255 else R := R + Val;
          end
          else
          begin
            if B + Val < 0 then B := 0 else B := B + Val;
            if G + Val < 0 then G := 0 else G := G + Val;
            if R + Val < 0 then R := 0 else R := R + Val;
          end;

    end; // for i

  end;

  Result := True;
end;

function PngSetBrightness(Value: TBrightInt; Png: TPngImage): Boolean; overload;
begin
  Result := PngSetBrightness(Png, Value);
end;

function SetPngBrightness(Value: TBrightInt; Png: TPngImage): Boolean;
begin
  Result := PngSetBrightness(Png, Value);
end;
  {$endregion PngSetBrightness}


  {$region ' -------------------- PngSetGamma ----------------- '}
function PngSetGamma(Png: TPngImage; Value: Single): Boolean; overload;
var
  i, j, RValue, GValue, BValue: integer;
  R, G, B: array[0..255] of Double;
  Color: TColor;
  LUT: array[0..255] of Double;
  Line: PRGBArray;
  {$IFDEF FPC}LineA: PRGB32Array;{$ENDIF}
begin
  Result := False;
  if Value = 1 then Exit;
  if Png.Empty then Exit;

  if Value < 0.1 then Value := 0.1;

  for i := 0 to 255 do
    if (255 * Power(i / 255, 1 / Value)) > 255 then LUT[i] := 255
    else LUT[i] := 255 * Power(i / 255, 1 / Value);

  FillChar(R, SizeOf(R), 0);
  FillChar(G, SizeOf(G), 0);
  FillChar(B, SizeOf(B), 0);

  {$IFDEF FPC}
  if PngHasAlphaChannel(Png) then

    for i := 0 to Png.Height - 1 do
    begin
      LineA := Png.{%H-}ScanLine[i];

      for j := 0 to Png.Width - 1 do
      begin
        RValue := LineA^[j].rgbRed;
        GValue := LineA^[j].rgbGreen;
        BValue := LineA^[j].rgbBlue;

        Color :=
          Round(LUT[RValue]) +
          (Round(LUT[GValue]) shl 8) +
          (Round(LUT[BValue]) shl 16);

        LineA^[j].rgbRed := Color and $000000FF;
        LineA^[j].rgbGreen := (Color and $0000FF00) shr 8;
        LineA^[j].rgbBlue := (Color and $00FF0000) shr 16;
      end;
    end

  else
  {$ENDIF}
    for i := 0 to Png.Height - 1 do
    begin
      Line := Png.{%H-}ScanLine[i];

      for j := 0 to Png.Width - 1 do
      begin
        RValue := Line^[j].R;
        GValue := Line^[j].G;
        BValue := Line^[j].B;

        Color :=
          Round(LUT[RValue]) +
          (Round(LUT[GValue]) shl 8) +
          (Round(LUT[BValue]) shl 16);

        Line^[j].R := Color and $000000FF;
        Line^[j].G := (Color and $0000FF00) shr 8;
        Line^[j].B := (Color and $00FF0000) shr 16;
      end;
    end;

  Result := True;
end;

function PngSetGamma(Value: Single; Png: TPngImage): Boolean; overload;
begin
  Result := PngSetGamma(Png, Value);
end;

function SetPngGamma(Value: Single; Png: TPngImage): Boolean;
begin
  Result := PngSetGamma(Png, Value);
end;
  {$endregion PngSetGamma}

{$endregion PNG Procs}

{$region '             Bitmap Procs                  '}
procedure BitmapGrayscale(Bmp: TBitmap);
var
  x, y: integer;
  Line: PRGB32Array;
  btGray: Byte;
begin
  // https://www.johndcook.com/blog/2009/08/24/algorithms-convert-color-grayscale/
  // GIMP grayscale algorithms:
  //   luminosity: is 0.21 R + 0.72 G + 0.07 B
  //   average: (R + G + B) / 3
  //   lightness: (max(R, G, B) + min(R, G, B)) / 2
  for x := 0 to Bmp.Height - 1 do
  begin
    Line := Bmp.{%H-}ScanLine[x];
    for y := 0 to Bmp.Width - 1 do
      with Line^[y] do
      begin
        btGray := (rgbBlue + rgbGreen + rgbRed) div 3;
        rgbBlue := btGray;
        rgbGreen := btGray;
        rgbRed := btGray;
      end;
  end;
end;

function RotateBitmap180(Bmp: TBitmap): Boolean;
begin
  Result := False;
  if Bmp.Empty then Exit;
  MirrorBitmap(Bmp);
  FlipBitmap(Bmp);
  Result := True;
end;

function RotateBitmap270(Bmp: TBitmap): Boolean;
var
  p: PRGBArray;
  i, j: integer;
  Bmp2: TBitmap;
  Color: TColor;
  k: integer;
begin
  Result := False;
  if Bmp.Empty then Exit;

  Bmp.PixelFormat := pf24Bit;

  Bmp2 := TBitmap.Create;
  try

    Bmp2.Width := Bmp.Height;
    Bmp2.Height := Bmp.Width;
    Bmp2.PixelFormat := pf24Bit;

    for i := 0 to Bmp.Height - 1 do
    begin
      p := Bmp.{%H-}ScanLine[i];

      for j := 0 to Bmp.Width - 1 do
      begin
        Color := p^[j].B + (p^[j].G shl 8) + (p^[j].R shl 16);
        k := Bmp2.Height - j;
        Bmp2.Canvas.Pixels[i, k] := Color;
      end;
    end;

    Bmp.Assign(Bmp2);

  finally
    Bmp2.Free;
  end;

  Result := True;
end;

function RotateBitmap90(Bmp: TBitmap): Boolean;
var
  p: PRGBArray;
  i, j: integer;
  Bmp2: TBitmap;
  Color: TColor;
  w, k: integer;
begin
  Result := False;
  if Bmp.Empty then Exit;

  Bmp.PixelFormat := pf24Bit;

  Bmp2 := TBitmap.Create;
  try
    Bmp2.Width := Bmp.Height;
    Bmp2.Height := Bmp.Width;
    Bmp2.PixelFormat := pf24Bit;

    for i := 0 to Bmp.Height - 1 do
    begin
      p := Bmp.{%H-}ScanLine[i];

      for j := 0 to Bmp.Width - 1 do
      begin
        Color := p^[j].B + (p^[j].G shl 8) + (p^[j].R shl 16);
        k := j;
        w := Bmp2.Width - i;
        Bmp2.Canvas.Pixels[w, k] := Color;
      end;
    end;

    Bmp.Assign(Bmp2);

  finally
    Bmp2.Free;
  end;

  Result := True;
end;

function SetBitmapGammaB(Value: Single; Bmp: TBitmap): Boolean;
var
  i, j, RValue, GValue, BValue: integer;
  R, G, B: array[0..255] of double;
  Color: TColor;
  LUT: array[0..255] of double;
  p: PRGBArray;
begin
  Result := False;
  if Bmp.Empty then Exit;

  Bmp.PixelFormat := pf24Bit;
  if Value < 0.1 then Value := 0.1;

  for i := 0 to 255 do
    if (255 * Power(i / 255, 1 / Value)) > 255 then LUT[i] := 255
    else LUT[i] := 255 * Power(i / 255, 1 / Value);

  FillChar(R, SizeOf(R), 0);
  FillChar(G, SizeOf(G), 0);
  FillChar(B, SizeOf(B), 0);

  for i := 0 to Bmp.Height - 1 do
  begin
    p := Bmp.{%H-}ScanLine[i];

    for j := 0 to Bmp.Width - 1 do
    begin
      RValue := p^[j].R;
      GValue := p^[j].G;
      BValue := p^[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      p^[j].R := Color and $000000FF;
      //p[j].G := (Color and $0000FF00) shr 8;
      //p[j].B := (Color and $00FF0000) shr 16;
    end;
  end;

  Result := True;
end;

function SetBitmapGammaG(Value: Single; Bmp: TBitmap): Boolean;
var
  i, j, RValue, GValue, BValue: integer;
  R, G, B: array[0..255] of double;
  Color: TColor;
  LUT: array[0..255] of double;
  p: PRGBArray;
begin
  Result := False;
  if Bmp.Empty then Exit;

  Bmp.PixelFormat := pf24Bit;
  if Value < 0.1 then Value := 0.1;

  for i := 0 to 255 do
    if (255 * Power(i / 255, 1 / Value)) > 255 then LUT[i] := 255
    else LUT[i] := 255 * Power(i / 255, 1 / Value);

  FillChar(R, SizeOf(R), 0);
  FillChar(G, SizeOf(G), 0);
  FillChar(B, SizeOf(B), 0);

  for i := 0 to Bmp.Height - 1 do
  begin
    p := Bmp.{%H-}ScanLine[i];

    for j := 0 to Bmp.Width - 1 do
    begin
      RValue := p^[j].R;
      GValue := p^[j].G;
      BValue := p^[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      //p[j].R := Color and $000000FF;
      p^[j].G := (Color and $0000FF00) shr 8;
      //p[j].B := (Color and $00FF0000) shr 16;
    end;
  end;

  Result := True;
end;

function SetBitmapGammaR(Value: Single; Bmp: TBitmap): Boolean;
var
  i, j, RValue, GValue, BValue: integer;
  R, G, B: array[0..255] of double;
  Color: TColor;
  LUT: array[0..255] of double;
  p: PRGBArray;
begin
  Result := False;
  if Bmp.Empty then Exit;

  Bmp.PixelFormat := pf24Bit;
  if Value < 0.1 then Value := 0.1;

  for i := 0 to 255 do
    if (255 * Power(i / 255, 1 / Value)) > 255 then LUT[i] := 255
    else LUT[i] := 255 * Power(i / 255, 1 / Value);

  FillChar(R, SizeOf(R), 0);
  FillChar(G, SizeOf(G), 0);
  FillChar(B, SizeOf(B), 0);

  for i := 0 to Bmp.Height - 1 do
  begin
    p := Bmp.{%H-}ScanLine[i];

    for j := 0 to Bmp.Width - 1 do
    begin
      RValue := p^[j].R;
      GValue := p^[j].G;
      BValue := p^[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      //p[j].R := Color and $000000FF;
      //p[j].G := (Color and $0000FF00) shr 8;
      p^[j].B := (Color and $00FF0000) shr 16;
    end;
  end;

  Result := True;
end;

function SetBitmapGamma(Value: Single; Bmp: TBitmap): Boolean;
var
  i, j, RValue, GValue, BValue: integer;
  R, G, B: array[0..255] of double;
  Color: TColor;
  LUT: array[0..255] of double;
  p: PRGBArray;
begin
  Result := False;
  if Bmp.Empty then Exit;

  Bmp.PixelFormat := pf24Bit;
  if Value < 0.1 then Value := 0.1;

  for i := 0 to 255 do
    if (255 * Power(i / 255, 1 / Value)) > 255 then LUT[i] := 255
    else LUT[i] := 255 * Power(i / 255, 1 / Value);

  FillChar(R, SizeOf(R), 0);
  FillChar(G, SizeOf(G), 0);
  FillChar(B, SizeOf(B), 0);

  for i := 0 to Bmp.Height - 1 do
  begin
    p := Bmp.{%H-}ScanLine[i];

    for j := 0 to Bmp.Width - 1 do
    begin
      RValue := p^[j].R;
      GValue := p^[j].G;
      BValue := p^[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      p^[j].R := Color and $000000FF;
      p^[j].G := (Color and $0000FF00) shr 8;
      p^[j].B := (Color and $00FF0000) shr 16;
    end;
  end;

  Result := True;
end;

function SetBitmapContrast(Value: Single; Bmp: TBitmap; ColorUnchanged: TColor): Boolean;
var
  LUT: array[0..255] of double;
  i, j, RValue, GValue, BValue: Integer;
  R, G, B: array[0..255] of double;
  Color, cl: TColor;
  p: PRGBArray;
begin
  Result := False;
  if Bmp.Empty then Exit;

  Bmp.PixelFormat := pf24Bit;
  if Value < 0.05 then Value := 0.05;

  for i := 0 to 255 do
    if (Value * i) > 255 then LUT[i] := 255
    else LUT[i] := Value * i;

  FillChar(R, SizeOf(R), 0);
  FillChar(G, SizeOf(G), 0);
  FillChar(B, SizeOf(B), 0);

  for i := 0 to Bmp.Height - 1 do
  begin
    p := Bmp.{%H-}ScanLine[i];

    for j := 0 to Bmp.Width - 1 do
    begin
      cl := RGB(p^[j].R, p^[j].G, p^[j].B);
      if cl = ColorUnchanged then Continue;

      RValue := p^[j].R;
      GValue := p^[j].G;
      BValue := p^[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      p^[j].R := Color and $000000FF;
      p^[j].G := (Color and $0000FF00) shr 8;
      p^[j].B := (Color and $00FF0000) shr 16;
    end;

  end;

  Result := True;
end;

function SetBitmapContrast(Value: Single; Bmp: TBitmap): Boolean;
var
  LUT: array[0..255] of double;
  i, j, RValue, GValue, BValue: Integer;
  R, G, B: array[0..255] of double;
  Color: TColor;
  p: PRGBArray;
begin
  Result := False;
  if Bmp.Empty then Exit;

  Bmp.PixelFormat := pf24Bit;
  if Value < 0.05 then Value := 0.05;

  for i := 0 to 255 do
    if (Value * i) > 255 then LUT[i] := 255
    else LUT[i] := Value * i;

  FillChar(R, SizeOf(R), 0);
  FillChar(G, SizeOf(G), 0);
  FillChar(B, SizeOf(B), 0);

  for i := 0 to Bmp.Height - 1 do
  begin
    p := Bmp.{%H-}ScanLine[i];

    for j := 0 to Bmp.Width - 1 do
    begin
      RValue := p^[j].R;
      GValue := p^[j].G;
      BValue := p^[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      p^[j].R := Color and $000000FF;
      p^[j].G := (Color and $0000FF00) shr 8;
      p^[j].B := (Color and $00FF0000) shr 16;
    end;

  end;

  Result := True;
end;

function SetBitmapBrightness(Value: TBrightInt; Bmp: TBitmap; ColorUnchanged: TColor): Boolean;
var
  i, j, Val: integer;
  Line: PRGBArray;
  Color: TColor;
begin
  Result := False;
  if Bmp.Empty then Exit;
  Bmp.PixelFormat := pf24Bit;

  Val := Value * 255 div 100;

  if Val > 0 then
    for i := 0 to Bmp.Height - 1 do
    begin
      Line := Bmp.{%H-}ScanLine[i];

      for j := 0 to Bmp.Width - 1 do
        with Line^[j] do
        begin
          Color := RGB(R, G, B);
          if Color = ColorUnchanged then Continue;
          if B + Val > 255 then B := 255 else B := B + Val;
          if G + Val > 255 then G := 255 else G := G + Val;
          if R + Val > 255 then R := 255 else R := R + Val;
        end;

    end // for i

  else // Val < 0

    for i := 0 to Bmp.Height - 1 do
    begin
      Line := Bmp.{%H-}ScanLine[i];

      for j := 0 to Bmp.Width - 1 do
        with Line^[j] do
        begin
          if B + Val < 0 then B := 0 else B := B + Val;
          if G + Val < 0 then G := 0 else G := G + Val;
          if R + Val < 0 then R := 0 else R := R + Val;
        end;

    end; // for i

  Result := True;
end;

function SetBitmapBrightness(Value: TBrightInt; Bmp: TBitmap): Boolean;
var
  i, j, Val: integer;
  Line: PRGBArray;
begin
  Result := False;
  if Bmp.Empty then Exit;
  Bmp.PixelFormat := pf24Bit;

  Val := Value * 255 div 100;

  if Val > 0 then
    for i := 0 to Bmp.Height - 1 do
    begin
      Line := Bmp.{%H-}ScanLine[i];

      for j := 0 to Bmp.Width - 1 do
        with Line^[j] do
        begin
          if B + Val > 255 then B := 255 else B := B + Val;
          if G + Val > 255 then G := 255 else G := G + Val;
          if R + Val > 255 then R := 255 else R := R + Val;
        end;

    end // for i

  else // Val < 0

    for i := 0 to Bmp.Height - 1 do
    begin
      Line := Bmp.{%H-}ScanLine[i];

      for j := 0 to Bmp.Width - 1 do
        with Line^[j] do
        begin
          if B + Val < 0 then B := 0 else B := B + Val;
          if G + Val < 0 then G := 0 else G := G + Val;
          if R + Val < 0 then R := 0 else R := R + Val;
        end;

    end; // for i

  Result := True;
end;

function MirrorBitmap(Bmp: TBitmap): Boolean;
var
  i, j: Integer;
  Line: PRGBArray;
  RGBRec: TRGBRec;
begin
  Result := False;
  if Bmp.Empty then Exit;
  Bmp.PixelFormat := pf24Bit;

  for i := 0 to Bmp.Height - 1 do
  begin
    Line := Bmp.{%H-}ScanLine[i];
    for j := 0 to (Bmp.Width - 1) div 2 do
    begin
      RGBRec := Line^[j];
      Line^[j] := Line^[Bmp.Width - j - 1];
      Line^[Bmp.Width - j - 1] := RGBRec;
    end;
  end;
  Result := True;
end;

function InvertBitmap(Bmp: TBitmap): Boolean;
var
  i, j: Integer;
  Line: PRGBArray;
begin
  Result := False;
  if Bmp.Empty then Exit;
  Bmp.PixelFormat := pf24Bit;

  for i := 0 to Bmp.Height - 1 do
  begin
    Line := Bmp.{%H-}ScanLine[i];
    for j := 0 to Bmp.Width - 1 do
      with Line^[j] do
      begin
        B := not B;
        G := not G;
        R := not R;
      end;
  end;
  Result := True;
end;

function FlipBitmap(Bmp: TBitmap): Boolean;
var
  i, j: Integer;
  Line, Line2: PRGBArray;
  RGBRec: TRGBRec;
begin
  Result := False;
  if Bmp.Empty then Exit;
  Bmp.PixelFormat := pf24Bit;

  for i := 0 to (Bmp.Height - 1) div 2 do
  begin
    Line := Bmp.{%H-}ScanLine[i];
    Line2 := Bmp.{%H-}ScanLine[Bmp.Height - i - 1];
    for j := 0 to Bmp.Width - 1 do
    begin
      RGBRec := Line^[j];
      Line^[j] := Line2^[j];
      Line2^[j] := RGBRec;
    end;
  end;
  Result := True;
end;
{$endregion Bitmap Procs}


{$region '                  TPixelConv                   '}

{$IFDEF MSWINDOWS}
class function TPixelConv.InfoStr: string;
const
  ENDL = #13#10;
begin
  Result :=
    'Screen resolution in pixels: ' + IntToStr(HorizontalRes) + ' x ' + IntToStr(VerticalRes) + ENDL +
    'Screen size in millimeters: ' + IntToStr(HorizontalSize) + ' x ' + IntToStr(VerticalSize) + ENDL +
    ENDL +
    'Horizontal parameters:' + ENDL +
    '    1 mm = ' + FormatFloat('0.0000 pix', PixelsPerMillimeterX) + ENDL +
    '    1 cm = ' + FormatFloat('0.0000 pix', PixelsPerMillimeterX * 10) + ENDL +
    ENDL +
    'Vertical parameters:' + ENDL +
    '    1 mm = ' + FormatFloat('0.0000 pix', PixelsPerMillimeterY) + ENDL +
    '    1 cm = ' + FormatFloat('0.0000 pix', PixelsPerMillimeterY * 10) + ENDL
    ;
end;

class procedure TPixelConv.Init(Canvas: TCanvas);
var
  h: HDC;
  xHorzRes, xVertRes, xHorzSize, xVertSize: integer;
begin
  h := Canvas.Handle;
  xHorzRes := GetDeviceCaps(h, HORZRES); // screen width in pixels
  xVertRes := GetDeviceCaps(h, VERTRES); // screen height in pixels
  xHorzSize := GetDeviceCaps(h, HORZSIZE); // screen width in mm
  xVertSize := GetDeviceCaps(h, VERTSIZE); // screen height in mm
  PixelsPerMillimeterX := xHorzRes / xHorzSize;
  PixelsPerMillimeterY := xVertRes / xVertSize;

  HorizontalRes := xHorzRes;
  VerticalRes := xVertRes;
  HorizontalSize := xHorzSize;
  VerticalSize := xVertSize;
end;

class function TPixelConv.ToPixelsX(const Millimeters: Single): Single;
begin
  Result := Millimeters * PixelsPerMillimeterX;
end;

class function TPixelConv.ToPixelsY(const Millimeters: Single): Single;
begin
  Result := Millimeters * PixelsPerMillimeterY;
end;
{$ENDIF} // MSWINDOWS

{$endregion TPixelConv}



end.
