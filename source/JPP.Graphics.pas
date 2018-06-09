{
  June, 2006

  Funkcje transformuj¹ce bitmapy
  i inne zwi¹zane z grafik¹
}

unit JPP.Graphics;




interface

uses
  Winapi.Windows, Winapi.ShellAPI, Winapi.Messages,
  System.SysUtils, System.Classes, System.Math,
  Vcl.Graphics, Vcl.Imaging.pngimage
  ;


const
  MaxPixelCount = 32767;


type

  TRGBRec = packed record
    R, G, B: BYTE;
  end;

  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..MaxPixelCount] of TRGBRec;

  TBrightInt = -100..100;


{$region '   INT - Bitmap Procs   '}
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
{$endregion}

{$region '   INT - PNG Procs   '}
procedure SetPNGAlpha(PNG: TPNGImage; Alpha: Byte);
function SetPngGamma(Value: Single; Png: TPngImage): Boolean;
function SetPngBrightness(Value: TBrightInt; Png: TPngImage): Boolean;
function SetPngContrast(Value: Single; Png: TPngImage): Boolean;
{$endregion}


function PointInRect(Point: TPoint; Rect: TRect): Boolean;

function GetIconCount(const FileName: string): Integer;
function RectHeight(R: TRect): integer;
function RectWidth(R: TRect): integer;



implementation


function RectWidth(R: TRect): integer;
begin
  Result := R.Left - R.Right;
end;

function RectHeight(R: TRect): integer;
begin
  Result := R.Bottom - R.Top;
end;


function GetIconCount(const FileName: string): Integer;
begin
  Result := ExtractIcon(hInstance, PChar(FileName), DWORD(-1));
end;

function PointInRect(Point: TPoint; Rect: TRect): Boolean;
begin
  Result := (Point.X >= Rect.Left) and (Point.X <= Rect.Width) and (Point.Y >= Rect.Top) and (Point.Y <= Rect.Bottom);
end;



{$region '            PNG Procs                '}
procedure SetPNGAlpha(PNG: TPNGImage; Alpha: Byte);
var
  pScanline: pByteArray;
  nScanLineCount, nPixelCount : Integer;
begin
  if Alpha = 255 then begin
    PNG.RemoveTransparency;
  end else
  begin
    PNG.CreateAlpha;

    for nScanLineCount := 0 to PNG.Height - 1 do
    begin
      pScanline := PNG.AlphaScanline[nScanLineCount];
      for nPixelCount := 0 to Png.Width - 1 do
        pScanline[nPixelCount] := Alpha;
    end;
  end;

  PNG.Modified := True;
end;

function SetPngContrast(Value: Single; Png: TPngImage): Boolean;
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
    p := Png.ScanLine[i];

    for j := 0 to Png.Width - 1 do
    begin
      RValue := p[j].R;
      GValue := p[j].G;
      BValue := p[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      p[j].R := Color and $000000FF;
      p[j].G := (Color and $0000FF00) shr 8;
      p[j].B := (Color and $00FF0000) shr 16;
    end;

  end;

  Result := True;
end;

function SetPngBrightness(Value: TBrightInt; Png: TPngImage): Boolean;
var
  i, j, Val: integer;
  Line: PRGBArray;
begin
  Result := False;
  if Png.Empty then Exit;

  Val := Value * 255 div 100;

  if Val > 0 then
    for i := 0 to Png.Height - 1 do
    begin
      Line := Png.ScanLine[i];

      for j := 0 to Png.Width - 1 do
        with Line[j] do
        begin
          if B + Val > 255 then B := 255 else B := B + Val;
          if G + Val > 255 then G := 255 else G := G + Val;
          if R + Val > 255 then R := 255 else R := R + Val;
        end;

    end // for i

  else // Val < 0

    for i := 0 to Png.Height - 1 do
    begin
      Line := Png.ScanLine[i];

      for j := 0 to Png.Width - 1 do
        with Line[j] do
        begin
          if B + Val < 0 then B := 0 else B := B + Val;
          if G + Val < 0 then G := 0 else G := G + Val;
          if R + Val < 0 then R := 0 else R := R + Val;
        end;

    end; // for i

  Result := True;
end;

function SetPngGamma(Value: Single; Png: TPngImage): Boolean;
var
  i, j, RValue, GValue, BValue: integer;
  R, G, B: array[0..255] of double;
  Color: TColor;
  LUT: array[0..255] of double;
  p: PRGBArray;
begin
  Result := False;
  if Png.Empty then Exit;

  //Png.PixelFormat := pf24Bit;
  if Value < 0.1 then Value := 0.1;

  for i := 0 to 255 do
    if (255 * Power(i / 255, 1 / Value)) > 255 then LUT[i] := 255
    else LUT[i] := 255 * Power(i / 255, 1 / Value);

  FillChar(R, SizeOf(R), 0);
  FillChar(G, SizeOf(G), 0);
  FillChar(B, SizeOf(B), 0);

  for i := 0 to Png.Height - 1 do
  begin
    p := Png.ScanLine[i];

    for j := 0 to Png.Width - 1 do
    begin
      RValue := p[j].R;
      GValue := p[j].G;
      BValue := p[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      p[j].R := Color and $000000FF;
      p[j].G := (Color and $0000FF00) shr 8;
      p[j].B := (Color and $00FF0000) shr 16;
    end;
  end;

  Result := True;
end;
{$endregion PNG Procs}


{$region '             Bitmap Procs                  '}
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
      p := Bmp.ScanLine[i];

      for j := 0 to Bmp.Width - 1 do
      begin
        Color := p[j].B + (p[j].G shl 8) + (p[j].R shl 16);
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
      p := Bmp.ScanLine[i];

      for j := 0 to Bmp.Width - 1 do
      begin
        Color := p[j].B + (p[j].G shl 8) + (p[j].R shl 16);
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
    p := Bmp.ScanLine[i];

    for j := 0 to Bmp.Width - 1 do
    begin
      RValue := p[j].R;
      GValue := p[j].G;
      BValue := p[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      p[j].R := Color and $000000FF;
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
    p := Bmp.ScanLine[i];

    for j := 0 to Bmp.Width - 1 do
    begin
      RValue := p[j].R;
      GValue := p[j].G;
      BValue := p[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      //p[j].R := Color and $000000FF;
      p[j].G := (Color and $0000FF00) shr 8;
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
    p := Bmp.ScanLine[i];

    for j := 0 to Bmp.Width - 1 do
    begin
      RValue := p[j].R;
      GValue := p[j].G;
      BValue := p[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      //p[j].R := Color and $000000FF;
      //p[j].G := (Color and $0000FF00) shr 8;
      p[j].B := (Color and $00FF0000) shr 16;
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
    p := Bmp.ScanLine[i];

    for j := 0 to Bmp.Width - 1 do
    begin
      RValue := p[j].R;
      GValue := p[j].G;
      BValue := p[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      p[j].R := Color and $000000FF;
      p[j].G := (Color and $0000FF00) shr 8;
      p[j].B := (Color and $00FF0000) shr 16;
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
    p := Bmp.ScanLine[i];

    for j := 0 to Bmp.Width - 1 do
    begin
      cl := RGB(p[j].R, p[j].G, p[j].B);
      if cl = ColorUnchanged then Continue;
      
      RValue := p[j].R;
      GValue := p[j].G;
      BValue := p[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      p[j].R := Color and $000000FF;
      p[j].G := (Color and $0000FF00) shr 8;
      p[j].B := (Color and $00FF0000) shr 16;
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
    p := Bmp.ScanLine[i];

    for j := 0 to Bmp.Width - 1 do
    begin
      RValue := p[j].R;
      GValue := p[j].G;
      BValue := p[j].B;

      Color :=
        Round(LUT[RValue]) +
        (Round(LUT[GValue]) shl 8) +
        (Round(LUT[BValue]) shl 16);

      p[j].R := Color and $000000FF;
      p[j].G := (Color and $0000FF00) shr 8;
      p[j].B := (Color and $00FF0000) shr 16;
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
      Line := Bmp.ScanLine[i];

      for j := 0 to Bmp.Width - 1 do
        with Line[j] do
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
      Line := Bmp.ScanLine[i];

      for j := 0 to Bmp.Width - 1 do
        with Line[j] do
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
      Line := Bmp.ScanLine[i];

      for j := 0 to Bmp.Width - 1 do
        with Line[j] do
        begin
          if B + Val > 255 then B := 255 else B := B + Val;
          if G + Val > 255 then G := 255 else G := G + Val;
          if R + Val > 255 then R := 255 else R := R + Val;
        end;

    end // for i

  else // Val < 0

    for i := 0 to Bmp.Height - 1 do
    begin
      Line := Bmp.ScanLine[i];

      for j := 0 to Bmp.Width - 1 do
        with Line[j] do
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
    Line := Bmp.ScanLine[i];
    for j := 0 to (Bmp.Width - 1) div 2 do
    begin
      RGBRec := Line[j];
      Line[j] := Line[Bmp.Width - j - 1];
      Line[Bmp.Width - j - 1] := RGBRec;
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
    Line := Bmp.ScanLine[i];
    for j := 0 to Bmp.Width - 1 do
      with Line[j] do
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
    Line := Bmp.ScanLine[i];
    Line2 := Bmp.ScanLine[Bmp.Height - i - 1];
    for j := 0 to Bmp.Width - 1 do
    begin
      RGBRec := Line[j];
      Line[j] := Line2[j];
      Line2[j] := RGBRec;
    end;
  end;
  Result := True;
end;
{$endregion Bitmap Procs}



end.
