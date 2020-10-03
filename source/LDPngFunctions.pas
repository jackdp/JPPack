{ $ID: LDPngFunctions $ }
{
  Routines related to PNG processing.

  Most of the code from PngFunctions.pas (PngComponents: https://github.com/UweRaabe/PngComponents)

}
unit LDPngFunctions;

{$I jpp.inc}

interface

uses
  {$IFDEF DCC}Windows, Graphics, Classes, ImgList, Contnrs, pngimage, Buttons, JPP.Graphics, Math, PngFunctions;{$ENDIF}

  {$IFDEF FPC}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  LCLType, LCLIntf, Classes, Types, Graphics, ImgList, Buttons, GraphType, JPP.Graphics, Math;
  {$ENDIF}


{$IFDEF DCC}
{$IF RTLVersion < 20.0 } // Delphi 2007 or below
  {$IF RTLVersion < 15.0 }
    PngComponents are only compatible with Delphi 7 and higher!
  {$IFEND}
type
  TPngImage = TPNGObject;
{$IFEND}
{$ENDIF}


{$IFDEF FPC}
type
  TPngImage = TPortableNetworkGraphic;

  TByteArray = Array[Word] of Byte;
  pByteArray = ^TByteArray;

  TRGBLine = array[Word] of TRGBTriple;
  PRGBLine = ^TRGBLine;
  TRGBALine = array[Word] of TRGBQuad;
  PRGBALine = ^TRGBALine;

{$ENDIF}


type
  TPngOption = (pngBlendOnDisabled, pngGrayscaleOnDisabled);
  TPngOptions = set of TPngOption;


procedure MakeDisabledImage(const Png: TPngImage; const BlendIntensity: TJppBlendIntensity; const GrayscaleIntensity: TJppGrayscaleIntensity;
  const bBlendEnabled, bGrayscaleEnabled: Boolean);

procedure MakeImageBlended(const Image: TPngImage; Amount: Byte = 127);
procedure MakeImageGrayscale(const Image: TPngImage; Amount: Byte = 255);
procedure DrawPNG(Png: TPngImage; Canvas: TCanvas; const ARect: TRect; const Options: TPngOptions);
{$IFDEF DCC}
procedure ConvertToPNG(Source: TGraphic; Dest: TPngImage);
procedure CreatePNG(Color, Mask: TBitmap; Dest: TPngImage; InverseMask: Boolean = False);
procedure CreatePNGMasked(Bitmap: TBitmap; Mask: TColor; Dest: TPngImage);
procedure SlicePNG(JoinedPNG: TPngImage; Columns, Rows: Integer; out SlicedPNGs: TObjectList);
{$ENDIF}

{$IFDEf FPC}
procedure CopyImageFromImageList(Dest: TPngImage; ImageList: TCustomImageList; Index: Integer);
procedure CalcButtonLayout(Canvas: TCanvas; pngimage: TPngImage; const Client: TRect; Pressed, Down: Boolean; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; var GlyphPos, TextPos: TPoint; BiDiFlags: LongInt);
{$ENDIF}


implementation

uses
  SysUtils;

function ColorToTriple(Color: TColor): TRGBTriple;
var
  ColorRGB: Longint;
begin
  ColorRGB := ColorToRGB(Color);
  Result.rgbtBlue := ColorRGB shr 16 and $FF;
  Result.rgbtGreen := ColorRGB shr 8 and $FF;
  Result.rgbtRed := ColorRGB and $FF;
end;



{$IFDEF FPC}
procedure CopyImageFromImageList(Dest: TPngImage; ImageList: TCustomImageList; Index: Integer);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    //Bmp.PixelFormat := pf32bit;
    ImageList.GetBitmap(Index, Bmp);
    Bmp.Transparent := True;
    Bmp.Width := ImageList.Width;
    Bmp.Height := ImageList.Height;
    Dest.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;
{$ENDIF}



function PngSetGammaAndGrayscale(const Png: TPngImage; GammaValue: Single; GrayscaleValue: Byte = 255): Boolean; overload;
var
  i, j, RValue, GValue, BValue, Y, X: integer;
  R, G, B: array[0..255] of Double;
  Color: TColor;
  LUT: array[0..255] of Double;
  Line: PRGBArray;
  LineA: PRGB32Array;
  LineG: PRGBLine;
  LineGA: PRGBALine;
begin
  Result := False;
  //if GammaValue = 1 then Exit;
  if Png.Empty then Exit;

  if GammaValue < 0.1 then GammaValue := 0.1;

  for i := 0 to 255 do
    if (255 * Power(i / 255, 1 / GammaValue)) > 255 then LUT[i] := 255
    else LUT[i] := 255 * Power(i / 255, 1 / GammaValue);

  FillChar(R, SizeOf(R), 0);
  FillChar(G, SizeOf(G), 0);
  FillChar(B, SizeOf(B), 0);

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

  Png.Modified := True;

  if GrayscaleValue > 0 then
  begin

    if PngHasAlphaChannel(Png) then

      for Y := 0 to Png.Height - 1 do
      begin
        LineGA := Png.{%H-}Scanline[Y];
        for X := 0 to Png.Width - 1 do
          GrayscaleRGB(LineGA^[X].rgbRed, LineGA^[X].rgbGreen, LineGA^[X].rgbBlue, GrayscaleValue);
      end

    else

      for Y := 0 to Png.Height - 1 do
      begin
        LineG := Png.{%H-}Scanline[Y];
        for X := 0 to Png.Width - 1 do
          GrayscaleRGB(LineG^[X].rgbtRed, LineG^[X].rgbtGreen, LineG^[X].rgbtBlue, GrayscaleValue);
      end;

  end;
         Png.Modified := True;
  Result := True;
end;


procedure MakeDisabledImage(const Png: TPngImage; const BlendIntensity: TJppBlendIntensity; const GrayscaleIntensity: TJppGrayscaleIntensity;
  const bBlendEnabled, bGrayscaleEnabled: Boolean);
var
  PngTmp: TPngImage;
  xgr: Byte;
  {$IFDEF FPC}
  Gamma: Single;
  Bright: TBrightInt;
  {$ELSE}
  BlendVal: Byte;
  {$ENDIF}
begin
  if (not bBlendEnabled) and (not bGrayscaleEnabled) then Exit;

  PngTmp := TPngImage.Create;
  try

    if not PngGetCopy(Png, PngTmp) then Exit;

    if bGrayscaleEnabled and (GrayscaleIntensity <> giNone) then
    begin

      case GrayscaleIntensity of
        gi100Percent: xgr := 255;
        gi90Percent: xgr := 230;
        gi80Percent: xgr := 204;
        gi70Percent: xgr := 179;
        gi60Percent: xgr := 153;
        gi50Percent: xgr := 128;
        gi40Percent: xgr := 102;
        gi30Percent: xgr := 77;
        gi20Percent: xgr := 51;
      else
        xgr := 25; // gi10Percent
      end;

      {$IFDEF DCC}
      //MakeImageGrayscale(PngTmp, xgr);
      PngGrayscale(PngTmp, xgr);
      {$ELSE}
      PngGrayscale(PngTmp, xgr);
      {$ENDIF}

    end;

    {$IFDEF FPC}

    // Pseudo blending
    if bBlendEnabled and (BlendIntensity <> biNone) then
    begin

      case BlendIntensity of
        biLow:
          begin
            Bright := 1;
            Gamma := 1.0;
          end;
        biBelowNormal:
          begin
            Bright := 1;
            Gamma := 1.5;
          end;
        biNormal:
          begin
            Bright := 1;
            Gamma := 2.0;
          end;
        biBelowMax:
          begin
            Bright := 2;
            Gamma := 2.5;
          end;
        biMax:
          begin
            Bright := 4;
            Gamma := 3.0;
          end;
        else
          begin
            Bright := 1;
            Gamma := 2.0;
          end;
      end;

      PngSetBrightness(PngTmp, Bright);
      PngSetGamma(PngTmp, Gamma);

    end;

    {$ELSE} // DCC

    if bBlendEnabled and (BlendIntensity <> biNone) then
    begin

      case BlendIntensity of
        biLow: BlendVal := 200;
        biBelowNormal: BlendVal := 160;
        biNormal: BlendVal := 127;
        biBelowMax: BlendVal := 100;
        biMax: BlendVal := 80;
      else
        BlendVal := 127;
      end;

      MakeImageBlended(PngTmp, BlendVal);

    end;

    {$ENDIF}

    Png.Assign(PngTmp);

  finally
    PngTmp.Free;
  end;
end;


{$IFDEF FPC}
{ DONE : Implement MakeImageBlended }
procedure MakeImageBlended(const Image: TPngImage; Amount: Byte = 127);
var
  PngTmp: TPngImage;
begin
  //PngSetGammaAndGrayscale(Image, 8, Amount);
  //Exit;
  PngTmp := TPngImage.Create;
  try

    if not PngGetCopy(Image, PngTmp) then Exit;
    //PngSetGamma(PngTmp, (Amount / 50));
    //PngGrayscale(PngTmp, Amount);

    //PngSetGammaAndGrayscale(PngTmp, 4, Amount);
    PngGrayscale(PngTmp, 255);
    PngSetBrightness(PngTmp, 5);
    PngSetGamma(PngTmp, 3);

    Image.Assign(PngTmp);
    //Image.Canvas.Draw(0, 0, PngTmp);
  finally
    PngTmp.free;
  end;
end;
{$ENDIF}


{$IFDEF FPC}

{ DONE : Implement MakeImageGrayscale for FPC }
procedure MakeImageGrayscale(const Image: TPngImage; Amount: Byte = 255);
var
  PngTmp: TPngImage;
begin
  PngTmp := TPngImage.Create;
  try
    if not PngGetCopy(Image, PngTmp) then Exit;
    PngGrayscale(PngTmp);
    Image.Assign(PngTmp);
  finally
    PngTmp.free;
  end;
end;
{$ENDIF}

{$IFDEF DCC}
procedure MakeImageGrayscale(const Image: TPngImage; Amount: Byte = 255);
var
  X, Y, PalCount: Integer;
  Line: PRGBLine;
  PaletteHandle: HPalette;
  Palette: array[Byte] of TPaletteEntry;
begin
  //Don't do anything if the image is already a grayscaled one
  if not (Image.Header.ColorType in [COLOR_GRAYSCALE, COLOR_GRAYSCALEALPHA]) then begin
    if Image.Header.ColorType = COLOR_PALETTE then begin
      //Grayscale every palette entry
      PaletteHandle := Image.Palette;
      PalCount := GetPaletteEntries(PaletteHandle, 0, 256, Palette);
      for X := 0 to PalCount - 1 do
        GrayscaleRGB(Palette[X].peRed, Palette[X].peGreen, Palette[X].peBlue);
      SetPaletteEntries(PaletteHandle, 0, PalCount, Palette);
      Image.Palette := PaletteHandle;
    end
    else begin
      //Grayscale every pixel
      for Y := 0 to Image.Height - 1 do begin
        Line := Image.Scanline[Y];
        for X := 0 to Image.Width - 1 do
          GrayscaleRGB(Line[X].rgbtRed, Line[X].rgbtGreen, Line[X].rgbtBlue);
      end;
    end;
  end;
end;
{$ENDIF}

{$IFDEF FPC}
procedure DrawPNG(Png: TPngImage; Canvas: TCanvas; const ARect: TRect; const Options: TPngOptions);
var
  PngCopy: TPngImage;
begin
  if Options <> [] then begin
    PngCopy := TPngImage.Create;
    try
      PngCopy.Assign(Png);
      if pngBlendOnDisabled in Options then
        MakeImageBlended(PngCopy);
      if pngGrayscaleOnDisabled in Options then
        MakeImageGrayscale(PngCopy);
      //PngCopy.Draw(Canvas, ARect);
      Canvas.Draw(ARect.Left, ARect.Top, PngCopy);
    finally
      PngCopy.Free;
    end;
  end
  else begin
    //Png.Draw(Canvas, ARect);
    Canvas.Draw(ARect.Left, ARect.Top, Png);
  end;
end;
{$ENDIF}


{$IFDEF DCC}
procedure MakeImageBlended(const Image: TPngImage; Amount: Byte = 127);

  procedure ForceAlphachannel(BitTransparency: Boolean; TransparentColor: TColor);
  var
    Assigner: TBitmap;
    Temp: TPngImage;
    X, Y: Integer;
    Line: pngimage.PByteArray;
    Current: TColor;
  begin
    //Not all formats of PNG support an alpha-channel (paletted images for example),
    //so with this function, I simply recreate the PNG as being 32-bits, effectivly
    //forcing an alpha-channel on it.
    Temp := TPngImage.Create;
    try
      Assigner := TBitmap.Create;
      try
        Assigner.Width := Image.Width;
        Assigner.Height := Image.Height;
        Temp.Assign(Assigner);
      finally
        Assigner.Free;
      end;
      Temp.CreateAlpha;
      for Y := 0 to Image.Height - 1 do begin
        Line := Temp.AlphaScanline[Y];
        for X := 0 to Image.Width - 1 do begin
          Current := Image.Pixels[X, Y];
          Temp.Pixels[X, Y] := Current;
          if BitTransparency and (Current = TransparentColor) then
            Line[X] := 0
          else
            Line[X] := Amount;
        end;
      end;
      Image.Assign(Temp);
    finally
      Temp.Free;
    end;
  end;

var
  X, Y: Integer;
  Line: pngimage.PByteArray;
  Forced: Boolean;
  TransparentColor: TColor;
  BitTransparency: Boolean;
begin
  //If the PNG doesn't have an alpha channel, then add one
  BitTransparency := Image.TransparencyMode = ptmBit;
  TransparentColor := Image.TransparentColor;
  Forced := False;
  if not (Image.Header.ColorType in [COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA]) then begin
    Forced := Image.Header.ColorType in [COLOR_GRAYSCALE, COLOR_PALETTE];
    if Forced then
      ForceAlphachannel(BitTransparency, TransparentColor)
    else
      Image.CreateAlpha;
  end;

  //Divide the alpha values by 2
  if not Forced and (Image.Header.ColorType in [COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA]) then begin
    for Y := 0 to Image.Height - 1 do begin
      Line := Image.AlphaScanline[Y];
      for X := 0 to Image.Width - 1 do begin
        if BitTransparency and (Image.Pixels[X, Y] = TransparentColor) then
          Line[X] := 0
        else
          Line[X] := Round(Line[X] / 256 * (Amount + 1));
      end;
    end;
  end;
end;
{$ENDIF}

{$IFDEF DCC}
// from PngComponents - PngFunctions.pas
procedure DrawPNG(Png: TPngImage; Canvas: TCanvas; const ARect: TRect; const Options: TPngOptions);
var
  PngCopy: TPngImage;
begin
  if Options <> [] then begin
    PngCopy := TPngImage.Create;
    try
      PngCopy.Assign(Png);
      if pngBlendOnDisabled in Options then
        MakeImageBlended(PngCopy);
      if pngGrayscaleOnDisabled in Options then
        MakeImageGrayscale(PngCopy);
      PngCopy.Draw(Canvas, ARect);
    finally
      PngCopy.Free;
    end;
  end
  else begin
    Png.Draw(Canvas, ARect);
  end;
end;
{$ENDIF}

{$IFDEF DCC}
// from PngComponents - PngFunctions.pas
procedure ConvertToPNG(Source: TGraphic; Dest: TPngImage);
type
  TRGBALine = array[Word] of TRGBQuad;
  PRGBALine = ^TRGBALine;
var
  MaskLines: array of pngimage.PByteArray;

  function ColorToTriple(const Color: TColor): TRGBTriple;
  begin
    Result.rgbtBlue := Color shr 16 and $FF;
    Result.rgbtGreen := Color shr 8 and $FF;
    Result.rgbtRed := Color and $FF;
  end;

  procedure GetAlphaMask(SourceColor: TBitmap);
  type
    TBitmapInfoV4 = packed record
      bmiHeader: TBitmapV4Header; //Otherwise I may not get per-pixel alpha values.
      bmiColors: array[0..2] of TRGBQuad; // reserve space for color lookup table
    end;
  var
    Bits: PRGBALine;
    { The BitmapInfo parameter to GetDIBits is delared as var parameter. So instead of casting around, we simply use
      the absolute directive to refer to the same memory area. }
    BitmapInfo: TBitmapInfoV4;
    BitmapInfoFake: TBitmapInfo absolute BitmapInfo;
    I, X, Y: Integer;
    HasAlpha: Boolean;
    BitsSize: Integer;
    bmpDC: HDC;
    bmpHandle: HBITMAP;
  begin
    BitsSize := 4 * SourceColor.Width * SourceColor.Height;
    Bits := AllocMem(BitsSize);
    try
      FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
      BitmapInfo.bmiHeader.bV4Size := SizeOf(BitmapInfo.bmiHeader);
      BitmapInfo.bmiHeader.bV4Width := SourceColor.Width;
      BitmapInfo.bmiHeader.bV4Height := -SourceColor.Height; //Otherwise the image is upside down.
      BitmapInfo.bmiHeader.bV4Planes := 1;
      BitmapInfo.bmiHeader.bV4BitCount := 32;
      BitmapInfo.bmiHeader.bV4V4Compression := BI_BITFIELDS;
      BitmapInfo.bmiHeader.bV4SizeImage := BitsSize;
      BitmapInfo.bmiColors[0].rgbRed := 255;
      BitmapInfo.bmiColors[1].rgbGreen := 255;
      BitmapInfo.bmiColors[2].rgbBlue := 255;

      { Getting the bitmap Handle will invalidate the Canvas.Handle, so it is important to retrieve them in the correct
        order. As parameter evaluation order is undefined and differs between Win32 and Win64, we get invalid values
        for Canvas.Handle when we use those properties directly in the call to GetDIBits. }
      bmpHandle := SourceColor.Handle;
      bmpDC := SourceColor.Canvas.Handle;
      if GetDIBits(bmpDC, bmpHandle, 0, SourceColor.Height, Bits, BitmapInfoFake, DIB_RGB_COLORS) > 0 then begin
        //Because Win32 API is a piece of crap when it comes to icons, I have to check
        //whether an has an alpha-channel the hard way.
        HasAlpha := False;
        for I := 0 to (SourceColor.Height * SourceColor.Width) - 1 do begin
          if Bits[I].rgbReserved <> 0 then begin
            HasAlpha := True;
            Break;
          end;
        end;
        if HasAlpha then begin
          //OK, so not all alpha-values are 0, which indicates the existence of an
          //alpha-channel.
          I := 0;
          for Y := 0 to SourceColor.Height - 1 do
            for X := 0 to SourceColor.Width - 1 do begin
              MaskLines[Y][X] := Bits[I].rgbReserved;
              Inc(I);
            end;
        end;
      end;
    finally
      FreeMem(Bits, BitsSize);
    end;
  end;

  function WinXPOrHigher: Boolean;
  var
    Info: TOSVersionInfo;
  begin
    Info.dwOSVersionInfoSize := SizeOf(Info);
    GetVersionEx(Info);
    Result := (Info.dwPlatformId = VER_PLATFORM_WIN32_NT) and
      ((Info.dwMajorVersion > 5) or
      ((Info.dwMajorVersion = 5) and (Info.dwMinorVersion >= 1)));
  end;

var
  Temp, SourceColor, SourceMask: TBitmap;
  X, Y: Integer;
  Line: PRGBLine;
  MaskLine, AlphaLine: pngimage.PByteArray;
  TransparentColor, CurrentColor: TColor;
  IconInfo: TIconInfo;
  AlphaNeeded: Boolean;
begin
  Assert(Dest <> nil, 'Dest is nil!');
  //A PNG does not have to be converted
  if Source is TPngImage then begin
    Dest.Assign(Source);
    Exit;
  end;

  AlphaNeeded := False;
  Temp := TBitmap.Create;
  SetLength(MaskLines, Source.Height);
  for Y := 0 to Source.Height - 1 do begin
    MaskLines[Y] := AllocMem(Source.Width);
    FillMemory(MaskLines[Y], Source.Width, 255);
  end;
  try
    //Initialize intermediate color bitmap
    Temp.Width := Source.Width;
    Temp.Height := Source.Height;
    Temp.PixelFormat := pf24bit;

    //Now figure out the transparency
    if Source is TBitmap then begin
      if Source.Transparent then begin
        //TBitmap is just about comparing the drawn colors against the TransparentColor
        if TBitmap(Source).TransparentMode = tmFixed then
          TransparentColor := TBitmap(Source).TransparentColor
        else
          TransparentColor := TBitmap(Source).Canvas.Pixels[0, Source.Height - 1];

        for Y := 0 to Temp.Height - 1 do begin
          Line := Temp.ScanLine[Y];
          MaskLine := MaskLines[Y];
          for X := 0 to Temp.Width - 1 do begin
            CurrentColor := GetPixel(TBitmap(Source).Canvas.Handle, X, Y);
            if CurrentColor = TransparentColor then begin
              MaskLine^[X] := 0;
              AlphaNeeded := True;
            end;
            Line[X] := ColorToTriple(CurrentColor);
          end;
        end;
      end
      else begin
        Temp.Canvas.Draw(0, 0, Source);
      end;
    end
    else if Source is TIcon then begin
      //TIcon is more complicated, because there are bitmasked (classic) icons and
      //alphablended (modern) icons. Not to forget about the "inverse" color.
      GetIconInfo(TIcon(Source).Handle, IconInfo);
      SourceColor := TBitmap.Create;
      SourceMask := TBitmap.Create;
      try
        SourceColor.Handle := IconInfo.hbmColor;
        SourceMask.Handle := IconInfo.hbmMask;
        Temp.Canvas.Draw(0, 0, SourceColor);
        for Y := 0 to Temp.Height - 1 do begin
          MaskLine := MaskLines[Y];
          for X := 0 to Temp.Width - 1 do begin
            if GetPixel(SourceMask.Canvas.Handle, X, Y) <> 0 then begin
              MaskLine^[X] := 0;
              AlphaNeeded := True;
            end;
          end;
        end;
        if (GetDeviceCaps(SourceColor.Canvas.Handle, BITSPIXEL) = 32) and WinXPOrHigher then begin
          //This doesn't neccesarily mean we actually have 32bpp in the icon, because the
          //bpp of an icon is always the same as the display settings, regardless of the
          //actual color depth of the icon :(
          AlphaNeeded := True;
          GetAlphaMask(SourceColor);
        end;
        //This still doesn't work for alphablended icons...
      finally
        SourceColor.Free;
        SourceMask.Free
      end;
    end;

    //And finally, assign the destination PNG image
    Dest.Assign(Temp);
    if AlphaNeeded then begin
      Dest.CreateAlpha;
      for Y := 0 to Dest.Height - 1 do begin
        AlphaLine := Dest.AlphaScanline[Y];
        CopyMemory(AlphaLine, MaskLines[Y], Temp.Width);
      end;
    end;

  finally
    for Y := 0 to Source.Height - 1 do
      FreeMem(MaskLines[Y], Source.Width);
    Temp.Free;
  end;
end;
{$ENDIF}

{$IFDEF DCC}
// from PngComponents - PngFunctions.pas
procedure CreatePNG(Color, Mask: TBitmap; Dest: TPngImage; InverseMask: Boolean = False);
var
  Temp: TBitmap;
  Line: pngimage.PByteArray;
  X, Y: Integer;
begin
  Assert(Dest <> nil, 'Dest is nil!');
  //Create a PNG from two separate color and mask bitmaps. InverseMask should be
  //True if white means transparent, and black means opaque.
  if not (Color.PixelFormat in [pf24bit, pf32bit]) then begin
    Temp := TBitmap.Create;
    try
      Temp.Assign(Color);
      Temp.PixelFormat := pf24bit;
      Dest.Assign(Temp);
    finally
      Temp.Free;
    end;
  end
  else begin
    Dest.Assign(Color);
  end;

  //Copy the alpha channel.
  Dest.CreateAlpha;
  for Y := 0 to Dest.Height - 1 do begin
    Line := Dest.AlphaScanline[Y];
    for X := 0 to Dest.Width - 1 do begin
      if InverseMask then
        Line[X] := 255 - (GetPixel(Mask.Canvas.Handle, X, Y) and $FF)
      else
        Line[X] := GetPixel(Mask.Canvas.Handle, X, Y) and $FF;
    end;
  end;
end;
{$ENDIF}

{$IFDEF DCC}
// from PngComponents - PngFunctions.pas
procedure CreatePNGMasked(Bitmap: TBitmap; Mask: TColor; Dest: TPngImage);
var
  Temp: TBitmap;
  Line: pngimage.PByteArray;
  X, Y: Integer;
begin
  Assert(Dest <> nil, 'Dest is nil!');
  //Create a PNG from two separate color and mask bitmaps. InverseMask should be
  //True if white means transparent, and black means opaque.
  if not (Bitmap.PixelFormat in [pf24bit, pf32bit]) then begin
    Temp := TBitmap.Create;
    try
      Temp.Assign(Bitmap);
      Temp.PixelFormat := pf24bit;
      Dest.Assign(Temp);
    finally
      Temp.Free;
    end;
  end
  else begin
    Dest.Assign(Bitmap);
  end;

  //Copy the alpha channel.
  Dest.CreateAlpha;
  for Y := 0 to Dest.Height - 1 do begin
    Line := Dest.AlphaScanline[Y];
    for X := 0 to Dest.Width - 1 do
      Line[X] := Integer(TColor(GetPixel(Bitmap.Canvas.Handle, X, Y)) <> Mask) * $FF;
  end;
end;
{$ENDIF}

{$IFDEF DCC}
// from PngComponents - PngFunctions.pas
procedure SlicePNG(JoinedPNG: TPngImage; Columns, Rows: Integer; out SlicedPNGs: TObjectList);
var
  X, Y, ImageX, ImageY, OffsetX, OffsetY: Integer;
  Width, Height: Integer;
  Bitmap: TBitmap;
  BitmapLine: PRGBLine;
  AlphaLineA, AlphaLineB: pngimage.PByteArray;
  PNG: TPngImage;
begin
  //This function slices a large PNG file (e.g. an image with all images for a
  //toolbar) into smaller, equally-sized pictures.
  SlicedPNGs := TObjectList.Create(False);
  Width := JoinedPNG.Width div Columns;
  Height := JoinedPNG.Height div Rows;

  //Loop through the columns and rows to create each individual image
  for ImageY := 0 to Rows - 1 do begin
    for ImageX := 0 to Columns - 1 do begin
      OffsetX := ImageX * Width;
      OffsetY := ImageY * Height;
      Bitmap := TBitmap.Create;
      try
        Bitmap.Width := Width;
        Bitmap.Height := Height;
        Bitmap.PixelFormat := pf24bit;

        //Copy the color information into a temporary bitmap. We can't use TPngImage.Draw
        //here, because that would combine the color and alpha values.
        for Y := 0 to Bitmap.Height - 1 do begin
          BitmapLine := Bitmap.Scanline[Y];
          for X := 0 to Bitmap.Width - 1 do
            BitmapLine[X] := ColorToTriple(JoinedPNG.Pixels[X + OffsetX, Y + OffsetY]);
        end;

        PNG := TPngImage.Create;
        PNG.Assign(Bitmap);

        if JoinedPNG.Header.ColorType in [COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA] then begin
          //Copy the alpha channel
          PNG.CreateAlpha;
          for Y := 0 to PNG.Height - 1 do begin
            AlphaLineA := JoinedPNG.AlphaScanline[Y + OffsetY];
            AlphaLineB := PNG.AlphaScanline[Y];
            for X := 0 to PNG.Width - 1 do
              AlphaLineB[X] := AlphaLineA[X + OffsetX];
          end;
        end;

        SlicedPNGs.Add(PNG);
      finally
        Bitmap.Free;
      end;
    end;
  end;
end;
{$ENDIF}


{$IFDEF FPC}
// from PngComponents - PngButtonFunctions.pas
procedure CalcButtonLayout(Canvas: TCanvas; pngimage: TPngImage; const Client: TRect; Pressed, Down: Boolean; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; var GlyphPos, TextPos: TPoint; BiDiFlags: LongInt);
var
  ClientSize, GlyphSize, TextSize, TotalSize: TPoint;
  TextBounds: TRect;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
  begin
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else if Layout = blGlyphRight then Layout := blGlyphLeft;
  end;

  //Calculate the item sizes
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);

  if pngimage <> nil then GlyphSize := Point(pngimage.Width, pngimage.Height)
  else GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CALCRECT or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom - TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0, 0);
  end;

  //If the layout has the glyph on the right or the left, then both the
  //text and the glyph are centered vertically.  If the glyph is on the top
  //or the bottom, then both the text and the glyph are centered horizontally.
  if Layout in [blGlyphLeft, blGlyphRight] then GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2
  else GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;

  //If there is no text or no bitmap, then Spacing is irrelevant
  if (TextSize.X = 0) or (GlyphSize.X = 0) then Spacing := 0;

  //Adjust Margin and Spacing
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then Margin := (ClientSize.X - TotalSize.X) div 3
      else Margin := (ClientSize.Y - TotalSize.Y) div 3;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y + Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then Margin := (ClientSize.X - TotalSize.X) div 2
      else Margin := (ClientSize.Y - TotalSize.Y) div 2;
    end
  end
  else if Spacing = -1 then
  begin
    TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y - (Margin + GlyphSize.Y));
  end;

  case Layout of
    blGlyphLeft: GlyphPos.X := Margin;
    blGlyphRight: GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
    blGlyphTop: GlyphPos.Y := Margin;
    blGlyphBottom: GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
  end;

  if Layout in [blGlyphLeft, blGlyphRight] then TextPos.Y := (ClientSize.Y - TextSize.Y) div 2
  else TextPos.X := (ClientSize.X - TextSize.X) div 2;
  case Layout of
    blGlyphLeft: TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
    blGlyphRight: TextPos.X := GlyphPos.X - Spacing - TextSize.X;
    blGlyphTop: TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
    blGlyphBottom: TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
  end;

  //Fixup the result variables
  with GlyphPos do
  begin
    Inc(X, Client.Left + Integer(Pressed or Down));
    Inc(Y, Client.Top + Integer(Pressed or Down));
  end;
  with TextPos do
  begin
    Inc(X, Client.Left + Integer(Pressed or Down));
    Inc(Y, Client.Top + Integer(Pressed or Down));
  end;
end;
{$ENDIF}


{$IFDEF DCC}
{$IF RTLVersion >= 20.0 }
type
  TPNGObject = class(TPngImage);
initialization
  TPicture.RegisterFileFormat('', '', TPNGObject);
finalization
  TPicture.UnregisterGraphicClass(TPNGObject);
{$IFEND}
{$ENDIF}
end.
