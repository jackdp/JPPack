unit JPP.PngButton.ColorMaps;

{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface

uses
  {$IFDEF DCC}
  Winapi.Windows, System.SysUtils, System.IniFiles, Vcl.Graphics,
  {$ELSE}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, IniFiles, Graphics,
  {$ENDIF}
  JPL.Colors;

const
  COLORMAP_DEFAULT_INI_SECTION = 'JppPngButton_ColorMap';

type

  TJppPngButtonColorMapType = (
    cmtCustom,
    cmtAero, cmtAeroMod1,
    cmtBlack,
    cmtBlueVeryDark, cmtBlue,
    cmtGray, cmtGrayDark, cmtGrayVeryDark, cmtGray_Blue, cmtGray_Yellow,
    cmtGreenDark, cmtGreenVeryDark, cmtGreen,
    cmtHighContrastBlack, cmtHighContrastWhite,

    cmtVclAquaLightSlate, cmtVclAuric, cmtVclCarbon, cmtVclCharcoalDarkSlate, cmtVclCobaltXEMedia, cmtVclCoral, cmtVclEmerald,
    cmtVclGlossy, cmtVclGoldenGraphite, cmtVclIcebergClassico, cmtVclJet, cmtVclLight, cmtVclLuna, cmtVclMetropolisUIBlack, cmtVclMetropolisUIDark,
    cmtVclMetropolisUIBlue, cmtVclMetropolisUIGreen, cmtVclRubyGraphite, cmtVclSilver, cmtVclSlateClassico, cmtVclTurquoiseGray
  );

  TJppPngButtonIniColorFormat = (icfDefault, icfJPColors);

  TJppPngButtonStateColors = record
    BorderColor: TColor;
    FontColor: TColor;
    Color: TColor;
    BottomGradientColorFrom: TColor;
    BottomGradientColorTo: TColor;
    UpperGradientColorFrom: TColor;
    UpperGradientColorTo: TColor;
  end;

  TJppPngButtonColorMap = record
  public
    BorderWhenDefaultColor: TColor;
    FocusRectColor: TColor;
    Normal: TJppPngButtonStateColors;
    Hot: TJppPngButtonStateColors;
    Down: TJppPngButtonStateColors;
    Focused: TJppPngButtonStateColors;
    Disabled: TJppPngButtonStateColors;
    procedure SaveToIniFile(FileName: string; Section: string = COLORMAP_DEFAULT_INI_SECTION; Format: TJppPngButtonIniColorFormat = icfDefault);
    procedure LoadFromIniFile(FileName: string; Section: string = COLORMAP_DEFAULT_INI_SECTION; Format: TJppPngButtonIniColorFormat = icfDefault);
    function AsDfm: string;
  end;

procedure GetJppPngButtonColorMapByType(ColorMapType: TJppPngButtonColorMapType; var ColorMapRec: TJppPngButtonColorMap);

var
  JppPngButtonColorMap_Aero: TJppPngButtonColorMap;
  JppPngButtonColorMap_AeroMod1: TJppPngButtonColorMap;
  JppPngButtonColorMap_Black: TJppPngButtonColorMap;
  JppPngButtonColorMap_BlueVeryDark: TJppPngButtonColorMap;
  JppPngButtonColorMap_Blue: TJppPngButtonColorMap;

  JppPngButtonColorMap_Gray: TJppPngButtonColorMap;
  JppPngButtonColorMap_GrayDark: TJppPngButtonColorMap;
  JppPngButtonColorMap_GrayVeryDark: TJppPngButtonColorMap;
  JppPngButtonColorMap_Gray_Blue: TJppPngButtonColorMap;
  JppPngButtonColorMap_Gray_Yellow: TJppPngButtonColorMap;

  JppPngButtonColorMap_GreenDark: TJppPngButtonColorMap;
  JppPngButtonColorMap_GreenVeryDark: TJppPngButtonColorMap;
  JppPngButtonColorMap_Green: TJppPngButtonColorMap;
  JppPngButtonColorMap_HighContrastBlack: TJppPngButtonColorMap;
  JppPngButtonColorMap_HighContrastWhite: TJppPngButtonColorMap;

  JppPngButtonColorMap_VclAquaLightSlate: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclAuric: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclCarbon: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclCharcoalDarkSlate: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclCobaltXEMedia: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclCoral: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclEmerald: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclGlossy: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclGoldenGraphite: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclIcebergClassico: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclJet: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclLight: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclLuna: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclMetropolisUIBlack: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclMetropolisUIDark: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclMetropolisUIBlue: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclMetropolisUIGreen: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclRubyGraphite: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclSilver: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclSlateClassico: TJppPngButtonColorMap;
  JppPngButtonColorMap_VclTurquoiseGray: TJppPngButtonColorMap;



implementation


procedure GetJppPngButtonColorMapByType(ColorMapType: TJppPngButtonColorMapType; var ColorMapRec: TJppPngButtonColorMap);
begin
  case ColorMapType of
    cmtAero: ColorMapRec := JppPngButtonColorMap_Aero;
    cmtAeroMod1: ColorMapRec := JppPngButtonColorMap_AeroMod1;
    cmtBlack: ColorMapRec := JppPngButtonColorMap_Black;
    cmtBlueVeryDark: ColorMapRec := JppPngButtonColorMap_BlueVeryDark;
    cmtBlue: ColorMapRec := JppPngButtonColorMap_Blue;

    cmtGray: ColorMapRec := JppPngButtonColorMap_Gray;
    cmtGrayDark: ColorMapRec := JppPngButtonColorMap_GrayDark;
    cmtGrayVeryDark: ColorMapRec := JppPngButtonColorMap_GrayVeryDark;
    cmtGray_Blue: ColorMapRec := JppPngButtonColorMap_Gray_Blue;
    cmtGray_Yellow: ColorMapRec := JppPngButtonColorMap_Gray_Yellow;


    cmtGreenDark: ColorMapRec := JppPngButtonColorMap_GreenDark;
    cmtGreenVeryDark: ColorMapRec := JppPngButtonColorMap_GreenVeryDark;
    cmtGreen: ColorMapRec := JppPngButtonColorMap_Green;
    cmtHighContrastBlack: ColorMapRec := JppPngButtonColorMap_HighContrastBlack;
    cmtHighContrastWhite: ColorMapRec := JppPngButtonColorMap_HighContrastWhite;

    cmtVclAquaLightSlate: ColorMapRec := JppPngButtonColorMap_VclAquaLightSlate;
    cmtVclAuric: ColorMapRec := JppPngButtonColorMap_VclAuric;
    cmtVclCarbon: ColorMapRec := JppPngButtonColorMap_VclCarbon;
    cmtVclCharcoalDarkSlate: ColorMapRec := JppPngButtonColorMap_VclCharcoalDarkSlate;
    cmtVclCobaltXEMedia: ColorMapRec := JppPngButtonColorMap_VclCobaltXEMedia;
    cmtVclCoral: ColorMapRec := JppPngButtonColorMap_VclCoral;
    cmtVclEmerald: ColorMapRec := JppPngButtonColorMap_VclEmerald;
    cmtVclGlossy: ColorMapRec := JppPngButtonColorMap_VclGlossy;
    cmtVclGoldenGraphite: ColorMapRec := JppPngButtonColorMap_VclGoldenGraphite;
    cmtVclIcebergClassico: ColorMapRec := JppPngButtonColorMap_VclIcebergClassico;
    cmtVclJet: ColorMapRec := JppPngButtonColorMap_VclJet;
    cmtVclLight: ColorMapRec := JppPngButtonColorMap_VclLight;
    cmtVclLuna: ColorMapRec := JppPngButtonColorMap_VclLuna;
    cmtVclMetropolisUIBlack: ColorMapRec := JppPngButtonColorMap_VclMetropolisUIBlack;
    cmtVclMetropolisUIDark: ColorMapRec := JppPngButtonColorMap_VclMetropolisUIDark;
    cmtVclMetropolisUIBlue: ColorMapRec := JppPngButtonColorMap_VclMetropolisUIBlue;
    cmtVclMetropolisUIGreen: ColorMapRec := JppPngButtonColorMap_VclMetropolisUIGreen;
    cmtVclRubyGraphite: ColorMapRec := JppPngButtonColorMap_VclRubyGraphite;
    cmtVclSilver: ColorMapRec := JppPngButtonColorMap_VclSilver;
    cmtVclSlateClassico: ColorMapRec := JppPngButtonColorMap_VclSlateClassico;
    cmtVclTurquoiseGray: ColorMapRec := JppPngButtonColorMap_VclTurquoiseGray;
  end;
end;





{$region ' ------------------ TJppPngButtonColorMap.AsDfm ------------------------- '}
function TJppPngButtonColorMap.AsDfm: string;
var
  n: string;
  function gcs(Color: TColor): string;
  var
    r, g, b: Byte;
  begin
    Color := ColorToRGB(Color);
    GetRgbChannels(Color, r, g, b);
    Result := 'RGB(' + IntToStr(r) + ', ' + IntToStr(g) + ', ' + IntToStr(b) + ')';
    //Result := 'RGB(' + IntToStr(GetRValue(Color)) + ', ' + IntToStr(GetGValue(Color)) + ', ' + IntToStr(GetBValue(Color)) + ')';
  end;
begin
  n := #13#10;

  Result :=
    '  with JppPngButtonColorMap do' + n +
    '  begin' + n +
    '    BorderWhenDefaultColor := ' + gcs(BorderWhenDefaultColor) + ';' + n +
    '    FocusRectColor := ' + gcs(FocusRectColor) + ';' + n +

    '    Normal.BorderColor := ' + gcs(Normal.BorderColor) + ';' + n +
    '    Normal.FontColor := ' + gcs(Normal.FontColor) + ';' + n +
    '    Normal.Color := ' + gcs(Normal.Color) + ';' + n +
    '    Normal.UpperGradientColorFrom := ' + gcs(Normal.UpperGradientColorFrom) + ';' + n +
    '    Normal.UpperGradientColorTo := ' + gcs(Normal.UpperGradientColorTo) + ';' + n +
    '    Normal.BottomGradientColorFrom := ' + gcs(Normal.BottomGradientColorFrom) + ';' + n +
    '    Normal.BottomGradientColorTo := ' + gcs(Normal.BottomGradientColorTo) + ';' + n +

    '    Hot.BorderColor := ' + gcs(Hot.BorderColor) + ';' + n +
    '    Hot.FontColor := ' + gcs(Hot.FontColor) + ';' + n +
    '    Hot.Color := ' + gcs(Hot.Color) + ';' + n +
    '    Hot.UpperGradientColorFrom := ' + gcs(Hot.UpperGradientColorFrom) + ';' + n +
    '    Hot.UpperGradientColorTo := ' + gcs(Hot.UpperGradientColorTo) + ';' + n +
    '    Hot.BottomGradientColorFrom := ' + gcs(Hot.BottomGradientColorFrom) + ';' + n +
    '    Hot.BottomGradientColorTo := ' + gcs(Hot.BottomGradientColorTo) + ';' + n +

    '    Down.BorderColor := ' + gcs(Down.BorderColor) + ';' + n +
    '    Down.FontColor := ' + gcs(Down.FontColor) + ';' + n +
    '    Down.Color := ' + gcs(Down.Color) + ';' + n +
    '    Down.UpperGradientColorFrom := ' + gcs(Down.UpperGradientColorFrom) + ';' + n +
    '    Down.UpperGradientColorTo := ' + gcs(Down.UpperGradientColorTo) + ';' + n +
    '    Down.BottomGradientColorFrom := ' + gcs(Down.BottomGradientColorFrom) + ';' + n +
    '    Down.BottomGradientColorTo := ' + gcs(Down.BottomGradientColorTo) + ';' + n +

    '    Focused.BorderColor := ' + gcs(Focused.BorderColor) + ';' + n +
    '    Focused.FontColor := ' + gcs(Focused.FontColor) + ';' + n +
    '    Focused.Color := ' + gcs(Focused.Color) + ';' + n +
    '    Focused.UpperGradientColorFrom := ' + gcs(Focused.UpperGradientColorFrom) + ';' + n +
    '    Focused.UpperGradientColorTo := ' + gcs(Focused.UpperGradientColorTo) + ';' + n +
    '    Focused.BottomGradientColorFrom := ' + gcs(Focused.BottomGradientColorFrom) + ';' + n +
    '    Focused.BottomGradientColorTo := ' + gcs(Focused.BottomGradientColorTo) + ';' + n +

    '    Disabled.BorderColor := ' + gcs(Disabled.BorderColor) + ';' + n +
    '    Disabled.FontColor := ' + gcs(Disabled.FontColor) + ';' + n +
    '    Disabled.Color := ' + gcs(Disabled.Color) + ';' + n +
    '    Disabled.UpperGradientColorFrom := ' + gcs(Disabled.UpperGradientColorFrom) + ';' + n +
    '    Disabled.UpperGradientColorTo := ' + gcs(Disabled.UpperGradientColorTo) + ';' + n +
    '    Disabled.BottomGradientColorFrom := ' + gcs(Disabled.BottomGradientColorFrom) + ';' + n +
    '    Disabled.BottomGradientColorTo := ' + gcs(Disabled.BottomGradientColorTo) + ';' + n +

    '  end;';
end;
{$endregion}


{$region ' ---------------- TJppPngButtonColorMap.LoadFromIniFile --------------- '}
procedure TJppPngButtonColorMap.LoadFromIniFile(FileName, Section: string; Format: TJppPngButtonIniColorFormat);
var
  Ini: TIniFile;

  function GetColor(Section: string; Default: integer): integer;
  var
    s: string;
  begin
    s := Ini.ReadString(Section, 'Color', '');
    if Copy(s, 1, 1) <> '$' then s := '$' + s;
    try
      Result := StrToInt(s);
    except
      Result := Default;
    end;
  end;

begin
  Ini := TIniFile.Create(FileName);
  try

    if Format = icfDefault then
    begin
      BorderWhenDefaultColor := Ini.ReadInteger(Section, 'BorderWhenDefaultColor', BorderWhenDefaultColor);
      FocusRectColor := Ini.ReadInteger(Section, 'FocusRectColor', FocusRectColor);

      Normal.BorderColor := Ini.ReadInteger(Section, 'Normal.BorderColor', Normal.BorderColor);
      Normal.FontColor := Ini.ReadInteger(Section, 'Normal.FontColor', Normal.FontColor);
      Normal.Color := Ini.ReadInteger(Section, 'Normal.Color', Normal.Color);
      Normal.BottomGradientColorFrom := Ini.ReadInteger(Section, 'Normal.BottomGradientColorFrom', Normal.BottomGradientColorFrom);
      Normal.BottomGradientColorTo := Ini.ReadInteger(Section, 'Normal.BottomGradientColorTo', Normal.BottomGradientColorTo);
      Normal.UpperGradientColorFrom := Ini.ReadInteger(Section, 'Normal.UpperGradientColorFrom', Normal.UpperGradientColorFrom);
      Normal.UpperGradientColorTo := Ini.ReadInteger(Section, 'Normal.UpperGradientColorTo', Normal.UpperGradientColorTo);

      Hot.BorderColor := Ini.ReadInteger(Section, 'Hot.BorderColor', Hot.BorderColor);
      Hot.FontColor := Ini.ReadInteger(Section, 'Hot.FontColor', Hot.FontColor);
      Hot.Color := Ini.ReadInteger(Section, 'Hot.Color', Hot.Color);
      Hot.BottomGradientColorFrom := Ini.ReadInteger(Section, 'Hot.BottomGradientColorFrom', Hot.BottomGradientColorFrom);
      Hot.BottomGradientColorTo := Ini.ReadInteger(Section, 'Hot.BottomGradientColorTo', Hot.BottomGradientColorTo);
      Hot.UpperGradientColorFrom := Ini.ReadInteger(Section, 'Hot.UpperGradientColorFrom', Hot.UpperGradientColorFrom);
      Hot.UpperGradientColorTo := Ini.ReadInteger(Section, 'Hot.UpperGradientColorTo', Hot.UpperGradientColorTo);

      Down.BorderColor := Ini.ReadInteger(Section, 'Down.BorderColor', Down.BorderColor);
      Down.FontColor := Ini.ReadInteger(Section, 'Down.FontColor', Down.FontColor);
      Down.Color := Ini.ReadInteger(Section, 'Down.Color', Down.Color);
      Down.BottomGradientColorFrom := Ini.ReadInteger(Section, 'Down.BottomGradientColorFrom', Down.BottomGradientColorFrom);
      Down.BottomGradientColorTo := Ini.ReadInteger(Section, 'Down.BottomGradientColorTo', Down.BottomGradientColorTo);
      Down.UpperGradientColorFrom := Ini.ReadInteger(Section, 'Down.UpperGradientColorFrom', Down.UpperGradientColorFrom);
      Down.UpperGradientColorTo := Ini.ReadInteger(Section, 'Down.UpperGradientColorTo', Down.UpperGradientColorTo);

      Focused.BorderColor := Ini.ReadInteger(Section, 'Focused.BorderColor', Focused.BorderColor);
      Focused.FontColor := Ini.ReadInteger(Section, 'Focused.FontColor', Focused.FontColor);
      Focused.Color := Ini.ReadInteger(Section, 'Focused.Color', Focused.Color);
      Focused.BottomGradientColorFrom := Ini.ReadInteger(Section, 'Focused.BottomGradientColorFrom', Focused.BottomGradientColorFrom);
      Focused.BottomGradientColorTo := Ini.ReadInteger(Section, 'Focused.BottomGradientColorTo', Focused.BottomGradientColorTo);
      Focused.UpperGradientColorFrom := Ini.ReadInteger(Section, 'Focused.UpperGradientColorFrom', Focused.UpperGradientColorFrom);
      Focused.UpperGradientColorTo := Ini.ReadInteger(Section, 'Focused.UpperGradientColorTo', Focused.UpperGradientColorTo);

      Disabled.BorderColor := Ini.ReadInteger(Section, 'Disabled.BorderColor', Disabled.BorderColor);
      Disabled.FontColor := Ini.ReadInteger(Section, 'Disabled.FontColor', Disabled.FontColor);
      Disabled.Color := Ini.ReadInteger(Section, 'Disabled.Color', Disabled.Color);
      Disabled.BottomGradientColorFrom := Ini.ReadInteger(Section, 'Disabled.BottomGradientColorFrom', Disabled.BottomGradientColorFrom);
      Disabled.BottomGradientColorTo := Ini.ReadInteger(Section, 'Disabled.BottomGradientColorTo', Disabled.BottomGradientColorTo);
      Disabled.UpperGradientColorFrom := Ini.ReadInteger(Section, 'Disabled.UpperGradientColorFrom', Disabled.UpperGradientColorFrom);
      Disabled.UpperGradientColorTo := Ini.ReadInteger(Section, 'Disabled.UpperGradientColorTo', Disabled.UpperGradientColorTo);
    end

    else

    begin

      BorderWhenDefaultColor := GetColor('Color_BorderWhenDefaultColor', BorderWhenDefaultColor);
      FocusRectColor := GetColor('Color_FocusRectColor', FocusRectColor);

      Normal.BorderColor := GetColor('Color_Normal.BorderColor', Normal.BorderColor);
      Normal.FontColor := GetColor('Color_Normal.FontColor', Normal.FontColor);
      Normal.Color := GetColor('Color_Normal.Color', Normal.Color);
      Normal.BottomGradientColorFrom := GetColor('Color_Normal.BottomGradientColorFrom', Normal.BottomGradientColorFrom);
      Normal.BottomGradientColorTo := GetColor('Color_Normal.BottomGradientColorTo', Normal.BottomGradientColorTo);
      Normal.UpperGradientColorFrom := GetColor('Color_Normal.UpperGradientColorFrom', Normal.UpperGradientColorFrom);
      Normal.UpperGradientColorTo := GetColor('Color_Normal.UpperGradientColorTo', Normal.UpperGradientColorTo);

      Hot.BorderColor := GetColor('Color_Hot.BorderColor', Hot.BorderColor);
      Hot.FontColor := GetColor('Color_Hot.FontColor', Hot.FontColor);
      Hot.Color := GetColor('Color_Hot.Color', Hot.Color);
      Hot.BottomGradientColorFrom := GetColor('Color_Hot.BottomGradientColorFrom', Hot.BottomGradientColorFrom);
      Hot.BottomGradientColorTo := GetColor('Color_Hot.BottomGradientColorTo', Hot.BottomGradientColorTo);
      Hot.UpperGradientColorFrom := GetColor('Color_Hot.UpperGradientColorFrom', Hot.UpperGradientColorFrom);
      Hot.UpperGradientColorTo := GetColor('Color_Hot.UpperGradientColorTo', Hot.UpperGradientColorTo);

      Down.BorderColor := GetColor('Color_Down.BorderColor', Down.BorderColor);
      Down.FontColor := GetColor('Color_Down.FontColor', Down.FontColor);
      Down.Color := GetColor('Color_Down.Color', Down.Color);
      Down.BottomGradientColorFrom := GetColor('Color_Down.BottomGradientColorFrom', Down.BottomGradientColorFrom);
      Down.BottomGradientColorTo := GetColor('Color_Down.BottomGradientColorTo', Down.BottomGradientColorTo);
      Down.UpperGradientColorFrom := GetColor('Color_Down.UpperGradientColorFrom', Down.UpperGradientColorFrom);
      Down.UpperGradientColorTo := GetColor('Color_Down.UpperGradientColorTo', Down.UpperGradientColorTo);

      Focused.BorderColor := GetColor('Color_Focused.BorderColor', Focused.BorderColor);
      Focused.FontColor := GetColor('Color_Focused.FontColor', Focused.FontColor);
      Focused.Color := GetColor('Color_Focused.Color', Focused.Color);
      Focused.BottomGradientColorFrom := GetColor('Color_Focused.BottomGradientColorFrom', Focused.BottomGradientColorFrom);
      Focused.BottomGradientColorTo := GetColor('Color_Focused.BottomGradientColorTo', Focused.BottomGradientColorTo);
      Focused.UpperGradientColorFrom := GetColor('Color_Focused.UpperGradientColorFrom', Focused.UpperGradientColorFrom);
      Focused.UpperGradientColorTo := GetColor('Color_Focused.UpperGradientColorTo', Focused.UpperGradientColorTo);

      Disabled.BorderColor := GetColor('Color_Disabled.BorderColor', Disabled.BorderColor);
      Disabled.FontColor := GetColor('Color_Disabled.FontColor', Disabled.FontColor);
      Disabled.Color := GetColor('Color_Disabled.Color', Disabled.Color);
      Disabled.BottomGradientColorFrom := GetColor('Color_Disabled.BottomGradientColorFrom', Disabled.BottomGradientColorFrom);
      Disabled.BottomGradientColorTo := GetColor('Color_Disabled.BottomGradientColorTo', Disabled.BottomGradientColorTo);
      Disabled.UpperGradientColorFrom := GetColor('Color_Disabled.UpperGradientColorFrom', Disabled.UpperGradientColorFrom);
      Disabled.UpperGradientColorTo := GetColor('Color_Disabled.UpperGradientColorTo', Disabled.UpperGradientColorTo);

    end;

  finally
    Ini.Free;
  end;
end;
{$endregion TJppPngButtonColorMap.LoadFromIniFile}


{$region ' ---------------- TJppPngButtonColorMap.SaveToIniFile ----------------- '}
procedure TJppPngButtonColorMap.SaveToIniFile(FileName: string; Section: string; Format: TJppPngButtonIniColorFormat);
var
  Ini: TIniFile;

  procedure SaveJPColor(ColorName: string; Color: TColor);
  var
    Section: string;
  begin
    Section := 'Color_' + ColorName;
    Ini.EraseSection(Section);
    Ini.WriteString(Section, 'Color', IntToHex(ColorToRGB(Color), 6));
    Ini.WriteString(Section, 'Name', ColorName);
  end;

begin
  Ini := TIniFile.Create(FileName);
  try

    if Format = icfDefault then
    begin
      Ini.WriteInteger(Section, 'BorderWhenDefaultColor', ColorToRGB(BorderWhenDefaultColor));
      Ini.WriteInteger(Section, 'FocusRectColor', ColorToRGB(FocusRectColor));

      Ini.WriteInteger(Section, 'Normal.BorderColor', ColorToRGB(Normal.BorderColor));
      Ini.WriteInteger(Section, 'Normal.FontColor', ColorToRGB(Normal.FontColor));
      Ini.WriteInteger(Section, 'Normal.Color', ColorToRGB(Normal.Color));
      Ini.WriteInteger(Section, 'Normal.UpperGradientColorFrom', ColorToRGB(Normal.UpperGradientColorFrom));
      Ini.WriteInteger(Section, 'Normal.UpperGradientColorTo', ColorToRGB(Normal.UpperGradientColorTo));
      Ini.WriteInteger(Section, 'Normal.BottomGradientColorFrom', ColorToRGB(Normal.BottomGradientColorFrom));
      Ini.WriteInteger(Section, 'Normal.BottomGradientColorTo', ColorToRGB(Normal.BottomGradientColorTo));

      Ini.WriteInteger(Section, 'Hot.BorderColor', ColorToRGB(Hot.BorderColor));
      Ini.WriteInteger(Section, 'Hot.FontColor', ColorToRGB(Hot.FontColor));
      Ini.WriteInteger(Section, 'Hot.Color', ColorToRGB(Hot.Color));
      Ini.WriteInteger(Section, 'Hot.UpperGradientColorFrom', ColorToRGB(Hot.UpperGradientColorFrom));
      Ini.WriteInteger(Section, 'Hot.UpperGradientColorTo', ColorToRGB(Hot.UpperGradientColorTo));
      Ini.WriteInteger(Section, 'Hot.BottomGradientColorFrom', ColorToRGB(Hot.BottomGradientColorFrom));
      Ini.WriteInteger(Section, 'Hot.BottomGradientColorTo', ColorToRGB(Hot.BottomGradientColorTo));

      Ini.WriteInteger(Section, 'Down.BorderColor', ColorToRGB(Down.BorderColor));
      Ini.WriteInteger(Section, 'Down.FontColor', ColorToRGB(Down.FontColor));
      Ini.WriteInteger(Section, 'Down.Color', ColorToRGB(Down.Color));
      Ini.WriteInteger(Section, 'Down.UpperGradientColorFrom', ColorToRGB(Down.UpperGradientColorFrom));
      Ini.WriteInteger(Section, 'Down.UpperGradientColorTo', ColorToRGB(Down.UpperGradientColorTo));
      Ini.WriteInteger(Section, 'Down.BottomGradientColorFrom', ColorToRGB(Down.BottomGradientColorFrom));
      Ini.WriteInteger(Section, 'Down.BottomGradientColorTo', ColorToRGB(Down.BottomGradientColorTo));

      Ini.WriteInteger(Section, 'Focused.BorderColor', ColorToRGB(Focused.BorderColor));
      Ini.WriteInteger(Section, 'Focused.FontColor', ColorToRGB(Focused.FontColor));
      Ini.WriteInteger(Section, 'Focused.Color', ColorToRGB(Focused.Color));
      Ini.WriteInteger(Section, 'Focused.UpperGradientColorFrom', ColorToRGB(Focused.UpperGradientColorFrom));
      Ini.WriteInteger(Section, 'Focused.UpperGradientColorTo', ColorToRGB(Focused.UpperGradientColorTo));
      Ini.WriteInteger(Section, 'Focused.BottomGradientColorFrom', ColorToRGB(Focused.BottomGradientColorFrom));
      Ini.WriteInteger(Section, 'Focused.BottomGradientColorTo', ColorToRGB(Focused.BottomGradientColorTo));

      Ini.WriteInteger(Section, 'Disabled.BorderColor', ColorToRGB(Disabled.BorderColor));
      Ini.WriteInteger(Section, 'Disabled.FontColor', ColorToRGB(Disabled.FontColor));
      Ini.WriteInteger(Section, 'Disabled.Color', ColorToRGB(Disabled.Color));
      Ini.WriteInteger(Section, 'Disabled.UpperGradientColorFrom', ColorToRGB(Disabled.UpperGradientColorFrom));
      Ini.WriteInteger(Section, 'Disabled.UpperGradientColorTo', ColorToRGB(Disabled.UpperGradientColorTo));
      Ini.WriteInteger(Section, 'Disabled.BottomGradientColorFrom', ColorToRGB(Disabled.BottomGradientColorFrom));
      Ini.WriteInteger(Section, 'Disabled.BottomGradientColorTo', ColorToRGB(Disabled.BottomGradientColorTo));
    end

    else

    begin

      SaveJPColor('BorderWhenDefaultColor', BorderWhenDefaultColor);
      SaveJPColor('FocusRectColor', FocusRectColor);

      // ----------------------- Normal ------------------------
      SaveJPColor('Normal.BorderColor', Normal.BorderColor);
      SaveJPColor('Normal.FontColor', Normal.FontColor);
      SaveJPColor('Normal.Color', Normal.Color);
      SaveJPColor('Normal.UpperGradientColorFrom', Normal.UpperGradientColorFrom);
      SaveJPColor('Normal.UpperGradientColorTo', Normal.UpperGradientColorTo);
      SaveJPColor('Normal.BottomGradientColorFrom', Normal.BottomGradientColorFrom);
      SaveJPColor('Normal.BottomGradientColorTo', Normal.BottomGradientColorTo);

      // ----------------------- Hot ------------------------
      SaveJPColor('Hot.BorderColor', Hot.BorderColor);
      SaveJPColor('Hot.FontColor', Hot.FontColor);
      SaveJPColor('Hot.Color', Hot.Color);
      SaveJPColor('Hot.UpperGradientColorFrom', Hot.UpperGradientColorFrom);
      SaveJPColor('Hot.UpperGradientColorTo', Hot.UpperGradientColorTo);
      SaveJPColor('Hot.BottomGradientColorFrom', Hot.BottomGradientColorFrom);
      SaveJPColor('Hot.BottomGradientColorTo', Hot.BottomGradientColorTo);

      // ----------------------- Down ------------------------
      SaveJPColor('Down.BorderColor', Down.BorderColor);
      SaveJPColor('Down.FontColor', Down.FontColor);
      SaveJPColor('Down.Color', Down.Color);
      SaveJPColor('Down.UpperGradientColorFrom', Down.UpperGradientColorFrom);
      SaveJPColor('Down.UpperGradientColorTo', Down.UpperGradientColorTo);
      SaveJPColor('Down.BottomGradientColorFrom', Down.BottomGradientColorFrom);
      SaveJPColor('Down.BottomGradientColorTo', Down.BottomGradientColorTo);

      // ----------------------- Focused ------------------------
      SaveJPColor('Focused.BorderColor', Focused.BorderColor);
      SaveJPColor('Focused.FontColor', Focused.FontColor);
      SaveJPColor('Focused.Color', Focused.Color);
      SaveJPColor('Focused.UpperGradientColorFrom', Focused.UpperGradientColorFrom);
      SaveJPColor('Focused.UpperGradientColorTo', Focused.UpperGradientColorTo);
      SaveJPColor('Focused.BottomGradientColorFrom', Focused.BottomGradientColorFrom);
      SaveJPColor('Focused.BottomGradientColorTo', Focused.BottomGradientColorTo);

      // ----------------------- Disabled ------------------------
      SaveJPColor('Disabled.BorderColor', Disabled.BorderColor);
      SaveJPColor('Disabled.FontColor', Disabled.FontColor);
      SaveJPColor('Disabled.Color', Disabled.Color);
      SaveJPColor('Disabled.UpperGradientColorFrom', Disabled.UpperGradientColorFrom);
      SaveJPColor('Disabled.UpperGradientColorTo', Disabled.UpperGradientColorTo);
      SaveJPColor('Disabled.BottomGradientColorFrom', Disabled.BottomGradientColorFrom);
      SaveJPColor('Disabled.BottomGradientColorTo', Disabled.BottomGradientColorTo);

    end;

  finally
    Ini.Free;
  end;

end;
{$endregion TJppPngButtonColorMap.SaveToIniFile}



initialization

{$region ' ------------------------ color maps --------------------------- '}

  {$region ' ---------- Aero ----------- '}
  with JppPngButtonColorMap_Aero do
  begin
    BorderWhenDefaultColor := RGB(36, 170, 208);
    FocusRectColor := RGB(36, 170, 208);
    Normal.BorderColor := RGB(112, 112, 112);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(243, 243, 243);
    Normal.UpperGradientColorFrom := RGB(241, 241, 241);
    Normal.UpperGradientColorTo := RGB(235, 235, 235);
    Normal.BottomGradientColorFrom := RGB(221, 221, 221);
    Normal.BottomGradientColorTo := RGB(207, 207, 207);
    Hot.BorderColor := RGB(60, 127, 177);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(232, 245, 252);
    Hot.UpperGradientColorFrom := RGB(234, 246, 253);
    Hot.UpperGradientColorTo := RGB(217, 240, 252);
    Hot.BottomGradientColorFrom := RGB(190, 230, 253);
    Hot.BottomGradientColorTo := RGB(167, 217, 245);
    Down.BorderColor := RGB(30, 64, 89);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(197, 226, 241);
    Down.UpperGradientColorFrom := RGB(229, 244, 252);
    Down.UpperGradientColorTo := RGB(196, 229, 246);
    Down.BottomGradientColorFrom := RGB(152, 209, 239);
    Down.BottomGradientColorTo := RGB(114, 185, 223);
    Focused.BorderColor := RGB(36, 170, 208);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(243, 243, 243);
    Focused.UpperGradientColorFrom := RGB(241, 241, 241);
    Focused.UpperGradientColorTo := RGB(235, 235, 235);
    Focused.BottomGradientColorFrom := RGB(221, 221, 221);
    Focused.BottomGradientColorTo := RGB(207, 207, 207);
    Disabled.BorderColor := RGB(173, 178, 181);
    Disabled.FontColor := RGB(160, 160, 160);
    Disabled.Color := RGB(244, 244, 244);
    Disabled.UpperGradientColorFrom := RGB(244, 244, 244);
    Disabled.UpperGradientColorTo := RGB(244, 244, 244);
    Disabled.BottomGradientColorFrom := RGB(244, 244, 244);
    Disabled.BottomGradientColorTo := RGB(244, 244, 244);
  end;
  {$endregion}

  {$region ' ----------- Aero Mod 1 ------------- '}
  with JppPngButtonColorMap_AeroMod1 do
  begin
    BorderWhenDefaultColor := RGB(36, 170, 208);
    FocusRectColor := RGB(36, 170, 208);
    Normal.BorderColor := RGB(112, 112, 112);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(243, 243, 243);
    Normal.UpperGradientColorFrom := RGB(241, 241, 241);
    Normal.UpperGradientColorTo := RGB(235, 235, 235);
    Normal.BottomGradientColorFrom := RGB(235, 235, 235);
    Normal.BottomGradientColorTo := RGB(207, 207, 207);
    Hot.BorderColor := RGB(60, 127, 177);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(232, 245, 252);
    Hot.UpperGradientColorFrom := RGB(234, 246, 253);
    Hot.UpperGradientColorTo := RGB(217, 240, 252);
    Hot.BottomGradientColorFrom := RGB(217, 240, 252);
    Hot.BottomGradientColorTo := RGB(167, 217, 245);
    Down.BorderColor := RGB(30, 64, 89);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(197, 226, 241);
    Down.UpperGradientColorFrom := RGB(229, 244, 252);
    Down.UpperGradientColorTo := RGB(169, 216, 241);
    Down.BottomGradientColorFrom := RGB(169, 216, 241);
    Down.BottomGradientColorTo := RGB(114, 185, 223);
    Focused.BorderColor := RGB(36, 170, 208);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(232, 245, 252);
    Focused.UpperGradientColorFrom := RGB(232, 245, 252);
    Focused.UpperGradientColorTo := RGB(211, 236, 250);
    Focused.BottomGradientColorFrom := RGB(211, 236, 250);
    Focused.BottomGradientColorTo := RGB(199, 231, 248);
    Disabled.BorderColor := RGB(173, 178, 181);
    Disabled.FontColor := RGB(160, 160, 160);
    Disabled.Color := RGB(244, 244, 244);
    Disabled.UpperGradientColorFrom := RGB(244, 244, 244);
    Disabled.UpperGradientColorTo := RGB(239, 239, 239);
    Disabled.BottomGradientColorFrom := RGB(239, 239, 239);
    Disabled.BottomGradientColorTo := RGB(233, 233, 233);
  end;
  {$endregion}

  {$region ' ----------- Black ------------- '}
  with JppPngButtonColorMap_Black do
  begin
    BorderWhenDefaultColor := RGB(36, 170, 208);
    FocusRectColor := RGB(75, 75, 75);
    Normal.BorderColor := RGB(54, 54, 54);
    Normal.FontColor := RGB(205, 205, 205);
    Normal.Color := RGB(30, 30, 30);
    Normal.UpperGradientColorFrom := RGB(42, 42, 42);
    Normal.UpperGradientColorTo := RGB(30, 30, 30);
    Normal.BottomGradientColorFrom := RGB(30, 30, 30);
    Normal.BottomGradientColorTo := RGB(25, 25, 25);
    Hot.BorderColor := RGB(91, 91, 91);
    Hot.FontColor := RGB(205, 205, 205);
    Hot.Color := RGB(68, 68, 68);
    Hot.UpperGradientColorFrom := RGB(81, 81, 81);
    Hot.UpperGradientColorTo := RGB(68, 68, 68);
    Hot.BottomGradientColorFrom := RGB(68, 68, 68);
    Hot.BottomGradientColorTo := RGB(48, 48, 48);
    Down.BorderColor := RGB(54, 54, 54);
    Down.FontColor := RGB(205, 205, 205);
    Down.Color := RGB(10, 10, 10);
    Down.UpperGradientColorFrom := RGB(20, 20, 20);
    Down.UpperGradientColorTo := RGB(12, 12, 12);
    Down.BottomGradientColorFrom := RGB(12, 12, 12);
    Down.BottomGradientColorTo := RGB(6, 6, 6);
    Focused.BorderColor := RGB(91, 91, 91);
    Focused.FontColor := RGB(205, 205, 205);
    Focused.Color := RGB(51, 51, 51);
    Focused.UpperGradientColorFrom := RGB(68, 68, 68);
    Focused.UpperGradientColorTo := RGB(42, 42, 42);
    Focused.BottomGradientColorFrom := RGB(42, 42, 42);
    Focused.BottomGradientColorTo := RGB(32, 32, 32);
    Disabled.BorderColor := RGB(53, 53, 53);
    Disabled.FontColor := RGB(98, 98, 98);
    Disabled.Color := RGB(41, 41, 41);
    Disabled.UpperGradientColorFrom := RGB(51, 51, 51);
    Disabled.UpperGradientColorTo := RGB(41, 41, 41);
    Disabled.BottomGradientColorFrom := RGB(41, 41, 41);
    Disabled.BottomGradientColorTo := RGB(36, 36, 36);
  end;
  {$endregion}

  {$region ' ----------- Blue Very Dark ---------- '}
  with JppPngButtonColorMap_BlueVeryDark do
  begin
    BorderWhenDefaultColor := RGB(120, 200, 243);
    FocusRectColor := RGB(6, 77, 115);
    Normal.BorderColor := RGB(3, 35, 52);
    Normal.FontColor := RGB(185, 205, 215);
    Normal.Color := RGB(7, 86, 129);
    Normal.UpperGradientColorFrom := RGB(10, 96, 141);
    Normal.UpperGradientColorTo := RGB(7, 86, 129);
    Normal.BottomGradientColorFrom := RGB(7, 86, 129);
    Normal.BottomGradientColorTo := RGB(6, 57, 85);
    Hot.BorderColor := RGB(4, 51, 77);
    Hot.FontColor := RGB(205, 219, 226);
    Hot.Color := RGB(12, 130, 192);
    Hot.UpperGradientColorFrom := RGB(14, 133, 197);
    Hot.UpperGradientColorTo := RGB(12, 130, 192);
    Hot.BottomGradientColorFrom := RGB(12, 130, 192);
    Hot.BottomGradientColorTo := RGB(12, 104, 154);
    Down.BorderColor := RGB(6, 77, 115);
    Down.FontColor := RGB(205, 219, 226);
    Down.Color := RGB(10, 117, 175);
    Down.UpperGradientColorFrom := RGB(13, 120, 179);
    Down.UpperGradientColorTo := RGB(9, 105, 157);
    Down.BottomGradientColorFrom := RGB(9, 105, 157);
    Down.BottomGradientColorTo := RGB(10, 86, 126);
    Focused.BorderColor := RGB(6, 77, 115);
    Focused.FontColor := RGB(205, 219, 226);
    Focused.Color := RGB(10, 117, 175);
    Focused.UpperGradientColorFrom := RGB(13, 120, 179);
    Focused.UpperGradientColorTo := RGB(9, 105, 157);
    Focused.BottomGradientColorFrom := RGB(9, 105, 157);
    Focused.BottomGradientColorTo := RGB(10, 86, 126);
    Disabled.BorderColor := RGB(3, 35, 52);
    Disabled.FontColor := RGB(84, 129, 150);
    Disabled.Color := RGB(5, 64, 95);
    Disabled.UpperGradientColorFrom := RGB(5, 70, 105);
    Disabled.UpperGradientColorTo := RGB(5, 64, 95);
    Disabled.BottomGradientColorFrom := RGB(5, 64, 95);
    Disabled.BottomGradientColorTo := RGB(4, 60, 89);
  end;
  {$endregion}

  {$region ' ----------- Blue ------------- '}
  with JppPngButtonColorMap_Blue do
  begin
    BorderWhenDefaultColor := RGB(120, 200, 243);
    FocusRectColor := RGB(120, 200, 243);
    Normal.BorderColor := RGB(120, 200, 243);
    Normal.FontColor := RGB(3, 26, 37);
    Normal.Color := RGB(207, 236, 252);
    Normal.UpperGradientColorFrom := RGB(188, 228, 250);
    Normal.UpperGradientColorTo := RGB(176, 223, 249);
    Normal.BottomGradientColorFrom := RGB(176, 223, 249);
    Normal.BottomGradientColorTo := RGB(146, 210, 245);
    Hot.BorderColor := RGB(17, 146, 215);
    Hot.FontColor := RGB(7, 64, 90);
    Hot.Color := RGB(175, 222, 250);
    Hot.UpperGradientColorFrom := RGB(188, 228, 250);
    Hot.UpperGradientColorTo := RGB(146, 210, 245);
    Hot.BottomGradientColorFrom := RGB(146, 210, 245);
    Hot.BottomGradientColorTo := RGB(188, 228, 250);
    Down.BorderColor := RGB(14, 111, 163);
    Down.FontColor := RGB(3, 26, 37);
    Down.Color := RGB(150, 213, 250);
    Down.UpperGradientColorFrom := RGB(158, 217, 248);
    Down.UpperGradientColorTo := RGB(138, 208, 247);
    Down.BottomGradientColorFrom := RGB(138, 208, 247);
    Down.BottomGradientColorTo := RGB(104, 192, 240);
    Focused.BorderColor := RGB(12, 100, 148);
    Focused.FontColor := RGB(7, 64, 90);
    Focused.Color := RGB(169, 220, 250);
    Focused.UpperGradientColorFrom := RGB(188, 228, 250);
    Focused.UpperGradientColorTo := RGB(146, 210, 245);
    Focused.BottomGradientColorFrom := RGB(146, 210, 245);
    Focused.BottomGradientColorTo := RGB(188, 228, 250);
    Disabled.BorderColor := RGB(192, 208, 218);
    Disabled.FontColor := RGB(158, 172, 182);
    Disabled.Color := RGB(234, 240, 244);
    Disabled.UpperGradientColorFrom := RGB(227, 234, 238);
    Disabled.UpperGradientColorTo := RGB(222, 231, 235);
    Disabled.BottomGradientColorFrom := RGB(222, 231, 235);
    Disabled.BottomGradientColorTo := RGB(213, 223, 230);
  end;
  {$endregion}

  {$region ' ----------- Gray ------------- '}
  with JppPngButtonColorMap_Gray do
  begin
    BorderWhenDefaultColor := RGB(36, 170, 208);
    FocusRectColor := RGB(20, 96, 118);
    Normal.BorderColor := RGB(108, 108, 108);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(223, 223, 223);
    Normal.UpperGradientColorFrom := RGB(234, 234, 234);
    Normal.UpperGradientColorTo := RGB(225, 225, 225);
    Normal.BottomGradientColorFrom := RGB(225, 225, 225);
    Normal.BottomGradientColorTo := RGB(202, 202, 202);
    Hot.BorderColor := RGB(32, 148, 181);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(234, 234, 234);
    Hot.UpperGradientColorFrom := RGB(248, 248, 248);
    Hot.UpperGradientColorTo := RGB(240, 240, 240);
    Hot.BottomGradientColorFrom := RGB(240, 240, 240);
    Hot.BottomGradientColorTo := RGB(219, 219, 219);
    Down.BorderColor := RGB(20, 96, 118);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(212, 212, 212);
    Down.UpperGradientColorFrom := RGB(228, 228, 228);
    Down.UpperGradientColorTo := RGB(223, 223, 223);
    Down.BottomGradientColorFrom := RGB(223, 223, 223);
    Down.BottomGradientColorTo := RGB(200, 200, 200);
    Focused.BorderColor := RGB(20, 96, 118);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(234, 234, 234);
    Focused.UpperGradientColorFrom := RGB(248, 248, 248);
    Focused.UpperGradientColorTo := RGB(240, 240, 240);
    Focused.BottomGradientColorFrom := RGB(240, 240, 240);
    Focused.BottomGradientColorTo := RGB(219, 219, 219);
    Disabled.BorderColor := RGB(173, 178, 181);
    Disabled.FontColor := RGB(160, 160, 160);
    Disabled.Color := RGB(242, 242, 242);
    Disabled.UpperGradientColorFrom := RGB(248, 248, 248);
    Disabled.UpperGradientColorTo := RGB(240, 240, 240);
    Disabled.BottomGradientColorFrom := RGB(240, 240, 240);
    Disabled.BottomGradientColorTo := RGB(234, 234, 234);
  end;
  {$endregion}

  {$region ' ----------- Gray Dark ------------- '}
  with JppPngButtonColorMap_GrayDark do
  begin
    BorderWhenDefaultColor := RGB(36, 170, 208);
    FocusRectColor := RGB(20, 96, 118);
    Normal.BorderColor := RGB(95, 95, 95);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(209, 209, 209);
    Normal.UpperGradientColorFrom := RGB(224, 224, 224);
    Normal.UpperGradientColorTo := RGB(212, 212, 212);
    Normal.BottomGradientColorFrom := RGB(212, 212, 212);
    Normal.BottomGradientColorTo := RGB(191, 191, 191);
    Hot.BorderColor := RGB(26, 118, 145);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(224, 224, 224);
    Hot.UpperGradientColorFrom := RGB(239, 239, 239);
    Hot.UpperGradientColorTo := RGB(232, 232, 232);
    Hot.BottomGradientColorFrom := RGB(232, 232, 232);
    Hot.BottomGradientColorTo := RGB(207, 207, 207);
    Down.BorderColor := RGB(78, 78, 78);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(202, 202, 202);
    Down.UpperGradientColorFrom := RGB(215, 215, 215);
    Down.UpperGradientColorTo := RGB(197, 197, 197);
    Down.BottomGradientColorFrom := RGB(197, 197, 197);
    Down.BottomGradientColorTo := RGB(185, 185, 185);
    Focused.BorderColor := RGB(20, 96, 118);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(234, 234, 234);
    Focused.UpperGradientColorFrom := RGB(248, 248, 248);
    Focused.UpperGradientColorTo := RGB(240, 240, 240);
    Focused.BottomGradientColorFrom := RGB(240, 240, 240);
    Focused.BottomGradientColorTo := RGB(219, 219, 219);
    Disabled.BorderColor := RGB(173, 178, 181);
    Disabled.FontColor := RGB(160, 160, 160);
    Disabled.Color := RGB(225, 225, 225);
    Disabled.UpperGradientColorFrom := RGB(235, 235, 235);
    Disabled.UpperGradientColorTo := RGB(225, 225, 225);
    Disabled.BottomGradientColorFrom := RGB(225, 225, 225);
    Disabled.BottomGradientColorTo := RGB(216, 216, 216);
  end;
  {$endregion}

  {$region ' ----------- Gray Very Dark ------------- '}
  with JppPngButtonColorMap_GrayVeryDark do
  begin
    BorderWhenDefaultColor := RGB(36, 170, 208);
    FocusRectColor := RGB(126, 126, 126);
    Normal.BorderColor := RGB(72, 72, 72);
    Normal.FontColor := RGB(234, 234, 234);
    Normal.Color := RGB(129, 129, 129);
    Normal.UpperGradientColorFrom := RGB(143, 143, 143);
    Normal.UpperGradientColorTo := RGB(129, 129, 129);
    Normal.BottomGradientColorFrom := RGB(129, 129, 129);
    Normal.BottomGradientColorTo := RGB(112, 112, 112);
    Hot.BorderColor := RGB(75, 75, 75);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(167, 167, 167);
    Hot.UpperGradientColorFrom := RGB(187, 187, 187);
    Hot.UpperGradientColorTo := RGB(174, 174, 174);
    Hot.BottomGradientColorFrom := RGB(174, 174, 174);
    Hot.BottomGradientColorTo := RGB(154, 154, 154);
    Down.BorderColor := RGB(53, 53, 53);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(193, 193, 193);
    Down.UpperGradientColorFrom := RGB(212, 212, 212);
    Down.UpperGradientColorTo := RGB(193, 193, 193);
    Down.BottomGradientColorFrom := RGB(193, 193, 193);
    Down.BottomGradientColorTo := RGB(177, 177, 177);
    Focused.BorderColor := RGB(22, 22, 22);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(167, 167, 167);
    Focused.UpperGradientColorFrom := RGB(187, 187, 187);
    Focused.UpperGradientColorTo := RGB(174, 174, 174);
    Focused.BottomGradientColorFrom := RGB(174, 174, 174);
    Focused.BottomGradientColorTo := RGB(154, 154, 154);
    Disabled.BorderColor := RGB(83, 83, 83);
    Disabled.FontColor := RGB(106, 106, 106);
    Disabled.Color := RGB(155, 155, 155);
    Disabled.UpperGradientColorFrom := RGB(164, 164, 164);
    Disabled.UpperGradientColorTo := RGB(154, 154, 154);
    Disabled.BottomGradientColorFrom := RGB(154, 154, 154);
    Disabled.BottomGradientColorTo := RGB(148, 148, 148);
  end;
  {$endregion}

  {$region ' ----------- Gray Blue ------------- '}
  with JppPngButtonColorMap_Gray_Blue do
  begin
    BorderWhenDefaultColor := RGB(36, 170, 208);
    FocusRectColor := RGB(15, 112, 164);
    Normal.BorderColor := RGB(108, 108, 108);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(223, 223, 223);
    Normal.UpperGradientColorFrom := RGB(234, 234, 234);
    Normal.UpperGradientColorTo := RGB(225, 225, 225);
    Normal.BottomGradientColorFrom := RGB(225, 225, 225);
    Normal.BottomGradientColorTo := RGB(202, 202, 202);
    Hot.BorderColor := RGB(92, 189, 241);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(197, 232, 250);
    Hot.UpperGradientColorFrom := RGB(232, 245, 253);
    Hot.UpperGradientColorTo := RGB(197, 232, 250);
    Hot.BottomGradientColorFrom := RGB(197, 232, 250);
    Hot.BottomGradientColorTo := RGB(159, 216, 247);
    Down.BorderColor := RGB(15, 112, 164);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(174, 223, 249);
    Down.UpperGradientColorFrom := RGB(214, 238, 252);
    Down.UpperGradientColorTo := RGB(174, 223, 249);
    Down.BottomGradientColorFrom := RGB(174, 223, 249);
    Down.BottomGradientColorTo := RGB(88, 188, 241);
    Focused.BorderColor := RGB(15, 112, 164);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(197, 232, 250);
    Focused.UpperGradientColorFrom := RGB(232, 245, 253);
    Focused.UpperGradientColorTo := RGB(197, 232, 250);
    Focused.BottomGradientColorFrom := RGB(197, 232, 250);
    Focused.BottomGradientColorTo := RGB(159, 216, 247);
    Disabled.BorderColor := RGB(173, 178, 181);
    Disabled.FontColor := RGB(160, 160, 160);
    Disabled.Color := RGB(242, 242, 242);
    Disabled.UpperGradientColorFrom := RGB(248, 248, 248);
    Disabled.UpperGradientColorTo := RGB(240, 240, 240);
    Disabled.BottomGradientColorFrom := RGB(240, 240, 240);
    Disabled.BottomGradientColorTo := RGB(234, 234, 234);
  end;
  {$endregion}

  {$region ' ----------- Gray Yellow ------------- '}
  with JppPngButtonColorMap_Gray_Yellow do
  begin
    BorderWhenDefaultColor := RGB(36, 170, 208);
    FocusRectColor := RGB(193, 155, 0);
    Normal.BorderColor := RGB(108, 108, 108);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(223, 223, 223);
    Normal.UpperGradientColorFrom := RGB(234, 234, 234);
    Normal.UpperGradientColorTo := RGB(225, 225, 225);
    Normal.BottomGradientColorFrom := RGB(225, 225, 225);
    Normal.BottomGradientColorTo := RGB(202, 202, 202);
    Hot.BorderColor := RGB(232, 185, 0);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(255, 244, 204);
    Hot.UpperGradientColorFrom := RGB(255, 249, 223);
    Hot.UpperGradientColorTo := RGB(255, 244, 204);
    Hot.BottomGradientColorFrom := RGB(255, 244, 204);
    Hot.BottomGradientColorTo := RGB(255, 228, 121);
    Down.BorderColor := RGB(113, 90, 0);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(255, 235, 166);
    Down.UpperGradientColorFrom := RGB(255, 247, 210);
    Down.UpperGradientColorTo := RGB(255, 235, 166);
    Down.BottomGradientColorFrom := RGB(255, 235, 166);
    Down.BottomGradientColorTo := RGB(255, 215, 53);
    Focused.BorderColor := RGB(147, 117, 0);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(255, 244, 204);
    Focused.UpperGradientColorFrom := RGB(255, 249, 223);
    Focused.UpperGradientColorTo := RGB(255, 244, 204);
    Focused.BottomGradientColorFrom := RGB(255, 244, 204);
    Focused.BottomGradientColorTo := RGB(255, 228, 121);
    Disabled.BorderColor := RGB(173, 178, 181);
    Disabled.FontColor := RGB(160, 160, 160);
    Disabled.Color := RGB(242, 242, 242);
    Disabled.UpperGradientColorFrom := RGB(248, 248, 248);
    Disabled.UpperGradientColorTo := RGB(240, 240, 240);
    Disabled.BottomGradientColorFrom := RGB(240, 240, 240);
    Disabled.BottomGradientColorTo := RGB(234, 234, 234);
  end;
  {$endregion}

  {$region ' ----------- Green Dark ---------- '}
  with JppPngButtonColorMap_GreenDark do
  begin
    BorderWhenDefaultColor := RGB(112, 166, 79);
    FocusRectColor := RGB(112, 166, 79);
    Normal.BorderColor := RGB(108, 142, 74);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(204, 221, 189);
    Normal.UpperGradientColorFrom := RGB(227, 240, 217);
    Normal.UpperGradientColorTo := RGB(209, 230, 193);
    Normal.BottomGradientColorFrom := RGB(190, 222, 182);
    Normal.BottomGradientColorTo := RGB(163, 201, 135);
    Hot.BorderColor := RGB(140, 194, 114);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(191, 218, 175);
    Hot.UpperGradientColorFrom := RGB(227, 244, 215);
    Hot.UpperGradientColorTo := RGB(191, 224, 171);
    Hot.BottomGradientColorFrom := RGB(186, 216, 165);
    Hot.BottomGradientColorTo := RGB(149, 198, 100);
    Down.BorderColor := RGB(97, 139, 58);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(167, 204, 153);
    Down.UpperGradientColorFrom := RGB(209, 230, 196);
    Down.UpperGradientColorTo := RGB(183, 219, 155);
    Down.BottomGradientColorFrom := RGB(162, 204, 136);
    Down.BottomGradientColorTo := RGB(160, 194, 141);
    Focused.BorderColor := RGB(79, 184, 61);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(175, 209, 154);
    Focused.UpperGradientColorFrom := RGB(218, 240, 202);
    Focused.UpperGradientColorTo := RGB(181, 219, 157);
    Focused.BottomGradientColorFrom := RGB(176, 210, 153);
    Focused.BottomGradientColorTo := RGB(139, 192, 86);
    Disabled.BorderColor := RGB(188, 202, 179);
    Disabled.FontColor := RGB(160, 171, 150);
    Disabled.Color := RGB(232, 239, 228);
    Disabled.UpperGradientColorFrom := RGB(232, 239, 228);
    Disabled.UpperGradientColorTo := RGB(232, 239, 228);
    Disabled.BottomGradientColorFrom := RGB(232, 239, 228);
    Disabled.BottomGradientColorTo := RGB(232, 239, 228);
  end;
  {$endregion}

  {$region ' ---------- Green Very Dark ------------ '}
  with JppPngButtonColorMap_GreenVeryDark do
  begin
    BorderWhenDefaultColor := RGB(107, 145, 91);
    FocusRectColor := RGB(107, 145, 91);
    Normal.BorderColor := RGB(49, 60, 38);
    Normal.FontColor := RGB(199, 209, 192);
    Normal.Color := RGB(80, 96, 64);
    Normal.UpperGradientColorFrom := RGB(97, 124, 67);
    Normal.UpperGradientColorTo := RGB(69, 88, 50);
    Normal.BottomGradientColorFrom := RGB(53, 79, 49);
    Normal.BottomGradientColorTo := RGB(27, 33, 20);
    Hot.BorderColor := RGB(96, 123, 79);
    Hot.FontColor := RGB(236, 239, 233);
    Hot.Color := RGB(126, 156, 107);
    Hot.UpperGradientColorFrom := RGB(140, 166, 123);
    Hot.UpperGradientColorTo := RGB(136, 163, 118);
    Hot.BottomGradientColorFrom := RGB(126, 154, 105);
    Hot.BottomGradientColorTo := RGB(105, 130, 87);
    Down.BorderColor := RGB(101, 130, 83);
    Down.FontColor := RGB(242, 247, 240);
    Down.Color := RGB(97, 121, 81);
    Down.UpperGradientColorFrom := RGB(122, 150, 101);
    Down.UpperGradientColorTo := RGB(116, 143, 97);
    Down.BottomGradientColorFrom := RGB(104, 128, 85);
    Down.BottomGradientColorTo := RGB(84, 105, 69);
    Focused.BorderColor := RGB(107, 145, 91);
    Focused.FontColor := RGB(236, 239, 233);
    Focused.Color := RGB(126, 156, 107);
    Focused.UpperGradientColorFrom := RGB(140, 166, 123);
    Focused.UpperGradientColorTo := RGB(136, 163, 118);
    Focused.BottomGradientColorFrom := RGB(126, 154, 105);
    Focused.BottomGradientColorTo := RGB(105, 130, 87);
    Disabled.BorderColor := RGB(142, 152, 137);
    Disabled.FontColor := RGB(91, 102, 83);
    Disabled.Color := RGB(152, 157, 138);
    Disabled.UpperGradientColorFrom := RGB(138, 148, 129);
    Disabled.UpperGradientColorTo := RGB(162, 166, 149);
    Disabled.BottomGradientColorFrom := RGB(162, 166, 149);
    Disabled.BottomGradientColorTo := RGB(162, 167, 148);
  end;
  {$endregion}

  {$region ' ---------- Green ----------- '}
  with JppPngButtonColorMap_Green do
  begin
    BorderWhenDefaultColor := RGB(41, 101, 31);
    FocusRectColor := RGB(112, 166, 79);
    Normal.BorderColor := RGB(142, 182, 137);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(208, 224, 205);
    Normal.UpperGradientColorFrom := RGB(228, 238, 227);
    Normal.UpperGradientColorTo := RGB(208, 224, 205);
    Normal.BottomGradientColorFrom := RGB(197, 217, 193);
    Normal.BottomGradientColorTo := RGB(188, 211, 184);
    Hot.BorderColor := RGB(160, 198, 142);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(219, 235, 205);
    Hot.UpperGradientColorFrom := RGB(240, 247, 234);
    Hot.UpperGradientColorTo := RGB(223, 237, 211);
    Hot.BottomGradientColorFrom := RGB(207, 230, 200);
    Hot.BottomGradientColorTo := RGB(177, 209, 154);
    Down.BorderColor := RGB(125, 181, 74);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(206, 227, 198);
    Down.UpperGradientColorFrom := RGB(224, 238, 215);
    Down.UpperGradientColorTo := RGB(197, 226, 175);
    Down.BottomGradientColorFrom := RGB(190, 219, 172);
    Down.BottomGradientColorTo := RGB(181, 207, 167);
    Focused.BorderColor := RGB(112, 166, 79);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(219, 235, 205);
    Focused.UpperGradientColorFrom := RGB(240, 247, 234);
    Focused.UpperGradientColorTo := RGB(223, 237, 211);
    Focused.BottomGradientColorFrom := RGB(207, 230, 200);
    Focused.BottomGradientColorTo := RGB(177, 209, 154);
    Disabled.BorderColor := RGB(191, 204, 187);
    Disabled.FontColor := RGB(155, 170, 151);
    Disabled.Color := RGB(242, 247, 240);
    Disabled.UpperGradientColorFrom := RGB(241, 245, 239);
    Disabled.UpperGradientColorTo := RGB(232, 241, 228);
    Disabled.BottomGradientColorFrom := RGB(232, 241, 228);
    Disabled.BottomGradientColorTo := RGB(242, 247, 240);
  end;
  {$endregion}

  {$region ' ------------- High Contrast Black --------------- '}
  with JppPngButtonColorMap_HighContrastBlack do
  begin
    BorderWhenDefaultColor := RGB(255, 204, 0);
    FocusRectColor := RGB(0, 0, 0);
    Normal.BorderColor := RGB(255, 255, 255);
    Normal.FontColor := RGB(255, 255, 255);
    Normal.Color := RGB(0, 0, 0);
    Normal.UpperGradientColorFrom := RGB(30, 30, 30);
    Normal.UpperGradientColorTo := RGB(0, 0, 0);
    Normal.BottomGradientColorFrom := RGB(0, 0, 0);
    Normal.BottomGradientColorTo := RGB(30, 30, 30);
    Hot.BorderColor := RGB(255, 255, 255);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(255, 255, 255);
    Hot.UpperGradientColorFrom := RGB(255, 255, 255);
    Hot.UpperGradientColorTo := RGB(244, 244, 244);
    Hot.BottomGradientColorFrom := RGB(244, 244, 244);
    Hot.BottomGradientColorTo := RGB(233, 233, 233);
    Down.BorderColor := RGB(255, 204, 0);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(255, 204, 0);
    Down.UpperGradientColorFrom := RGB(255, 219, 72);
    Down.UpperGradientColorTo := RGB(255, 204, 0);
    Down.BottomGradientColorFrom := RGB(255, 204, 0);
    Down.BottomGradientColorTo := RGB(255, 219, 72);
    Focused.BorderColor := RGB(255, 255, 255);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(255, 255, 255);
    Focused.UpperGradientColorFrom := RGB(255, 255, 255);
    Focused.UpperGradientColorTo := RGB(244, 244, 244);
    Focused.BottomGradientColorFrom := RGB(244, 244, 244);
    Focused.BottomGradientColorTo := RGB(233, 233, 233);
    Disabled.BorderColor := RGB(128, 128, 128);
    Disabled.FontColor := RGB(165, 165, 165);
    Disabled.Color := RGB(51, 51, 51);
    Disabled.UpperGradientColorFrom := RGB(68, 68, 68);
    Disabled.UpperGradientColorTo := RGB(51, 51, 51);
    Disabled.BottomGradientColorFrom := RGB(51, 51, 51);
    Disabled.BottomGradientColorTo := RGB(68, 68, 68);
  end;
  {$endregion}

  {$region ' ------------- High Contrast White -------------- '}
  with JppPngButtonColorMap_HighContrastWhite do
  begin
    BorderWhenDefaultColor := RGB(0, 255, 255);
    FocusRectColor := RGB(0, 255, 255);
    Normal.BorderColor := RGB(0, 0, 0);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(255, 255, 255);
    Normal.UpperGradientColorFrom := RGB(255, 255, 255);
    Normal.UpperGradientColorTo := RGB(250, 250, 250);
    Normal.BottomGradientColorFrom := RGB(250, 250, 250);
    Normal.BottomGradientColorTo := RGB(255, 255, 255);
    Hot.BorderColor := RGB(0, 0, 0);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(0, 0, 0);
    Hot.UpperGradientColorFrom := RGB(30, 30, 30);
    Hot.UpperGradientColorTo := RGB(0, 0, 0);
    Hot.BottomGradientColorFrom := RGB(0, 0, 0);
    Hot.BottomGradientColorTo := RGB(30, 30, 30);
    Down.BorderColor := RGB(51, 204, 204);
    Down.FontColor := RGB(255, 255, 255);
    Down.Color := RGB(51, 204, 204);
    Down.UpperGradientColorFrom := RGB(77, 210, 210);
    Down.UpperGradientColorTo := RGB(51, 204, 204);
    Down.BottomGradientColorFrom := RGB(51, 204, 204);
    Down.BottomGradientColorTo := RGB(77, 210, 210);
    Focused.BorderColor := RGB(0, 0, 0);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(0, 0, 0);
    Focused.UpperGradientColorFrom := RGB(30, 30, 30);
    Focused.UpperGradientColorTo := RGB(0, 0, 0);
    Focused.BottomGradientColorFrom := RGB(0, 0, 0);
    Focused.BottomGradientColorTo := RGB(30, 30, 30);
    Disabled.BorderColor := RGB(165, 165, 165);
    Disabled.FontColor := RGB(128, 128, 128);
    Disabled.Color := RGB(244, 244, 244);
    Disabled.UpperGradientColorFrom := RGB(244, 244, 244);
    Disabled.UpperGradientColorTo := RGB(250, 250, 250);
    Disabled.BottomGradientColorFrom := RGB(250, 250, 250);
    Disabled.BottomGradientColorTo := RGB(244, 244, 244);
  end;
  {$endregion}





  {$region ' ------------- VCL Aqua Light Slate ------------- '}
  with JppPngButtonColorMap_VclAquaLightSlate do
  begin
    BorderWhenDefaultColor := RGB(36, 170, 208);
    FocusRectColor := RGB(122, 158, 181);
    Normal.BorderColor := RGB(156, 156, 156);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(253, 253, 253);
    Normal.UpperGradientColorFrom := RGB(249, 249, 249);
    Normal.UpperGradientColorTo := RGB(234, 234, 234);
    Normal.BottomGradientColorFrom := RGB(224, 224, 224);
    Normal.BottomGradientColorTo := RGB(244, 244, 244);
    Hot.BorderColor := RGB(123, 159, 181);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(223, 243, 250);
    Hot.UpperGradientColorFrom := RGB(188, 227, 245);
    Hot.UpperGradientColorTo := RGB(131, 201, 239);
    Hot.BottomGradientColorFrom := RGB(116, 186, 228);
    Hot.BottomGradientColorTo := RGB(133, 200, 240);
    Down.BorderColor := RGB(116, 155, 177);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(139, 193, 230);
    Down.UpperGradientColorFrom := RGB(164, 209, 237);
    Down.UpperGradientColorTo := RGB(101, 176, 227);
    Down.BottomGradientColorFrom := RGB(96, 165, 211);
    Down.BottomGradientColorTo := RGB(112, 183, 230);
    Focused.BorderColor := RGB(122, 158, 181);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(191, 237, 249);
    Focused.UpperGradientColorFrom := RGB(187, 231, 247);
    Focused.UpperGradientColorTo := RGB(116, 195, 238);
    Focused.BottomGradientColorFrom := RGB(100, 178, 226);
    Focused.BottomGradientColorTo := RGB(156, 219, 247);
    Disabled.BorderColor := RGB(177, 177, 177);
    Disabled.FontColor := RGB(147, 147, 147);
    Disabled.Color := RGB(230, 230, 230);
    Disabled.UpperGradientColorFrom := RGB(223, 223, 223);
    Disabled.UpperGradientColorTo := RGB(203, 203, 203);
    Disabled.BottomGradientColorFrom := RGB(195, 195, 195);
    Disabled.BottomGradientColorTo := RGB(204, 204, 204);
  end;
  {$endregion}

  {$region ' ------------- VCL Auric ------------- '}
  with JppPngButtonColorMap_VclAuric do
  begin
    BorderWhenDefaultColor := RGB(0, 0, 0);
    FocusRectColor := RGB(0, 0, 0);
    Normal.BorderColor := RGB(0, 0, 0);
    Normal.FontColor := RGB(255, 255, 255);
    Normal.Color := RGB(119, 119, 119);
    Normal.UpperGradientColorFrom := RGB(107, 107, 107);
    Normal.UpperGradientColorTo := RGB(96, 96, 96);
    Normal.BottomGradientColorFrom := RGB(94, 94, 94);
    Normal.BottomGradientColorTo := RGB(85, 85, 85);
    Hot.BorderColor := RGB(12, 12, 12);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(252, 223, 102);
    Hot.UpperGradientColorFrom := RGB(255, 217, 56);
    Hot.UpperGradientColorTo := RGB(255, 217, 54);
    Hot.BottomGradientColorFrom := RGB(254, 214, 53);
    Hot.BottomGradientColorTo := RGB(237, 204, 61);
    Down.BorderColor := RGB(12, 12, 12);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(189, 158, 36);
    Down.UpperGradientColorFrom := RGB(255, 217, 56);
    Down.UpperGradientColorTo := RGB(255, 217, 54);
    Down.BottomGradientColorFrom := RGB(254, 214, 53);
    Down.BottomGradientColorTo := RGB(239, 204, 60);
    Focused.BorderColor := RGB(12, 12, 12);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(252, 223, 102);
    Focused.UpperGradientColorFrom := RGB(255, 217, 56);
    Focused.UpperGradientColorTo := RGB(255, 217, 54);
    Focused.BottomGradientColorFrom := RGB(254, 214, 53);
    Focused.BottomGradientColorTo := RGB(237, 204, 61);
    Disabled.BorderColor := RGB(0, 0, 0);
    Disabled.FontColor := RGB(147, 147, 147);
    Disabled.Color := RGB(69, 69, 69);
    Disabled.UpperGradientColorFrom := RGB(57, 57, 57);
    Disabled.UpperGradientColorTo := RGB(57, 57, 57);
    Disabled.BottomGradientColorFrom := RGB(57, 57, 57);
    Disabled.BottomGradientColorTo := RGB(57, 57, 57);
  end;
  {$endregion}

  {$region ' ------------- VCL Carbon ------------- '}
  with JppPngButtonColorMap_VclCarbon do
  begin
    BorderWhenDefaultColor := RGB(36, 97, 122);
    FocusRectColor := RGB(36, 97, 122);
    Normal.BorderColor := RGB(25, 25, 25);
    Normal.FontColor := RGB(192, 192, 192);
    Normal.Color := RGB(78, 78, 78);
    Normal.UpperGradientColorFrom := RGB(67, 67, 67);
    Normal.UpperGradientColorTo := RGB(59, 59, 59);
    Normal.BottomGradientColorFrom := RGB(59, 59, 59);
    Normal.BottomGradientColorTo := RGB(49, 49, 49);
    Hot.BorderColor := RGB(25, 25, 25);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(93, 93, 93);
    Hot.UpperGradientColorFrom := RGB(71, 71, 71);
    Hot.UpperGradientColorTo := RGB(63, 63, 63);
    Hot.BottomGradientColorFrom := RGB(63, 63, 63);
    Hot.BottomGradientColorTo := RGB(53, 53, 53);
    Down.BorderColor := RGB(25, 25, 25);
    Down.FontColor := RGB(255, 255, 255);
    Down.Color := RGB(77, 77, 77);
    Down.UpperGradientColorFrom := RGB(55, 55, 55);
    Down.UpperGradientColorTo := RGB(61, 61, 61);
    Down.BottomGradientColorFrom := RGB(61, 61, 61);
    Down.BottomGradientColorTo := RGB(72, 72, 72);
    Focused.BorderColor := RGB(25, 25, 25);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(36, 97, 122);
    Focused.UpperGradientColorFrom := RGB(71, 71, 71);
    Focused.UpperGradientColorTo := RGB(63, 63, 63);
    Focused.BottomGradientColorFrom := RGB(63, 63, 63);
    Focused.BottomGradientColorTo := RGB(54, 54, 54);
    Disabled.BorderColor := RGB(44, 44, 44);
    Disabled.FontColor := RGB(128, 128, 128);
    Disabled.Color := RGB(60, 60, 60);
    Disabled.UpperGradientColorFrom := RGB(60, 60, 60);
    Disabled.UpperGradientColorTo := RGB(54, 54, 54);
    Disabled.BottomGradientColorFrom := RGB(54, 54, 54);
    Disabled.BottomGradientColorTo := RGB(48, 48, 48);
  end;
  {$endregion}

  {$region ' ------------- VCL Charcoal Dark Slate ------------- '}
  with JppPngButtonColorMap_VclCharcoalDarkSlate do
  begin
    BorderWhenDefaultColor := RGB(93, 93, 93);
    FocusRectColor := RGB(83, 83, 83);
    Normal.BorderColor := RGB(39, 39, 39);
    Normal.FontColor := RGB(163, 163, 163);
    Normal.Color := RGB(93, 93, 93);
    Normal.UpperGradientColorFrom := RGB(64, 64, 64);
    Normal.UpperGradientColorTo := RGB(40, 40, 40);
    Normal.BottomGradientColorFrom := RGB(40, 40, 40);
    Normal.BottomGradientColorTo := RGB(40, 40, 40);
    Hot.BorderColor := RGB(34, 34, 34);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(93, 93, 93);
    Hot.UpperGradientColorFrom := RGB(50, 50, 50);
    Hot.UpperGradientColorTo := RGB(31, 31, 31);
    Hot.BottomGradientColorFrom := RGB(31, 31, 31);
    Hot.BottomGradientColorTo := RGB(31, 31, 31);
    Down.BorderColor := RGB(34, 34, 34);
    Down.FontColor := RGB(255, 255, 255);
    Down.Color := RGB(65, 65, 65);
    Down.UpperGradientColorFrom := RGB(20, 20, 20);
    Down.UpperGradientColorTo := RGB(20, 20, 20);
    Down.BottomGradientColorFrom := RGB(20, 20, 20);
    Down.BottomGradientColorTo := RGB(32, 32, 32);
    Focused.BorderColor := RGB(34, 34, 34);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(83, 83, 83);
    Focused.UpperGradientColorFrom := RGB(50, 50, 50);
    Focused.UpperGradientColorTo := RGB(31, 31, 31);
    Focused.BottomGradientColorFrom := RGB(31, 31, 31);
    Focused.BottomGradientColorTo := RGB(31, 31, 31);
    Disabled.BorderColor := RGB(47, 47, 47);
    Disabled.FontColor := RGB(128, 128, 128);
    Disabled.Color := RGB(91, 91, 91);
    Disabled.UpperGradientColorFrom := RGB(76, 76, 76);
    Disabled.UpperGradientColorTo := RGB(57, 57, 57);
    Disabled.BottomGradientColorFrom := RGB(57, 57, 57);
    Disabled.BottomGradientColorTo := RGB(57, 57, 57);
  end;
  {$endregion}

  {$region ' ------------- VCL Cobalt XE Media ------------- '}
  with JppPngButtonColorMap_VclCobaltXEMedia do
  begin
    BorderWhenDefaultColor := RGB(15, 34, 66);
    FocusRectColor := RGB(15, 34, 66);
    Normal.BorderColor := RGB(17, 26, 40);
    Normal.FontColor := RGB(192, 192, 192);
    Normal.Color := RGB(45, 71, 111);
    Normal.UpperGradientColorFrom := RGB(77, 91, 115);
    Normal.UpperGradientColorTo := RGB(33, 49, 69);
    Normal.BottomGradientColorFrom := RGB(17, 28, 45);
    Normal.BottomGradientColorTo := RGB(37, 54, 82);
    Hot.BorderColor := RGB(15, 34, 66);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(35, 75, 143);
    Hot.UpperGradientColorFrom := RGB(105, 133, 175);
    Hot.UpperGradientColorTo := RGB(39, 69, 110);
    Hot.BottomGradientColorFrom := RGB(15, 37, 76);
    Hot.BottomGradientColorTo := RGB(26, 69, 149);
    Down.BorderColor := RGB(15, 34, 66);
    Down.FontColor := RGB(255, 255, 255);
    Down.Color := RGB(29, 62, 116);
    Down.UpperGradientColorFrom := RGB(69, 94, 126);
    Down.UpperGradientColorTo := RGB(27, 48, 76);
    Down.BottomGradientColorFrom := RGB(10, 25, 52);
    Down.BottomGradientColorTo := RGB(20, 70, 137);
    Focused.BorderColor := RGB(15, 34, 66);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(35, 75, 143);
    Focused.UpperGradientColorFrom := RGB(105, 133, 175);
    Focused.UpperGradientColorTo := RGB(39, 69, 110);
    Focused.BottomGradientColorFrom := RGB(15, 37, 76);
    Focused.BottomGradientColorTo := RGB(26, 69, 149);
    Disabled.BorderColor := RGB(40, 47, 53);
    Disabled.FontColor := RGB(65, 65, 65);
    Disabled.Color := RGB(23, 26, 28);
    Disabled.UpperGradientColorFrom := RGB(79, 83, 89);
    Disabled.UpperGradientColorTo := RGB(38, 41, 45);
    Disabled.BottomGradientColorFrom := RGB(22, 24, 28);
    Disabled.BottomGradientColorTo := RGB(48, 54, 65);
  end;
  {$endregion}

  {$region ' ------------- VCL Coral ------------ '}
  with JppPngButtonColorMap_VclCoral do
  begin
    BorderWhenDefaultColor := RGB(255, 255, 255);
    FocusRectColor := RGB(255, 255, 255);
    Normal.BorderColor := RGB(211, 205, 213);
    Normal.FontColor := RGB(255, 255, 255);
    Normal.Color := RGB(216, 109, 0);
    Normal.UpperGradientColorFrom := RGB(216, 109, 0);
    Normal.UpperGradientColorTo := RGB(216, 109, 0);
    Normal.BottomGradientColorFrom := RGB(216, 109, 0);
    Normal.BottomGradientColorTo := RGB(216, 109, 0);
    Hot.BorderColor := RGB(216, 109, 0);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(255, 255, 255);
    Hot.UpperGradientColorFrom := RGB(216, 109, 0);
    Hot.UpperGradientColorTo := RGB(216, 109, 0);
    Hot.BottomGradientColorFrom := RGB(216, 109, 0);
    Hot.BottomGradientColorTo := RGB(216, 109, 0);
    Down.BorderColor := RGB(255, 255, 255);
    Down.FontColor := RGB(255, 255, 255);
    Down.Color := RGB(186, 93, 0);
    Down.UpperGradientColorFrom := RGB(186, 93, 0);
    Down.UpperGradientColorTo := RGB(186, 93, 0);
    Down.BottomGradientColorFrom := RGB(186, 93, 0);
    Down.BottomGradientColorTo := RGB(186, 93, 0);
    Focused.BorderColor := RGB(216, 109, 0);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(255, 255, 255);
    Focused.UpperGradientColorFrom := RGB(216, 109, 0);
    Focused.UpperGradientColorTo := RGB(216, 109, 0);
    Focused.BottomGradientColorFrom := RGB(216, 109, 0);
    Focused.BottomGradientColorTo := RGB(216, 109, 0);
    Disabled.BorderColor := RGB(209, 209, 209);
    Disabled.FontColor := RGB(192, 192, 192);
    Disabled.Color := RGB(167, 167, 167);
    Disabled.UpperGradientColorFrom := RGB(167, 167, 167);
    Disabled.UpperGradientColorTo := RGB(167, 167, 167);
    Disabled.BottomGradientColorFrom := RGB(167, 167, 167);
    Disabled.BottomGradientColorTo := RGB(167, 167, 167);
  end;
  {$endregion}

  {$region ' ------------- VCL Emerald ---------------- '}
  with JppPngButtonColorMap_VclEmerald do
  begin
    BorderWhenDefaultColor := RGB(255, 255, 255);
    FocusRectColor := RGB(255, 255, 255);
    Normal.BorderColor := RGB(203, 203, 199);
    Normal.FontColor := RGB(255, 255, 255);
    Normal.Color := RGB(0, 165, 125);
    Normal.UpperGradientColorFrom := RGB(0, 165, 125);
    Normal.UpperGradientColorTo := RGB(0, 165, 125);
    Normal.BottomGradientColorFrom := RGB(0, 165, 125);
    Normal.BottomGradientColorTo := RGB(0, 165, 125);
    Hot.BorderColor := RGB(0, 165, 125);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(255, 255, 255);
    Hot.UpperGradientColorFrom := RGB(0, 165, 125);
    Hot.UpperGradientColorTo := RGB(0, 165, 125);
    Hot.BottomGradientColorFrom := RGB(0, 165, 125);
    Hot.BottomGradientColorTo := RGB(0, 165, 125);
    Down.BorderColor := RGB(30, 64, 89);
    Down.FontColor := RGB(255, 255, 255);
    Down.Color := RGB(0, 129, 98);
    Down.UpperGradientColorFrom := RGB(0, 129, 98);
    Down.UpperGradientColorTo := RGB(0, 129, 98);
    Down.BottomGradientColorFrom := RGB(0, 129, 98);
    Down.BottomGradientColorTo := RGB(0, 129, 98);
    Focused.BorderColor := RGB(0, 165, 125);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(255, 255, 255);
    Focused.UpperGradientColorFrom := RGB(0, 165, 125);
    Focused.UpperGradientColorTo := RGB(0, 165, 125);
    Focused.BottomGradientColorFrom := RGB(0, 165, 125);
    Focused.BottomGradientColorTo := RGB(0, 165, 125);
    Disabled.BorderColor := RGB(203, 203, 199);
    Disabled.FontColor := RGB(192, 192, 192);
    Disabled.Color := RGB(152, 152, 152);
    Disabled.UpperGradientColorFrom := RGB(152, 152, 152);
    Disabled.UpperGradientColorTo := RGB(152, 152, 152);
    Disabled.BottomGradientColorFrom := RGB(152, 152, 152);
    Disabled.BottomGradientColorTo := RGB(152, 152, 152);
  end;
  {$endregion}

  {$region ' ------------- VCL Glossy ----------------- '}
  with JppPngButtonColorMap_VclGlossy do
  begin
    BorderWhenDefaultColor := RGB(0, 0, 0);
    FocusRectColor := RGB(0, 0, 0);
    Normal.BorderColor := RGB(0, 0, 0);
    Normal.FontColor := RGB(255, 255, 255);
    Normal.Color := RGB(70, 70, 70);
    Normal.UpperGradientColorFrom := RGB(75, 75, 75);
    Normal.UpperGradientColorTo := RGB(57, 57, 57);
    Normal.BottomGradientColorFrom := RGB(17, 17, 17);
    Normal.BottomGradientColorTo := RGB(44, 44, 44);
    Hot.BorderColor := RGB(0, 0, 0);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(125, 145, 172);
    Hot.UpperGradientColorFrom := RGB(113, 136, 163);
    Hot.UpperGradientColorTo := RGB(82, 115, 158);
    Hot.BottomGradientColorFrom := RGB(38, 84, 136);
    Hot.BottomGradientColorTo := RGB(77, 144, 233);
    Down.BorderColor := RGB(0, 0, 0);
    Down.FontColor := RGB(255, 255, 255);
    Down.Color := RGB(107, 120, 141);
    Down.UpperGradientColorFrom := RGB(82, 100, 122);
    Down.UpperGradientColorTo := RGB(60, 84, 117);
    Down.BottomGradientColorFrom := RGB(23, 53, 90);
    Down.BottomGradientColorTo := RGB(55, 107, 195);
    Focused.BorderColor := RGB(0, 0, 0);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(125, 145, 172);
    Focused.UpperGradientColorFrom := RGB(113, 136, 163);
    Focused.UpperGradientColorTo := RGB(82, 115, 158);
    Focused.BottomGradientColorFrom := RGB(38, 84, 136);
    Focused.BottomGradientColorTo := RGB(77, 144, 233);
    Disabled.BorderColor := RGB(28, 28, 28);
    Disabled.FontColor := RGB(128, 128, 128);
    Disabled.Color := RGB(50, 50, 50);
    Disabled.UpperGradientColorFrom := RGB(48, 48, 48);
    Disabled.UpperGradientColorTo := RGB(41, 41, 41);
    Disabled.BottomGradientColorFrom := RGB(31, 31, 31);
    Disabled.BottomGradientColorTo := RGB(37, 37, 37);
  end;
  {$endregion}

  {$region ' ------------- VCL Golden Graphite ---------------- '}
  with JppPngButtonColorMap_VclGoldenGraphite do
  begin
    BorderWhenDefaultColor := RGB(92, 62, 4);
    FocusRectColor := RGB(92, 62, 4);
    Normal.BorderColor := RGB(92, 62, 4);
    Normal.FontColor := RGB(234, 234, 234);
    Normal.Color := RGB(248, 193, 88);
    Normal.UpperGradientColorFrom := RGB(233, 173, 39);
    Normal.UpperGradientColorTo := RGB(179, 131, 0);
    Normal.BottomGradientColorFrom := RGB(179, 131, 0);
    Normal.BottomGradientColorTo := RGB(191, 138, 0);
    Hot.BorderColor := RGB(92, 62, 4);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(255, 217, 138);
    Hot.UpperGradientColorFrom := RGB(237, 188, 79);
    Hot.UpperGradientColorTo := RGB(179, 131, 0);
    Hot.BottomGradientColorFrom := RGB(179, 131, 0);
    Hot.BottomGradientColorTo := RGB(191, 138, 0);
    Down.BorderColor := RGB(92, 62, 4);
    Down.FontColor := RGB(255, 255, 255);
    Down.Color := RGB(228, 170, 0);
    Down.UpperGradientColorFrom := RGB(176, 129, 0);
    Down.UpperGradientColorTo := RGB(190, 136, 0);
    Down.BottomGradientColorFrom := RGB(190, 136, 0);
    Down.BottomGradientColorTo := RGB(222, 153, 0);
    Focused.BorderColor := RGB(92, 62, 4);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(248, 211, 138);
    Focused.UpperGradientColorFrom := RGB(229, 179, 69);
    Focused.UpperGradientColorTo := RGB(173, 126, 0);
    Focused.BottomGradientColorFrom := RGB(168, 123, 0);
    Focused.BottomGradientColorTo := RGB(183, 135, 10);
    Disabled.BorderColor := RGB(45, 45, 45);
    Disabled.FontColor := RGB(147, 147, 147);
    Disabled.Color := RGB(140, 140, 140);
    Disabled.UpperGradientColorFrom := RGB(136, 136, 136);
    Disabled.UpperGradientColorTo := RGB(89, 89, 89);
    Disabled.BottomGradientColorFrom := RGB(89, 89, 89);
    Disabled.BottomGradientColorTo := RGB(103, 103, 103);
  end;
  {$endregion}

  {$region ' ------------- VCL Iceberg Classico --------------- '}
  with JppPngButtonColorMap_VclIcebergClassico do
  begin
    BorderWhenDefaultColor := RGB(50, 155, 211);
    FocusRectColor := RGB(126, 159, 185);
    Normal.BorderColor := RGB(148, 175, 194);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(245, 245, 245);
    Normal.UpperGradientColorFrom := RGB(253, 253, 253);
    Normal.UpperGradientColorTo := RGB(231, 231, 231);
    Normal.BottomGradientColorFrom := RGB(227, 227, 227);
    Normal.BottomGradientColorTo := RGB(233, 233, 233);
    Hot.BorderColor := RGB(126, 159, 185);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(185, 217, 234);
    Hot.UpperGradientColorFrom := RGB(185, 218, 233);
    Hot.UpperGradientColorTo := RGB(155, 197, 221);
    Hot.BottomGradientColorFrom := RGB(153, 195, 220);
    Hot.BottomGradientColorTo := RGB(169, 205, 226);
    Down.BorderColor := RGB(126, 159, 185);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(197, 226, 241);
    Down.UpperGradientColorFrom := RGB(153, 196, 220);
    Down.UpperGradientColorTo := RGB(162, 200, 222);
    Down.BottomGradientColorFrom := RGB(165, 204, 226);
    Down.BottomGradientColorTo := RGB(186, 219, 233);
    Focused.BorderColor := RGB(126, 159, 185);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(193, 225, 238);
    Focused.UpperGradientColorFrom := RGB(185, 218, 233);
    Focused.UpperGradientColorTo := RGB(152, 194, 219);
    Focused.BottomGradientColorFrom := RGB(152, 194, 219);
    Focused.BottomGradientColorTo := RGB(169, 205, 226);
    Disabled.BorderColor := RGB(161, 161, 161);
    Disabled.FontColor := RGB(147, 147, 147);
    Disabled.Color := RGB(233, 233, 233);
    Disabled.UpperGradientColorFrom := RGB(238, 238, 238);
    Disabled.UpperGradientColorTo := RGB(217, 217, 217);
    Disabled.BottomGradientColorFrom := RGB(217, 217, 217);
    Disabled.BottomGradientColorTo := RGB(219, 219, 219);
  end;
  {$endregion}

  {$region ' ------------- VCL Jet ----------------- '}
  with JppPngButtonColorMap_VclJet do
  begin
    BorderWhenDefaultColor := RGB(50, 50, 50);
    FocusRectColor := RGB(50, 50, 50);
    Normal.BorderColor := RGB(42, 42, 42);
    Normal.FontColor := RGB(255, 255, 255);
    Normal.Color := RGB(52, 52, 52);
    Normal.UpperGradientColorFrom := RGB(59, 59, 59);
    Normal.UpperGradientColorTo := RGB(49, 49, 49);
    Normal.BottomGradientColorFrom := RGB(48, 48, 48);
    Normal.BottomGradientColorTo := RGB(37, 37, 37);
    Hot.BorderColor := RGB(50, 50, 50);
    Hot.FontColor := RGB(206, 233, 248);
    Hot.Color := RGB(62, 62, 62);
    Hot.UpperGradientColorFrom := RGB(77, 77, 77);
    Hot.UpperGradientColorTo := RGB(61, 61, 61);
    Hot.BottomGradientColorFrom := RGB(60, 60, 60);
    Hot.BottomGradientColorTo := RGB(41, 41, 41);
    Down.BorderColor := RGB(22, 22, 22);
    Down.FontColor := RGB(206, 233, 248);
    Down.Color := RGB(14, 14, 14);
    Down.UpperGradientColorFrom := RGB(29, 29, 29);
    Down.UpperGradientColorTo := RGB(40, 40, 40);
    Down.BottomGradientColorFrom := RGB(40, 40, 40);
    Down.BottomGradientColorTo := RGB(33, 33, 33);
    Focused.BorderColor := RGB(42, 42, 42);
    Focused.FontColor := RGB(206, 233, 248);
    Focused.Color := RGB(62, 95, 104);
    Focused.UpperGradientColorFrom := RGB(59, 59, 59);
    Focused.UpperGradientColorTo := RGB(51, 51, 51);
    Focused.BottomGradientColorFrom := RGB(50, 50, 50);
    Focused.BottomGradientColorTo := RGB(37, 37, 37);
    Disabled.BorderColor := RGB(42, 42, 42);
    Disabled.FontColor := RGB(83, 83, 83);
    Disabled.Color := RGB(52, 52, 52);
    Disabled.UpperGradientColorFrom := RGB(59, 59, 59);
    Disabled.UpperGradientColorTo := RGB(49, 49, 49);
    Disabled.BottomGradientColorFrom := RGB(48, 48, 48);
    Disabled.BottomGradientColorTo := RGB(37, 37, 37);
  end;
  {$endregion}

  {$region ' ------------- VCL Light ------------ '}
  with JppPngButtonColorMap_VclLight do
  begin
    BorderWhenDefaultColor := RGB(162, 199, 228);
    FocusRectColor := RGB(162, 199, 228);
    Normal.BorderColor := RGB(171, 171, 171);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(253, 253, 253);
    Normal.UpperGradientColorFrom := RGB(253, 253, 253);
    Normal.UpperGradientColorTo := RGB(253, 253, 253);
    Normal.BottomGradientColorFrom := RGB(253, 253, 253);
    Normal.BottomGradientColorTo := RGB(253, 253, 253);
    Hot.BorderColor := RGB(162, 199, 228);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(212, 230, 243);
    Hot.UpperGradientColorFrom := RGB(212, 230, 243);
    Hot.UpperGradientColorTo := RGB(212, 230, 243);
    Hot.BottomGradientColorFrom := RGB(212, 230, 243);
    Hot.BottomGradientColorTo := RGB(212, 230, 243);
    Down.BorderColor := RGB(59, 127, 183);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(162, 199, 228);
    Down.UpperGradientColorFrom := RGB(162, 199, 228);
    Down.UpperGradientColorTo := RGB(162, 199, 228);
    Down.BottomGradientColorFrom := RGB(162, 199, 228);
    Down.BottomGradientColorTo := RGB(162, 199, 228);
    Focused.BorderColor := RGB(162, 199, 228);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(212, 230, 243);
    Focused.UpperGradientColorFrom := RGB(212, 230, 243);
    Focused.UpperGradientColorTo := RGB(212, 230, 243);
    Focused.BottomGradientColorFrom := RGB(212, 230, 243);
    Focused.BottomGradientColorTo := RGB(212, 230, 243);
    Disabled.BorderColor := RGB(205, 205, 205);
    Disabled.FontColor := RGB(147, 147, 147);
    Disabled.Color := RGB(253, 253, 253);
    Disabled.UpperGradientColorFrom := RGB(253, 253, 253);
    Disabled.UpperGradientColorTo := RGB(253, 253, 253);
    Disabled.BottomGradientColorFrom := RGB(253, 253, 253);
    Disabled.BottomGradientColorTo := RGB(253, 253, 253);
  end;
  {$endregion}

  {$region ' ------------- VCL Luna --------------- '}
  with JppPngButtonColorMap_VclLuna do
  begin
    BorderWhenDefaultColor := RGB(19, 143, 236);
    FocusRectColor := RGB(153, 181, 222);
    Normal.BorderColor := RGB(153, 181, 222);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(218, 231, 244);
    Normal.UpperGradientColorFrom := RGB(201, 220, 240);
    Normal.UpperGradientColorTo := RGB(201, 221, 246);
    Normal.BottomGradientColorFrom := RGB(189, 209, 234);
    Normal.BottomGradientColorTo := RGB(204, 222, 244);
    Hot.BorderColor := RGB(153, 181, 222);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(255, 247, 215);
    Hot.UpperGradientColorFrom := RGB(254, 247, 213);
    Hot.UpperGradientColorTo := RGB(250, 231, 174);
    Hot.BottomGradientColorFrom := RGB(255, 208, 72);
    Hot.BottomGradientColorTo := RGB(255, 228, 155);
    Down.BorderColor := RGB(153, 181, 222);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(213, 155, 105);
    Down.UpperGradientColorFrom := RGB(247, 193, 113);
    Down.UpperGradientColorTo := RGB(239, 180, 93);
    Down.BottomGradientColorFrom := RGB(236, 159, 52);
    Down.BottomGradientColorTo := RGB(240, 173, 64);
    Focused.BorderColor := RGB(153, 181, 222);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(255, 247, 215);
    Focused.UpperGradientColorFrom := RGB(254, 247, 213);
    Focused.UpperGradientColorTo := RGB(250, 231, 174);
    Focused.BottomGradientColorFrom := RGB(255, 208, 72);
    Focused.BottomGradientColorTo := RGB(255, 228, 155);
    Disabled.BorderColor := RGB(182, 202, 231);
    Disabled.FontColor := RGB(147, 147, 147);
    Disabled.Color := RGB(229, 238, 247);
    Disabled.UpperGradientColorFrom := RGB(216, 230, 244);
    Disabled.UpperGradientColorTo := RGB(216, 231, 249);
    Disabled.BottomGradientColorFrom := RGB(207, 221, 239);
    Disabled.BottomGradientColorTo := RGB(219, 231, 247);
  end;
  {$endregion}

  {$region ' ------------- VCL Metropolis UI Black ------------- '}
  with JppPngButtonColorMap_VclMetropolisUIBlack do
  begin
    BorderWhenDefaultColor := RGB(255, 255, 255);
    FocusRectColor := RGB(255, 255, 255);
    Normal.BorderColor := RGB(255, 255, 255);
    Normal.FontColor := RGB(192, 192, 192);
    Normal.Color := RGB(26, 26, 26);
    Normal.UpperGradientColorFrom := RGB(26, 26, 26);
    Normal.UpperGradientColorTo := RGB(26, 26, 26);
    Normal.BottomGradientColorFrom := RGB(26, 26, 26);
    Normal.BottomGradientColorTo := RGB(26, 26, 26);
    Hot.BorderColor := RGB(245, 249, 251);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(19, 115, 169);
    Hot.UpperGradientColorFrom := RGB(19, 115, 169);
    Hot.UpperGradientColorTo := RGB(19, 115, 169);
    Hot.BottomGradientColorFrom := RGB(19, 115, 169);
    Hot.BottomGradientColorTo := RGB(19, 115, 169);
    Down.BorderColor := RGB(255, 255, 255);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(255, 255, 255);
    Down.UpperGradientColorFrom := RGB(255, 255, 255);
    Down.UpperGradientColorTo := RGB(255, 255, 255);
    Down.BottomGradientColorFrom := RGB(255, 255, 255);
    Down.BottomGradientColorTo := RGB(255, 255, 255);
    Focused.BorderColor := RGB(245, 249, 251);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(19, 115, 169);
    Focused.UpperGradientColorFrom := RGB(19, 115, 169);
    Focused.UpperGradientColorTo := RGB(19, 115, 169);
    Focused.BottomGradientColorFrom := RGB(19, 115, 169);
    Focused.BottomGradientColorTo := RGB(19, 115, 169);
    Disabled.BorderColor := RGB(200, 204, 206);
    Disabled.FontColor := RGB(126, 126, 126);
    Disabled.Color := RGB(26, 26, 26);
    Disabled.UpperGradientColorFrom := RGB(26, 26, 26);
    Disabled.UpperGradientColorTo := RGB(26, 26, 26);
    Disabled.BottomGradientColorFrom := RGB(26, 26, 26);
    Disabled.BottomGradientColorTo := RGB(26, 26, 26);
  end;
  {$endregion}

  {$region ' ------------- VCL Metropolis UI Dark ------------------ '}
  with JppPngButtonColorMap_VclMetropolisUIDark do
  begin
    BorderWhenDefaultColor := RGB(255, 255, 255);
    FocusRectColor := RGB(255, 255, 255);
    Normal.BorderColor := RGB(255, 255, 255);
    Normal.FontColor := RGB(192, 192, 192);
    Normal.Color := RGB(26, 26, 26);
    Normal.UpperGradientColorFrom := RGB(26, 26, 26);
    Normal.UpperGradientColorTo := RGB(26, 26, 26);
    Normal.BottomGradientColorFrom := RGB(26, 26, 26);
    Normal.BottomGradientColorTo := RGB(26, 26, 26);
    Hot.BorderColor := RGB(248, 248, 248);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(94, 94, 94);
    Hot.UpperGradientColorFrom := RGB(94, 94, 94);
    Hot.UpperGradientColorTo := RGB(94, 94, 94);
    Hot.BottomGradientColorFrom := RGB(94, 94, 94);
    Hot.BottomGradientColorTo := RGB(94, 94, 94);
    Down.BorderColor := RGB(255, 255, 255);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(255, 255, 255);
    Down.UpperGradientColorFrom := RGB(255, 255, 255);
    Down.UpperGradientColorTo := RGB(255, 255, 255);
    Down.BottomGradientColorFrom := RGB(255, 255, 255);
    Down.BottomGradientColorTo := RGB(255, 255, 255);
    Focused.BorderColor := RGB(248, 248, 248);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(94, 94, 94);
    Focused.UpperGradientColorFrom := RGB(94, 94, 94);
    Focused.UpperGradientColorTo := RGB(94, 94, 94);
    Focused.BottomGradientColorFrom := RGB(94, 94, 94);
    Focused.BottomGradientColorTo := RGB(94, 94, 94);
    Disabled.BorderColor := RGB(203, 203, 203);
    Disabled.FontColor := RGB(126, 126, 126);
    Disabled.Color := RGB(26, 26, 26);
    Disabled.UpperGradientColorFrom := RGB(26, 26, 26);
    Disabled.UpperGradientColorTo := RGB(26, 26, 26);
    Disabled.BottomGradientColorFrom := RGB(26, 26, 26);
    Disabled.BottomGradientColorTo := RGB(26, 26, 26);
  end;
  {$endregion}

  {$region ' ------------- VCL Metropolis UI Blue --------------- '}
  with JppPngButtonColorMap_VclMetropolisUIBlue do
  begin
    BorderWhenDefaultColor := RGB(255, 255, 255);
    FocusRectColor := RGB(255, 255, 255);
    Normal.BorderColor := RGB(255, 255, 255);
    Normal.FontColor := RGB(255, 255, 255);
    Normal.Color := RGB(19, 115, 169);
    Normal.UpperGradientColorFrom := RGB(19, 115, 169);
    Normal.UpperGradientColorTo := RGB(19, 115, 169);
    Normal.BottomGradientColorFrom := RGB(19, 115, 169);
    Normal.BottomGradientColorTo := RGB(19, 115, 169);
    Hot.BorderColor := RGB(255, 255, 255);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(232, 102, 37);
    Hot.UpperGradientColorFrom := RGB(232, 102, 37);
    Hot.UpperGradientColorTo := RGB(232, 102, 37);
    Hot.BottomGradientColorFrom := RGB(232, 102, 37);
    Hot.BottomGradientColorTo := RGB(232, 102, 37);
    Down.BorderColor := RGB(255, 255, 255);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(255, 255, 255);
    Down.UpperGradientColorFrom := RGB(255, 255, 255);
    Down.UpperGradientColorTo := RGB(255, 255, 255);
    Down.BottomGradientColorFrom := RGB(255, 255, 255);
    Down.BottomGradientColorTo := RGB(255, 255, 255);
    Focused.BorderColor := RGB(255, 255, 255);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(232, 102, 37);
    Focused.UpperGradientColorFrom := RGB(232, 102, 37);
    Focused.UpperGradientColorTo := RGB(232, 102, 37);
    Focused.BottomGradientColorFrom := RGB(232, 102, 37);
    Focused.BottomGradientColorTo := RGB(232, 102, 37);
    Disabled.BorderColor := RGB(169, 199, 221);
    Disabled.FontColor := RGB(13, 82, 123);
    Disabled.Color := RGB(19, 115, 169);
    Disabled.UpperGradientColorFrom := RGB(19, 115, 169);
    Disabled.UpperGradientColorTo := RGB(19, 115, 169);
    Disabled.BottomGradientColorFrom := RGB(19, 115, 169);
    Disabled.BottomGradientColorTo := RGB(19, 115, 169);
  end;
  {$endregion}

  {$region ' ------------- VCL Metropolis UI Green ----------------- '}
  with JppPngButtonColorMap_VclMetropolisUIGreen do
  begin
    BorderWhenDefaultColor := RGB(255, 255, 255);
    FocusRectColor := RGB(255, 255, 255);
    Normal.BorderColor := RGB(255, 255, 255);
    Normal.FontColor := RGB(255, 255, 255);
    Normal.Color := RGB(9, 108, 55);
    Normal.UpperGradientColorFrom := RGB(9, 108, 55);
    Normal.UpperGradientColorTo := RGB(9, 108, 55);
    Normal.BottomGradientColorFrom := RGB(9, 108, 55);
    Normal.BottomGradientColorTo := RGB(9, 108, 55);
    Hot.BorderColor := RGB(255, 255, 255);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(36, 124, 77);
    Hot.UpperGradientColorFrom := RGB(36, 124, 77);
    Hot.UpperGradientColorTo := RGB(36, 124, 77);
    Hot.BottomGradientColorFrom := RGB(36, 124, 77);
    Hot.BottomGradientColorTo := RGB(36, 124, 77);
    Down.BorderColor := RGB(255, 255, 255);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(255, 255, 255);
    Down.UpperGradientColorFrom := RGB(255, 255, 255);
    Down.UpperGradientColorTo := RGB(255, 255, 255);
    Down.BottomGradientColorFrom := RGB(255, 255, 255);
    Down.BottomGradientColorTo := RGB(255, 255, 255);
    Focused.BorderColor := RGB(255, 255, 255);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(36, 124, 77);
    Focused.UpperGradientColorFrom := RGB(36, 124, 77);
    Focused.UpperGradientColorTo := RGB(36, 124, 77);
    Focused.BottomGradientColorFrom := RGB(36, 124, 77);
    Focused.BottomGradientColorTo := RGB(36, 124, 77);
    Disabled.BorderColor := RGB(132, 182, 155);
    Disabled.FontColor := RGB(88, 155, 119);
    Disabled.Color := RGB(9, 108, 55);
    Disabled.UpperGradientColorFrom := RGB(9, 108, 55);
    Disabled.UpperGradientColorTo := RGB(9, 108, 55);
    Disabled.BottomGradientColorFrom := RGB(9, 108, 55);
    Disabled.BottomGradientColorTo := RGB(9, 108, 55);
  end;
  {$endregion}

  {$region ' ------------- VCL Ruby Graphite -------------- '}
  with JppPngButtonColorMap_VclRubyGraphite do
  begin
    BorderWhenDefaultColor := RGB(92, 4, 4);
    FocusRectColor := RGB(92, 4, 4);
    Normal.BorderColor := RGB(92, 4, 4);
    Normal.FontColor := RGB(234, 234, 234);
    Normal.Color := RGB(249, 104, 104);
    Normal.UpperGradientColorFrom := RGB(233, 44, 39);
    Normal.UpperGradientColorTo := RGB(179, 12, 0);
    Normal.BottomGradientColorFrom := RGB(179, 12, 0);
    Normal.BottomGradientColorTo := RGB(206, 10, 0);
    Hot.BorderColor := RGB(92, 4, 4);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(255, 100, 100);
    Hot.UpperGradientColorFrom := RGB(237, 83, 79);
    Hot.UpperGradientColorTo := RGB(179, 12, 0);
    Hot.BottomGradientColorFrom := RGB(179, 12, 0);
    Hot.BottomGradientColorTo := RGB(206, 10, 0);
    Down.BorderColor := RGB(92, 4, 4);
    Down.FontColor := RGB(255, 255, 255);
    Down.Color := RGB(228, 19, 0);
    Down.UpperGradientColorFrom := RGB(176, 12, 0);
    Down.UpperGradientColorTo := RGB(190, 10, 0);
    Down.BottomGradientColorFrom := RGB(190, 10, 0);
    Down.BottomGradientColorTo := RGB(231, 9, 0);
    Focused.BorderColor := RGB(92, 4, 4);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(249, 157, 157);
    Focused.UpperGradientColorFrom := RGB(229, 73, 69);
    Focused.UpperGradientColorTo := RGB(168, 11, 0);
    Focused.BottomGradientColorFrom := RGB(168, 11, 0);
    Focused.BottomGradientColorTo := RGB(183, 20, 10);
    Disabled.BorderColor := RGB(45, 45, 45);
    Disabled.FontColor := RGB(147, 147, 147);
    Disabled.Color := RGB(140, 140, 140);
    Disabled.UpperGradientColorFrom := RGB(136, 136, 136);
    Disabled.UpperGradientColorTo := RGB(89, 89, 89);
    Disabled.BottomGradientColorFrom := RGB(89, 89, 89);
    Disabled.BottomGradientColorTo := RGB(95, 95, 95);
  end;
  {$endregion}

  {$region ' ------------- VCL Silver ---------------- '}
  with JppPngButtonColorMap_VclSilver do
  begin
    BorderWhenDefaultColor := RGB(185, 190, 200);
    FocusRectColor := RGB(185, 190, 200);
    Normal.BorderColor := RGB(185, 190, 200);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(241, 241, 241);
    Normal.UpperGradientColorFrom := RGB(233, 236, 239);
    Normal.UpperGradientColorTo := RGB(228, 231, 236);
    Normal.BottomGradientColorFrom := RGB(222, 226, 231);
    Normal.BottomGradientColorTo := RGB(229, 231, 235);
    Hot.BorderColor := RGB(185, 190, 200);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(243, 241, 233);
    Hot.UpperGradientColorFrom := RGB(241, 239, 228);
    Hot.UpperGradientColorTo := RGB(237, 225, 189);
    Hot.BottomGradientColorFrom := RGB(255, 208, 72);
    Hot.BottomGradientColorTo := RGB(246, 224, 165);
    Down.BorderColor := RGB(185, 190, 200);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(213, 179, 151);
    Down.UpperGradientColorFrom := RGB(242, 193, 119);
    Down.UpperGradientColorTo := RGB(238, 181, 96);
    Down.BottomGradientColorFrom := RGB(236, 159, 52);
    Down.BottomGradientColorTo := RGB(242, 179, 64);
    Focused.BorderColor := RGB(185, 190, 200);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(243, 241, 233);
    Focused.UpperGradientColorFrom := RGB(241, 239, 228);
    Focused.UpperGradientColorTo := RGB(237, 225, 189);
    Focused.BottomGradientColorFrom := RGB(255, 208, 72);
    Focused.BottomGradientColorTo := RGB(246, 224, 165);
    Disabled.BorderColor := RGB(207, 210, 214);
    Disabled.FontColor := RGB(147, 147, 147);
    Disabled.Color := RGB(237, 239, 241);
    Disabled.UpperGradientColorFrom := RGB(229, 231, 234);
    Disabled.UpperGradientColorTo := RGB(231, 233, 236);
    Disabled.BottomGradientColorFrom := RGB(224, 226, 229);
    Disabled.BottomGradientColorTo := RGB(232, 234, 237);
  end;
  {$endregion}

  {$region ' ------------- VCL Slate Classico ------------- '}
  with JppPngButtonColorMap_VclSlateClassico do
  begin
    BorderWhenDefaultColor := RGB(67, 158, 233);
    FocusRectColor := RGB(120, 150, 178);
    Normal.BorderColor := RGB(171, 171, 171);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(245, 245, 245);
    Normal.UpperGradientColorFrom := RGB(253, 253, 253);
    Normal.UpperGradientColorTo := RGB(231, 231, 231);
    Normal.BottomGradientColorFrom := RGB(231, 231, 231);
    Normal.BottomGradientColorTo := RGB(233, 233, 233);
    Hot.BorderColor := RGB(120, 150, 178);
    Hot.FontColor := RGB(0, 0, 0);
    Hot.Color := RGB(202, 227, 240);
    Hot.UpperGradientColorFrom := RGB(200, 227, 240);
    Hot.UpperGradientColorTo := RGB(154, 197, 226);
    Hot.BottomGradientColorFrom := RGB(154, 197, 226);
    Hot.BottomGradientColorTo := RGB(182, 216, 235);
    Down.BorderColor := RGB(120, 150, 178);
    Down.FontColor := RGB(0, 0, 0);
    Down.Color := RGB(193, 221, 238);
    Down.UpperGradientColorFrom := RGB(159, 201, 227);
    Down.UpperGradientColorTo := RGB(160, 201, 227);
    Down.BottomGradientColorFrom := RGB(160, 201, 227);
    Down.BottomGradientColorTo := RGB(202, 228, 241);
    Focused.BorderColor := RGB(120, 150, 178);
    Focused.FontColor := RGB(0, 0, 0);
    Focused.Color := RGB(202, 227, 240);
    Focused.UpperGradientColorFrom := RGB(200, 227, 240);
    Focused.UpperGradientColorTo := RGB(154, 197, 226);
    Focused.BottomGradientColorFrom := RGB(154, 197, 226);
    Focused.BottomGradientColorTo := RGB(182, 216, 235);
    Disabled.BorderColor := RGB(161, 161, 161);
    Disabled.FontColor := RGB(128, 128, 128);
    Disabled.Color := RGB(230, 230, 230);
    Disabled.UpperGradientColorFrom := RGB(238, 238, 238);
    Disabled.UpperGradientColorTo := RGB(212, 212, 212);
    Disabled.BottomGradientColorFrom := RGB(212, 212, 212);
    Disabled.BottomGradientColorTo := RGB(219, 219, 219);
  end;
  {$endregion}

  {$region ' ------------- VCL Turquoise Gray --------------- '}
  with JppPngButtonColorMap_VclTurquoiseGray do
  begin
    BorderWhenDefaultColor := RGB(0, 154, 196);
    FocusRectColor := RGB(0, 154, 196);
    Normal.BorderColor := RGB(211, 211, 211);
    Normal.FontColor := RGB(0, 0, 0);
    Normal.Color := RGB(244, 244, 244);
    Normal.UpperGradientColorFrom := RGB(244, 244, 244);
    Normal.UpperGradientColorTo := RGB(238, 238, 238);
    Normal.BottomGradientColorFrom := RGB(238, 238, 238);
    Normal.BottomGradientColorTo := RGB(232, 232, 232);
    Hot.BorderColor := RGB(0, 154, 196);
    Hot.FontColor := RGB(255, 255, 255);
    Hot.Color := RGB(71, 200, 234);
    Hot.UpperGradientColorFrom := RGB(69, 199, 234);
    Hot.UpperGradientColorTo := RGB(44, 193, 233);
    Hot.BottomGradientColorFrom := RGB(44, 193, 233);
    Hot.BottomGradientColorTo := RGB(14, 186, 232);
    Down.BorderColor := RGB(0, 154, 196);
    Down.FontColor := RGB(255, 255, 255);
    Down.Color := RGB(9, 175, 220);
    Down.UpperGradientColorFrom := RGB(11, 185, 232);
    Down.UpperGradientColorTo := RGB(36, 191, 233);
    Down.BottomGradientColorFrom := RGB(36, 191, 233);
    Down.BottomGradientColorTo := RGB(66, 198, 234);
    Focused.BorderColor := RGB(0, 154, 196);
    Focused.FontColor := RGB(255, 255, 255);
    Focused.Color := RGB(71, 200, 234);
    Focused.UpperGradientColorFrom := RGB(69, 199, 234);
    Focused.UpperGradientColorTo := RGB(44, 193, 233);
    Focused.BottomGradientColorFrom := RGB(44, 193, 233);
    Focused.BottomGradientColorTo := RGB(14, 186, 232);
    Disabled.BorderColor := RGB(229, 229, 229);
    Disabled.FontColor := RGB(147, 147, 147);
    Disabled.Color := RGB(244, 244, 244);
    Disabled.UpperGradientColorFrom := RGB(244, 244, 244);
    Disabled.UpperGradientColorTo := RGB(237, 237, 237);
    Disabled.BottomGradientColorFrom := RGB(237, 237, 237);
    Disabled.BottomGradientColorTo := RGB(232, 232, 232);
  end;
  {$endregion}


{$endregion}

end.
