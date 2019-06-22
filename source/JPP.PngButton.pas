unit JPP.PngButton;

{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface

uses
  {$IFDEF DCC}
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.UITypes, System.StrUtils,
  Vcl.Controls, Vcl.Graphics,  Vcl.StdCtrls, Vcl.Buttons, Vcl.GraphUtil, Vcl.ActnList, Vcl.Imaging.pngimage,
  PngFunctions,
  {$ELSE}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, Controls, Graphics, StdCtrls, ActnList, StrUtils, LazPngFunctions, Buttons, LCLType, LCLIntf, LMessages, Messages,
  {$ENDIF}

  JPL.Colors, JPL.Strings,
  JPP.Types, JPP.Common, JPP.Common.Procs, JPP.Graphics, JPP.Gradient, JPP.PngButton.ColorMaps
  ;


type
  {$IFDEF FPC}
  {$IFDEF UNIX}TWMDrawItem = TLMDrawItems;{$ENDIF}
  {$ENDIF}


  {$region ' -------------- TJppItemStateParams --------------------- '}
  TJppItemStateParams = class(TPersistent)
  private
    FColor: TColor;
    FFont: TFont;
    FBorder: TPen;
    FGradientEnabled: Boolean;
    FUpperGradient: TJppGradientEx;
    FBottomGradient: TJppGradientEx;
    FUpperGradientPercent: Byte;
    FBorderToGradientMargin: integer;
    FTransparentBackground: Boolean;
    FTransparentFrame: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetBorder(const Value: TPen);
    procedure SetGradientEnabled(const Value: Boolean);
    procedure SetUpperGradient(const Value: TJppGradientEx);
    procedure SetBottomGradient(const Value: TJppGradientEx);
    procedure SetUpperGradientPercent(const Value: Byte);
    procedure SetBorderToGradientMargin(const Value: integer);
    procedure SetTransparentBackground(const Value: Boolean);
    procedure SetTransparentFrame(const Value: Boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  protected
    procedure PropsChanged(Sender: TObject);
  published
    property Border: TPen read FBorder write SetBorder;
    property Color: TColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;
    property UpperGradient: TJppGradientEx read FUpperGradient write SetUpperGradient;
    property BottomGradient: TJppGradientEx read FBottomGradient write SetBottomGradient;
    property GradientEnabled: Boolean read FGradientEnabled write SetGradientEnabled default True;
    property UpperGradientPercent: Byte read FUpperGradientPercent write SetUpperGradientPercent;
    property BorderToGradientMargin: integer read FBorderToGradientMargin write SetBorderToGradientMargin default 2;
    property TransparentBackground: Boolean read FTransparentBackground write SetTransparentBackground default False;
    property TransparentFrame: Boolean read FTransparentFrame write SetTransparentFrame default False;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}


  {$region ' ----------- TJppButtonAppearance ---------- '}
  TJppButtonAppearance = class(TPersistent)
  private
    FNormal: TJppItemStateParams;
    FHot: TJppItemStateParams;
    FDefaultDrawing: Boolean;
    FFocusRect: TJppFocusRectParams;
    FBorderWhenDefault: TPen;
    FDown: TJppItemStateParams;
    FDisabled: TJppItemStateParams;
    FGlyphDisabledGrayscaleFactor: Byte;
    FGlyphDisabledBlendFactor: Byte;
    FMoveWhenDown: Boolean;
    FGlyphHotGammaFactor: Byte;
    FOnChange: TNotifyEvent;
    FFocused: TJppItemStateParams;
    FShowCaption: Boolean;
    procedure SetNormal(const Value: TJppItemStateParams);
    procedure SetHot(const Value: TJppItemStateParams);
    procedure SetDefaultDrawing(const Value: Boolean);
    procedure SetFocusRect(const Value: TJppFocusRectParams);
    procedure SetBorderWhenDefault(const Value: TPen);
    procedure SetDown(const Value: TJppItemStateParams);
    procedure SetDisabled(const Value: TJppItemStateParams);
    procedure SetGlyphDisabledGrayscaleFactor(const Value: Byte);
    procedure SetGlyphDisabledBlendFactor(const Value: Byte);
    procedure SetMoveWhenDown(const Value: Boolean);
    procedure SetGlyphHotGammaFactor(const Value: Byte);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetFocused(const Value: TJppItemStateParams);
    procedure SetShowCaption(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppButtonAppearance); reintroduce;
    procedure PropsChanged(Sender: TObject);
  published
    property DefaultDrawing: Boolean read FDefaultDrawing write SetDefaultDrawing default False;
    property Normal: TJppItemStateParams read FNormal write SetNormal;
    property Hot: TJppItemStateParams read FHot write SetHot;
    property Down: TJppItemStateParams read FDown write SetDown;
    property Disabled: TJppItemStateParams read FDisabled write SetDisabled;
    property Focused: TJppItemStateParams read FFocused write SetFocused;
    property FocusRect: TJppFocusRectParams read FFocusRect write SetFocusRect;
    property BorderWhenDefault: TPen read FBorderWhenDefault write SetBorderWhenDefault;
    property GlyphDisabledGrayscaleFactor: Byte read FGlyphDisabledGrayscaleFactor write SetGlyphDisabledGrayscaleFactor default 255;
    property GlyphDisabledBlendFactor: Byte read FGlyphDisabledBlendFactor write SetGlyphDisabledBlendFactor default 127;
    property GlyphHotGammaFactor: Byte read FGlyphHotGammaFactor write SetGlyphHotGammaFactor default 130;
    property MoveWhenDown: Boolean read FMoveWhenDown write SetMoveWhenDown default False;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
  end;
  {$endregion}


  //TJppPngButtonTagExt = class(TJppTagExt);

  {$region ' ----------------- TJppPngButton ------------------- '}
  TJppPngButton = class(TBitBtn)
  {$IFDEF DCC}
    {$IF RTLVersion >= 24.0 }
    strict private
      class constructor Create;
      class destructor Destroy;
    {$IFEND}
  {$ENDIF}
  private
    bOver: Boolean;
    FPngImage: TPngImage;
    FPngOptions: TPngOptions;
    FCanvas: TCanvas;
    FLastKind: TBitBtnKind;
    FImageFromAction: Boolean;
    {$IFDEF DCC} FMouseInControl: Boolean; {$ENDIF}
    IsFocused: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FTagExt: TJppTagExt;
    FAppearance: TJppButtonAppearance;
    FColorMapType: TJppPngButtonColorMapType;
    function PngImageStored: Boolean;
    procedure SetPngImage(const Value: TPngImage);
    procedure SetPngOptions(const Value: TPngOptions);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetOnMouseEnter(const Value: TNotifyEvent);
    procedure SetOnMouseLeave(const Value: TNotifyEvent);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetAppearance(const Value: TJppButtonAppearance);
    procedure SetColorMapType(const Value: TJppPngButtonColorMapType);
    //property Glyph stored False;
    //property NumGlyphs stored False;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    {$IFDEF DCC} procedure SetButtonStyle(ADefault: Boolean); override; {$ENDIF}
    procedure PropsChanged(Sender: TObject);
    procedure Paint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyColorMap(ColorMap: TJppPngButtonColorMap);
    procedure SaveColorMapToIniFile(FileName: string; Section: string = COLORMAP_DEFAULT_INI_SECTION; Format: TJppPngButtonIniColorFormat = icfDefault);
    procedure LoadColorMapFromIniFile(FileName: string; Section: string = COLORMAP_DEFAULT_INI_SECTION; Format: TJppPngButtonIniColorFormat = icfDefault);
  published
    property PngImage: TPngImage read FPngImage write SetPngImage stored PngImageStored;
    property PngOptions: TPngOptions read FPngOptions write SetPngOptions default [pngBlendOnDisabled];
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write SetOnMouseLeave;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property Appearance: TJppButtonAppearance read FAppearance write SetAppearance;
    property ColorMapType: TJppPngButtonColorMapType read FColorMapType write SetColorMapType default cmtCustom;
  end;
  {$endregion TJppPngButton}


  {$IFDEF DCC}
    {$IF RTLVersion >= 24.0 }
    TJppPngButtonStyleHook = class(TBitBtnStyleHook)
    strict protected
      procedure DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean); override;
    end;
    {$IFEND}
  {$ENDIF}



procedure SetJppPngButtonColorMap(Button: TJppPngButton; ColorMap: TJppPngButtonColorMap);
procedure GetJppPngButtonColorMap(Button: TJppPngButton; var ColorMap: TJppPngButtonColorMap);
procedure SetJppPngButtonFonts(Button: TJppPngButton; FontName: string = 'Segoe UI'; FontSize: integer = 8);
procedure SetJppPngButtonVclStyle(Button: TJppPngButton; StyleName: string);


implementation

{$IFDEF DCC}
uses
  Vcl.Themes, PngButtonFunctions, PngImageList;
{$ENDIF}


{$region ' -------------- helpers ------------ '}

procedure SetJppPngButtonVclStyle(Button: TJppPngButton; StyleName: string);
begin
  StyleName := TrimUp(StyleName);
  StyleName := RemoveSpaces(StyleName);

  if (StyleName = '') or (StyleName = 'WINDOWS') then
  begin
    {$IFDEF DCC}{$IF RTLVersion > 23} Button.StyleElements := [seFont, seClient, seBorder]; {$IFEND}{$ENDIF}
    Button.Appearance.DefaultDrawing := True;
  end

  else

  begin
    {$IFDEF DCC}{$IF RTLVersion > 23} Button.StyleElements := []; {$IFEND}{$ENDIF}
    Button.Appearance.DefaultDrawing := False;

    if AnsiStartsText('AQUALIGHT', StyleName) then Button.ColorMapType := cmtVclAquaLightSlate
    else if AnsiStartsText('AURIC', StyleName) then Button.ColorMapType := cmtVclAuric
    else if AnsiStartsText('CARBON', StyleName) then Button.ColorMapType := cmtVclCarbon
    else if AnsiStartsText('CHARCOALDARK', StyleName) then Button.ColorMapType := cmtVclCharcoalDarkSlate
    else if AnsiStartsText('ICEBERG', StyleName) then Button.ColorMapType := cmtVclIcebergClassico
    else if AnsiStartsText('LUNA', StyleName) then Button.ColorMapType := cmtVclLuna
    else if AnsiStartsText('SLATECLASSICO', StyleName) then Button.ColorMapType := cmtVclSlateClassico
    else if AnsiStartsText('COBALTXEMEDIA', StyleName) then Button.ColorMapType := cmtVclCobaltXEMedia
    else if AnsiStartsText('GLOSSY', StyleName) then Button.ColorMapType := cmtVclGlossy
    else if AnsiStartsText('JET', StyleName) then Button.ColorMapType := cmtVclJet;
  end;
end;

procedure SetJppPngButtonFonts(Button: TJppPngButton; FontName: string = 'Segoe UI'; FontSize: integer = 8);
begin
  Button.Font.Name := FontName;
  Button.Font.Size := FontSize;
  Button.Appearance.Normal.Font.Name := FontName;
  Button.Appearance.Normal.Font.Size := FontSize;
  Button.Appearance.Hot.Font.Name := FontName;
  Button.Appearance.Hot.Font.Size := FontSize;
  Button.Appearance.Down.Font.Name := FontName;
  Button.Appearance.Down.Font.Size := FontSize;
  Button.Appearance.Focused.Font.Name := FontName;
  Button.Appearance.Focused.Font.Size := FontSize;
  Button.Appearance.Disabled.Font.Name := FontName;
  Button.Appearance.Disabled.Font.Size := FontSize;
end;

procedure GetJppPngButtonColorMap(Button: TJppPngButton; var ColorMap: TJppPngButtonColorMap);
begin
  ColorMap.BorderWhenDefaultColor := ColorToRGB(Button.Appearance.BorderWhenDefault.Color);
  ColorMap.FocusRectColor := ColorToRGB(Button.Appearance.FocusRect.Pen.Color);

  ColorMap.Normal.BorderColor := ColorToRGB(Button.Appearance.Normal.Border.Color);
  ColorMap.Normal.FontColor := ColorToRGB(Button.Appearance.Normal.Font.Color);
  ColorMap.Normal.Color := ColorToRGB(Button.Appearance.Normal.Color);
  ColorMap.Normal.BottomGradientColorFrom := ColorToRGB(Button.Appearance.Normal.BottomGradient.ColorFrom);
  ColorMap.Normal.BottomGradientColorTo := ColorToRGB(Button.Appearance.Normal.BottomGradient.ColorTo);
  ColorMap.Normal.UpperGradientColorFrom := ColorToRGB(Button.Appearance.Normal.UpperGradient.ColorFrom);
  ColorMap.Normal.UpperGradientColorTo := ColorToRGB(Button.Appearance.Normal.UpperGradient.ColorTo);

  ColorMap.Hot.BorderColor := ColorToRGB(Button.Appearance.Hot.Border.Color);
  ColorMap.Hot.FontColor := ColorToRGB(Button.Appearance.Hot.Font.Color);
  ColorMap.Hot.Color := ColorToRGB(Button.Appearance.Hot.Color);
  ColorMap.Hot.BottomGradientColorFrom := ColorToRGB(Button.Appearance.Hot.BottomGradient.ColorFrom);
  ColorMap.Hot.BottomGradientColorTo := ColorToRGB(Button.Appearance.Hot.BottomGradient.ColorTo);
  ColorMap.Hot.UpperGradientColorFrom := ColorToRGB(Button.Appearance.Hot.UpperGradient.ColorFrom);
  ColorMap.Hot.UpperGradientColorTo := ColorToRGB(Button.Appearance.Hot.UpperGradient.ColorTo);

  ColorMap.Down.BorderColor := ColorToRGB(Button.Appearance.Down.Border.Color);
  ColorMap.Down.FontColor := ColorToRGB(Button.Appearance.Down.Font.Color);
  ColorMap.Down.Color := ColorToRGB(Button.Appearance.Down.Color);
  ColorMap.Down.BottomGradientColorFrom := ColorToRGB(Button.Appearance.Down.BottomGradient.ColorFrom);
  ColorMap.Down.BottomGradientColorTo := ColorToRGB(Button.Appearance.Down.BottomGradient.ColorTo);
  ColorMap.Down.UpperGradientColorFrom := ColorToRGB(Button.Appearance.Down.UpperGradient.ColorFrom);
  ColorMap.Down.UpperGradientColorTo := ColorToRGB(Button.Appearance.Down.UpperGradient.ColorTo);

  ColorMap.Focused.BorderColor := ColorToRGB(Button.Appearance.Focused.Border.Color);
  ColorMap.Focused.FontColor := ColorToRGB(Button.Appearance.Focused.Font.Color);
  ColorMap.Focused.Color := ColorToRGB(Button.Appearance.Focused.Color);
  ColorMap.Focused.BottomGradientColorFrom := ColorToRGB(Button.Appearance.Focused.BottomGradient.ColorFrom);
  ColorMap.Focused.BottomGradientColorTo := ColorToRGB(Button.Appearance.Focused.BottomGradient.ColorTo);
  ColorMap.Focused.UpperGradientColorFrom := ColorToRGB(Button.Appearance.Focused.UpperGradient.ColorFrom);
  ColorMap.Focused.UpperGradientColorTo := ColorToRGB(Button.Appearance.Focused.UpperGradient.ColorTo);

  ColorMap.Disabled.BorderColor := ColorToRGB(Button.Appearance.Disabled.Border.Color);
  ColorMap.Disabled.FontColor := ColorToRGB(Button.Appearance.Disabled.Font.Color);
  ColorMap.Disabled.Color := ColorToRGB(Button.Appearance.Disabled.Color);
  ColorMap.Disabled.BottomGradientColorFrom := ColorToRGB(Button.Appearance.Disabled.BottomGradient.ColorFrom);
  ColorMap.Disabled.BottomGradientColorTo := ColorToRGB(Button.Appearance.Disabled.BottomGradient.ColorTo);
  ColorMap.Disabled.UpperGradientColorFrom := ColorToRGB(Button.Appearance.Disabled.UpperGradient.ColorFrom);
  ColorMap.Disabled.UpperGradientColorTo := ColorToRGB(Button.Appearance.Disabled.UpperGradient.ColorTo);

end;

procedure SetJppPngButtonColorMap(Button: TJppPngButton; ColorMap: TJppPngButtonColorMap);
begin
  Button.Appearance.BorderWhenDefault.Color := ColorMap.BorderWhenDefaultColor;
  Button.Appearance.FocusRect.Pen.Color := ColorMap.FocusRectColor;

  Button.Appearance.Normal.Border.Color := ColorMap.Normal.BorderColor;
  Button.Appearance.Normal.Font.Color := ColorMap.Normal.FontColor;
  Button.Appearance.Normal.Color := ColorMap.Normal.Color;
  Button.Appearance.Normal.UpperGradient.ColorFrom := ColorMap.Normal.UpperGradientColorFrom;
  Button.Appearance.Normal.UpperGradient.ColorTo := ColorMap.Normal.UpperGradientColorTo;
  Button.Appearance.Normal.BottomGradient.ColorFrom := ColorMap.Normal.BottomGradientColorFrom;
  Button.Appearance.Normal.BottomGradient.ColorTo := ColorMap.Normal.BottomGradientColorTo;

  Button.Appearance.Hot.Border.Color := ColorMap.Hot.BorderColor;
  Button.Appearance.Hot.Font.Color := ColorMap.Hot.FontColor;
  Button.Appearance.Hot.Color := ColorMap.Hot.Color;
  Button.Appearance.Hot.UpperGradient.ColorFrom := ColorMap.Hot.UpperGradientColorFrom;
  Button.Appearance.Hot.UpperGradient.ColorTo := ColorMap.Hot.UpperGradientColorTo;
  Button.Appearance.Hot.BottomGradient.ColorFrom := ColorMap.Hot.BottomGradientColorFrom;
  Button.Appearance.Hot.BottomGradient.ColorTo := ColorMap.Hot.BottomGradientColorTo;

  Button.Appearance.Down.Border.Color := ColorMap.Down.BorderColor;
  Button.Appearance.Down.Font.Color := ColorMap.Down.FontColor;
  Button.Appearance.Down.Color := ColorMap.Down.Color;
  Button.Appearance.Down.UpperGradient.ColorFrom := ColorMap.Down.UpperGradientColorFrom;
  Button.Appearance.Down.UpperGradient.ColorTo := ColorMap.Down.UpperGradientColorTo;
  Button.Appearance.Down.BottomGradient.ColorFrom := ColorMap.Down.BottomGradientColorFrom;
  Button.Appearance.Down.BottomGradient.ColorTo := ColorMap.Down.BottomGradientColorTo;

  Button.Appearance.Focused.Border.Color := ColorMap.Focused.BorderColor;
  Button.Appearance.Focused.Font.Color := ColorMap.Focused.FontColor;
  Button.Appearance.Focused.Color := ColorMap.Focused.Color;
  Button.Appearance.Focused.UpperGradient.ColorFrom := ColorMap.Focused.UpperGradientColorFrom;
  Button.Appearance.Focused.UpperGradient.ColorTo := ColorMap.Focused.UpperGradientColorTo;
  Button.Appearance.Focused.BottomGradient.ColorFrom := ColorMap.Focused.BottomGradientColorFrom;
  Button.Appearance.Focused.BottomGradient.ColorTo := ColorMap.Focused.BottomGradientColorTo;

  Button.Appearance.Disabled.Border.Color := ColorMap.Disabled.BorderColor;
  Button.Appearance.Disabled.Font.Color := ColorMap.Disabled.FontColor;
  Button.Appearance.Disabled.Color := ColorMap.Disabled.Color;
  Button.Appearance.Disabled.UpperGradient.ColorFrom := ColorMap.Disabled.UpperGradientColorFrom;
  Button.Appearance.Disabled.UpperGradient.ColorTo := ColorMap.Disabled.UpperGradientColorTo;
  Button.Appearance.Disabled.BottomGradient.ColorFrom := ColorMap.Disabled.BottomGradientColorFrom;
  Button.Appearance.Disabled.BottomGradient.ColorTo := ColorMap.Disabled.BottomGradientColorTo;

end;
{$endregion}


{$region ' ---------------- Themes ------------------------- '}
{$IFDEF DCC}

{$IF RTLVersion < 23.0 }
type
  TThemeServicesHelper = class helper for TThemeServices
  private
    function GetEnabled: Boolean;
  public
    function GetElementContentRect(DC: HDC; Details: TThemedElementDetails; const BoundingRect: TRect; out ContentRect: TRect): Boolean; overload;
    property Enabled: Boolean read GetEnabled;
  end;

function TThemeServicesHelper.GetElementContentRect(DC: HDC; Details: TThemedElementDetails; const BoundingRect: TRect; out ContentRect: TRect): Boolean;
begin
  ContentRect := Self.ContentRect(DC, Details, BoundingRect);
  Result := true;
end;

function TThemeServicesHelper.GetEnabled: Boolean;
begin
  Result := ThemesEnabled;
end;

function StyleServices: TThemeServices;
begin
  Result := ThemeServices;
end;
{$IFEND}

{$IF RTLVersion >= 24.0 }

class constructor TJppPngButton.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TJppPngButton, TJppPngButtonStyleHook);
end;

class destructor TJppPngButton.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TJppPngButton, TJppPngButtonStyleHook);
end;
{$IFEND}

{$ENDIF}
{$endregion Themes}



{$region ' -------------------- TJppItemStateParams ------------------------ '}
constructor TJppItemStateParams.Create(AOwner: TComponent);
begin
  inherited Create;
  //FGradient := TJppGradient.Create(AOwner);
  FUpperGradient := TJppGradientEx.Create(AOwner);
  FBottomGradient := TJppGradientEx.Create(AOwner);
  FGradientEnabled := True;
  FBorderToGradientMargin := 2;
  FFont := TFont.Create;
  FBorder := TPen.Create;
  FTransparentBackground := False;
  FTransparentFrame := False;

  FUpperGradient.OnChange := PropsChanged;
  FBottomGradient.OnChange := PropsChanged;
  FFont.OnChange := PropsChanged;
  FBorder.OnChange := PropsChanged;
end;

destructor TJppItemStateParams.Destroy;
begin
  //FGradient.Free;
  FUpperGradient.Free;
  FBottomGradient.Free;
  FFont.Free;
  FBorder.Free;
  inherited;
end;

procedure TJppItemStateParams.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppItemStateParams.SetBorder(const Value: TPen);
begin
  FBorder := Value;
  PropsChanged(Self);
end;

procedure TJppItemStateParams.SetBorderToGradientMargin(const Value: integer);
begin
  FBorderToGradientMargin := Value;
  PropsChanged(Self);
end;

procedure TJppItemStateParams.SetBottomGradient(const Value: TJppGradientEx);
begin
  FBottomGradient := Value;
  PropsChanged(Self);
end;

procedure TJppItemStateParams.SetColor(const Value: TColor);
begin
  FColor := Value;
  PropsChanged(Self);
end;


procedure TJppItemStateParams.SetFont(const Value: TFont);
begin
  FFont := Value;
  PropsChanged(Self);
end;

procedure TJppItemStateParams.SetGradientEnabled(const Value: Boolean);
begin
  FGradientEnabled := Value;
  PropsChanged(Self);
end;

procedure TJppItemStateParams.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppItemStateParams.SetTransparentBackground(const Value: Boolean);
begin
  FTransparentBackground := Value;
  PropsChanged(Self);
end;

procedure TJppItemStateParams.SetTransparentFrame(const Value: Boolean);
begin
  FTransparentFrame := Value;
  PropsChanged(Self);
end;

procedure TJppItemStateParams.SetUpperGradient(const Value: TJppGradientEx);
begin
  FUpperGradient := Value;
  PropsChanged(Self);
end;

procedure TJppItemStateParams.SetUpperGradientPercent(const Value: Byte);
begin
  FUpperGradientPercent := Value;
  PropsChanged(Self);
end;

{$endregion}



{$region ' -------------------- TJppButtonAppearance --------------------- '}
constructor TJppButtonAppearance.Create(AOwner: TComponent);
begin
  inherited Create;

  FNormal := TJppItemStateParams.Create(AOwner);
  FNormal.Color := $00F3F3F3;
  FNormal.UpperGradient.ColorFrom := $00F1F1F1;
  FNormal.UpperGradient.ColorTo := $00EBEBEB;
  FNormal.BottomGradient.ColorFrom := $00DDDDDD;
  FNormal.BottomGradient.ColorTo := $00CFCFCF;
  FNormal.Border.Color := $00707070;
  FNormal.UpperGradientPercent := 46;
  FNormal.Font.Color := 0;

  FHot := TJppItemStateParams.Create(AOwner);
  FHot.Color := $00FCF5E8;
  FHot.UpperGradient.ColorFrom := $00FDF6EA;
  FHot.UpperGradient.ColorTo := $00FCF0D9;
  FHot.BottomGradient.ColorFrom := $00FDE6BE;
  FHot.BottomGradient.ColorTo := $00F5D9A7;
  FHot.Border.Color := $00B17F3C;
  FHot.UpperGradientPercent := FNormal.UpperGradientPercent;
  FHot.Font.Color := 0;

  FFocused := TJppItemStateParams.Create(AOwner);
  FFocused.Color := FNormal.Color;
  FFocused.UpperGradient.ColorFrom := FNormal.UpperGradient.ColorFrom;
  FFocused.UpperGradient.ColorTo := FNormal.UpperGradient.ColorTo;
  FFocused.BottomGradient.ColorFrom := FNormal.BottomGradient.ColorFrom;
  FFocused.BottomGradient.ColorTo := FNormal.BottomGradient.ColorTo;
  FFocused.Border.Color := $00D0AA24;
  FFocused.UpperGradientPercent := FNormal.UpperGradientPercent;
  FFocused.Font.Color := FNormal.Font.Color;

  FDown := TJppItemStateParams.Create(AOwner);
  FDown.Color := $00F1E2C5;
  FDown.UpperGradient.ColorFrom := $00FCF4E5;
  FDown.UpperGradient.ColorTo := $00F6E5C4;
  FDown.BottomGradient.ColorFrom := $00EFD198;
  FDown.BottomGradient.ColorTo := $00DFB972;
  FDown.Border.Color := GetSimilarColor(FHot.Border.Color , 50, False);
  FDown.UpperGradientPercent := 52;
  //FDown.BorderToGradientMargin := 0;
  FDown.Font.Color := 0;

  FDisabled := TJppItemStateParams.Create(AOwner);
  FDisabled.Color := $00F4F4F4; // $00FCFCFC;
  FDisabled.UpperGradient.ColorFrom := $00F4F4F4;
  FDisabled.UpperGradient.ColorTo := $00F4F4F4;
  FDisabled.BottomGradient.ColorFrom := $00F4F4F4;
  FDisabled.BottomGradient.ColorTo := $00F4F4F4;
  FDisabled.Border.Color := $00B5B2AD;
  FDisabled.UpperGradientPercent := FNormal.UpperGradientPercent;
  FDisabled.Font.Color := RGB(160,160,160); // clBtnShadow - 3x160; //clGrayText;

  FFocusRect := TJppFocusRectParams.Create(AOwner);
  FFocusRect.Pen.Color := $00D0AA24;

  FBorderWhenDefault := TPen.Create;
  //FBorderWhenDefault.Width := 3;
  FBorderWhenDefault.Color := $00D0AA24; // FNormal.Border.Color;

  FGlyphDisabledGrayscaleFactor := 255;
  FGlyphDisabledBlendFactor := 127;
  FGlyphHotGammaFactor := 130;


  FNormal.OnChange := PropsChanged;
  FHot.OnChange := PropsChanged;
  FFocused.OnChange := PropsChanged;
  FDown.OnChange := PropsChanged;
  FDisabled.OnChange := PropsChanged;
  FFocusRect.OnChange := PropsChanged;

  FShowCaption := True;

end;

destructor TJppButtonAppearance.Destroy;
begin
  FNormal.Free;
  FHot.Free;
  FFocused.Free;
  FDefaultDrawing := False;
  FDown.Free;
  FDisabled.Free;
  FFocusRect.Free;
  FBorderWhenDefault.Free;
  inherited;
end;

procedure TJppButtonAppearance.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppButtonAppearance.Assign(Source: TJppButtonAppearance);
begin

  DefaultDrawing := Source.DefaultDrawing;
  GlyphDisabledGrayscaleFactor := Source.GlyphDisabledGrayscaleFactor;
  GlyphDisabledBlendFactor := Source.GlyphDisabledBlendFactor;
  GlyphHotGammaFactor := Source.GlyphHotGammaFactor;
  MoveWhenDown := Source.MoveWhenDown;
  BorderWhenDefault.Assign(Source.BorderWhenDefault);

  FocusRect.FocusType := Source.FocusRect.FocusType;
  FocusRect.Spacing := Source.FocusRect.Spacing;
  FocusRect.Pen.Assign(Source.FocusRect.Pen);

  ShowCaption := Source.ShowCaption;


  Normal.Font.Assign(Source.Normal.Font);
  Normal.Border.Assign(Source.Normal.Border);
  Normal.Color := Source.Normal.Color;
  Normal.GradientEnabled := Source.Normal.GradientEnabled;
  Normal.UpperGradient.Assign(Source.Normal.UpperGradient);
  Normal.BottomGradient.Assign(Source.Normal.BottomGradient);
  Normal.UpperGradientPercent := Source.Normal.UpperGradientPercent;
  Normal.BorderToGradientMargin := Source.Normal.BorderToGradientMargin;
  Normal.TransparentBackground := Source.Normal.TransparentBackground;
  Normal.TransparentFrame := Source.Normal.TransparentFrame;

  Hot.Font.Assign(Source.Hot.Font);
  Hot.Border.Assign(Source.Hot.Border);
  Hot.Color := Source.Hot.Color;
  Hot.GradientEnabled := Source.Hot.GradientEnabled;
  Hot.UpperGradient.Assign(Source.Hot.UpperGradient);
  Hot.BottomGradient.Assign(Source.Hot.BottomGradient);
  Hot.UpperGradientPercent := Source.Hot.UpperGradientPercent;
  Hot.BorderToGradientMargin := Source.Hot.BorderToGradientMargin;
  Hot.TransparentBackground := Source.Hot.TransparentBackground;
  Hot.TransparentFrame := Source.Hot.TransparentFrame;

  Down.Font.Assign(Source.Down.Font);
  Down.Border.Assign(Source.Down.Border);
  Down.Color := Source.Down.Color;
  Down.GradientEnabled := Source.Down.GradientEnabled;
  Down.UpperGradient.Assign(Source.Down.UpperGradient);
  Down.BottomGradient.Assign(Source.Down.BottomGradient);
  Down.UpperGradientPercent := Source.Down.UpperGradientPercent;
  Down.BorderToGradientMargin := Source.Down.BorderToGradientMargin;
  Down.TransparentBackground := Source.Down.TransparentBackground;
  Down.TransparentFrame := Source.Down.TransparentFrame;

  Focused.Font.Assign(Source.Focused.Font);
  Focused.Border.Assign(Source.Focused.Border);
  Focused.Color := Source.Focused.Color;
  Focused.GradientEnabled := Source.Focused.GradientEnabled;
  Focused.UpperGradient.Assign(Source.Focused.UpperGradient);
  Focused.BottomGradient.Assign(Source.Focused.BottomGradient);
  Focused.UpperGradientPercent := Source.Focused.UpperGradientPercent;
  Focused.BorderToGradientMargin := Source.Focused.BorderToGradientMargin;
  Focused.TransparentBackground := Source.Focused.TransparentBackground;
  Focused.TransparentFrame := Source.Focused.TransparentFrame;

  Disabled.Font.Assign(Source.Disabled.Font);
  Disabled.Border.Assign(Source.Disabled.Border);
  Disabled.Color := Source.Disabled.Color;
  Disabled.GradientEnabled := Source.Disabled.GradientEnabled;
  Disabled.UpperGradient.Assign(Source.Disabled.UpperGradient);
  Disabled.BottomGradient.Assign(Source.Disabled.BottomGradient);
  Disabled.UpperGradientPercent := Source.Disabled.UpperGradientPercent;
  Disabled.BorderToGradientMargin := Source.Disabled.BorderToGradientMargin;
  Disabled.TransparentBackground := Source.Disabled.TransparentBackground;
  Disabled.TransparentFrame := Source.Disabled.TransparentFrame;

end;

procedure TJppButtonAppearance.SetBorderWhenDefault(const Value: TPen);
begin
  FBorderWhenDefault := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetDefaultDrawing(const Value: Boolean);
begin
  FDefaultDrawing := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetDisabled(const Value: TJppItemStateParams);
begin
  FDisabled := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetGlyphDisabledBlendFactor(const Value: Byte);
begin
  FGlyphDisabledBlendFactor := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetGlyphDisabledGrayscaleFactor(const Value: Byte);
begin
  FGlyphDisabledGrayscaleFactor := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetDown(const Value: TJppItemStateParams);
begin
  FDown := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetFocusRect(const Value: TJppFocusRectParams);
begin
  FFocusRect := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetFocused(const Value: TJppItemStateParams);
begin
  FFocused := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetHot(const Value: TJppItemStateParams);
begin
  FHot := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetGlyphHotGammaFactor(const Value: Byte);
begin
  FGlyphHotGammaFactor := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetMoveWhenDown(const Value: Boolean);
begin
  FMoveWhenDown := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetNormal(const Value: TJppItemStateParams);
begin
  FNormal := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  PropsChanged(Self);
end;

procedure TJppButtonAppearance.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
  PropsChanged(Self);
end;

{$endregion}



{$region ' ------------------------------- TJppPngButton ------------------------------------- '}

  {$region ' ---------------------- Cretae & Destroy ----------------------------- '}
constructor TJppPngButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPngImage := TPngImage.Create;
  FPngOptions := [pngBlendOnDisabled];
  FCanvas := TCanvas.Create;
  FLastKind := bkCustom;
  FImageFromAction := False;
  FTagExt := TJppTagExt.Create(Self);
  FAppearance := TJppButtonAppearance.Create(Self);
  FAppearance.OnChange := PropsChanged;
  FColorMapType := cmtCustom;

  bOver := False;
end;

destructor TJppPngButton.Destroy;
begin
  FPngImage.Free;
  FCanvas.Free;
  FTagExt.Free;
  FAppearance.Free;
  inherited Destroy;
end;
  {$endregion Create & Destroy}


  {$region ' ---------------------- Color Maps -------------------------- '}

procedure TJppPngButton.SetColorMapType(const Value: TJppPngButtonColorMapType);
var
  ColorMap: TJppPngButtonColorMap;
begin
  FColorMapType := Value;
  if Value = cmtCustom then Exit;

  GetJppPngButtonColorMap(Self, ColorMap);
  GetJppPngButtonColorMapByType(Value, ColorMap);
  ApplyColorMap(ColorMap);
end;

procedure TJppPngButton.SaveColorMapToIniFile(FileName: string; Section: string; Format: TJppPngButtonIniColorFormat);
var
  ColorMap: TJppPngButtonColorMap;
begin
  GetJppPngButtonColorMap(Self, ColorMap);
  ColorMap.SaveToIniFile(FileName, Section, Format);
end;

procedure TJppPngButton.LoadColorMapFromIniFile(FileName: string; Section: string = COLORMAP_DEFAULT_INI_SECTION; Format: TJppPngButtonIniColorFormat = icfDefault);
var
  ColorMap: TJppPngButtonColorMap;
begin
  ColorMap.LoadFromIniFile(FileName, Section, Format);
  ApplyColorMap(ColorMap);
end;

procedure TJppPngButton.ApplyColorMap(ColorMap: TJppPngButtonColorMap);
begin
  SetJppPngButtonColorMap(Self, ColorMap);
end;
  {$endregion}


  {$region ' --------------------------------- misc ------------------------------------ '}
procedure TJppPngButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
  begin
    with TCustomAction(Sender) do
    begin
      //Copy image from action's imagelist
      if (pngimage.Empty or FImageFromAction) and (ActionList <> nil) and (ActionList.Images <> nil) and (ImageIndex >= 0) and
        (ImageIndex < ActionList.Images.Count) then
      begin
        CopyImageFromImageList(FPngImage, ActionList.Images, ImageIndex);
        FImageFromAction := true;
      end;
    end;
  end;
end;

procedure TJppPngButton.SetAppearance(const Value: TJppButtonAppearance);
begin
  FAppearance := Value;
end;

{$IFDEF DCC}
procedure TJppPngButton.SetButtonStyle(ADefault: Boolean);
begin
  inherited SetButtonStyle(ADefault);
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Refresh;
  end;
end;
{$ENDIF}

function TJppPngButton.PngImageStored: Boolean;
begin
  Result := not FImageFromAction;
end;

procedure TJppPngButton.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;

procedure TJppPngButton.Paint;
begin
  //inherited;
  with FCanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clRed;
    Rectangle(ClientRect);
  end;
end;

procedure TJppPngButton.SetPngImage(const Value: TPngImage);
begin
  //This is all neccesary, because you can't assign a nil to a TPngImage
  if Value = nil then
  begin
    FPngImage.Free;
    FPngImage := TPngImage.Create;
  end
  else
  begin
    FPngImage.Assign(Value);
  end;

  {$IFDEF DCC}
  //To work around the gamma-problem
  with FPngImage do
    if not Empty and (Header.ColorType in [COLOR_RGB, COLOR_RGBALPHA, COLOR_PALETTE]) then Chunks.RemoveChunk(Chunks.ItemFromClass(TChunkgAMA));
  {$ENDIF}

  FImageFromAction := False;
  Repaint;
end;

procedure TJppPngButton.SetPngOptions(const Value: TPngOptions);
begin
  if FPngOptions <> Value then
  begin
    FPngOptions := Value;
    Repaint;
  end;
end;

procedure TJppPngButton.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;
  {$endregion misc}

  {$region ' -------------------------------------- CNDrawItem --------------------------------------- '}
procedure TJppPngButton.CNDrawItem(var Message: TWMDrawItem);

type
  TGradientParams = record
    AngleDegree: Word;
    Balance: Word;
    BalanceMode: JPP.Gradient.TDgradBalanceMode;
    MaxDegrade: Byte;
    Orientation: JPP.Gradient.TDgradOrientation;
    SpeedPercent: integer;
    ColorFrom, ColorTo: TColor;
  end;

  TDrawingParams = record
    ColorGradientStart, ColorGradientEnd, Color: TColor;
    bGradientEnabled: Boolean;
    GradientType: TJppGradientType;
    GradientSteps: Byte;
    Pen: TPen;
    Font: TFont;
    UpperGradientParams, BottomGradientParams: TGradientParams;
    UpperGradientPercent: Byte;
    BorderToGradientMargin: integer;
    TransparentBackground: Boolean;
    TransparentFrame: Boolean;
  end;

  procedure CopyGradientParams(var GradientParams: TGradientParams; GradientEx: TJppGradientEx);
  begin
    if not Assigned(GradientEx) then Exit;
    GradientParams.AngleDegree := GradientEx.AngleDegree;
    GradientParams.Balance := GradientEx.Balance;
    GradientParams.BalanceMode := GradientEx.BalanceMode;
    GradientParams.MaxDegrade := GradientEx.MaxDegrade;
    GradientParams.Orientation := GradientEx.Orientation;
    GradientParams.SpeedPercent := GradientEx.SpeedPercent;
    GradientParams.ColorFrom := GradientEx.ColorFrom;
    GradientParams.ColorTo := GradientEx.ColorTo;
  end;

var
  PaintRect: TRect;
  GlyphPos, TextPos: TPoint;
  IsDown, IsDefault: Boolean;
  Flags: Cardinal;
  {$IFDEF DCC}
  Button: TThemedButton;
  Details: TThemedElementDetails;
  {$ENDIF}

  bDown, bDefault: Boolean;
  R, R2, BgRect: TRect;
  Canvas: TCanvas;
  //xRound: integer;
  imgDisabled, imgHot: TPngImage;
  dp: TDrawingParams;
  xBottomGradientTop: integer;
  s: string;

begin


  if not Appearance.DefaultDrawing then
  begin


    //xRound := 0;
    dp.TransparentBackground := true;

    bDown := Message.DrawItemStruct^.itemState and ODS_SELECTED <> 0;
    bDefault := Message.DrawItemStruct^.itemState and ODS_FOCUS <> 0;

    Canvas := TCanvas.Create;
    try
      {$IFDEF MSWINDOWS}
      Canvas.Handle := Message.DrawItemStruct^.hDC;
      {$ELSE}
      Canvas.Handle := Message.DrawItemStruct^._hDC;
      {$ENDIF}
      R := ClientRect;

      with Canvas do
      begin

        //RectWidth := R.Right - R.Left; // rect width
        //RectHeight := R.Bottom - R.Top; // rect height

        dp.bGradientEnabled := True;
        dp.TransparentBackground := False;
        dp.TransparentFrame := False;
        dp.Pen := Pen;
        dp.Font := Font;


        {$region ' ----------- Disabled, Hot, Down, Focused, Normal ------------ '}
        // Disabled
        if not Enabled then
        begin
          dp.bGradientEnabled := Appearance.Disabled.GradientEnabled;
          if dp.bGradientEnabled then
          begin
            CopyGradientParams(dp.UpperGradientParams, Appearance.Disabled.UpperGradient);
            CopyGradientParams(dp.BottomGradientParams, Appearance.Disabled.BottomGradient);
          end;
          dp.Color := Appearance.Disabled.Color;
          dp.Pen := Appearance.Disabled.Border;
          dp.Font := Appearance.Disabled.Font;
          dp.UpperGradientPercent := Appearance.Disabled.UpperGradientPercent;
          dp.BorderToGradientMargin := Appearance.Disabled.BorderToGradientMargin;
          dp.TransparentBackground := Appearance.Disabled.TransparentBackground;
          dp.TransparentFrame := Appearance.Disabled.TransparentFrame;
        end

        else

        begin


          // Hot
          if bOver and (not bDown) then
          begin
            dp.bGradientEnabled := Appearance.Hot.GradientEnabled;
            if dp.bGradientEnabled then
            begin
              CopyGradientParams(dp.UpperGradientParams, Appearance.Hot.UpperGradient);
              CopyGradientParams(dp.BottomGradientParams, Appearance.Hot.BottomGradient);
            end;
            dp.Color := Appearance.Hot.Color;
            dp.Pen := Appearance.Hot.Border;
            dp.Font := Appearance.Hot.Font;
            dp.UpperGradientPercent := Appearance.Hot.UpperGradientPercent;
            dp.BorderToGradientMargin := Appearance.Hot.BorderToGradientMargin;
            dp.TransparentBackground := Appearance.Hot.TransparentBackground;
            dp.TransparentFrame := Appearance.Hot.TransparentFrame;
          end

          // Pressed
          else if bDown then
          begin
            dp.bGradientEnabled := Appearance.Down.GradientEnabled;
            if dp.bGradientEnabled then
            begin
              CopyGradientParams(dp.UpperGradientParams, Appearance.Down.UpperGradient);
              CopyGradientParams(dp.BottomGradientParams, Appearance.Down.BottomGradient);
            end;
            dp.Color := Appearance.Down.Color;
            dp.Pen := Appearance.Down.Border;
            dp.Font := Appearance.Down.Font;
            dp.UpperGradientPercent := Appearance.Down.UpperGradientPercent;
            dp.BorderToGradientMargin := Appearance.Down.BorderToGradientMargin;
            dp.TransparentBackground := Appearance.Down.TransparentBackground;
            dp.TransparentFrame := Appearance.Down.TransparentFrame;
          end

          // Focused
          else if Focused then
          begin
            dp.bGradientEnabled := Appearance.Focused.GradientEnabled;
            if dp.bGradientEnabled then
            begin
              CopyGradientParams(dp.UpperGradientParams, Appearance.Focused.UpperGradient);
              CopyGradientParams(dp.BottomGradientParams, Appearance.Focused.BottomGradient);
            end;
            dp.Color := Appearance.Focused.Color;
            dp.Pen := Appearance.Focused.Border;
            dp.Font := Appearance.Focused.Font;
            dp.UpperGradientPercent := Appearance.Focused.UpperGradientPercent;
            dp.BorderToGradientMargin := Appearance.Focused.BorderToGradientMargin;
            dp.TransparentBackground := Appearance.Focused.TransparentBackground;
            dp.TransparentFrame := Appearance.Focused.TransparentFrame;
          end

          else

          // Normal
          begin
            dp.bGradientEnabled := Appearance.Normal.GradientEnabled;
            if dp.bGradientEnabled then
            begin
              CopyGradientParams(dp.UpperGradientParams, Appearance.Normal.UpperGradient);
              CopyGradientParams(dp.BottomGradientParams, Appearance.Normal.BottomGradient);
            end;
            dp.Color := Appearance.Normal.Color;
            dp.Pen := Appearance.Normal.Border;
            dp.Font := Appearance.Normal.Font;
            dp.UpperGradientPercent := Appearance.Normal.UpperGradientPercent;
            dp.BorderToGradientMargin := Appearance.Normal.BorderToGradientMargin;
            dp.TransparentBackground := Appearance.Normal.TransparentBackground;
            dp.TransparentFrame := Appearance.Normal.TransparentFrame;
          end;


        end;
        {$endregion Disabled, Hot, Down, Focused, Normal}


        {$region ' ------- Background --------- '}
        // -------------------------------------- BACKGROUND ---------------------------------------

        //Pen.Style := psClear;
        Brush.Style := bsSolid;
        Brush.Color := dp.Color;
        if not dp.TransparentBackground then FillRect(Canvas.ClipRect);

        if dp.bGradientEnabled and (not dp.TransparentBackground) then
        begin

          // ------------- Upper gradient --------------
          BgRect := R;

          if dp.BorderToGradientMargin < 0 then dp.BorderToGradientMargin := 0;
          if dp.UpperGradientPercent > 100 then dp.UpperGradientPercent := 100;

          BgRect.Bottom := Round((dp.UpperGradientPercent * BgRect.Height) / 100);

          BgRect.Left := BgRect.Left + dp.BorderToGradientMargin;
          BgRect.Right := BgRect.Right - dp.BorderToGradientMargin;
          BgRect.Top := BgRect.Top + dp.BorderToGradientMargin;
          xBottomGradientTop := BgRect.Bottom + 0;

          JPP.Gradient.cyGradientFill(
            Canvas,
            BgRect,
            dp.UpperGradientParams.ColorFrom,
            dp.UpperGradientParams.ColorTo,
            dp.UpperGradientParams.Orientation,
            dp.UpperGradientParams.Balance,
            dp.UpperGradientParams.AngleDegree,
            dp.UpperGradientParams.BalanceMode,
            dp.UpperGradientParams.MaxDegrade,
            dp.UpperGradientParams.SpeedPercent
          );

          // -------------- Bottom gradient -------------------
          BgRect := R;
          BgRect.Left := BgRect.Left + dp.BorderToGradientMargin;
          BgRect.Right := BgRect.Right - dp.BorderToGradientMargin;
          BgRect.Bottom := BgRect.Bottom - dp.BorderToGradientMargin;
          BgRect.Top := xBottomGradientTop;

          JPP.Gradient.cyGradientFill(
            Canvas,
            BgRect,
            dp.BottomGradientParams.ColorFrom,
            dp.BottomGradientParams.ColorTo,
            dp.BottomGradientParams.Orientation,
            dp.BottomGradientParams.Balance,
            dp.BottomGradientParams.AngleDegree,
            dp.BottomGradientParams.BalanceMode,
            dp.BottomGradientParams.MaxDegrade,
            dp.BottomGradientParams.SpeedPercent
          );

        end;
        {$endregion Background}


        {$region ' --------- Frame ----------- '}
        // ----------------------------------------- FRAME -----------------------------------------

        Brush.Style := bsClear;

        if not dp.TransparentFrame then
        begin
          if bDefault and Enabled and (not bDown) and (not Focused) then Pen.Assign(Appearance.BorderWhenDefault)
          else Pen.Assign(dp.Pen);
          JppFrame3D(Canvas, R, Pen.Color, Pen.Width);
          //RoundRect(R, xRound, xRound);
        end;
        {$endregion Frame}


        {$region ' --------- Focus Rectangle ----------- '}
        // ---------------------------------------- FOCUS RECTANGLE ----------------------------------------

        if Focused and (not dp.TransparentFrame) then
        begin

          if Appearance.FocusRect.FocusType = frtNone then
          begin
            // do not draw the focus rectangle
          end
          else
          begin
            R2 := R;
            InflateRect(R2, -Appearance.FocusRect.Spacing, -Appearance.FocusRect.Spacing);

            if Appearance.FocusRect.FocusType = frtCustom then
            begin
              Brush.Style := bsClear;
              Pen.Assign(Appearance.FocusRect.Pen);
              JppFrame3D(Canvas, R2, Pen.Color, Pen.Width);
              //RoundRect(R2, xRound, xRound);
            end
            else if Appearance.FocusRect.FocusType = frtSystem then
            begin
              Brush.Style := bsSolid;
              Brush.Color := clBtnFace;
              DrawFocusRect(R2);
            end;
          end;

        end;
        {$endregion Focus rectangle}


        // --------------------------------------------- IMAGE ------------------------------------------------------
        Canvas.Font := dp.Font; // <-- potrzebne aby dopasowaæ pozycjê obrazka i tekstu
        if Appearance.ShowCaption then s := Caption else s := '';

        CalcButtonLayout(
          Canvas, FPngImage, ClientRect, bDown and Appearance.MoveWhenDown, False, s, Layout, Margin, Spacing, GlyphPos, TextPos,
          {$IFDEF DCC}DrawTextBiDiModeFlags(0){$ELSE}0{$ENDIF}
        );

        if (FPngImage <> nil) {and (Kind = bkCustom)} and not FPngImage.Empty then
        begin
          PaintRect := Bounds(GlyphPos.X, GlyphPos.Y, FPngImage.Width, FPngImage.Height);

          if Enabled and (bOver or bDown) then
          begin
            imgHot := TPngImage.Create;
            try
              imgHot.Assign(FPngImage);
              PngSetGamma(imgHot, Appearance.GlyphHotGammaFactor / 100);
              Draw(GlyphPos.X, GlyphPos.Y, imgHot)
            finally
              imgHot.Free;
            end;
          end

          else if Enabled then Draw(GlyphPos.X, GlyphPos.Y, FPngImage)

          else
          // disabled
          begin
            imgDisabled := TPngImage.Create;
            try
              imgDisabled.Assign(FPngImage);
              MakeImageGrayscale(imgDisabled, Appearance.GlyphDisabledGrayscaleFactor);
              MakeImageBlended(imgDisabled, Appearance.GlyphDisabledBlendFactor);
              Draw(GlyphPos.X, GlyphPos.Y, imgDisabled)
            finally
              imgDisabled.Free;
            end;
          end;

        end;


        // ------------------------------------------------- TEXT ------------------------------------------------------------

        if Appearance.ShowCaption and (Length(Caption) > 0) then
        begin
          //Canvas.Font := dp.Font; <-- to musi byæ wykonane przed obliczeniem pozycji obrazka w CalcButtonLayout
          PaintRect := Rect(TextPos.X, TextPos.Y, Width, Height);
          Canvas.Brush.Style := bsClear;
          DrawText(
            Canvas.Handle, PChar(Caption), -1, PaintRect,
            {$IFDEF DCC}DrawTextBiDiModeFlags(0) or {$ENDIF} DT_TOP or DT_LEFT or DT_SINGLELINE
          );
        end;



      end; // with Canvas



    finally
      Canvas.Free;
    end;


  end



  else



  // ------------------------------------------------- Deafult Drawing ---------------------------------------------------------

  begin

    R := ClientRect;
    FCanvas.Handle := Message.DrawItemStruct^.{$IFDEF MSWINDOWS}HDC{$ELSE}_hDC{$ENDIF};
    FCanvas.Font := Self.Font;
    IsDown := Message.DrawItemStruct^.itemState and ODS_SELECTED <> 0;   //IsDown := False;
    IsDefault := Message.DrawItemStruct^.itemState and ODS_FOCUS <> 0;


    //Draw the border
    {$IFDEF DCC}
    if StyleServices.Enabled then
    begin
      //Themed border
      if not Enabled then Button := tbPushButtonDisabled
      else if IsDown then Button := tbPushButtonPressed
      else if FMouseInControl then Button := tbPushButtonHot
      else if IsFocused or IsDefault then Button := tbPushButtonDefaulted
      else Button := tbPushButtonNormal;

      //Paint the background, border, and finally get the inner rect
      Details := StyleServices.GetElementDetails(Button);
      StyleServices.DrawParentBackground(Handle, Message.DrawItemStruct.HDC, @Details, true);
      StyleServices.DrawElement(Message.DrawItemStruct.HDC, Details, Message.DrawItemStruct.rcItem);
      StyleServices.GetElementContentRect(FCanvas.Handle, Details, Message.DrawItemStruct.rcItem, R);
    end

    else
    {$ENDIF}

    begin

      //Draw the outer border, when focused
      if IsFocused or IsDefault then
      begin
        FCanvas.Pen.Color := clWindowFrame;
        FCanvas.Pen.Width := 1;
        FCanvas.Brush.Style := bsClear;
        FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        InflateRect(R, -1, -1);
      end;

      //Draw the inner border
      if IsDown then
      begin
        FCanvas.Pen.Color := clBtnShadow;
        FCanvas.Pen.Width := 1;
        FCanvas.Brush.Color := clBtnFace;
        FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        InflateRect(R, -1, -1);
      end

      else

      begin
        Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
        if Message.DrawItemStruct.itemState and ODS_DISABLED <> 0 then Flags := Flags or DFCS_INACTIVE;
        DrawFrameControl(Message.DrawItemStruct^.{$IFDEF MSWINDOWS}HDC{$ELSE}_hDC{$ENDIF}, R, DFC_BUTTON, Flags);
      end;


      //Adjust the rect when focused and/or down
      if IsFocused then
      begin
        R := ClientRect;
        InflateRect(R, -1, -1);
      end;

      if IsDown then OffsetRect(R, 1, 1);
    end;




    //Calculate the position of the PNG glyph
    if Appearance.ShowCaption then s := Caption else s := '';

    if not FAppearance.MoveWhenDown then
      CalcButtonLayout(
        FCanvas, FPngImage, ClientRect, False, False, s, Layout, Margin, Spacing, GlyphPos, TextPos,
        {$IFDEF DCC}DrawTextBiDiModeFlags(0){$ELSE}0{$ENDIF}
      )
    else
      CalcButtonLayout(
        FCanvas, FPngImage, ClientRect, IsDown, False, s, Layout, Margin, Spacing, GlyphPos, TextPos,
        {$IFDEF DCC}DrawTextBiDiModeFlags(0){$ELSE}0{$ENDIF}
      );

    //Draw the image
    if (FPngImage <> nil) and (Kind = bkCustom) and not FPngImage.Empty then
    begin
      PaintRect := Bounds(GlyphPos.X, GlyphPos.Y, FPngImage.Width, FPngImage.Height);
      if Enabled then DrawPNG(FPngImage, FCanvas, PaintRect, [])
      else DrawPNG(FPngImage, FCanvas, PaintRect, FPngOptions);
    end;

    //Draw the text
    if Appearance.ShowCaption and (Length(Caption) > 0) then
    begin
      PaintRect := Rect(TextPos.X, TextPos.Y, Width, Height);
      FCanvas.Brush.Style := bsClear;
      //grayed Caption when disabled
      if not Enabled then
      begin
        OffsetRect(PaintRect, 1, 1);
        FCanvas.Font.Color := clBtnHighlight;
        DrawText(FCanvas.Handle, PChar(Caption), -1, PaintRect, {$IFDEF DCC}DrawTextBiDiModeFlags(0) or {$ENDIF} DT_TOP or DT_LEFT or DT_SINGLELINE);
        OffsetRect(PaintRect, -1, -1);
        FCanvas.Font.Color := clBtnShadow;
      end;
      DrawText(FCanvas.Handle, PChar(Caption), -1, PaintRect, {$IFDEF DCC}DrawTextBiDiModeFlags(0) or {$ENDIF} DT_TOP or DT_LEFT or DT_SINGLELINE);
    end;

    //Draw the focus rectangle
    if IsFocused and IsDefault then
    begin
      {$IFDEF DCC} if not StyleServices.Enabled then {$ENDIF}
      begin
        R := ClientRect;
        InflateRect(R, -3, -3);
      end;
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;

    FLastKind := Kind;
    FCanvas.Handle := 0;


  end;
  //{$IFDEF FPC}Invalidate;{$ENDIF}
end;
  {$endregion CNDrawItem}


  {$region ' ---------------------- Mouse Enter & Leave --------------------------- '}
procedure TJppPngButton.SetOnMouseEnter(const Value: TNotifyEvent);
begin
  FOnMouseEnter := Value;
end;

procedure TJppPngButton.SetOnMouseLeave(const Value: TNotifyEvent);
begin
  FOnMouseLeave := Value;
end;

procedure TJppPngButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  bOver := True;
  {$IFDEF DCC}
  if StyleServices.Enabled and not FMouseInControl and not(csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    Repaint;
  end
  else
  {$ENDIF}
  begin
    if csDesigning in ComponentState then Exit;
    if Assigned(FOnMouseEnter) then OnMouseEnter(Self);
    Repaint;
  end;
end;

procedure TJppPngButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  bOver := False;
  {$IFDEF DCC}
  if StyleServices.Enabled and FMouseInControl then
  begin
    FMouseInControl := False;
    Repaint;
  end
  else
  {$ENDIF}
  begin
    if csDesigning in ComponentState then Exit;
    if Assigned(FOnMouseLeave) then OnMouseLeave(Self);
    Repaint;
  end;
end;
  {$endregion}


{$endregion TJppPngButton}


{$region ' ------------------------- StyleHook -------------------------- '}
{$IFDEF DCC}

{$IF RTLVersion >= 24.0 }
procedure TJppPngButtonStyleHook.DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean);
const
  WordBreakFlag: array [Boolean] of Integer = (0, DT_WORDBREAK);
var
  Details: TThemedElementDetails;
  DrawRect, PaintRect, TextRect: TRect;
  State: TButtonState;
  btn: TJppPngButton;
  GlyphPos, TextPos: TPoint;

  LColor: TColor;
  LFormats: TTextFormat;
begin
  if not(Control is TJppPngButton) then
  begin
    inherited;
    Exit;
  end;
  if FPressed then Details := StyleServices.GetElementDetails(tbPushButtonPressed)
  else if AMouseInControl then Details := StyleServices.GetElementDetails(tbPushButtonHot)
  else if Focused or TJppPngButton(Control).Default then Details := StyleServices.GetElementDetails(tbPushButtonDefaulted)
  else if Control.Enabled then Details := StyleServices.GetElementDetails(tbPushButtonNormal)
  else Details := StyleServices.GetElementDetails(tbPushButtonDisabled);
  DrawRect := Control.ClientRect;
  StyleServices.DrawElement(ACanvas.Handle, Details, DrawRect);

  btn := Control as TJppPngButton;
  ACanvas.Font := btn.Font;
  if not btn.Enabled then State := bsDisabled
  else if FPressed then State := bsDown
  else State := bsUp;

  //Calculate the position of the PNG glyph
  CalcButtonLayout(ACanvas, btn.FPngImage, btn.ClientRect, FPressed, False, btn.Caption, btn.Layout, btn.Margin, btn.Spacing, GlyphPos, TextPos,
    btn.DrawTextBiDiModeFlags(0));

  //Draw the image
  if (btn.FPngImage <> nil) and (btn.Kind = bkCustom) and not btn.FPngImage.Empty then
  begin
    PaintRect := Bounds(GlyphPos.X, GlyphPos.Y, btn.FPngImage.Width, btn.FPngImage.Height);
    if btn.Enabled then DrawPNG(btn.FPngImage, ACanvas, PaintRect, [])
    else DrawPNG(btn.FPngImage, ACanvas, PaintRect, btn.FPngOptions);
  end;

  ACanvas.Brush.Style := bsClear;
  if (State = bsDisabled) or (not StyleServices.IsSystemStyle and (seFont in btn.StyleElements)) then
  begin
    if not StyleServices.GetElementColor(Details, ecTextColor, LColor) or (LColor = clNone) then LColor := ACanvas.Font.Color;
  end
  else LColor := ACanvas.Font.Color;

  LFormats := TTextFormatFlags(DT_NOCLIP or DT_CENTER or DT_VCENTER or btn.DrawTextBiDiModeFlags(0) or WordBreakFlag[btn.WordWrap]);

  if Length(btn.Caption) > 0 then
  begin
    TextRect := Rect(0, 0, btn.ClientRect.Right - btn.ClientRect.Left, 0);
    DrawText(ACanvas.Handle, PChar(btn.Caption), Length(btn.Caption), TextRect, DT_CALCRECT or btn.DrawTextBiDiModeFlags(0));
  end
  else
  begin
    TextRect := Rect(0, 0, 0, 0);
  end;

  OffsetRect(TextRect, TextPos.X + btn.ClientRect.Left, TextPos.Y + btn.ClientRect.Top);
  StyleServices.DrawText(ACanvas.Handle, Details, btn.Caption, TextRect, LFormats, LColor);

end;
{$IFEND}

{$ENDIF}
{$endregion}



end.
