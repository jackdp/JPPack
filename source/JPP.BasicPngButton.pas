unit JPP.BasicPngButton;

{$I jpp.inc}

interface

uses
  {$IFDEF MSWINDOWS}Windows, Messages,{$ENDIF}
  Classes, SysUtils, {$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF} StrUtils,
  Graphics, Controls, Buttons, GraphUtil, Dialogs, {$IFDEF HAS_UNIT_SCOPE}Vcl.Imaging.pngimage,{$ELSE}pngimage,{$ENDIF}

  PngFunctions,
  JPL.Strings, JPL.Colors,
  JPP.Types, JPP.Common, JPP.Common.Procs, JPP.AnchoredControls, JPP.Gradient, JPP.Graphics
  ;

type


  {$region ' -------------- TJppBasicPngButtonStateParams --------------------- '}
  TJppBasicPngButtonStateParams = class(TPersistent)
  private
    FColor: TColor;
    FFont: TFont;
    FBorder: TPen;
    FGradientEnabled: Boolean;
    FGradient: TJppGradientEx;
    FBorderToGradientMargin: integer;
    FTransparentBackground: Boolean;
    FTransparentFrame: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetBorder(const Value: TPen);
    procedure SetGradientEnabled(const Value: Boolean);
    procedure SetGradient(const Value: TJppGradientEx);
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
    property Gradient: TJppGradientEx read FGradient write SetGradient;
    property GradientEnabled: Boolean read FGradientEnabled write SetGradientEnabled default True;
    property BorderToGradientMargin: integer read FBorderToGradientMargin write SetBorderToGradientMargin default 2;
    property TransparentBackground: Boolean read FTransparentBackground write SetTransparentBackground default False;
    property TransparentFrame: Boolean read FTransparentFrame write SetTransparentFrame default False;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}


  {$region ' ----------- TJppBasicPngButtonAppearance ---------- '}
  TJppBasicPngButtonAppearance = class(TPersistent)
  private
    FNormal: TJppBasicPngButtonStateParams;
    FHot: TJppBasicPngButtonStateParams;
    FDefaultDrawing: Boolean;
    FFocusRect: TJppFocusRectParams;
    FBorderWhenDefault: TPen;
    FDown: TJppBasicPngButtonStateParams;
    FDisabled: TJppBasicPngButtonStateParams;
    FGlyphDisabledGrayscaleFactor: Byte;
    FGlyphDisabledBlendFactor: Byte;
    FMoveWhenDown: Boolean;
    FGlyphHotGammaFactor: Byte;
    FOnChange: TNotifyEvent;
    FFocused: TJppBasicPngButtonStateParams;
    FShowCaption: Boolean;
    procedure SetNormal(const Value: TJppBasicPngButtonStateParams);
    procedure SetHot(const Value: TJppBasicPngButtonStateParams);
    procedure SetDefaultDrawing(const Value: Boolean);
    procedure SetFocusRect(const Value: TJppFocusRectParams);
    procedure SetBorderWhenDefault(const Value: TPen);
    procedure SetDown(const Value: TJppBasicPngButtonStateParams);
    procedure SetDisabled(const Value: TJppBasicPngButtonStateParams);
    procedure SetGlyphDisabledGrayscaleFactor(const Value: Byte);
    procedure SetGlyphDisabledBlendFactor(const Value: Byte);
    procedure SetMoveWhenDown(const Value: Boolean);
    procedure SetGlyphHotGammaFactor(const Value: Byte);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetFocused(const Value: TJppBasicPngButtonStateParams);
    procedure SetShowCaption(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppBasicPngButtonAppearance); reintroduce;
    procedure PropsChanged(Sender: TObject);
    procedure SetFontName(const FontName: string);
    procedure SetFontSize(const FontSize: integer);
    procedure SetFontStyles(const FontStyle: TFontStyles);
  published
    property DefaultDrawing: Boolean read FDefaultDrawing write SetDefaultDrawing default False;
    property Normal: TJppBasicPngButtonStateParams read FNormal write SetNormal;
    property Hot: TJppBasicPngButtonStateParams read FHot write SetHot;
    property Down: TJppBasicPngButtonStateParams read FDown write SetDown;
    property Disabled: TJppBasicPngButtonStateParams read FDisabled write SetDisabled;
    property Focused: TJppBasicPngButtonStateParams read FFocused write SetFocused;
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



  //TJppBasicPngButtonTagExt = class(TJppTagExt);

  {$region ' -------------- TJppBasicPngButton -------------- '}
  TJppBasicPngButton = class(TBitBtn)
  {$IF RTLVersion >= 24.0 }
  strict private
    class constructor Create;
    class destructor Destroy;
  {$IFEND}
  private
    bOver: Boolean;
    FPngImage: TPngImage;
    FPngOptions: TPngOptions;
    FCanvas: TCanvas;
    FLastKind: TBitBtnKind;
    FImageFromAction: Boolean;
    FMouseInControl: Boolean;
    IsFocused: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FTagExt: TJppTagExt;
    FAppearance: TJppBasicPngButtonAppearance;
    FAnchoredControls: TJppAnchoredControls;
    function PngImageStored: Boolean;
    procedure SetPngImage(const Value: TPngImage);
    procedure SetPngOptions(const Value: TPngOptions);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetOnMouseEnter(const Value: TNotifyEvent);
    procedure SetOnMouseLeave(const Value: TNotifyEvent);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetAppearance(const Value: TJppBasicPngButtonAppearance);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure PropsChanged(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property PngImage: TPngImage read FPngImage write SetPngImage stored PngImageStored;
    property PngOptions: TPngOptions read FPngOptions write SetPngOptions default [pngBlendOnDisabled];
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write SetOnMouseLeave;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property Appearance: TJppBasicPngButtonAppearance read FAppearance write SetAppearance;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  end;
  {$endregion TJppBasicPngButton}


  {$IF RTLVersion >= 24.0 }
  TJppBasicPngButtonStyleHook = class(TBitBtnStyleHook)
  strict protected
    procedure DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean); override;
  end;
  {$IFEND}




procedure SetJppBasicPngButtonFonts(Button: TJppBasicPngButton; FontName: string = 'Segoe UI'; FontSize: integer = 8);


implementation


uses
  ActnList, Themes, PngButtonFunctions, PngImageList;




{$region ' -------------- helpers ------------ '}

procedure SetJppBasicPngButtonFonts(Button: TJppBasicPngButton; FontName: string = 'Segoe UI'; FontSize: integer = 8);
begin
  Button.Font.Name := FontName;
  Button.Font.Size := FontSize;
  Button.Appearance.SetFontName(FontName);
  Button.Appearance.SetFontSize(FontSize);
end;

{$endregion}

{$region ' ---------------- Themes ------------------------- '}
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



{$IFDEF DELPHIXE3_OR_ABOVE}
class constructor TJppBasicPngButton.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TJppBasicPngButton, TJppBasicPngButtonStyleHook);
end;

class destructor TJppBasicPngButton.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TJppBasicPngButton, TJppBasicPngButtonStyleHook);
end;
{$ENDIF}
{$endregion Themes}



{$region ' -------------------- TJppBasicPngButtonStateParams ---------------------- '}
constructor TJppBasicPngButtonStateParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FGradient := TJppGradientEx.Create(AOwner);
  FGradientEnabled := True;
  FBorderToGradientMargin := 2;
  FFont := TFont.Create;
  FBorder := TPen.Create;
  FTransparentBackground := False;
  FTransparentFrame := False;

  FGradient.OnChange := PropsChanged;
  FFont.OnChange := PropsChanged;
  FBorder.OnChange := PropsChanged;
end;

destructor TJppBasicPngButtonStateParams.Destroy;
begin
  FGradient.Free;
  FFont.Free;
  FBorder.Free;
  inherited;
end;

procedure TJppBasicPngButtonStateParams.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppBasicPngButtonStateParams.SetBorder(const Value: TPen);
begin
  FBorder := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonStateParams.SetBorderToGradientMargin(const Value: integer);
begin
  FBorderToGradientMargin := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonStateParams.SetColor(const Value: TColor);
begin
  FColor := Value;
  PropsChanged(Self);
end;


procedure TJppBasicPngButtonStateParams.SetFont(const Value: TFont);
begin
  //FFont := Value;
  FFont.Assign(Value);
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonStateParams.SetGradientEnabled(const Value: Boolean);
begin
  FGradientEnabled := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonStateParams.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppBasicPngButtonStateParams.SetTransparentBackground(const Value: Boolean);
begin
  FTransparentBackground := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonStateParams.SetTransparentFrame(const Value: Boolean);
begin
  FTransparentFrame := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonStateParams.SetGradient(const Value: TJppGradientEx);
begin
  FGradient := Value;
  PropsChanged(Self);
end;


{$endregion}


{$region ' --------------------- TJppBasicPngButtonAppearance -------------------- '}
constructor TJppBasicPngButtonAppearance.Create(AOwner: TComponent);
begin
  inherited Create;

  FNormal := TJppBasicPngButtonStateParams.Create(AOwner);
  FNormal.Color := $00F3F3F3;
  FNormal.Gradient.ColorFrom := $00F1F1F1;
  FNormal.Gradient.ColorTo := $00CFCFCF;
  FNormal.Border.Color := $00707070;
  FNormal.Font.Color := 0;

  FHot := TJppBasicPngButtonStateParams.Create(AOwner);
  FHot.Color := $00FCF5E8;
  FHot.Gradient.ColorFrom := $00FDF6EA;
  FHot.Gradient.ColorTo := $00F5D9A7;
  FHot.Border.Color := $00B17F3C;
  FHot.Font.Color := 0;

  FFocused := TJppBasicPngButtonStateParams.Create(AOwner);
  FFocused.Color := FNormal.Color;
  FFocused.Gradient.ColorFrom := FNormal.Gradient.ColorFrom;
  FFocused.Gradient.ColorTo := FNormal.Gradient.ColorTo;
  FFocused.Border.Color := $00D0AA24;
  FFocused.Font.Color := FNormal.Font.Color;

  FDown := TJppBasicPngButtonStateParams.Create(AOwner);
  FDown.Color := $00F1E2C5;
  FDown.Gradient.ColorFrom := $00FCF4E5;
  FDown.Gradient.ColorTo := $00DFB972;
  FDown.Border.Color := GetSimilarColor(FHot.Border.Color , 50, False);
  FDown.Font.Color := 0;

  FDisabled := TJppBasicPngButtonStateParams.Create(AOwner);
  FDisabled.Color := $00F4F4F4;
  FDisabled.Gradient.ColorFrom := $00F4F4F4;
  FDisabled.Gradient.ColorTo := $00F4F4F4;
  FDisabled.Border.Color := $00B5B2AD;
  FDisabled.Font.Color := RGB(160,160,160);

  FFocusRect := TJppFocusRectParams.Create(AOwner);
  FFocusRect.Pen.Color := $00D0AA24;

  FBorderWhenDefault := TPen.Create;
  FBorderWhenDefault.Color := $00D0AA24;

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

destructor TJppBasicPngButtonAppearance.Destroy;
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

procedure TJppBasicPngButtonAppearance.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppBasicPngButtonAppearance.Assign(Source: TJppBasicPngButtonAppearance);
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
  Normal.Gradient.Assign(Source.Normal.Gradient);
  Normal.BorderToGradientMargin := Source.Normal.BorderToGradientMargin;
  Normal.TransparentBackground := Source.Normal.TransparentBackground;
  Normal.TransparentFrame := Source.Normal.TransparentFrame;

  Hot.Font.Assign(Source.Hot.Font);
  Hot.Border.Assign(Source.Hot.Border);
  Hot.Color := Source.Hot.Color;
  Hot.GradientEnabled := Source.Hot.GradientEnabled;
  Hot.Gradient.Assign(Source.Hot.Gradient);
  Hot.BorderToGradientMargin := Source.Hot.BorderToGradientMargin;
  Hot.TransparentBackground := Source.Hot.TransparentBackground;
  Hot.TransparentFrame := Source.Hot.TransparentFrame;

  Down.Font.Assign(Source.Down.Font);
  Down.Border.Assign(Source.Down.Border);
  Down.Color := Source.Down.Color;
  Down.GradientEnabled := Source.Down.GradientEnabled;
  Down.Gradient.Assign(Source.Down.Gradient);
  Down.BorderToGradientMargin := Source.Down.BorderToGradientMargin;
  Down.TransparentBackground := Source.Down.TransparentBackground;
  Down.TransparentFrame := Source.Down.TransparentFrame;

  Focused.Font.Assign(Source.Focused.Font);
  Focused.Border.Assign(Source.Focused.Border);
  Focused.Color := Source.Focused.Color;
  Focused.GradientEnabled := Source.Focused.GradientEnabled;
  Focused.Gradient.Assign(Source.Focused.Gradient);
  Focused.BorderToGradientMargin := Source.Focused.BorderToGradientMargin;
  Focused.TransparentBackground := Source.Focused.TransparentBackground;
  Focused.TransparentFrame := Source.Focused.TransparentFrame;

  Disabled.Font.Assign(Source.Disabled.Font);
  Disabled.Border.Assign(Source.Disabled.Border);
  Disabled.Color := Source.Disabled.Color;
  Disabled.GradientEnabled := Source.Disabled.GradientEnabled;
  Disabled.Gradient.Assign(Source.Disabled.Gradient);
  Disabled.BorderToGradientMargin := Source.Disabled.BorderToGradientMargin;
  Disabled.TransparentBackground := Source.Disabled.TransparentBackground;
  Disabled.TransparentFrame := Source.Disabled.TransparentFrame;

end;

procedure TJppBasicPngButtonAppearance.SetFontName(const FontName: string);
begin
  FNormal.Font.Name := FontName;
  FHot.Font.Name := FontName;
  FDown.Font.Name := FontName;
  FFocused.Font.Name := FontName;
  FDisabled.Font.Name := FontName;
end;

procedure TJppBasicPngButtonAppearance.SetFontSize(const FontSize: integer);
begin
  FNormal.Font.Size := FontSize;
  FHot.Font.Size := FontSize;
  FDown.Font.Size := FontSize;
  FFocused.Font.Size := FontSize;
  FDisabled.Font.Size := FontSize;
end;

procedure TJppBasicPngButtonAppearance.SetFontStyles(const FontStyle: TFontStyles);
begin
  FNormal.Font.Style := FontStyle;
  FHot.Font.Style := FontStyle;
  FDown.Font.Style := FontStyle;
  FFocused.Font.Style := FontStyle;
  FDisabled.Font.Style := FontStyle;
end;

procedure TJppBasicPngButtonAppearance.SetBorderWhenDefault(const Value: TPen);
begin
  FBorderWhenDefault := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetDefaultDrawing(const Value: Boolean);
begin
  FDefaultDrawing := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetDisabled(const Value: TJppBasicPngButtonStateParams);
begin
  FDisabled := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetGlyphDisabledBlendFactor(const Value: Byte);
begin
  FGlyphDisabledBlendFactor := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetGlyphDisabledGrayscaleFactor(const Value: Byte);
begin
  FGlyphDisabledGrayscaleFactor := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetDown(const Value: TJppBasicPngButtonStateParams);
begin
  FDown := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetFocusRect(const Value: TJppFocusRectParams);
begin
  FFocusRect := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetFocused(const Value: TJppBasicPngButtonStateParams);
begin
  FFocused := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetHot(const Value: TJppBasicPngButtonStateParams);
begin
  FHot := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetGlyphHotGammaFactor(const Value: Byte);
begin
  FGlyphHotGammaFactor := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetMoveWhenDown(const Value: Boolean);
begin
  FMoveWhenDown := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetNormal(const Value: TJppBasicPngButtonStateParams);
begin
  FNormal := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonAppearance.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
  PropsChanged(Self);
end;

{$endregion}



{$region ' -------------------------------- TJppBasicPngButton ------------------------------------- '}


  {$region ' ---------------------- Cretae & Destroy ----------------------------- '}
constructor TJppBasicPngButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPngImage := TPngImage.Create;
  FPngOptions := [pngBlendOnDisabled];
  FCanvas := TCanvas.Create;
  FLastKind := bkCustom;
  FImageFromAction := False;
  FTagExt := TJppTagExt.Create(Self);
  FAppearance := TJppBasicPngButtonAppearance.Create(Self);
  FAppearance.OnChange := PropsChanged;
  bOver := False;
  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppBasicPngButton.Destroy;
begin
  FPngImage.Free;
  FCanvas.Free;
  FTagExt.Free;
  FAppearance.Free;
  FAnchoredControls.Free;
  inherited Destroy;
end;



{$endregion Create & Destroy}


procedure TJppBasicPngButton.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppBasicPngButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppBasicPngButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if not (csDestroying in ComponentState) then
      if Assigned(FAnchoredControls) then
      begin
        if AComponent = FAnchoredControls.Top.Control then FAnchoredControls.Top.Control := nil
        else if AComponent = FAnchoredControls.Bottom.Control then FAnchoredControls.Bottom.Control := nil
        else if AComponent = FAnchoredControls.Left.Control then FAnchoredControls.Left.Control := nil
        else if AComponent = FAnchoredControls.Right.Control then FAnchoredControls.Right.Control := nil;
      end;
end;


  {$region ' --------------------------------- misc ------------------------------------ '}
procedure TJppBasicPngButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
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


procedure TJppBasicPngButton.SetAppearance(const Value: TJppBasicPngButtonAppearance);
begin
  FAppearance := Value;
end;

procedure TJppBasicPngButton.SetButtonStyle(ADefault: Boolean);
begin
  inherited SetButtonStyle(ADefault);
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Refresh;
  end;
end;

function TJppBasicPngButton.PngImageStored: Boolean;
begin
  Result := not FImageFromAction;
end;

procedure TJppBasicPngButton.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;

procedure TJppBasicPngButton.SetPngImage(const Value: TPngImage);
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

  //To work around the gamma-problem
  with FPngImage do
    if not Empty and (Header.ColorType in [COLOR_RGB, COLOR_RGBALPHA, COLOR_PALETTE]) then Chunks.RemoveChunk(Chunks.ItemFromClass(TChunkgAMA));

  FImageFromAction := False;
  Repaint;
end;

procedure TJppBasicPngButton.SetPngOptions(const Value: TPngOptions);
begin
  if FPngOptions <> Value then
  begin
    FPngOptions := Value;
    Repaint;
  end;
end;

procedure TJppBasicPngButton.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;


{$endregion misc}


  {$region ' -------------------------------------------------------- CNDrawItem ------------------------------------------------------------ '}
procedure TJppBasicPngButton.CNDrawItem(var Message: TWMDrawItem);

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
    GradientParams: TGradientParams;
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
  Button: TThemedButton;
  Details: TThemedElementDetails;

  bDown, bDefault: Boolean;
  R, R2, BgRect: TRect;
  Canvas: TCanvas;
  //xRound: integer;
  imgDisabled, imgHot: TPngImage;
  dp: TDrawingParams;
  //xBottomGradientTop: integer;
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

      Canvas.Handle := Message.DrawItemStruct^.hDC;
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
            CopyGradientParams(dp.GradientParams, Appearance.Disabled.Gradient);
          end;
          dp.Color := Appearance.Disabled.Color;
          dp.Pen := Appearance.Disabled.Border;
          dp.Font := Appearance.Disabled.Font;
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
              CopyGradientParams(dp.GradientParams, Appearance.Hot.Gradient);
            end;
            dp.Color := Appearance.Hot.Color;
            dp.Pen := Appearance.Hot.Border;
            dp.Font := Appearance.Hot.Font;
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
              CopyGradientParams(dp.GradientParams, Appearance.Down.Gradient);
            end;
            dp.Color := Appearance.Down.Color;
            dp.Pen := Appearance.Down.Border;
            dp.Font := Appearance.Down.Font;
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
              CopyGradientParams(dp.GradientParams, Appearance.Focused.Gradient);
            end;
            dp.Color := Appearance.Focused.Color;
            dp.Pen := Appearance.Focused.Border;
            dp.Font := Appearance.Focused.Font;
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
              CopyGradientParams(dp.GradientParams, Appearance.Normal.Gradient);
            end;
            dp.Color := Appearance.Normal.Color;
            dp.Pen := Appearance.Normal.Border;
            dp.Font := Appearance.Normal.Font;
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

          // ------------- Gradient --------------
          BgRect := R;

          if dp.BorderToGradientMargin < 0 then dp.BorderToGradientMargin := 0;
          //if dp.UpperGradientPercent > 100 then dp.UpperGradientPercent := 100;

          //BgRect.Bottom := Round((dp.UpperGradientPercent * BgRect.Height) / 100);
          BgRect.Bottom := BgRect.Bottom - dp.BorderToGradientMargin;

          BgRect.Left := BgRect.Left + dp.BorderToGradientMargin;
          BgRect.Right := BgRect.Right - dp.BorderToGradientMargin;
          BgRect.Top := BgRect.Top + dp.BorderToGradientMargin;
          //xBottomGradientTop := BgRect.Bottom + 0;

          Jpp.Gradient.cyGradientFill(
            Canvas,
            BgRect,
            dp.GradientParams.ColorFrom,
            dp.GradientParams.ColorTo,
            dp.GradientParams.Orientation,
            dp.GradientParams.Balance,
            dp.GradientParams.AngleDegree,
            dp.GradientParams.BalanceMode,
            dp.GradientParams.MaxDegrade,
            dp.GradientParams.SpeedPercent
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
        CalcButtonLayout(Canvas, FPngImage, ClientRect, bDown and Appearance.MoveWhenDown, False, s, Layout, Margin, Spacing, GlyphPos, TextPos, DrawTextBiDiModeFlags(0));
        if (FPngImage <> nil) {and (Kind = bkCustom)} and not FPngImage.Empty then
        begin
          PaintRect := Bounds(GlyphPos.X, GlyphPos.Y, FPngImage.Width, FPngImage.Height);

          if Enabled and (bOver or bDown) then
          begin
            imgHot := TPngImage.Create;
            try
              //if not FPngImage.Empty then imgHot.SetSize(FPngImage.Width, FPngImage.Height);
              imgHot.Assign(FPngImage);
              PngSetGamma(imgHot, Appearance.GlyphHotGammaFactor / 100);
              //SetPngGamma(Appearance.GlyphHotGammaFactor / 100, imgHot);
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
          DrawText(Canvas.Handle, PChar(Caption), -1, PaintRect, DrawTextBiDiModeFlags(0) or DT_TOP or DT_LEFT or DT_SINGLELINE);
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
    FCanvas.Handle := Message.DrawItemStruct^.HDC;
    FCanvas.Font := Self.Font;
    IsDown := Message.DrawItemStruct^.itemState and ODS_SELECTED <> 0;   //IsDown := False;
    IsDefault := Message.DrawItemStruct^.itemState and ODS_FOCUS <> 0;


    //Draw the border
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
        DrawFrameControl(Message.DrawItemStruct.HDC, R, DFC_BUTTON, Flags);
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
    if not FAppearance.MoveWhenDown then CalcButtonLayout(FCanvas, FPngImage, ClientRect, False, False, s, Layout, Margin, Spacing, GlyphPos, TextPos, DrawTextBiDiModeFlags(0))
    else CalcButtonLayout(FCanvas, FPngImage, ClientRect, IsDown, False, s, Layout, Margin, Spacing, GlyphPos, TextPos, DrawTextBiDiModeFlags(0));

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
        DrawText(FCanvas.Handle, PChar(Caption), -1, PaintRect, DrawTextBiDiModeFlags(0) or DT_TOP or DT_LEFT or DT_SINGLELINE);
        OffsetRect(PaintRect, -1, -1);
        FCanvas.Font.Color := clBtnShadow;
      end;
      DrawText(FCanvas.Handle, PChar(Caption), -1, PaintRect, DrawTextBiDiModeFlags(0) or DT_TOP or DT_LEFT or DT_SINGLELINE);
    end;

    //Draw the focus rectangle
    if IsFocused and IsDefault then
    begin
      if not StyleServices.Enabled then
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
end;
  {$endregion CNDrawItem}


  {$region ' ---------------------- Mouse Enter & Leave --------------------------- '}
procedure TJppBasicPngButton.SetOnMouseEnter(const Value: TNotifyEvent);
begin
  FOnMouseEnter := Value;
end;

procedure TJppBasicPngButton.SetOnMouseLeave(const Value: TNotifyEvent);
begin
  FOnMouseLeave := Value;
end;

procedure TJppBasicPngButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  bOver := True;
  if StyleServices.Enabled and not FMouseInControl and not(csDesigning in ComponentState) then
  begin
    FMouseInControl := true;
    Repaint;
  end
  else
  begin
    if csDesigning in ComponentState then Exit;
    if Assigned(FOnMouseEnter) then OnMouseEnter(Self);
    Repaint;
  end;
end;

procedure TJppBasicPngButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  bOver := False;
  if StyleServices.Enabled and FMouseInControl then
  begin
    FMouseInControl := False;
    Repaint;
  end
  else
  begin
    if csDesigning in ComponentState then Exit;
    if Assigned(FOnMouseLeave) then OnMouseLeave(Self);
    Repaint;
  end;
end;
  {$endregion Mouse Enter & Leave}


{$endregion TJppBasicPngButton}


{$region ' ------------------------- StyleHook -------------------------- '}
{$IF RTLVersion >= 24.0 }
procedure TJppBasicPngButtonStyleHook.DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean);
const
  WordBreakFlag: array [Boolean] of Integer = (0, DT_WORDBREAK);
var
  Details: TThemedElementDetails;
  DrawRect, PaintRect, TextRect: TRect;
  State: TButtonState;
  btn: TJppBasicPngButton;
  GlyphPos, TextPos: TPoint;

  LColor: TColor;
  LFormats: TTextFormat;
begin
  if not(Control is TJppBasicPngButton) then
  begin
    inherited;
    Exit;
  end;
  if FPressed then Details := StyleServices.GetElementDetails(tbPushButtonPressed)
  else if AMouseInControl then Details := StyleServices.GetElementDetails(tbPushButtonHot)
  else if Focused or TJppBasicPngButton(Control).Default then Details := StyleServices.GetElementDetails(tbPushButtonDefaulted)
  else if Control.Enabled then Details := StyleServices.GetElementDetails(tbPushButtonNormal)
  else Details := StyleServices.GetElementDetails(tbPushButtonDisabled);
  DrawRect := Control.ClientRect;
  StyleServices.DrawElement(ACanvas.Handle, Details, DrawRect);

  btn := Control as TJppBasicPngButton;
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
{$endregion}



end.
