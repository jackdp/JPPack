unit JPP.Common;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Buttons, Vcl.Graphics, Vcl.Dialogs, System.UITypes,
  JPP.Types, JPP.Gradient, JPP.Graphics;

type

  TJppFocusRectType = (frtSystem, frtCustom, frtNone);

  {$region ' --------- TJppTagExt ------------- '}
  TJppTagExt = class(TPersistent)
  private
    FIntValue: integer;
    FStrValue: string;
    FRealValue: Real;
    FDateValue: TDateTime;
    FPointerValue: Pointer;
    procedure SetIntValue(const Value: integer);
    procedure SetStrValue(const Value: string);
    procedure SetRealValue(const Value: Real);
    procedure SetDateValue(const Value: TDateTime);
    procedure SetPointerValue(const Value: Pointer);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property PointerValue: Pointer read FPointerValue write SetPointerValue;
    property DateValue: TDateTime read FDateValue write SetDateValue;
  published
    property IntValue: integer read FIntValue write SetIntValue default 0;
    property StrValue: string read FStrValue write SetStrValue;
    property RealValue: Real read FRealValue write SetRealValue;
  end;
  {$endregion}

  {$region ' ---------- TJppGradient -------------- '}
  TJppGradient = class(TPersistent)
  private
    FColorStart: TColor;
    FColorEnd: TColor;
    FGradientType: TJppGradientType;
    FSteps: Byte;
    FOnChange: TNotifyEvent;
    procedure SetColorStart(const Value: TColor);
    procedure SetColorEnd(const Value: TColor);
    procedure SetGradientType(const Value: TJppGradientType);
    procedure SetSteps(const Value: Byte);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property ColorStart: TColor read FColorStart write SetColorStart;
    property ColorEnd: TColor read FColorEnd write SetColorEnd;
    property GradientType: TJppGradientType read FGradientType write SetGradientType;
    property Steps: Byte read FSteps write SetSteps default 255;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  {$endregion}

  {$region ' --------------- TJppGradientEx ---------------- '}
  // TcyGradient extracted from VCL.cyClasses.pas
  // Class name changed to TJppGradientEx to avoid conflicts
  // FromColor -> ColorFrom
  // ToColor -> ColorTo
  TJppGradientEx = class(TPersistent)
  private
    FAngleViewCache: TBitmap; // Buffer with angle gradient
    FAngleViewModified: Boolean;
    FAngleClipRect: Boolean;
    FBalance: Word;
    FColorFrom: TColor;
    FColorTo: TColor;
    FOrientation: TDgradOrientation;
    FOnChange: TNotifyEvent;
    FAngleDegree: Word;
    FBalanceMode: TDgradBalanceMode;
    FMaxDegrade: byte;
    FSpeedPercent: Integer;
    procedure SetBalance(const Value: Word);
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetOrientation(const Value: TDgradOrientation);
    procedure SetAngleDegree(const Value: Word);
    procedure SetBalanceMode(const Value: TDgradBalanceMode);
    procedure SetMaxDegrade(const Value: byte);
    procedure SetSpeedPercent(const Value: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(aCanvas: TCanvas; aRect: TRect);
    property AngleClipRect: Boolean read FAngleClipRect write FAngleClipRect default false;
  published
    property AngleDegree: Word read FAngleDegree Write SetAngleDegree default 45;
    property Balance: Word read FBalance write SetBalance default 50;
    property BalanceMode: TDgradBalanceMode read FBalanceMode write SetBalanceMode default bmNormal;
    property MaxDegrade: byte read FMaxDegrade write SetMaxDegrade default 255;
    property Orientation: TDgradOrientation read FOrientation write SetOrientation default dgdVertical;
    property SpeedPercent: Integer read FSpeedPercent write SetSpeedPercent; // default 100; Cannot set default because SpeedPercent modified in some components
    property ColorFrom: TColor read FColorFrom write SetColorFrom;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  {$endregion}

  {$region ' ------- TJppFocusRectParams ---------- '}
  TJppFocusRectParams = class(TPersistent)
  private
    FPen: TPen;
    FSpacing: integer;
    FFocusType: TJppFocusRectType;
    FOnChange: TNotifyEvent;
    procedure SetPen(const Value: TPen);
    procedure SetSpacing(const Value: integer);
    procedure SetFocusType(const Value: TJppFocusRectType);
    procedure SetOnChange(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  protected
    procedure PropsChanged(Sender: TObject);
  published
    property FocusType: TJppFocusRectType read FFocusType write SetFocusType default frtNone;
    property Pen: TPen read FPen write SetPen;
    property Spacing: integer read FSpacing write SetSpacing default 2;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}

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


  
implementation


{$region ' ------------------------------ TJppTagExt ------------------------------ '}
constructor TJppTagExt.Create(AOwner: TComponent);
begin
  inherited Create;
  StrValue := '';
  RealValue := 0;
  PointerValue := nil;
  DateValue := Now;
end;

destructor TJppTagExt.Destroy;
begin
  inherited;
end;


procedure TJppTagExt.SetDateValue(const Value: TDateTime);
begin
  FDateValue := Value;
end;

procedure TJppTagExt.SetIntValue(const Value: integer);
begin
  FIntValue := Value;
end;

procedure TJppTagExt.SetPointerValue(const Value: Pointer);
begin
  FPointerValue := Value;
end;

procedure TJppTagExt.SetRealValue(const Value: Real);
begin
  FRealValue := Value;
end;

procedure TJppTagExt.SetStrValue(const Value: string);
begin
  FStrValue := Value;
end;
{$endregion}

{$region ' ---------------------- TJppGradient --------------------------- '}
constructor TJppGradient.Create(AOwner: TComponent);
begin
  inherited Create;
  ColorStart := clWindow;
  ColorEnd := clBtnFace;
  GradientType := gtHorizontalBar;
  Steps := 255;
end;

destructor TJppGradient.Destroy;
begin
  inherited;
end;

procedure TJppGradient.SetColorEnd(const Value: TColor);
begin
  FColorEnd := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJppGradient.SetColorStart(const Value: TColor);
begin
  FColorStart := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJppGradient.SetGradientType(const Value: TJppGradientType);
begin
  FGradientType := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJppGradient.SetSteps(const Value: Byte);
begin
  FSteps := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{$endregion TJppGradient}


{$region ' ---------------------------- TJppFocusRectParams ---------------------------------- '}
constructor TJppFocusRectParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FPen := TPen.Create;
  FSpacing := 2;
  FFocusType := frtNone;
  FPen.OnChange := PropsChanged;
end;

destructor TJppFocusRectParams.Destroy;
begin
  FPen.Free;
  inherited;
end;

procedure TJppFocusRectParams.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppFocusRectParams.SetPen(const Value: TPen);
begin
  FPen := Value;
  PropsChanged(Self);
end;

procedure TJppFocusRectParams.SetFocusType(const Value: TJppFocusRectType);
begin
  FFocusType := Value;
  PropsChanged(Self);
end;

procedure TJppFocusRectParams.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppFocusRectParams.SetSpacing(const Value: integer);
begin
  FSpacing := Value;
  PropsChanged(Self);
end;

{$endregion}

{$region ' ------------------------------- TJppItemStateParams ------------------------------------------- '}
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

{$region ' ------------------------------- TJppButtonAppearance ------------------------------------- '}
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
  FDisabled.Color := $00F4F4F4; //$00FCFCFC;
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

{$region ' ------------------------------------ TJppGradientEx -------------------------------- '}
constructor TJppGradientEx.Create(AOwner: TComponent);
begin
  FBalance := 50;
  FColorFrom := clWhite;
  FColorTo := clBtnFace;
  FOrientation := dgdVertical;
  FBalanceMode := bmNormal;
  FMaxDegrade := 255;
  FSpeedPercent := 100;
  FAngleDegree := 45;
  FAngleViewModified := true;
  FAngleViewCache := Nil;
  FAngleClipRect := false;
end;

destructor TJppGradientEx.Destroy;
begin
  if Assigned(FAngleViewCache) then
  begin
    FAngleViewCache.Free;
    FAngleViewCache := Nil;
  end;

  inherited;
end;

procedure TJppGradientEx.Draw(aCanvas: TCanvas; aRect: TRect);
var
  Size: Integer;
  Rgn: HRGN;
  P: TPoint;
begin
  Rgn := 0;
  if FColorFrom = FColorTo then
  begin
    aCanvas.Brush.Color := FColorFrom;
    aCanvas.Brush.Style := bsSolid;
    aCanvas.FillRect(aRect);
  end
  else
  begin
    if FOrientation = dgdAngle then
    begin
      // We use FAngleViewCache bitmap as an image buffer in order to avoid recalculating gradient for each time Draw() is called :
      Size := Round( Sqrt( Sqr(aRect.Right - aRect.Left) + Sqr(aRect.Bottom - aRect.Top) ) );

      if FAngleViewModified or (FAngleViewCache.Width <> Size) then
      begin
        cyGradientFill(aCanvas, aRect, FColorFrom, FColorTo, FOrientation, FBalance, FAngleDegree, FBalanceMode, FMaxDegrade, FSpeedPercent, FAngleClipRect, FAngleViewCache);
        FAngleViewModified := false;
      end
      else begin
        if FAngleClipRect then
        begin
          Rgn := CreateRectRgn(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
          GetWindowOrgEx(aCanvas.Handle, P);
          OffsetRgn(Rgn, -P.X, -P.Y);
          SelectClipRgn(aCanvas.Handle, Rgn);
        end;
        aCanvas.Draw( (aRect.Left+(aRect.Right-aRect.Left) - FAngleViewCache.Width) div 2, (aRect.Top+(aRect.Bottom-aRect.Top) - FAngleViewCache.Height) div 2, FAngleViewCache );
        if FAngleClipRect then
        begin
          SelectClipRgn(aCanvas.Handle, HRGN(nil));
          DeleteObject(Rgn);
        end;
      end;
    end
    else
      cyGradientFill(aCanvas, aRect, FColorFrom, FColorTo, FOrientation, FBalance, FAngleDegree, FBalanceMode, FMaxDegrade, FSpeedPercent);
  end;
end;

procedure TJppGradientEx.Assign(Source: TPersistent);
begin
  if Source is TJppGradientEx
  then begin
    FAngleDegree := TJppGradientEx(Source).AngleDegree;
    FBalance := TJppGradientEx(Source).Balance;
    FColorFrom := TJppGradientEx(Source).ColorFrom;
    FColorTo := TJppGradientEx(Source).ColorTo;
    FBalanceMode := TJppGradientEx(Source).BalanceMode;
    FOrientation := TJppGradientEx(Source).Orientation;
    FSpeedPercent := TJppGradientEx(Source).SpeedPercent;
    FAngleViewModified := true;
    { Do not assign
    FAngleViewCache
    FAngleClipRect
    }

    if Assigned(FOnChange) then FOnChange(Self);
  end;
//  inherited Assign(Source);
end;

procedure TJppGradientEx.SetColorFrom(const Value: TColor);
begin
  if Value <> FColorFrom then
  begin
    FColorFrom:= Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJppGradientEx.SetColorTo(const Value: TColor);
begin
  if Value <> FColorTo then
  begin
    FColorTo:= Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJppGradientEx.SetOrientation(const Value: TDgradOrientation);
begin
  if Value = fOrientation then Exit;

  FOrientation := Value;
  FAngleViewModified := true;

  if Value <> dgdAngle then
  begin
    if Assigned(FAngleViewCache) then
    begin
      FAngleViewCache.Free;
      FAngleViewCache := Nil;
    end;
  end
  else
    FAngleViewCache := TBitmap.Create;

  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJppGradientEx.SetSpeedPercent(const Value: Integer);
begin
  if (Value <> FSpeedPercent) and (Value In [0..100]) then
  begin
    FSpeedPercent := Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJppGradientEx.SetBalance(const Value: Word);
begin
  if (Value <> FBalance) and (Value In [0..100]) then
  begin
    FBalance := Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJppGradientEx.SetBalanceMode(const Value: TDgradBalanceMode);
begin
  if (Value <> FBalanceMode) then
  begin
    FBalanceMode := Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJppGradientEx.SetAngleDegree(const Value: Word);
begin
  if (Value <> FAngleDegree) and (Value < 360) then
  begin
    FAngleDegree := Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJppGradientEx.SetMaxDegrade(const Value: byte);
begin
  if (Value <> FMaxDegrade) and (Value <> 0) then
  begin
    FMaxDegrade := Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;
{$endregion TJppGradientEx}

end.
