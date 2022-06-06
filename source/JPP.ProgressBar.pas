unit JPP.ProgressBar;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  Highly customizable ProgressBar based on JvSpecialProgress from JVCL package, but with maaaany of my modifications.
  https://github.com/project-jedi/jvcl/blob/master/jvcl/run/JvSpecialProgress.pas
  Original license: MPL 1.1
  My modifications: no license (public domain)
}

// TODO : TJppProgressBar: Hint (show percentage, position...)



{$I jpp.inc}

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType,
  {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Messages,
  SysUtils, Classes, Types, Graphics, Controls, Forms, ExtCtrls,
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls, JPP.Gradient,
  JPL.Strings, JPL.Conversion, JPL.Math, JPL.Colors;

type

  TJppProgressTextDisplayMode = (ptdmPosition, ptdmPercentage, ptdmCaption, ptdPixel, ptdmNone);

  TJppProgressGradientType = (pgtVertical, pgtHorizontal, pgtDiagonal, pgtDiagonal2, pgtVerticalBar, pgtHorizontalBar, pgtDiagonalBar,
    pgtDiagonalBar2, pgtTopBar, pgtBottomBar);


  {$Region '   ProgressBar Stages   '}
  TJppProgressBarStage = class(TJppPersistent)
  private
    FOwner: TComponent;
    FEnabled: Boolean;
    FPercentThreshold: integer;
    FMixWithColor: TColor;
    FPaleness: ShortInt;
    procedure SetEnabled(const Value: Boolean);
    procedure SetPercentThreshold(const Value: integer);
    procedure SetMixWithColor(const Value: TColor);
    procedure SetPaleness(const Value: ShortInt);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppProgressBarStage); reintroduce;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property PercentThreshold: integer read FPercentThreshold write SetPercentThreshold;
    property MixWithColor: TColor read FMixWithColor write SetMixWithColor;
    property Paleness: ShortInt read FPaleness write SetPaleness;
  end;


  TJppProgressBarMidStage = class(TJppProgressBarStage)
  public
    constructor Create(AOwner: TComponent);
  published
    property Enabled default True;
    property PercentThreshold default 50;
    property Paleness default 20;
    property MixWithColor default clNone;
  end;

  TJppProgressBarFinalStage = class(TJppProgressBarStage)
  public
    constructor Create(AOwner: TComponent);
  published
    property Enabled default True;
    property PercentThreshold default 90;
    property Paleness default 30;
    property MixWithColor default clNone;
  end;
  {$endregion ProgressBar Stages}


  {$region '   TJppProgressBarAppearance   '}
  TJppProgressBarAppearance = class(TJppPersistent)
  private
    FOwner: TComponent;
    FBackgroundColor: TColor;
    FProgressColor: TColor;
    FBorderColor: TColor;
    FBackgroundColorTo: TColor;
    FProgressColorTo: TColor;
    FDrawGradient: Boolean;
    FGradientType: TJppProgressGradientType;
    FFont: TFont;
    FBorderSize: Byte;
    FDisabledBackGroundColor: TColor;
    FDisabledBackgroundColorTo: TColor;
    FDisabledProgressColor: TColor;
    FDisabledProgressColorTo: TColor;
    FDisabledBorderColor: TColor;
    FDisabledFont: TFont;
    FTextDisplayMode: TJppProgressTextDisplayMode;
    FTextPrefix: string;
    FTextPostfix: string;
    FEndMarkerColor: TColor;
    FEndMarkerWidth: integer;
    FDisabledEndMarkerColor: TColor;
    FTextAlignment: TJppTextAlignment;
    FGradientSteps: Byte;
    FProgressBarVisible: Boolean;
    FTextShadowSize: ShortInt;
    FTextShadowColor: TColor;
    FDisabledTextShadowColor: TColor;
    FMidStage: TJppProgressBarMidStage;
    FFinalStage: TJppProgressBarFinalStage;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetProgressColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBackgroundColorTo(const Value: TColor);
    procedure SetProgressColorTo(const Value: TColor);
    procedure SetDrawGradient(const Value: Boolean);
    procedure SetGradientType(const Value: TJppProgressGradientType);
    procedure SetFont(const Value: TFont);
    procedure SetBorderSize(const Value: Byte);
    procedure SetDisabledBackGroundColor(const Value: TColor);
    procedure SetDisabledBackgroundColorTo(const Value: TColor);
    procedure SetDisabledProgressColor(const Value: TColor);
    procedure SetDisabledProgressColorTo(const Value: TColor);
    procedure SetDisabledBorderColor(const Value: TColor);
    procedure SetDisabledFont(const Value: TFont);
    procedure SetTextDisplayMode(const Value: TJppProgressTextDisplayMode);
    procedure SetTextPrefix(const Value: string);
    procedure SetTextPostfix(const Value: string);
    procedure SetEndMarkerColor(const Value: TColor);
    procedure SetEndMarkerWidth(const Value: integer);
    procedure SetDisabledEndMarkerColor(const Value: TColor);
    procedure SetTextAlignment(const Value: TJppTextAlignment);
    procedure SetGradientSteps(const Value: Byte);
    procedure SetProgressBarVisible(const Value: Boolean);
    procedure SetTextShadowSize(const Value: ShortInt);
    procedure SetTextShadowColor(const Value: TColor);
    procedure SetDisabledTextShadowColor(const Value: TColor);
    procedure SetMidStage(const Value: TJppProgressBarMidStage);
    procedure SetFinalStage(const Value: TJppProgressBarFinalStage);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppProgressBarAppearance); reintroduce;
  published
    property DrawGradient: Boolean read FDrawGradient write SetDrawGradient default True;
    property GradientType: TJppProgressGradientType read FGradientType write SetGradientType default pgtVertical;
    property GradientSteps: Byte read FGradientSteps write SetGradientSteps default 128;

    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clWhite;
    property BackgroundColorTo: TColor read FBackgroundColorTo write SetBackgroundColorTo default $00F2F2F2;
    property ProgressColor: TColor read FProgressColor write SetProgressColor default $00F2AD68;
    property ProgressColorTo: TColor read FProgressColorTo write SetProgressColorTo default $00F0A051;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property BorderSize: Byte read FBorderSize write SetBorderSize default 1;

    property DisabledBackGroundColor: TColor read FDisabledBackGroundColor write SetDisabledBackGroundColor default $00F8F8F8;
    property DisabledBackgroundColorTo: TColor read FDisabledBackgroundColorTo write SetDisabledBackgroundColorTo default $00EAEAEA;
    property DisabledProgressColor: TColor read FDisabledProgressColor write SetDisabledProgressColor default $00F0E7E1;
    property DisabledProgressColorTo: TColor read FDisabledProgressColorTo write SetDisabledProgressColorTo default $00E2CEC2;
    property DisabledBorderColor: TColor read FDisabledBorderColor write SetDisabledBorderColor default clMedGray;

    property Font: TFont read FFont write SetFont;
    property DisabledFont: TFont read FDisabledFont write SetDisabledFont;

    property TextDisplayMode: TJppProgressTextDisplayMode read FTextDisplayMode write SetTextDisplayMode default ptdmPercentage;
    property TextPrefix: string read FTextPrefix write SetTextPrefix;
    property TextPostfix: string read FTextPostfix write SetTextPostfix;
    property TextAlignment: TJppTextAlignment read FTextAlignment write SetTextAlignment default talCenter;
    property TextShadowSize: ShortInt read FTextShadowSize write SetTextShadowSize default 0;
    property TextShadowColor: TColor read FTextShadowColor write SetTextShadowColor default clMedGray;
    property DisabledTextShadowColor: TColor read FDisabledTextShadowColor write SetDisabledTextShadowColor default clSilver;

    property EndMarkerColor: TColor read FEndMarkerColor write SetEndMarkerColor default clGray;
    property DisabledEndMarkerColor: TColor read FDisabledEndMarkerColor write SetDisabledEndMarkerColor default clMedGray;
    property EndMarkerWidth: integer read FEndMarkerWidth write SetEndMarkerWidth default 1;

    property ProgressBarVisible: Boolean read FProgressBarVisible write SetProgressBarVisible default True;

    property MidStage: TJppProgressBarMidStage read FMidStage write SetMidStage;
    property FinalStage: TJppProgressBarFinalStage read FFinalStage write SetFinalStage;
  end;
  {$endregion TJppProgressBarAppearance}


  {$region '   TJppCustomProgressBar   '}
  TJppCustomProgressBar = class(TGraphicControl)
  private
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FStep: integer;
    FBitmap: TBitmap;
    FBitmapCanvas: TCanvas;
    FPixPos: integer;
    FNeedRepaint: Boolean;
    FTagExt: TJppTagExt;
    FAnchoredControls: TJppAnchoredControls;
    FAppearance: TJppProgressBarAppearance;
    FOnProgressChanged: TNotifyEvent;
    FUpdateProgressIfDisabled: Boolean;
    FBoundLabel: TJppControlBoundLabel;
    FBoundLabelPosition: TLabelPosition;
    FBoundLabelSpacing: Integer;
    FForceClear: Boolean;
    function GetPercentDone: integer;
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure DrawBorder;
    procedure DrawProgress;
    procedure DrawText;
    procedure DoEraseBackground;
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
    procedure SetAppearance(const Value: TJppProgressBarAppearance);
    function GetJppGradientType(const GradientType: TJppProgressGradientType): TJppGradientType;
    procedure SetOnProgressChanged(const Value: TNotifyEvent);
    procedure SetUpdateProgressIfDisabled(const Value: Boolean);
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure AdjustLabelBounds(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure UpdateBitmap;
    procedure UpdatePixPos;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StepIt;
    procedure StepBy(const DeltaPos: integer);
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure SetupInternalLabel;
    function PixelPos: integer;
  protected
    procedure PropsChanged(Sender: TObject);
    property PercentDone: integer read GetPercentDone;
    property Caption;
    property Min: integer read FMin write SetMin default 0;
    property Max: integer read FMax write SetMax default 100;
    property Position: integer read FPosition write SetPosition default 0;
    property Step: integer read FStep write FStep default 10;

    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
    property Appearance: TJppProgressBarAppearance read FAppearance write SetAppearance;
    property OnProgressChanged: TNotifyEvent read FOnProgressChanged write SetOnProgressChanged;
    property UpdateProgressIfDisabled: Boolean read FUpdateProgressIfDisabled write SetUpdateProgressIfDisabled default False;

    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpAbove;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;
  end;
  {$endregion TJppCustomProgressBar}


  {$region '   TJppProgressBar   '}
  TJppProgressBar = class(TJppCustomProgressBar)
  published
    property Align;
    property Anchors;
    {$IFDEF FPC}
    property BorderSpacing;
    {$ENDIF}
    property Caption;
    property Enabled;
    property ShowHint;
    property Step;
    property Visible;

    property PercentDone; // read-only
    property Max;
    property Min;
    property Position;
    property TagExt;
    property AnchoredControls;
    property Appearance;
    property UpdateProgressIfDisabled;
    property BoundLabel;
    property BoundLabelPosition;
    property BoundLabelSpacing;

    // Events
    property OnContextPopup;
    property OnClick;
    property OnDblClick;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHI2010_OR_ABOVE}property OnGesture;{$ENDIF}
    {$IFDEF DCC}property OnMouseActivate;{$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnResize;
    property OnStartDrag;

    // Additional events
    property OnProgressChanged;
  end;
  {$endregion}



implementation



{$region '                        TJppCustomProgressBar                             '}

constructor TJppCustomProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBoundLabelPosition := lpAbove;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  FBitmap := TBitmap.Create;
  FBitmapCanvas := FBitmap.Canvas;

  ControlStyle := ControlStyle + [csOpaque];
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FStep := 10;

  Width := 150;
  Height := 17;
  FNeedRepaint := True;

  FTagExt := TJppTagExt.Create(Self);
  FAnchoredControls := TJppAnchoredControls.Create(Self);

  FAppearance := TJppProgressBarAppearance.Create(AOwner);
  FAppearance.OnChange := PropsChanged;
  FAppearance.Font.OnChange := PropsChanged;
  FAppearance.DisabledFont.OnChange := PropsChanged;

  FOnProgressChanged := nil;

  FUpdateProgressIfDisabled := False;

  //PropsChanged(Self);
end;

destructor TJppCustomProgressBar.Destroy;
begin
  FTagExt.Free;
  FAnchoredControls.Free;
  FAppearance.Free;
  FBitmap.Free;
  inherited;
end;

procedure TJppCustomProgressBar.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  FNeedRepaint := True;
  if Enabled then FBitmapCanvas.Font.Assign(FAppearance.Font) else FBitmapCanvas.Font.Assign(FAppearance.DisabledFont);
  UpdateBitmap;
end;

procedure TJppCustomProgressBar.Loaded;
begin
  inherited Loaded;
  if Enabled then FBitmapCanvas.Font.Assign(FAppearance.Font) else FBitmapCanvas.Font.Assign(FAppearance.DisabledFont);
  UpdatePixPos;
  UpdateBitmap;
end;

procedure TJppCustomProgressBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if not (csDestroying in ComponentState) then
      if Assigned(FAnchoredControls) then
      begin
        if AComponent = FBoundLabel then FBoundLabel := nil
        else if AComponent = FAnchoredControls.Top.Control then FAnchoredControls.Top.Control := nil
        else if AComponent = FAnchoredControls.Bottom.Control then FAnchoredControls.Bottom.Control := nil
        else if AComponent = FAnchoredControls.Left.Control then FAnchoredControls.Left.Control := nil
        else if AComponent = FAnchoredControls.Right.Control then FAnchoredControls.Right.Control := nil;
      end;
end;

procedure TJppCustomProgressBar.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomProgressBar.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
end;

procedure TJppCustomProgressBar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
end;

procedure TJppCustomProgressBar.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
end;

procedure TJppCustomProgressBar.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomProgressBar.SetAppearance(const Value: TJppProgressBarAppearance);
begin
  FAppearance := Value;
  PropsChanged(Self);
end;

procedure TJppCustomProgressBar.SetBoundLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FBoundLabel = nil then Exit;
  FBoundLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FBoundLabel.Height - FBoundLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FBoundLabelSpacing);
    lpLeft : P := Point(Left - FBoundLabel.Width - FBoundLabelSpacing, Top + ((Height - FBoundLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FBoundLabelSpacing, Top + ((Height - FBoundLabel.Height) div 2));
  end;

  FBoundLabel.SetBounds({%H-}P.x, {%H-}P.y, FBoundLabel.Width, FBoundLabel.Height);
  //FBoundLabel.Visible := FShowLabel;
end;

procedure TJppCustomProgressBar.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomProgressBar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  SetBoundLabelPosition(FBoundLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomProgressBar.SetEnabled(Value: Boolean);
begin
  inherited;
  PropsChanged(Self);
end;

procedure TJppCustomProgressBar.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomProgressBar.SetUpdateProgressIfDisabled(const Value: Boolean);
begin
  if FUpdateProgressIfDisabled = Value then Exit;
  FUpdateProgressIfDisabled := Value;
  if not Enabled then PropsChanged(Self);
end;

procedure TJppCustomProgressBar.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  //FBoundLabel.FocusControl := Self;
  {$IFDEF FPC}FBoundLabel.OnChangeBounds := AdjustLabelBounds;{$ENDIF}
end;

function TJppCustomProgressBar.GetJppGradientType(const GradientType: TJppProgressGradientType): TJppGradientType;
begin
  case GradientType of
    pgtVertical: Result := gtVertical;
    pgtHorizontal: Result := gtHorizontal;
    pgtDiagonal: Result := gtDiagonal;
    pgtDiagonal2: Result := gtDiagonal2;
    pgtVerticalBar: Result := gtVerticalBar;
    pgtHorizontalBar: Result := gtHorizontalBar;
    pgtDiagonalBar: Result := gtDiagonalBar;
    pgtDiagonalBar2: Result := gtDiagonalBar2;
    pgtTopBar: Result := gtTopBar;
    pgtBottomBar: Result := gtBottomBar;
  else
    Result := gtVertical;
  end;
end;

procedure TJppCustomProgressBar.Paint;
begin
  if (FBitmap.Width <> ClientWidth) or (FBitmap.Height <> ClientHeight) then
  begin
    FNeedRepaint := True;
    UpdatePixPos;
    UpdateBitmap;
  end;

  if (ClientWidth > 2) and (ClientHeight > 2) then
    BitBlt(Self.Canvas.Handle, 0, 0, ClientWidth, ClientHeight, FBitmapCanvas.Handle, 0, 0, SRCCOPY);
end;

function TJppCustomProgressBar.PixelPos: integer;
begin
  Result := FPixPos;
end;

procedure TJppCustomProgressBar.DrawBorder;
var
  R: TRect;
  clBorder: TColor;
begin
  if FAppearance.BorderSize = 0 then Exit;
  R := ClientRect;
  if Enabled then clBorder := FAppearance.BorderColor else clBorder := FAppearance.DisabledBorderColor;
  JppFrame3D(FBitmapCanvas, R, clBorder, FAppearance.BorderSize);
end;

procedure TJppCustomProgressBar.DoEraseBackground;
var
  R: TRect;
  clStart, clEnd: TColor;
  xbs: integer;
begin
  xbs := FAppearance.BorderSize;

  if (not FForceClear) and (FPixPos >= ClientWidth - (2 * xbs)) then Exit;

  FForceClear := False;

  if FAppearance.ProgressBarVisible then R := Rect(FPixPos + xbs, xbs, ClientWidth - xbs, ClientHeight - xbs)
  else R := Rect(xbs, xbs, ClientWidth - xbs, ClientHeight - xbs);

  if Enabled then
  begin
    clStart := FAppearance.BackgroundColor;
    clEnd := FAppearance.BackgroundColorTo;
  end
  else
  begin
    clStart := FAppearance.DisabledBackgroundColor;
    clEnd := FAppearance.DisabledBackgroundColorTo;
  end;

  if FAppearance.DrawGradient and (clStart <> clEnd) and (clEnd <> clNone) and (FAppearance.GradientSteps > 0) then
    JppGradientFill(FBitmapCanvas, R, clStart, clEnd, GetJppGradientType(FAppearance.GradientType), FAppearance.GradientSteps)
  else
    with FBitmapCanvas do
    begin
      Brush.Color := clStart;
      Brush.Style := bsSolid;
      FillRect(R);
    end;
end;

procedure TJppCustomProgressBar.DrawProgress;
var
  R: TRect;
  clStart, clEnd: TColor;
  xbs: integer;
  clMarker: TColor;
  xp: integer;
  Stage: TJppProgressBarStage;
  bFinal: Boolean;
begin
  if not FAppearance.ProgressBarVisible then Exit;
  if FPixPos = 0 then Exit;

  xbs := FAppearance.BorderSize;
  R := Rect(xbs, xbs, FPixPos + xbs, ClientHeight - xbs);

  if Enabled then
  begin
    clStart := FAppearance.ProgressColor;
    clEnd := FAppearance.ProgressColorTo;
  end
  else
  begin
    clStart := FAppearance.DisabledProgressColor;
    clEnd := FAppearance.DisabledProgressColorTo;
  end;

  xp := PercentDone;
  bFinal := False;

  Stage := FAppearance.FinalStage;
  if (xp >= Stage.PercentThreshold) and Stage.Enabled then
  begin
    bFinal := True;
    if Stage.MixWithColor <> clNone then
    begin
      clStart := AvgColor(clStart, Stage.MixWithColor);
      clEnd := AvgColor(clStart, Stage.MixWithColor);
    end;
    if Stage.Paleness <> 0 then
    begin
      clStart := ColorSetPercentPale(clStart, Stage.Paleness);
      clEnd := ColorSetPercentPale(clEnd, Stage.Paleness);
    end;
  end;

  Stage := FAppearance.MidStage;
  if (not bFinal) and (xp >= Stage.PercentThreshold) and Stage.Enabled then
  begin
    if Stage.MixWithColor <> clNone then
    begin
      clStart := AvgColor(clStart, Stage.MixWithColor);
      clEnd := AvgColor(clStart, Stage.MixWithColor);
    end;
    if Stage.Paleness <> 0 then
    begin
      clStart := ColorSetPercentPale(clStart, Stage.Paleness);
      clEnd := ColorSetPercentPale(clEnd, Stage.Paleness);
    end;
  end;


  if (FAppearance.DrawGradient) and (clStart <> clEnd) and (clEnd <> clNone) and (FAppearance.GradientSteps > 0) then
    JppGradientFill(FBitmapCanvas, R, clStart, clEnd, GetJppGradientType(FAppearance.GradientType), FAppearance.GradientSteps)
  else
    with FBitmapCanvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := clStart;
      FillRect(R);
    end;

  if FAppearance.EndMarkerWidth > 0 then
  begin
    if Enabled then clMarker := FAppearance.EndMarkerColor else clMarker := FAppearance.DisabledEndMarkerColor;
    if clMarker <> clNone then
      with FBitmapCanvas do
      begin
        R := Rect(
          FPixPos - FAppearance.EndMarkerWidth + xbs, xbs,
          FPixPos + xbs, ClientHeight - xbs
        );
        Brush.Color := clMarker;
        FillRect(R);
      end;
  end;
end;

function TJppCustomProgressBar.GetPercentDone: integer;
begin
  if FMin = FMax then Result := 0
  else Result := MulDiv(FPosition - FMin, 100, FMax - FMin);
end;

procedure TJppCustomProgressBar.DrawText;
var
  s: string;
  x, y, xLeft, xTop, xWidth, xHeight, HalfWidth, HalfHeight, xbs: integer;
  tw, th: integer;
  clFont: TColor;
  xpDone: integer;
begin
  xpDone := PercentDone;

  case FAppearance.TextDisplayMode of
    ptdmPosition: s := IntToStr(FPosition);
    ptdmPercentage: s := IntToStr(xpDone);
    ptdmCaption: s := Caption;
    ptdPixel: s := IntToStr(FPixPos);
    ptdmNone: s := '';
  end;

  if s = '' then Exit;

  s := FAppearance.TextPrefix + s + FAppearance.TextPostfix;

  tw := FBitmapCanvas.TextWidth(s);
  th := FBitmapCanvas.TextHeight(s);

  xLeft := FAppearance.BorderSize;
  xTop := FAppearance.BorderSize;
  xWidth := ClientWidth;
  xHeight := ClientHeight;
  HalfWidth := ClientWidth div 2;
  HalfHeight := ClientHeight div 2;
  xbs := FAppearance.BorderSize;

  {$IFDEF DCC}
  // initialize
  x := 0;
  y := 0;
  {$ENDIF}

  case FAppearance.TextAlignment of
    talTopLeft:
      begin
        x := xLeft;
        y := xTop;
      end;
    talTopCenter:
      begin
        x := HalfWidth - tw div 2;
        y := xTop;
      end;
    talTopRight:
      begin
        x := xWidth - tw - xbs;
        y := xTop;
      end;
    talLeft:
      begin
        x := xLeft;
        y := HalfHeight - (th div 2);
      end;
    talCenter:
      begin
        x := HalfWidth - (tw div 2);
        y := HalfHeight - (th div 2);
      end;
    talRight:
      begin
        x := xWidth - tw - xbs;
        y := HalfHeight - (th div 2);
      end;
    talBottomLeft:
      begin
        x := xLeft;
        y := xHeight - th - xbs;
      end;
    talBottomCenter:
      begin
        x := HalfWidth - (tw div 2);
        y := xHeight - th - xbs;
      end;
    talBottomRight:
      begin
        x := xWidth - tw - xbs;
        y := xHeight - th - xbs;
      end;
  end;

  if x < 0 then x := 0;
  if y < 0 then y := 0;

  if xpDone in [0, 100] then
  begin
    FForceClear := True;
    DoEraseBackground;
  end;

  SetBkMode(FBitmapCanvas.Handle, {$IFDEF FPC}LCLType{$ELSE}Windows{$ENDIF}.TRANSPARENT);


  clFont := FBitmapCanvas.Font.Color;

  if FAppearance.TextShadowSize <> 0 then
  begin
    if Enabled then FBitmapCanvas.Font.Color := FAppearance.TextShadowColor
    else FBitmapCanvas.Font.Color := FAppearance.DisabledTextShadowColor;
    FBitmapCanvas.TextOut(x + FAppearance.TextShadowSize, y + FAppearance.TextShadowSize, s);
  end;

  FBitmapCanvas.Font.Color := clFont;
  FBitmapCanvas.TextOut(x, y, s);

end;

procedure TJppCustomProgressBar.SetMin(const Value: integer);
var
  OldPercentageDone: Integer;
begin
  if FMin = Value then Exit;

  OldPercentageDone := GetPercentDone;

  FMin := Value;
  if FMin > FMax then FMin := FMax;
  if FPosition < Value then FPosition := Value;

  FNeedRepaint := OldPercentageDone <> GetPercentDone;
  UpdatePixPos;
  UpdateBitmap;
end;

procedure TJppCustomProgressBar.SetOnProgressChanged(const Value: TNotifyEvent);
begin
  FOnProgressChanged := Value;
end;

procedure TJppCustomProgressBar.SetMax(const Value: integer);
var
  OldPercentageDone: Integer;
begin
  if FMax = Value then Exit;

  OldPercentageDone := GetPercentDone;

  FMax := Value;
  if FMax < FMin then FMax := FMin;
  if FPosition > Value then FPosition := Value;

  FNeedRepaint := OldPercentageDone <> GetPercentDone;
  UpdatePixPos;
  UpdateBitmap;
end;

procedure TJppCustomProgressBar.SetParent(AParent: TWinControl);
begin
  inherited;
  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;
end;

procedure TJppCustomProgressBar.SetPosition(const Value: Integer);
var
  OldPercentageDone: Integer;
begin
  if (not Enabled) and (not FUpdateProgressIfDisabled) and (not (csLoading in ComponentState)) then Exit;
  if FPosition = Value then Exit;

  OldPercentageDone := GetPercentDone;

  FPosition := Value;
  if FPosition > FMax then FPosition := FMax
  else if FPosition < FMin then FPosition := FMin;

  FNeedRepaint := OldPercentageDone <> GetPercentDone;

  UpdatePixPos;
  UpdateBitmap;

  if Assigned(FOnProgressChanged) then FOnProgressChanged(Self);
end;

procedure TJppCustomProgressBar.StepIt;
begin
  if (not Enabled) and (not FUpdateProgressIfDisabled) then Exit;
  StepBy(FStep);
end;

procedure TJppCustomProgressBar.StepBy(const DeltaPos: integer);
var
  NewPos: integer;
begin
  if DeltaPos = 0 then Exit;
  NewPos := FPosition + DeltaPos;
  NewPos := GetIntInRange(NewPos, FMin, FMax);
  SetPosition(NewPos);
end;

procedure TJppCustomProgressBar.UpdateBitmap;
begin
  if not FNeedRepaint or (csLoading in ComponentState) then Exit;
  if (ClientWidth <= 0) or (ClientHeight <= 0) then Exit;

  FNeedRepaint := False;

  FBitmap.Width := ClientWidth;
  FBitmap.Height := ClientHeight;

  DrawProgress;
  DoEraseBackground;
  DrawText;
  DrawBorder;

  Invalidate;
end;

procedure TJppCustomProgressBar.UpdatePixPos;
var
  NewPixPos: integer;
begin
  if csLoading in ComponentState then Exit;

  if (FMin = FMax) or (ClientWidth < (2 * FAppearance.BorderSize)) then Exit;

  NewPixPos := MulDiv(FPosition - FMin, ClientWidth - (2 * FAppearance.BorderSize), FMax - FMin);
  if NewPixPos = FPixPos then Exit;

  FPixPos := NewPixPos;
  FNeedRepaint := True;
  UpdateBitmap;
end;
{$endregion TJppCustomProgressBar}



{$region '                        TJppProgressBarAppearance                          '}
constructor TJppProgressBarAppearance.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FDrawGradient := True;
  FGradientType := pgtVertical;
  FGradientSteps := 128;

  FBackgroundColor := clWhite;
  FBackgroundColorTo := $00F2F2F2;
  FProgressColor := $00F2AD68;
  FProgressColorTo := $00F0A051;

  FDisabledBackGroundColor := $00F8F8F8;
  FDisabledBackgroundColorTo := $00EAEAEA;
  FDisabledProgressColor := $00F0E7E1;
  FDisabledProgressColorTo := $00E2CEC2;

  FBorderColor := clGray;
  FDisabledBorderColor := clMedGray;
  FBorderSize := 1;

  FFont := TFont.Create;
  FDisabledFont := TFont.Create;
  FDisabledFont.Color := clMedGray;

  FTextDisplayMode := ptdmPercentage;
  FTextPrefix := '';
  FTextPostfix := '%';
  FTextAlignment := talCenter;
  FTextShadowSize := 0;
  FTextShadowColor := clMedGray;
  FDisabledTextShadowColor := clSilver;

  FEndMarkerColor := clGray;
  FDisabledEndMarkerColor := clMedGray;
  FEndMarkerWidth := 1;

  FProgressBarVisible := True;

  MidStage := TJppProgressBarMidStage.Create(AOwner);
  MidStage.OnChange := PropsChanged;

  FinalStage := TJppProgressBarFinalStage.Create(AOwner);
  FinalStage.OnChange := PropsChanged;
end;

destructor TJppProgressBarAppearance.Destroy;
begin
  FFont.Free;
  FDisabledFont.Free;
  MidStage.Free;
  FinalStage.Free;
  inherited;
end;

procedure TJppProgressBarAppearance.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor = Value then Exit;
  FBackgroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetBackgroundColorTo(const Value: TColor);
begin
  if FBackgroundColorTo = Value then Exit;
  FBackgroundColorTo := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetBorderColor(const Value: TColor);
begin
  if FBorderColor = Value then Exit;
  FBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetBorderSize(const Value: Byte);
begin
  if FBorderSize = Value then Exit;
  FBorderSize := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetDisabledBackGroundColor(const Value: TColor);
begin
  if FDisabledBackGroundColor = Value then Exit;
  FDisabledBackGroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetDisabledBackgroundColorTo(const Value: TColor);
begin
  if FDisabledBackgroundColorTo = Value then Exit;
  FDisabledBackgroundColorTo := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetDisabledBorderColor(const Value: TColor);
begin
  if FDisabledBorderColor = Value then Exit;
  FDisabledBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetDisabledEndMarkerColor(const Value: TColor);
begin
  if FDisabledEndMarkerColor = Value then Exit;
  FDisabledEndMarkerColor := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetDisabledFont(const Value: TFont);
begin
  FDisabledFont.Assign(Value);
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetDisabledProgressColor(const Value: TColor);
begin
  if FDisabledProgressColor = Value then Exit;
  FDisabledProgressColor := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetDisabledProgressColorTo(const Value: TColor);
begin
  if FDisabledProgressColorTo = Value then Exit;
  FDisabledProgressColorTo := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetDisabledTextShadowColor(const Value: TColor);
begin
  if FDisabledTextShadowColor = Value then Exit;
  FDisabledTextShadowColor := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetDrawGradient(const Value: Boolean);
begin
  if FDrawGradient = Value then Exit;
  FDrawGradient := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetEndMarkerColor(const Value: TColor);
begin
  if FEndMarkerColor = Value then Exit;
  FEndMarkerColor := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetEndMarkerWidth(const Value: integer);
begin
  if FEndMarkerWidth = Value then Exit;
  FEndMarkerWidth := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetFinalStage(const Value: TJppProgressBarFinalStage);
begin
  FFinalStage := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetGradientSteps(const Value: Byte);
begin
  if FGradientSteps = Value then Exit;
  FGradientSteps := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetGradientType(const Value: TJppProgressGradientType);
begin
  if FGradientType = Value then Exit;
  FGradientType := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetMidStage(const Value: TJppProgressBarMidStage);
begin
  FMidStage := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetProgressBarVisible(const Value: Boolean);
begin
  if FProgressBarVisible = Value then Exit;
  FProgressBarVisible := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetProgressColor(const Value: TColor);
begin
  if FProgressColor = Value then Exit;
  FProgressColor := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetProgressColorTo(const Value: TColor);
begin
  if FProgressColorTo = Value then Exit;
  FProgressColorTo := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetTextAlignment(const Value: TJppTextAlignment);
begin
  if FTextAlignment = Value then Exit;
  FTextAlignment := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetTextDisplayMode(const Value: TJppProgressTextDisplayMode);
begin
  if FTextDisplayMode = Value then Exit;
  FTextDisplayMode := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetTextPostfix(const Value: string);
begin
  if FTextPostfix = Value then Exit;
  FTextPostfix := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetTextPrefix(const Value: string);
begin
  if FTextPrefix = Value then Exit;
  FTextPrefix := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetTextShadowColor(const Value: TColor);
begin
  if FTextShadowColor = Value then Exit;
  FTextShadowColor := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.SetTextShadowSize(const Value: ShortInt);
begin
  if FTextShadowSize = Value then Exit;
  FTextShadowSize := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarAppearance.Assign(Source: TJppProgressBarAppearance);
begin
  FDrawGradient := Source.DrawGradient;
  FGradientType := Source.GradientType;
  FGradientSteps := Source.GradientSteps;
  FBackgroundColor := Source.BackgroundColor;
  FBackgroundColorTo := Source.BackgroundColorTo;
  FProgressColor := Source.ProgressColor;
  FProgressColorTo := Source.ProgressColorTo;
  FBorderColor := Source.BorderColor;
  FBorderSize := Source.BorderSize;
  FDisabledBackGroundColor := Source.DisabledBackGroundColor;
  FDisabledBackgroundColorTo := Source.DisabledBackgroundColorTo;
  FDisabledProgressColor := Source.DisabledProgressColor;
  FDisabledProgressColorTo := Source.DisabledProgressColorTo;
  FDisabledBorderColor := Source.DisabledBorderColor;
  FFont.Assign(Source.Font);
  FDisabledFont.Assign(Source.DisabledFont);
  FTextDisplayMode := Source.TextDisplayMode;
  FTextPrefix := Source.TextPrefix;
  FTextPostfix := Source.TextPostfix;
  FTextAlignment := Source.TextAlignment;
  FTextShadowSize := Source.TextShadowSize;
  FTextShadowColor := Source.TextShadowColor;
  FDisabledTextShadowColor := Source.DisabledTextShadowColor;
  FEndMarkerColor := Source.EndMarkerColor;
  FDisabledEndMarkerColor := Source.DisabledEndMarkerColor;
  FEndMarkerWidth := Source.EndMarkerWidth;
  FProgressBarVisible := Source.ProgressBarVisible;

  FMidStage.Assign(Source.MidStage);
  FFinalStage.Assign(Source.FinalStage);

  PropsChanged(Self);
end;
{$endregion TJppProgressBarAppearance}



{$Region '               ProgressBar Stages              '}

constructor TJppProgressBarStage.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TJppProgressBarStage.Destroy;
begin
  inherited;
end;

procedure TJppProgressBarStage.Assign(Source: TJppProgressBarStage);
begin
  FMixWithColor := Source.MixWithColor;
  FPaleness := Source.Paleness;
  FEnabled := Source.Enabled;
  FPercentThreshold := Source.PercentThreshold;
end;

procedure TJppProgressBarStage.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then Exit;
  FEnabled := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarStage.SetMixWithColor(const Value: TColor);
begin
  if FMixWithColor = Value then Exit;
  FMixWithColor := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarStage.SetPaleness(const Value: ShortInt);
begin
  if FPaleness = Value then Exit;
  FPaleness := Value;
  PropsChanged(Self);
end;

procedure TJppProgressBarStage.SetPercentThreshold(const Value: integer);
begin
  if FPercentThreshold = Value then Exit;
  FPercentThreshold := Value;
  PropsChanged(Self);
end;

constructor TJppProgressBarMidStage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FPercentThreshold := 50;
  FPaleness := 20;
  FMixWithColor := clNone;
end;

constructor TJppProgressBarFinalStage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FPercentThreshold := 90;
  FPaleness := 30;
  FMixWithColor := clNone;
end;

{$endregion ProgressBar Stages}



end.

